library(data.table)
library(gausscov)
library(paradox)
library(mlr3)
library(mlr3pipelines)
library(mlr3viz)
library(mlr3tuning)
library(mlr3misc)
library(mlr3extralearners)
library(future)
library(future.apply)
library(batchtools)
library(mlr3batchmark)
library(torch)
library(mlr3torch)


# SETUP -------------------------------------------------------------------
# utils https://stackoverflow.com/questions/1995933/number-of-months-between-two-dates
monnb <- function(d) {
  lt <- as.POSIXlt(as.Date(d, origin="1900-01-01"))
  lt$year*12 + lt$mon }
mondf <- function(d1, d2) { monnb(d2) - monnb(d1) }
diff_in_weeks = function(d1, d2) difftime(d2, d1, units = "weeks") # weeks

# weeknb <- function(d) {
#   as.numeric(difftime(as.Date(d), as.Date("1900-01-01"), units = "weeks"))
# }
# weekdf <- function(d1, d2) {
#   weeknb(d2) - weeknb(d1)
# }

# snake to camel
snakeToCamel <- function(snake_str) {
  # Replace underscores with spaces
  spaced_str <- gsub("_", " ", snake_str)
  
  # Convert to title case using tools::toTitleCase
  title_case_str <- tools::toTitleCase(spaced_str)
  
  # Remove spaces and make the first character lowercase
  camel_case_str <- gsub(" ", "", title_case_str)
  camel_case_str <- sub("^.", tolower(substr(camel_case_str, 1, 1)), camel_case_str)
  
  # I haeve added this to remove dot
  camel_case_str <- gsub("\\.", "", camel_case_str)
  
  return(camel_case_str)
}


# PREPARE DATA ------------------------------------------------------------
print("Prepare data")

# read predictors
if (interactive()) {
  data_tbl <- fread("D:/features/pead-predictors-20231031.csv")
} else {
  data_tbl <- fread("pead-predictors-20231106.csv")
}

# convert tibble to data.table
DT = as.data.table(data_tbl)

# create group variable
DT[, date_rolling := as.IDate(date_rolling)]
DT[, yearmonthid := round(date_rolling, digits = "month")]
DT[, weekid := round(date_rolling, digits = "week")]
DT[, .(date, date_rolling, yearmonthid, weekid)]
DT[, yearmonthid := as.integer(yearmonthid)]
DT[, weekid := as.integer(weekid)]
DT[, .(date, date_rolling, yearmonthid, weekid)]

# remove industry and sector vars
DT[, `:=`(industry = NULL, sector = NULL)]

# define predictors
cols_non_features <- c("symbol", "date", "time", "right_time",
                       "bmo_return", "amc_return",
                       "open", "high", "low", "close", "volume", "returns",
                       "yearmonthid", "weekid", "date_rolling"
)
targets <- c(colnames(DT)[grep("ret_excess", colnames(DT))])
cols_features <- setdiff(colnames(DT), c(cols_non_features, targets))

# change feature and targets columns names due to lighgbm
cols_features_new = vapply(cols_features, snakeToCamel, FUN.VALUE = character(1L), USE.NAMES = FALSE)
setnames(DT, cols_features, cols_features_new)
cols_features = cols_features_new
targets_new = vapply(targets, snakeToCamel, FUN.VALUE = character(1L), USE.NAMES = FALSE)
setnames(DT, targets, targets_new)
targets = targets_new

# convert columns to numeric. This is important only if we import existing features
chr_to_num_cols <- setdiff(colnames(DT[, .SD, .SDcols = is.character]), c("symbol", "time", "right_time"))
print(chr_to_num_cols)
DT <- DT[, (chr_to_num_cols) := lapply(.SD, as.numeric), .SDcols = chr_to_num_cols]

# remove constant columns in set
features_ <- DT[, ..cols_features]
remove_cols <- colnames(features_)[apply(features_, 2, var, na.rm=TRUE) == 0]
print(paste0("Removing feature with 0 standard deviation: ", remove_cols))
cols_features <- setdiff(cols_features, remove_cols)

# convert variables with low number of unique values to factors
int_numbers = na.omit(DT[, ..cols_features])[, lapply(.SD, function(x) all(floor(x) == x))]
int_cols = colnames(DT[, ..cols_features])[as.matrix(int_numbers)[1,]]
factor_cols = DT[, ..int_cols][, lapply(.SD, function(x) length(unique(x)))]
factor_cols = as.matrix(factor_cols)[1, ]
factor_cols = factor_cols[factor_cols <= 100]
DT = DT[, (names(factor_cols)) := lapply(.SD, as.factor), .SD = names(factor_cols)]

# remove observations with missing target
# if we want to keep as much data as possible an use only one predicitn horizont
# we can skeep this step
DT = na.omit(DT, cols = setdiff(targets, colnames(DT)[grep("xtreme", colnames(DT))]))

# change IDate to date, because of error
# Assertion on 'feature types' failed: Must be a subset of
# {'logical','integer','numeric','character','factor','ordered','POSIXct'},
# but has additional elements {'IDate'}.
DT[, date := as.POSIXct(date, tz = "UTC")]
# DT[, .(symbol,date, date_rolling, yearmonthid)]

# sort
# this returns error on HPC. Some problem with memory
# setorder(DT, date)
print("This was the problem")
# DT = DT[order(date)] # DOESNT WORK TOO
DT = DT[order(yearmonthid, weekid)]
DT[, .(symbol, date, weekid, yearmonthid)]
print("This was the problem. Solved.")


# TASKS -------------------------------------------------------------------
print("Tasks")

# id coluns we always keep
id_cols = c("symbol", "date", "yearmonthid", "weekid")

# convert date to PosixCt because it is requireed by mlr3
DT[, date := as.POSIXct(date, tz = "UTC")]

# task with future week returns as target
target_ = colnames(DT)[grep("^ret.*xcess.*tand.*5", colnames(DT))]
cols_ = c(id_cols, target_, cols_features)
task_ret_week <- as_task_regr(DT[, ..cols_],
                              id = "taskRetWeek",
                              target = target_)

# task with future month returns as target
target_ = colnames(DT)[grep("^ret.*xcess.*tand.*22", colnames(DT))]
cols_ = c(id_cols, target_, cols_features)
task_ret_month <- as_task_regr(DT[, ..cols_],
                               id = "taskRetMonth",
                               target = target_)

# task with future 2 months returns as target
target_ = colnames(DT)[grep("^ret.*xcess.*tand.*44", colnames(DT))]
cols_ = c(id_cols, target_, cols_features)
task_ret_month2 <- as_task_regr(DT[, ..cols_],
                                id = "taskRetMonth2",
                                target = target_)

# task with future 2 months returns as target
target_ = colnames(DT)[grep("^ret.*xcess.*tand.*66", colnames(DT))]
cols_ = c(id_cols, target_, cols_features)
task_ret_quarter <- as_task_regr(DT[, ..cols_],
                                 id = "taskRetQuarter",
                                 target = target_)

# set roles for symbol, date and yearmonth_id
task_ret_week$col_roles$feature = setdiff(task_ret_week$col_roles$feature,
                                          id_cols)
task_ret_month$col_roles$feature = setdiff(task_ret_month$col_roles$feature,
                                           id_cols)
task_ret_month2$col_roles$feature = setdiff(task_ret_month2$col_roles$feature,
                                            id_cols)
task_ret_quarter$col_roles$feature = setdiff(task_ret_quarter$col_roles$feature,
                                             id_cols)


# CROSS VALIDATIONS -------------------------------------------------------
print("Cross validations")

# create train, tune and test set
nested_cv_split = function(task,
                           train_length = 12,
                           tune_length = 1,
                           test_length = 1,
                           gap_tune = 3,
                           gap_test = 3,
                           id = task$id) {
  
  # get year month id data
  # task = task_ret_week$clone()
  task_ = task$clone()
  yearmonthid_ = task_$backend$data(cols = c("yearmonthid", "..row_id"),
                                    rows = 1:task_$nrow)
  stopifnot(all(task_$row_ids == yearmonthid_$`..row_id`))
  groups_v = yearmonthid_[, unlist(unique(yearmonthid))]
  
  # create cusom CV's for inner and outer sampling
  custom_inner = rsmp("custom", id = task$id)
  custom_outer = rsmp("custom", id = task$id)
  
  # util vars
  start_folds = 1:(length(groups_v)-train_length-tune_length-test_length-gap_test-gap_tune)
  get_row_ids = function(mid) unlist(yearmonthid_[yearmonthid %in% mid, 2], use.names = FALSE)
  
  # create train data
  train_groups <- lapply(start_folds,
                         function(x) groups_v[x:(x+train_length-1)])
  train_sets <- lapply(train_groups, get_row_ids)
  
  # create tune set
  tune_groups <- lapply(start_folds,
                        function(x) groups_v[(x+train_length+gap_tune):(x+train_length+gap_tune+tune_length-1)])
  tune_sets <- lapply(tune_groups, get_row_ids)
  
  # test train and tune
  test_1 = vapply(seq_along(train_groups), function(i) {
    mondf(
      tail(as.Date(train_groups[[i]], origin = "1970-01-01"), 1),
      head(as.Date(tune_groups[[i]], origin = "1970-01-01"), 1)
    )
  }, FUN.VALUE = numeric(1L))
  stopifnot(all(test_1 == (1+gap_tune)))
  # test_2 = vapply(seq_along(train_groups), function(i) {
  #   unlist(head(tune_sets[[i]], 1) - tail(train_sets[[i]], 1))
  # }, FUN.VALUE = numeric(1L))
  # stopifnot(all(test_2 > ))
  
  # create test sets
  insample_length = train_length + gap_tune + tune_length + gap_test
  test_groups <- lapply(start_folds,
                        function(x) groups_v[(x+insample_length):(x+insample_length+test_length-1)])
  test_sets <- lapply(test_groups, get_row_ids)
  
  # test tune and test
  test_3 = vapply(seq_along(train_groups), function(i) {
    mondf(
      tail(as.Date(tune_groups[[i]], origin = "1970-01-01"), 1),
      head(as.Date(test_groups[[i]], origin = "1970-01-01"), 1)
    )
  }, FUN.VALUE = numeric(1L))
  stopifnot(all(test_1 == 1 + gap_test))
  # test_4 = vapply(seq_along(train_groups), function(i) {
  #   unlist(head(test_sets[[i]], 1) - tail(tune_sets[[i]], 1))
  # }, FUN.VALUE = numeric(1L))
  # stopifnot(all(test_2 == 1))
  
  # test
  # as.Date(train_groups[[2]])
  # as.Date(tune_groups[[2]])
  # as.Date(test_groups[[2]])
  
  # create inner and outer resamplings
  custom_inner$instantiate(task, train_sets, tune_sets)
  inner_sets = lapply(seq_along(train_groups), function(i) {
    c(train_sets[[i]], tune_sets[[i]])
  })
  custom_outer$instantiate(task, inner_sets, test_sets)
  return(list(custom_inner = custom_inner, custom_outer = custom_outer))
}

# generate cv's
train_sets = seq(12, 12 * 4, 12)
gap_sets = c(0:3)
mat = cbind(train = train_sets)
expanded_list  = lapply(gap_sets, function(v) {
  cbind.data.frame(mat, gap = v)
})
cv_param_grid = rbindlist(expanded_list)
cv_param_grid[ ,tune := 3]
custom_cvs = list()
for (i in 1:nrow(cv_param_grid)) {
  print(i)
  param_ = cv_param_grid[i]
  if (param_$gap == 0) {
    custom_cvs[[i]] = nested_cv_split(task_ret_week,
                                      param_$train,
                                      param_$tune,
                                      1,
                                      1,
                                      1)
    
  } else if (param_$gap == 1) {
    custom_cvs[[i]] = nested_cv_split(task_ret_month,
                                      param_$train,
                                      param_$tune,
                                      1,
                                      param_$gap,
                                      param_$gap)
    
  } else if (param_$gap == 2) {
    custom_cvs[[i]] = nested_cv_split(task_ret_month2,
                                      param_$train,
                                      param_$tune,
                                      1,
                                      param_$gap,
                                      param_$gap)
    
  } else if (param_$gap == 3) {
    custom_cvs[[i]] = nested_cv_split(task_ret_quarter,
                                      param_$train,
                                      param_$tune,
                                      1,
                                      param_$gap,
                                      param_$gap)
    
  }
}

# # visualize test
# library(ggplot2)
# library(patchwork)
# prepare_cv_plot = function(x, set = "train") {
#   x = lapply(x, function(x) data.table(ID = x))
#   x = rbindlist(x, idcol = "fold")
#   x[, fold := as.factor(fold)]
#   x[, set := as.factor(set)]
#   x[, ID := as.numeric(ID)]
# }
# plot_cv = function(cv, n = 5) {
#   cv_test_inner = cv$custom_inner
#   cv_test_outer = cv$custom_outer
# 
#   # define task
#   if (cv_test_inner$id == "taskRetQuarter") {
#     task_ = task_ret_quarter$clone()
#   } else if (cv_test_inner$id == "taskRetMonth2") {
#     task_ = task_ret_month2$clone()
#   } else if (cv_test_inner$id == "taskRetMonth") {
#     task_ = task_ret_month$clone()
#   } else if (cv_test_inner$id == "taskRetWeek") {
#     task_ = task_ret_week$clone()
#   }
# 
#   # prepare train, tune and test folds
#   train_sets = cv_test_inner$instance$train[1:n]
#   train_sets = prepare_cv_plot(train_sets)
#   tune_sets = cv_test_inner$instance$test[1:n]
#   tune_sets = prepare_cv_plot(tune_sets, set = "tune")
#   test_sets = cv_test_outer$instance$test[1:n]
#   test_sets = prepare_cv_plot(test_sets, set = "test")
#   dt_vis = rbind(train_sets, tune_sets, test_sets)
#   ggplot(dt_vis, aes(x = fold, y = ID, color = set)) +
#     geom_point() +
#     theme_minimal() +
#     coord_flip() +
#     labs(x = "", y = '', title = cv_test_inner$id)
# }
# plots = lapply(custom_cvs[c(1, 4, 7, 11)], plot_cv, n = 12)
# wrap_plots(plots)


# ADD PIPELINES -----------------------------------------------------------
print("Add pipelines")

# source pipes, filters and other
source("mlr3_winsorization.R")
source("mlr3_uniformization.R")
source("mlr3_gausscov_f1st.R")
source("mlr3_gausscov_f3st.R")
source("mlr3_dropna.R")
source("mlr3_dropnacol.R")
source("mlr3_filter_drop_corr.R")
source("mlr3_winsorizationsimple.R")
source("mlr3_winsorizationsimplegroup.R")
source("PipeOpPCAExplained.R")
# measures
source("Linex.R")
source("AdjLoss2.R")
source("PortfolioRet.R")

# add my pipes to mlr dictionary
mlr_pipeops$add("uniformization", PipeOpUniform)
mlr_pipeops$add("winsorize", PipeOpWinsorize)
mlr_pipeops$add("winsorizesimple", PipeOpWinsorizeSimple)
mlr_pipeops$add("winsorizesimplegroup", PipeOpWinsorizeSimpleGroup)
mlr_pipeops$add("dropna", PipeOpDropNA)
mlr_pipeops$add("dropnacol", PipeOpDropNACol)
mlr_pipeops$add("dropcorr", PipeOpDropCorr)
mlr_pipeops$add("pca_explained", PipeOpPCAExplained)
mlr_filters$add("gausscov_f1st", FilterGausscovF1st)
mlr_filters$add("gausscov_f3st", FilterGausscovF3st)
mlr_measures$add("linex", Linex)
mlr_measures$add("adjloss2", AdjLoss2)
mlr_measures$add("portfolio_ret", PortfolioRet)


# GRAPH V2 ----------------------------------------------------------------
print("Create graph")

# cretate learners graph node
learners_l = list(
  ranger  = lrn("regr.ranger", id = "ranger"),
  xgboost = lrn("regr.xgboost", id = "xgboost"),
  bart    = lrn("regr.bart", id = "bart"),
  nnet    = lrn("regr.nnet", id = "nnet", MaxNWts = 50000)
  # mlp     = lrn(
  #   "regr.mlp",
  #   # defining network parameters
  #   activation     = nn_relu,
  #   layers         = 2,
  #   d_hidden       = 10,
  #   # training parameters
  #   batch_size     = 16,
  #   epochs         = 50,
  #   device         = "cpu",
  #   # Defining the optimizer, loss, and callbacks
  #   optimizer      = t_opt("adam", lr = 0.1),
  #   loss           = t_loss("mse"),
  #   callbacks      = t_clbk("history"),
  #   # this saves the history in the learner
  #   # Measures to track
  #   measures_valid = msrs(c("regr.mse", "regr.mae")),
  #   measures_train = msrs(c("regr.mse")),
  #   predict_type = "response"
  # )
)

# create regression average of all learners
choices = c("ranger", "xgboost", "bart", "nnet") # , "mlp"
learners = po("branch", choices) %>>% 
  gunion(learners_l) %>>%
  po("unbranch")

# non pca ghraph
graph_nonpca = po("dropnacol", id = "dropnacol", cutoff = 0.05) %>>%
  po("dropna", id = "dropna") %>>%
  po("removeconstants", id = "removeconstants_1", ratio = 0)  %>>%
  po("winsorizesimple", id = "winsorizesimple", probs_low = 0.01, probs_high = 0.99, na.rm = TRUE) %>>%
  po("removeconstants", id = "removeconstants_2", ratio = 0)  %>>%
  po("dropcorr", id = "dropcorr", cutoff = 0.99) %>>%
  po("uniformization") %>>%
  po("dropna", id = "dropna_v2") %>>%
  learners
plot(graph_nonpca)
graph_nonpca_lrn = as_learner(graph_nonpca)

# pca ghraph
graph_pca = po("dropnacol", id = "dropnacol", cutoff = 0.05) %>>%
  po("dropna", id = "dropna") %>>%
  po("removeconstants", id = "removeconstants_1", ratio = 0)  %>>%
  po("winsorizesimple", id = "winsorizesimple", probs_low = 0.01, probs_high = 0.99, na.rm = TRUE) %>>%
  po("removeconstants", id = "removeconstants_2", ratio = 0)  %>>%
  po("dropcorr", id = "dropcorr", cutoff = 0.99) %>>%
  po("uniformization") %>>%
  po("dropna", id = "dropna_v2") %>>%
  # po("pca") %>>%
  po("pca_explained") %>>%
  learners
plot(graph_pca)
graph_pca_lrn = as_learner(graph_pca)

# threads
print("Set threads")
threads = 4
set_threads(graph_pca_lrn, n = threads)
set_threads(graph_nonpca_lrn, n = threads)

# pca params
as.data.table(graph_pca_lrn$param_set)[, .(id, class, lower, upper)]
as.data.table(graph_pca_lrn$param_set)[1:100, .(id, class, lower, upper)]
search_space = ps(
  pca_explained.var. = p_fct(levels = c("0.90", "0.95", "0.99"),
                             trafo = function(x, param_set) {
                               switch(x,
                                      "0.90" = 0.90,
                                      "0.95" = 0.95,
                                      "0.99" = 0.99)
                             }),
  # learner branch
  branch.selection = p_fct(choices)
)

# create designs
print("Create designs")
designs_l = lapply(custom_cvs, function(cv_) {
  # debug
  # cv_ = custom_cvs[[1]]
  
  # get cv inner object
  cv_inner = cv_$custom_inner
  cv_outer = cv_$custom_outer
  cat("Number of iterations fo cv inner is ", cv_inner$iters, "\n")
  
  # debuging
  if (interactive()) {
    to_ = 2
  } else {
    to_ = cv_inner$iters
  }
  
  # create desings
  designs_cv_l = lapply(1:to_, function(i) { # 1:cv_inner$iters
    # debug
    # i = 1
    
    # choose task_
    print(cv_inner$id)
    if (cv_inner$id == "taskRetWeek") {
      task_ = task_ret_week$clone()
    } else if (cv_inner$id == "taskRetMonth") {
      task_ = task_ret_month$clone()
    } else if (cv_inner$id == "taskRetMonth2") {
      task_ = task_ret_month2$clone()
    } else if (cv_inner$id == "taskRetQuarter") {
      task_ = task_ret_quarter$clone()
    }
    
    # have to create new task because of the change in the new mlr3 version
    task_inner = task_ret_week$clone()
    task_inner$filter(c(cv_inner$train_set(i), cv_inner$test_set(i)))
    
    # inner resampling
    custom_ = rsmp("custom")
    custom_$id = paste0("custom_", cv_inner$iters, "_", i)
    custom_$instantiate(task_inner,
                        list(cv_inner$train_set(i)),
                        list(cv_inner$test_set(i)))
    
    
    # auto tuner
    at_pca = auto_tuner(
      tuner = tnr("grid_search", resolution = 20, batch_size = 2),
      learner = graph_pca_lrn,
      resampling = custom_,
      measure = msr("regr.mse"),
      search_space = search_space
    )
    
    # outer resampling
    customo_ = rsmp("custom")
    customo_$id = paste0("custom_", cv_inner$iters, "_", i)
    customo_$instantiate(task_, list(cv_outer$train_set(i)), list(cv_outer$test_set(i)))
    
    # nested CV for one round
    design = benchmark_grid(
      tasks = list(task_),
      learners = list(at_pca, graph_nonpca_lrn),
      resamplings = customo_
    )
  })
  designs_cv = do.call(rbind, designs_cv_l)
  # batchmark(designs)
})
designs = do.call(rbind, designs_l)

# exp dir
if (interactive()) {
  dirname_ = "experiments_test"
  if (dir.exists(dirname_)) system(paste0("rm -r ", dirname_))
} else {
  dirname_ = "experiments"
}

# create registry
print("Create registry")
packages = c("data.table", "gausscov", "paradox", "mlr3", "mlr3pipelines",
             "mlr3tuning", "mlr3misc", "future", "future.apply",
             "mlr3extralearners")
reg = makeExperimentRegistry(file.dir = dirname_, seed = 1, packages = packages)

# populate registry with problems and algorithms to form the jobs
print("Batchmark")
batchmark(designs, reg = reg, store_models = TRUE)

# save registry
print("Save registry")
saveRegistry(reg = reg)

# create sh file
sh_file = sprintf("
#!/bin/bash

#PBS -N H2
#PBS -l ncpus=4
#PBS -l mem=15GB
#PBS -J 1-%d
#PBS -o experiments/logs
#PBS -j oe

cd ${PBS_O_WORKDIR}
apptainer run image.sif run_job.R
", nrow(designs))
sh_file_name = "padobran.sh"
file.create(sh_file_name)
writeLines(sh_file, sh_file_name)
