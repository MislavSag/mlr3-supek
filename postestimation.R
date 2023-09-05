library(data.table)
library(mlr3)
library(mlr3viz)
library(AzureStor)
library(ggplot2)



# azure creds
# blob_key = Sys.getenv("KEY")
# endpoint = Sys.getenv("ENDPOINT")
# BLOBENDPOINT = storage_endpoint(endpoint, key=blob_key)
mlr3_save_path = "F:/H2"

# benchmark files
files_ = list.files(mlr3_save_path, full.names = TRUE)
files_info = file.info(files_)
files_info = files_info[order(files_info$ctime), ]

# import all benchmarks
bmr = readRDS(rownames(files_info)[1])
# Reduce(function(x) bmrs$combine(x), lapply(rownames(files_info)[2:3], readRDS)) # TO IMPORT MULTIPLE
bmr_dt = as.data.table(bmrs)

# bmrs <- Reduce(function(x, y) x$combine(y), lapply(rownames(files_info)[1:3], readRDS)) # TO IMPORT MULTIPLE

# vector of id coluimns
id_cols = c("symbol", "date", "yearmonthid", "..row_id")

# help objects
task_names = lapply(bmr_dt$task, `[[`, "id")

# get backends
backs = lapply(bmr_dt$task, function(task) {
  cbind(task$backend$data(cols = c(id_cols, "eps_diff", "nincr", "nincr_2y", "nincr_3y"),
                    rows = task$row_ids),
        task_name = task$id)
})
lapply(backs, setnames, "..row_id", "row_ids")
lapply(backs, function(dt) dt[, yearmonthid := as.Date(yearmonthid, origin = "1970-01-01")])

# get predictions
predictions = lapply(bmr_dt$prediction, function(x) as.data.table(x))
names(predictions) <- task_names

# merge backs and predictions
predictions <- lapply(seq_along(predictions), function(i) {
  y = backs[[i]][predictions[[i]], on = "row_ids"]
  # y[, date := as.Date(date, origin = "1970-01-01")]
  cbind(task_name = names(backs)[i], y)
})
predictions = rbindlist(predictions)
setorder(predictions, task_name, yearmonthid)

# aggregated results
aggregate = bmr$aggregate(msrs(c("regr.mse", "regr.mae")))

# extract learner results
bmr_dt$learner[[1]]$archive
bmr_dt$learner[[1]]$archive$
bmr_dt$learner[[1]]$archive$benchmark_result$aggregate(msrs(c("regr.mse", "regr.mae")))

# number of components
bmr_dt$learner[[1]]$state$model$learner$state$model$pca_explained
bmr_dt$learner[[3]]$state$model$learner$state$model$pca_explained
ncol(bmr_dt$learner[[3]]$state$model$learner$state$model$pca_explained$rotation)
bmr_dt$learner[[2]]$state$model$learner$state$model$pca_explained # NO PCA

# 

