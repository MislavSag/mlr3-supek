library(paradox)
library(mlr3)
library(mlr3pipelines)
library(mlr3viz)
library(mlr3tuning)
library(mlr3misc)
library(future)
library(future.apply)
library(mlr3extralearners)

# featureless baseline
lrn_baseline = lrn("classif.featureless", id = "featureless")

# logistic regression pipeline
lrn_lr = lrn("classif.log_reg")
lrn_lr = as_learner(ppl("robustify", learner = lrn_lr) %>>% lrn_lr)
lrn_lr$id = "logreg"
lrn_lr$fallback = lrn_baseline
lrn_lr$encapsulate = c(train = "try", predict = "try")

# random forest pipeline
lrn_rf = lrn("classif.ranger")
lrn_rf = as_learner(ppl("robustify", learner = lrn_rf) %>>% lrn_rf)
lrn_rf$id = "ranger"
lrn_rf$fallback = lrn_baseline
lrn_rf$encapsulate = c(train = "try", predict = "try")

learners = list(lrn_lr, lrn_rf, lrn_baseline)

design = benchmark_grid(tsks(c("german_credit", "sonar", "pima")),
                        learners, rsmp("cv", folds = 10))
bmr = benchmark(design)
bmr$aggregate(msr("classif.acc"))[, .(task_id, learner_id, classif.acc)]


library(mlr3oml)


binary_cc18 = list_oml_tasks(
  limit = 6,
  task_id = otask_collection$task_ids,
  number_classes = 2
)

# load tasks as a list
otasks = lapply(binary_cc18$task_id, otsk)

# convert to mlr3 tasks and resamplings
tasks = as_tasks(otasks)
resamplings = as_resamplings(otasks)

large_design = benchmark_grid(tasks, learners, resamplings,
                              paired = TRUE)
large_design[1:6] # first 6 rows


library(batchtools)

# create registry
reg = makeExperimentRegistry(
  file.dir = "./experiments",
  seed = 1,
  packages = "mlr3verse"
)

library(mlr3batchmark)
batchmark(large_design, reg = reg)

reg

job_table = getJobTable(reg = reg)
job_table = unwrap(job_table)
job_table = job_table[,
                      .(job.id, learner_id, task_id, resampling_id, repl)
]

result = testJob(1, external = TRUE, reg = reg)

cf = makeClusterFunctionsSocket(ncpus=2)
reg$cluster.functions = cf
saveRegistry(reg = reg)

ids = job_table$job.id
chunks = data.table(
  job.id = ids, chunk = chunk(ids, chunk.size = 5, shuffle = FALSE)
)
chunks[1:6] # first 6 jobs

resources = list(ncpus = 1, walltime = 3600, memory = 8000)

submitJobs(ids = chunks, resources = resources, reg = reg)

waitForJobs(reg = reg)

getStatus(reg = reg)
waitForJobs(reg = reg)

bmr = reduceResultsBatchmark(1:3, reg = reg)
bmr$aggregate()
