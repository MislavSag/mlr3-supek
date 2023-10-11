library(data.table)
library(gausscov)
library(paradox)
library(mlr3)
library(mlr3pipelines)
library(mlr3viz)
library(mlr3tuning)
library(mlr3misc)
library(future)
library(future.apply)
library(mlr3extralearners)
library(batchtools)
library(mlr3batchmark)
library(checkmate)
library(stringi)
library(fs)


# utils
dir = function(reg, what) {
  fs::path(fs::path_expand(reg$file.dir), what)
}
getResultFiles = function(reg, ids) {
  fs::path(dir(reg, "results"), sprintf("%i.rds", if (is.atomic(ids)) ids else ids$job.id))
}
waitForFile = function(fn, timeout = 0, must.work = TRUE) {
  if (timeout == 0 || fs::file_exists(fn)) 
    return(TRUE)
  "!DEBUG [waitForFile]: `fn` not found via 'file.exists()'"
  timeout = timeout + Sys.time()
  path = fs::path_dir(fn)
  repeat {
    Sys.sleep(0.5)
    if (basename(fn) %chin% list.files(path, all.files = TRUE)) 
      return(TRUE)
    if (Sys.time() > timeout) {
      if (must.work) 
        stopf("Timeout while waiting for file '%s'", 
              fn)
      return(FALSE)
    }
  }
}
writeRDS = function (object, file, compress = "gzip") {
  batchtools:::file_remove(file)
  saveRDS(object, file = file, version = 2L, compress = compress)
  waitForFile(file, 300)
  invisible(TRUE)
}

# load registry
reg = loadRegistry("experiments")

# create job collection
resources = list(ncpus = 6) # this shouldnt be important
jc = makeJobCollection(resources = resources, reg = reg)

# extract integer
i = as.integer(Sys.getenv('PBS_ARRAY_INDEX'))
# i = 10

# get job
job = batchtools:::getJob(jc, i)
id = job$id

# execute job
result = execJob(job)

# save ojb
writeRDS(result, file = getResultFiles(jc, id), compress = jc$compress)
