library(data.table)
library(mlr3batchmark)
library(mlr3)
library(mlr3pipelines)
library(mlr3viz)
library(mlr3tuning)
library(mlr3misc)

library(batchtools)


PATH = "F:/H2"
reg = loadRegistry(PATH, work.dir=PATH)

# add mannually
reg$status$done = batchtools:::ustamp()

bmrs = reduceResultsBatchmark(ids = 1:4, store_backends = TRUE, reg = reg)

bmrs$aggregate()
