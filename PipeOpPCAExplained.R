PipeOpPCAExplained = R6::R6Class(
  "PipeOpPCAExplained",
  inherit = PipeOpTaskPreproc,
  public = list(
    initialize = function(id = "pca_explained", param_vals = list()) {
      ps = ParamSet$new(params = list(
        ParamLgl$new("center", default = FALSE, tags = c("train", "pca")),
        ParamLgl$new("scale.", default = FALSE, tags = c("train", "pca")),
        ParamDbl$new("var.", default = NULL, lower = 0, upper = 0.999, special_vals = list(NULL), tags = c("train"))
      ))
      ps$values = list(center = FALSE, scale. = FALSE)
      super$initialize(
        id,
        param_set = ps,
        param_vals = param_vals,
        feature_types = c("numeric", "integer")
      )
    }
  ),
  private = list(
    .train_dt = function(dt, levels, target) {
      pv = self$param_set$values
      pcr = mlr3misc::invoke(stats::prcomp,
                             as.matrix(dt),
                             .args = self$param_set$get_values(tags = "pca"))
      cumulative_proportion <- cumsum(pcr$sdev^2) / sum(pcr$sdev^2)
      n_components <- which(cumulative_proportion >= pv$var.)[1]
      pcr <- prcomp(as.matrix(dt),
                    center = pv$center,
                    rank. = n_components)
      self$state = pcr
      self$state$x = NULL
      
      # save scores
      # dir_name = "./pcr"
      # if (!dir.exists(dir_name)) {
      #   dir.create(dir_name)
      # }
      # random_id <- paste0(sample(0:9, 15, replace = TRUE), collapse = "")
      # file_name = paste0("pcr-", task$id, "-", random_id, ".rds")
      # file_name = file.path(dir_name, file_name)
      # saveRDS(pcr, file_name)
      
      # self$state$n_components = n_components
      pcr$x
    },

    .predict_dt = function(dt, levels) {
      stats::predict(self$state, as.matrix(dt))
    }
  )
)

# # test PCA explained
# task = tsk("iris")
# gr = Graph$new()
# gr$add_pipeop(PipeOpPCAExplained$new(param_vals = list(var. = 0.99)))
# result = gr$train(task)
# result[[1]]$data()
# gr$predict(task)
