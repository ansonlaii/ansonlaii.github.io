

logistic_lasso <- function(mode = "classification", penalty){
  
  ## This function specifies the mode that the model is working in and in this 
  ## case it is classification. It also specifies the argument penalty that is an
  ## argument that is not evaluated immediately since we need to tune it.
  ##
  ## Input:
  ## - mode: The mode for this model is classification
  ##
  ## - penalty: The penalty for the logistic lasso
  ##
  ## Output:
  ## - It contains all the arguments that we will need to pass to the fit function which is
  ## fit_logistic_lasso. It also specifies which model we are working with.
  ##
  ## Example:
  ##
  ## spec <- logistic_lasso(penalty = 0.3) %>% set_engine("fit_logistic_lasso")
  ##
  ## fit <- workflow() %>% add_recipe(dat) %>% add_model(spec) %>% fit(train)
  ##
  
  args <- list(penalty = rlang::enquo(penalty))
  new_model_spec("logistic_lasso", 
                 args = args,
                 mode = mode,
                 eng_args = NULL,
                 method = NULL,
                 engine = NULL)
}

set_new_model("logistic_lasso")
set_model_mode(model = "logistic_lasso", mode = "classification")
set_model_engine("logistic_lasso", mode = "classification", eng = "fit_logistic_lasso")
set_dependency("logistic_lasso", eng = "fit_logistic_lasso", pkg = "base")

set_model_arg(
  model = "logistic_lasso",
  eng = "fit_logistic_lasso",
  parsnip = "penalty",
  original = "lambda", 
  func = list(pkg = "dials", fun = "penalty"),
  has_submodel = FALSE
  )

set_encoding(
  model = "logistic_lasso",
  eng = "fit_logistic_lasso",
  mode = "classification",
  options = list(
    predictor_indicators = "traditional",
    compute_intercept = TRUE,
    remove_intercept = TRUE,
    allow_sparse_x = FALSE
  )
             )

set_fit(
  model = "logistic_lasso",
  eng = "fit_logistic_lasso",
  mode = "classification",
  value = list(
    interface = "matrix",
    protect = c("x", "y"),
    func = c(fun  = "fit_logistic_lasso"),
    defaults =list()
  )
)

set_pred(
  model = "logistic_lasso",
  eng = "fit_logistic_lasso",
  mode = "classification",
  type = "class",
  value =list(
    pre = NULL,
    post = NULL,
    func = c(fun = "predict_logistic_lasso"),
    args = list(
      fit = expr(object$fit),
      new_x = expr(as.matrix(new_data[,names(object$fit$beta)])))
  ))


update.logistic_lasso <- function(object, penalty = NULL, ...){

  ## Finalizing the workflow will not work since we registered our model but 
  ## we don't have a version of update that works for our model so this function
  ## creates a new spec that has the final parameter.
  ##
  ## Input:
  ## - object: The outcome of the fit procedure
  ##
  ## - penalty: The penalty for the logistic lasso
  ##
  ## Output:
  ## - It creates a new model with the new specification of penalty since we updated the
  ##   penalty through cross validation.
  ##
  ## Example:
  ## wf_final <- wf %>% finalize_workflow(penalty_final)
  ##
  ## This above would not have worked if we didn't define this new function.
  ##
  
  if(!is.null(penalty)) {
    object$args <- list(penalty = enquo(penalty))
  }
  
  new_model_spec("logistic_lasso", args = object$args, eng_args = NULL,
                 mode = "classification", method = NULL, engine = object$engine)
  
}

