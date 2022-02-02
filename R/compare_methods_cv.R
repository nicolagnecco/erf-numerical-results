#' Compare methods vs cross-validated ERF
#'
#' Compare performance of cross-validated ERF against
#' GAM, GRF, Meinshausen quantile forest (2006),
#' unconditional GPD estimate over GRF threshold, 
#' full unconditional GPD estimate, and ground truth.
#'
#' @param n (numeric): number of training observations
#'
#' @param p (numeric): number of predictors
#'
#' @param ntest (numeric): number of testing observations.
#'
#' @param model (character): generative model. Default is `step`.
#'
#' @param distr (character): distribution of the noise variables. Default is
#'  `gaussian`.
#'
#' @param df (numeric): number of degrees of freedom if
#'  `distr = "student_t"`.
#'
#' @param test_data (character): method to generate testing observations. One of
#'  `halton`, `beta`.
#'  
#' @param alpha_beta (numeric): parameter from Beta distribution.
#'  When `alpha_beta = 1`, it corresponds to Uniform distribution.
#'  Default is `alpha_beta = 1`.
#'  
#'  @param methods (numeric_vector): competitor methods among `"truth"`,
#'   `"grf"`, `"meins"`, `"unc_gpd"`, `"full_unc"`, `"evgam"`.
#'  
#' @param min.node.size (numeric_vector): target for the minimum number of
#'  observations in each tree leaf. 
#'  Default is `min.node.size = c(5)`.
#'  
#' @param lambda (numeric_vector): vector of penalties for the GPD likelihood
#'  of `erf`.
#'  Default is `lambda = c(0)`.
#'  
#' @param nfolds (numeric): number of folds in cross-validation.
#'  Default is `nfolds = 5`.
#'  
#' @param nreps (numeric): number of repetitions in cross-validation.
#'  Default is `nreps = 3`.
#'  
#' @param taillardat_method (character): one of `grf` or `meins` to fit method
#' of Taillardat et al. (2018).
#' Default is `grf`.
#' 
#' @param taillardat_remove_negatives (boolean): whether to keep only
#' observations corresponding to positive response when fitting the method
#' of Taillardat et al. (2018).
#' Default is `FALSE`.
#' 
#' @param gbex_cv (boolean): whether to cross validate tuning parameter `B` for
#' the `gbex` method of Velthoen et al. (2021).
#'  
#' @param quantiles (numeric_vector): vector of quantile levels to predict.
#'  Default is `quantiles = c(0.95)`.
#'
#' @param intermediate_quantile (numeric): quantile level for the intermediate
#'  threshold.
#'  Default is `intermediate_quantile = 0.8`.
#'  
#' @param shape_is_known (logical): whether the shape parameter of the GPD 
#'  distribution is known. If `shape_is_known = TRUE`, the true shape parameter
#'  is used as prior in the penalized weighted likelihood.
#'  
#' @param return_value (character): whether to return 
#'  `ise`, `predicted_quantiles`, `cv`, or `quantile_loss`.
#'
#'
#' @return
#'  * If `return_value = "ise"`, `tibble` with columns
#'    `method`, `quantile`, and `ise`.
#'
#'  * If `return_value = "predicted_quantiles"`, `tibble` with columns
#'   `method`, `X1`, ..., `Xp`, `quantile`, `predicted_quantiles`.
#'   
#'   *  If `return_value = "cv"`, `tibble` with columns
#'   `method`, `quantile`, `best.min.node.size`, `best.lambda`, and `ise`.
#'   
#'   * If `return_value = "quantile_loss"`, `tibble` with columns
#'    `method`, `quantile`, and `quantile_loss`.
#'
compare_methods_cv <- function(n, p, ntest,
                          model = MODELS,
                          distr = DISTR, df = 4,
                          test_data = c("halton", "beta"),
                          alpha_beta = 1,
                          methods = METHODS,
                          min.node.size = c(5), lambda = c(0),
                          nfolds = 5, nreps = 3,
                          taillardat_method = c("grf", "meins"),
                          taillardat_remove_negatives = FALSE,
                          gbex_cv = FALSE,
                          quantiles = c(0.95),
                          intermediate_quantile = 0.8,
                          shape_is_known = FALSE,
                          return_value = c("predicted_quantiles", "ise", "cv", "quantile_loss")) {
  # check arguments
  model <- match.arg(model)
  distr <- match.arg(distr)
  test_data <- match.arg(test_data)
  return_value <- match.arg(return_value)
  methods <- match.arg(methods, several.ok = TRUE)
  evgam_model_shape <- grepl(".+_.+", model)
  taillardat_method <- match.arg(taillardat_method)

  # generate training data
  dat <- generate_joint_distribution(
    n = n, p = p, model = model,
    distr = distr, df = df
  )
  
  X <- dat$X
  Y <- dat$Y

  # generate test data
  X_test <- generate_test_data(ntest, p, test_data, alpha_beta)
  Y_test <- generate_conditional_distribution(model, distr, df, X_test)$Y

  # fit-predict methods
  if (!shape_is_known) {
    fit_erf <- erf::erf_cv(
      X = X, Y = Y, 
      min.node.size = min.node.size, lambda = lambda,
      intermediate_estimator = "grf", 
      intermediate_quantile = intermediate_quantile,
      nfolds = nfolds, nreps = nreps
    )$fit.erf
    
    pred_erf <- predict(
      object = fit_erf,
      newdata = X_test,
      quantiles = quantiles
    )
    
  } else {
    prior <- compute_prior(model, df, distr, X_test)
    
    fit_erf <- fit_erf_cv_devel(
      X = X, Y = Y, 
      min.node.size = min.node.size, lambda = lambda,
      intermediate_estimator = "grf", 
      intermediate_quantile = intermediate_quantile,
      nfolds = nfolds, nreps = nreps, prior = prior
    )$fit.erf
    
    pred_erf <- predict.erf_devel(
      object = fit_erf,
      newdata = X_test,
      quantiles = quantiles,
      prior = prior
    )
  }

  other_methods <- purrr::set_names(methods) %>% 
    map(fit_predict_methods, 
        X = X, Y = Y, Q_X = fit_erf$Q_X, 
        intermediate_threshold = fit_erf$intermediate_threshold, 
        intermediate_quantile = intermediate_quantile, 
        model = model, distr = distr, df = df, newdata = X_test,
        quantiles = quantiles,
        evgam_model_shape = evgam_model_shape,
        taillardat_method = taillardat_method, 
        taillardat_remove_negatives = taillardat_remove_negatives,
        gbex_cv = gbex_cv
    )

  # collect results
  ll <- list()
  ll$data <- list(X_train = dat$X, Y_train = dat$Y, 
                  X_test = X_test, Y_test = Y_test)
  ll$predict <- c(list(erf = pred_erf), other_methods)
  ll$quantiles <- quantiles
  ll$best.min.node.size <- as.numeric(fit_erf$min.node.size)
  ll$best.lambda <- as.numeric(fit_erf$lambda)

  # return results
  return_method_results(ll, return_value = return_value)
}
