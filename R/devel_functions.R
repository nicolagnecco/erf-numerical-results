# !!! if prior works, add this as a property to the object `erf`.
fit_erf_cv_devel <- function(X,
                       Y,
                       min.node.size = c(5, 40, 100),
                       lambda = c(0, 0.001, 0.01),
                       intermediate_estimator = c("grf", "neural_nets"),
                       intermediate_quantile = 0.8,
                       nfolds = 5, nreps = 3, seed = NULL, prior = NULL){
  
  # fit intermediate quantile estimator
  intermediate_threshold <- erf:::fit_intermediate_threshold(
    X, Y,
    intermediate_estimator)
  
  # predict intermediate_threshold Q_X
  Q_X <- erf:::predict_intermediate_quantile(
    intermediate_threshold = intermediate_threshold,
    intermediate_quantile = intermediate_quantile
  )
  
  # create splits
  splits <- erf:::get_repeated_k_folds(nrow(X), nfolds, nreps, seed)
  
  # create parameter grid
  params <- erf:::get_param_grid(min.node.size, lambda)
  
  # create full grid with parameters and splits
  full_grid <- tidyr::crossing(splits, params)
  fun_args <- full_grid %>%
    dplyr::select(- .data$rep_id, - .data$fold_id)
  
  # partialise fit_and_score_erf_lw
  fit_and_score_partial <- purrr::partial(
    fit_and_score_erf_lw_devel,
    X = X, Y = Y,
    Q_X = Q_X,
    intermediate_quantile = intermediate_quantile,
    prior = prior
  )
  
  # sweep through folds and parameters
  scores <- full_grid %>%
    dplyr::mutate(cv_err = purrr::pmap_dbl(fun_args, fit_and_score_partial))
  
  # compute optimal parameters
  opt_params <- scores %>%
    dplyr::group_by(.data$min.node.size, .data$lambda) %>%
    dplyr::summarise(cvm = mean(cv_err)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(.data$cvm == min(.data$cvm))
  
  # refit `erf` on full dataset
  fit.erf <- erf:::fit_erf(X, Y, opt_params$min.node.size, opt_params$lambda,
                     intermediate_estimator, intermediate_quantile)
  
  # return `erf_cv`
  structure(
    list(scores = scores, fit.erf = fit.erf),
    class = "erf_cv"
  )
  
}

predict.erf_devel <- function(object,
                        newdata = NULL,
                        quantiles = c(0.95, 0.99),
                        prior,
                        ...) {
  
  # predict intermediate quantile
  # !!! if newdata is not null
  Q_x <- erf:::predict_intermediate_quantile(
    intermediate_threshold = object$intermediate_threshold,
    newdata = newdata,
    intermediate_quantile = object$intermediate_quantile
  )
  
  # compute optimal GPD parameters
  gpd_pars <- predict_gpd_params_devel(
    object,
    newdata,
    prior
  )
  
  # predict quantiles
  erf:::predict_extreme_quantiles(
    gpd_pars,
    Q_x,
    quantiles,
    object$intermediate_quantile
  )
}

fit_and_score_erf_lw_devel <- function(X, Y, Q_X, folds,
                                 min.node.size, lambda, intermediate_quantile,
                                 prior){
  ## numeric_matrix numeric_vector numeric_vector numeric (4x) -> numeric
  ## fit a light-weight erf and computes its cross validation error
  
  # split X, Y, Q_X
  X <- erf:::split_data(X, folds)
  Y <- erf:::split_data(Y, folds)
  Q_X <- erf:::split_data(Q_X, folds)
  
  # fit light-weight erf
  fitted_erf_lw <- erf:::fit_mini_erf_lw(X$train, Y$train, Q_X$train,
                                         min.node.size, lambda, intermediate_quantile)
  
  # keep only test observations where Y > Q_X
  exc_id <- Y$test > Q_X$test
  X$test <- X$test[exc_id, , drop = FALSE]
  Y$test <- Y$test[exc_id]
  Q_X$test <- Q_X$test[exc_id]
  
  # predict gpd parameters
  gpd_params <- predict_gpd_params_devel(fitted_erf_lw, X$test, prior)
  
  # evaluate deviance
  if (length(Y$test) == 0){
    return(10 ^ 6) # !!! ask Seb
  } else {
    erf:::evaluate_deviance(gpd_params, Y$test, Q_X$test)
  }
}


predict_gpd_params_devel <- function(object, newdata, prior) {
  ## erf|erf_lw numeric_matrix|NULL numeric -> tibble
  ## produce a tibble with MLE GPD scale (sigma) and shape (xi) parameter
  ## for each test point;
  ## each row corresponds to a test point, each column to a GPD parameter,
  ## namely, `sigma` and `xi`
  
  # check size for similarity weights
  # !!!
  
  # extract response and intermediate quantile vector
  Y <- object$quantile_forest$Y.orig
  Q_X <- object$Q_X
  
  # compute similarity weights
  W <- as.matrix(grf::get_sample_weights(
    forest = object$quantile_forest,
    newdata = newdata
  ))
  
  # compute optimal GPD parameters
  predict_gpd_params_helper_devel(W, Y, Q_X, object$lambda, prior)
  
}


predict_gpd_params_helper_devel <- function(W, Y, Q, lambda, prior) {
  ## numeric_matrix numeric_vector numeric_vector numeric numeric -> tibble
  ## produce a tibble with MLE GPD scale (sigma) and shape (xi) parameter
  ## for each test point;
  ## each row corresponds to a test point, each column to a GPD parameter,
  ## namely, `sigma` and `xi`
  
  # compute exceedances
  exc_ind <- which(Y > Q)
  Z <- (Y - Q)[exc_ind]
  W <- W[, exc_ind]
  ntest <- nrow(W)
  
  # initial guess for GPD parameters
  init_pars <- ismev::gpd.fit(Z, 0, show = FALSE)$mle
  
  # set prior
  if (is.null(prior)) {
    prior <- init_pars[2]
  } else {
    prior <- prior
  }
  
  # GPD parameters for each test observation
  tibble::tibble(
    "sigma" = numeric(0),
    "xi" = numeric(0)
  ) %>%
    dplyr::bind_rows(
      purrr::map_dfr(seq_len(ntest), function(i) {
        wi_x <- W[i, ]
        erf:::optim_wrapper(wi_x, init_pars, Z, lambda, prior)
      })
    )
  
}

compute_prior <- function(model, df, distr, X) {
  ## character numeric character numeric_matrix -> numeric
  ## compute prior
  
  if (distr == "gaussian") {
    
    return(1e-6)
    
  } else {
    
    if (grepl("_tanh", model)) {
      
      return(1 / mean(df_tanh(X, df)))
      
    } else if (grepl("_sigmoid", model)) {
      
      return(1 / mean(df_sigmoid(X, df)))
      
    } else if (grepl("_", model)) {
      
      stop(glue::glue("cannot compute prior for model `{model}`"),
           call. = FALSE)
      
    } else {
      
      return(1 / df)
      
    }
  }
}
