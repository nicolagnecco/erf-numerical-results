fit_gbex <- function(X, Y, Q_X,
                     intermediate_threshold,
                     intermediate_quantile = 0.8,
                     B = 100,
                     lambda_scale = 0.01, lambda_ratio = 7,
                     depth_trees = c(2, 2),
                     cv = TRUE) {
  
  ## numeric_matrix numeric_vector numeric_vector quantile_forest
  ##  numeric numeric numeric numeric numeric_vector boolean
  ##  -> gbex
  ## fits a GBEX model
  
  # consider only exceedances
  Z <- Y - Q_X
  XX <- as_tibble(X, .name_repair = ~ paste0("X", seq_len(ncol(X))))
  
  # fit gbex
  if (cv) {
    gbex_cv <- CV_gbex(Z[Z > 0], XX[Z > 0, ],
                       num_folds = 5, Bmax = 500,
                       lambda_scale = lambda_scale, lambda_ratio = lambda_ratio,
                       depth = depth_trees
    )
    B <- gbex_cv$par_CV
  }
  
  
  gbex.fit <- gbex(Z[Z > 0], XX[Z > 0, ],
                   B = B,
                   lambda_scale = lambda_scale, lambda_ratio = lambda_ratio,
                   depth = depth_trees
  )
  
  # save data
  data <- bind_cols(
    as_tibble(X, .name_repair = ~ paste0("X", seq_len(ncol(X)))),
    Y = Y
  )
  
  # return object
  structure(list(
    "gbex.fit" = gbex.fit,
    "intermediate_threshold" = intermediate_threshold,
    "intermediate_quantile" = intermediate_quantile,
    "Q_X" = Q_X,
    "data" = data
  ),
  class = "gbex"
  )
}


predict_gbex <- function(object, newdata = NULL,
                         quantiles = c(0.95, 0.99)) {
  ## gbex numeric_matrix numeric_vector -> numeric_matrix
  ## predicts extreme quantiles using gbex
  
  # predict intermediate quantile
  if (!is.null(newdata)) {
    Q_x <- erf:::predict_intermediate_quantile(
      intermediate_threshold = object$intermediate_threshold,
      newdata = newdata,
      intermediate_quantile = object$intermediate_quantile
    )
    newdata <- as_tibble(newdata, 
                         .name_repair = ~ paste0("X", seq_len(ncol(newdata))))
  } else {
    Q_x <- object$Q_X
    newdata <- object$data %>% select(-Y)
  }
  
  
  # compute optimal GPD parameters
  gbex_pars <- predict(object$gbex.fit, newdata = newdata)
  
  
  # predict quantiles
  erf:::predict_extreme_quantiles(
    gbex_pars,
    Q_x,
    quantiles,
    object$intermediate_quantile
  )
}


fit_extreme_gam <- function(X, Y, Q_X,
                            intermediate_threshold,
                            intermediate_quantile = 0.8,
                            evgam_model_shape = FALSE) {
  ## numeric_matrix numeric_vector numeric_vector quantile_forest
  ##  numeric boolean
  ##  -> extreme_gam
  ## fit an extreme GAM
  
  # reformat data
  p <- ncol(X)
  
  data <- bind_cols(
    as_tibble(X, .name_repair = ~ paste0("X", 1:p)),
    Y = Y
  )
  
  
  # Prepare formula for evgam
  data$excess <- data$Y - Q_X
  data$excess[data$excess <= 0] <- NA
  
  fmla_gpd <- list(
    sfsmisc::wrapFormula(
      excess ~ .,
      data %>% select(-Y, -excess)
    ),
    if (evgam_model_shape) {
      sfsmisc::wrapFormula(
        excess ~ .,
        data %>% select(-Y, -excess)
      )
    } else {
      excess ~ 1
    }
  )
  
  # return object
  structure(list(
    "evgam" = evgam::evgam(fmla_gpd, data, family = "gpd"),
    "intermediate_threshold" = intermediate_threshold,
    "intermediate_quantile" = intermediate_quantile,
    "Q_X" = Q_X,
    data = data
  ),
  class = "extreme_gam"
  )
}


predict_extreme_gam <- function(object,
                                newdata = NULL,
                                quantiles = c(0.95, 0.99)) {
  ## extreme_gam numeric_matrix numeric_vector -> numeric_matrix
  ## predicts extreme quantiles using evgam
  
  # validate and reformat newdata
  if (!is.null(newdata)) {
    p <- ncol(newdata)
    data_test <- as_tibble(
      newdata,
      .name_repair = ~ paste0("X", 1:p)
    )
  } else {
    data_test <- object$data
  }
  
  
  # predict intermediate quantile
  if (!is.null(newdata)) {
    Q_x <- erf:::predict_intermediate_quantile(
      intermediate_threshold = object$intermediate_threshold,
      newdata = newdata,
      intermediate_quantile = object$intermediate_quantile
    )
  } else {
    Q_x <- object$Q_X
  }
  
  
  # compute optimal GPD parameters
  gam_pars <- predict(object$evgam, data_test)
  gam_pars[, 1] <- exp(gam_pars[, 1])
  
  # write is as tibble
  gam_pars <- tibble::tibble(
    "sigma" = gam_pars[, 1],
    "xi" = gam_pars[, 2]
  )
  
  
  # predict quantiles
  erf:::predict_extreme_quantiles(
    gam_pars,
    Q_x,
    quantiles,
    object$intermediate_quantile
  )
}


fit_taillardat <- function(X, Y,
                           intermediate_quantile = 0.8,
                           th0 = 0.05,
                           th1 = 3,
                           decay = FALSE,
                           remove_negatives = FALSE,
                           method = c("grf", "meins")) {
  ## numeric_matrix numeric_vector numeric
  ##  -> taillardat_qrf
  ## fit a QRF with the method proposed by taillardat et al.
  
  # validate arguments
  method <- match.arg(method)
  
  # check whether to remove negatives
  if (remove_negatives) {
    Y_cens <- Y[Y >= 0]
    X_cens <- X[Y >= 0, ]
  } else {
    Y_cens <- Y
    X_cens <- X
  }
  
  # fit quantile forest
  if (method == "grf") {
    forest <- quantile_forest(
      X_cens, Y_cens,
      regression.splitting = FALSE
    )
  } else {
    forest <- quantile_forest(
      X_cens, Y_cens,
      regression.splitting = TRUE
    )
  }
  
  # return object
  structure(list(
    "quantile_forest" = forest,
    "th0" = th0,
    "th1" = th1,
    "decay" = decay
  ),
  class = "taillardat_qrf"
  )
}


predict_taillardat <- function(object,
                               newdata = NULL,
                               quantiles = c(0.95, 0.99)) {
  ## taillardat_qrf numeric_matrix numeric_vector -> numeric_matrix
  ## predicts extreme quantiles using QRF from Taillardat et al. (2018)
  
  # set up constants
  n_quantiles <- 500
  
  # set up parameters
  th0 <- object$th0
  th1 <- object$th1
  decay <- object$decay
  qqtil <- quantiles
  forest <- object$quantile_forest
  
  # predict quantiles
  prev_init <- predict(
    forest,
    newdata,
    seq_len(n_quantiles) / (n_quantiles + 1)
  )[[1]]
  
  # prev_init is a matrix where
  # each row: contains a test predictor value
  # each column: contains a quantile level
  prev_tmp <- prev_init
  prev_final <- matrix(nrow = nrow(prev_init), ncol = length(qqtil))
  
  for (i in seq_len(nrow(prev_init))) {
    # select predictions for test predictor value x_i for all quantiles
    qr_ini <- prev_init[i, ]
    qr0 <- qr_ini[qr_ini >= th0]
    qr1 <- qr_ini[qr_ini > th1]
    
    nm <- length(qr_ini)
    
    # We activate GP tail iff at least 20% of the qrf distrib. is > th0
    # && 5% > th1 (to avoid to fit GP over full 0 and/or small values).
    if ((length(qr0) >= 0.2 * n_quantiles) &
        (length(qr1) >= 0.05 * n_quantiles)) {
      
      # Use of the mev function fit.extgp : code of Naveau et al.
      # has been put in this package.
      # Note : I do not use this function in the operational forecast chain :
      # The fit.extgp function uses gmm and allow a upper set of
      # param = (100,infty,0.99). I changed this upper bound to (5,8,.249)
      # for hourly rainfall in France.
      
      para <- fit.extgp(qr0,
                        model = 1, method = "pwm",
                        plots = FALSE,
                        init = c(0.8, 1, 0.05)
      )$fit$pwm
      
      prev_tmp[i, (nm - length(qr1) + 1):nm] <- qextgp(
        p = seq(1 - length(qr1) / (nm + 1), nm / (nm + 1),
                length.out = length(qr1)
        ),
        kappa = para[1],
        sigma = para[2],
        xi = para[3],
        type = 1
      )
      
      
      if (!decay) {
        dif <- qr_ini - prev_tmp[i, ]
        sus <- which(dif > 0)
        prev_tmp[i, sus] <- qr_ini[sus]
      }
      prev_final[i, ] <- quantile(prev_tmp[i, ],
                                  probs = qqtil, na.rm = TRUE,
                                  type = 6, names = FALSE
      )
    } else {
      prev_final[i, ] <- quantile(prev_tmp[i, ],
                                  probs = qqtil, na.rm = TRUE,
                                  type = 6, names = FALSE
      )
    }
  }
  
  # We chose type=6 and not 7 in quantile because values in prev_tmp
  # are already quantiles.
  
  return(prev_final)
}


fit_unconditional_gpd <- function(X, Y, Q_X,
                                  intermediate_threshold,
                                  intermediate_quantile = 0.8) {
  ## numeric_matrix numeric_vector numeric_vector quantile_forest numeric
  ##  -> unconditional_gpd
  ## fit an unconditional GPD model over *conditional* threshold
  ## (i.e., `intermediate_threshold`)
  
  # fit GPD params with MLE
  pars <- ismev::gpd.fit(Y, Q_X, show = FALSE)$mle
  sigma <- pars[1]
  xi <- pars[2]
  
  # return object
  structure(list(
    "sigma" = sigma,
    "xi" = xi,
    "intermediate_threshold" = intermediate_threshold,
    "intermediate_quantile" = intermediate_quantile,
    "Q_X" = Q_X
  ),
  class = "unconditional_gpd"
  )
}


predict_unconditional_gpd <- function(object,
                                      newdata = NULL,
                                      quantiles = c(0.95, 0.99)) {
  ## unconditional_gpd numeric_matrix numeric_vector -> numeric_matrix
  ## predicts extreme quantiles using unconditional GPD estimates over
  ## *conditional* threshold
  
  # predict intermediate quantile
  if (!is.null(newdata)) {
    Q_x <- erf:::predict_intermediate_quantile(
      intermediate_threshold = object$intermediate_threshold,
      newdata = newdata,
      intermediate_quantile = object$intermediate_quantile
    )
  } else {
    Q_x <- object$Q_X
  }
  
  
  # compute optimal GPD parameters
  gpd_pars <- tibble::tibble(
    "sigma" = rep(object$sigma, length(Q_x)),
    "xi" = rep(object$xi, length(Q_x))
  )
  
  
  # predict quantiles
  erf:::predict_extreme_quantiles(
    gpd_pars,
    Q_x,
    quantiles,
    object$intermediate_quantile
  )
}


fit_full_unconditional <- function(X, Y, Q_X,
                                   intermediate_threshold,
                                   intermediate_quantile = 0.8) {
  ## numeric_matrix numeric_vector numeric_vector quantile_forest numeric
  ##  -> full_unconditional
  ## fit an unconditional GPD model over *unconditional* threshold
  
  # redefine *unconditional* threshold
  Q_X <- quantile(Y, intermediate_quantile) %>%
    rep(length(Y)) %>%
    unname()
  
  # fit GPD params with MLE
  pars <- ismev::gpd.fit(Y, Q_X, show = FALSE)$mle
  sigma <- pars[1]
  xi <- pars[2]
  
  # return object
  structure(list(
    "sigma" = sigma,
    "xi" = xi,
    "intermediate_threshold" = intermediate_threshold,
    "intermediate_quantile" = intermediate_quantile,
    "Q_X" = Q_X
  ),
  class = "full_unconditional"
  )
}


predict_full_unconditional <- function(object,
                                       newdata = NULL,
                                       quantiles = c(0.95, 0.99)) {
  ## full_unconditional numeric_matrix numeric_vector -> numeric_matrix
  ## predicts extreme quantiles using unconditional GPD estimates over
  ## *unconditional* threshold
  
  # predict intermediate quantile
  if (!is.null(newdata)) {
    Q_x <- rep(object$Q_X[1], nrow(newdata))
  } else {
    Q_x <- object$Q_X
  }
  
  
  # compute optimal GPD parameters
  gpd_pars <- tibble::tibble(
    "sigma" = rep(object$sigma, length(Q_x)),
    "xi" = rep(object$xi, length(Q_x))
  )
  
  
  # predict quantiles
  erf:::predict_extreme_quantiles(
    gpd_pars,
    Q_x,
    quantiles,
    object$intermediate_quantile
  )
}


fit_ground_truth <- function(X, Y, Q_X,
                             intermediate_threshold,
                             intermediate_quantile = 0.8,
                             model, distr, df) {
  ## numeric_matrix numeric_vector numeric_vector quantile_forest numeric
  ##  character (2x) numeric -> ground_truth
  ## fit ground model
  structure(
    list(
      "model" = model,
      "distr" = distr,
      "df" = df,
      "intermediate_threshold" = intermediate_threshold,
      "intermediate_quantile" = intermediate_quantile,
      "Q_X" = Q_X
    ),
    class = "ground_truth"
  )
}


predict_ground_truth <- function(object,
                                 newdata = NULL,
                                 quantiles = c(0.95, 0.99)) {
  ## ground_truth numeric_matrix numeric_vector -> numeric_matrix
  ## predicts ground truth extreme quantiles
  
  # validate newdata
  if (is.null(newdata)) {
    newdata <- object$intermediate_threshold$X.orig
  }
  
  # predict quantiles
  generate_theoretical_quantiles(
    quantiles = quantiles,
    X = newdata, model = object$model,
    distr = object$distr, df = object$df
  )
}


fit_predict_methods <- function(
  method = c(
    "grf", "meins",
    "unc_gpd", "full_unc",
    "evgam", "truth",
    "taillardat_qrf", "gbex",
    "zero", "inf"
  ),
  X, Y, Q_X,
  intermediate_threshold,
  intermediate_quantile,
  newdata = NULL, quantiles,
  model = "step", distr = "student_t", df = 4,
  evgam_model_shape = FALSE,
  taillardat_remove_negatives = FALSE,
  taillardat_method = c("grf", "meins"),
  gbex_cv = FALSE) {
  ## character numeric_matrix numeric_vector numeric_vector
  ##  quantile_forest numeric
  ##  character character numeric
  ##  numeric_matrix numeric_vector
  ##  character character numeric
  ##  boolean
  ##  boolean
  ##  character_vector
  ##  boolean
  ##  -> numeric_matrix
  ## fit and predict with the given method
  
  # validate arguments
  method <- match.arg(method)
  taillardat_method <- match.arg(taillardat_method)
  
  # fit-predict method
  if (method == "grf") {
    fit_obj <- intermediate_threshold
    
    predict(
      object = fit_obj,
      newdata = newdata,
      quantiles = quantiles
    )[[1]]
  } else if (method == "meins") {
    fit_obj <- quantile_forest(
      X, Y,
      regression.splitting = TRUE
    )
    
    predict(
      object = fit_obj,
      newdata = newdata,
      quantiles = quantiles
    )[[1]]
  } else if (method == "unc_gpd") {
    fit_obj <- fit_unconditional_gpd(
      X, Y, Q_X,
      intermediate_threshold = intermediate_threshold,
      intermediate_quantile = intermediate_quantile
    )
    
    predict_unconditional_gpd(
      object = fit_obj,
      newdata = newdata,
      quantiles = quantiles
    )
  } else if (method == "full_unc") {
    fit_obj <- fit_full_unconditional(
      X, Y, Q_X,
      intermediate_threshold = intermediate_threshold,
      intermediate_quantile = intermediate_quantile
    )
    
    predict_full_unconditional(
      object = fit_obj,
      newdata = newdata,
      quantiles = quantiles
    )
  } else if (method == "evgam") {
    if (ncol(X) > 40) {
      warning("Cannot fit evgam with more than 40 predictors.")
      matrix(
        nrow = nrow(newdata),
        ncol = length(quantiles)
      )
    } else {
      fit_obj <- fit_extreme_gam(
        X, Y, Q_X,
        intermediate_threshold = intermediate_threshold,
        intermediate_quantile = intermediate_quantile,
        evgam_model_shape = evgam_model_shape
      )
      
      predict_extreme_gam(
        object = fit_obj,
        newdata = newdata,
        quantiles = quantiles
      )
    }
  } else if (method == "truth") {
    fit_obj <- fit_ground_truth(
      X, Y, Q_X,
      intermediate_threshold = intermediate_threshold,
      intermediate_quantile = intermediate_quantile,
      model = model, distr = distr, df = df
    )
    
    predict_ground_truth(
      object = fit_obj,
      newdata = newdata,
      quantiles = quantiles
    )
  } else if (method == "taillardat_qrf") {
    fit_obj <- fit_taillardat(
      X = X, Y = Y,
      intermediate_quantile = intermediate_quantile,
      remove_negatives = taillardat_remove_negatives,
      method = taillardat_method
    )
    
    predict_taillardat(
      fit_obj,
      newdata = newdata,
      quantiles = quantiles
    )
  } else if (method == "gbex") {
    fit_obj <- fit_gbex(X, Y, Q_X,
                        intermediate_threshold = intermediate_threshold,
                        intermediate_quantile = intermediate_quantile,
                        cv = gbex_cv
    )
    
    predict_gbex(
      fit_obj,
      newdata = newdata,
      quantiles = quantiles
    )
  } else if (method == "zero"){
    matrix(0, 
           nrow = nrow(newdata),
           ncol = length(quantiles)
    )
  } else if (method == "inf"){
    matrix(1e23, 
           nrow = nrow(newdata),
           ncol = length(quantiles)
    )
  } else {
    stop("Wrong method provided.", call. = FALSE)
  }
}


compute_model_assessment_unconditional_gpd <-
  function(object,
           newdata_X = NULL,
           newdata_Y = NULL,
           class_nm = "unconditional_gpd") {
    ## unconditional_gpd numeric_matrix numeric_vector character
    ##  -> tibble
    ## produce a tibble with columns:
    ## - `observed_quantiles`
    ## - `theoretical_quantiles`
    
    # check class is valid
    if (!is_of_class(object, class_nm)) {
      stop(glue::glue("object must be of class `{class_nm}`"),
           call. = FALSE
      )
    }
    
    # store newdata_X condition
    newdata_X_isnull <- is.null(newdata_X)
    
    # validate newdata_X and newdata_Y
    if (is.null(newdata_X) & is.null(newdata_Y)) {
      newdata_X <- object$intermediate_threshold$X.orig
      newdata_Y <- object$intermediate_threshold$Y.orig
    } else if (!is.null(newdata_X) & !is.null(newdata_Y)) {
      newdata_X <- newdata_X
      newdata_Y <- newdata_Y
    } else {
      stop("`newdata_X` and `newdata_Y` must both be `NULL` or not `NULL`.",
           call. = FALSE
      )
    }
    
    # predict intermediate quantile
    if (newdata_X_isnull) {
      Q_x <- object$Q_X
    } else {
      Q_x <- erf:::predict_intermediate_quantile(
        intermediate_threshold = object$intermediate_threshold,
        newdata = newdata_X,
        intermediate_quantile = object$intermediate_quantile
      )
    }
    
    # compute optimal GPD parameters
    gpd_pars <- tibble::tibble(
      sigma = rep(object$sigma, length(newdata_Y)),
      xi = rep(object$xi, length(newdata_Y))
    )
    
    # compute model assessment
    erf:::compute_model_assessment_helper(gpd_pars, newdata_Y, Q_x)
  }


compute_model_assessment_gbex <-
  function(object,
           newdata_X = NULL,
           newdata_Y = NULL,
           class_nm = "gbex") {
    ## gbex numeric_matrix numeric_vector character
    ##  -> tibble
    ## produce a tibble with columns:
    ## - `observed_quantiles`
    ## - `theoretical_quantiles`
    
    # check class is valid
    if (!is_of_class(object, class_nm)) {
      stop(glue::glue("object must be of class `{class_nm}`"),
           call. = FALSE
      )
    }
    
    # store newdata_X condition
    newdata_X_isnull <- is.null(newdata_X)
    
    # validate newdata_X and newdata_Y
    if (is.null(newdata_X) & is.null(newdata_Y)) {
      newdata_X <- object$intermediate_threshold$X.orig
      newdata_Y <- object$intermediate_threshold$Y.orig
    } else if (!is.null(newdata_X) & !is.null(newdata_Y)) {
      p <- ncol(newdata_X)
      newdata_X <- as_tibble(
        newdata_X,
        .name_repair = ~ paste0("X", 1:p)
      )
      newdata_Y <- newdata_Y
    } else {
      stop("`newdata_X` and `newdata_Y` must both be `NULL` or not `NULL`.",
           call. = FALSE
      )
    }
    
    # predict intermediate quantile
    if (newdata_X_isnull) {
      Q_x <- object$Q_X
    } else {
      Q_x <- erf:::predict_intermediate_quantile(
        intermediate_threshold = object$intermediate_threshold,
        newdata = newdata_X,
        intermediate_quantile = object$intermediate_quantile
      )
    }
    
    # compute optimal GPD parameters
    gbex_pars <- predict(object$gbex.fit, newdata = newdata_X)
    
    # compute model assessment
    erf:::compute_model_assessment_helper(gbex_pars, newdata_Y, Q_x)
  }


compute_model_assessment_full_unconditional <-
  function(object,
           newdata_X = NULL,
           newdata_Y = NULL,
           class_nm = "full_unconditional") {
    ## full_unconditional numeric_matrix numeric_vector character
    ##  -> tibble
    ## produce a tibble with columns:
    ## - `observed_quantiles`
    ## - `theoretical_quantiles`
    
    # check class is valid
    if (!is_of_class(object, class_nm)) {
      stop(glue::glue("object must be of class `{class_nm}`"),
           call. = FALSE
      )
    }
    
    # store newdata_X condition
    newdata_X_isnull <- is.null(newdata_X)
    
    # validate newdata_X and newdata_Y
    if (is.null(newdata_X) & is.null(newdata_Y)) {
      newdata_X <- object$intermediate_threshold$X.orig
      newdata_Y <- object$intermediate_threshold$Y.orig
    } else if (!is.null(newdata_X) & !is.null(newdata_Y)) {
      newdata_X <- newdata_X
      newdata_Y <- newdata_Y
    } else {
      stop("`newdata_X` and `newdata_Y` must both be `NULL` or not `NULL`.",
           call. = FALSE
      )
    }
    
    # predict intermediate quantile
    if (newdata_X_isnull) {
      Q_x <- object$Q_X
    } else {
      Q_x <- rep(object$Q_X[1], nrow(newdata_X))
    }
    
    # compute optimal GPD parameters
    gpd_pars <- tibble::tibble(
      sigma = rep(object$sigma, length(newdata_Y)),
      xi = rep(object$xi, length(newdata_Y))
    )
    
    # compute model assessment
    erf:::compute_model_assessment_helper(gpd_pars, newdata_Y, Q_x)
  }


compute_model_assessment_extreme_gam <-
  function(object,
           newdata_X = NULL,
           newdata_Y = NULL,
           class_nm = "extreme_gam") {
    ## extreme_gam numeric_matrix numeric_vector character
    ##  -> tibble
    ## produce a tibble with columns:
    ## - `observed_quantiles`
    ## - `theoretical_quantiles`
    
    # check class is valid
    if (!is_of_class(object, class_nm)) {
      stop(glue::glue("object must be of class `{class_nm}`"),
           call. = FALSE
      )
    }
    
    # store newdata_X condition
    newdata_X_isnull <- is.null(newdata_X)
    
    # validate newdata_X and newdata_Y
    if (is.null(newdata_X) & is.null(newdata_Y)) {
      newdata_X <- object$data
      newdata_Y <- object$intermediate_threshold$Y.orig
    } else if (!is.null(newdata_X) & !is.null(newdata_Y)) {
      p <- ncol(newdata_X)
      newdata_X <- as_tibble(
        newdata_X,
        .name_repair = ~ paste0("X", 1:p)
      )
      newdata_Y <- newdata_Y
    } else {
      stop("`newdata_X` and `newdata_Y` must both be `NULL` or not `NULL`.",
           call. = FALSE
      )
    }
    
    # predict intermediate quantile
    if (newdata_X_isnull) {
      Q_x <- object$Q_X
    } else {
      Q_x <- erf:::predict_intermediate_quantile(
        intermediate_threshold = object$intermediate_threshold,
        newdata = newdata_X,
        intermediate_quantile = object$intermediate_quantile
      )
    }
    
    # compute optimal GPD parameters
    gpd_pars <- predict(object$evgam, data_test)
    gpd_pars[, 1] <- exp(gpd_pars[, 1])
    gpd_pars <- tibble::tibble(
      "sigma" = gpd_pars[, 1],
      "xi" = gpd_pars[, 2]
    )
    
    # compute model assessment
    erf:::compute_model_assessment_helper(gpd_pars, newdata_Y, Q_x)
  }


return_method_results <- function(lst,
                                  return_value = c(
                                    "predicted_quantiles", "ise",
                                    "cv", "quantile_loss"
                                  )) {
  #' @param lst : list made of
  #'  -`quantiles`,
  #'  -`predict`: list made of numeric_matrices for method_1, ..., method_n,
  #'  -`time` (optional): list made of numeric for method_1, ..., method_n,
  #'  -`data`: list containing:
  #'      -- a numeric matrix for test predictors,
  #'      -- a numeric vector for test response,
  #'  - `best.min.node.size` (if `return value = "cv`): best `min.node.size`
  #'   chosen by cross-validation,
  #'  - `best.lambda`: (if `return value = "cv`): best `lambda` chosen by
  #'   cross-validation.
  #'
  #'  @param return_value : character, one of `predicted_quantiles`, `ise`,
  #'   `cv`, `quantile_loss`.
  #'
  #'  @return `tibble` with columns:
  #'   - `method`,
  #'   - `quantiles`,
  #'   - `ise`, if `return_value = "ise"` or `return_value = "cv"`,
  #'   - `quantile_loss`, if `return_value = "quantile_loss"`,
  #'   - `time`, if `lst$time` exists,
  #'   - `rowid`, if `return_value = "predicted_quantiles`,
  #'   - `X1, ..., Xp`, if `return_value = "predicted_quantiles`,
  #'   - `predictions`, if `return_value = "predicted_quantiles`,
  #'   - `best.min.node.size`, if `return_value = "cv"`,
  #'   - `best.lambda`, if `return_value = "cv"`.
  #'
  
  # validate arguments
  return_value <- match.arg(return_value)
  
  if (return_value == "ise" | return_value == "cv") {
    if (!("truth" %in% names(lst$predict))) {
      stop("Method `truth` must be provided when `return_value = 'ise'` or `return_value = 'cv'`.",
           call. = FALSE
      )
    }
  }
  
  a <- purrr::map(
    names(lst$predict),
    function(name) {
      tibble(
        predictions = matrix2list(lst$predict[[name]]),
        method = name
      ) %>%
        rowid_to_column()
    }
  )
  
  methods <- names(lst$predict)
  
  b <- purrr::reduce(a, bind_rows) %>%
    mutate(quantiles = list(lst$quantiles)) %>%
    pivot_wider(names_from = "method", values_from = "predictions") %>%
    bind_cols(
      as_tibble(lst$data$X_test)
    ) %>%
    unnest(cols = all_of(c(methods, "quantiles")))
  
  if (return_value == "predicted_quantiles") {
    c <- b %>%
      pivot_longer(
        cols = all_of(methods),
        names_to = "method", values_to = "predictions"
      )
  } else if (return_value == "ise" | return_value == "cv") {
    c <- b %>%
      select(!(matches("X[0-9]+"))) %>%
      mutate(across(
        !all_of(c("rowid", "quantiles")),
        function(x) {
          (x - truth)^2
        }
      )) %>%
      select(-"truth") %>%
      pivot_longer(
        cols = !all_of(c("rowid", "quantiles")),
        names_to = "method", values_to = "se"
      ) %>%
      group_by(method, quantiles) %>%
      summarise(ise = mean(se)) %>%
      ungroup()
  } else if (return_value == "quantile_loss") {
    b <- purrr::reduce(a, bind_rows) %>%
      mutate(quantiles = list(lst$quantiles)) %>%
      pivot_wider(names_from = "method", values_from = "predictions") %>%
      bind_cols(
        as_tibble(lst$data$X_test)
      ) %>%
      mutate(Y_test = lst$data$Y_test) %>%
      unnest(cols = all_of(c(methods, "quantiles")))
    
    c <- b %>%
      select(!(matches("X[0-9]+"))) %>%
      mutate(across(
        !all_of(c("rowid", "quantiles", "Y_test")),
        function(x) {
          quantile_loss_vec(y = Y_test, q = x, p = quantiles)
        }
      )) %>%
      select(-"Y_test") %>%
      pivot_longer(
        cols = !all_of(c("rowid", "quantiles")),
        names_to = "method", values_to = "loss"
      ) %>%
      group_by(method, quantiles) %>%
      summarise(quantile_loss = mean(loss)) %>%
      ungroup()
  } else {
    stop("`return_value` must be one of `predicted_quantiles`, `ise`, `cv`, or `quantile_loss`.",
         call. = FALSE
    )
  }
  
  if (any(names(lst) == "time")) {
    time_tbl <- lst$time %>%
      as_tibble() %>%
      pivot_longer(
        cols = everything(),
        names_to = "method", values_to = "time"
      )
    
    c <- c %>%
      left_join(time_tbl, by = "method")
  }
  
  if (return_value == "cv") {
    c <- c %>%
      mutate(
        best.min.node.size = if_else(method == "erf",
                                     lst$best.min.node.size, NaN
        ),
        best.lambda = if_else(method == "erf",
                              lst$best.lambda, NaN
        )
      )
  }
  
  return(c)
}

#' Wrapper to fit-predict different methods.
#' 
#' @param dat_train A `tibble` representing the training dataset.
#' 
#' @param dat_test A `tibble` representing the training dataset.
#' 
#' @param response A `character` representing the response variable.
#' 
#' @param methods A `list` of `list("name" = character, "params" = named_list)`.
#' 
#' @param quantiles A `numeric_vector`.
#' 
#' @param cv_erf A `boolean` on whether cross-validate ERF or not.
#'  Default is `cv_erf = FALSE`.
#' 
#' @return A `list` of 
#'   - `"name" = character`
#'   - `"params" = named_list`
#'   - `"preds" = numeric_matrix`
#'   - `"quantiles" = numeric_vector`
#'   - `"fit" = erf` (optional)
#' 
fit_predict_wrapper <- function(dat_train, dat_test, response,
                                methods, quantiles, cv_erf = FALSE){
  # check if "erf" is provided
  if (all(purrr::transpose(methods)$name != "erf")){
    stop('`methods` must contain "erf".')
  } else {
    erf_cond <- purrr::transpose(methods)$name == "erf"
  }
  
  # get predictor-response matrices
  mat_train <- get_regression_data(dat = dat_train, response = response)
  mat_test <- get_regression_data(dat = dat_test, response = response)
  
  
  # fit-predict erf
  if (cv_erf) {
    fit_erf_cv <- rlang::exec(
      .fn = purrr::partial(erf_cv, X = mat_train$X, Y = mat_train$Y), 
      !!!methods[erf_cond][[1]]$params
    )
    
    fit_erf <- fit_erf_cv$fit.erf
    
  } else {
    fit_erf <- rlang::exec(
      .fn = purrr::partial(erf, X = mat_train$X, Y = mat_train$Y), 
      !!!methods[erf_cond][[1]]$params
    )
  }
  
  
  pred_erf <- predict(object = fit_erf, newdata = mat_test$X, 
                      quantiles = quantiles)
  
  
  # fit-predict other methods
  purrr::map(methods, function(lst){
    
    if (lst$name == "erf"){
      list(
        name = lst$name,
        params = lst$params,
        preds = pred_erf,
        quantiles = quantiles,
        fit = fit_erf
      )
    } else {
      partial_fn <- purrr::partial(
        fit_predict_methods,
        method = lst$name, 
        X = mat_train$X, Y = mat_train$Y, 
        Q_X = fit_erf$Q_X, 
        intermediate_threshold = fit_erf$intermediate_threshold,
        intermediate_quantile = fit_erf$intermediate_quantile,
        newdata = mat_test$X,
        quantiles = quantiles)
      
      list(
        name = lst$name, 
        params = lst$params,
        preds = purrr::exec(.fn = partial_fn, !!!lst$params),
        quantiles = quantiles
      )
    }
  })
}
