generate_joint_distribution <- function(n, p,
                                        model = MODELS,
                                        distr = DISTR, df) {
  ## integer (x2) character (x2) numeric -> list
  ## generate n iid observations of (X, Y), where X is p-dimensional predictor
  ## and Y is the response following the given model with the
  ## given distribution.
  ## Returns a list with:
  ## - X, nxp matrix, p-dimensional predictor
  ## - Y, vector with n elements, response variable

  generate_joint_distribution_internal(n, p, model, distr, df)
}

generate_conditional_distribution <- function(model = MODELS,
                                              distr = DISTR, df,
                                              X) {
  ## integer (x2) character (x2) numeric numeric_matrix -> list
  ## generate n iid observations of (Y| X = x), where X is p-dimensional predictor
  ## and Y is the response following the given model with the
  ## given distribution.
  ## Returns a list with:
  ## - X, nxp matrix, p-dimensional predictor
  ## - Y, vector with n elements, response variable
  n <- nrow(X)
  p <- ncol(X)
  generate_joint_distribution_internal(n, p, model, distr, df, X)
}

generate_theoretical_quantiles <- function(quantiles, X,
                                           model = MODELS,
                                           distr = DISTR,
                                           df) {
  ## numeric_vector numeric_matrix character (x2) numeric -> numeric_matrix
  ## produce theoretical quantiles for the given model and distribution
  ## for the different observations (rows of X)

  model <- match.arg(model)
  distr <- match.arg(distr)
  n <- nrow(X)
  p <- ncol(X)

  if (!missing(df) & distr == "gaussian") {
    warning("`df` is not considered when distr = 'gaussian'", call. = FALSE)
  }

  switch(model,
    "iid" = {
      q_vec <- generate_quantile_model(
        quantiles, X, distr, df,
        sigma_iid, df_constant
      )
    },
    "step" = {
      q_vec <- generate_quantile_model(
        quantiles, X, distr, df,
        sigma_step, df_constant
      )
    },
    "step2d" = {
      q_vec <- generate_quantile_model(
        quantiles, X, distr, df,
        sigma_step2d, df_constant
      )
    },
    "linear" = {
      q_vec <- generate_quantile_model(
        quantiles, X, distr, df,
        sigma_linear, df_constant
      )
    },
    "linearstep" = {
      q_vec <- generate_quantile_model(
        quantiles, X, distr, df,
        sigma_linearstep, df_constant
      )
    },
    "quadratic" = {
      q_vec <- generate_quantile_model(
        quantiles, X, distr, df,
        sigma_quadratic, df_constant
      )
    },
    "mixture" = {
      q_vec <- generate_quantile_model(
        quantiles, X, distr, df,
        sigma_mixture, df_constant
      )
    },
    "bell" = {
      q_vec <- generate_quantile_model(
        quantiles, X, distr, df,
        sigma_bell, df_constant
      )
    },
    "tanh" = {
      q_vec <- generate_quantile_model(
        quantiles, X, distr, df,
        sigma_tanh, df_constant
      )
    },
    "step_step" = {
      q_vec <- generate_quantile_model(
        quantiles, X, distr, df,
        sigma_step, df_step
      )
    },
    "step2d_step" = {
      q_vec <- generate_quantile_model(
        quantiles, X, distr, df,
        sigma_step2d, df_step
      )
    },
    "linear_step" = {
      q_vec <- generate_quantile_model(
        quantiles, X, distr, df,
        sigma_linear, df_step
      )
    },
    "linearstep_step" = {
      q_vec <- generate_quantile_model(
        quantiles, X, distr, df,
        sigma_linearstep, df_step
      )
    },
    "quadratic_step" = {
      q_vec <- generate_quantile_model(
        quantiles, X, distr, df,
        sigma_quadratic, df_step
      )
    },
    "bell_tanh" = {
      q_vec <- generate_quantile_model(
        quantiles, X, distr, df,
        sigma_bell, df_tanh
      )
    },
    "bell_sigmoid" = {
      q_vec <- generate_quantile_model(
        quantiles, X, distr, df,
        sigma_bell, df_sigmoid
      )
    },
    "tanh_tanh" = {
      q_vec <- generate_quantile_model(
        quantiles, X, distr, df,
        sigma_tanh, df_tanh
      )
    },
    "quadratic2d_tanh" = {
      q_vec <- generate_quantile_model(
        quantiles, X, distr, df,
        sigma_quadratic2d, df_tanh
      )
    }
  )

  return(q_vec)
}

generate_Y_model <- function(X, distr = c("gaussian", "student_t"),
                             df, sigma_fun, df_fun) {
  ## numeric_matrix character numeric function function -> numeric_vector
  ## generate random response Y for the given model

  distr <- match.arg(distr)
  n <- nrow(X)
  p <- ncol(X)

  switch(distr,
    "gaussian" = {
      Y_tilde <- rnorm(n)
    },
    "student_t" = {
      Y_tilde <- rt(n, df = df_fun(X, df))
    }
  )

  sigma_x <- sigma_fun(X)

  sigma_x * Y_tilde
}

generate_quantile_model <- function(quantiles, X,
                                    distr = c("gaussian", "student_t"),
                                    df, sigma_fun, df_fun) {
  ## numeric_vector numeric_matrix character numeric -> numeric_matrix
  ## produce theoretical quantiles for the given model

  distr <- match.arg(distr)
  n <- nrow(X)
  p <- ncol(X)
  l <- length(quantiles)

  switch(distr,
    "gaussian" = {
      q_tilde <- quantiles_gaussian(quantiles, n)
    },
    "student_t" = {
      q_tilde <- quantiles_student_t(quantiles, df_fun(X, df))
    }
  )

  sigma_x <- sigma_fun(X) %>% rep_vector2matrix(nrep = l, dim = "col")

  sigma_x * q_tilde
}

quantiles_gaussian <- function(quantiles, n) {
  ## numeric_vector integer -> numeric_matrix
  ## produce matrix with gaussian quantiles where nrow = n and
  ## ncol = length(quantiles)

  rep_vector2matrix(qnorm(quantiles),
    nrep = n,
    dim = "row"
  )
}

quantiles_student_t <- function(quantiles, df) {
  ## numeric_vector numeric_vector -> numeric_matrix
  ## produce matrix with student_t quantiles where nrow = length(df) and
  ## ncol = length(quantiles)

  purrr::map(quantiles, function(q) {
    qt(q, df = df)
  }) %>%
    list2matrix(dim = "col")
}

# sigma functions

sigma_iid <- function(X) {
  ## numeric_matrix -> numeric_vector
  ## produce scale function: iid data not depending on X's

  rep(1, times = nrow(X))
}

sigma_step <- function(X) {
  ## numeric_matrix -> numeric_vector
  ## produce scale function: scale(X1) = step function

  sigma_x <- 1 + 1 * (X[, 1] > 0)

  return(sigma_x)
}

sigma_step2d <- function(X) {
  ## numeric_matrix -> numeric_vector
  ## produce scale function: scale(X1, X2) = step function

  sigma_x <- numeric(nrow(X))
  sigma_x[X[, 1] < 0] <- 1
  sigma_x[X[, 1] >= 0 & X[, 2] < 0] <- 2
  sigma_x[X[, 1] >= 0 & X[, 2] >= 0] <- 1 / 4

  return(sigma_x)
}

sigma_linear <- function(X) {
  ## numeric_matrix -> numeric_vector
  ## produce scale function: linear model scale(X1) = 2 + 1 * X1

  X1 <- X[, 1]
  2 + 1 * X1
}

sigma_linearstep <- function(X) {
  ## numeric_matrix -> numeric_vector
  ## produce scale function: linear model scale(X1, X2) = linear in X1,
  ##  step in X2
  
  sigma_x <- numeric(nrow(X))
  sigma_x[X[, 2] >= 0] <- 2 + 1 * X[X[, 2] >=0, 1]
  sigma_x[X[, 2] < 0] <- 2 + 2 * X[X[, 2] < 0, 1]
  
  return(sigma_x)
}

sigma_quadratic <- function(X) {
  ## numeric_matrix -> numeric_vector
  ## produce scale function: quadratic model scale(X1) = a - b * X1 ^2

  X1 <- X[, 1]
  3 - 2 * X1^2
  
}

sigma_mixture <- function(X) {
  ## numeric_matrix -> numeric_vector
  ## produce scale function: scale(X1, X2) = mixture of 2 Gaussians

  mu_1 <- c(-.5, .5)
  sigma_1 <- rbind(c(1 / 8, 0), c(0, 1 / 8))
  w_1 <- 6
  mu_2 <- c(.5, -.5)
  sigma_2 <- rbind(c(1 / 8, 0), c(0, 1 / 8))
  w_2 <- 6

  sigma_x <- 1 +
    w_1 * mvtnorm::dmvnorm(X[, c(1, 2)], mean = mu_1, sigma = sigma_1) +
    w_2 * mvtnorm::dmvnorm(X[, c(1, 2)], mean = mu_2, sigma = sigma_2)

  # sigma_x[sigma_x < 1.5] <- 1
  # sigma_x[sigma_x >= 1.5] <- 5

  return(sigma_x)
}

sigma_quadratic2d <- function(X){
  ## numeric_matrix -> numeric_vector
  ## produce scale function: scale(X1, X2) = a - b * X1^2 - c * X2^2
  
   4 - (1 * X[, 1]^2 + 2 * X[, 2]^2)
  
}

sigma_tanh <- function(X) {
  ## numeric_matrix -> numeric_vector
  ## produce scale function: scale(X1) = g(tanh(f(X1)))
  
  
  (2 + tanh(2 * X[, 1])) * (1 + 0.5 * X[, 2])
}

sigma_bell <- function(X){
  ## numeric_matrix -> numeric_vector
  ## produce scale function: scale(X1, X2) = gaussian(X1, X2; mu, Cov)
  
  # gbex version
  # mu <- c(0, 0)
  # Cov <- rbind(
  #   c(1, 0.9),
  #   c(0.9, 1)
  # )
  # 
  # 1 + (6 * mvtnorm::dmvnorm(X[, c(1, 2)], mean = mu, sigma = Cov))
  
  # our version
  mu <- c(0, 0)
  Cov <- rbind(
    c(1, 0.75),
    c(0.75, 1)
  )
  
  1 + (2 * pi * mvtnorm::dmvnorm(2 * X[, c(1, 2)], mean = mu, sigma = Cov))
  
}

# shape functions (expressed as degrees of freedom (df), where df = 1 / shape)

df_constant <- function(X, df) {
  ## numeric_matrix numeric -> numeric_vector
  ## produce degree of freedom function: constant equal to df

  rep(df, times = nrow(X))
}

df_step <- function(X, df) {
  ## numeric_matrix numeric -> numeric_vector
  ## produce degree of freedom function: shape(X2) = step function

  df_x <- df + 2 * df * (X[, 2] > 0)

  return(df_x)
}

df_sigmoid <- function(X, df) {
  ## numeric_matrix numeric -> numeric_vector
  ## produce degree of freedom function: shape(X1) = sigmoid(X1)
  warning("`df` is not considered with `df_sigmoid` function", call. = FALSE)
  7 *  (1 + exp(4 * X[, 1] + 1.2)) ^(-1) + 3
}

df_tanh <- function(X, df) {
  ## numeric_matrix numeric -> numeric_vector
  ## produce degree of freedom function: shape(X1) = tanh(X1)
  warning("`df` is not considered with `df_tanh` function", call. = FALSE)
  3 + 3 * (1 + tanh(-2 * X[, 1]))
}

# df(x) = 3 + 3 * (1 + tanh(-2 * X1))
# xi(x) = 1 / df(x) = 1 / (3 + 3 * (1 + tanh(-2 * X1)))

# other functions
generate_test_data <- function(ntest, p,
                               method = c("halton", "halton_leaped", "beta"),
                               alpha_beta = 1) {
  ## numeric numeric character numeric -> numeric_matrix
  ## generate predictor test data using "halton", "halton_leaped" sequence 
  ## or "beta" distribution

  # validate arguments
  method <- match.arg(method)
  
  if(p < 1){
    stop("`p` must be a positive integer.", call. = FALSE)
  }

  if (method == "halton") {
    X_test <- cbind(
      randtoolbox::halton(ntest, p) * 2 - 1
    )
  } else if (method == "halton_leaped") {
    X_test <- cbind(
      sfsmisc::QUnif(n = ntest, p = p, min = -1, max = 1, leap = 31)
    )
  } else if (method == "beta") {
    X_test_1 <- cbind(
      2 * rbeta(
        n = ntest,
        shape1 = alpha_beta,
        shape2 = 1 / alpha_beta
      ) - 1
    )
    if (p > 1) {
      X_test <- cbind(
        X_test_1,
        matrix(
          runif(
            n = ntest * (p - 1),
            min = -1,
            max = 1
          ),
          ncol = p - 1,
          nrow = ntest
        )
      )
    }
    
  }

  colnames(X_test) <- paste0("X", seq_len(p))
  return(X_test)
}

generate_iid_data <- function(n,
                              distr = c("gaussian", "log_normal", "student_t"),
                              df = 2) {
  ## integer character numeric -> numeric_vector
  ## generate n random observations from distribution distr (with dof = df)

  distr <- match.arg(distr)

  if (distr == "gaussian") {
    Y <- rnorm(n)
  } else if (distr == "log_normal") {
    Y <- rlnorm(n)
  } else if (distr == "student_t") {
    Y <- rt(n, df)
  }

  return(Y)
}

generate_iid_quantile <- function(quantiles_predict,
                                  distr = c(
                                    "gaussian", "log_normal",
                                    "student_t"
                                  ),
                                  df = 2) {
  ## numeric_vector character numeric -> numeric_vector
  ## generate theoretical quantiles of distribution distr (with dof = df)

  distr <- match.arg(distr)

  if (distr == "gaussian") {
    q <- qnorm(p = quantiles_predict)
  } else if (distr == "log_normal") {
    q <- qlnorm(p = quantiles_predict)
  } else if (distr == "student_t") {
    q <- qt(p = quantiles_predict, df = df)
  }

  return(q)
}

generate_joint_distribution_internal <- function(n, p,
                                                 model = MODELS,
                                                 distr = DISTR, df,
                                                 X = NULL) {
  ## integer (x2) character (x2) numeric numeric_matrix|NULL-> list
  ## generate n iid observations of (X, Y), where X is p-dimensional predictor
  ## and Y is the response following the given model with the
  ## given distribution.
  ## Returns a list with:
  ## - X, nxp matrix, p-dimensional predictor
  ## - Y, vector with n elements, response variable

  model <- match.arg(model)
  distr <- match.arg(distr)

  if (is.null(X)) {
    X <- matrix(runif(n * p, min = -1, max = 1), n, p)
  } else {
    check_X_matrix(X, n, p)
  }

  if (!missing(df) & distr == "gaussian") {
    warning("`df` is not considered when distr = 'gaussian'", call. = FALSE)
  }

  switch(model,
    "iid" = {
      Y <- generate_Y_model(X, distr, df, sigma_iid, df_constant)
    },
    "step" = {
      Y <- generate_Y_model(X, distr, df, sigma_step, df_constant)
    },
    "step2d" = {
      Y <- generate_Y_model(X, distr, df, sigma_step2d, df_constant)
    },
    "linear" = {
      Y <- generate_Y_model(X, distr, df, sigma_linear, df_constant)
    },
    "linearstep" = {
      Y <- generate_Y_model(X, distr, df, sigma_linearstep, df_constant)
    },
    "quadratic" = {
      Y <- generate_Y_model(X, distr, df, sigma_quadratic, df_constant)
    },
    "mixture" = {
      Y <- generate_Y_model(X, distr, df, sigma_mixture, df_constant)
    },
    "bell" = {
      Y <- generate_Y_model(X, distr, df, sigma_bell, df_constant)
    },
    "tanh" = {
      Y <- generate_Y_model(X, distr, df, sigma_tanh, df_constant)
    },
    "step_step" = {
      Y <- generate_Y_model(X, distr, df, sigma_step, df_step)
    },
    "step2d_step" = {
      Y <- generate_Y_model(X, distr, df, sigma_step2d, df_step)
    },
    "linear_step" = {
      Y <- generate_Y_model(X, distr, df, sigma_linear, df_step)
    },
    "linearstep_step" = {
      Y <- generate_Y_model(X, distr, df, sigma_linearstep, df_step)
    },
    "quadratic_step" = {
      Y <- generate_Y_model(X, distr, df, sigma_quadratic, df_step)
    },
    "bell_tanh" = {
      Y <- generate_Y_model(X, distr, df, sigma_bell, df_tanh)
    },
    "bell_sigmoid" = {
      Y <- generate_Y_model(X, distr, df, sigma_bell, df_sigmoid)
    },
    "tanh_tanh" = {
      Y <- generate_Y_model(X, distr, df, sigma_tanh, df_tanh)
    },
    "quadratic2d_tanh" = {
      Y <- generate_Y_model(X, distr, df, sigma_quadratic2d, df_tanh)
    }
  )

  return(list(X = X, Y = Y))
}
