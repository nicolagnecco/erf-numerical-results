# Model functions ####
df_step_deprecated <- function(X, df) {
  ## numeric_matrix numeric -> numeric_vector
  ## produce degree of freedom function: constant equal to df
  
  rep(df, times = nrow(X))
}

df_mixture_deprecated <- function(X, df) {
  ## numeric_matrix numeric -> numeric_vector
  ## produce degree of freedom function: constant equal to df
  
  rep(df, times = nrow(X))
}

df_iid_deprecated <- function(X, df) {
  ## numeric_matrix numeric -> numeric_vector
  ## produce degree of freedom function: constant equal to df
  
  rep(df, times = nrow(X))
}

df_linear_deprecated <- function(X, df) {
  ## numeric_matrix numeric -> numeric_vector
  ## produce degree of freedom function: constant equal to df
  
  rep(df, times = nrow(X))
}

df_step2d_deprecated <- function(X, df) {
  ## numeric_matrix numeric -> numeric_vector
  ## produce degree of freedom function: constant equal to df
  
  rep(df, times = nrow(X))
}


# Method functions ####
predict_unconditional_quantiles <- function(threshold, quantiles = c(0.95),
                                            Y, ntest) {
  ## numeric (x2) numeric_vector integer-> list
  ## predict unconditional quantiles with MLE and returns a list with
  ## - predictions: Numeric matrix. Predictions at the ntest test points (on rows)
  ##                for each desired quantile (on columns).
  ## - pars: Numeric matrix. Estimated parameters at each test point (on rows).
  ##         The columns contain the estimates for the sigma and csi parameter,
  ##         respectively.
  ##
  
  # body
  p0 <- threshold
  t0 <- quantile(Y, p0)
  pars <- ismev::gpd.fit(Y, t0, show = FALSE)$mle
  sigma <- pars[1]
  xi <- pars[2]
  
  q_hat <- q_GPD_deprecated(quantiles, p0, t0, sigma, xi)
  
  predictions <- matrix(q_hat, nrow = ntest, ncol = length(quantiles), byrow = T)
  pars <- cbind(rep(sigma, ntest), rep(xi, ntest))
  return(list(predictions = predictions, pars = pars))
}

predict_quantile_mle <- function(threshold, quantiles_predict, Y) {
  ## numeric numeric_vector numeric_vector -> numeric_vector
  ## predict quantiles using MLE
  
  if (any(threshold > quantiles_predict)) {
    stop("threshold cannot be larger than quantiles_predict")
  }
  
  n <- length(Y)
  
  # compute exceedances
  thres <- rep(quantile(Y, threshold), n)
  k <- floor(n * (1 - threshold))
  exc_idx <- which(Y - thres > 0)
  exc_data <- (Y - thres)[exc_idx]
  
  # estimate params
  ml_par <- ismev::gpd.fit(exc_data, 0, show = FALSE)$mle
  
  # predict quantiles using GPD
  q_GPD_deprecated(
    p = quantiles_predict,
    p0 = threshold,
    t_x0 = quantile(Y, threshold),
    xi = ml_par[2], sigma = ml_par[1]
  )
}

predict_quantile_hill <- function(threshold, quantiles_predict, Y) {
  ## numeric numeric_vector numeric_vector -> numeric_vector
  ## predict quantiles using Hill
  
  if (any(threshold > quantiles_predict)) {
    stop("threshold cannot be larger than quantiles_predict")
  }
  
  n <- length(Y)
  
  # compute exceedances
  thres <- rep(quantile(Y, threshold), n)
  k <- floor(n * (1 - threshold))
  exc_idx <- which(Y - thres > 0)
  
  # estimate params
  xi <- 1 / k * sum(log(Y[exc_idx] / thres[exc_idx]))
  sigma <- xi / (1 - 2^(-xi)) * (quantile(Y, threshold) -
                                   quantile(Y, 1 - 2 * (1 - threshold)))
  
  # predict quantiles using GPD
  q_GPD_deprecated(
    p = quantiles_predict,
    p0 = threshold,
    t_x0 = quantile(Y, threshold),
    xi = xi, sigma = sigma
  )
}

predict_quantile_empirical <- function(quantiles_predict, Y) {
  ## numeric_vector numeric_vector -> numeric_vector
  ## predict empirical quantiles
  
  stats::quantile(Y, probs = quantiles_predict)
}

q_GPD_deprecated <- function(p, p0, t_x0, sigma, xi) {
  ## numeric(0, 1) numeric(0, 1) numeric_matrix numeric_vector
  ## numeric_vector -> numeric_vector
  ## produce the estimated extreme quantiles of GPD
  
  (((1 - p) / (1 - p0))^
     {
       -xi
     } - 1) * (sigma / xi) + t_x0
}

gpd_2_exponential_quantiles <- function(t_x0, Y.test, gpd_pars) {
  ## numeric_vector numeric_vector numeric_matrix -> plot
  ## produce exponential quantiles from GPD observations
  
  Y <- Y.test
  exc_idx <- which(Y - t_x0 > 0)
  exc_data <- (Y - t_x0)[exc_idx]
  xi <- gpd_pars[exc_idx, 2]
  sigma <- gpd_pars[exc_idx, 1]
  
  pseudo_obs <- erf:::compute_pseudo_observations(exc_data, sigma, xi)
  
  return(pseudo_obs)
}
