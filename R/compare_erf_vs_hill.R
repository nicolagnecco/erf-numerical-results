#' Compare ERF vs weighted hill
#'
#' Compare performance of ERF (not cross-validated with lambda = 0) against
#' weighted Hill estimator
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
#' @param df (numeric): number of degrees of freedom/tail index when
#'  `distr != "gaussian"`. Higher means lighter tails.
#'
#' @param test_data (character): method to generate testing observations. One of
#'  `halton`, `beta`.
#'
#' @param alpha_beta (numeric): parameter from Beta distribution.
#'  When `alpha_beta = 1`, it corresponds to Uniform distribution.
#'  Default is `alpha_beta = 1`.
#'
#' @param min.node.size (numeric): target for the minimum number of
#'  observations in each tree leaf.
#'  Default is `min.node.size = 5`.
#'
#' @param lambda (numeric): vector of penalties for the GPD likelihood
#'  of `erf`.
#'  Default is `lambda = 0`.
#'
#' @param quantiles (numeric_vector): vector of quantile levels to predict.
#'  Default is `quantiles = c(0.95)`.
#'
#' @param intermediate_quantiles (numeric_vector): vector of intermediate
#'  quantile levels.
#'  Default is `intermediate_quantiles = c(0.8)`.
#'
#'
#' @return `tibble` with column names-types
#'  * `method` (`<chr>`)
#'  * `quantiles` (`<dbl>`)
#'  * `intermediate_quantile` (`<dbl>`)
#'  *  `ise` (`<dbl>`)
#'
compare_erf_vs_hill <- function(n, p, ntest,
                                model = MODELS,
                                distr = DISTR, df = 4,
                                test_data = c("halton", "beta"),
                                alpha_beta = 1,
                                min.node.size = 5, lambda = 0,
                                quantiles = c(0.99),
                                intermediate_quantiles = c(0.8)) {
    # check arguments
    model <- match.arg(model)
    distr <- match.arg(distr)
    test_data <- match.arg(test_data)


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

    res <- purrr::map_dfr(
        intermediate_quantiles,
        function(intermediate_quantile) {
            # ERF
            fit_erf <- erf::erf(
                X = X, Y = Y,
                min.node.size = min.node.size, lambda = lambda,
                intermediate_estimator = "grf",
                intermediate_quantile = intermediate_quantile
            )

            q_erf <- predict(
                object = fit_erf,
                newdata = X_test,
                quantiles = quantiles
            )


            # Hill
            hill <- fit_hill(
                X, Y, fit_erf$Q_X,
                intermediate_threshold = fit_erf$intermediate_threshold,
                intermediate_quantile = fit_erf$intermediate_quantile,
                fit_erf = fit_erf
            )

            q_hill <- predict_hill(
                hill,
                newdata = X_test, quantiles = quantiles
            )$preds

            # Hill exponential
            shape_rf <- fit_shape_rf(
                X, Y, fit_erf$Q_X,
                intermediate_threshold = fit_erf$intermediate_threshold,
                intermediate_quantile = fit_erf$intermediate_quantile,
                fit_erf = fit_erf
            )

            q_shape_rf <- predict_shape_rf(
                shape_rf,
                newdata = X_test, quantiles = quantiles
            )$preds


            # Ground truth
            ground_truth <- fit_ground_truth(
                X, Y, fit_erf$Q_X,
                intermediate_threshold = fit_erf$intermediate_threshold,
                intermediate_quantile = fit_erf$intermediate_quantile,
                model = model,
                df = df, distr = distr
            )

            q_true <- predict_ground_truth(ground_truth, X_test, quantiles)

            purrr::map_dfr(seq_along(quantiles), function(q) {
                # if want ise
                quantile <- quantiles[q]
                bind_rows(
                    tibble(
                        method = "erf",
                        quantiles = quantile,
                        intermediate_quantile = intermediate_quantile,
                        ise = mean((q_erf[, q] - q_true[, q])^2)
                    ),
                    tibble(
                        method = "hill",
                        quantiles = quantile,
                        intermediate_quantile = intermediate_quantile,
                        ise = mean((q_hill[, q] - q_true[, q])^2)
                    ),
                    tibble(
                        method = "shape_rf",
                        quantiles = quantile,
                        intermediate_quantile = intermediate_quantile,
                        ise = mean((q_shape_rf[, q] - q_true[, q])^2)
                    )
                )

                # if want quantile predictions (something along this line)
                # bind_rows(
                #   tibble(method = "erf", quantiles = q_erf[, q], X1 = X_test[, 1]),
                #   tibble(method = "hill", quantiles = q_hill[, q], X1 = X_test[, 1]),
                #   tibble(method = "truth,", quantiles = q_true[, q], X1 = X_test[, 1])
                # )
            })
        }
    )

    return(res)
}
