#' Function to run EDA on real-data
#' 
#' @param dat `tibble` containing the dataset
#' 
#' @param response `character` name of the response variables
#' 
#' @param methods `list` containing the name of the methods to fit and their 
#'  tuning parameters
#'  
#' @param quantiles `numeric_vector` containing the quantile levels to predict
#' 
#' @param noise_vars `numeric_vector` with number of noise variables to keep in  
#'  `dat`
#'  
#' @param n_workers `integer` represents number of independent R session to run
#'  in parallel
#'  
#' @param prop_train `numeric(0, 1)` represents fraction of data used to train
#'  the methods
#'  
#' @param file_log `character` filepath to save logs
#' 
#' @param file_quantiles `character` filepath to save predicted quantiles
#' 
#' @param file_gpdparams `character` filepath to save predicted GPD params
#' 
#' @param seed `integer` to reproduce results
#'
run_wagedata_eda <- function(dat, response, methods, quantiles,
                    noise_vars, n_workers, prop_train, file_log,
                    file_quantiles, file_gpdparams, seed){
  
  # set seed
  set.seed(seed)
  
  # Constants
  dat_eda <- dat
  NP <- length(noise_vars)
  
  # Set rng to reproduce parallel loop
  rng <- rngtools::RNGseq(1)
  
  # Set up cluster
  doFuture::registerDoFuture()
  future::plan(future::multisession, workers = min(NP, n_workers))
  
  # Loop over noise_vars
  ptm <- proc.time()
  cat("****", "EDA of ERF, GRF, GBEX and Unconditional GPD",
      "**** \n", file = file_log)
  
  
  res <- foreach(p = seq_len(NP), 
                 .combine = bind_rows, 
                 .options.future = list(scheduling = FALSE)) %dorng% 
    {
      
      # log
      cat("Simulation", p, "out of", length(noise_vars), 
          "PID:", Sys.getpid(), "\n", 
          file = file_log, append = TRUE)
      
      # Set RNG seed
      rngtools::setRNG(rng)
      
      # Choose correct number of noise variables
      dat_eda_noise <- dat_eda %>% 
        select(!starts_with("N_") | num_range("N_", seq_len(noise_vars[p])))
      
      # Split into train and test
      train_test <- split_data(dat = dat_eda_noise, 
                               prop_train = prop_train)
      dat_train <- train_test$dat_train
      dat_test <- train_test$dat_test
      
      # Fit-predict different methods
      fit_pred <- fit_predict_wrapper(
        dat_train = dat_train, 
        dat_test = dat_test, 
        response = response,
        methods = methods, 
        quantiles = quantiles,
        cv_erf = TRUE
      )
      
      # Compute predicted quantiles
      pred_quantiles <- lopred2res(fit_pred, dat_test) %>% 
        select(!starts_with("N_"))
      
      # Save predicted quantiles
      safe_save_rds(pred_quantiles, 
                    add_word_before_ext(file_quantiles,
                                        paste0("noisevars=", noise_vars[p])))
      
      # Compute predicted parameters
      fit_erf <- fit_pred["erf" == map_chr(methods, function(el){el$name})][[1]]$fit
      mat_train <-  get_regression_data(dat = dat_test, response = response)
      
      params <- erf:::predict_gpd_params(object = fit_erf, newdata = mat_train$X)
      
      pred_params <- bind_cols(dat_test, params) %>% 
        pivot_longer(cols = c(sigma, xi), 
                     names_to = "param",
                     values_to = "param_value") %>% 
        mutate(param = refactor_param(param)) %>% 
        select(!starts_with("N_"))
      
      # Save predicted parameters
      safe_save_rds(pred_params, 
                    add_word_before_ext(file_gpdparams,
                                        paste0("noisevars=", noise_vars[p])))
      
      
    }
  
  sink(file = file_log, append = TRUE)
  cat("\n Time \n")
  print(proc.time() - ptm)
  sink()
  
}



#' Function to run cross-validation on real-data
#' 
#' @param dat `tibble` containing the dataset
#' 
#' @param response `character` name of the response variables
#' 
#' @param methods `list` containing the name of the methods to fit and their 
#'  tuning parameters
#'  
#' @param quantiles `numeric_vector` containing the quantile levels to predict
#' 
#' @param noise_vars `numeric_vector` with number of noise variables to keep in 
#'  `dat`
#'  
#' @param n_workers `integer` represents number of independent R session to run
#'  in parallel
#'  
#' @param prop_train `numeric(0, 1)` represents fraction of data used to train
#'  the methods
#'  
#' @param file_log `character` filepath to save logs
#' 
#' @param file_cv `character` filepath to save cross-validation results
#' 
#' @param seed `integer` to reproduce results
#' 
#' @param k `integer` number of repetitions of cross-validation
#'
run_wagedata_cv <- function(dat, response, methods, quantiles,
                            noise_vars, n_workers, prop_train, file_log,
                            file_cv, seed, k){
  
  # set seed
  set.seed(seed)
  
  # Constants
  dat_val <- dat
  NP <- length(noise_vars)
  
  # Create folds for cv
  folds <- create_folds(n = nrow(dat_val), K = k)
  
  # Set rng to reproduce parallel loop
  rng <- rngtools::RNGseq(NP * k)
  
  # Set up cluster and run loop
  doFuture::registerDoFuture()
  future::plan(future::multisession, workers = n_workers)
  
  # Loop over noise_vars and folds
  ptm <- proc.time()
  cat("****", "Cross-validation of ERF, GRF, GBEX and Unconditional GPD",
      "**** \n", file = file_log)
  
  res <- foreach(p = seq_len(NP), 
                 .combine = bind_rows, 
                 .options.future = list(scheduling = FALSE)) %:%
    foreach(i = seq_len(k),
            .combine = bind_rows, 
            .options.future = list(scheduling = FALSE)) %dopar% 
    {
      
      # log
      cat("Simulation", p, "out of", length(noise_vars), 
          "--- Inner iteration", i, "out of", k,
          "PID:", Sys.getpid(), "\n", 
          file = file_log, append = TRUE)
      
      
      # Set RNG seed
      r = (p - 1) * k + i
      rngtools::setRNG(rng[[r]])
      
      # Choose correct number of noise variables
      dat_val_noise <- dat_val %>% 
        select(!starts_with("N_") | num_range("N_", seq_len(noise_vars[p])))
      
      # Split into train and test
      dat_train <- dat_val_noise[folds[[i]], ]
      dat_test <- dat_val_noise[-folds[[i]], ]
      
      # Fit predict methods
      fit_pred <- fit_predict_wrapper(
        dat_train = dat_train, dat_test = dat_test, 
        response = response,
        methods = methods, quantiles = quantiles,
        cv_erf = TRUE
      )
      
      # Collect results
      res_iter <- lopred2res(fit_pred, dat_test)
      
      # Compute losses
      losses <- res_iter %>% 
        group_by(method, tau, tau_factor) %>% 
        summarise(loss_wang = quantile_loss_wang_vec(!!sym(response),
                                                     quantile, tau)) 
      
      tibble(n_noise_vars = noise_vars[p],
             ncolss = ncol(dat_val_noise),
             iter = i) %>% 
        bind_cols(losses)
      
    }
  
  sink(file = file_log, append = TRUE)
  cat("\n Time \n")
  print(proc.time() - ptm)
  sink()
  
  
  # save rds
  safe_save_rds(res, file_cv)
}


create_noise_variables <- function(n, p){
  ## integer integer -> tibble (n x p)
  ## create tibble with n x p uniform random variables in [-1, 1]
  
  matrix(runif(n = n * p, min = -1, max = 1),
         nrow = n,
         ncol = p) %>% 
    as_tibble(.name_repair = ~ paste0("N_", seq_len(p), recycle0 = TRUE))
}
