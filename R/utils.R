add_word_before_ext <- function(s1, s2){
  ## character -> character
  ## produce string `s1_s2.ext`
  
  paste0(tools::file_path_sans_ext(s1), "_", s2, ".", tools::file_ext(s1))
}

add_timestamp <- function(str){
  ## character -> character
  ## add timestamp to str
  dttime <- gsub(pattern = " ", 
                 gsub(pattern = ":", x = Sys.time(), replacement = "_"),
                 replacement = "--")
  paste0(tools::file_path_sans_ext(str), "--", dttime, ".", tools::file_ext(str))
}

safe_save_rds <- function(object, file_path){
  ## object character -> ___
  ## save object to file_path. If dirname(file_path) does not exists, create it
  
  dir_name <- dirname(file_path)
  if (!file.exists(dir_name)){
    dir.create(dir_name)
  }
  
  saveRDS(object, file = file_path)
  
}

rep_tibble <- function(tbl, m){
  ## tibble integer -> tibble
  ## replicate tibble m times and append named column rep_id = 1:m
  
  tbl <-  tbl %>% 
    rownames_to_column()
  
  expand_grid(rep_id = 1:m,
              rowname = tbl$rowname) %>% 
    left_join(tbl, by = "rowname") %>% 
    select(-rowname)
  
}

rep_vector2matrix <- function(vec, nrep, dim = c("row", "col")){
  ## vector integer character -> matrix
  ## stack nrep of vec in (row|col) of a matrix
  
  dim <- match.arg(dim)
  l <- length(vec)
  
  if (l == 0){
    stop ("vec must contain at least one element.")
  }
  
  if (dim == "col"){
    matrix(vec, nrow = l, ncol = nrep)
  } else {
    matrix(vec, nrow = nrep, ncol = l, byrow = TRUE)
  }
}

list2matrix <- function(lst, dim = c("row", "col")){
  ## list character -> matrix
  ## stack element of lst in (row|col) of a matrix
  
  dim <- match.arg(dim)
  l <- length(lst)
  
  if (l == 0){
    stop ("lst must contain at least one element.")
  }
  
  if (dim == "col"){
    matrix(unlist(lst), ncol = l)
  } else {
    matrix(unlist(lst), nrow = l, byrow = TRUE)
  }
}

matrix2list <- function(mat){
  ## numeric_matrix -> list
  ## produces a list with elements corresponding to rows of mat
  split(mat, rep(1:nrow(mat), times = ncol(mat)))
}

pred2tibble <- function(pred, X_test, quantiles, method) {
  ## numeric_matrix numeric_matrix numeric_vector character -> tibble
  ## produce tibble with cols `quantile_level`, `quantile`, `X1`, `method`
  pred %>% 
    as_tibble(.name_repair = ~ paste0("q = ", quantiles)) %>% 
    bind_cols(as_tibble(X_test[, 1], .name_repair = ~ "X1")) %>% 
    pivot_longer(cols = matches("q = "), 
                 names_to = "quantile_level", 
                 values_to = "quantile") %>% 
    mutate(method = method)
}

square_loss <- function(y, y_hat){
  ## numeric_vector numeric_vector -> numeric
  ## produce square loss
  (y - y_hat) ^ 2
}

quantile_loss <- function(y, q, p){
  ## numeric_vector numeric_vector numeric(0, 1) -> numeric
  ## compute quantile loss function 
  
  abs_diff <- abs(y - q)
  
  abs_diff[y > q] <- abs_diff[y > q] * p
  abs_diff[!(y > q)] <- abs_diff[!(y > q)] * (1 - p)
  
  return(abs_diff)
}

quantile_loss_vec <- function(y, q, p){
  ## numeric_vector numeric_vector numeric_vector(0, 1) -> numeric_vector
  ## compute quantile loss function 
  
  if (length(y) != length(q) | length(q) != length(p)){
    stop("Vectors `y`, `q`, `p` must have all the same number of elements.",
         call. = FALSE)
  }
  
  abs(y - q) * ((y > q) * p + (y <= q) * (1 - p))
  
}

quantile_loss_wang <- function(y, q, tau) {
  ## numeric_vector numeric_vector numeric(0, 1) -> numeric
  ## returns custom loss proposed by Wang et al. (2013)
  abs(sum((y < q) - tau) / sqrt((length(y) * tau * (1 - tau))))
}

quantile_loss_wang_vec <- function(y, q, tau_vec) {
  ## numeric_vector numeric_vector numeric(0, 1) -> numeric_vector
  ## returns custom loss proposed by Wang et al. (2013)
  abs(sum(((y < q) - tau_vec) / sqrt(length(y) * tau_vec * (1 - tau_vec))))
}

normalize <- function(x){
  ## numeric_vector -> numeric_vector
  ## normalize numeric vector
  
  if (var(x) == 0){
    y <- scale(x, scale = FALSE)
  } else {
    y <- scale(x)
    # (x - min(x))/(max(x) - min(x))
  }
  as.numeric(y)
}

set_parallel_strategy <- function(
  strategy = c("sequential", "parallel", "mixed"), n_workers){
  ## character integer -> ___
  ## set up the evaluation strategy
  
  strategy <- match.arg(strategy)
  
  doFuture::registerDoFuture()
  if(strategy == "parallel"){
    future::plan(future::multisession, workers = n_workers)
  } else if (strategy == "sequential"){
    future::plan(future::sequential)
  } else if (strategy == "mixed"){
    strategy_1 <- future::tweak(future::sequential)
    strategy_2 <- future::tweak(future::multisession, workers = n_workers)
    future::plan(list(strategy_1, strategy_2))
  }
}

check_X_matrix <- function(X, n, p){
  ## numeric_matrix integer integer -> boolean
  ## produce true if X is a matrix with dimension n * p
  
  cond_1 <- is.matrix(X)
  
  if (cond_1){
    cond_2 <- all.equal(dim(X), c(n, p))
  } else {
    cond_2 <- FALSE
  }
  
  if (cond_1 & cond_2){
    return(TRUE)
  } else {
    stop(paste0("X must be a matrix with ", deparse(substitute(n)),
                " rows and ", deparse(substitute(p)), " columns."))
  }
}

uniform_confidence_bands <- function (n_obs, n_rep) {
  ## integer (2x) -> tibble
  ## produce tibble with confidence bands from uniform dist.
  
  theoretical_quantiles <- stats::qunif((1:n_obs)/(n_obs + 1))
  
  res <- purrr::map_dfr(seq_len(n_rep),
                        function(i){
                          random_quantiles <- sort(stats::runif(n = n_obs))
                          tibble(n_obs = 1:n_obs,
                                 n_rep = i,
                                 theoretical_quantiles = theoretical_quantiles,
                                 random_quantiles = random_quantiles)
                        })
  res %>% 
    group_by(theoretical_quantiles) %>% 
    summarise(q_low = quantile(random_quantiles, 0.025),
              q_hi = quantile(random_quantiles, 0.975))
}

exponential_confidence_bands <- function (n_obs, n_rep) {
  ## integer (2x) -> tibble
  ## produce tibble with confidence bands from standard exponential dist.

  theoretical_quantiles <- stats::qexp((1:n_obs)/(n_obs + 1))
  
  
  res <- purrr::map_dfr(seq_len(n_rep),
                        function(i){
                          random_quantiles <- sort(stats::rexp(n = n_obs))
                          tibble(n_obs = 1:n_obs,
                                 n_rep = i,
                                 theoretical_quantiles = theoretical_quantiles,
                                 random_quantiles = random_quantiles)
                        })
  res %>% 
    group_by(theoretical_quantiles) %>% 
    summarise(q_low = quantile(random_quantiles, 0.025),
              q_hi = quantile(random_quantiles, 0.975))
}

is_of_class <- function(obj, class_nms) {
  ## object character_vector -> boolean
  ## produce true if `obj` is of one of the classes in `class_nms`
  any(
    purrr::map_lgl(class_nms, function(cn){
      any(class(obj) == cn)
    })
  )
}

split_data <- function(dat, prop_train) {
  ## tibble numeric(0, 1) -> list(tibble, tibble, numeric_vector)
  ## splits data into train and test returning
  ## list(dat_train, dat_test, train_id)
  
  train_id <- sample(x = 1:nrow(dat), 
                     size = floor(nrow(dat) * prop_train))
  return(list(
    dat_train = dat[train_id, ],
    dat_test = dat[-train_id, ],
    train_id = train_id
  ))
}

get_regression_data <- function(dat, response) {
  ## tibble character -> list(X = numeric_matrix, Y = numeric_vector)
  ## splits `dat` into a named list with covariates and response.
  ## NOTE: factors are converted to dummy variables
  
  if (any(map_chr(head(dat, 0), class) == "factor")){
    dat <- dat %>%
      fastDummies::dummy_cols(remove_selected_columns = TRUE) 
  }
  
  X <- dat %>%
    select(-all_of(c(response))) %>%
    as.matrix()
  Y <- dat %>%
    select(all_of(c(response))) %>%
    as.matrix()
  
  return(list(X = X, Y = Y[, 1]))
}

#' Converts a `prediction` into a `tibble`  suitable for analysis.
#' 
#' @param pred A `list` of 
#'   - `"name" = character`
#'   - `"params" = named_list`
#'   - `"preds" = numeric_matrix`
#'   - `"quantiles" = numeric_vector`
#'   - `"fit" = erf` (optional)
#' 
#' @param dat A `tibble` representing the data on which predictions where made.
#' 
#' @return A `tibble` made of `dat` and `preds` joined together.
#' 
pred2res <- function(pred, dat){
  # check # observation in pred$preds == # observations in dat
  if (nrow(pred$preds) != nrow(dat)){
    stop(paste0("Prediction matrix `pred$preds` must have",
    " the same number of observations as `dat`."))
  }
  
  # join `pred` and `dat` together
  as_tibble(pred$preds, .name_repair = ~ paste0("q = ", pred$quantiles)) %>% 
    mutate(method = pred$name) %>% 
    bind_cols(dat) %>% 
    select(method, everything()) %>% 
    pivot_longer(cols = matches("q = "), 
                 names_to = "tau_factor", 
                 values_to = "quantile") %>% 
    mutate(tau = as.numeric(str_sub(tau_factor, start = 5)))
}

#' Converts a list of  `predictions` into a `tibble` suitable for analysis.
#' 
#' @param lopred A `list` of `predictions`.
#' 
#' @param dat A `tibble` representing the data on which predictions 
#'   where made.
#' 
#' @return A `tibble` made of row binding `pred2res(lopred[[i]], dat)` for each
#'   element `i` of `lopred`.
#'   
lopred2res <- function(lopred, dat){
  purrr::reduce(.x = purrr::map(lopred, pred2res, dat),
                .f = bind_rows)
}

erf2params <- function(erf, dat){
  
  erf:::predict_gpd_params(erf, )
}

create_folds <- function (n, K, seed) {
  if (K < 2){
    stop("`K` must be larger than 1.", call. = FALSE)
  }
  rows_id <- seq_len(n)
  chunk(sample(rows_id), K)
}

chunk <- function (x, K) {
  unname(split(x, factor(sort(rank(x)%%K))))
}

