source("main/dependencies.R")

# Constants
OUTPUT_FILE1 <- here("output/exp_sec_5/wage_pred_quant.rds")
OUTPUT_FILE2 <- here("output/exp_sec_5/wage_pred_param.rds")
OUTPUT_FILE3 <- here("output/exp_sec_5/wage_cv.rds")

n_workers <- 10
K <- 10

PROP_DATA <- 1
PROP_TRAIN <- 0.1

RESPONSE <- "wage"

METHODS <- list(
  list(name = "erf", params = list(lambda = 1e-2, # 0.007689705
                                   min.node.size = c(5, 40, 100),
                                   intermediate_quantile = .8)),
  list(name = "grf", params = list()),
  list(name = "gbex", params = list(gbex_cv = FALSE)),
  list(name = "inf", params = list()),
  list(name = "zero", params = list()),
  list(name = "unc_gpd", params = list())
)
QUANTILES <- c(.8, .9, .95, .99, .995)
NOISE_VARS <- c(10)
SEED0 <- 82438535
SEED1 <- 58655138
SEED2 <- 87760048
FILE_LOG <- here("output", "progress.txt")
FUNC_RESPONSE <- c("identity", "log")

# Load data
load("Data/census80.RData")

for (i in seq_along(FUNC_RESPONSE)) {
  
  # Initialize seed
  set.seed(SEED0)
  
  if (FUNC_RESPONSE[i] == "identity"){
    # Prepare data
    dat <- x %>% 
      as_tibble() %>% 
      drop_na() %>% 
      mutate(wage = exp(logwk)) %>% 
      select(wage, everything(), -logwk, -perwt, -exper, -exper2)
  } else if (FUNC_RESPONSE[i] == "log"){
    # Prepare data
    dat <- x %>% 
      as_tibble() %>% 
      drop_na() %>% 
      mutate(wage = logwk) %>% 
      select(wage, everything(), -logwk, -perwt, -exper, -exper2)
  } else {
    stop("Transformations of response must be one of `identity` and `log`.")
  }
  
  # Add noise variables
  dat <-  bind_cols(
    dat,
    create_noise_variables(n = nrow(dat), p = max(NOISE_VARS))
  )
  
  # Split data for EDA and validation
  eda_ind <- sample(nrow(dat), nrow(dat) %/% 2)
  dat_eda <- dat[eda_ind, ] %>% 
    slice_sample(prop = PROP_DATA)
  dat_val <- dat[-eda_ind, ] %>% 
    slice_sample(prop = PROP_DATA)
  
  # 1. EDA
  
  run_wagedata_eda(
    dat = dat_eda, 
    response = RESPONSE, 
    methods = METHODS,
    quantiles = QUANTILES, 
    noise_vars = NOISE_VARS, 
    n_workers = n_workers, 
    prop_train = PROP_TRAIN, 
    file_log = FILE_LOG,
    file_quantiles = add_word_before_ext(OUTPUT_FILE1, FUNC_RESPONSE[i]), 
    file_gpdparams = add_word_before_ext(OUTPUT_FILE2, FUNC_RESPONSE[i]),
    seed = SEED1
  )
  
  
  # 2. Validation
  
  
  run_wagedata_cv(
    dat = dat_val, 
    response = RESPONSE, 
    methods = METHODS,
    quantiles = QUANTILES, 
    noise_vars = NOISE_VARS, 
    n_workers = n_workers, 
    prop_train = PROP_TRAIN, 
    file_log = FILE_LOG,
    file_cv = add_word_before_ext(OUTPUT_FILE3, FUNC_RESPONSE[i]),
    seed = SEED2,
    k = K
  )
  
  
}
