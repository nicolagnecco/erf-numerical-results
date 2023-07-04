# Dependency imports
source("main/dependencies.R")


# Constant definitions
CONFIG_1 <- "configs/exp_sec_4_2_hill_vs_erf.json"
# CONFIG_2 <- "configs/exp_sec_3_cv_lambda.json"


res1 <- run_simulations(CONFIG_1)
# res2 <- run_simulations(CONFIG_2)
