# Dependency imports
source("main/dependencies.R")


# Constant definitions
RUN_SIMULATION <- FALSE
CONFIG_1 <- "configs/exp_sec_3_lambda.json"
CONFIG_2 <- "configs/exp_sec_3_cv_lambda.json"

FILE_1 <- "output/exp_sec_3_lambda/res_models--2021-03-23--10_01_24.rds"
FILE_2 <- "output/exp_sec_3_lambda/res_models_cv_lambda--2021-03-23--10_09_34.rds"

figure_path <- here("figures", "cv_lambda.pdf")

# Run simulations
if (RUN_SIMULATION) {
    res1 <- run_simulations(CONFIG_1)
    res2 <- run_simulations(CONFIG_2)
} else {
    res1 <- read_rds(FILE_1)
    res2 <- read_rds(FILE_2)
}



# CV wrt to lambda
quantile_levels <- c(.99, .995, .9995)
kappa_base <- 5

dat_ise <- res1 %>%
    select(-quantiles) %>%
    unnest(res) %>%
    select("method", "quantiles", "ise", "nsim", "min.node.size", "lambda", "rng")

dat_cv <- res2 %>%
    select(-quantiles) %>%
    unnest(res) %>%
    filter(
        min.node.size == kappa_base,
        quantiles %in% quantile_levels
    ) %>%
    select("quantiles", "nsim", "best.min.node.size", "best.lambda", "rng") %>%
    rename(
        "min.node.size" = "best.min.node.size",
        "lambda" = "best.lambda"
    ) %>%
    left_join(dat_ise, by = c(
        "nsim", "min.node.size", "lambda",
        "quantiles", "rng"
    )) %>%
    group_by(method, quantiles) %>%
    summarise(rmise = sqrt(mean(ise))) %>%
    mutate(
        method = "erf_cv",
        min.node.size = NaN
    ) %>%
    mutate(
        method = refactor_methods(method),
        quantiles = texify_column(quantiles, "\\tau")
    )


tbl_trans <- tibble(
    lambda = dat_ise$lambda %>% unique(),
    lambda_t = 1:10
)

dat_not_cv <- dat_ise %>%
    filter(
        min.node.size == kappa_base,
        quantiles %in% quantile_levels
    ) %>%
    group_by(method, quantiles, min.node.size, lambda) %>%
    summarise(rmise = sqrt(mean(ise))) %>%
    mutate(method = refactor_methods(method)) %>%
    left_join(tbl_trans, by = "lambda") %>%
    mutate(quantiles = texify_column(quantiles, "\\tau"))


gg <- ggplot() +
    facet_nested_wrap(~quantiles,
        nrow = 1, scales = "free",
        labeller = label_parsed
    ) +
    geom_hline(
        data = dat_cv,
        mapping = aes(yintercept = rmise, lty = method)
    ) +
    geom_line(
        data = dat_not_cv,
        mapping = aes(x = lambda_t, y = rmise, lty = method)
    ) +
    geom_point(
        data = dat_not_cv,
        mapping = aes(x = lambda_t, y = rmise),
        shape = 20, size = 3
    ) +
    geom_point(
        data = dat_not_cv,
        mapping = aes(x = lambda_t, y = rmise),
        shape = 20, color = "white", size = 2
    ) +
    scale_x_continuous(
        minor_breaks = NULL,
        breaks = tbl_trans$lambda_t,
        labels = tbl_trans$lambda
    ) +
    labs(colour = "Methods:", shape = "Methods:", lty = "Methods:") +
    xlab(TeX("$\\lambda")) +
    ylab(expression(sqrt(MISE))) +
    theme(axis.text.x = element_text(angle = 90))
gg

save_myplot(
    plt = gg,
    plt_nm = figure_path,
    width = 1.5,
    height = 1.5
)
