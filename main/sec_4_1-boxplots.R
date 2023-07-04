# Dependency imports
source("main/dependencies.R")

# Constant definitions
RUN_SIMULATION <- FALSE
CONFIG <- "configs/exp_sec_4_1_boxplots.json"
FILE <- "output/exp_sec_4_1_boxplots/res_models--2021-08-11--11_35_06.rds"
METHODS_OUTLIERS <- c("GRF", "QRF")

figure_path <- here("figures", "boxplots_ise_vs_methods_step.pdf")


# Run simulations
if (RUN_SIMULATION) {
    res <- run_simulations(CONFIG)
} else {
    res <- read_rds(FILE)
}


# Plot results
dat <- res %>%
    select(-quantiles) %>%
    unnest(res) %>%
    filter(quantiles %in% c(0.9995)) %>%
    filter(method != "full_unc") %>%
    mutate(deg_fr = refactor_distr_df(distr, df, param = "xi")) %>%
    group_by(method, deg_fr, quantiles) %>%
    mutate(
        quantiles = texify_column(quantiles, "\\tau"),
        method = refactor_methods(method, short_names = TRUE)
    )

dat_boxplots <- dat %>%
    summarise(compute_boxplot_stats(ise))

dat_outlier <- dat %>%
    mutate(compute_boxplot_stats(ise)) %>%
    mutate(
        ise_out = if_else(ise < ymin | ise > ymax,
            ise, NaN
        ),
        ise_out = if_else(ise_out > 3 * ymax & method %in% METHODS_OUTLIERS,
            NaN, ise_out
        )
    )

gg <- ggplot() +
    facet_grid(~deg_fr,
        scales = "free",
        labeller = label_parsed
    ) +
    geom_point(
        aes(x = method, y = sqrt(ise_out), col = method),
        data = dat_outlier
    ) +
    geom_boxplot(
        aes(
            x = method,
            lower = sqrt(lower),
            middle = sqrt(middle),
            upper = sqrt(upper),
            ymin = sqrt(ymin),
            ymax = sqrt(ymax),
            col = method
        ),
        data = dat_boxplots,
        width = 3 / 4,
        stat = "identity"
    ) +
    # geom_boxplot(
    #   aes(x = method,
    #       lower = NaN,
    #       middle = sqrt(mean),
    #       upper = NaN,
    #       ymin = NaN,
    #       ymax = NaN),
    #   data = dat_boxplots,
    #   width = 3/4,
    #   stat = "identity"
    # )  +
    stat_summary(
        data = dat_boxplots,
        fun = sqrt,
        aes(x = method, y = mean, col = method),
        shape = "triangle"
    ) +
    scale_color_branded(
        palette = my_palette[-5],
        other = "blue", primary = "red"
    ) +
    labs(colour = "Methods:") +
    xlab("") +
    ylab(expression(sqrt(ISE))) +
    # theme(axis.text.y=element_blank(),
    # legend.position = c(0.5, -0.4), legend.direction = "horizontal") +
    theme(legend.position = "none") +
    coord_flip()
gg

save_myplot(
    plt = gg,
    plt_nm = figure_path,
    width = 1.75,
    height = 1.75
)
