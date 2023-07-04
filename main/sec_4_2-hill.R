# Dependency imports
source("main/dependencies.R")

# Constant definitions
RUN_SIMULATION <- FALSE
CONFIG <- "configs/exp_sec_4_2_hill_vs_erf.json"
FILE <- "output/exp_sec_4_2_hill/res_models--2023-06-18--22_04_35.rds"

figure_path <- here("figures", "hill_vs_erf_main_paper.pdf")
figure_path_2 <- here("figures", "hill_vs_erf_appendix.pdf")
figure_path_3 <- here("figures", "hill_vs_erf_boxplots.pdf")


methods_tbl <- list(
    c("method" = lst_methods$erf, "shape" = 21, "color" = "black"),
    c("method" = lst_methods$hill, "shape" = 22, "color" = "black"),
    c("method" = lst_methods$shape_rf, "shape" = 24, "color" = "black")
) %>%
    purrr::transpose() %>%
    as_tibble() %>%
    unnest(cols = c(method, color, shape)) %>%
    mutate(shape = as.numeric(shape))

my_shapes <- methods_tbl %>%
    select(method, shape) %>%
    deframe()

my_colors <- methods_tbl %>%
    select(method, color) %>%
    deframe()



# Run simulations
if (RUN_SIMULATION) {
    res <- run_simulations(CONFIG)
} else {
    res <- read_rds(FILE)
}


# Main paper plots
#  model = "step"
#  quantiles = 0.9995
#  distr = "pareto", "student_t"

dat <- res %>%
  select(-quantiles, -intermediate_quantiles) %>% 
    unnest(res) %>%
    drop_na() %>%
    filter(model == "step") %>%
    filter(quantiles == 0.9995) %>%
    filter(distr %in% c("pareto", "student_t")) %>% 
    group_by(n, distr, method, model, quantiles, intermediate_quantile) %>%
    summarise(mise = sqrt(median(ise, na.rm = TRUE))) %>%
    mutate(model = refactor_models(model)) %>%
    mutate(quantiles = texify_column(quantiles, "\\tau")) %>%
    mutate(method = refactor_methods(method, short_names = TRUE)) %>%
    mutate(distr = refactor_distr(distr))

gg <- ggplot(dat, aes(x = intermediate_quantile, y = mise)) +
    facet_nested_wrap(~ distr, 
                      labeller = label_parsed, scales = "free", nrow = 1) +
    stat_summary(aes(color = method, group = method),
        fun = sum, geom = "line", linewidth = .375, linetype = "dashed"
    ) +
    geom_point(aes(shape = method),
        stroke = .75, fill = "white"
    ) +
    scale_color_manual(values = my_colors) +
    scale_shape_manual(values = my_shapes) +
    labs(colour = "Methods:", shape = "Methods:") +
    xlab(TeX("Intermediate quantile level $(\\tau_n)$")) +
    ylab(expression(sqrt(Median ~ ISE))) +
    theme(legend.position = c(0.5, -0.25), legend.direction = "horizontal")

gg

save_myplot(
    plt = gg,
    plt_nm = figure_path,
    width = 2,
    height = 2
)


# Appendix plots
#  Model:  "step", "iid_step", "quadratic2d_tanh"
#  quantiles = 0.9995
#  distr = "pareto", "student_t"

dat <- res %>%
  select(-quantiles, -intermediate_quantiles) %>% 
  unnest(res) %>%
  drop_na() %>%
  filter(model != "iid") %>%
  filter(quantiles == 0.9995) %>%
  filter(distr %in% c("pareto", "student_t")) %>% 
  group_by(n, distr, method, model, quantiles, intermediate_quantile) %>%
  summarise(mise = sqrt(median(ise, na.rm = TRUE))) %>%
  mutate(model = refactor_models(model)) %>%
  mutate(quantiles = texify_column(quantiles, "\\tau")) %>%
  mutate(method = refactor_methods(method, short_names = TRUE)) %>%
  mutate(distr = refactor_distr(distr))

gg <- ggplot(dat, aes(x = intermediate_quantile, y = mise)) +
  facet_nested_wrap(~ model + distr, 
                    labeller = label_parsed, scales = "free", nrow = 3) +
  stat_summary(aes(color = method, group = method),
               fun = sum, geom = "line", linewidth = .375, linetype = "dashed"
  ) +
  geom_point(aes(shape = method),
             stroke = .75, fill = "white"
  ) +
  scale_color_manual(values = my_colors) +
  scale_shape_manual(values = my_shapes) +
  labs(colour = "Methods:", shape = "Methods:") +
  xlab(TeX("Intermediate quantile level $(\\tau_n)$")) +
  ylab(expression(sqrt(Median ~ ISE))) +
  theme(legend.position = c(0.5, -0.10), legend.direction = "horizontal")


save_myplot(
    plt = gg,
    plt_nm = figure_path_2,
    width = 2,
    height = 2
)



# Boxplots
dat <- res %>%
  select(-quantiles, -intermediate_quantiles) %>% 
    unnest(res) %>%
    drop_na() %>%
    filter(model == "step") %>%
    group_by(n, distr, method, model, quantiles, intermediate_quantile) %>%
    mutate(intermediate_quantile = factor(round(intermediate_quantile, 3))) %>%
    mutate(model = refactor_models(model)) %>%
    mutate(method = refactor_methods(method, short_names = TRUE)) %>%
    mutate(distr = refactor_distr(distr))

gg <- ggplot(
    dat %>% filter(quantiles == 0.9995),
    aes(
        x = intermediate_quantile,
        y = sqrt(ise)
    )
) +
    facet_wrap(~ distr + method, scales = "free") +
    facet_grid(distr ~ method, scales = "free") +
    geom_boxplot() +
    scale_color_manual(values = my_colors) +
    scale_shape_manual(values = my_shapes) +
    labs(colour = "Methods:", shape = "Methods:") +
    xlab(TeX("Intermediate quantile level $(\\tau_n)$")) +
    ylab(expression(sqrt(ISE))) +
    theme(legend.position = c(0.5, -0.25), legend.direction = "horizontal")
gg

save_myplot(
    plt = gg,
    plt_nm = figure_path_3,
    width = 2,
    height = 2
)
