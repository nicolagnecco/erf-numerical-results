# Dependency imports
source("main/dependencies.R")

# Constant definitions
RUN_SIMULATION <- FALSE
CONFIG <- "configs/exp_sec_4_2_boxplots.json"
FILE <- "output/exp_sec_4_2_boxplots/res_models--2021-08-11--12_16_24.rds"

figure_path <- here("figures", "boxplots_ise_vs_methods_.pdf")
figure_path_slides <- here("figures/slides", "boxplots_ise_vs_methods_slides_.pdf")

METHODS_OUTLIERS <- c("GRF", "QRF", "EGP Tail")

# Run simulations
if (RUN_SIMULATION) {
  res <- run_simulations(CONFIG)
} else {
  res <-  read_rds(FILE)
}


# Plot results
dat <- res %>% 
  select(-quantiles) %>% 
  unnest(res) %>% 
  group_by(method, model, quantiles) %>% 
  filter(quantiles %in% c(0.99, 0.995, 0.9995)) %>% 
  filter(method != "full_unc") %>% 
  mutate(quantiles = texify_column(quantiles, "\\tau"),
         method = refactor_methods(method, short_names = TRUE),
         model = refactor_models(model)) 

desc <- glue::glue(
  "_n=", dat$n %>% unique(),
  "_p=", dat$p %>% unique()
)

nmodels <- dat$model %>% unique() %>% length()

dat_boxplots <- dat %>% 
  summarise(compute_boxplot_stats(ise))

dat_outlier <- dat %>% 
  mutate(compute_boxplot_stats(ise)) %>% 
  mutate(ise_out = if_else(ise < ymin | ise > ymax,
                           ise, NaN),
         ise_out = if_else(ise_out > 3 * ymax & method %in% METHODS_OUTLIERS,
                           NaN, ise_out))


gg <- ggplot() +
  facet_nested_wrap(~ model + quantiles, nrow = nmodels, 
                    scales = "free_x", 
                    labeller = label_parsed) +
  geom_point(
    aes(x = method, y = sqrt(ise_out), col = method),
    data = dat_outlier
  ) +
  geom_boxplot(
    aes(x = method, 
        lower = sqrt(lower), 
        middle = sqrt(middle),
        upper = sqrt(upper),
        ymin = sqrt(ymin), 
        ymax = sqrt(ymax), 
        col = method),
    data = dat_boxplots,
    width = 3/4,
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
  stat_summary(data = dat_boxplots, 
               fun = sqrt,
               aes(x= method, y = mean, col = method),
               shape = "triangle") +
  scale_color_branded(palette = my_palette[-5], 
                      other = "blue", primary = "red") +
  labs(colour = "Methods:") +
  xlab("") +
  ylab(expression(sqrt(ISE))) +
  # theme(axis.text.y=element_blank(),
        # legend.position = c(0.5, -0.1), legend.direction = "horizontal") +
  theme(legend.position = "none") +
  coord_flip(); gg

save_myplot(
  plt = gg,
  plt_nm = add_word_before_ext(figure_path, desc),
  width  = 1.65,
  height = 1.65
)


# Plot for slides
dat <- res %>% 
  select(-quantiles) %>% 
  unnest(res) %>% 
  group_by(method, model, quantiles) %>% 
  filter(quantiles %in% c(0.9995)) %>% 
  filter(method != "full_unc") %>% 
  mutate(quantiles = texify_column(quantiles, "\\tau"),
         method = refactor_methods(method),
         model = refactor_models(model)) 

desc <- glue::glue(
  "_n=", dat$n %>% unique(),
  "_p=", dat$p %>% unique()
)

nmodels <- dat$model %>% unique() %>% length()

dat_boxplots <- dat %>% 
  summarise(compute_boxplot_stats(ise))

dat_outlier <- dat %>% 
  mutate(compute_boxplot_stats(ise)) %>% 
  mutate(ise_out = if_else(ise < ymin | ise > ymax,
                           ise, NaN),
         ise_out = if_else(ise_out > 3 * ymax & method %in% METHODS_OUTLIERS,
                           NaN, ise_out))


gg <- ggplot() +
  facet_nested_wrap(~ model, ncol = nmodels, scales = "free", 
                    labeller = label_parsed) +
  geom_point(
    aes(x = method, y = sqrt(ise_out), col = method),
    data = dat_outlier
  ) +
  geom_boxplot(
    aes(x = method, 
        lower = sqrt(lower), 
        middle = sqrt(middle),
        upper = sqrt(upper),
        ymin = sqrt(ymin), 
        ymax = sqrt(ymax), 
        col = method),
    data = dat_boxplots,
    width = 3/4,
    stat = "identity"
  ) +
  geom_boxplot(
    aes(x = method, 
        lower = NaN,
        middle = sqrt(mean), 
        upper = NaN,
        ymin = NaN, 
        ymax = NaN),
    data = dat_boxplots,
    width = 3/4,
    stat = "identity"
  )  +
  scale_color_branded(palette = my_palette[-5], 
                      other = "blue", primary = "red") +
  labs(colour = "Methods:") +
  xlab("") +
  ylab(expression(sqrt(ISE))) +
  theme(axis.text.y=element_blank()) +
  theme(legend.position = c(0.5, -0.4), legend.direction = "horizontal") +
  coord_flip(); gg

save_myplot(
  plt = gg,
  plt_nm = add_word_before_ext(figure_path_slides, desc),
  width  = 1.65,
  height = 1.65,
  cairo = FALSE
)

