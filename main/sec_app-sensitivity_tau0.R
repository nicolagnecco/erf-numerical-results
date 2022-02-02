# Dependency imports
source("main/dependencies.R")

# Constant definitions
RUN_SIMULATION <- FALSE
CONFIG <- "configs/exp_sec_app_sensitivity_tau0.json"
FILE <- "output/exp_sec_app/res_models--2022-01-28--13_08_51.rds"

figure_path <- here("figures", "sensitivity.pdf")


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
  filter(quantiles %in% c(0.99, 0.995)) %>%
  mutate(deg_fr = refactor_distr(distr, df, param = "xi")) %>%
  group_by(method, model, deg_fr, quantiles, intermediate_quantile) %>%
  summarise(rmise = sqrt(mean(ise))) %>%
  mutate(quantiles = texify_column(quantiles, "\\tau"),
         method = refactor_methods(method))

gg <- ggplot(dat %>% 
         filter(intermediate_quantile > .74)) +
  facet_nested_wrap(~quantiles + deg_fr,
                    scales = "free", nrow = 2, labeller = label_parsed) +
  geom_line(aes(x = intermediate_quantile, y = rmise)) +
  geom_point(aes(x = intermediate_quantile, y = rmise), shape = 20, size = 3) +
  geom_point(aes(x = intermediate_quantile, y = rmise), 
             shape = 20, color = "white", size = 2) +
  scale_color_manual("Shape", labels = parse.labels, values = "black") +
  xlab(TeX("$\\tau_0$")) +
  ylab(expression(sqrt(MISE)))

save_myplot(
  plt = gg,
  plt_nm = figure_path,
  width = 1.75,
  height = 1.75
)
