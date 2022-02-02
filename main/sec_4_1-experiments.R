# Dependency imports
source("main/dependencies.R")

# Constant definitions
RUN_SIMULATION <- FALSE
CONFIG <- "configs/exp_sec_4_1.json"
FILE <- "output/exp_sec_4_1/res_models--2021-08-11--11_15_44.rds"

figure_path <- here("figures", "step_mise.pdf")


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
  filter(method != "full_unc")


# wrt p
dat2plot <- dat %>%
  filter(quantiles %in% c(0.9995)) %>%  
  filter(p != 7) %>% 
  group_by(method, quantiles, p) %>% 
  summarise(rmise = sqrt(mean(ise))) %>% 
  mutate(quantiles = texify_column(quantiles, "\\tau"),
         method = refactor_methods(method))


gg <- ggplot(data = dat2plot) +
  facet_nested_wrap(~ quantiles, nrow = 1,
                    labeller = label_parsed, scales = "free_y") +
  geom_point(aes(x = p, y = rmise, col = method, shape = method)) +
  geom_line(aes(x = p, y = rmise, col = method, lty = method), size = 3/4) +
  scale_color_branded(palette = my_palette, other = "blue", primary = "red") +
  scale_shape_manual(values = c(19, 17, 15, 4, 8, 1, 2)) +
  xlab(TeX("$p$")) +
  labs(colour = "Methods:", shape = "Methods:", lty = "Methods:") +
  # theme(legend.position =c(0.5, -0.25), legend.direction = "horizontal") +
  theme(axis.text = element_text(size = 7)) +
  ylab(""); gg



# wrt quantiles
dat2plot <- dat %>% 
  filter(p == 10) %>% 
  group_by(method, quantiles, p) %>% 
  summarise(rmise = sqrt(mean(ise))) %>% 
  mutate(p = texify_column(p, "p"),
         method = refactor_methods(method))

tbl_tau <- tibble(quantiles = dat2plot$quantiles %>% unique(),
                  quantiles_t = 1:7)

dat2plot <- dat2plot %>% 
  left_join(tbl_tau, by = "quantiles")

gg2 <- ggplot(data = dat2plot) +
  facet_nested_wrap(~ p, nrow = 1,
                    labeller = label_parsed, scales = "free_y") +
  geom_point(aes(x = quantiles_t, y = rmise, col = method,
                 group = method, shape = method)) +
  geom_line(aes(x = quantiles_t, y = rmise, col = method,
                group = method, lty = method), size = 3/4) +
  scale_color_branded(palette = my_palette, other = "blue", primary = "red") +
  scale_x_continuous(breaks = tbl_tau$quantiles_t, 
                     labels = tbl_tau$quantiles) +
  scale_shape_manual(values = c(19, 17, 15, 4, 8, 1, 2)) +
  labs(colour = "Methods:", shape = "Methods:", lty = "Methods:") +
  theme(legend.position =c(0.5, -0.2), legend.direction = "horizontal") +
  xlab(TeX("$\\tau$")) +
  # theme(axis.text.x = element_text(angle = 90)) +
  theme(axis.text = element_text(size = 7)) +
  ylab(expression(sqrt(MISE))); gg2


gg3 <- plot_grid(gg2 + theme(legend.position = "none"), 
                 gg + theme(legend.position = "none"))

legend <- get_legend(
  # create some space to the left of the legend
  gg + 
    guides(color = guide_legend(nrow = 1)) +
    theme(legend.position = "bottom")
)

gg4 <- plot_grid(gg3, legend, ncol = 1, 
                 rel_heights = c(1, .1)); gg4


save_myplot(
  plt = gg4,
  plt_nm = figure_path,
  width = 5 * 1.25,
  height = 2.5 * 1.25
)
