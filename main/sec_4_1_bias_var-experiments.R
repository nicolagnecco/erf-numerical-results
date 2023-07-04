# Dependency imports
source("main/dependencies.R")

# Constant definitions
RUN_SIMULATION <- FALSE
CONFIG <- "configs/exp_sec_4_1_bias_var.json"
FILE <- "output/exp_sec_4_1_bias_var/res_models--2022-09-16--16_40_16.rds"

figure_path <- here("figures", "bias_var.pdf")


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
  
x_names <- dat %>% 
  select(all_of(starts_with("X"))) %>%
  unique() %>% 
  rownames_to_column(var = "X_i")

dat <- dat %>% 
  left_join(x_names)

dat2 <- dat %>% 
  select(nsim, method, quantiles, p, X_i, predictions) %>% 
  pivot_wider(names_from = "method", values_from = "predictions") %>% 
  pivot_longer(
    cols = !all_of(c("nsim", "quantiles", "p", "X_i", "truth")),
    names_to = "method", values_to = "predictions"
  ) %>% 
  group_by(method, quantiles, p, X_i) %>% 
  mutate(avg_predictions = mean(predictions)) %>% 
  mutate(bias_2 = (avg_predictions - truth)^2,
         var = var(predictions)) %>% 
  summarise(bias_2 = mean(bias_2),
            var = mean(var)) %>% 
  summarise(bias_2 = mean(bias_2),
            var = mean(var)) %>% 
  mutate(mise = bias_2 + var)

dat3 <- dat2 %>% 
  pivot_longer(
    cols = !all_of(c("method", "quantiles", "p")),
    names_to = "biasvar", values_to = "result"
  )


# wrt p
dat2plot <- dat3 %>%
  filter(quantiles %in% c(0.9995)) %>% 
  group_by(method, quantiles, p) %>% 
  mutate(quantiles = texify_column(quantiles, "\\tau"),
         method = refactor_methods(method),
         biasvar = factor(biasvar, 
                          levels = c("mise", "bias_2", "var"), 
                          labels = c("MISE", TeX("bias$^2$"), "variance")))


gg <- ggplot(data = dat2plot) +
  facet_nested_wrap(~ biasvar + quantiles, nrow = 1,
                    labeller = label_parsed, scales = "free_y") +
  geom_point(aes(x = p, y = result, col = method, shape = method)) +
  geom_line(aes(x = p, y = result, col = method, lty = method), size = 3/4) +
  scale_color_branded(palette = my_palette, other = "blue", primary = "red") +
  scale_shape_manual(values = c(19, 17, 15, 4, 8, 1, 2)) +
  xlab(TeX("$p$")) +
  labs(colour = "Methods:", shape = "Methods:", lty = "Methods:") +
  # theme(legend.position =c(0.5, -0.25), legend.direction = "horizontal") +
  theme(axis.text = element_text(size = 7)) +
  ylab(""); gg



# wrt quantiles
dat2plot <- dat3 %>% 
  filter(p == 10) %>% 
  group_by(method, quantiles, p) %>% 
  mutate(p = texify_column(p, "p"),
         method = refactor_methods(method),
         biasvar = factor(biasvar, 
                          levels = c("mise", "bias_2", "var"), 
                          labels = c("MISE", TeX("bias$^2$"), "variance")))

tbl_tau <- tibble(quantiles = dat2plot$quantiles %>% unique(),
                  quantiles_t = 1:4)

dat2plot <- dat2plot %>% 
  left_join(tbl_tau, by = "quantiles")

gg2 <- ggplot(data = dat2plot) +
  facet_nested_wrap(~ biasvar + p, nrow = 1,
                    labeller = label_parsed, scales = "free_y") +
  geom_point(aes(x = quantiles_t, y = result, col = method,
                 group = method, shape = method)) +
  geom_line(aes(x = quantiles_t, y = result, col = method,
                group = method, lty = method), size = 3/4) +
  scale_color_branded(palette = my_palette, other = "blue", primary = "red") +
  scale_x_continuous(breaks = tbl_tau$quantiles_t, 
                     labels = tbl_tau$quantiles) +
  scale_shape_manual(values = c(19, 17, 15, 4, 8, 1, 2)) +
  labs(colour = "Methods:", shape = "Methods:", lty = "Methods:") +
  theme(legend.position =c(0.5, -0.2), legend.direction = "horizontal") +
  xlab(TeX("$\\tau$")) +
  ylab("") +
  # theme(axis.text.x = element_text(angle = 90)) +
  theme(axis.text = element_text(size = 7)); gg2


gg3 <- plot_grid(gg2 + theme(legend.position = "none"), 
                 gg + theme(legend.position = "none"), 
                 nrow = 2)

legend <- get_legend(
  # create some space to the left of the legend
  gg + 
    guides(color = guide_legend(nrow = 1)) +
    theme(legend.position = "bottom")
)


gg4 <- ggarrange(gg2, NULL, gg,
                nrow = 3, ncol = 1, align = "hv",
                heights = c(1, -0.0, 1),
                legend = "bottom", legend.grob = legend)

save_myplot(
  plt = gg4,
  plt_nm = figure_path,
  width = 6.5,
  height = 5.5
)

# sanity check
dat4 <- dat2 %>% 
  mutate(result = sqrt(bias_2 + var))


# wrt p
dat2plot <- dat4 %>%
  filter(quantiles %in% c(0.9995)) %>% 
  group_by(method, quantiles, p) %>% 
  mutate(quantiles = texify_column(quantiles, "\\tau"),
         method = refactor_methods(method))


gg <- ggplot(data = dat2plot) +
  facet_nested_wrap(~ quantiles, nrow = 1,
                    labeller = label_parsed, scales = "free_y") +
  geom_point(aes(x = p, y = result, col = method, shape = method)) +
  geom_line(aes(x = p, y = result, col = method, lty = method), size = 3/4) +
  scale_color_branded(palette = my_palette, other = "blue", primary = "red") +
  scale_shape_manual(values = c(19, 17, 15, 4, 8, 1, 2)) +
  xlab(TeX("$p$")) +
  labs(colour = "Methods:", shape = "Methods:", lty = "Methods:") +
  # theme(legend.position =c(0.5, -0.25), legend.direction = "horizontal") +
  theme(axis.text = element_text(size = 7)) +
  ylab(""); gg

# wrt quantiles
dat2plot <- dat4 %>% 
  filter(p == 10) %>% 
  group_by(method, quantiles, p) %>% 
  mutate(p = texify_column(p, "p"),
         method = refactor_methods(method))

tbl_tau <- tibble(quantiles = dat2plot$quantiles %>% unique(),
                  quantiles_t = 1:4)

dat2plot <- dat2plot %>% 
  left_join(tbl_tau, by = "quantiles")

gg2 <- ggplot(data = dat2plot) +
  facet_nested_wrap(~ p, nrow = 1,
                    labeller = label_parsed, scales = "free_y") +
  geom_point(aes(x = quantiles_t, y = result, col = method,
                 group = method, shape = method)) +
  geom_line(aes(x = quantiles_t, y = result, col = method,
                group = method, lty = method), size = 3/4) +
  scale_color_branded(palette = my_palette, other = "blue", primary = "red") +
  scale_x_continuous(breaks = tbl_tau$quantiles_t, 
                     labels = tbl_tau$quantiles) +
  scale_shape_manual(values = c(19, 17, 15, 4, 8, 1, 2)) +
  labs(colour = "Methods:", shape = "Methods:", lty = "Methods:") +
  theme(legend.position =c(0.5, -0.2), legend.direction = "horizontal") +
  xlab(TeX("$\\tau$")) +
  ylab("") +
  # theme(axis.text.x = element_text(angle = 90)) +
  theme(axis.text = element_text(size = 7)); gg2
