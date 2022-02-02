# Dependency imports
source("main/dependencies.R")

# Constant definitions
n <- 2e3
p <- 40
quantiles <- c(.1, .8, .9, .99, .995,.999,.9995)
quantiles_2 <- c(.8, .9995)
model <- "step"
distr <- "gaussian"
df <- 4
seed <- 158520283
figure_dest_folder <- "figures/slides/"
x_text <- 1.3
text_size <- 3.5
x_test <- -0.25

# Quantile regression ####
# Generate data
set.seed(seed)
train_dat <- generate_joint_distribution(n, p, model = model, df = df, 
                                         distr = distr)
X <- train_dat$X %>%  as.matrix()
Y <- train_dat$Y

theo_quantiles <- generate_theoretical_quantiles(
  quantiles = quantiles, X = X, model = model, distr = distr, df = df
)

dat <- as_tibble(X,
                 .name_repair = ~paste0("X", seq(1, ncol(X)))) %>% 
  bind_cols(as_tibble(Y, .name_repair = ~"Y")) %>% 
  bind_cols(as_tibble(theo_quantiles, 
                      .name_repair = ~paste0("q", 1:length(quantiles)))) %>% 
  mutate(exceed = if_else(Y > q2, TRUE, FALSE),
         Z = if_else(exceed, Y, NaN),
         Z_low = if_else(!exceed, Y, NaN))


# Plots
gg1 <- ggplot(dat) +
  geom_point(aes(x = X1, y = Y), color = my_palette$grey, alpha = 0.5) +
  annotate("text", x = x_text, y = 0, 
           label = TeX("$E\\[Y | X = x\\]$"), 
           color = my_palette$background,
           size = text_size) +
  annotate("text", x = x_text, y = max(dat$q1), 
           label = TeX("$Q_x(0.1)$"), 
           color = my_palette$background,
           size = text_size) +
  annotate("text", x = x_text, y = max(dat$q3), 
           label = TeX("$Q_x(0.9)$"), 
           color = my_palette$background,
           size = text_size) +
  coord_cartesian(xlim = c(-.95, .95), clip = "off") +
  theme(plot.margin = unit(c(1,4.5,1,1), "lines"),
        panel.border = element_rect(size = 1/8))

save_myplot(
  plt = gg1,
  plt_nm = paste0(figure_dest_folder, "plot_01", ".pdf"),
  width = 3,
  height = 3,
  cairo = FALSE
)


gg2 <- ggplot(dat) +
  geom_point(aes(x = X1, y = Y), color = my_palette$grey, alpha = 0.5) +
  geom_line(aes(x = X1, y = 0), linetype = "dashed") +
  annotate("text", x = x_text, y = 0, 
           label = TeX("$E\\[Y | X = x\\]$"), 
           # color = my_palette$background,
           size = text_size) +
  annotate("text", x = x_text, y = min(dat$q1), 
           label = TeX("$Q_x(0.1)$"), 
           color = my_palette$background,
           size = text_size) +
  annotate("text", x = x_text, y = max(dat$q3), 
           label = TeX("$Q_x(0.9)$"), 
           color = my_palette$background,
           size = text_size) +
  coord_cartesian(xlim = c(-.95, .95), clip = "off") +
  theme(plot.margin = unit(c(1,4.5,1,1), "lines"),
        panel.border = element_rect(size = 1/8))

save_myplot(
  plt = gg2,
  plt_nm = paste0(figure_dest_folder, "plot_02", ".pdf"),
  width = 3,
  height = 3,
  cairo = FALSE
)


gg3 <- ggplot(dat) +
  geom_point(aes(x = X1, y = Y), color = my_palette$grey, alpha = 0.5) +
  geom_line(aes(x = X1, y = 0), linetype = "dashed",
            colour = "black") +
  geom_line(aes(x = X1, y = dat$q1), linetype = "dashed",
            colour = "black") +
  geom_line(aes(x = X1, y = dat$q3), linetype = "dashed",
            colour = "black") +
  annotate("text", x = x_text, y = 0, 
           label = TeX("$E\\[Y | X = x\\]$"), 
           # color = my_palette$background,
           size = text_size) +
  annotate("text", x = x_text, y = min(dat$q1), 
           label = TeX("$Q_x(0.1)$"), 
           # color = my_palette$background,
           size = text_size) +
  annotate("text", x = x_text, y = max(dat$q3), 
           label = TeX("$Q_x(0.9)$"), 
           # color = my_palette$background,
           size = text_size) +
  coord_cartesian(xlim = c(-.95, .95), clip = "off") +
  theme(plot.margin = unit(c(1,4.5,1,1), "lines"),
        panel.border = element_rect(size = 1/8))

save_myplot(
  plt = gg3,
  plt_nm = paste0(figure_dest_folder, "plot_03", ".pdf"),
  width = 3,
  height = 3,
  cairo = FALSE
)

# Challenges ####
# Plots
gg4 <- ggplot(dat) +
  geom_point(aes(x = X1, y = Y), color = my_palette$grey, alpha = 0.5) +
  annotate("text", x = x_text, y = max(dat$q2), 
           label = TeX("$Q_x(0.8)$"), 
           color = my_palette$background,
           size = text_size) +
  annotate("text", x = x_text, y = max(dat$q5), 
           label = TeX("$Q_x(0.995)$"), 
           color = my_palette$background,
           size = text_size) +
  coord_cartesian(xlim = c(-.95, .95), clip = "off") +
  theme(plot.margin = unit(c(1,4.5,1,1), "lines"),
        panel.border = element_rect(size = 1/8))

save_myplot(
  plt = gg4,
  plt_nm = paste0(figure_dest_folder, "plot_04", ".pdf"),
  width = 3,
  height = 3,
  cairo = FALSE
)


gg5 <- ggplot(dat) +
  geom_point(aes(x = X1, y = Y), color = my_palette$grey, alpha = 0.5) +
  geom_line(aes(x = X1, y = dat$q2), linetype = "dashed",
            colour = "black") +
  # geom_line(aes(x = X1, y = dat$q5), linetype = "dashed",
  #           colour = "black") +
  annotate("text", x = x_text, y = max(dat$q2), 
           label = TeX("$Q_x(0.8)$"), 
           # color = my_palette$background,
           size = text_size) +
  annotate("text", x = x_text, y = max(dat$q5), 
           label = TeX("$Q_x(0.995)$"), 
           color = my_palette$background,
           size = text_size) +
  coord_cartesian(xlim = c(-.95, .95), clip = "off") +
  theme(plot.margin = unit(c(1,4.5,1,1), "lines"),
        panel.border = element_rect(size = 1/8))

save_myplot(
  plt = gg5,
  plt_nm = paste0(figure_dest_folder, "plot_05", ".pdf"),
  width = 3,
  height = 3,
  cairo = FALSE
)


gg6 <- ggplot(dat) +
  geom_point(aes(x = X1, y = Y), color = my_palette$grey, alpha = 0.5) +
  geom_line(aes(x = X1, y = dat$q2), linetype = "dashed",
            colour = "black") +
  geom_line(aes(x = X1, y = dat$q5), linetype = "dashed",
            colour = "black") +
  annotate("text", x = x_text, y = max(dat$q2), 
           label = TeX("$Q_x(0.8)$"), 
           # color = my_palette$background,
           size = text_size) +
  annotate("text", x = x_text, y = max(dat$q5), 
           label = TeX("$Q_x(0.995)$"), 
           # color = my_palette$background,
           size = text_size) +
  coord_cartesian(xlim = c(-.95, .95), clip = "off") +
  theme(plot.margin = unit(c(1,4.5,1,1), "lines"),
        panel.border = element_rect(size = 1/8))

save_myplot(
  plt = gg6,
  plt_nm = paste0(figure_dest_folder, "plot_06", ".pdf"),
  width = 3,
  height = 3,
  cairo = FALSE
)


# GPD ####
dat_2 <- tibble(x = seq(-4, 4, length.out = 1e4), y = dt(x, df = 4))
gg8 <- ggplot(dat_2) +
  geom_line(aes(x = x, y = y), size = 1.5) +
  xlab("Y | X = x") +
  ylab("Density") +
  coord_cartesian(ylim = c(0, 0.375))

save_myplot(
  plt = gg8,
  plt_nm = paste0(figure_dest_folder, "plot_08", ".pdf"),
  width = 3,
  height = 3,
  cairo = FALSE
)


gg9 <- gg8 +
  geom_area(aes(x = ifelse(x > 1, x, NA), y = y), 
            fill = my_palette$blue, alpha = .3) +
  annotate(
    geom = "segment", x = 2, y = .3, xend = 1.15, yend = .21, 
    arrow = arrow(length = unit(2, "mm"))
  ) +
  annotate(geom = "text", x = 2, y = .31, label = TeX("$u = Q_x(\\tau_0)$"), hjust = "left")

save_myplot(
  plt = gg9,
  plt_nm = paste0(figure_dest_folder, "plot_09", ".pdf"),
  width = 3,
  height = 3,
  cairo = FALSE
)


gg10 <- gg9 +
  geom_area(aes(x = ifelse(x > 2, x, NA), y = y), 
            fill = my_palette$red, alpha = .3) +
  annotate(
    geom = "segment", x = 2.5, y = .15, xend = 2.16, yend = .07, 
    arrow = arrow(length = unit(2, "mm"))
  ) +
  annotate(geom = "text", x = 2.5, y = .165, label = TeX("$y = Q_x(\\tau)$"), hjust = "left")

save_myplot(
  plt = gg10,
  plt_nm = paste0(figure_dest_folder, "plot_10", ".pdf"),
  width = 3,
  height = 3,
  cairo = FALSE
)


# ERF ####
erf.fit <- erf::erf(X, Y, min.node.size = 40, lambda = .01)
qq <- predict(erf.fit, quantiles = quantiles_2)
qq_grf <- predict(erf.fit$intermediate_threshold, quantiles = quantiles_2)[[1]]
fitted_quantiles <- tibble(
  X1 = dat$X1,
  Y = dat$Y,
  Q_interm = qq[, 1],
  Q_hi = qq[, 2],
  Q_higrf = qq_grf[, 2]
) %>% 
  mutate(exceed = if_else(Y > Q_interm, TRUE, FALSE),
         Z = if_else(exceed, Y, NaN))


gg1_bis <- ggplot(dat) +
  geom_point(aes(x = X1, y = Y), color = my_palette$grey, alpha = 0.5) +
  annotate("text", x = x_text, y = max(dat$q2), 
           label = TeX("$Q_x(0.8)$"), 
           color = my_palette$background,
           size = text_size) +
  annotate("text", x = x_text, y = max(dat$q6), 
           label = TeX("$Q_x(0.9995)$"), 
           color = my_palette$background,
           size = text_size) +
  coord_cartesian(xlim = c(-.95, .95), ylim = c(-6, 7.5), clip = "off") +
  theme(plot.margin = unit(c(1,4.5,1,1), "lines"),
        panel.border = element_rect(size = 1/8))

save_myplot(
  plt = gg1_bis,
  plt_nm = paste0(figure_dest_folder, "plot_10_bis", ".pdf"),
  width = 3,
  height = 3,
  cairo = FALSE
)


gg11 <- gg1_bis +
  geom_line(data = fitted_quantiles, 
            aes(x = X1, y = Q_interm), linetype = "dashed") +
  annotate("text", x = x_text, y = max(dat$q2), 
           label = TeX("$Q_x(0.8)$"), 
           #color = my_palette$background,
           size = text_size)

save_myplot(
  plt = gg11,
  plt_nm = paste0(figure_dest_folder, "plot_11", ".pdf"),
  width = 3,
  height = 3,
  cairo = FALSE
)


gg12 <- gg11 +
  geom_vline(aes(xintercept = x_test), linetype = "dotted", size = 1/2)

save_myplot(
  plt = gg12,
  plt_nm = paste0(figure_dest_folder, "plot_12", ".pdf"),
  width = 3,
  height = 3,
  cairo = FALSE
)


gg12_helper <- gg1_bis +
  geom_point(data = fitted_quantiles,
             aes(x = X1, y = Z), color = my_palette$red, alpha = .75) +
  geom_line(data = fitted_quantiles, 
            aes(x = X1, y = Q_interm), linetype = "dashed") +
  annotate("text", x = x_text, y = max(dat$q2), 
           label = TeX("$Q_x(0.8)$"), 
           #color = my_palette$background,
           size = text_size)


gg13 <- gg1_bis +
  geom_point(data = fitted_quantiles,
             aes(x = X1, y = Z), color = my_palette$red, alpha = .75) +
  geom_line(data = fitted_quantiles, 
            aes(x = X1, y = Q_interm), linetype = "dashed") +
  geom_vline(aes(xintercept = x_test), linetype = "dotted", size = 1/2) +
  annotate("text", x = x_text, y = max(dat$q2), 
           label = TeX("$Q_x(0.8)$"), 
           #color = my_palette$background,
           size = text_size)

save_myplot(
  plt = gg13,
  plt_nm = paste0(figure_dest_folder, "plot_13", ".pdf"),
  width = 3,
  height = 3,
  cairo = FALSE
)


X_test <- generate_test_data(1, p = 40, "halton")
X_test[1, ] <- x_test
qq_test <- predict(erf.fit, newdata = X_test, quantiles = quantiles_2)

gg14 <- gg13 +
  geom_vline(aes(xintercept = x_test), linetype = "dotted", size = 1/2) +
  geom_point(aes(x = x_test, y = qq_test[, 2]), shape = 4)

save_myplot(
  plt = gg14,
  plt_nm = paste0(figure_dest_folder, "plot_14", ".pdf"),
  width = 3,
  height = 3,
  cairo = FALSE
)


gg15 <- gg12_helper +
  geom_line(data = fitted_quantiles,
            aes(x = X1, y = Q_hi), linetype = "dashed") +
  annotate("text", x = x_text, y = max(dat$q6),
           label = TeX("$Q_x(0.9995)$"),
           #color = my_palette$background,
           size = text_size)

save_myplot(
  plt = gg15,
  plt_nm = paste0(figure_dest_folder, "plot_15", ".pdf"),
  width = 3,
  height = 3,
  cairo = FALSE
)


gg15_2 <- gg12_helper +
  geom_line(data = fitted_quantiles,
            aes(x = X1, y = Q_higrf)) +
  annotate("text", x = x_text, y = max(dat$q6),
           label = TeX("$Q_x(0.9995)$"),
           #color = my_palette$background,
           size = text_size)

save_myplot(
  plt = gg15_2,
  plt_nm = paste0(figure_dest_folder, "plot_15_2", ".pdf"),
  width = 3,
  height = 3,
  cairo = FALSE
)


gg15_3 <- gg12_helper +
  geom_line(data = dat,
            aes(x = X1, y = q6), linetype = "dashed") +
  annotate("text", x = x_text, y = max(dat$q6),
           label = TeX("$Q_x(0.9995)$"),
           #color = my_palette$background,
           size = text_size)

save_myplot(
  plt = gg15_3,
  plt_nm = paste0(figure_dest_folder, "plot_15_3", ".pdf"),
  width = 3,
  height = 3,
  cairo = FALSE
)



# Simulation study 1
set.seed(1234)
train_dat <- generate_joint_distribution(n, p, model = model, df = df, 
                                         distr = "student_t")
X <- train_dat$X %>%  as.matrix()
Y <- train_dat$Y

theo_quantiles <- generate_theoretical_quantiles(
  quantiles = quantiles, X = X, model = model, distr = "student_t", df = df
)

dat <- as_tibble(X,
                 .name_repair = ~paste0("X", seq(1, ncol(X)))) %>% 
  bind_cols(as_tibble(Y, .name_repair = ~"Y")) %>% 
  bind_cols(as_tibble(theo_quantiles, 
                      .name_repair = ~paste0("q", 1:length(quantiles)))) %>% 
  mutate(exceed = if_else(Y > q2, TRUE, FALSE),
         Z = if_else(exceed, Y, NaN),
         Z_low = if_else(!exceed, Y, NaN))


# Plots
gg16 <- ggplot(dat) +
  geom_point(aes(x = X1, y = Y), color = my_palette$grey, alpha = 0.5)

save_myplot(
  plt = gg16,
  plt_nm = paste0(figure_dest_folder, "plot_16", ".pdf"),
  width = 3,
  height = 3,
  cairo = FALSE
)
