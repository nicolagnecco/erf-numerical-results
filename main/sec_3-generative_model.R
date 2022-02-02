# Dependency imports ####
source("main/dependencies.R")

# Constant definitions ####
n <- 2e3
p <- 40
quantiles <- c(.8,.95, .9995)
model <- "step"
distr <- "student_t"
df <- 4
seed <- 55653059
figure_path <- here("figures", "running-example-1.pdf")


# Generate data ####
set.seed(seed)

train_dat <- generate_joint_distribution(n, p, model = model, df = df, 
                                         distr = distr)
X <- train_dat$X %>%  as.matrix()
Y <- train_dat$Y


# Plot generative model ####
theo_quantiles <- generate_theoretical_quantiles(
  quantiles = quantiles, X = X, model = model, distr = distr, df = df
)
dat <- as_tibble(X,
                 .name_repair = ~paste0("X", seq(1, ncol(X)))) %>% 
  bind_cols(as_tibble(Y, .name_repair = ~"Y")) %>% 
  bind_cols(as_tibble(theo_quantiles, 
                      .name_repair = ~paste0("q", 1:length(quantiles)))) %>% 
  mutate(exceed = if_else(Y > q1, TRUE, FALSE),
         Z = if_else(exceed, Y, NaN),
         Z_low = if_else(!exceed, Y, NaN))

gg <- ggplot(dat %>% filter(Z_low > -5 | Z < 20)) +
  geom_point(aes(x = X1, y = Z_low), color = my_palette$grey, alpha = .5) +
  geom_point(aes(x = X1, y = Z), shape = 17, color = my_palette$red, alpha = .75) +
  geom_line(aes(x = X1, y = q1), linetype = "dashed") +
  geom_line(aes(x = X1, y = q3), linetype = "dashed") +
  scale_color_branded(palette = my_palette, primary = "red") +
  xlab(TeX("$X_1$")) +
  ylab("Y") +
  annotate("text", x = 1.3, y = max(dat$q3), 
           label = TeX("$\\tau = 0.9995$"), size = 3.5) +
  annotate("text", x = 1.25, y = max(dat$q1), 
           label = TeX("$\\tau_0 = 0.8$"), size = 3.5) +
  coord_cartesian(xlim = c(-.95, .95), ylim = c(-5, 20), clip = "off") +
  theme(plot.margin = unit(c(1,4.5,1,1), "lines"),
        panel.border = element_rect(size = 1/8)); gg

save_myplot(
  plt = gg,
  plt_nm = figure_path,
  width = 3,
  height = 3
)
