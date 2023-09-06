# Dependency imports
source("main/dependencies.R")

# Function definitions ####
build_X_test <- function(test_points, p){
  ## numeric_vector numeric -> numeric_matrix
  ## produce a matrix with nrow = length(test_points) and ncol = p where:
  ## - first column = test_points
  ## - other columns = Uniform numbers in [-1, 1]
  
  n_points <- length(test_points)
  
  if (p == 1){
    matrix(test_points, ncol = 1)
  } else if (p > 1){
    cbind(
      matrix(test_points, ncol = 1),
      matrix(runif(n_points * (p - 1), min = -1, max = 1), ncol = p - 1)
    )
  } else {
    stop("`p` must be greater than zero.", call. = FALSE)
  }
}

get_similarity_weights <- function(grf_fit, X_test){
  ## grf_object numeric_matrix -> numeric_matrix
  ## produce matrix n = nrow(X_test), p = ncol(X_test) with similarity weights
  
  get_forest_weights(grf_fit, newdata = X_test) %>% 
    as.matrix()
}

weights2tibble <- function(w, X, test_points, min.node.size){
  ## numeric_matrix numeric_matrix numeric_vector numeric -> tibble
  ## tibble made of the following columns
  ## `X1`, `w`, `x`, `min.node.size`
  
  tbl <- tibble(X1 = numeric(), w = numeric(), 
                x = numeric(), min.node.size = numeric())
  
  for (i in seq_along(test_points)){
    tbl <- tbl %>% 
      bind_rows(
        tibble(X1 = X[, 1], w = w[i, ], 
               x = test_points[i], min.node.size = min.node.size)
      )
  }
  
  return(tbl)
  
}


# Constant definitions ####
n <- 2e3
p <- 10
model <- "step"
distr <- "gaussian"
df <- 10
seed <- 1519520123
figure_dest_folder <- "figures/slides/"
x_text <- 1.3
text_size <- 3.5
min.node.size <- 40
test_points <- 0.5
min_weights <- 0 #5e-6
max_weight <- 0.0075
tau_n <- 0.8
tau_hi <- .9995


# generate data
train_dat <- generate_joint_distribution(n, p,
                                         model = model, df = df,
                                         distr = distr
)

X <- train_dat$X %>% as.matrix()
Y <- train_dat$Y
X_test <- build_X_test(test_points, p = p)
X_test[1, ] <- test_points

# fit erf
erf.fit <- erf::erf(X, Y, min.node.size = min.node.size, lambda = .01)
fit.grf <- erf.fit$intermediate_threshold


# predict intermediate quantile (using grf)
Q_hat <- predict(fit.grf, quantiles = tau_n)$predictions[, 1]
Q_hat_text <- glue::glue("$\\hat{Q}_x({{round(tau_n, 2)})$", .open = "{{")

# predict high quantile (using grf)
Q_hat_hi_grf <- predict(fit.grf, quantiles = tau_hi)$predictions[, 1]
Q_hat_hi_text <- glue::glue("$\\hat{Q}_x({{round(tau_hi, 4)})$", .open = "{{")

# predict high quantile (using erf)
Q_hat_hi_erf <- predict(erf.fit, quantiles = tau_hi)[, 1]
qq_test <- predict(erf.fit, newdata = X_test, quantiles = tau_hi)

# predict high quantile (oracle)
Q_hat_hi_oracle <- generate_theoretical_quantiles(
  quantiles = tau_hi, X = X, model = model, distr = distr, df = df
)

# extract forest weights
dat_weights <- get_similarity_weights(erf.fit$quantile_forest, X_test) %>% 
  weights2tibble(X, test_points, 
                 min.node.size = min.node.size)


# create dataset
dat <- dat_weights %>% 
  mutate(Y = Y,
         Q_hat = Q_hat, 
         Q_hat_hi_grf = Q_hat_hi_grf,
         Q_hat_hi_erf = Q_hat_hi_erf,
         Q_hat_hi_oracle = Q_hat_hi_oracle,
         Exc_ind = Y > Q_hat,
         W_ind = w > min_weights
  )


# base plot
gg_base <- ggplot() +
  coord_cartesian(
    xlim = c(-.95, .95), 
    ylim = c(-5, 12), 
    clip = "off") +
  theme(
    plot.margin = unit(c(1, 4.5, 1, 1), "lines"),
    panel.border = element_rect(linewidth = 1 / 8),
    legend.position = "none"
  )+
  xlab(TeX("Predictor vector $X$")) + 
  ylab(TeX("Response $Y$")) +
  annotate("text",
           x = x_text, y = max(Q_hat_hi_oracle),
           label = TeX(Q_hat_hi_text),
           size = text_size,
           color = my_palette$background
  )


# Only data
gg_00 <- gg_base +
  geom_point(data = dat %>% filter(Y > -5),
             aes(x = X1, y = Y), color = my_palette$grey, alpha = 0.5)

save_myplot(
  plt = gg_00,
  plt_nm = paste0(figure_dest_folder, glue::glue("plot_weighted_erf_00"),".pdf"),
  width  = 3,
  height = 3
)


# Add intermediate quantile
gg_01 <- gg_00 +
  geom_line(
    data = dat %>% filter(Y > -5),
    aes(x = X1, y = Q_hat)) +
  annotate("text",
           x = x_text, y = max(Q_hat),
           label = TeX(Q_hat_text),
           size = text_size
  )

gg_01


save_myplot(
  plt = gg_01,
  plt_nm = paste0(figure_dest_folder, glue::glue("plot_weighted_erf_01"),".pdf"),
  width  = 3,
  height = 3
)


# Add test point
gg_02 <- gg_01 + 
  geom_vline(aes(xintercept = test_points[1]), linewidth = 1, linetype = 2,
             color = my_palette2$red)


save_myplot(
  plt = gg_02,
  plt_nm = paste0(
    figure_dest_folder, 
    glue::glue("plot_weighted_erf_02"),
    ".pdf"),
  width  = 3,
  height = 3
)


gg_03 <-
  gg_base +
  geom_point(data = dat %>% filter(Y > -5, Y < Q_hat),
             aes(x = X1, y = Y), color = my_palette$grey, alpha = 0.5) +
  geom_point(data = dat %>% filter(Exc_ind), 
             mapping = aes(x = X1, y = Y, size = w), 
             alpha = 0.5,
             color = my_palette$red) +  
  geom_line(data = dat %>% filter(Y > -5),
            aes(x = X1, y = Q_hat)) +
  geom_vline(aes(xintercept = test_points[1]), linewidth = 1, linetype = 2,
             color = my_palette2$red) +
  annotate("text",
           x = x_text, y = max(Q_hat),
           label = TeX(Q_hat_text),
           # color = my_palette$background,
           size = text_size
  )

gg_03

save_myplot(
  plt = gg_03,
  plt_nm = paste0(
    figure_dest_folder, 
    glue::glue("plot_weighted_erf_03"),
    ".pdf"),
  width  = 3,
  height = 3
)


gg_04 <- gg_03 +
  geom_vline(aes(xintercept = test_points[1]), linewidth = 1, linetype = 2,
             color = my_palette2$red) +
  geom_point(data = dat, aes(x = x, y = qq_test), shape = 19, size = 2)

save_myplot(
  plt = gg_04,
  plt_nm = paste0(
    figure_dest_folder, 
    glue::glue("plot_weighted_erf_04"),
    ".pdf"),
  width  = 3,
  height = 3
)

gg_05 <- gg_base +
  geom_point(data = dat %>% filter(Y > -5, Y < Q_hat),
             aes(x = X1, y = Y), color = my_palette$grey, alpha = 0.5) +
  geom_point(data = dat %>% filter(Exc_ind), 
             mapping = aes(x = X1, y = Y), 
             alpha = 0.5,
             color = my_palette$red) +  
  geom_line(data = dat %>% filter(Y > -5),
            aes(x = X1, y = Q_hat)) +
  annotate("text",
           x = x_text, y = max(Q_hat),
           label = TeX(Q_hat_text),
           # color = my_palette$background,
           size = text_size
  )

gg_05

save_myplot(
  plt = gg_05,
  plt_nm = paste0(
    figure_dest_folder, 
    glue::glue("plot_weighted_erf_05"),
    ".pdf"),
  width  = 3,
  height = 3
)

gg_06 <- gg_05 +
  geom_line(
    data = dat,
    aes(x = X1, y = Q_hat_hi_erf), linetype = "dashed"
  ) +
  annotate("text",
           x = x_text, y = max(Q_hat_hi_oracle),
           label = TeX(Q_hat_hi_text),
           # color = my_palette$background,
           size = text_size
  )

gg_06


save_myplot(
  plt = gg_06,
  plt_nm = paste0(
    figure_dest_folder, 
    glue::glue("plot_weighted_erf_06"),
    ".pdf"),
  width  = 3,
  height = 3
)


gg_07 <- gg_05 +
  geom_line(
    data = dat,
    aes(x = X1, y = Q_hat_hi_oracle), linetype = "dashed"
  ) +
  annotate("text",
           x = x_text, y = max(Q_hat_hi_oracle),
           label = TeX(Q_hat_hi_text),
           # color = my_palette$background,
           size = text_size
  )

gg_07


save_myplot(
  plt = gg_07,
  plt_nm = paste0(
    figure_dest_folder, 
    glue::glue("plot_weighted_erf_07"),
    ".pdf"),
  width  = 3,
  height = 3
)


gg_08 <- gg_05 +
  geom_line(
    data = dat,
    aes(x = X1, y = Q_hat_hi_grf), linetype = "dashed"
  ) +
  annotate("text",
           x = x_text, y = max(Q_hat_hi_oracle),
           label = TeX(Q_hat_hi_text),
           # color = my_palette$background,
           size = text_size
  )

gg_08


save_myplot(
  plt = gg_08,
  plt_nm = paste0(
    figure_dest_folder, 
    glue::glue("plot_weighted_erf_08"),
    ".pdf"),
  width  = 3,
  height = 3
)