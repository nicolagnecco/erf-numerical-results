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

get_similarity_weights <- function(X, Y, X_test, min.node.size, ...){
  ## numeric_matrix numeric_vector numeric_matrix numeric  ... -> numeric_matrix
  ## produce matrix n = nrow(X_test), p = ncol(X_test) with similarity weights
  
  fit.grf <- grf::quantile_forest(X, Y, min.node.size = min.node.size, ...)
  get_forest_weights(fit.grf, newdata = X_test) %>% 
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
p <- 40
quantiles <- c(.1, .8, .9, .99, .995, .999, .9995)
quantiles_2 <- c(.8, .9995)
model <- "step"
distr <- "gaussian"
df <- 4
seed <- 158520283
figure_dest_folder <- "figures/slides/"
x_text <- 1.3
text_size <- 3.5
x_test <- -0.25
min.node.size <- 100
test_points <- 0.5

# Quantile regression ####
# Generate data
set.seed(seed)
train_dat <- generate_joint_distribution(n, p,
                                         model = model, df = df,
                                         distr = distr
)
X <- train_dat$X %>% as.matrix()
Y <- train_dat$Y

theo_quantiles <- generate_theoretical_quantiles(
  quantiles = quantiles, X = X, model = model, distr = distr, df = df
)

dat <- as_tibble(X,
                 .name_repair = ~ paste0("X", seq(1, ncol(X)))
) %>%
  bind_cols(as_tibble(Y, .name_repair = ~"Y")) %>%
  bind_cols(as_tibble(theo_quantiles,
                      .name_repair = ~ paste0("q", 1:length(quantiles))
  )) %>%
  mutate(
    exceed = if_else(Y > q2, TRUE, FALSE),
    Z = if_else(exceed, Y, NaN),
    Z_low = if_else(!exceed, Y, NaN)
  )


gg1 <- ggplot(dat) +
  geom_point(aes(x = X1, y = Y), color = my_palette$grey, alpha = 0.5) +
  geom_line(aes(x = X1, y = 0),
            linetype = "dashed",
            colour = "black"
  ) +
  # geom_line(aes(x = X1, y = dat$q1),
  #           linetype = "dashed",
  #           colour = "black"
  # ) +
  # geom_line(aes(x = X1, y = dat$q3),
  #           linetype = "dashed",
  #           colour = "black"
  # ) +
  annotate("text",
           x = x_text, y = 0,
           label = TeX("$E\\[Y | X = x\\]$"),
           # color = my_palette$background,
           size = text_size
  ) +
  annotate("text",
           x = x_text, y = min(dat$q1),
           label = TeX("$Q_x(0.1)$"),
           color = my_palette$background,
           size = text_size
  ) +
  annotate("text",
           x = x_text, y = max(dat$q3),
           label = TeX("$Q_x(0.9)$"),
           color = my_palette$background,
           size = text_size
  ) +
  geom_vline(aes(xintercept = test_points[1]), linewidth = 1, linetype = 2,
             color = my_palette2$red) +
  coord_cartesian(xlim = c(-.95, .95), clip = "off") +
  theme(
    plot.margin = unit(c(1, 4.5, 1, 1), "lines"),
    panel.border = element_rect(linewidth = 1 / 8)
  ); gg1


save_myplot(
  plt = gg1,
  plt_nm = paste0(figure_dest_folder, "plot_01-left-sim-weight", ".pdf"),
  width = 3,
  height = 3,
  cairo = FALSE
)

gg2 <- ggplot(dat) +
  geom_point(aes(x = X1, y = Y), color = my_palette$grey, alpha = 0.5) +
  geom_line(aes(x = X1, y = 0),
            linetype = "dashed",
            colour = "black"
  ) +
  geom_line(aes(x = X1, y = dat$q1),
            linetype = "dashed",
            colour = "black"
  ) +
  geom_line(aes(x = X1, y = dat$q3),
            linetype = "dashed",
            colour = "black"
  ) +
  annotate("text",
           x = x_text, y = 0,
           label = TeX("$E\\[Y | X = x\\]$"),
           # color = my_palette$background,
           size = text_size
  ) +
  annotate("text",
           x = x_text, y = min(dat$q1),
           label = TeX("$Q_x(0.1)$"),
           # color = my_palette$background,
           size = text_size
  ) +
  annotate("text",
           x = x_text, y = max(dat$q3),
           label = TeX("$Q_x(0.9)$"),
           # color = my_palette$background,
           size = text_size
  ) +
  geom_vline(aes(xintercept = test_points[1]), linewidth = 1, linetype = 2,
             color = my_palette2$red) +
  coord_cartesian(xlim = c(-.95, .95), clip = "off") +
  theme(
    plot.margin = unit(c(1, 4.5, 1, 1), "lines"),
    panel.border = element_rect(linewidth = 1 / 8)
  ); gg2


save_myplot(
  plt = gg2,
  plt_nm = paste0(figure_dest_folder, "plot_02-left-sim-weight", ".pdf"),
  width = 3,
  height = 3,
  cairo = FALSE
)

# Compute similarity weights ####

X_test <- build_X_test(test_points, 40)

dat_weights <- get_similarity_weights(X, Y, X_test,
                              min.node.size = min.node.size,
                              regression.splitting  = TRUE) %>% 
  weights2tibble(X, test_points, 
                 min.node.size = min.node.size)

gg <- ggplot(data = dat_weights) +
  facet_wrap(~ x) +
  geom_point(aes(x = X1, y = w), color = my_palette$grey, alpha = .5) +
  geom_vline(aes(xintercept = x), linewidth = 1, linetype = 2,
             color = my_palette2$red) +
  xlab(TeX("$X1$")) +
  ylab(TeX("$w_n(x, X1)$")) +
  ylim(0, 0.002) +
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank()
  ); gg

save_myplot(
  plt = gg,
  plt_nm = paste0(figure_dest_folder, "plot_01-right-sim-weight", ".pdf"),
  width  = 3,
  height = 3
)


dat_weights <- get_similarity_weights(X, Y, X_test,
                                      min.node.size = min.node.size,
                                      regression.splitting  = FALSE) %>% 
  weights2tibble(X, test_points, 
                 min.node.size = min.node.size)

gg <- ggplot(data = dat_weights) +
  facet_wrap(~ x) +
  geom_point(aes(x = X1, y = w), color = my_palette$grey, alpha = .5) +
  geom_vline(aes(xintercept = x), linewidth = 1, linetype = 2,
             color = my_palette2$red) +
  xlab(TeX("$X1$")) +
  ylab(TeX("$w_n(x, X1)$")) +
  ylim(0, 0.002) +
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank()
  ); gg

save_myplot(
  plt = gg,
  plt_nm = paste0(figure_dest_folder, "plot_02-right-sim-weight", ".pdf"),
  width  = 3,
  height = 3
)

