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

# Constant definitions ####
p <- 1
model <- "step"
distr <- "student_t"
df <- 10
seed <- 158520283
figure_dest_folder <- "figures/slides/"
x_text <- 1.3
text_size <- 3.5
# x_test <- -0.25
min.node.size <- 10
test_points <- 0.5
min_weights <- 0
max_weight <- 0.0075
n <- 1000

# Variables ####
params <- tibble(
  k = c(100, 50, 25, 10)
)


for(i in seq_len(nrow(params))){
  cat("Rep", i, "\r")
  k <- params$k[i]
  tau_n <- 1 - k / n
  
  set.seed(seed)
  
  
  
  train_dat <- generate_joint_distribution(n, p,
                                           model = model, df = df,
                                           distr = distr
  )
  
  X <- train_dat$X %>% as.matrix()
  Y <- train_dat$Y
  X_test <- build_X_test(test_points, p = p)
  
  
  Q_hat <- generate_theoretical_quantiles(
    quantiles = tau_n, X = X, model = model, distr = distr, df = df
  )[, 1]
  Q_hat_text <- glue::glue("$Q_x({{round(tau_n, 3)})$", .open = "{{")
  Q_hat_x <- generate_theoretical_quantiles(
    quantiles = tau_n, X = X_test, model = model, distr = distr, df = df
  )[, 1]
  
  
  dat_weights <- tibble(X1 = X[, 1], x = test_points)
  
  dat <- dat_weights %>% 
    mutate(Y = Y,
           Q_hat = Q_hat, 
           Exc_ind = Y > Q_hat
    )
  
  
  gg3 <- ggplot(dat %>% filter(Y > -5)) +
    geom_point(aes(x = X1, y = Y), color = my_palette$grey, alpha = 0.5) +
    geom_point(data = dat %>% filter(Exc_ind), 
               mapping = aes(x = X1, y = Y),  
               col = my_palette$red) +
    geom_line(aes(x = X1, y = Q_hat)) +
    # annotate("text",
    #          x = x_text, y = max(Q_hat),
    #          label = TeX(Q_hat_text),
    #          # color = my_palette$background,
    #          size = text_size
    # ) +
    coord_cartesian(xlim = c(-.95, .95), ylim = c(-5, 12), clip = "off") +
    theme(
      plot.margin = unit(c(1, 4.5, 1, 1), "lines"),
      panel.border = element_rect(linewidth = 1 / 8)
    ); gg3
  
  save_myplot(
    plt = gg3,
    plt_nm = paste0(
      figure_dest_folder, 
      glue::glue("plot-problem-challenge-k-{k}"),
      ".pdf"),
    width  = 3,
    height = 3
  )
  
  gg4 <- ggplot(dat %>% filter(Y > -5)) +
    geom_point(aes(x = X1, y = Y), color = my_palette$grey, alpha = 0.5) +
    geom_line(aes(x = X1, y = Q_hat)) +
    # annotate("text",
    #          x = x_text, y = max(Q_hat),
    #          label = TeX(Q_hat_text),
    #          # color = my_palette$background,
    #          size = text_size
    # ) +
    coord_cartesian(xlim = c(-.95, .95), ylim = c(-5, 12), clip = "off") +
    theme(
      plot.margin = unit(c(1, 4.5, 1, 1), "lines"),
      panel.border = element_rect(linewidth = 1 / 8)
    ); gg4
  
  save_myplot(
    plt = gg4,
    plt_nm = paste0(
      figure_dest_folder, 
      glue::glue("plot-problem-challenge-k-{k}-simple"),
      ".pdf"),
    width  = 3,
    height = 3
  )
  
}

