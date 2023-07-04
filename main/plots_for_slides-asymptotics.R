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
min_weights <- 0 #5e-6
max_weight <- 0.0075
beta_k <- 0.65
beta_s <- 0.63

# Variables ####
params <- tibble(
  n = c(1, 1, 2, 3, 5, 7, 10) * 1000,
  predict_intermediate = c(T, F, F, F, F, F, F)
)


for(i in seq_len(nrow(params))){
  cat("Rep", i, "\r")
  n <- params$n[i]
  predict_intermediate <- params$predict_intermediate[i]
  k <- n^beta_k
  s <- n^beta_s
  tau_n <- 1 - k / n
  
  set.seed(seed)
  

  
  train_dat <- generate_joint_distribution(n, p,
                                           model = model, df = df,
                                           distr = distr
  )
  
  X <- train_dat$X %>% as.matrix()
  Y <- train_dat$Y
  X_test <- build_X_test(test_points, p = p)
  
  fit.grf <- grf::quantile_forest(X, Y, min.node.size = min.node.size,
                                  regression.splitting  = FALSE, 
                                  sample.fraction = s / n)
  
  if(predict_intermediate){
    Q_hat <- predict(fit.grf, quantiles = tau_n)$predictions[, 1]
    Q_hat_text <- glue::glue("$\\hat{Q}_x({{round(tau_n, 2)})$", .open = "{{")
    Q_hat_x <- predict(fit.grf, X_test, quantiles = tau_n)$predictions[, 1]
  } else {
    Q_hat <- generate_theoretical_quantiles(
      quantiles = tau_n, X = X, model = model, distr = distr, df = df
    )[, 1]
    Q_hat_text <- glue::glue("$Q_x({{round(tau_n, 2)})$", .open = "{{")
    Q_hat_x <- generate_theoretical_quantiles(
      quantiles = tau_n, X = X_test, model = model, distr = distr, df = df
    )[, 1]
  }
  
  
  dat_weights <- get_similarity_weights(fit.grf, X_test) %>% 
    weights2tibble(X, test_points, 
                   min.node.size = min.node.size)
  
  dat <- dat_weights %>% 
    mutate(Y = Y,
           Q_hat = Q_hat, 
           Exc_ind = Y > Q_hat,
           W_ind = w > min_weights,
           # hill = W_ind * Exc_ind * n / k * log(1 + (Y - Q_hat) / Q_hat_x)
           )
  
  gg <- ggplot(data = dat_weights) +
    facet_wrap(~ x) +
    geom_point(aes(x = X1, y = w), color = my_palette$grey, alpha = .5) +
    geom_vline(aes(xintercept = x), linewidth = 1, linetype = 2,
               color = my_palette2$red) +
    xlab(TeX("$X_1$")) +
    ylab(TeX("$w_n(x, X_1)$")) +
    coord_cartesian(ylim = c(0, max_weight)) +
    # geom_hline(aes(yintercept = min_weights)) +
    theme(
      strip.background = element_blank(),
      strip.text.x = element_blank()
    ); gg
  
  save_myplot(
    plt = gg,
    plt_nm = paste0(
      figure_dest_folder, 
      glue::glue("plot-asy-weights-predict_intermediate_{predict_intermediate}-n_{n}"),
      ".pdf"),
    width  = 3,
    height = 3
  )
  
  
  gg2 <- ggplot(dat %>% filter(Y > -5)) +
    geom_point(aes(x = X1, y = Y), color = my_palette$grey, alpha = 0.5) +
    geom_point(data = dat %>% filter(W_ind & Exc_ind), 
               mapping = aes(x = X1, y = Y),  
               col = my_palette$red) +
    geom_line(aes(x = X1, y = Q_hat)) +
    geom_vline(aes(xintercept = test_points[1]), linewidth = 1, linetype = 2,
               color = my_palette2$red) +
    annotate("text",
             x = x_text, y = max(Q_hat),
             label = TeX(Q_hat_text),
             # color = my_palette$background,
             size = text_size
    ) +
    coord_cartesian(xlim = c(-.95, .95), ylim = c(-5, 12), clip = "off") +
    theme(
      plot.margin = unit(c(1, 4.5, 1, 1), "lines"),
      panel.border = element_rect(linewidth = 1 / 8)
    ); gg2
  
  save_myplot(
    plt = gg2,
    plt_nm = paste0(
      figure_dest_folder, 
      glue::glue("plot-asy-data-predict_intermediate_{predict_intermediate}-n_{n}"),
      ".pdf"),
    width  = 3,
    height = 3
  )
  
  gg3 <- ggplot(dat %>% filter(Y > -5)) +
    geom_point(aes(x = X1, y = Y), color = my_palette$grey, alpha = 0.5) +
    geom_line(aes(x = X1, y = Q_hat)) +
    annotate("text",
             x = x_text, y = max(Q_hat),
             label = TeX(Q_hat_text),
             # color = my_palette$background,
             size = text_size
    ) +
    coord_cartesian(xlim = c(-.95, .95), ylim = c(-5, 12), clip = "off") +
    theme(
      plot.margin = unit(c(1, 4.5, 1, 1), "lines"),
      panel.border = element_rect(linewidth = 1 / 8)
    ); gg3
  
  save_myplot(
    plt = gg3,
    plt_nm = paste0(
      figure_dest_folder, 
      glue::glue("plot-asy-data-predict_intermediate_{predict_intermediate}-n_{n}-simple"),
      ".pdf"),
    width  = 3,
    height = 3
  )
    
}
