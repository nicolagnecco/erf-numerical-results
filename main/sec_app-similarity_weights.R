# Dependency imports ####
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

get_similarity_weights <- function(X, Y, X_test, min.node.size){
  ## numeric_matrix numeric_vector numeric_matrix numeric -> numeric_matrix
  ## produce matrix n = nrow(X_test), p = ncol(X_test) with similarity weights
  
  fit.grf <- grf::quantile_forest(X, Y, min.node.size = min.node.size)
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
ntest <- 2
p <- 40
quantiles <- c(.8,.95, .9995)
model <- "step"
distr <- "student_t"
df <- 4
seed <- 158520283
min.node.size <- 40
test_points <- c(-0.2, 0.5)

figure_path <- here("figures", "similarity_weights.pdf")


# Generate data ####
set.seed(seed)

train_dat <- generate_joint_distribution(n, p, model = model, df = df, 
                                         distr = distr)
X <- train_dat$X %>%  as.matrix()
Y <- train_dat$Y

X_test <- build_X_test(test_points, p)

# Compute similarity weights
dat <- get_similarity_weights(X, Y, X_test,
                              min.node.size = min.node.size) %>% 
  weights2tibble(X, test_points, 
                 min.node.size = min.node.size)

gg <- ggplot(data = dat) +
  facet_wrap(~ x) +
  geom_point(aes(x = X1, y = w), alpha = .2) +
  geom_vline(aes(xintercept = x), linetype = 2, color = my_palette2$red) +
  xlab(TeX("$X_1$")) +
  ylab(TeX("$w_n(x, X_i)$")) +
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank()
  ); gg

save_myplot(
  plt = gg,
  plt_nm = figure_path,
  width  = 2.5,
  height = 2.5
)
