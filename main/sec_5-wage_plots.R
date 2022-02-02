source("main/dependencies.R")

# Input files
FILE1 <- "output/exp_sec_5/wage_pred_param.rds"
FILE2 <- "output/exp_sec_5/wage_pred_quant.rds"
FILE3 <- "output/exp_sec_5/wage_cv.rds"
PLT1 <- here("figures/fitted_params.pdf")
PLT2 <- here("figures/fitted_quantiles.pdf")
PLT3 <- here("figures/loss_methods_boxplots_identity.pdf")
PLT3bis <- here("figures/loss_methods_boxplots_log.pdf")
PLT4 <- here("figures/loss_methods.pdf")

# Constants
quantiles2plot <- c(.9, .995)
quantiles2plot_losses <-  c(.9, .99, .995)

# Files to loop over
func_responses <- c("identity", "log")
noise2add <- paste0("noisevars=", c(10))
predictors <- c("educ" = "Years of Education", "age" = "Age")

# Plot EDA
for (l in seq_along(func_responses)) {
  for (i in seq_along(noise2add)){
    for (j in seq_along(predictors)){
      
      func_response <- func_responses[l]
      
      if (func_response == "identity") {
        y_quantile <-  "quantile"
        quantile_label <- TeX("$\\hat{Q}_{x}(\\tau)$")
      } else if (func_response == "log"){
        y_quantile = "exp(quantile)"
        quantile_label <- TeX("$exp\\[\\hat{Q}_{x}(\\tau)\\]$")
      } else {
        stop("Transformations of response must be one of `identity` and `log`.")
      }
      
      noise <- noise2add[i]
      predictor <- names(predictors)[j]
      
      if (predictor == "educ") {
        col_factor <- "factor(black)"
        col_legend <- "Black:"
      } else if (predictor == "age") {
        col_factor <- "educ_factor"
        col_legend <- "Years of Education:"
      }
      
      label_predictor <- predictors[j]
      
      file_name_plot1 <- add_word_before_ext(
        add_word_before_ext(add_word_before_ext(PLT1, 
                                                func_response),
                            predictor), 
        noise
      )
      
      file_name_plot2 <- add_word_before_ext(
        add_word_before_ext(add_word_before_ext(PLT2, 
                                                func_response),
                            predictor), 
        noise
      )
      
      # seed to reproduce results
      set.seed(42)
      
      # Plot predicted parameters
      pred_param <- read_rds(add_word_before_ext(
        add_word_before_ext(FILE1, func_response),
        noise)
      ) %>% 
        mutate(educ_factor = factor(educ < 15, levels = c(TRUE, FALSE),
                                    labels = c("< 15", "15 or more")))
      
      g1 <- ggplot(pred_param %>%
                     group_by(param) %>% slice_sample(n = 3000)) +
        facet_wrap(~ param, scales = "free", labeller = label_parsed) +
        geom_point(aes_string(x = predictor, y = "param_value", 
                              col = col_factor, shape = col_factor), 
                   alpha = .5, 
                   position = position_jitterdodge(jitter.width = 0.0,
                                                   seed = 42)) +
        scale_color_branded(my_palette2) +
        xlab(label_predictor) +
        ylab("") +
        labs(colour = col_legend, shape = col_legend); g1
      
      save_myplot(plt = g1, plt_nm = file_name_plot1,
                  width = 2, 
                  height = 2) 
      
      # Plot predicted quantiles
      pred_quantiles <- read_rds(add_word_before_ext(
        add_word_before_ext(FILE2, func_response),
        noise)
      ) %>% 
        filter(tau %in% quantiles2plot) %>% 
        filter(method != "zero", method != "inf") %>% 
        mutate(tau_factor = texify_column(tau, "\\tau")) %>% 
        mutate(method = refactor_methods_parse(method)) %>% 
        mutate(educ_factor = factor(educ < 15, levels = c(TRUE, FALSE),
                                    labels = c("< 15", "15 or more")))
      
      
      g2 <- ggplot(pred_quantiles %>%
                     filter(quantile < if_else(func_response == "log", 
                                               log(6000),
                                               6000)) %>% 
                     group_by(method, tau) %>% slice_sample(n = 500)) +
        facet_grid(tau_factor ~ method, labeller = label_parsed,
                   scales = "free_y") +
        geom_point(aes_string(x = predictor, y = y_quantile), 
                   alpha = .25) +
        scale_color_branded(my_palette2) +
        xlab(label_predictor) +
        ylab(quantile_label) +
        labs(colour = col_legend, shape = col_legend); g2
      
      save_myplot(plt = g2, plt_nm = file_name_plot2,
                  width = 1.45, 
                  height = 1.45)
    }
  }
}

# Plot losses
res <- bind_rows(
  read_rds(add_word_before_ext(FILE3, "identity")) %>% 
    mutate(resp = "Y = wage"),
  read_rds(add_word_before_ext(FILE3, "log")) %>% 
    mutate(resp = "Y = log(wage)")
) %>% 
  mutate(resp = factor(resp, levels = c("Y = wage", "Y = log(wage)")))
  

losses <- res %>% 
  filter(method != "zero", method!= "inf") %>% 
  filter(tau %in% quantiles2plot_losses) %>% 
  mutate(method = refactor_methods(method),
         n_noise_vars = texify_column(n_noise_vars, "noise\\, vars"))

losses_identity <- losses %>% filter(resp == "Y = wage")
losses_log <- losses %>% filter(resp != "Y = wage")

set.seed(42)
upper_lim <- quantile(purrr::map_dbl(1:10000, function(i){
  max(abs(rnorm(1)))
}), .95)


g3 <- ggplot() +
  facet_wrap(~ method, nrow = 1) +
  geom_point(data = losses_identity,
             mapping = aes(x = factor(tau), 
                           y = loss_wang, 
                           col = method),
             position = position_jitterdodge(jitter.width = 0), alpha = .5) +
  geom_boxplot(data = losses_identity,
               mapping = aes(x = factor(tau), 
                             y = loss_wang, 
                             col = method),
               outlier.shape = NA, alpha = 1) +
  scale_color_manual(values = my_palette_methods) +
  scale_y_continuous(minor_breaks = NULL) +
  xlab(TeX("$\\tau$")) +
  ylab("|Loss|") + 
  # theme(axis.text.x = element_text(angle = 90)) +
  geom_ribbon(data = tibble(tau = c(0, length(losses$tau %>% unique()) + 1)),
              mapping = aes(x = tau, ymin=0, ymax=upper_lim),
              color = "black",
              linetype = "dashed",
              size = .2,
              alpha = .4) +
  theme(legend.position = c(0.5, -0.25), legend.direction = "horizontal") +
  theme(legend.position = "none") +
  labs(colour = "Method:"); g3


save_myplot(plt = g3, plt_nm = PLT3,
            width = 1.5, 
            height = 1.5)

g3bis <- ggplot() +
  facet_wrap(~ method, nrow = 1) +
  geom_point(data = losses_log,
             mapping = aes(x = factor(tau), 
                           y = loss_wang, 
                           col = method),
             position = position_jitterdodge(jitter.width = 0), alpha = .5) +
  geom_boxplot(data = losses_log,
               mapping = aes(x = factor(tau), 
                             y = loss_wang, 
                             col = method),
               outlier.shape = NA, alpha = 1) +
  scale_color_manual(values = my_palette_methods) +
  scale_y_continuous(minor_breaks = NULL) +
  xlab(TeX("$\\tau$")) +
  ylab("|Loss|") + 
  # theme(axis.text.x = element_text(angle = 90)) +
  geom_ribbon(data = tibble(tau = c(0, length(losses$tau %>% unique()) + 1)),
              mapping = aes(x = tau, ymin=0, ymax=upper_lim),
              color = "black",
              linetype = "dashed",
              size = .2,
              alpha = .4) +
  theme(legend.position = c(0.5, -0.25), legend.direction = "horizontal") +
  theme(legend.position = "none") +
  labs(colour = "Method:"); g3bis


save_myplot(plt = g3bis, plt_nm = PLT3bis,
            width = 1.5, 
            height = 1.5)

losses_grouped <- losses %>% 
  group_by(n_noise_vars, resp, method, tau, tau_factor) %>% 
  summarise(avg_loss_wang = mean(loss_wang))

g4 <- ggplot() +
  facet_wrap(~ resp, nrow = 1) +
  stat_summary(data = losses_grouped, 
               mapping =aes(x = factor(tau), 
                            y = avg_loss_wang,
                            col = method,
                            group = method), 
               fun=sum,
               geom="line") +
  geom_point(data = losses_grouped,
             mapping = aes(x = factor(tau), 
                           y = avg_loss_wang, 
                           col = method),
             shape = 20, size = 2, alpha = 1) +
  geom_ribbon(data = tibble(tau = c(0, length(losses$tau %>% unique()) + 1)),
              mapping = aes(x = tau, ymin=0, ymax=upper_lim),
              color = "black",
              linetype = "dashed",
              size = .2,
              alpha = .4) +
  scale_color_manual(values = my_palette_methods) +
  scale_y_continuous(minor_breaks = NULL) +
  xlab(TeX("$\\tau$")) +
  ylab("|Loss|") + 
  # theme(axis.text.x = element_text(angle = 90)) +
  theme(legend.position = c(0.5, -0.25), legend.direction = "horizontal") +
  labs(colour = "Method:"); g4

save_myplot(plt = g4, plt_nm = PLT4,
            width = 2.5, 
            height = 2.5)
