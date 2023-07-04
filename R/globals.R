MODELS <- c(
    "iid",
    "step",
    "step2d",
    "linear",
    "linearstep",
    "quadratic",
    "mixture",
    "bell",
    "tanh",
    "iid_step",
    "step_step",
    "step2d_step",
    "linear_step",
    "linearstep_step",
    "quadratic_step",
    "bell_tanh",
    "bell_sigmoid",
    "tanh_tanh",
    "quadratic2d_tanh"
)

METHODS <- c(
    "grf", "meins",
    "unc_gpd", "full_unc",
    "evgam", "truth",
    "taillardat_qrf",
    "gbex",
    "zero", "inf", "hill", "shape_rf"
)

DISTR <- c(
    "gaussian",
    "student_t",
    "pareto",
    "frechet"
)

lst_distr <- list(
    "gaussian" = "Gaussian",
    "student_t" = "Student-t",
    "frechet" = "Frechet",
    "pareto" = "Pareto"
)

lst_methods <- list(
    "erf" = "ERF",
    "erf_cv" = "ERF CV",
    "grf" = "GRF",
    "meins" = "QRF",
    "gbex" = "GBEX",
    "full_unc" = "Full unconditional",
    "truth" = "Ground truth",
    "egam" = "Extreme GAM",
    "evgam" = "EGAM",
    "taillardat_qrf" = "EGP Tail",
    "unc" = "Unconditional",
    "unconditional" = "Unconditional",
    "unc_gpd" = "Unconditional GPD",
    "zero" = "Zero",
    "inf" = "Infinity",
    "hill" = "Random Forest Hill estimator",
    "shape_rf" = "Random Forest shape estimator"
)

lst_methods_short <- lst_methods
lst_methods_short$unc_gpd <- "Unc. GPD"

lst_models <- list(
    "iid" = TeX("IID Data"),
    "iid_step" = "Constant - Step",
    "step" = "Step",
    "linear" = "Linear",
    "linearstep" = "LinearStep",
    "step2d" = "Step2dims",
    "quadratic" = "Quadratic",
    "quadratic2d" = "Quadratic2d",
    "tanh" = "Tanh",
    "bell" = "Bell",
    "mixture" = "Mixture",
    "step_step" = "Step - Step",
    "linear_step" = "Linear - Step",
    "linearstep_step" = "LinearStep - Step",
    "step2d_step" = "Step2dims - Step",
    "quadratic_step" = "Quadratic - Step",
    "tanh_tanh" = TeX("Model 1: $(s_1(x), \\nu(x))$"), # Tanh - Tanh",
    "quadratic2d_tanh" = TeX("Model 2: $(s_2(x), \\nu(x))"), # "Quadratic2d - Tanh",
    "bell_tanh" = TeX("Model 3: $(s_3(x), \\nu(x))$"), # Bell - Tanh",
    "bell_sigmoid" = "Bell - Sigmoid"
)
