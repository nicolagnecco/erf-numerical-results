# options setup
my_palette <- list(
    "red" = "#D55E00",
    "blue" = "#0072B2",
    "green" = "#009E73",
    "yellow" = "#E69F00",
    "pink" = "#CC79A7",
    "light_blue" = "#56B4E9",
    "grey" = "#999999",
    "background" = "#FAFAFA"
)

my_palette2 <- list(
    "blue"   = "#00798c",
    "red"    = "#d1495b",
    "green"  = "#a2d729", # "#66a182",
    "yellow" = "#edae49",
    "navy"   = "#2e4057",
    "grey"   = "#8d96a3"
)

my_palette_methods <- list(
    c("method" = lst_methods$erf, "color" = my_palette$blue),
    c("method" = lst_methods$erf_cv, "color" = my_palette$blue),
    c("method" = lst_methods$grf, "color" = my_palette$red),
    c("method" = lst_methods$meins, "color" = my_palette$green),
    c("method" = lst_methods$gbex, "color" = my_palette$yellow),
    c("method" = lst_methods$full_unc, "color" = my_palette$grey),
    c("method" = lst_methods$unc, "color" = my_palette$grey),
    c("method" = lst_methods$unc_gpd, "color" = my_palette$grey),
    c("method" = lst_methods$taillardat_qrf, "color" = my_palette$light_blue)
) %>%
    purrr::transpose() %>%
    as_tibble() %>%
    unnest(cols = c(method, color)) %>%
    deframe()


cb_palette <- c(
    "#999999", "#E69F00", "#56B4E9", "#009E73",
    "#F0E442", "#0072B2", "#D55E00", "#CC79A7"
)

theme_set(theme_bw() +
    theme(
        plot.background = element_blank(),
        panel.background = element_blank(),
        legend.background = element_blank(),
        strip.background = element_rect(fill = "white"),
        plot.caption = element_text(
            size = 7.5, hjust = 0,
            margin = margin(t = 15)
        ),
        text = element_text(size = 11),
        axis.ticks = element_blank(),
        panel.grid.major = element_line(linewidth = 0.25)
    ))


# function definitions
branded_pal <- function(palette, primary = "blue", other = "grey",
                        direction = 1) {
    ## character_vector character character integer -> function
    ## returns a function that produces a discrete palette with n colors


    function(n) {
        n_max <- length(palette)

        if (!(primary %in% names(palette))) {
            stop(paste0(
                "The primary color ", primary,
                " must exist in the given palette."
            ))
        }

        if (n > n_max) {
            stop(paste0(
                "The palette you supplied only has ",
                n_max, " colors."
            ))
        }

        if (other %in% names(palette)) {
            other <- palette[other]
        }
        color_list <- c(other, palette[primary])
        remaining_colors <- palette[!(palette %in% color_list)]
        color_list <- c(color_list, remaining_colors)[1:n]

        color_list <- unname(unlist(color_list))

        if (direction >= 0) {
            color_list
        } else {
            rev(color_list)
        }
    }
}


scale_color_branded <- function(palette, primary = "blue", other = "grey",
                                direction = 1, ...) {
    ## character_vector character character integer ... -> ggplot_constructor
    ## wrapper around ggplot2::discrete_scale
    ggplot2::discrete_scale(
        "colour", "branded",
        branded_pal(palette, primary, other, direction),
        ...
    )
}

scale_fill_branded <- function(palette, primary = "blue", other = "grey",
                               direction = 1, ...) {
    ## character_vector character character integer ... -> ggplot_constructor
    ## wrapper around ggplot2::discrete_scale
    ggplot2::discrete_scale(
        "fill", "branded",
        branded_pal(palette, primary, other, direction),
        ...
    )
}

reverse_factor <- function(column) {
    ## factor -> factor
    ## reverse order of a factor
    factor(column, levels = rev(levels(column)))
}

texify_column <- function(column, letter) {
    ## vector character -> factor
    ## paste latex formula of the form "$letter = column$"
    factor(column,
        levels = unique(column),
        labels = TeX(paste0(
            "$", letter, " = ",
            unique(column), "$"
        ))
    )
}

refactor_param <- function(param) {
    ## character_vector -> factor
    ## refactor param

    factor(param,
        levels = unique(param),
        labels = TeX(c("$\\hat{\\sigma}(x)$", "$\\hat{\\xi}(x)$"))
    )
}

refactor_methods <- function(methods, short_names = FALSE) {
    ## character_vector boolean -> factor
    ## refactor column with methods

    if (short_names) {
        lst_methods <- lst_methods_short
    } else {
        lst_methods <- lst_methods
    }

    unique_methods <- unique(methods)

    new_levels <- names(lst_methods)
    new_labels <- lst_methods %>%
        unlist() %>%
        unname()

    factor(methods,
        levels = new_levels,
        labels = new_labels
    )
}

refactor_methods_parse <- function(methods) {
    ## character_vector -> factor
    ## refactor column with methods
    unique_methods <- unique(methods)

    new_levels <- names(lst_methods)
    new_labels <- lst_methods %>%
        unlist() %>%
        unname()

    factor(methods,
        levels = new_levels,
        labels = TeX(new_labels)
    )
}

refactor_models_distr <- function(model, distr, df) {
    ## character_vector (2x) numeric_vector -> factor
    ## refactor column

    deg_fr <- if_else(distr == "gaussian", 999, df)

    models_tbl <- lst_models %>%
        as_tibble() %>%
        pivot_longer(
            cols = everything(), names_to = "model_id",
            values_to = "model_name"
        )

    factor_tbl <- expand_grid(
        model_name = lst_models %>% unlist(),
        df = deg_fr %>% unique() %>% sort(decreasing = TRUE)
    ) %>%
        left_join(models_tbl, by = "model_name") %>%
        mutate(
            xi = round(1 / df, 2),
            new_labels = paste0("model: ", model_name, "; $\\xi = ", xi, "$"),
            new_levels = paste0(model_id, df)
        )

    factor(paste0(model, deg_fr),
        levels = factor_tbl$new_levels,
        labels = TeX(factor_tbl$new_labels)
    )
}

refactor_models <- function(models) {
    ## character_vector -> factor
    ## refactor column with models
    unique_models <- unique(models)

    new_levels <- names(lst_models)
    new_labels <- lst_models %>%
        unlist() %>%
        unname()

    factor(models,
        levels = new_levels,
        labels = new_labels
    )
}

refactor_distr_df <- function(distr, df, param = c("nu", "xi")) {
    ## numeric_character numeric_vector characther -> factor
    ## refactor columns with distribution and degrees of freedom

    param <- match.arg(param)

    deg_fr <- if_else(distr == "gaussian", 999, df)

    if (param == "nu") {
        # option 1: nu
        new_levels <- deg_fr %>%
            sort() %>%
            unique()
        new_labels <- if_else(new_levels == 999,
            TeX("$\\nu = \\infty$"),
            TeX(paste0("$\\nu = ", new_levels, "$"))
        )
    } else if (param == "xi") {
        # option 2: xi
        new_levels <- deg_fr %>%
            sort(decreasing = TRUE) %>%
            unique()
        new_labels <- if_else(new_levels == 999,
            TeX("$\\xi = 0$"),
            TeX(paste0("$\\xi = ", round(1 / new_levels, 2), "$"))
        )
    }

    factor(deg_fr,
        levels = new_levels,
        labels = new_labels
    )
}

refactor_distr <- function(distr) {
    ## character_vector -> factor
    ## refactor column with distributions
    unique_distr <- unique(distr)

    new_levels <- names(lst_distr)
    new_labels <- lst_distr %>%
        unlist() %>%
        unname()

    factor(distr,
        levels = new_levels,
        labels = new_labels
    )
}

refactor_lambda <- function(lambda) {
    ## list_of_numeric_vector -> factor
    ## refactor column

    new_lambda <- purrr::map_dbl(
        lambda,
        function(ll) {
            if (length(ll) > 1) {
                return(500)
            } else {
                return(ll)
            }
        }
    )

    new_levels <- new_lambda %>%
        sort() %>%
        unique()
    new_labels <- if_else(new_levels == 500,
        TeX("$\\lambda$ cross-validated"),
        if_else(new_levels == 1000,
            TeX("$\\lambda = +\\infty$"),
            TeX(paste0("$\\lambda = ", new_levels, "$"))
        )
    )
    factor(new_lambda,
        levels = new_levels,
        labels = new_labels
    )
}

refactor_shape_is_known <- function(shape_is_known) {
    ## logical_vector -> factor
    ## refactor column

    new_levels <- c(FALSE, TRUE)
    new_labels <- c(
        TeX("$\\xi_{prior}$ estimated"),
        TeX("$\\xi_{prior}$ known")
    )

    factor(shape_is_known,
        levels = new_levels,
        labels = new_labels
    )
}

compute_boxplot_stats <- function(x) {
    ## numeric_vector -> tibble
    ## compute statistics for boxplots
    iqr <- IQR(x)
    tibble(
        mean = mean(x, na.rm = TRUE),
        lower = quantile(x, .25),
        ymin = min(x[x > lower - 1.5 * iqr]),
        middle = median(x),
        upper = quantile(x, .75),
        ymax = max(x[x < upper + 1.5 * iqr])
    )
}

interpolate_cols2tibble <- function(x, y, z,
                                    nx = 100, ny = 100,
                                    x_name = "x", y_name = "y", z_name = "z") {
    ## numeric_vector (3x) numeric (3x) character (3x) -> tibble
    ## produce a tibble with `nx * ny` interpolated observations for `z`

    interp(x = x, y = y, z = z, nx = nx, ny = ny) %>%
        interp2xyz() %>%
        as_tibble(.name_repair = ~ c(x_name, y_name, z_name))
}

parse.labels <- function(x) {
    parse(text = x)
}

save_myplot <- function(plt, plt_nm,
                        width, height,
                        width_pdf = 50, height_pdf = 50,
                        crop = TRUE, cairo = TRUE) {
    dir_name <- dirname(plt_nm)
    if (!file.exists(dir_name)) {
        dir.create(dir_name)
    }

    if (cairo) {
        ggsave(plt_nm,
            egg::set_panel_size(
                p = plt,
                width = unit(width, "in"),
                height = unit(height, "in")
            ),
            width = width_pdf, height = height_pdf,
            limitsize = FALSE, units = c("in"),
            device = cairo_pdf, family = "Arial"
        )
    } else {
        ggsave(plt_nm,
            egg::set_panel_size(
                p = plt,
                width = unit(width, "in"),
                height = unit(height, "in")
            ),
            width = width_pdf, height = height_pdf,
            limitsize = FALSE, units = c("in")
        )
    }

    if (crop) {
        knitr::plot_crop(plt_nm)
    }
}
