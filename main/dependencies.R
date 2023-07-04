# dependency imports
chooseCRANmirror(ind = 1)
is_pacman_installed <- "pacman" %in% rownames(installed.packages())
if (is_pacman_installed == FALSE) install.packages("pacman")

# load-install-cran
cran_packs <- c(
    "tidyverse", "grf",
    "evgam", "here", "cowplot", "grid", "gridExtra", "jsonlite", "ggh4x",
    "doFuture", "doRNG", "latex2exp", "rngtools", "randtoolbox", "mvtnorm",
    "sfsmisc", "egg", "mev", "gmm", "POT", "treeClust", "fastDummies",
    "backports", "ggpubr", "ismev", "evd"
)

pacman::p_load(cran_packs, update = FALSE, character.only = TRUE)

# load-install-github
github_packs <- c("JVelthoen/gbex", "nicolagnecco/erf")

pacman::p_load_gh(github_packs, update = FALSE)

# load local functions
purrr::map(here::here("R", list.files(here::here("R"))), source)

# R options
options(future.rng.onMisuse = "ignore")
RNGkind(kind = "L'Ecuyer-CMRG")
