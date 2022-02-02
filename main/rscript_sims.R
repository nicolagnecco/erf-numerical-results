library(optparse)
# specify desired options in a list
option_list <- list(
  make_option(c("-j", "--json-file"), type="character",
              default=NULL,
              help=paste0("Path of the JSON file ",
                          "containing the configuration of the simulation study. ",
                          "Note that the path is with respect to the project ",
                          "root directory"),
              metavar="character"))

sim_study <- parse_args(OptionParser(option_list=option_list))[[1]]


# Dependency imports
source("main/dependencies.R")

# Run simulations
run_simulations(sim_study)

