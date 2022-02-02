run_simulations <- function(json_file_nm){
  #' @param json_file_nm : character, name of the JSON file containing the 
  #'  arguments for the simulation. The files is made of:
  #'  - `set_params` : # !!! add docs
  #'  - `set_parallel_strategy` : # !!! add docs
  #'  - `run_experiments` : # !!! add docs
  #'  
  #' @return `tibble` containing the simulation arguments taken from the
  #'   `json_file_nm`, and the (nested) results `res`.
   
  # Load JSON config
  configs <- fromJSON(here(json_file_nm))
  
  # process parameters
  exec(set_params, !!!configs$set_params)
  
  # set up parallel strategy
  exec(set_parallel_strategy, !!!configs$set_parallel_strategy)
  
  # run simulations
  exec(run_simulations_helper, !!!configs$run_experiments)
}

run_simulations_helper <- function(experiment,
                            input_file, 
                            output_file = NULL){
  #' @param experiment : character, name of experiment function.
  #' @param input_file : character, rds file of a tibble with simulation 
  #'  arguments.
  #' @param output_file : character or `NULL`. If character, file name of the
  #'  rds file containing the results. If `NULL`, no rds file is saved.
  #'  
  #' @return `tibble` containing the simulation arguments from `input_file` and
  #'  the (nested) results, `res` obtained from calling function `experiment`.
  #'   
  
  # select simulation experiment
  sim_fn <- eval(as.name(experiment))
  
  # load parameters
  sims_args <- read_rds(here(input_file))
  iterations <- seq_len(nrow(sims_args))
  m <- max(iterations)
  
  # split arguments between fun_args and other_args
  required_args <- names(formals(sim_fn))
  fun_args <- sims_args %>% select(any_of(required_args))
  other_args <- sims_args %>% select(!any_of(required_args))
  
  # iterate over simulations
  file_log <- here("output", "progress.txt")
  ptm <- proc.time()
  cat("****", comment(sims_args) , "**** \n", file = file_log)
  
  ll <- foreach(i = iterations, .combine = bind_rows, 
                .options.future = list(scheduling = FALSE)) %dopar% {
                  
                  
                  # log
                  cat("Simulation", i, "out of", m, "PID:", Sys.getpid(), "\n", 
                      file = file_log, append = TRUE)
                  
                  # set random seed
                  rngtools::setRNG(other_args$rng[[i]])
                  
                  # run experiment
                  exec(sim_fn, !!!purrr::flatten(fun_args[i, ])) %>% 
                    mutate(rowid = other_args$rowid[i])
                }
  sink(file = file_log, append = TRUE)
  cat("\n Time \n")
  print(proc.time() - ptm)
  sink()
  
  # error log
  rowid_errors <- which(!(sims_args$rowid %in% ll$rowid))
  if (length(rowid_errors) > 0){
    sink(file = file_log, append = TRUE)
    cat("\nError handling \n")
    cat(paste0("Error occured in iteration with rowid: ", rowid_errors, "\n"))
    sink()
  }

  # collect results
  ll <- ll %>%
    nest(res = !c("rowid")) %>%
    left_join(sims_args, by = "rowid")
  
  # add description
  comment(ll) <- comment(sims_args)
  
  # save results
  if (!is.null(output_file)){
    safe_save_rds(ll, here(add_timestamp(output_file)))
  }
  
  # return results
  return(ll)
  
}
