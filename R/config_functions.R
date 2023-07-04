#' Set up configuration parameters
#'
#' Set up the parameters for simulation study
#'
#' @param description (character): description of the simulation study.
#' @param nsim (integer): number of simulations to repeat.
#' @param params (list): list of parameters used to create the simulation 
#'        grid.
#' @param base_params (character_vector): character vector with names of 
#'        base parameters used when \code{grid_mode = "partial"}.
#'        Default is \code{character()}.
#' @param grid_mode (character): one of "full" or "partial". When 
#'        \code{grid_mode = "full"}, it takes the outer products of the 
#'        \code{params}. When \code{grid_mode = "partial"}, it takes
#'        a partial grid of \code{params}, using as base value 
#'        \code{base_params}.
#' @param seed (integer): random seed for the simulation study.
#'        Default is \code{NULL}.
#' @param seed_for_each (character_vector or NULL): assign different seed for 
#'        each repetition and the variables specified in the vector.
#'        If \code{NULL}, assigns a different seed for each repetition.
#'        Default is \code{NULL}.
#' @param rows_to_debug (numeric_vector or NULL): row identifiers to debug.
#'        If \code{NULL}, the whole parameter grid is saved.
#'        Default is \code{NULL}.
#' @param output_file (character or NULL): .rds file to store the results.
#'        If \code{NULL}, the object is not saved.
#'        Default is \code{NULL}.
#'        
#'        
#' @return `tibble` Each row contains the parameter configuration of one
#'         repetition.
#'
set_params <- function(description, nsim, params, base_params = character(), 
                       grid_mode = c("full", "partial"),
                       seed = NULL, seed_for_each = NULL,
                       rows_to_debug = NULL, output_file = NULL){
  
  grid_mode <- match.arg(grid_mode)
  
  # make sure params has well-formed-names
  params <- fix_list_names(params)
  
  # create grid for params
  if (grid_mode == "full" | is_empty(base_params)){
    params <- full_grid(params)
  } else if (grid_mode == "partial"){
    
    params <- partial_grid(params, base_params)
 
  }
  
  # convert integers -> double
  params <- params %>% 
    mutate(across(where(is_integer), as.numeric))
  
  # remove df for Gaussian
  params <- params %>% 
    mutate(df = if_else(distr == "gaussian", NaN, df)) %>% 
    distinct()
    
  # rep tibble 
  params <- rep_tibble(params, m = nsim) %>% 
    rename(nsim = rep_id)
  
  # add rowid
  params <- params %>% 
    rowid_to_column()
    
  # add rng
  params <- params %>% 
    assign_random_seed(grouping_vars = seed_for_each, seed = seed)

  # add description to tbl 
  comment(params) <- description
  
  # decide whether to debug
  if (!is.null(rows_to_debug)){
    params <- params %>% 
      filter(rowid %in% rows_to_debug)
  }
  
  # decide whether saving file
  if (!is.null(output_file)){
    safe_save_rds(params, here(output_file))
  }
  
  
  
  # return object
  return(params)
}
