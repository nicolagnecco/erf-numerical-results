full_grid <- function(lst){
  ## list -> tibble
  ## expand a full grid on lst
  expand.grid(lst, stringsAsFactors = FALSE) %>% tibble::as_tibble()
}

partial_grid <- function(lst, base_param_nms){
  ## list character_vector -> tibble
  ## expand a partial grid on lst where the base values are the first entry of
  ## each parameter in base_param_nms
  
  # compute full grid 
  tbl <- full_grid(lst)
  
  # take base parameters
  tbl_base_params <- tbl %>% select(all_of(base_param_nms))
  which_base_params <- names(lst) %in% base_param_nms
  base_params <- purrr::map(lst[which_base_params],
                            function(x){x[1]})
  
  
  # for each row of tbl, count how many columns match with base_params
  count_check <- purrr::map2(tbl_base_params,
                             base_params, 
                             function(x, y){x == y}) %>%
    tibble::as_tibble() %>%
    apply(MARGIN = 1, FUN =  sum)
  
  # keep only the rows that differ from base_params by at most one entry
  cond <- which(count_check >= ncol(tbl_base_params) - 1)
  tbl[cond, ]
  
}

check_list <- function(lst, required_names){
  ## lst character_vector -> boolean
  ## check whether the names of lst match exactly with required_names
  
  my_test <- dplyr::setequal(names(lst), required_names)
  if (my_test){
    return(TRUE)
  } else {
    stop(paste0("The given list should contain exactly the following entries: ",
                paste0(required_names, collapse = ", ")))
  }
  
}

set_rng <- function(tbl, seed){
  ## adds to tbl a column with seeds to generate independent streams of random
  ## numbers.
  ##
  ## Args:
  ##     - tbl: a tibble where the columns contain the parameter settings and the
  ##       rows contain the simulation runs.
  ##
  ## Returns:
  ##     The function returns tbl appending a column with seeds used to generate
  ##     independent streams of random numbers.
  ##
  ## Note:
  ##     This function ensures that the simulations are fully repeatable.
  ##     This is possible because it assigns to each simulation run a unique 
  ##     random seed (generated with L'Ecuyer RNG method, which is suitable 
  ##     for parallel processing, too).
  
  m <- n_groups(tbl)
  group_idxs <- group_indices_rebased(tbl)
  
  # create independent RNG streams with L'Ecuyer method
  rng <- rngtools::RNGseq(m, seed = seed, simplify = FALSE)
  curr_seed <- rng[[m]]
  rng <- rng[group_idxs]
  
  # add RNG streams to tbl
  tbl$rng <- rng
  
  # create independent RNG streams with L'Ecuyer method for CV
  if (has_name(tbl, "cv")){
    if (tbl[["cv"]][1] == TRUE){
      K <- tbl[["K"]][1]
      n_rep <- tbl[["n_rep"]][1]
      min.node.size <- tbl[["min.node.size"]][[1]]
      lambda <- tbl[["lambda"]][[1]]
      next_seed <- parallel::nextRNGStream(curr_seed)
      rng <- rngtools::RNGseq(m * (K * n_rep * length(min.node.size) *
                                     length(lambda)  + 1), 
                              seed = next_seed)
      
      fac <- rep(1:m, each = K * n_rep * length(min.node.size) * 
                   length(lambda) + 1)
      rng_cv <- tibble(rng_cv = unname(split(rng, fac)))
      tbl <- tbl %>% 
        bind_cols(rng_cv)
    }
  }
  
  # return tibble
  return(tbl)
}

group_indices_rebased <- function(tbl){
  ## tibble -> numeric_vector
  ## produce group labels in ascending order
  
  group_idxs <- tibble(idx_dplyr = group_indices(tbl))
  group_labels <- distinct(group_idxs) %>% 
    rownames_to_column(var = "idx_new") %>% 
    mutate(idx_new = as.integer(idx_new))
  
  idxs <- group_idxs %>% 
    left_join(group_labels, by = "idx_dplyr") %>% 
    select(idx_new)
  
  idxs$idx_new
  
}

assign_random_seed <- function(tbl, grouping_vars, seed){
  ## tibble character_vector integer -> tibble
  ## assign random seed according to the variables in grouping_vars
  grouping_vars <- sort(grouping_vars)
  
  if (is.null(grouping_vars)){
    tbl <- tbl %>%
      rowwise()
  } else {
    tbl <- tbl %>% 
      group_by(across(all_of(grouping_vars)))
  }
  
  tbl %>% 
    set_rng(seed) %>% 
    ungroup()
}

fix_list_names <- function(lst){
  ## list -> list
  ## fix list names by removing them and adding them back
  
  map(lst, function(x){unname(x)})
  
}