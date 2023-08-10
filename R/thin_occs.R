#' Thin occurrences for all species
#'
#'
#' @title thin_occs
#' @param occs_df species occurrence records after cleaning
#' @return thinned species occurrence records
#' @author Cecilia Sanchez
#' @example
#' thin_occs(occs_cc)
#' 
thin_occs <- function(occs_cc){
  
  focal_species <- sort(unique(occs_cc$name)) 
  
  occs_thinned_list <- list()
  
  for(i in 1:length(focal_species)){
    
    occs_species <- occs_cc %>% 
      filter(name == focal_species[i]) %>% 
      dplyr::select(name, latitude, longitude)
    
      # print(paste("Number of  occurrence points before thinning:", 
      #         nrow(occs_species)))
      
    # spatial thin at 10 km
    thin_output <- spThin::thin(occs_species, "latitude", "longitude", "name", 
                                thin.par = 10, reps = 100, 
                                locs.thinned.list.return = TRUE, 
                                write.files = FALSE,
                                verbose = FALSE)
    
    # maximize number of localities
    # find the iteration that returns the max number of occurrences
    max_thin <- which(sapply(thin_output, nrow) == max(sapply(thin_output, nrow)))
    # if there's more than one max, pick the first one
    max_thin <- thin_output[[ifelse(length(max_thin) > 1, max_thin[1], max_thin)]]  
    # subset occs to match only thinned occs
    occs_thinned <- occs_species[as.numeric(rownames(max_thin)), ]  
    
    # print(paste("Number of occurrence points after thinning:", nrow(occs_thinned)))
    
    occs_thinned_list[[i]] <- occs_thinned
  }
  
  occs_thinned_df <- bind_rows(occs_thinned_list)
  
  return(occs_thinned_df)
  
}

  