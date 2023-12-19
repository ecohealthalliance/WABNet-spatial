#' Subset the IUCN data to only species of interest
#'
#'
#' @title get_iucn
#' @param IUCN_data mammal distribution data from IUCN
#' @param species character, names of species of interest
#' @return 
#' @author Cecilia Sanchez
#' @example
#' get_iucn(IUCN_data = mammals_file, species = species_for_enm)

get_iucn <- function(IUCN_data, species){
  
  mammals <- terra::vect(IUCN_data)
  
  iucn_ranges <- terra::subset(mammals, mammals$sci_name %in% species)
  
  # free memory
  gc()
  
  return(iucn_ranges)
  
}


