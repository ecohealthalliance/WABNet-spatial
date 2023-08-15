#' Subset the IUCN data to only species of interest
#'
#'
#' @title get_iucn
#' @param IUCN_data mammal distribution data from IUCN
#' @param species character, species of interest
#' @return 
#' @author Cecilia Sanchez
#' @example
#' get_iucn(IUCN_data = mammals_file, species = species_for_enm)

get_iucn <- function(IUCN_data, species){
  
  mammals <- terra::vect(IUCN_data)
  
  # tedious but only way I could get this to work
  iucn_ranges <- subset(mammals, mammals[[2]] == species[1] | 
                          mammals[[2]] == species[2] | 
                          mammals[[2]] == species[3] |
                          mammals[[2]] == species[4] |
                          mammals[[2]] == species[5] |
                          mammals[[2]] == species[6] |
                          mammals[[2]] == species[7] |
                          mammals[[2]] == species[8] |
                          mammals[[2]] == species[9] |
                          mammals[[2]] == species[10] |
                          mammals[[2]] == species[11] |
                          mammals[[2]] == species[12] 
                        ) 
  
  return(iucn_ranges)
  
}


