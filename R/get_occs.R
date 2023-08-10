#' Query species occurrence using spocc package
#'
#'
#' @title get_occs

#' @param species_names vector of scientific species names
#' @param sources vector of data sources to query with occ. choices are
#'               "gbif", "inat", "ebird", "vertnet", "idigbio", "obis", "ala"
#' @return dataframe of occurrences
#' @author Cecilia Sanchez
#' @example
#' get_occs(species_names = CoV_species_names[1], sources = "gbif", limit = 50)

get_occs <- function(species_names, sources = c("gbif", "vertnet"),
                     limit){

  occs <- spocc::occ(query = species_names, from = sources, 
                     limit = limit,
                     has_coords = T, 
                     gbifopts = list(occurrenceStatus = "PRESENT"))
  print(warnings())
  
  occs_df <- occ2df(occs)
  
  return(occs_df)
  
}
