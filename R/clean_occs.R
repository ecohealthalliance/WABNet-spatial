#' Merge and clean occurrence records from multiple sources
#'
#'
#' @title clean_occs
#' @param occs_df results returned by occ2df
#' @param WABNet_coords coordinate data from WABNet project
#' @param darkcides coordinate data from DarkCideS
#' @param species_keep final species to keep
#' @return cleaned species occurrence records
#' @author Cecilia Sanchez
#' @example
#' clean_occs(occs_df, WABNet_coords, darkcides, CoV_species_names)
#' 
clean_occs <- function(occs_df, WABNet_coords, darkcides, species_keep){
  
  # for occs sourced from databases, only keep records since 1993 (inclusive)
  post_1992 <- occs_df %>% 
    mutate(year = lubridate::year(date)) %>% 
    filter(year >= 1993)
  
  # clean wabnet occs to bind
  occs_wabnet <- WABNet_coords %>% 
    rename(name = Species, latitude = Latitude, longitude = Longitude) %>% 
    mutate(latitude = gsub(" N", "", latitude),
           longitude = gsub(" E", "", longitude)) %>% 
    mutate(prov = "wabnet")
  
  # clean darkcides occs to bind
  occs_darkcides <- darkcides %>% 
    rename(name = Species.name, latitude = Lattitude, longitude = Longitude) %>% 
    mutate(prov = "darkcides") %>% 
    filter(Continent %in% c("Asia", "Europe"))
  
  # join the sourced occs with the wabnet and darkcides occs
  # and do some pre-cleaning
  occs_all <- bind_rows(post_1992, occs_wabnet) %>% 
    
    # change coords to numeric
    mutate(latitude = as.numeric(latitude),
           longitude = as.numeric(longitude)) %>% 
    
    bind_rows(occs_darkcides) %>% 
    
    # exclude any with missing coordinates
    drop_na(latitude) %>% 
    drop_na(longitude) %>% 
    
    # remove longitudinal outlier
    filter(longitude > -18) %>% 
    
    # get rid of all the subspecies
    tidyr::separate_wider_delim(cols = name, delim = " ",
                                names = c("genus", "species"), 
                                too_many  = "drop", too_few = "align_start") %>% 
    
    drop_na(species) %>% 
    # then unite back into a single column
    tidyr::unite(col = "name", genus:species, sep = " ") %>% 
    
    # filter out some species without real names
    filter(!name %in% c("Small Myotis", "Large Myotis", "Rhinolophus")) %>% 
    filter(!grepl("BOLD", name)) %>% 
    
    # fix some mis-spellings
    mutate(name = gsub("Miniopterus schreibersi", "Miniopterus schreibersii", 
                       name, fixed = T),
           name = gsub("Miniopterus schreibersiii", "Miniopterus schreibersii", 
                       name, fixed = T),
           name = gsub("Rousettus leschenaulti", "Rousettus leschenaultii", 
                       name, fixed = T),
           name = gsub("Rousettus leschenaultiii", "Rousettus leschenaultii", 
                       name, fixed = T),
           name = gsub("Scotophilus heathi", "Scotophilus heathii", 
                       name, fixed = T),
           name = gsub("Scotophilus heathiii", "Scotophilus heathii", 
                       name, fixed = T)
           ) %>% 
    
    # filter to only the CoV+ species
    filter(name %in% species_keep)
  
  # clean based on several CoordinateCleaner tests
  occs_cc <- CoordinateCleaner::clean_coordinates(
    x = occs_all,
    lon = "longitude",
    lat = "latitude",
    species = "name",
    tests = c("capitals", "centroids", "duplicates", "equal", "gbif", 
              "institutions", "seas", "validity", "zeros"),
    value = "clean"
    )
  
  return(occs_cc)
  
}

