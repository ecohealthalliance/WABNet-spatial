#' Main ENM function
#'
#'
#' @title build_sdm
#' @param species character, scientific name
#' @param occs all species locations
#' @param env_stack SpatRaster of all predictor variables
#' @param seed Seed to ensure replication in random sampling of background pts
#' @return 
#' @author Cecilia Sanchez
#' @example
#' build_sdm(species = "Myotis myotis", occs = occs_ENM, env_stack)

build_sdm <- function(species, occs, env_stack, seed = 42){
  
  env_stack <- terra::rast(env_stack)
  
  # subset all occurrence points to those of the focal species
  occs_species <- occs %>% 
    filter(name == species)
  occs_sf <- st_as_sf(occs_species, coords = c("longitude", "latitude"),
                      crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
  occs_xy <- occs_species[, c("longitude", "latitude")]
  
  # build convex hull around focal species points
  occs_vect <- terra::vect(occs_sf)
  occs_hull <- terra::convHull(occs_vect)
  
  # buffer the hull by 10km (width param is in meters)
  occs_buff <- terra::buffer(occs_hull, width = 10000)
  
  # crop and mask the env_stack by the buffered hull
  envs_mask <- terra::crop(env_stack, occs_buff, mask = T) 
  
  # sample background (pseudo absence) points within the buffered hull
  bg_xy <- terra::spatSample(envs_mask, 10000, xy = T, values = F, na.rm = T)
  # convert matrix output to data frame
  bg_xy <- as.data.frame(bg_xy)
  colnames(bg_xy) <- c("longitude", "latitude")
  
  # partition occurrence data into spatial blocks
  blocks <- ENMeval::get.block(occs = occs_xy, bg = bg_xy)
  
  # pull out the occurrence and background partition group numbers from the list
  occs.grp <- blocks$occs.grp
  bg.grp <- blocks$bg.grp
  
  # ensure enough Java heap space to run maxent
  options(java.parameters = "-Xmx8000m")
  
  # build and evaluate environmental niche model
  # iterate model building over all chosen parameter settings
  # can take a few min depending on number of points
  e <- ENMeval::ENMevaluate(occs = occs_xy, envs = envs_mask, bg = bg_xy, 
                            # feature classes and regularization multipliers
                            # fcs are linear, quadratic, hinge, product
                            tune.args = list(fc = c("L", "LQ", "H", "LQH", 
                                                    "LQHP"),
                                             rm = seq(0.5, 2.5, 0.5)),
                            partitions = "user", algorithm = "maxent.jar",
                            user.grp = list(occs.grp = occs.grp, 
                                            bg.grp = bg.grp),
                            categoricals = c("karst"))
  
  return(e)
  
}

# species <- "Myotis myotis"
# tar_load(occs_ENM)
# occs <- occs_ENM
# tar_load(env_stack)
# env_stack <- terra::rast(env_stack)
# seed <- 42
