#' Main ENM function
#'
#'
#' @title build_sdm
#' @param species character, scientific name for one species
#' @param occs all species locations
#' @param iucn_ranges trimmed SpatVector for all focal species
#' @param env_stack SpatRaster of all predictor variables
#' @param n_bg number of background points
#' @param seed Seed to ensure replication in random sampling of background pts
#' @return 
#' @author Cecilia Sanchez
#' @example
#' build_sdm(species = "Myotis myotis", ccs = occs_ENM, iucn_ranges, env_stack, n_bg = 500)

build_sdm <- function(species, occs, iucn_ranges, env_stack, n_bg, seed = 42){
  
  env_stack <- terra::rast(env_stack)
  iucn_ranges <- terra::vect(iucn_ranges)
  
  # subset all occurrence points to those of the focal species
  occs_species <- occs %>% 
    filter(name == species)
  occs_sf <- sf::st_as_sf(occs_species, coords = c("longitude", "latitude"),
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
  bg_xy <- terra::spatSample(envs_mask, n_bg, xy = T, values = F, na.rm = T)
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
  # can take a while depending on number of points
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
  
  # Select optimal model using "a sequential method that uses cross-validation 
  # results by selecting models with the lowest average test omission rate, and 
  # to break ties, with the highest average validation AUC (Radosavljevic & Anderson 2014)"
  # https://jamiemkass.github.io/ENMeval/articles/ENMeval-2.0-vignette.html#select
  opt_mod <- e@results %>% 
    filter(or.10p.avg == min(or.10p.avg)) %>% 
    filter(auc.val.avg == max(auc.val.avg))
  
  best_mod_name <- paste0("fc.", opt_mod$fc, "_rm.", opt_mod$rm)
  best_mod <- e@models[[best_mod_name]]
  
  print(paste0("Optimal model for ", species, ": ", best_mod_name))
  print(paste("AUC.val.avg =", round(opt_mod$auc.val.avg, 2)))
  
  
  # IUCN range of the species as an extent to project the SDM to
  species_range <- subset(iucn_ranges, iucn_ranges[[2]] == species)

  #iucn_range <- terra::project(iucn_range, "+proj=longlat +ellps=WGS84 +no_defs") 

  # crop and mask the environmental stack by the species IUCN range
  pred_proj <- terra::crop(env_stack, species_range, mask = T)
  # predict values within this range
  species_preds <- dismo::predict(best_mod, pred_proj, 
                                  args = c("outputformat=cloglog"))
  
  # save raster of predictions 
  writeRaster(species_preds, paste0("outputs/full_", species, ".tif"),
              overwrite = TRUE)
  
  # binary output based on 75% probability thresholds (IUCN range constrained)
  if(max(values(species_preds), na.rm = T) >= 0.75){
    preds75 <- species_preds
    preds75[preds75 >= 0.75] <- 1
    preds75[preds75 < 0.75] <- NA
    writeRaster(preds75, paste0("outputs/binary_", species, "_75.tif"),
                overwrite = TRUE)
  }else(print("No areas have >= 75% predicted probability of presence"))
  
  
  return(e)
  
}

# species <- "Myotis myotis"
# tar_load(occs_ENM)
# occs <- occs_ENM
# tar_load(env_stack)
# env_stack <- terra::rast(env_stack)
# tar_load(iucn_ranges)
# iucn_ranges <- terra::vect(iucn_ranges)
# n_bg = 500
# seed <- 42
