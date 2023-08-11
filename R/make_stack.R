#' Create stack of predictor variables
#'
#'
#' @title make_stack
#' @param bio1_file file of annual mean temperature
#' @param bio4_file file of temperature seasonality
#' @param bio12_file file of annual precipitation
#' @param bio15_file file of precipitation seasonality
#' @param karst_file shapefile of karstifiable rocks
#' @param ntl_file nighttime lights file
#' @return SpatRaster of all predictor layers
#' @author Cecilia Sanchez
#' @example
#' make_stack(bio1_file = bio1_2.5m_file, bio4_file = bio4_2.5m_file, bio12_file = bio12_2.5m_file, bio15_file = bio15_2.5m_file, karst_file, ntl_file)


make_stack <- function(bio1_file, bio4_file, bio12_file, bio15_file, karst_file,
                       ntl_file){

  new_ext <- terra::ext(c(-20, 140, -35, 65))

  # load and crop bioclim layers
  bio1 <- terra::rast(bio1_file)
  bio1_crop <- terra::crop(bio1, new_ext)
  names(bio1_crop) <- "bio1"
  rm(bio1)
  bio4 <- terra::rast(bio4_file)
  bio4_crop <- terra::crop(bio4, new_ext)
  names(bio4_crop) <- "bio4"
  rm(bio4)
  bio12 <- terra::rast(bio12_file)
  bio12_crop <- terra::crop(bio12, new_ext)
  names(bio12_crop) <- "bio12"
  rm(bio12)
  bio15 <- terra::rast(bio15_file)
  bio15_crop <- terra::crop(bio15, new_ext)
  names(bio15_crop) <- "bio15"
  rm(bio15)

  # load and rasterize and crop the karst layer
  karst <- terra::vect(karst_file)
  # crop wider to start
  karst_init_crop <- terra::crop(karst, terra::ext(c(-40, 140, -35, 65)))
  rm(karst)
  
  # decide cell size based on which resolution of bioclim layer is used
  if(dim(bio1_crop)[1] == 12000){
    r <- terra::rast(nrow = 12000, ncol = 21600)
  } else if(dim(bio1_crop)[1] == 2400){
    r <- terra::rast(nrow = 2400, ncol = 4320)
  }

  terra::ext(r) <- terra::ext(karst_init_crop)
  karst_ras <- terra::rasterize(karst_init_crop, r)
  rm(r)
  rm(karst_init_crop)
  # now crop again to match the other files
  karst_crop <- terra::crop(karst_ras, new_ext)
  # set NAs to 0
  karst_crop[is.na(karst_crop)] <- 0
  names(karst_crop) <- "karst"
  rm(karst_ras)

  # load and crop nighttime lights layer
  ntl <- terra::rast(ntl_file)
  ntl_crop <- terra::crop(ntl, new_ext)
  # force extent to match exactly
  terra::ext(ntl_crop) <- new_ext
  rm(ntl)
  
  # if using the lower res bioclim data, need to resample ntl
  if(dim(bio1_crop)[1] == 2400){
    ntl_crop <- terra::resample(ntl_crop, bio1_crop)
  }
    names(ntl_crop) <- "ntl"

  
  # finally stack all the layers
  env_stack <- c(bio1_crop, bio4_crop, bio12_crop, bio15_crop, karst_crop, 
                 ntl_crop)
  rm(list = c("bio1_crop", "bio4_crop", "bio12_crop", "bio15_crop", 
              "karst_crop", "ntl_crop"))
  
  return(env_stack)

}
