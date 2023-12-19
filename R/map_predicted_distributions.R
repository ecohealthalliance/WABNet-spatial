#' Make a multi-panel grid of predicted species presence
#'
#'
#' @title map_predicted_distributions
#' @return 
#' @author Cecilia Sanchez

map_predicted_distributions <- function(){
  # import a world countries map
  countries <- geodata::world(resolution = 3, path = tempdir())
  
  # countries of interest
  WA_names <- c("Afghanistan", "Armenia", "Azerbaijan", "Bahrain", "Georgia",
                "Iran", "Iraq", "Israel", "Jordan", "Kuwait", "Lebanon", "Oman",
                "Pakistan", "Palestine", "Qatar", "Saudi Arabia", "Syria", 
                "Turkey", "United Arab Emirates", "Yemen")
  
  # get ISO3 country codes for WA countries
  WA_codes <- geodata::country_codes() %>% 
    dplyr::filter(NAME %in% WA_names) %>% 
    pull(ISO3)
  
  # subset the full countries map to just WA countries
  WA_countries <- countries[countries$GID_0 %in% WA_codes]
  # make an sf object for later plotting
  WA_countries_sf <- sf::st_as_sf(WA_countries)
  
  # get list of all generated predictions rasters
  bat_ras_names <- list.files(path = "outputs/", pattern = "full", 
                              full.names = TRUE)
  
  maps_list <- list()
  
  for(i in 1:length(bat_ras_names)){
    
    # pull one raster
    bat_ras <- terra::rast(bat_ras_names[i])
    # get the species name for adding plot title
    species_name <- strsplit(bat_ras_names[i], '[_.]')[[1]][2]
    
    # crop/mask to WA countries
    bat_ras_cropped <- terra::crop(bat_ras, WA_countries, mask = T)
    # align CRS
    terra::crs(bat_ras_cropped) <- terra::crs(WA_countries)
    
    # plot using tidyterra functionality
    maps_list[[i]] <- ggplot() +
      tidyterra::geom_spatvector(data = WA_countries, fill = "gray95") +
      tidyterra::geom_spatraster(data = bat_ras_cropped) +
      scale_fill_viridis_c(option = "D", na.value = "transparent", 
                           name = "Predicted probability \nof species presence",
                           breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1)) +
      tidyterra::geom_spatvector(data = WA_countries, fill = "transparent",
                                 lwd = 0.5) +
      labs(x = "Longitude", y = "Latitude") +
      ggtitle(species_name) +
      theme_bw() +
      theme(axis.text = element_text(color = "black"))
  }
  
  # create initial grid with no legends
  cowplot::plot_grid(maps_list[[1]] + theme(legend.position = "none"), 
                     maps_list[[2]] + theme(legend.position = "none"), 
                     maps_list[[3]] + theme(legend.position = "none"), 
                     maps_list[[4]] + theme(legend.position = "none"), 
                     maps_list[[5]] + theme(legend.position = "none"), 
                     maps_list[[6]] + theme(legend.position = "none"), 
                     maps_list[[7]] + theme(legend.position = "none"), 
                     maps_list[[8]] + theme(legend.position = "none"), 
                     maps_list[[9]] + theme(legend.position = "none"), 
                     maps_list[[10]] + theme(legend.position = "none"), 
                     maps_list[[11]] + theme(legend.position = "none"), 
                     maps_list[[12]] + theme(legend.position = "none"), 
                     maps_list[[13]] + theme(legend.position = "none"), 
                     maps_list[[14]] + theme(legend.position = "none"), 
                     maps_list[[15]] + theme(legend.position = "none"), 
                     ncol = 3) -> my_grid
  
  # make one legend to go at the bottom
  legend_below <- cowplot::get_legend(maps_list[[1]] + 
                                        guides(color = guide_legend(nrow = 1)) +
                                        theme(legend.position = "bottom"))
  
  # add initial grid plus the single legend
  cowplot::plot_grid(my_grid, legend_below, ncol = 1, rel_heights = c(1, 0.1))
  
  
  rm(list = "bat_ras", "bat_ras_cropped", "countries", "WA_countries", 
     "WA_countries_sf", "my_grid", "maps_list")
  
  ggsave("outputs/species_predictions.png", width = 8.5, height = 11, 
         units = "in", dpi = 600)
}
