################################################################################
#
# Project build script
#
################################################################################

# Load packages (in packages.R) and load project-specific functions in R folder
suppressPackageStartupMessages(source("packages.R"))
for (f in list.files(here::here("R"), full.names = TRUE)) source (f)

# Set build options ------------------------------------------------------------
tar_option_set(
  resources = tar_resources(
    qs = tar_resources_qs(preset = "fast")),
  format = "qs"
)

# Groups of targets ------------------------------------------------------------

## Data input
data_input_targets <- tar_plan(
  tar_file(CoV_species_file, "data/Habitat info - CoV+ Western Asia bat species - updated 11 Dec 2023.csv"),
  tar_file(WABNet_coords_file, "data/GPS coordinates for WABNet captured species.csv"),
  # https://figshare.com/articles/dataset/Metadata_for_DarkCideS_1_0_a_global_database_for_bats_in_karsts_and_caves/16413405?file=34091939
  tar_file(darkcides_file, "data/DarkCideS_v4_dataset 2.csv"),
  
  tar_file(mammals_file, "data/MAMMALS_TERRESTRIAL_ONLY/MAMMALS_TERRESTRIAL_ONLY.shp"),
  
  #geodata::worldclim_global(var = "bio", res = 2.5, path = "data/")
  tar_file(bio1_2.5m_file, "data/wc2.1_2.5m/wc2.1_2.5m_bio_1.tif"),
  tar_file(bio4_2.5m_file, "data/wc2.1_2.5m/wc2.1_2.5m_bio_4.tif"),
  tar_file(bio12_2.5m_file, "data/wc2.1_2.5m/wc2.1_2.5m_bio_12.tif"),
  tar_file(bio15_2.5m_file, "data/wc2.1_2.5m/wc2.1_2.5m_bio_15.tif"),
  
  # https://figshare.com/articles/dataset/Harmonization_of_DMSP_and_VIIRS_nighttime_light_data_from_1992-2018_at_the_global_scale/9828827/7
  tar_file(ntl_file, "data/Harmonized_DN_NTL_2021_simVIIRS.tif"),
  
  # https://download.bgr.de/bgr/grundwasser/whymap/shp/WHYMAP_WOKAM_v1.zip
  # https://www.whymap.org/whymap/EN/Maps_Data/Wokam/wokam_node_en.html
  tar_file(karst_file, "data/WHYMAP_WOKAM/shp/whymap_karst__v1_poly.shp"),
  
  CoV_species = read.csv(CoV_species_file, na.strings = c("NA", "n/a")),
  WABNet_coords = read.csv(WABNet_coords_file, na.strings = c("NA", "n/a")),
  darkcides = read.csv(darkcides_file, na.strings = c("N/A")),
  
  tar_file(CoV_prev_file, "data/WABNet_CoV_prevalence_01December2023.csv"),
  CoV_prev = read.csv(CoV_prev_file, na.strings = "")
)


## Data processing
data_processing_targets <- tar_plan(
  
  # 2.5 min resolution
  env_stack = terra::wrap(make_stack(bio1_2.5m_file, bio4_2.5m_file,
                                     bio12_2.5m_file, bio15_2.5m_file,
                                     karst_file, ntl_file)),
    
  CoV_species_names = CoV_species$Species,
  # takes a couple hours
  occs_df = get_occs(species_names = CoV_species_names, 
                     sources = c("gbif", "vertnet"), 
                     limit = 30000),
  occs_cc = clean_occs(occs_df, WABNet_coords, darkcides, CoV_species_names),
  occs_thinned = thin_occs(occs_cc),
  
  species_for_enm = occs_cc %>% 
    group_by(name) %>% 
    dplyr::summarise(n = n()) %>% 
    filter(n > 40) %>% 
    pull(name),
  
  occs_ENM = occs_thinned %>% 
    filter(name %in% species_for_enm),
  
  iucn_ranges = terra::wrap(get_iucn(IUCN_data = mammals_file, 
                                     species = species_for_enm)),
  
  heat_data = prep_quarterly_heatmap_data(CoV_prev)

)


## Analysis
analysis_targets <- tar_plan(
  e_Hysa = build_sdm(species = "Hypsugo savii", occs = occs_ENM,
                     iucn_ranges, env_stack, n_bg = 10000),
  e_Misc = build_sdm(species = "Miniopterus schreibersii", occs = occs_ENM,
                     iucn_ranges, env_stack, n_bg = 10000),
  e_Mybl = build_sdm(species = "Myotis blythii", occs = occs_ENM, iucn_ranges,
                     env_stack, n_bg = 10000),
  e_Myem = build_sdm(species = "Myotis emarginatus", occs = occs_ENM,
                     iucn_ranges, env_stack, n_bg = 10000),
  e_Mymy = build_sdm(species = "Myotis myotis", occs = occs_ENM, iucn_ranges,
                     env_stack, n_bg = 10000),
  e_Pija = build_sdm(species = "Pipistrellus javanicus", occs = occs_ENM,
                     iucn_ranges, env_stack, n_bg = 10000),
  e_Piku = build_sdm(species = "Pipistrellus kuhlii", occs = occs_ENM,
                     iucn_ranges, env_stack, n_bg = 10000),
  e_Plma = build_sdm(species = "Plecotus macrobullaris", occs = occs_ENM,
                     iucn_ranges, env_stack, n_bg = 10000),
  e_Rhbl = build_sdm(species = "Rhinolophus blasii", occs = occs_ENM,
                     iucn_ranges, env_stack, n_bg = 10000),
  e_Rheu = build_sdm(species = "Rhinolophus euryale", occs = occs_ENM,
                     iucn_ranges, env_stack, n_bg = 10000),
  e_Rhfe = build_sdm(species = "Rhinolophus ferrumequinum", occs = occs_ENM,
                     iucn_ranges, env_stack, n_bg = 10000),
  e_Rhmi = build_sdm(species = "Rhinopoma microphyllum", occs = occs_ENM,
                     iucn_ranges, env_stack, n_bg = 10000),
  e_Roae = build_sdm(species = "Rousettus aegyptiacus", occs = occs_ENM,
                     iucn_ranges, env_stack, n_bg = 10000),
  e_Role = build_sdm(species = "Rousettus leschenaultii", occs = occs_ENM,
                     iucn_ranges, env_stack, n_bg = 10000),
  e_Sche = build_sdm(species = "Scotophilus heathii", occs = occs_ENM,
                     iucn_ranges, env_stack, n_bg = 10000)
)

## Outputs
outputs_targets <- tar_plan(

  multipanel = map_predicted_distributions(),
  quarterly_heatmap = plot_quarterly_heatmap(heat_data)
)

# List targets -----------------------------------------------------------------

list(
  data_input_targets,
  data_processing_targets,
  analysis_targets,
  outputs_targets
)
