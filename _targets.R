################################################################################
#
# Project build script
#
################################################################################

# Load packages (in packages.R) and load project-specific functions in R folder
suppressPackageStartupMessages(source("packages.R"))
for (f in list.files(here::here("R"), full.names = TRUE)) source (f)

# Set build options ------------------------------------------------------------


# Groups of targets ------------------------------------------------------------

## Data input
data_input_targets <- tar_plan(
  tar_file(CoV_species_file, "data/Habitat info - CoV+ Western Asia bat species - updated 29 June 2023.csv"),
  tar_file(WABNet_coords_file, "data/GPS coordinates for WABNet captured species.csv"),
  # https://figshare.com/articles/dataset/Metadata_for_DarkCideS_1_0_a_global_database_for_bats_in_karsts_and_caves/16413405?file=34091939
  tar_file(darkcides_file, "data/DarkCideS_v4_dataset 2.csv"),
  
  CoV_species = read.csv(CoV_species_file, na.strings = c("NA", "n/a")),
  WABNet_coords = read.csv(WABNet_coords_file, na.strings = c("NA", "n/a")),
  darkcides = read.csv(darkcides_file, na.strings = c("N/A"))
)


## Data processing
data_processing_targets <- tar_plan(
  CoV_species_names = CoV_species$Species,
  occs_df = get_occs(species_names = CoV_species_names, 
                     sources = c("gbif", "vertnet"), 
                     limit = 30000),
  occs_cc = clean_occs(occs_df, WABNet_coords, darkcides, CoV_species_names),
  occs_thinned = thin_occs(occs_cc),
  
  #species_counts = occs_cc %>% group_by(name) %>% dplyr::summarise(n = n()),
  
  occs_ENM = occs_thinned %>% 
    group_by(name) %>%
    mutate(n_individ = n()) %>%
    filter(n_individ > 40) %>%
    ungroup()

)


## Analysis
analysis_targets <- tar_plan(

)

## Outputs
outputs_targets <- tar_plan(
  ## This is a placeholder for any targets that produces outputs such as
  ## tables of model outputs, plots, etc. Delete or keep empty if you will not
  ## produce any of these types of outputs
)


## Report
report_targets <- tar_plan(
  ## Example Rmarkdown report target/s; delete and replace with your own
  ## Rmarkdown report target/s
  
  # tar_render(
  #   example_report, path = "reports/example_report.Rmd", 
  #   output_dir = "outputs", knit_root_dir = here::here()
  # )
)

## Deploy targets
deploy_targets <- tar_plan(
  ## This is a placeholder for any targets that are meant to deploy reports or
  ## any outputs externally e.g., website, Google Cloud Storage, Amazon Web
  ## Services buckets, etc. Delete or keep empty if you will not perform any
  ## deployments. The aws_s3_upload function requires AWS credentials to be loaded
  ## but will print a warning and do nothing if not
  
  # html_files = containerTemplateUtils::get_file_paths(tar_obj = example_report,
  #                                                     pattern = "\\.html$"),
  # uploaded_report = containerTemplateUtils::aws_s3_upload(html_files,
  #                                                       bucket = Sys.getenv("AWS_BUCKET"),
  #                                                       error = FALSE,
  #                                                       file_type = "html"),
  # email_updates= 
  #   containerTemplateUtils::send_email_update(
  #     to = strsplit(Sys.getenv("EMAIL_RECIPIENTS"),";")[[1]],
  #     from = Sys.getenv("EMAIL_SENDER"),
  #     project_name = "My Project",
  #     attach = TRUE
  #   )
)

# List targets -----------------------------------------------------------------

list(
  data_input_targets,
  data_processing_targets,
  analysis_targets,
  outputs_targets,
  report_targets,
  deploy_targets
)
