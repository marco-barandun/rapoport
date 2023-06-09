library(raster)
library(tidyverse)
library(dismo)

#setwd("~/GitHub/clean_rapoport_v2")
base::setwd("."); getwd()

run_mess <- function(species_list, envs_var) {
  
  for (species in species_list) {
    
    print(species)
    
    model <- readRDS(paste("./../3_generated_data/res_saved_models/", species, "_models.rds", sep = ""))
    
    points <- rbind(model@occs, model@bg) %>% # for the new mess I need to sample random points from the whole world
      distinct() %>%
      dplyr::select(-latitude, -longitude)
    
    start <- Sys.time()
    ms <- mess(envs_var, points, full = FALSE)
    end <- Sys.time()
    
    print(paste("Computing the mess value took:", difftime(end, start, units='mins'), "minutes"))
    
    value <- cellStats(ms, function(i, ...) sum(na.omit(i) >= 0))/cellStats(ms, function(i, ...) length(na.omit(i)))
    
    write_csv(data.frame(species, value), paste("./../3_generated_data/res_MESS/", species, "_MESS.csv", sep = ""))
    
    print("Done!")
    
  }
}

envs <- raster::stack(list.files(path="./../1_original_data/envs_standardized/", pattern='*.tif$*', full.names=TRUE)) %>%
  raster::brick(.)

modelled_species <- list.files(path ="./../3_generated_data/res_maxent_result_tables/") %>%
  sub("_maxent_results.csv", "", .)
MESS_species <- list.files(path ="./../3_generated_data/res_MESS/") %>%
  sub("_MESS.csv", "", .)

species_to_run <- modelled_species %>%
  setdiff(., MESS_species)

parallel::mclapply(species_to_run, function(species) {
  run_mess(species_list = species,
                        envs_var = envs)
  }, mc.cores = 8)

