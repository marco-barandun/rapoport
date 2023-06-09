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
    
    # set the extent of the northern hemisphere
    extent_northern_hemisphere <- extent(-180, 180, 0, 90)
    
    # set the extent of the southern hemisphere
    extent_southern_hemisphere <- extent(-180, 180, -90, 0)
    
    # subset the raster brick to only one hemisphere
    envs_var_N <- envs_var %>% crop(., extent_northern_hemisphere)
    envs_var_S <- envs_var %>% crop(., extent_southern_hemisphere)
    
    ms_N <- mess(envs_var_N, points, full = FALSE)
    ms_S <- mess(envs_var_S, points, full = FALSE)
    
    #print(paste("Computing the mess values took:", difftime(end, start, units='mins'), "minutes"))
    
    mess_n <- cellStats(ms_N, function(i, ...) sum(na.omit(i) >= 0))/cellStats(ms_N, function(i, ...) length(na.omit(i)))
    mess_s <- cellStats(ms_S, function(i, ...) sum(na.omit(i) >= 0))/cellStats(ms_S, function(i, ...) length(na.omit(i)))
    
    write_csv(data.frame(species, value_N, value_S), paste("./../3_generated_data/res_MESS_perHemisphere/", species, "_MESS_perHemisphere.csv", sep = ""))
    
    #writeRaster(ms_N, filename= paste("./../3_generated_data/res_MESS_perHemisphere_tif/", species, "_MESS_perHemisphere_N.tif", sep = ""), format="GTiff")
    #writeRaster(ms_S, filename= paste("./../3_generated_data/res_MESS_perHemisphere_tif/", species, "_MESS_perHemisphere_S.tif", sep = ""), format="GTiff")
    
    print("Done!")
    
  }
}

envs <- raster::stack(list.files(path="./../1_original_data/envs_standardized/", pattern='*.tif$*', full.names=TRUE)) %>%
  raster::brick(.)

# view the subsetted raster stack
plot(envs %>% crop(., extent(-180, 180, -90, 0)))
  
modelled_species <- list.files(path ="./../3_generated_data/res_maxent_result_tables/") %>%
  sub("_maxent_results.csv", "", .)
MESS_perHemisphere_species <- list.files(path ="./../3_generated_data/res_MESS_perHemisphere/") %>%
  sub("_MESS_perHemisphere.csv", "", .)

species_to_run <- modelled_species %>%
  setdiff(., MESS_perHemisphere_species)

parallel::mclapply(species_to_run, function(species) {
  run_mess(species_list = species,
                        envs_var = envs)
  }, mc.cores = 8)

