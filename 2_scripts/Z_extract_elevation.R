library(raster)

# Extract elevation from a raster

occurrences <- read_csv("./../1_original_data/herb_occs-105450sp.csv")
occ_xy <- occurrences %>% dplyr::select(latitude, longitude)
coordinates(occ_xy) <- c("longitude", "latitude")

elevation <- raster::stack("./../1_original_data/EarthEnvTopoMed_Elevation.tif") %>%
  raster::brick(.)

occurrences$elevation <- raster::extract(elevation, occ_xy)
occurrences <- as.data.frame(occurrences) %>%
  rename("elevation" = "EarthEnvTopoMed_Elevation")

occurrences$elevation <- round(occurrences$elevation, digits = 0)

write_csv(occurrences, "./../1_original_data/herb_occs_elevation-105450sp.csv")
