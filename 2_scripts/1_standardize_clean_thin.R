library(dismo)
library(sp)
library(rgdal)
library(rgeos)
library(rgbif)
library(tidyverse)

setwd(".")

# Function to standardize datasets
standardize.ds <- function(ds, # Species occurrence dataset
                           sp.excl = read_csv("./../1_original_data/tree_species.csv"), # Species to exclude (in our case, species of trees)
                           taxonomic.backbone = tax.backbone # Taxonomic backbone
                           ) {
  
  # Matching the scientific names within the dataset with the scientific names in the taxonomic backbone and extracting the accepted names
  ds <- ds %>%
    mutate(accepted_name = tax.backbone[match(.$Species, tax.backbone$scientific_name),]$accepted_name) %>% # complete, accepted scientific name
    mutate(accepted_name_aggr = tax.backbone[match(.$Species, tax.backbone$scientific_name),]$accepted_name_aggr) # accepted name, aggregated to the species level
  
  # In case the scientific name in the dataset was not present in the taxonomic backbone, we try to standardize it and match it to the aggregated species name
  ds[is.na(ds$accepted_name),]$accepted_name_aggr <- ds[is.na(ds$accepted_name),] %>%
    mutate(Species = sub("^(\\S*\\s+\\S+).*", "\\1", .$Species)) %>%
    mutate(Species = gsub(" ", "_", Species)) %>%
    mutate(accepted_name_aggr = tax.backbone[match(.$Species, tax.backbone$accepted_name_aggr),]$accepted_name_aggr) %>%
    .$accepted_name_aggr
  
  # Removing all the observations which we were not able to identify with the backbone, and removing the trees from the dataset
  ds <- ds[!is.na(ds$accepted_name_aggr),] %>%
    mutate(family = tax.backbone[match(.$accepted_name_aggr, tax.backbone$accepted_name_aggr),]$family) %>%
    dplyr::anti_join(., sp.excl, by = "accepted_name_aggr") %>%
    select(accepted_name_aggr, everything()) %>%
    select(-Species) %>%
    rename(Species = accepted_name_aggr)
}

### Function to clean occurrences
clean.occ <- function(ds, # Dataset with specues occurrence data - 3rd column = latitude; 4th column = longitude
                     world.shapefile.dir = "./../1_original_data/world_shapefile/world_adm0.shp" # World GADM shapefile, level 0
                     ) {
  
  # Creating a SpatialPointsDataFrame
  occs.to.clean <- sp::SpatialPointsDataFrame(coords = ds[, c(4,3)], 
                                              data = ds,
                                              )
  
  world_adm <- rgdal::readOGR(world.shapefile.dir) 
  raster::crs(occs.to.clean) <- raster::crs(world_adm)
  ovr <- sp::over(occs.to.clean, world_adm) # Overlay of the world shapefile and species observation points 
  cntr <- as.character(ovr$NAME) # generate control, areas with named land
  
  ## Select all points in land of which the countries in the database match the country from the world shapefile
  occs.cleaned <- as.data.frame(occs.to.clean[intersect(which(!is.na(cntr)), which(cntr == as.character(occs.to.clean$country))) ,]) %>%
    select(Species, id, longitude, latitude, country, source, accepted_name, family)
  
  return(occs.cleaned)
  }

# Creating the taxonomic backbone
#####
# Reading in the GBIF taxonomic backbone
tax.backbone <- read_tsv("./../1_original_data/backbone_taxonomy/backbone/Taxon.tsv") %>%
  filter(kingdom == "Plantae") %>%
  filter(phylum == "Tracheophyta") %>%
  filter(!is.na(specificEpithet)) %>%
  rename(scientific_name = scientificName)

# Selecting the accepted scientific names
accepted_names_db <- tax.backbone %>%
  filter(taxonomicStatus == "accepted") %>%
  mutate(accepted_name = scientific_name) %>%
  filter(!is.na(scientific_name)) %>%
  select(taxonID, accepted_name, scientific_name, family); head(accepted_names_db)

# Selecting the synonyms of the accepted scientific names
synonym_names_db <- tax.backbone %>%
  filter(taxonomicStatus == "synonym" | 
           taxonomicStatus == "heterotypic synonym" |
           taxonomicStatus == "homotypic synonym" |
           taxonomicStatus == "proparte synonym") %>%
  mutate(accepted_name = tax.backbone[match(.$acceptedNameUsageID, tax.backbone$taxonID),]$scientific_name) %>%
  filter(!is.na(scientific_name)) %>%
  filter(!is.na(accepted_name))%>%
  select(taxonID, accepted_name, scientific_name, family); head(synonym_names_db)

# Merging the accepted scientific names and the synonyms and standardizing them
tax.backbone <- rbind(accepted_names_db, synonym_names_db) %>%
  mutate(accepted_name_aggr = sub("^(\\S*\\s+\\S+).*", "\\1", .$accepted_name)) %>%
  mutate(accepted_name_aggr = gsub(" ", "_", accepted_name_aggr))

rm(accepted_names_db, synonym_names_db)
#####

#---------------------------------------------------------- Start cleaning

#------- Biodiversidata
#read_csv("./../1_original_data/original_datasets/X1_URUGUAY_Biodiversidata_Plants.csv") %>%
 # mutate(id = paste("Biodiversidata_", row_number(), sep = "")) %>%
 # write_csv("./original_datasets/X1_URUGUAY_Biodiversidata_Plants_withID.csv")

biodiversidata <- read_csv("./../1_original_data/original_datasets/X1_URUGUAY_Biodiversidata_Plants_withID.csv") %>%
  mutate(country = "Uruguay") %>%
  rename(Species = scientificName) %>%
  rename(latitude = decimalLatitude) %>%
  rename(longitude = decimalLongitude) %>%
  mutate(source = "Biodiversidata") %>%
  select(Species, id, latitude, longitude, country, source) %>%
  standardize.ds(.) %>%
  clean.occ(.); colnames(biodiversidata)

#------- sPlot
#sPlotOpen_header <- read.table("./../1_original_data/original_datasets/X2_sPlot_data/sPlotOpen_header.txt", header = TRUE, sep = "\t") %>%
#  select(PlotObservationID, Latitude, Longitude, Country); nrow(sPlotOpen_header)
#
#sPlotOpen_DT <- read.delim("./../1_original_data/original_datasets/X2_sPlot_data/sPlotOpen_DT.txt", na.strings = c("", "NA")) %>%
#  select(PlotObservationID, Species) %>%
#  drop_na(); nrow(sPlotOpen_DT)

#plyr::join(sPlotOpen_header, sPlotOpen_DT, by="PlotObservationID", type="inner") %>%
#  mutate(id = paste("sPlot_", row_number(), sep = "")) %>%
#  write_csv("./../1_original_data/original_datasets/X2_sPlot_withID.csv")

sPlot <- read_csv("./../1_original_data/original_datasets/X2_sPlot_withID.csv") %>%
  drop_na() %>%
  mutate(source = "sPlot") %>%
  rename(latitude = Latitude) %>%
  rename(longitude = Longitude) %>%
  rename(country = Country) %>%
  select(Species, id, latitude, longitude, country, source) %>%
  standardize.ds(.) %>%
  clean.occ(.); colnames(sPlot)

#------- PREDICTS
#read_csv("./../1_original_data/original_datasets/X3_PREDICTS.csv") %>%
#  mutate(id = paste("PREDICTS_", row_number(), sep = "")) %>%
#  write_csv("./../1_original_data/original_datasets/X3_PREDICTS_withID.csv")
  
PREDICTS <- read_csv("./../1_original_data/original_datasets/X3_PREDICTS_withID.csv") %>%
  filter(Kingdom == "Plantae") %>%
  filter(Phylum == "Tracheophyta") %>%
  rename(latitude = Latitude) %>%
  rename(longitude = Longitude) %>%
  rename(country = Country) %>%
  mutate(source = "PREDICTS") %>%
  select(Best_guess_binomial, id, latitude, longitude, country, source) %>%
  rename(Species = Best_guess_binomial) %>%
  drop_na() %>%
  standardize.ds(.) %>%
  clean.occ(.); colnames(PREDICTS)

#------- RAINBIO
#read_csv("./../1_original_data/original_datasets/X4_RAINBIO.csv") %>%
#  mutate(id = paste("RAINBIO_", row_number(), sep = "")) %>%
#  write_csv("./../1_original_data/original_datasets/X4_RAINBIO_withID.csv")

RAINBIO <- read_csv("./../1_original_data/original_datasets/X4_RAINBIO_withID.csv") %>%
  rename(latitude = decimalLatitude) %>%
  rename(longitude = decimalLongitude) %>%
  mutate(source = "RAINBIO") %>%
  select(tax_sp_level, id, latitude, longitude, country, source) %>%
  rename(Species = tax_sp_level) %>%
  drop_na() %>%
  standardize.ds(.) %>%
  clean.occ(.); colnames(RAINBIO)

#------- DRYFLOR
#read_csv("./../1_original_data/original_datasets/X5_dryflor_Science_rec_gr.csv") %>%
#  mutate(id = paste("DRYFLOR_", row_number(), sep = "")) %>%
#  write_csv(./../1_original_data/original_datasets/X5_dryflor_Science_rec_gr_withID.csv")

DRYFLOR <- read_csv("./../1_original_data/original_datasets/X5_dryflor_Science_rec_gr_withID.csv") %>%
  rename(latitude = Lat) %>%
  rename(longitude = Long) %>%
  mutate(source = "DRYFLOR") %>%
  rename(country = Cou) %>%
  select(sp_, id, latitude, longitude, country, source) %>%
  rename(Species = sp_) %>%
  drop_na() %>%
  standardize.ds(.) %>%
  clean.occ(.); colnames(DRYFLOR)

custom_match <- c(Bahamas = 'Bah', `Costa Rica` = 'Cos', `Paraguay` = 'Par', `Puerto Rico` = 'Pue', `Saint Lucia` = 'Sai', `Trinidad and Tobago` = 'Tri')
DRYFLOR$country <- countrycode(as.character(DRYFLOR$country), origin = "iso3c", destination = 'iso.name.en', custom_match = custom_match)
DRYFLOR <- DRYFLOR[!is.na(DRYFLOR$country),]

#------- IDIGBIO
#read_csv("./../1_original_data/iDigBio/occurrence.csv") %>%
#  mutate(id = paste("IDIGBIO_", row_number(), sep = "")) %>%
#  write_csv("./../1_original_data/original_datasets/X6_IDIGBIO_withID.csv")

IDIGBIO <- read_csv("./../1_original_data/original_datasets/X6_IDIGBIO_withID.csv") %>%
  separate(., 'idigbio:geoPoint', c("lat", "long"), sep = ",") %>%
  mutate(latitude = parse_number(.$lat)) %>%
  mutate(longitude = parse_number(.$long)) %>%
  rename(country = dwc:country) %>%
  rename(Species = dwc:scientificName) %>%
  mutate(source = "RAINBIO") %>%
  select(Species, id, latitude, longitude, country, source) %>%
  drop_na() %>%
  standardize.ds(.) %>%
  clean.occ(.); head(IDIGBIO)

#------- GBIF
GBIF <- read_csv("./../1_original_data/original_datasets/X7_GBIF.csv") %>%
  rename(id = gbifID) %>%
  rename(latitude = decimalLatitude) %>%
  rename(longitude = decimalLongitude) %>%
  rename(country = Cou) %>%
  mutate(source = "GBIF") %>%
  mutate(Species = sub("^(\\S*\\s+\\S+).*", "\\1", .$scientificName)) %>%
  select(Species, id, latitude, longitude, country, source) %>%
  drop_na() %>%
  standardize.ds(.) %>%
  clean.occ(.); colnames(GBIF)

GBIF$country <- countrycode(as.character(GBIF$country), origin = "iso3c", destination = 'iso.name.en', custom_match = custom_match)
GBIF <- GBIF[!is.na(GBIF$country),]

#------- rbind all non-GBIF datasets together
not_GBIF <- rbind(biodiversidata, sPlot, PREDICTS, RAINBIO, DRYFLOR, IDIGBIO) %>% clean.occ(.)
write_csv(not_GBIF, "./../tmp/all_not_GBIF_clean.csv")

#------- clean the GBIF dataset
# The GBIF dataset has to be split in order for the computer to clean it
GBIF_temp1 <- head(GBIF, 100000000) %>% clean.occ(.)
write_csv(GBIF_temp1, "./../tmp/X7_GBIF_temp1.csv"); gc()

GBIF_temp2 <- GBIF[100000001:200000000,] %>% clean.occ(.)
write_csv(GBIF_temp2, "./../tmp/X7_GBIF_temp2.csv"); gc()

GBIF_temp3 <- GBIF[200000001:243950820,] %>% clean.occ(.)

GBIF_clean <- rbind(GBIF3, GBIF4, GBIF5)
write_csv(GBIF_clean, "./tmp/X7_gbif_clean.csv")

#rm(*) # Free up the computer's memory

# Re-read in everything again
not_GBIF <- read_csv("./../tmp/all_not_GBIF_clean.csv")
GBIF <- read_csv("./../tmp/X7_gbif_clean.csv")

occs.cleaned <- rbind(not_GBIF, GBIF); rm(not_GBIF, GBIF)

# Now let's reduce the dataset to only one observation per species per pixel
#####
#Load and stack predictor variables 

envs <- raster::stack(list.files(path="./../1_original_data/envs/", pattern='*.tif$*', full.names=TRUE)) %>%
  raster::brick(.)

# Building the grid to select only one observation per grid cell
ext <- raster::extent(envs)
resolution.raster <- raster::res(envs)[1]
rst <- raster::raster(xmn = ext@xmin, xmx = ext@xmax, ymn = ext@ymin, ymx = ext@ymax,
                      res = resolution.raster)

### Function to thin occurrences to only one per pixel
parallel::mclapply(unique(occs.cleaned$Species), function(species, 
                                                          occs = occs.cleaned,
                                                          envs.rst = rst
                                                          ) {
  
  if (file.exists(paste("./../tmp/pixelThin_occs/", species, "_pixelThin_occs.csv", sep = "")) == FALSE) {
    
    sp.occ <- subset(occs, Species == species) %>%
      distinct(.keep_all = TRUE) %>%
      as.data.frame(.)
    
    envs.rst[] <- 1:raster::ncell(envs.rst)
    xy <- data.frame(biogeo::coord2numeric(as.vector(sp.occ[,"longitude"])),
                     biogeo::coord2numeric(as.vector(sp.occ[,"latitude"])))
    cid <- raster::cellFromXY(envs.rst, xy)
    dups <- (duplicated(cid)) * 1
    sp.occ$Exclude <- dups
    sp.occ <- sp.occ[which(sp.occ$Exclude==0), ]
    sp.occ <- sp.occ[, 1:length(sp.occ)-1]
    
    write_csv(sp.occ, paste("./../tmp/pixelThin_occs/", species, "_pixelThin_occs.csv", sep = "")) 
    
    gc()
    
  }
  
}, mc.cores = 36)


### Merge the files
occs_dt <- list.files(path = "./../tmp/pixelThin_occs/",
                      pattern = "*_pixelThin_occs.csv", full.names = TRUE) %>%
  map_df(~fread(.)) %>%
  data.table::as.data.table(.)

write_csv("./../1_original_data/herb_occs-105450sp.csv")




