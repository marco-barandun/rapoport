### Analysis of the tree species for publication

library("tidyverse")
library("data.table")
library("dplyr")
library("ggpubr")
library("viridis")
library("plotly")
library("gridExtra")
library("data.table")

# --------------------- Notes
# To find a good transformation plot the hist of the envs and look hist((var)^1/2) --> sqrt seems the best
# ---------------------

base::setwd("."); getwd()
dir_archive <- "/Users/marco/Documents/env_breadth_archive/"

##### Reading in the first line of all the species *_maxent_results.csv file (the line with the niche breadth estimate)
##### using the data.table package with fread as performance is much much faster than other solutions
##### https://statisticsglobe.com/merge-csv-files-in-r
raw_niche_data_trees <- list.files(path = paste(dir_archive, "res_maxent_result_tables_trees/", sep = ""), 
                                   pattern = "*_maxent_results.csv", full.names = TRUE) %>%
  map_df(~fread(., nrows = 1)) %>%
  mutate(growthform = "tree")

raw_niche_data_herbs <- list.files(path = paste(dir_archive, "res_maxent_result_tables_herbs/", sep = ""), 
                                   pattern = "*_maxent_results.csv", full.names = TRUE) %>%
  map_df(~fread(., nrows = 1)) %>%
  mutate(growthform = "herb")

# -------------------------------------------------------------------------------------------------------
##### Reading in the MESS weight for the niche breadth and add it to the raw niche data
mess_trees <- list.files(path = paste(dir_archive, "res_representativeness_trees/", sep = ""), 
                         pattern = "*_representativeness.csv", full.names = TRUE) %>%
  map_df(~fread(., nrows = 1)) %>%
  rename(Species = species) %>%
  rename(mess = value) %>%
  data.table::as.data.table(.)

raw_niche_data_trees_mess <- raw_niche_data_trees %>%
  data.table::as.data.table(.) %>%
  .[mess_trees, nomatch=0, on = "Species"]

mess_herbs <- list.files(path = paste(dir_archive, "res_representativeness_herbs/", sep = ""), 
                         pattern = "*_representativeness.csv", full.names = TRUE) %>%
  map_df(~fread(., nrows = 1)) %>%
  rename(Species = species) %>%
  rename(mess = value)

raw_niche_data_herbs_mess <- raw_niche_data_herbs %>%
  data.table::as.data.table(.) %>%
  .[mess_herbs, nomatch=0, on = "Species"]

# -------------------------------------------------------------------------------------------------------
##### Generate the colums for the additional variables which will be computed below
raw_niche_data_mess <- rbind(raw_niche_data_trees_mess, raw_niche_data_herbs_mess)

#write_csv(raw_niche_data_mess, "./../3_generated_data/raw_niche_data_mess.csv")
rm(list=setdiff(ls(), c("dir_archive", "raw_niche_data_mess")))
raw_niche_data_mess <- read_csv("./../3_generated_data/raw_niche_data_mess.csv")

# -------------------------------------------------------------------------------------------------------
##### Reading in the species occurrences
occurrences_trees <- read_csv(paste("./../1_original_data/tree_occurrences_Andrea_clean_withElevation.csv", sep = "")) %>%
  filter(Species %in% raw_niche_data_mess$Species) %>%
  mutate(hemisphere = sign(latitude)) %>%
  filter(hemisphere != 0) %>%
  dplyr::select(-...1) %>%
  mutate(growthform = "tree")

occurrences_herbs <- read_csv(paste("./../1_original_data/herb_occs_elevation-105450sp.csv", sep = "")) %>%
  filter(Species %in% raw_niche_data_mess$Species) %>%
  mutate(hemisphere = sign(latitude)) %>%
  filter(hemisphere != 0) %>%
  mutate(growthform = "herb") %>%
  rename(Source = source) %>%
  select(occID, Species, longitude, latitude, Source, elevation, hemisphere, growthform)

occurrences <- rbind(occurrences_trees, occurrences_herbs)

# -------------------------------------------------------------------------------------------------------
# Calculating stats globally and per hemisphere
latitudinal_stats_g <- occurrences %>% 
  group_by(Species) %>% 
  summarize(lat_range_sd_g = abs(round(sd(latitude, na.rm = TRUE), digits = 3)), 
            lat_median_g = round(median(latitude, na.rm = TRUE), digits = 3))

latitudinal_stats_n <- occurrences %>% 
  filter(hemisphere == 1) %>%
  group_by(Species) %>% 
  summarize(lat_range_sd_n = abs(round(sd(latitude, na.rm = TRUE), digits = 3)), 
            lat_median_n = abs(round(median(latitude, na.rm = TRUE), digits = 3)))

latitudinal_stats_s <- occurrences %>% 
  filter(hemisphere == -1) %>%
  group_by(Species) %>% 
  summarize(lat_range_sd_s = abs(round(sd(latitude, na.rm = TRUE), digits = 3)), 
            lat_median_s = abs(round(median(latitude, na.rm = TRUE), digits = 3)))

# Joining and reordering columns as in paper occurrence
latitudinal_stats <- latitudinal_stats_g %>%
  left_join(., latitudinal_stats_n, by = "Species") %>%
  left_join(., latitudinal_stats_s, by = "Species") %>%
  select("Species", "lat_range_sd_n", "lat_median_n", "lat_range_sd_s", "lat_median_s", "lat_range_sd_g", "lat_median_g")

# Joining stats to dataset
niche_data <- raw_niche_data_mess %>%
  inner_join(., latitudinal_stats, by = "Species")

# -------------------------------------------------------------------------------------------------------
# Check if the number of NAs is reasonable. Most columns should not have any NAs, elevation might have some
as.data.frame(niche_data) %>% summarise_all(~ sum(is.na(.)))

### Filter
niche_data <- niche_data %>%
  filter(auc.val.avg > 0.55) %>%
  filter(0.05 < or.10p.avg) %>%
  filter(or.10p.avg < 0.3)

# Analyzing error
old_niche_data <- read_csv("./../3_generated_data/niche_data_final_v3.csv")
hist(old_niche_data$lat_median_g-niche_data$lat_median_g)

# Export
write_csv(niche_data, "./../3_generated_data/niche_data_final_summarized_v4.csv")
rm(list=setdiff(ls(), c("dir_archive", "niche_data")))

