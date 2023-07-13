library("tidyverse")
library("data.table")
library("dplyr")
library("ggpubr")
library("viridis")
library("plotly")
library("gridExtra")
library("data.table")

base::setwd("."); getwd()
dir_archive <- "/Volumes/1TB_SSD/"

hs_mess_trees <- list.files(path = paste(dir_archive, "res_MESS_perHemisphere_trees/", sep = ""), 
                                   pattern = "*_MESS_perHemisphere.csv", full.names = TRUE) %>%
  map_df(~fread(., nrows = 1)) %>%
  mutate(growthform = "tree")

hs_mess_herbs <- list.files(path = paste(dir_archive, "res_MESS_perHemisphere_herbs/", sep = ""), 
                                   pattern = "*_MESS_perHemisphere.csv", full.names = TRUE) %>%
  map_df(~fread(., nrows = 1)) %>%
  mutate(growthform = "herb")

hs_mess <- hs_mess_trees %>%
  rbind(hs_mess_herbs)

write_csv(hs_mess, "./3_generated_data/MESSvalues_perHemisphere.csv")
