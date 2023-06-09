library(tidyverse)
library(sjPlot)
library(sjmisc)
library(sjlabelled)

#setwd("/Users/marco/GitHub/environmental_breadth_final/4_plotting")

dt <- read_csv("./3_generated_data/niche_data_final_summarized_v4.csv") %>%
  mutate(e_breadth = (env_breadth*mess)^(1/4))

# Selecting only species occurring on only one hemisphere
pure_northern <- dt %>% filter(is.na(lat_median_s))
pure_southern <- dt %>% filter(is.na(lat_median_n))

as.data.frame(pure_southern) %>% summarise_all(~ sum(is.na(.)))

pures <- rbind(pure_northern, pure_southern); dt <- pures
write_csv(pures, "./3_generated_data/niche_data_final_HSsp.csv")

# Create a file with the zones of each species
#dt <- dt %>%
#  mutate(zone_n = ifelse(abs(lat_median_n) >= 0, "tropical", NA)) %>%
#  mutate(zone_n = ifelse(abs(lat_median_n) > 10, "else", zone_n))
#
#dt <- dt %>%
#  mutate(zone_s = ifelse(abs(lat_median_s) >= 0, "tropical", NA)) %>%
#  mutate(zone_s = ifelse(abs(lat_median_s) > 10, "else", zone_s))
#
#dt <- dt %>%
#  mutate(zone_g = ifelse(abs(lat_median_g) >= 0, "tropical", NA)) %>%
#  mutate(zone_g = ifelse(abs(lat_median_g) > 10, "else", zone_g))
#
#dt %>% select(Species, zone_n, zone_s, zone_g) %>% write_csv("./../3_generated_data/zones_v3.csv")

dt_n <- dt %>%
  filter(lat_median_n >= 0) %>%
  mutate(zone = ifelse(abs(lat_median_n) >= 0, "tropical", NA)) %>%
  mutate(zone = ifelse(abs(lat_median_n) > 10, "else", zone))

dt_s <- dt %>%
  filter(lat_median_s >= 0) %>%
  mutate(zone = ifelse(abs(lat_median_s) >= 0, "tropical", NA)) %>%
  mutate(zone = ifelse(abs(lat_median_s) > 10, "else", zone))

dt_g <- dt %>%
  mutate(zone = ifelse(abs(lat_median_g) >= 0, "tropical", NA)) %>%
  mutate(zone = ifelse(abs(lat_median_g) > 10, "else", zone))


combs <- expand.grid(zone = c("tropical", "else"),
                     hemi = c("North", "South"),
                     form = c("tree", "herb"))

combs2 <- expand.grid(hemi = c("North", "South"),
                     form = c("tree", "herb"))

# -----------------------------------------------------------------------------
# 1 Results for the latitudinal median vs. latitudinal range 
allres_lmlr <- tibble()

for(i in 1:nrow(combs)){

  if(combs$hemi[i] == "North"){
    
    # Northern hemisphere
    tmpdat <- dt_n %>% filter(zone == all_of(combs$zone[i]), 
                             growthform == all_of(combs$form[i]))
    lrlm <- lm(lat_range_sd_n ~ lat_median_n, na.action = na.omit, tmpdat)
  
    } else {
    
    # Southern hemisphere
    tmpdat <- dt_s %>% filter(zone == all_of(combs$zone[i]), 
                               growthform == all_of(combs$form[i]))
    lrlm <- lm(lat_range_sd_s ~ lat_median_s, na.action = na.omit, tmpdat)   
  }
  
  tmpres <- tibble(
    model = "lat median vs lat range", 
    zone = combs$zone[i], 
    hemisphere = combs$hemi[i],
    growthform = combs$form[i],
    val = paste0(round(coef(lrlm)[2], 3), " (", paste(round(confint(lrlm)[2,],3), collapse = ", "), ")"))
  
  allres_lmlr <- allres_lmlr %>% bind_rows(tmpres)
}

allres_lmlr %>% spread(zone, val) %>% arrange(model, growthform, hemisphere)

# -----------------------------------------------------------------------------
# 2 Results for the env.breadth vs. latitudinal range 
#allres_eblr <- tibble()  
#for(i in 1:nrow(combs2)){
#
#  if(combs2$hemi[i] == "North"){
#    
#    # Northern hemisphere
#    tmpdat <- dt_n %>% filter(growthform == all_of(combs2$form[i]))
#        eblr <- lm(lat_range_sd_n ~ e_breadth, na.action = na.omit, tmpdat)
#  
#        } else {
#    
#    # Southern hemisphere
#    tmpdat <- dt_s %>% filter(growthform == all_of(combs2$form[i]))
#        eblr <- lm(lat_range_sd_s ~ e_breadth, na.action = na.omit, tmpdat)   
#  }
#  
#  tmpres <- tibble(
#    model = "env.breadth vs lat range", 
#    hemisphere = combs2$hemi[i],
#    growthform = combs2$form[i],
#    val = paste0(round(coef(eblr)[2], 3), " (", paste(round(confint(eblr)[2,],3), collapse = ", "), ")"))
#  
#  allres_eblr <- allres_eblr %>% bind_rows(tmpres)
#}
#
#allres_eblr

allres_eblr <- tibble()  
for(i in 1:nrow(combs)){ #combs2
  
  if(combs$hemi[i] == "North"){
    
    # Northern hemisphere
    tmpdat <- dt_n %>% filter(zone == all_of(combs$zone[i])) %>%
      filter(growthform == all_of(combs$form[i]))
    eblr <- lm(lat_range_sd_n ~ e_breadth, na.action = na.omit, tmpdat)
    
  } else {
    
    # Southern hemisphere
    tmpdat <- dt_s %>% filter(zone == all_of(combs$zone[i])) %>%
      filter(growthform == all_of(combs$form[i]))
    eblr <- lm(lat_range_sd_s ~ e_breadth, na.action = na.omit, tmpdat)   
  }
  
  tmpres <- tibble(
    model = "env.breadth vs lat range", 
    zone = combs$zone[i],
    hemisphere = combs$hemi[i],
    growthform = combs$form[i],
    val = paste0(round(coef(eblr)[2], 3), " (", paste(round(confint(eblr)[2,],3), collapse = ", "), ")"))
  
  allres_eblr <- allres_eblr %>% bind_rows(tmpres)
}

allres_eblr %>% spread(zone, val) %>% arrange(model, growthform, hemisphere)

# -----------------------------------------------------------------------------
# 3 Results for the latitudinal median vs. environmental breadth
allres_lmeb <- tibble()
for(i in 1:nrow(combs)){

  if(combs$hemi[i] == "North"){
    
    # Northern hemisphere
    tmpdat <- dt_n %>% filter(zone == all_of(combs$zone[i]), 
                               growthform == all_of(combs$form[i]))
    eblm <- lm(e_breadth ~ lat_median_n, na.action = na.omit, tmpdat)
    
    } else {
    
    # Southern hemisphere
    tmpdat <- dt_s %>% filter(zone == all_of(combs$zone[i]), 
                               growthform == all_of(combs$form[i]))
    eblm <- lm(e_breadth ~ lat_median_s, na.action = na.omit, tmpdat)   
  }
  
  tmpres <- tibble(
    model = "env.breadth vs lat median", 
    zone = combs$zone[i], 
    hemisphere = combs$hemi[i],
    growthform = combs$form[i],
    val = paste0(round(coef(eblm)[2], 3), " (", paste(round(confint(eblm)[2,],3), collapse = ", "), ")"))
  
  allres_lmeb <- allres_lmeb %>% bind_rows(tmpres)
}

allres_lmeb %>% spread(zone, val) %>% arrange(model, growthform, hemisphere)


rm(list=setdiff(ls(), c("allres_lmlr", "allres_eblr", "allres_lmeb", "dt")))
