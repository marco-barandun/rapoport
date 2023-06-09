### Analysis of the tree species for publication

library("tidyverse")
library("data.table")
library("dplyr")
library("ggpubr")
library("viridis")
library("plotly")
library("gridExtra")
library("data.table")
library("viridis")

base::setwd("."); getwd()
dir_archive <- "/Users/marco/Documents/env_breadth_archive/"

source("./2_scripts/5_coefficients_HS.R")

allres_eblr <- as.data.frame(allres_eblr)
allres_lmeb <- as.data.frame(allres_lmeb)
allres_lmlr <- as.data.frame(allres_lmlr)

# Before running this script, run 5_coefficients.R

# -------------------------------------------------------------------------------------------------------
##### Import and filter data

#hs_sp <- read_csv("./../3_generated_data/hs_sp_final_v2.csv") %>%
#  mutate(e_breadth = (env_breadth*mess)^(1/4)); range(hs_sp$e_breadth)
#  rename(l_median = lat_median) %>%   # Define which column to use as median latitude
#  mutate(l_range = (lat_n95q+90)-(lat_s05q+90)) %>%     # Define which column to use as latitudinal range #(lat_n95q+90)-(lat_s05q+90) lat_range_mad
#  mutate(e_breadth = env_breadth) %>%     # Define which column to use as environmental breadth
#  mutate(al_range = l_range/max(l_range)) %>%
#  mutate(al_median = abs(l_median)/max(abs(l_median))) %>%
#  mutate(ae_breadth = e_breadth/max(e_breadth)) %>%
#  mutate(bin = ntile(l_median, n=40))

#zones <- read_csv("./../3_generated_data/zones_v2.csv")

#hs_sp <- left_join(hs_sp, zones, by = "Species"); ds <- hs_sp

#hs_sp <- obs

hs_sp <- read_csv("./../3_generated_data/niche_data_final_HSsp.csv") %>%
  mutate(e_breadth = (env_breadth*mess)^(1/4)); range(hs_sp$e_breadth)
zones <- read_csv("./../3_generated_data/zones_v3.csv")
hs_sp <- left_join(hs_sp, zones, by = "Species"); ds <- hs_sp

# -------------------------------------------------------------------------------------------------------
##### Defining function for single normal plot

my_plotting <- function(df, 
                        xc, 
                        yc,
                        gfc,
                        binc,
                        gf_sel,
                        ttitle,
                        xxlab,
                        yylab,
                        plot_type,
                        binning,
                        y_lim_s,
                        y_lim_n,
                        x_lim,
                        zonec,
                        label_trop,
                        label_notrop,
                        label_trop_x,
                        label_trop_y,
                        label_notrop_x,
                        label_notrop_y) {
  
  df <- df %>% filter(.data[[gfc]] == gf_sel)
  mycolors <- c("#00000000", colorRampPalette(c("#FFFFD6FF", "yellow", "orange", "red"), alpha=TRUE)(20))
  
  p <- ggplot(data = df, 
              aes(y = .data[[yc]])) +
    ggtitle(paste(ttitle, gf_sel)) +
    xlab(xxlab) +
    ylab(yylab) +
    theme(text = element_text(size = 25)) +
    theme_classic() +
    scale_fill_manual(values = mycolors) +
    geom_density_2d_filled(data = df, 
                           aes(x = .data[[xc]]),
                           bins = 20, show.legend = FALSE) +
    stat_summary_bin(data = df, 
                    aes(x = df[[xc]]),
                    breaks = quantile(df[[xc]], probs = seq(0, 1, by = 0.05), na.rm = TRUE),
                    fun = mean, 
                    fun.min = function(x) mean(x)-sd(x)/sqrt(length(x)), 
                    fun.max = function(x) mean(x)+sd(x)/sqrt(length(x)),
                    color = "azure4") +
    geom_smooth(
      aes(x = df[[xc]], linetype = df[[zonec]]),
      color = "black",
      method = "lm",
      na.rm = TRUE,
      show.legend = FALSE) +
    coord_cartesian(ylim=c(y_lim_s, y_lim_n), xlim = c(0, x_lim)) +
    annotate("text", x= label_trop_x, y= label_trop_y, label= label_trop, size=2.5) +
    annotate("text", x= label_notrop_x, y= label_notrop_y, label= label_notrop, size=2.5)
  
  return(p)
}

# -------------------------------------------------------------------------------------------------------
##### Creating plot grid

# Figure 1: Rapoport's rule

allres_lmlr_2 <- allres_lmlr %>% mutate(hemisphere = ifelse(hemisphere == "North", 1, -1))
      
rapoport <- function(ds,
                       #growthform = "tree",
                       plot.type = "p_mean",
                       bin.ning = "eq_points") {

  ### Trees
  
  lm_lr_t_n <- my_plotting(ds %>% filter(!is.na(lat_median_n)),
                           xc = "lat_median_n", 
                           yc = "lat_range_sd_n",
                           gfc = "growthform",
                           binc = "bin",
                           gf_sel = "tree",
                           ttitle = "Northern hemisphere",
                           xxlab = 'Latitudinal median',
                           yylab = 'Latitudinal range',
                           plot_type = plot.type,
                           binning = bin.ning,
                           y_lim_s = 0,
                           y_lim_n = 7.5,
                           x_lim = 70,
                           zonec = "zone_n",
                           label_trop = paste("Slope:\n",
                                              allres_lmlr_2 %>%
                                                filter(hemisphere == 1) %>%
                                                filter(growthform == "tree") %>%
                                                filter(zone == "tropical") %>%
                                                .$val),
                           label_notrop = paste("Slope:\n",
                                                allres_lmlr_2 %>%
                                                  filter(hemisphere == 1) %>%
                                                  filter(growthform == "tree") %>%
                                                  filter(zone == "else") %>%
                                                  .$val),
                           label_trop_x = 8,
                           label_trop_y = 7,
                           label_notrop_x = 35,
                           label_notrop_y = 7)

  lm_lr_t_s <- my_plotting(ds %>% filter(!is.na(lat_median_s)),
                           xc = "lat_median_s", 
                           yc = "lat_range_sd_s",
                           gfc = "growthform",
                           binc = "bin",
                           gf_sel = "tree",
                           ttitle = "Southern hemisphere",
                           xxlab = 'Latitudinal median',
                           yylab = 'Latitudinal range',
                           plot_type = plot.type,
                           binning = bin.ning,
                           y_lim_s = 0,
                           y_lim_n = 7.5,
                           x_lim = 70,
                           zonec = "zone_s",
                           label_trop = paste("Slope:\n",
                                              allres_lmlr_2 %>%
                                                filter(hemisphere == -1) %>%
                                                filter(growthform == "tree") %>%
                                                filter(zone == "tropical") %>%
                                                .$val),
                           label_notrop = paste("Slope:\n",
                                                allres_lmlr_2 %>%
                                                  filter(hemisphere == -1) %>%
                                                  filter(growthform == "tree") %>%
                                                  filter(zone == "else") %>%
                                                  .$val),
                           label_trop_x = 8,
                           label_trop_y = 7,
                           label_notrop_x = 35,
                           label_notrop_y = 7)
  
  
  ### Herbs
  
  lm_lr_h_n <- my_plotting(ds %>% filter(!is.na(lat_median_n)),
                           xc = "lat_median_n", 
                           yc = "lat_range_sd_n",
                           gfc = "growthform",
                           binc = "bin",
                           gf_sel = "herb",
                           ttitle = "Northern hemisphere",
                           xxlab = 'Latitudinal median',
                           yylab = 'Latitudinal range',
                           plot_type = plot.type,
                           binning = bin.ning,
                           y_lim_s = 0,
                           y_lim_n = 7.5,
                           x_lim = 70,
                           zonec = "zone_n",
                           label_trop = paste("Slope:\n",
                                              allres_lmlr_2 %>%
                                                filter(hemisphere == 1) %>%
                                                filter(growthform == "herb") %>%
                                                filter(zone == "tropical") %>%
                                                .$val),
                           label_notrop = paste("Slope:\n",
                                                allres_lmlr_2 %>%
                                                  filter(hemisphere == 1) %>%
                                                  filter(growthform == "herb") %>%
                                                  filter(zone == "else") %>%
                                                  .$val),
                           label_trop_x = 8,
                           label_trop_y = 7,
                           label_notrop_x = 35,
                           label_notrop_y = 7)
  
  lm_lr_h_s <- my_plotting(ds %>% filter(!is.na(lat_median_s)),
                           xc = "lat_median_s", 
                           yc = "lat_range_sd_s",
                           gfc = "growthform",
                           binc = "bin",
                           gf_sel = "herb",
                           ttitle = "Southern hemisphere",
                           xxlab = 'Latitudinal median',
                           yylab = 'Latitudinal range',
                           plot_type = plot.type,
                           binning = bin.ning,
                           y_lim_s = 0,
                           y_lim_n = 7.5,
                           x_lim = 70,
                           zonec = "zone_s",
                           label_trop = paste("Slope:\n",
                                              allres_lmlr_2 %>%
                                                filter(hemisphere == -1) %>%
                                                filter(growthform == "herb") %>%
                                                filter(zone == "tropical") %>%
                                                .$val),
                           label_notrop = paste("Slope:\n",
                                                allres_lmlr_2 %>%
                                                  filter(hemisphere == -1) %>%
                                                  filter(growthform == "herb") %>%
                                                  filter(zone == "else") %>%
                                                  .$val),
                           label_trop_x = 8,
                           label_trop_y = 7,
                           label_notrop_x = 35,
                           label_notrop_y = 7)
  
  
  p <- cowplot::plot_grid(lm_lr_t_n,
                          lm_lr_t_s,
                          lm_lr_h_n,
                          lm_lr_h_s, 
                          rel_widths = c(1, 1,
                                         1, 1),
                          ncol = 2, align = "h",
                          labels = c("A", "B",
                                     "C", "D"),
                          label_size = 25)
  
  return(p)
}

figure1 <- rapoport(ds = hs_sp,
                    #growthform = "herb",
                    plot.type = "p_mean",
                    bin.ning = "eq_points")

ggsave("./../tmp/figure1_v3_HSsp.jpg",
       width = 3000, height = 2000, units = "px")

# -------------------------------------------------------------------------------------------------------
# Figure 2: Environmental breadth and latitudinal range

allres_eblr_2 <- allres_eblr %>% mutate(hemisphere = ifelse(hemisphere == "North", 1, -1))

range <- function(ds,
                     #growthform = "tree",
                     plot.type = "p_mean",
                     bin.ning = "eq_points") {
  
  ### Trees
  
  eb_lr_t_n <- my_plotting(ds %>% filter(!is.na(lat_median_n)),
                           xc = "e_breadth", 
                           yc = "lat_range_sd_n",
                           gfc = "growthform",
                           binc = "bin",
                           gf_sel = "tree",
                           ttitle = "Northern hemisphere",
                           xxlab = 'Environmental breadth',
                           yylab = 'Latitudinal range',
                           plot_type = plot.type,
                           binning = bin.ning, 
                           y_lim_s = 0,
                           y_lim_n = 10,
                           x_lim = 1,
                           zonec = "zone_n",
                           label_trop = paste("Slope tropical:\n",
                                              allres_eblr_2 %>%
                                                filter(hemisphere == 1) %>%
                                                filter(zone == "tropical") %>%
                                                filter(growthform == "tree") %>%
                                                .$val),
                           label_notrop = paste("Slope else:\n",
                                                allres_eblr_2 %>%
                                                  filter(hemisphere == 1) %>%
                                                  filter(zone == "else") %>%
                                                  filter(growthform == "tree") %>%
                                                  .$val),
                           label_trop_x = 0.10,
                           label_trop_y = 9,
                           label_notrop_x = 0.5,
                           label_notrop_y = 9)
  
  eb_lr_t_s <- my_plotting(ds %>% filter(!is.na(lat_median_s)),
                           xc = "e_breadth", 
                           yc = "lat_range_sd_s",
                           gfc = "growthform",
                           binc = "bin",
                           gf_sel = "tree",
                           ttitle = "Southern hemisphere",
                           xxlab = 'Environmental breadth',
                           yylab = 'Latitudinal range',
                           plot_type = plot.type,
                           binning = bin.ning,
                           y_lim_s = 0,
                           y_lim_n = 10,
                           x_lim = 1,
                           zonec = "zone_s",
                           label_trop = paste("Slope tropical:\n",
                                              allres_eblr_2 %>%
                                                filter(hemisphere == -1) %>%
                                                filter(growthform == "tree") %>%
                                                filter(zone == "tropical") %>%
                                                .$val),
                           label_notrop = paste("Slope else:\n",
                                                allres_eblr_2 %>%
                                                  filter(hemisphere == -1) %>%
                                                  filter(growthform == "tree") %>%
                                                  filter(zone == "else") %>%
                                                  .$val),
                           label_trop_x = 0.10,
                           label_trop_y = 9,
                           label_notrop_x = 0.5,
                           label_notrop_y = 9)
  
  ### Herbs
  
  eb_lr_h_n <- my_plotting(ds %>% filter(!is.na(lat_median_n)),
                           xc = "e_breadth", 
                           yc = "lat_range_sd_n",
                           gfc = "growthform",
                           binc = "bin",
                           gf_sel = "herb",
                           ttitle = "Northern hemisphere",
                           xxlab = 'Environmental breadth',
                           yylab = 'Latitudinal range',
                           plot_type = plot.type,
                           binning = bin.ning,
                           y_lim_s = 0,
                           y_lim_n = 10,
                           x_lim = 1,
                           zonec = "zone_n",
                           label_trop = paste("Slope tropical:\n",
                                              allres_eblr_2 %>%
                                                filter(hemisphere == 1) %>%
                                                filter(growthform == "herb") %>%
                                                filter(zone == "tropical") %>%
                                                .$val),
                           label_notrop = paste("Slope else:\n",
                                                allres_eblr_2 %>%
                                                  filter(hemisphere == 1) %>%
                                                  filter(growthform == "herb") %>%
                                                  filter(zone == "else") %>%
                                                  .$val),
                           label_trop_x = 0.10,
                           label_trop_y = 9,
                           label_notrop_x = 0.5,
                           label_notrop_y = 9)
  
  eb_lr_h_s <- my_plotting(ds %>% filter(!is.na(lat_median_s)),
                           xc = "e_breadth", 
                           yc = "lat_range_sd_s",
                           gfc = "growthform",
                           binc = "bin",
                           gf_sel = "herb",
                           ttitle = "Southern hemisphere",
                           xxlab = 'Environmental breadth',
                           yylab = 'Latitudinal range',
                           plot_type = plot.type,
                           binning = bin.ning,
                           y_lim_s = 0,
                           y_lim_n = 10,
                           x_lim = 1,
                           zonec = "zone_s",
                           label_trop = paste("Slope tropical:\n",
                                              allres_eblr_2 %>%
                                                filter(hemisphere == -1) %>%
                                                filter(growthform == "herb") %>%
                                                filter(zone == "tropical") %>%
                                                .$val),
                           label_notrop = paste("Slope else:\n",
                                                allres_eblr_2 %>%
                                                  filter(hemisphere == -1) %>%
                                                  filter(growthform == "herb") %>%
                                                  filter(zone == "else") %>%
                                                  .$val),
                           label_trop_x = 0.10,
                           label_trop_y = 9,
                           label_notrop_x = 0.5,
                           label_notrop_y = 9)
  
  
  p <- cowplot::plot_grid(eb_lr_t_n,
                          eb_lr_t_s,
                          eb_lr_h_n,
                          eb_lr_h_s, 
                          rel_widths = c(1, 1,
                                         1, 1),
                          ncol = 2, align = "h",
                          labels = c("A", "B", "C",
                                     "D", "E", "F"),
                          label_size = 25)
  
  return(p)
}

# tropical = dashed
figure2 <- range(ds = hs_sp,
                    #growthform = "herb",
                    plot.type = "p_mean",
                    bin.ning = "eq_points")

#ggsave("./../tmp/figure2_v3_HS.jpg",
#       width = 3000, height = 2000, units = "px")

# -------------------------------------------------------------------------------------------------------
# Figure 3: Environmental breadth and median latitude

allres_lmeb_2 <- allres_lmeb %>% mutate(hemisphere = ifelse(hemisphere == "North", 1, -1))

lmebreadth <- function(ds,
                  #growthform = "tree",
                  plot.type = "p_mean",
                  bin.ning = "eq_points") {
  
  ### Trees
  
  lm_eb_t_n <- my_plotting(ds %>% filter(!is.na(lat_median_n)),
                           xc = "lat_median_n", 
                           yc = "e_breadth",
                           gfc = "growthform",
                           binc = "bin",
                           gf_sel = "tree",
                           ttitle = "Northern hemisphere",
                           xxlab = 'Latitudinal median',
                           yylab = 'Environmental breadth',
                           plot_type = plot.type,
                           binning = bin.ning, 
                           y_lim_s = 0,
                           y_lim_n = 1,
                           x_lim = 70,
                           zonec = "zone_n",
                           label_trop = paste("Slope tropical:\n",
                                              allres_lmeb_2 %>%
                                                filter(hemisphere == 1) %>%
                                                filter(growthform == "tree") %>%
                                                filter(zone == "tropical") %>%
                                                .$val),
                           label_notrop = paste("Slope else:\n",
                                                allres_lmeb_2 %>%
                                                  filter(hemisphere == 1) %>%
                                                  filter(growthform == "tree") %>%
                                                  filter(zone == "else") %>%
                                                  .$val),
                           label_trop_x = 7,
                           label_trop_y = 0.9,
                           label_notrop_x = 35,
                           label_notrop_y = 0.9)
  
  lm_eb_t_s <- my_plotting(ds %>% filter(!is.na(lat_median_s)),
                           xc = "lat_median_s", 
                           yc = "e_breadth",
                           gfc = "growthform",
                           binc = "bin",
                           gf_sel = "tree",
                           ttitle = "Southern hemisphere",
                           xxlab = 'Latitudinal median',
                           yylab = 'Environmental breadth',
                           plot_type = plot.type,
                           binning = bin.ning,
                           y_lim_s = 0,
                           y_lim_n = 1,
                           x_lim = 70,
                           zonec = "zone_s",
                           label_trop = paste("Slope tropical:\n",
                                              allres_lmeb_2 %>%
                                                filter(hemisphere == -1) %>%
                                                filter(growthform == "tree") %>%
                                                filter(zone == "tropical") %>%
                                                .$val),
                           label_notrop = paste("Slope else:\n",
                                                allres_lmeb_2 %>%
                                                  filter(hemisphere == -1) %>%
                                                  filter(growthform == "tree") %>%
                                                  filter(zone == "else") %>%
                                                  .$val),
                           label_trop_x = 7,
                           label_trop_y = 0.9,
                           label_notrop_x = 35,
                           label_notrop_y = 0.9)

  
  ### Herbs
  
  lm_eb_h_n <- my_plotting(ds %>% filter(!is.na(lat_median_n)),
                           xc = "lat_median_n", 
                           yc = "e_breadth",
                           gfc = "growthform",
                           binc = "bin",
                           gf_sel = "herb",
                           ttitle = "Northern hemisphere",
                           xxlab = 'Latitudinal median',
                           yylab = 'Environmental breadth',
                           plot_type = plot.type,
                           binning = bin.ning,
                           y_lim_s = 0,
                           y_lim_n = 1,
                           x_lim = 70,
                           zonec = "zone_n",
                           label_trop = paste("Slope tropical:\n",
                                              allres_lmeb_2 %>%
                                                filter(hemisphere == 1) %>%
                                                filter(growthform == "herb") %>%
                                                filter(zone == "tropical") %>%
                                                .$val),
                           label_notrop = paste("Slope else:\n",
                                                allres_lmeb_2 %>%
                                                  filter(hemisphere == 1) %>%
                                                  filter(growthform == "herb") %>%
                                                  filter(zone == "else") %>%
                                                  .$val),
                           label_trop_x = 7,
                           label_trop_y = 0.9,
                           label_notrop_x = 35,
                           label_notrop_y = 0.9)
  
  lm_eb_h_s <- my_plotting(ds %>% filter(!is.na(lat_median_s)),
                           xc = "lat_median_s", 
                           yc = "e_breadth",
                           gfc = "growthform",
                           binc = "bin",
                           gf_sel = "herb",
                           ttitle = "Southern hemisphere",
                           xxlab = 'Latitudinal median',
                           yylab = 'Environmental breadth',
                           plot_type = plot.type,
                           binning = bin.ning,
                           y_lim_s = 0,
                           y_lim_n = 1,
                           x_lim = 70,
                           zonec = "zone_s",
                           label_trop = paste("Slope tropical:\n",
                                              allres_lmeb_2 %>%
                                                filter(hemisphere == -1) %>%
                                                filter(growthform == "herb") %>%
                                                filter(zone == "tropical") %>%
                                                .$val),
                           label_notrop = paste("Slope else:\n",
                                                allres_lmeb_2 %>%
                                                  filter(hemisphere == -1) %>%
                                                  filter(growthform == "herb") %>%
                                                  filter(zone == "else") %>%
                                                  .$val),
                           label_trop_x = 7,
                           label_trop_y = 0.9,
                           label_notrop_x = 35,
                           label_notrop_y = 0.9)
  
  
  p <- cowplot::plot_grid(lm_eb_t_n,
                          lm_eb_t_s,
                          lm_eb_h_n,
                          lm_eb_h_s, 
                          rel_widths = c(1, 1,
                                         1, 1),
                          ncol = 2, align = "h",
                          labels = c("A", "B",
                                     "F", "D"),
                          label_size = 25)
  
  return(p)
}

# tropical = dashed
figure3 <- lmebreadth(ds = hs_sp,
                 #growthform = "herb",
                 plot.type = "p_mean",
                 bin.ning = "eq_points")

#ggsave("./../tmp/figure3_v3_HS.jpg",
#       width = 3000, height = 2000, units = "px")





