library("tidyverse")
library("data.table")
library("dplyr")
library("ggpubr")
library("viridis")
library("plotly")
library("gridExtra")
library("data.table")
library("viridis")

setwd("/Users/marco/GitHub/environmental_breadth_final/"); getwd()

source("./2_scripts/5-3_coefficients_MESS_perHemisphere.R")

allres_eblr <- as.data.frame(allres_eblr)
allres_lmeb <- as.data.frame(allres_lmeb)
allres_eblr <- as.data.frame(allres_eblr)

# Set global text size options
element_text_size <- 17
annotate_text_size <- 5

# ------ Import and filter data ----------------------------------------------------------------------------------------

niche_data <- read_csv("./3_generated_data/niche_data_final_summarized_v4.csv") %>%
  left_join(read_csv("./3_generated_data/MESSvalues_perHemisphere.csv") %>% rename(Species = species), by = c("Species", "growthform")) %>%
  mutate(e_breadth_n = (env_breadth*mess_n)^(1/4)) %>%
  mutate(e_breadth_s = (env_breadth*mess_s)^(1/4)) %>%
  left_join(read_csv("./3_generated_data/zones_v3.csv"), by = "Species"); ds <- niche_data

# Defining color palette
mycolors <- c("#00000000", colorRampPalette(c("#FFFFD6FF", "yellow", "orange", "red"), alpha=TRUE)(20))



# ------ Figure S6A: Relationship between environmental breadth and latitudinal range - Northern hemisphere, tree -----

S6A_data <- niche_data %>% filter(growthform == "tree") %>% filter(lat_median_n >= 0)
S6A_breaks <- quantile(S6A_data[["e_breadth_n"]], probs = seq(0, 1, by = 0.05), na.rm = TRUE)

S6A <- ggplot(data = S6A_data,
              aes(x = e_breadth_n, y = lat_range_sd_n)) +
  
  geom_density_2d_filled(bins = 20, show.legend = FALSE) +
  stat_summary_bin(breaks = S6A_breaks,
                   fun = mean, 
                   fun.min = function(x) mean(x)-sd(x)/sqrt(length(x)), 
                   fun.max = function(x) mean(x)+sd(x)/sqrt(length(x)),
                   color = "azure4") +
  geom_smooth(
    aes(linetype = .data[["zone_n"]]),
    color = "black",
    method = "lm",
    na.rm = TRUE,
    show.legend = FALSE) +
  
  ggtitle("Northern hemisphere, tree") +
  xlab("Environmental breadth") +
  ylab("Latitudinal range (SD)") +
  annotate("text", x = 0.15, y = 7, size = annotate_text_size, 
           label = paste("β (tropical) =\n",
                         allres_eblr %>%
                           filter(hemisphere == "North") %>%
                           filter(growthform == "tree") %>%
                           filter(zone == "tropical") %>%
                           .$valse)) +
  annotate("text", x = 0.55, y = 7, size = annotate_text_size, 
           label = paste("β (else) =\n",
                         allres_eblr %>%
                           filter(hemisphere == "North") %>%
                           filter(growthform == "tree") %>%
                           filter(zone == "else") %>%
                           .$valse)) +
  
  theme_classic() +
  theme(text = element_text(size = element_text_size)) +
  scale_fill_manual(values = mycolors) +
  coord_cartesian(ylim = c(0, 7.5), xlim = c(0, 1))

# ------ Figure S6B: Relationship between environmental breadth and latitudinal range - Northern hemisphere, herb -----

S6B_data <- niche_data %>% filter(growthform == "herb") %>% filter(lat_median_n >= 0)
S6B_breaks <- quantile(S6B_data[["e_breadth_n"]], probs = seq(0, 1, by = 0.05), na.rm = TRUE)

S6B <- ggplot(data = S6B_data,
              aes(x = e_breadth_n, y = lat_range_sd_n)) +
  
  geom_density_2d_filled(bins = 20, show.legend = FALSE) +
  stat_summary_bin(breaks = S6B_breaks,
                   fun = mean, 
                   fun.min = function(x) mean(x)-sd(x)/sqrt(length(x)), 
                   fun.max = function(x) mean(x)+sd(x)/sqrt(length(x)),
                   color = "azure4") +
  geom_smooth(
    aes(linetype = .data[["zone_n"]]),
    color = "black",
    method = "lm",
    na.rm = TRUE,
    show.legend = FALSE) +
  
  ggtitle("Northern hemisphere, non-tree") +
  xlab("Environmental breadth") +
  ylab("Latitudinal range (SD)") +
  annotate("text", x = 0.15, y = 7, size = annotate_text_size, 
           label = paste("β (tropical) =\n",
                         allres_eblr %>%
                           filter(hemisphere == "North") %>%
                           filter(growthform == "herb") %>%
                           filter(zone == "tropical") %>%
                           .$valse)) +
  annotate("text", x = 0.55, y = 7, size = annotate_text_size, 
           label = paste("β (else) =\n",
                         allres_eblr %>%
                           filter(hemisphere == "North") %>%
                           filter(growthform == "herb") %>%
                           filter(zone == "else") %>%
                           .$valse)) +
  
  theme_classic() +
  theme(text = element_text(size = element_text_size)) +
  scale_fill_manual(values = mycolors) +
  coord_cartesian(ylim = c(0, 7.5))

# ------ Figure S6C: Relationship between environmental breadth and latitudinal range - Southern hemisphere, tree -----

S6C_data <- niche_data %>% filter(growthform == "tree") %>% filter(lat_median_s >= 0)
S6C_breaks <- quantile(S6C_data[["e_breadth_s"]], probs = seq(0, 1, by = 0.05), na.rm = TRUE)

S6C <- ggplot(data = S6C_data,
              aes(x = e_breadth_s, y = lat_range_sd_s)) +
  
  geom_density_2d_filled(bins = 20, show.legend = FALSE) +
  stat_summary_bin(breaks = S6C_breaks,
                   fun = mean, 
                   fun.min = function(x) mean(x)-sd(x)/sqrt(length(x)), 
                   fun.max = function(x) mean(x)+sd(x)/sqrt(length(x)),
                   color = "azure4") +
  geom_smooth(
    aes(linetype = .data[["zone_s"]]),
    color = "black",
    method = "lm",
    na.rm = TRUE,
    show.legend = FALSE) +
  
  ggtitle("Southern hemisphere, tree") +
  xlab("Environmental breadth") +
  ylab("Latitudinal range (SD)") +
  annotate("text", x = 0.15, y = 7, size = annotate_text_size, 
           label = paste("β (tropical) =\n",
                         allres_eblr %>%
                           filter(hemisphere == "South") %>%
                           filter(growthform == "tree") %>%
                           filter(zone == "tropical") %>%
                           .$valse)) +
  annotate("text", x = 0.55, y = 7, size = annotate_text_size, 
           label = paste("β (else) =\n",
                         allres_eblr %>%
                           filter(hemisphere == "South") %>%
                           filter(growthform == "tree") %>%
                           filter(zone == "else") %>%
                           .$valse)) +
  
  theme_classic() +
  theme(text = element_text(size = element_text_size)) +
  scale_fill_manual(values = mycolors) +
  coord_cartesian(ylim = c(0, 7.5))

# ------ Figure S6D: Relationship between environmental breadth and latitudinal range - Southern hemisphere, herb -----

S6D_data <- niche_data %>% filter(growthform == "herb") %>% filter(lat_median_s >= 0)
S6D_breaks <- quantile(S6D_data[["e_breadth_s"]], probs = seq(0, 1, by = 0.05), na.rm = TRUE)

S6D <- ggplot(data = S6D_data,
              aes(x = e_breadth_s, y = lat_range_sd_s)) +
  
  geom_density_2d_filled(bins = 20, show.legend = FALSE) +
  stat_summary_bin(breaks = S6D_breaks,
                   fun = mean, 
                   fun.min = function(x) mean(x)-sd(x)/sqrt(length(x)), 
                   fun.max = function(x) mean(x)+sd(x)/sqrt(length(x)),
                   color = "azure4") +
  geom_smooth(
    aes(linetype = .data[["zone_s"]]),
    color = "black",
    method = "lm",
    na.rm = TRUE,
    show.legend = FALSE) +
  
  ggtitle("Southern hemisphere, non-tree") +
  xlab("Environmental breadth") +
  ylab("Latitudinal range (SD)") +
  annotate("text", x = 0.15, y = 7, size = annotate_text_size, 
           label = paste("β (tropical) =\n",
                         allres_eblr %>%
                           filter(hemisphere == "South") %>%
                           filter(growthform == "herb") %>%
                           filter(zone == "tropical") %>%
                           .$valse)) +
  annotate("text", x = 0.55, y = 7, size = annotate_text_size, 
           label = paste("β (else) =\n",
                         allres_eblr %>%
                           filter(hemisphere == "South") %>%
                           filter(growthform == "herb") %>%
                           filter(zone == "else") %>%
                           .$valse)) +
  
  theme_classic() +
  theme(text = element_text(size = element_text_size)) +
  scale_fill_manual(values = mycolors) +
  coord_cartesian(ylim = c(0, 7.5))

# ------ Joining and saving the plots -------------------------------------------------------------

(S6 <- cowplot::plot_grid(S6A,
                         S6B,
                         S6C,
                         S6D,
                         rel_widths = c(1.3, 1, 1,
                                        1.3, 1, 1),
                         ncol = 2, byrow = FALSE,
                         labels = c("A", "C",
                                    "B", "D"),
                         label_size = 20)
)
#ggsave("./tmp/final/figure_S6.jpg",
 #      width = 4000, height = 3000, units = "px")
