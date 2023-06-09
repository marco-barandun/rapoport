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

# Reading in the Hemisphere Specific coefficients
source("./2_scripts/5-2_coefficients_HS.R")

allres_lmeb <- as.data.frame(allres_lmeb)
allres_lmeb <- as.data.frame(allres_lmeb)
allres_lmeb <- as.data.frame(allres_lmeb)

# Set global text size options
element_text_size <- 17
annotate_text_size <- 5

# ------ Import and filter data --------------------------------------------------------------------------

niche_data <- read_csv("./3_generated_data/niche_data_final_summarized_v4.csv") %>%
  mutate(e_breadth = (env_breadth*mess)^(1/4)) %>%
  left_join(read_csv("./3_generated_data/zones_v3.csv"), by = "Species")

# Defining color palette
mycolors <- c("#00000000", colorRampPalette(c("#FFFFD6FF", "yellow", "orange", "red"), alpha=TRUE)(20))


# ------ Figure S4A: Relationship between environmental breadth and latitudinal range - Northern hemisphere, tree -------------

S4A_data <- niche_data %>% filter(growthform == "tree") %>% filter(is.na(zone_s))
S4A_breaks <- quantile(S4A_data[["e_breadth"]], probs = seq(0, 1, by = 0.05), na.rm = TRUE)

S4A <- ggplot(data = S4A_data,
              aes(x = e_breadth, y = lat_range_sd_n)) +
  
  geom_density_2d_filled(bins = 20, show.legend = FALSE) +
  stat_summary_bin(breaks = S4A_breaks,
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

# ------ Figure S4B: Relationship between environmental breadth and latitudinal range - Northern hemisphere, herb -------------

S4B_data <- niche_data %>% filter(growthform == "herb") %>% filter(is.na(zone_s))
S4B_breaks <- quantile(S4B_data[["e_breadth"]], probs = seq(0, 1, by = 0.05), na.rm = TRUE)

S4B <- ggplot(data = S4B_data,
              aes(x = e_breadth, y = lat_range_sd_n)) +
  
  geom_density_2d_filled(bins = 20, show.legend = FALSE) +
  stat_summary_bin(breaks = S4B_breaks,
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

# ------ Figure S4C: Relationship between environmental breadth and latitudinal range - Southern hemisphere, tree -------------

S4C_data <- niche_data %>% filter(growthform == "tree") %>% filter(is.na(zone_n))
S4C_breaks <- quantile(S4C_data[["e_breadth"]], probs = seq(0, 1, by = 0.05), na.rm = TRUE)

S4C <- ggplot(data = S4C_data,
              aes(x = e_breadth, y = lat_range_sd_s)) +
  
  geom_density_2d_filled(bins = 20, show.legend = FALSE) +
  stat_summary_bin(breaks = S4C_breaks,
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

# ------ Figure S4D: Relationship between environmental breadth and latitudinal range - Southern hemisphere, herb -------------

S4D_data <- niche_data %>% filter(growthform == "herb") %>% filter(is.na(zone_n))
S4D_breaks <- quantile(S4D_data[["e_breadth"]], probs = seq(0, 1, by = 0.05), na.rm = TRUE)

S4D <- ggplot(data = S4D_data,
              aes(x = e_breadth, y = lat_range_sd_s)) +
  
  geom_density_2d_filled(bins = 20, show.legend = FALSE) +
  stat_summary_bin(breaks = S4D_breaks,
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

(S4 <- cowplot::plot_grid(S4A,
                         S4B,
                         S4C,
                         S4D,
                         rel_widths = c(1.3, 1, 1,
                                        1.3, 1, 1),
                         ncol = 2, byrow = FALSE,
                         labels = c("A", "C",
                                    "B", "D"),
                         label_size = 20)
)
#ggsave("./tmp/final/figure_S4.jpg",
#       width = 4000, height = 3000, units = "px")
 