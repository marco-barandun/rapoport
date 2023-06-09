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



# ------ Figure S5A: Latitudinal gradient environmental breadth - Northern hemisphere, tree -------------

S5A_data <- niche_data %>% filter(growthform == "tree") %>% filter(is.na(zone_s))
S5A_breaks <- quantile(S5A_data[["lat_median_n"]], probs = seq(0, 1, by = 0.05), na.rm = TRUE)

S5A <- ggplot(data = S5A_data,
              aes(x = lat_median_n, y = e_breadth)) +
  
  geom_density_2d_filled(bins = 20, show.legend = FALSE) +
  stat_summary_bin(breaks = S5A_breaks,
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
  xlab("Latitudinal median") +
  ylab("Environmental breadth") +
  annotate("text", x = 10, y = 0.75, size = annotate_text_size, 
           label = paste("β (tropical) =\n",
                         allres_lmeb %>%
                           filter(hemisphere == "North") %>%
                           filter(growthform == "tree") %>%
                           filter(zone == "tropical") %>%
                           .$valse)) +
  annotate("text", x = 45, y = 0.75, size = annotate_text_size, 
           label = paste("β (else) =\n",
                         allres_lmeb %>%
                           filter(hemisphere == "North") %>%
                           filter(growthform == "tree") %>%
                           filter(zone == "else") %>%
                           .$valse)) +
  
  theme_classic() +
  theme(text = element_text(size = element_text_size)) +
  scale_fill_manual(values = mycolors) +
  coord_cartesian(ylim = c(0, 0.8), xlim = c(0, 70))

# ------ Figure S5B: Latitudinal gradient environmental breadth - Northern hemisphere, herb -------------

S5B_data <- niche_data %>% filter(growthform == "herb") %>% filter(is.na(zone_s))
S5B_breaks <- quantile(S5B_data[["lat_median_n"]], probs = seq(0, 1, by = 0.05), na.rm = TRUE)

S5B <- ggplot(data = S5B_data,
              aes(x = lat_median_n, y = e_breadth)) +
  
  geom_density_2d_filled(bins = 20, show.legend = FALSE) +
  stat_summary_bin(breaks = S5B_breaks,
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
  xlab("Latitudinal median") +
  ylab("Environmental breadth") +
  annotate("text", x = 10, y = 0.75, size = annotate_text_size, 
           label = paste("β (tropical) =\n",
                         allres_lmeb %>%
                           filter(hemisphere == "North") %>%
                           filter(growthform == "herb") %>%
                           filter(zone == "tropical") %>%
                           .$valse)) +
  annotate("text", x = 45, y = 0.75, size = annotate_text_size, 
           label = paste("β (else) =\n",
                         allres_lmeb %>%
                           filter(hemisphere == "North") %>%
                           filter(growthform == "herb") %>%
                           filter(zone == "else") %>%
                           .$valse)) +
  
  theme_classic() +
  theme(text = element_text(size = element_text_size)) +
  scale_fill_manual(values = mycolors) +
  coord_cartesian(ylim = c(0, 0.8), xlim = c(0, 70))

# ------ Figure S5C: Latitudinal gradient environmental breadth - Southern hemisphere, tree -------------

S5C_data <- niche_data %>% filter(growthform == "tree") %>% filter(is.na(zone_n))
S5C_breaks <- quantile(S5C_data[["lat_median_s"]], probs = seq(0, 1, by = 0.05), na.rm = TRUE)

S5C <- ggplot(data = S5C_data,
              aes(x = lat_median_s, y = e_breadth)) +
  
  geom_density_2d_filled(bins = 20, show.legend = FALSE) +
  stat_summary_bin(breaks = S5C_breaks,
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
  xlab("Latitudinal median") +
  ylab("Environmental breadth") +
  annotate("text", x = 10, y = 0.75, size = annotate_text_size, 
           label = paste("β (tropical) =\n",
                         allres_lmeb %>%
                           filter(hemisphere == "South") %>%
                           filter(growthform == "tree") %>%
                           filter(zone == "tropical") %>%
                           .$valse)) +
  annotate("text", x = 45, y = 0.75, size = annotate_text_size, 
           label = paste("β (else) =\n",
                         allres_lmeb %>%
                           filter(hemisphere == "South") %>%
                           filter(growthform == "tree") %>%
                           filter(zone == "else") %>%
                           .$valse)) +
  
  theme_classic() +
  theme(text = element_text(size = element_text_size)) +
  scale_fill_manual(values = mycolors) +
  coord_cartesian(ylim = c(0, 0.8), xlim = c(0, 70))

# ------ Figure S5D: Latitudinal gradient environmental breadth - Southern hemisphere, herb -------------

S5D_data <- niche_data %>% filter(growthform == "herb") %>% filter(is.na(zone_n))
S5D_breaks <- quantile(S5D_data[["lat_median_s"]], probs = seq(0, 1, by = 0.05), na.rm = TRUE)

S5D <- ggplot(data = S5D_data,
              aes(x = lat_median_s, y = e_breadth)) +
  
  geom_density_2d_filled(bins = 20, show.legend = FALSE) +
  stat_summary_bin(breaks = S5D_breaks,
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
  xlab("Latitudinal median") +
  ylab("Environmental breadth") +
  annotate("text", x = 10, y = 0.75, size = annotate_text_size, 
           label = paste("β (tropical) =\n",
                         allres_lmeb %>%
                           filter(hemisphere == "South") %>%
                           filter(growthform == "herb") %>%
                           filter(zone == "tropical") %>%
                           .$valse)) +
  annotate("text", x = 45, y = 0.75, size = annotate_text_size, 
           label = paste("β (else) =\n",
                         allres_lmeb %>%
                           filter(hemisphere == "South") %>%
                           filter(growthform == "herb") %>%
                           filter(zone == "else") %>%
                           .$valse)) +
  
  theme_classic() +
  theme(text = element_text(size = element_text_size)) +
  scale_fill_manual(values = mycolors) +
  coord_cartesian(ylim = c(0, 0.8), xlim = c(0, 70))

# ------ Joining and saving the plots -------------------------------------------------------------

(S5 <- cowplot::plot_grid(S5A,
                         S5B,
                         S5C,
                         S5D,
                         rel_widths = c(1.3, 1, 1,
                                        1.3, 1, 1),
                         ncol = 2, byrow = FALSE,
                         labels = c("A", "C",
                                    "B", "D"),
                         label_size = 20)
)
#ggsave("./tmp/final/figure_S5.jpg",
#       width = 4000, height = 3000, units = "px")
 