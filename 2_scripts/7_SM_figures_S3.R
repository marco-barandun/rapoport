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


# ------ Figure S3A: The latitudinal gradient of latitudinal range - Northern hemisphere, tree -------------

S3A_data <- niche_data %>% filter(growthform == "tree") %>% filter(is.na(zone_s))
S3A_breaks <- quantile(S3A_data[["lat_median_n"]], probs = seq(0, 1, by = 0.05), na.rm = TRUE)

S3A <- ggplot(data = S3A_data,
              aes(x = lat_median_n, y = lat_range_sd_n)) +
  
  geom_density_2d_filled(bins = 20, show.legend = FALSE) +
  stat_summary_bin(breaks = S3A_breaks,
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
  ylab("Latitudinal range (SD)") +
  annotate("text", x = 10, y = 7, size = annotate_text_size, 
           label = paste("β (tropical) =\n",
                         allres_lmlr %>%
                           filter(hemisphere == "North") %>%
                           filter(growthform == "tree") %>%
                           filter(zone == "tropical") %>%
                           .$valse)) +
  annotate("text", x = 45, y = 7, size = annotate_text_size, 
           label = paste("β (else) =\n",
                         allres_lmlr %>%
                           filter(hemisphere == "North") %>%
                           filter(growthform == "tree") %>%
                           filter(zone == "else") %>%
                           .$valse)) +
  
  theme_classic() +
  theme(text = element_text(size = element_text_size)) +
  scale_fill_manual(values = mycolors) +
  coord_cartesian(ylim = c(0, 7.5), xlim = c(0, 70))

# ------ Figure S3B: The latitudinal gradient of latitudinal range - Northern hemisphere, herb -------------

S3B_data <- niche_data %>% filter(growthform == "herb") %>% filter(is.na(zone_s))
S3B_breaks <- quantile(S3B_data[["lat_median_n"]], probs = seq(0, 1, by = 0.05), na.rm = TRUE)

S3B <- ggplot(data = S3B_data,
              aes(x = lat_median_n, y = lat_range_sd_n)) +
  
  geom_density_2d_filled(bins = 20, show.legend = FALSE) +
  stat_summary_bin(breaks = S3B_breaks,
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
  ylab("Latitudinal range (SD)") +
  annotate("text", x = 10, y = 7, size = annotate_text_size, 
           label = paste("β (tropical) =\n",
                         allres_lmlr %>%
                           filter(hemisphere == "North") %>%
                           filter(growthform == "herb") %>%
                           filter(zone == "tropical") %>%
                           .$valse)) +
  annotate("text", x = 45, y = 7, size = annotate_text_size, 
           label = paste("β (else) =\n",
                         allres_lmlr %>%
                           filter(hemisphere == "North") %>%
                           filter(growthform == "herb") %>%
                           filter(zone == "else") %>%
                           .$valse)) +
  
  theme_classic() +
  theme(text = element_text(size = element_text_size)) +
  scale_fill_manual(values = mycolors) +
  coord_cartesian(ylim = c(0, 7.5), xlim = c(0, 70))

# ------ Figure S3C: The latitudinal gradient of latitudinal range - Southern hemisphere, tree -------------

S3C_data <- niche_data %>% filter(growthform == "tree") %>% filter(is.na(zone_n))
S3C_breaks <- quantile(S3C_data[["lat_median_s"]], probs = seq(0, 1, by = 0.05), na.rm = TRUE)

S3C <- ggplot(data = S3C_data,
              aes(x = lat_median_s, y = lat_range_sd_s)) +
  
  geom_density_2d_filled(bins = 20, show.legend = FALSE) +
  stat_summary_bin(breaks = S3C_breaks,
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
  ylab("Latitudinal range (SD)") +
  annotate("text", x = 10, y = 7, size = annotate_text_size, 
           label = paste("β (tropical) =\n",
                         allres_lmlr %>%
                           filter(hemisphere == "South") %>%
                           filter(growthform == "tree") %>%
                           filter(zone == "tropical") %>%
                           .$valse)) +
  annotate("text", x = 45, y = 7, size = annotate_text_size, 
           label = paste("β (else) =\n",
                         allres_lmlr %>%
                           filter(hemisphere == "South") %>%
                           filter(growthform == "tree") %>%
                           filter(zone == "else") %>%
                           .$valse)) +
  
  theme_classic() +
  theme(text = element_text(size = element_text_size)) +
  scale_fill_manual(values = mycolors) +
  coord_cartesian(ylim = c(0, 7.5), xlim = c(0, 70))

# ------ Figure S3D: The latitudinal gradient of latitudinal range - Southern hemisphere, herb -------------

S3D_data <- niche_data %>% filter(growthform == "herb") %>% filter(is.na(zone_n))
S3D_breaks <- quantile(S3D_data[["lat_median_s"]], probs = seq(0, 1, by = 0.05), na.rm = TRUE)

S3D <- ggplot(data = S3D_data,
              aes(x = lat_median_s, y = lat_range_sd_s)) +
  
  geom_density_2d_filled(bins = 20, show.legend = FALSE) +
  stat_summary_bin(breaks = S3D_breaks,
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
  ylab("Latitudinal range (SD)") +
  annotate("text", x = 10, y = 7, size = annotate_text_size, 
           label = paste("β (tropical) =\n",
                         allres_lmlr %>%
                           filter(hemisphere == "South") %>%
                           filter(growthform == "herb") %>%
                           filter(zone == "tropical") %>%
                           .$valse)) +
  annotate("text", x = 45, y = 7, size = annotate_text_size, 
           label = paste("β (else) =\n",
                         allres_lmlr %>%
                           filter(hemisphere == "South") %>%
                           filter(growthform == "herb") %>%
                           filter(zone == "else") %>%
                           .$valse)) +
  
  theme_classic() +
  theme(text = element_text(size = element_text_size)) +
  scale_fill_manual(values = mycolors) +
  coord_cartesian(ylim = c(0, 7.5), xlim = c(0, 70))

# ------ Joining and saving the plots -------------------------------------------------------------

(S3 <- cowplot::plot_grid(S3A,
                         S3B,
                         S3C,
                         S3D,
                         rel_widths = c(1.3, 1, 1,
                                        1.3, 1, 1),
                         ncol = 2, byrow = FALSE,
                         labels = c("A", "C",
                                    "B", "D"),
                         label_size = 20)
)
ggsave("./tmp/final/figure_S3.jpg",
       width = 4000, height = 3000, units = "px")
 
