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

source("./2_scripts/5-1_coefficients.R")

allres_lmlr <- as.data.frame(allres_lmlr)
allres_lmeb <- as.data.frame(allres_lmeb)
allres_lmlr <- as.data.frame(allres_lmlr)

# Set global text size options
element_text_size <- 17
element_title_size <- 15
annotate_text_size <- 5

# ------ Import and filter data --------------------------------------------------------------------------

niche_data <- read_csv("./3_generated_data/niche_data_final_summarized_v4.csv") %>%
  mutate(e_breadth = (env_breadth*mess)^(1/4)) %>%
  left_join(read_csv("./3_generated_data/zones_v3.csv"), by = "Species")

# Defining color palette
mycolors <- c("#00000000", colorRampPalette(c("#FFFFD6FF", "yellow", "orange", "red"), alpha=TRUE)(20))



# ------ Figure 2A: The latitudinal gradient of latitudinal range - Global, tree -------------

F2A_data <- niche_data %>% filter(growthform == "tree"); range(F2A_data$lat_range_sd_g)
F2A_breaks <- quantile(F2A_data[["lat_median_g"]], probs = seq(0, 1, by = 0.05), na.rm = TRUE)

F2A <- ggplot(data = F2A_data,
              aes(x = lat_median_g, y = lat_range_sd_g)) +
  
  geom_density_2d_filled(bins = 20, show.legend = FALSE) +
  stat_summary_bin(breaks = F2A_breaks,
                   fun = mean, 
                   fun.min = function(x) mean(x)-sd(x)/sqrt(length(x)), 
                   fun.max = function(x) mean(x)+sd(x)/sqrt(length(x)),
                   color = "azure4") +
  
  ggtitle("Global, tree") +
  xlab("Latitudinal median") +
  ylab("Latitudinal range (SD)") +
  
  theme_classic() +
  theme(text = element_text(size = element_text_size),         title = element_text(size = element_title_size)) +
  scale_fill_manual(values = mycolors) +
  coord_cartesian(ylim=c(0, 10))

# ------ Figure 2B: The latitudinal gradient of latitudinal range - Global, non-tree -------------

F2B_data <- niche_data %>% filter(growthform == "herb"); range(F2B_data$lat_range_sd_g)
F2B_breaks <- quantile(F2B_data[["lat_median_g"]], probs = seq(0, 1, by = 0.05), na.rm = TRUE)

F2B <- ggplot(data = F2B_data,
              aes(x = lat_median_g, y = lat_range_sd_g)) +
  
  geom_density_2d_filled(bins = 20, show.legend = FALSE) +
  stat_summary_bin(breaks = F2B_breaks,
                   fun = mean, 
                   fun.min = function(x) mean(x)-sd(x)/sqrt(length(x)), 
                   fun.max = function(x) mean(x)+sd(x)/sqrt(length(x)),
                   color = "azure4") +
  
  ggtitle("Global, non-tree") +
  xlab("Latitudinal median") +
  ylab("Latitudinal range (SD)") +
  
  theme_classic() +
  theme(text = element_text(size = element_text_size),         title = element_text(size = element_title_size)) +
  scale_fill_manual(values = mycolors) +
  coord_cartesian(ylim=c(0, 10))

# ------ Figure 2C: The latitudinal gradient of latitudinal range - Northern hemisphere, tree -------------

F2C_data <- niche_data %>% filter(growthform == "tree"); range(F2C_data$lat_range_sd_g)
F2C_breaks <- quantile(F2C_data[["lat_median_n"]], probs = seq(0, 1, by = 0.05), na.rm = TRUE)

F2C <- ggplot(data = F2C_data,
              aes(x = lat_median_n, y = lat_range_sd_n)) +
  
  geom_density_2d_filled(bins = 20, show.legend = FALSE) +
  stat_summary_bin(breaks = F2C_breaks,
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
  theme(text = element_text(size = element_text_size),         title = element_text(size = element_title_size)) +
  scale_fill_manual(values = mycolors) +
  coord_cartesian(ylim = c(0, 7.5), xlim = c(0, 70))

# ------ Figure 2D: The latitudinal gradient of latitudinal range - Northern hemisphere, non-tree -------------

F2D_data <- niche_data %>% filter(growthform == "herb"); range(F2D_data$lat_range_sd_n)
F2D_breaks <- quantile(F2D_data[["lat_median_n"]], probs = seq(0, 1, by = 0.05), na.rm = TRUE)

F2D <- ggplot(data = F2D_data,
              aes(x = lat_median_n, y = lat_range_sd_n)) +
  
  geom_density_2d_filled(bins = 20, show.legend = FALSE) +
  stat_summary_bin(breaks = F2D_breaks,
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
  theme(text = element_text(size = element_text_size),         title = element_text(size = element_title_size)) +
  scale_fill_manual(values = mycolors) +
  coord_cartesian(ylim = c(0, 7.5), xlim = c(0, 70))

# ------ Figure 2E: The latitudinal gradient of latitudinal range - Southern hemisphere, tree -------------

F2E_data <- niche_data %>% filter(growthform == "tree"); range(F2E_data$lat_range_sd_s)
F2E_breaks <- quantile(F2E_data[["lat_median_s"]], probs = seq(0, 1, by = 0.05), na.rm = TRUE)

F2E <- ggplot(data = F2E_data,
              aes(x = lat_median_s, y = lat_range_sd_s)) +
  
  geom_density_2d_filled(bins = 20, show.legend = FALSE) +
  stat_summary_bin(breaks = F2E_breaks,
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
  theme(text = element_text(size = element_text_size),         title = element_text(size = element_title_size)) +
  scale_fill_manual(values = mycolors) +
  coord_cartesian(ylim = c(0, 7.5), xlim = c(0, 70))

# ------ Figure 2F: The latitudinal gradient of latitudinal range - Southern hemisphere, non-tree -------------

F2F_data <- niche_data %>% filter(growthform == "herb"); range(F2F_data$lat_range_sd_s)
F2F_breaks <- quantile(F2F_data[["lat_median_s"]], probs = seq(0, 1, by = 0.05), na.rm = TRUE)

F2F <- ggplot(data = F2F_data,
              aes(x = lat_median_s, y = lat_range_sd_s)) +
  
  geom_density_2d_filled(bins = 20, show.legend = FALSE) +
  stat_summary_bin(breaks = F2F_breaks,
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
  theme(text = element_text(size = element_text_size),
        title = element_text(size = element_title_size)) +
  scale_fill_manual(values = mycolors) +
  coord_cartesian(ylim = c(0, 7.5), xlim = c(0, 70))

# ------ Joining and saving the plots -------------------------------------------------------------

(F2 <- cowplot::plot_grid(F2A,
                         F2B,
                         F2C,
                         F2D,
                         F2E,
                         F2F,
                         rel_widths = c(1.3, 1, 1,
                                        1.3, 1, 1),
                         ncol = 3, byrow = FALSE,
                         labels = c("A", "C", "E",
                                    "B", "D", "F"),
                         label_size = 20)
)
ggsave("./tmp/final/figure_2.jpg",
       width = 4000, height = 2000, units = "px")


