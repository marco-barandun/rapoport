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
element_title_size <- 14.5
annotate_text_size <- 5

# ------ Import and filter data --------------------------------------------------------------------------

niche_data <- read_csv("./3_generated_data/niche_data_final_summarized_v4.csv") %>%
  mutate(e_breadth = (env_breadth*mess)^(1/4)) %>%
  left_join(read_csv("./3_generated_data/zones_v3.csv"), by = "Species")

# Defining color palette
mycolors <- c("#00000000", colorRampPalette(c("#FFFFD6FF", "yellow", "orange", "red"), alpha=TRUE)(20))


# ------ Figure 4A: Latitudinal gradients of environmental breadth - Global, tree -------------

F4A_data <- niche_data %>% filter(growthform == "tree")
F4A_breaks <- quantile(F4A_data[["lat_median_g"]], probs = seq(0, 1, by = 0.05), na.rm = TRUE)

F4A <- ggplot(data = F4A_data,
              aes(x = lat_median_g, y = e_breadth)) +
  
  geom_density_2d_filled(bins = 20, show.legend = FALSE) +
  stat_summary_bin(breaks = F4A_breaks,
                   fun = mean, 
                   fun.min = function(x) mean(x)-sd(x)/sqrt(length(x)), 
                   fun.max = function(x) mean(x)+sd(x)/sqrt(length(x)),
                   color = "azure4") +
  
  ggtitle("Global, tree") +
  xlab("Latitudinal median") +
  ylab("Environmental breath") +
  
  theme_classic() +
  theme(text = element_text(size = element_text_size),         
        title = element_text(size = element_title_size)) +  
  scale_fill_manual(values = mycolors) +
  coord_cartesian(ylim=c(0, 0.8))

# ------ Figure 4B: Latitudinal gradients of environmental breadth - Global, herb -------------

F4B_data <- niche_data %>% filter(growthform == "herb")
F4B_breaks <- quantile(F4B_data[["lat_median_g"]], probs = seq(0, 1, by = 0.05), na.rm = TRUE)

F4B <- ggplot(data = F4B_data,
              aes(x = lat_median_g, y = e_breadth)) +
  
  geom_density_2d_filled(bins = 20, show.legend = FALSE) +
  stat_summary_bin(breaks = F4B_breaks,
                   fun = mean, 
                   fun.min = function(x) mean(x)-sd(x)/sqrt(length(x)), 
                   fun.max = function(x) mean(x)+sd(x)/sqrt(length(x)),
                   color = "azure4") +
  
  ggtitle("Global, non-tree") +
  xlab("Latitudinal median") +
  ylab("Environmental breadth") +
  
  theme_classic() +
  theme(text = element_text(size = element_text_size),         
        title = element_text(size = element_title_size)) +  
  scale_fill_manual(values = mycolors) +
  coord_cartesian(ylim=c(0, 0.8))

# ------ Figure 4C: Latitudinal gradients of environmental breadth - Northern hemisphere, tree -------------

F4C_data <- niche_data %>% filter(growthform == "tree")
F4C_breaks <- quantile(F4C_data[["lat_median_n"]], probs = seq(0, 1, by = 0.05), na.rm = TRUE)

F4C <- ggplot(data = F4C_data,
              aes(x = lat_median_n, y = e_breadth)) +
  
  geom_density_2d_filled(bins = 20, show.legend = FALSE) +
  stat_summary_bin(breaks = F4C_breaks,
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
  annotate("text", x = 12, y = 0.75, size = 5, 
           label = paste("β =",
                         allres_lmeb %>%
                           filter(hemisphere == "North") %>%
                           filter(growthform == "tree") %>%
                           filter(zone == "tropical") %>%
                           .$valse)) +
  annotate("text", x = 45, y = 0.75, size = 5, 
           label = paste("β =",
                         allres_lmeb %>%
                           filter(hemisphere == "North") %>%
                           filter(growthform == "tree") %>%
                           filter(zone == "else") %>%
                           .$valse)) +
  
  theme_classic() +
  theme(text = element_text(size = element_text_size),         
        title = element_text(size = element_title_size)) +  
  scale_fill_manual(values = mycolors) +
  coord_cartesian(ylim = c(0, 0.8), xlim = c(0, 70))

# ------ Figure 4D: Latitudinal gradients of environmental breadth - Northern hemisphere, herb -------------

F4D_data <- niche_data %>% filter(growthform == "herb")
F4D_breaks <- quantile(F4D_data[["lat_median_n"]], probs = seq(0, 1, by = 0.05), na.rm = TRUE)

F4D <- ggplot(data = F4D_data,
              aes(x = lat_median_n, y = e_breadth)) +
  
  geom_density_2d_filled(bins = 20, show.legend = FALSE) +
  stat_summary_bin(breaks = F4D_breaks,
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
  annotate("text", x = 12, y = 0.75, size = 5, 
           label = paste("β =",
                         allres_lmeb %>%
                           filter(hemisphere == "North") %>%
                           filter(growthform == "herb") %>%
                           filter(zone == "tropical") %>%
                           .$valse)) +
  annotate("text", x = 45, y = 0.75, size = 5, 
           label = paste("β =",
                         allres_lmeb %>%
                           filter(hemisphere == "North") %>%
                           filter(growthform == "herb") %>%
                           filter(zone == "else") %>%
                           .$valse)) +
  
  theme_classic() +
  theme(text = element_text(size = element_text_size),         
        title = element_text(size = element_title_size)) +  
  scale_fill_manual(values = mycolors) +
  coord_cartesian(ylim = c(0, 0.8), xlim = c(0, 70))

# ------ Figure 4E: Latitudinal gradients of environmental breadth - Southern hemisphere, tree -------------

F4E_data <- niche_data %>% filter(growthform == "tree"); range(F4E_data$lat_range_sd_s)
F4E_breaks <- quantile(F4E_data[["lat_median_s"]], probs = seq(0, 1, by = 0.05), na.rm = TRUE)

F4E <- ggplot(data = F4E_data,
              aes(x = lat_median_s, y = e_breadth)) +
  
  geom_density_2d_filled(bins = 20, show.legend = FALSE) +
  stat_summary_bin(breaks = F4E_breaks,
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
  annotate("text", x = 12, y = 0.75, size = 5, 
           label = paste("β =",
                         allres_lmeb %>%
                           filter(hemisphere == "South") %>%
                           filter(growthform == "tree") %>%
                           filter(zone == "tropical") %>%
                           .$valse)) +
  annotate("text", x = 45, y = 0.75, size = 5, 
           label = paste("β =",
                         allres_lmeb %>%
                           filter(hemisphere == "South") %>%
                           filter(growthform == "tree") %>%
                           filter(zone == "else") %>%
                           .$valse)) +
  
  theme_classic() +
  theme(text = element_text(size = element_text_size),        
        title = element_text(size = element_title_size)) +  
  scale_fill_manual(values = mycolors) +
  coord_cartesian(ylim = c(0, 0.8), xlim = c(0, 70))

# ------ Figure 4F: Latitudinal gradients of environmental breadth - Southern hemisphere, herb -------------

F4F_data <- niche_data %>% filter(growthform == "herb"); range(F4F_data$lat_range_sd_s)
F4F_breaks <- quantile(F4F_data[["lat_median_s"]], probs = seq(0, 1, by = 0.05), na.rm = TRUE)

F4F <- ggplot(data = F4F_data,
              aes(x = lat_median_s, y = e_breadth)) +
  
  geom_density_2d_filled(bins = 20, show.legend = FALSE) +
  stat_summary_bin(breaks = F4F_breaks,
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
  annotate("text", x = 12, y = 0.75, size = 5, 
           label = paste("β =",
                         allres_lmeb %>%
                           filter(hemisphere == "South") %>%
                           filter(growthform == "herb") %>%
                           filter(zone == "tropical") %>%
                           .$valse)) +
  annotate("text", x = 45, y = 0.75, size = 5, 
           label = paste("β =",
                         allres_lmeb %>%
                           filter(hemisphere == "South") %>%
                           filter(growthform == "herb") %>%
                           filter(zone == "else") %>%
                           .$valse)) +
  
  theme_classic() +
  theme(text = element_text(size = element_text_size),
        title = element_text(size = element_title_size)) +  
  scale_fill_manual(values = mycolors) +
  coord_cartesian(ylim = c(0, 0.8), xlim = c(0, 70))

# ------ Joining and saving the plots -------------------------------------------------------------

(F4 <- cowplot::plot_grid(F4A,
                         F4B,
                         F4C,
                         F4D,
                         F4E,
                         F4F,
                         rel_widths = c(1.3, 1, 1,
                                        1.3, 1, 1),
                         ncol = 3, byrow = FALSE,
                         labels = c("A", "C", "E",
                                    "B", "D", "F"),
                         label_size = 20)
)
ggsave("./tmp/final/figure_4.jpg",
       width = 4000, height = 2000, units = "px")
