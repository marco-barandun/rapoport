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



# ------ Figure 3A: Environmental breadth and latitudinal range - Global, tree -------------

F3A_data <- niche_data %>% filter(growthform == "tree"); range(F3A_data$lat_range_sd_g)
F3A_breaks <- quantile(F3A_data[["e_breadth"]], probs = seq(0, 1, by = 0.05), na.rm = TRUE)

F3A <- ggplot(data = F3A_data,
              aes(x = e_breadth, y = lat_range_sd_g)) +
  
  geom_density_2d_filled(bins = 20, show.legend = FALSE) +
  stat_summary_bin(breaks = F3A_breaks,
                   fun = mean, 
                   fun.min = function(x) mean(x)-sd(x)/sqrt(length(x)), 
                   fun.max = function(x) mean(x)+sd(x)/sqrt(length(x)),
                   color = "azure4") +
  geom_smooth(color = "black",
              method = "lm",
              na.rm = TRUE,
              show.legend = FALSE) +
  
  ggtitle("Global, tree") +
  xlab("Environmental breadth") +
  ylab("Latitudinal range (SD)") +
  
  theme_classic() +
  theme(text = element_text(size = element_text_size),         title = element_text(size = element_title_size)) +  
  scale_fill_manual(values = mycolors) +
  coord_cartesian(ylim=c(0, 10))

# ------ Figure 3B: The latitudinal gradient of latitudinal range - Global, herb -------------

F3B_data <- niche_data %>% filter(growthform == "herb"); range(F3B_data$lat_range_sd_g)
F3B_breaks <- quantile(F3B_data[["e_breadth"]], probs = seq(0, 1, by = 0.05), na.rm = TRUE)

F3B <- ggplot(data = F3B_data,
              aes(x = e_breadth, y = lat_range_sd_g)) +
  
  geom_density_2d_filled(bins = 20, show.legend = FALSE) +
  stat_summary_bin(breaks = F3B_breaks,
                   fun = mean, 
                   fun.min = function(x) mean(x)-sd(x)/sqrt(length(x)), 
                   fun.max = function(x) mean(x)+sd(x)/sqrt(length(x)),
                   color = "azure4") +
  geom_smooth(color = "black",
    method = "lm",
    na.rm = TRUE,
    show.legend = FALSE) +
  
  ggtitle("Global, non-tree") +
  xlab("Environmental breadth") +
  ylab("Latitudinal range (SD)") +
  
  theme_classic() +
  theme(text = element_text(size = element_text_size),         title = element_text(size = element_title_size)) +  
  scale_fill_manual(values = mycolors) +
  coord_cartesian(ylim=c(0, 10))

# ------ Figure 3C: The latitudinal gradient of latitudinal range - Northern hemisphere, tree -------------

F3C_data <- niche_data %>% filter(growthform == "tree"); range(F3C_data$lat_range_sd_g)
F3C_breaks <- quantile(F3C_data[["e_breadth"]], probs = seq(0, 1, by = 0.05), na.rm = TRUE)

F3C <- ggplot(data = F3C_data,
              aes(x = e_breadth, y = lat_range_sd_n)) +
  
  geom_density_2d_filled(bins = 20, show.legend = FALSE) +
  stat_summary_bin(breaks = F3C_breaks,
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
  theme(text = element_text(size = element_text_size),         title = element_text(size = element_title_size)) +  
  scale_fill_manual(values = mycolors) +
  coord_cartesian(ylim = c(0, 7.5), xlim = c(0, 1))

# ------ Figure 3D: The latitudinal gradient of latitudinal range - Northern hemisphere, herb -------------

F3D_data <- niche_data %>% filter(growthform == "herb"); range(F3D_data$lat_range_sd_n)
F3D_breaks <- quantile(F3D_data[["e_breadth"]], probs = seq(0, 1, by = 0.05), na.rm = TRUE)

F3D <- ggplot(data = F3D_data,
              aes(x = e_breadth, y = lat_range_sd_n)) +
  
  geom_density_2d_filled(bins = 20, show.legend = FALSE) +
  stat_summary_bin(breaks = F3D_breaks,
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
  theme(text = element_text(size = element_text_size),         title = element_text(size = element_title_size)) +  
  scale_fill_manual(values = mycolors) +
  coord_cartesian(ylim = c(0, 7.5), xlim = c(0, 1))

# ------ Figure 3E: The latitudinal gradient of latitudinal range - Southern hemisphere, tree -------------

F3E_data <- niche_data %>% filter(growthform == "tree"); range(F3E_data$lat_range_sd_s)
F3E_breaks <- quantile(F3E_data[["e_breadth"]], probs = seq(0, 1, by = 0.05), na.rm = TRUE)

F3E <- ggplot(data = F3E_data,
              aes(x = e_breadth, y = lat_range_sd_s)) +
  
  geom_density_2d_filled(bins = 20, show.legend = FALSE) +
  stat_summary_bin(breaks = F3E_breaks,
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
  annotate("text", x = 0.68, y = 7, size = annotate_text_size, 
           label = paste("β (else) =\n",
                         allres_eblr %>%
                           filter(hemisphere == "South") %>%
                           filter(growthform == "tree") %>%
                           filter(zone == "else") %>%
                           .$valse)) +
  
  theme_classic() +
  theme(text = element_text(size = element_text_size),         title = element_text(size = element_title_size)) +  
  scale_fill_manual(values = mycolors) +
  coord_cartesian(ylim = c(0, 7.5), xlim = c(0, 1))

# ------ Figure 3F: The latitudinal gradient of latitudinal range - Southern hemisphere, herb -------------

F3F_data <- niche_data %>% filter(growthform == "herb"); range(F3F_data$lat_range_sd_s)
F3F_breaks <- quantile(F3F_data[["e_breadth"]], probs = seq(0, 1, by = 0.05), na.rm = TRUE)

F3F <- ggplot(data = F3F_data,
              aes(x = e_breadth, y = lat_range_sd_s)) +
  
  geom_density_2d_filled(bins = 20, show.legend = FALSE) +
  stat_summary_bin(breaks = F3F_breaks,
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
  annotate("text", x = 0.68, y = 7, size = annotate_text_size, 
           label = paste("β (else) =\n",
                         allres_eblr %>%
                           filter(hemisphere == "South") %>%
                           filter(growthform == "herb") %>%
                           filter(zone == "else") %>%
                           .$valse)) +
  
  theme_classic() +
  theme(text = element_text(size = element_text_size),
        title = element_text(size = element_title_size)) +  
  scale_fill_manual(values = mycolors) +
  coord_cartesian(ylim = c(0, 7.5), xlim = c(0, 1))

# ------ Joining and saving the plots -------------------------------------------------------------

(F3 <- cowplot::plot_grid(F3A,
                         F3B,
                         F3C,
                         F3D,
                         F3E,
                         F3F,
                         rel_widths = c(1.3, 1, 1,
                                        1.3, 1, 1),
                         ncol = 3, byrow = FALSE,
                         labels = c("A", "C", "E",
                                    "B", "D", "F"),
                         label_size = 20)
)
ggsave("./tmp/final/figure_3.jpg",
       width = 4000, height = 2000, units = "px")
