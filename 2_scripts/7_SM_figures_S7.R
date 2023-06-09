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

# ------ Import and filter data --------------------------------------------------------------------------

niche_data <- read_csv("./3_generated_data/niche_data_final_summarized_v4.csv") %>%
  left_join(read_csv("./3_generated_data/MESSvalues_perHemisphere.csv") %>% rename(Species = species), by = c("Species", "growthform")) %>%
  mutate(e_breadth_n = (env_breadth*mess_n)^(1/4)) %>%
  mutate(e_breadth_s = (env_breadth*mess_s)^(1/4)) %>%
  left_join(read_csv("./3_generated_data/zones_v3.csv"), by = "Species"); ds <- niche_data

# Defining color palette
mycolors <- c("#00000000", colorRampPalette(c("#FFFFD6FF", "yellow", "orange", "red"), alpha=TRUE)(20))



# ------ Figure S7A: Latitudinal gradient environmental breadth - Northern hemisphere, tree -------------

S7A_data <- niche_data %>% filter(growthform == "tree") %>% filter(lat_median_n >= 0)
S7A_breaks <- quantile(S7A_data[["lat_median_n"]], probs = seq(0, 1, by = 0.05), na.rm = TRUE)

S7A <- ggplot(data = S7A_data,
              aes(x = lat_median_n, y = e_breadth_n)) +
  
  geom_density_2d_filled(bins = 20, show.legend = FALSE) +
  stat_summary_bin(breaks = S7A_breaks,
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
  scale_fill_manual(values = mycolors) +
  theme(text = element_text(size = element_text_size)) +
  coord_cartesian(ylim = c(0, 1), xlim = c(0, 70))

# ------ Figure S7B: Latitudinal gradient environmental breadth - Northern hemisphere, herb -------------

S7B_data <- niche_data %>% filter(growthform == "herb") %>% filter(lat_median_n >= 0)
S7B_breaks <- quantile(S7B_data[["lat_median_n"]], probs = seq(0, 1, by = 0.05), na.rm = TRUE)

S7B <- ggplot(data = S7B_data,
              aes(x = lat_median_n, y = e_breadth_n)) +
  
  geom_density_2d_filled(bins = 20, show.legend = FALSE) +
  stat_summary_bin(breaks = S7B_breaks,
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
  scale_fill_manual(values = mycolors) +
  theme(text = element_text(size = element_text_size)) +
  coord_cartesian(ylim = c(0, 1), xlim = c(0, 70))

# ------ Figure S7C: Latitudinal gradient environmental breadth - Southern hemisphere, tree -------------

S7C_data <- niche_data %>% filter(growthform == "tree") %>% filter(lat_median_s >= 0)
S7C_breaks <- quantile(S7C_data[["lat_median_s"]], probs = seq(0, 1, by = 0.05), na.rm = TRUE)

S7C <- ggplot(data = S7C_data,
              aes(x = lat_median_s, y = e_breadth_s)) +
  
  geom_density_2d_filled(bins = 20, show.legend = FALSE) +
  stat_summary_bin(breaks = S7C_breaks,
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
  scale_fill_manual(values = mycolors) +
  theme(text = element_text(size = element_text_size)) +
  coord_cartesian(ylim = c(0, 1), xlim = c(0, 70))

# ------ Figure S7D: Latitudinal gradient environmental breadth - Southern hemisphere, herb -------------

S7D_data <- niche_data %>% filter(growthform == "herb") %>% filter(lat_median_s >= 0)
S7D_breaks <- quantile(S7D_data[["lat_median_s"]], probs = seq(0, 1, by = 0.05), na.rm = TRUE)

S7D <- ggplot(data = S7D_data,
              aes(x = lat_median_s, y = e_breadth_s)) +
  
  geom_density_2d_filled(bins = 20, show.legend = FALSE) +
  stat_summary_bin(breaks = S7D_breaks,
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
  scale_fill_manual(values = mycolors) +
  theme(text = element_text(size = element_text_size)) +
  coord_cartesian(ylim = c(0, 1), xlim = c(0, 70))

# ------ Joining and saving the plots -------------------------------------------------------------

(S7 <- cowplot::plot_grid(S7A,
                         S7B,
                         S7C,
                         S7D,
                         rel_widths = c(1.3, 1, 1,
                                        1.3, 1, 1),
                         ncol = 2, byrow = FALSE,
                         labels = c("A", "C",
                                    "B", "D"),
                         label_size = 20)
)

#ggsave("./tmp/final/figure_S7.jpg",
#       width = 4000, height = 3000, units = "px")
 
