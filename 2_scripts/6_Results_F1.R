library(tidyverse)

setwd("/Users/marco/GitHub/environmental_breadth_final/")

# Set global text size options
element_text_size <- 17

niche_data <- read_csv("./3_generated_data/niche_data_final_summarized_v4.csv") %>%
  mutate(e_breadth = (env_breadth*mess)^(1/4)) %>%
  mutate(growthform = ifelse(growthform == "herb", "non-tree", growthform))

A <- ggplot(data = niche_data, aes(x = n_inc_obs, group = growthform, fill = growthform)) +
  geom_density(stat = "count", show.legend = FALSE) +
  xlab("Number of included observations") +
  ylab("Number of species") +
  theme_classic() +
  theme(text = element_text(size = element_text_size)) +
  coord_cartesian(xlim=c(0, 500)) + 
  scale_fill_grey(start = 0.35, 
                  end = .9,
                  name = "Growthform")

B <- ggplot(data = niche_data, aes(x = lat_median_g, group = growthform, fill = growthform)) +
  geom_density(alpha = 0.7, show.legend = FALSE) +
  xlab("Global median latitude") +
  ylab("Density of species") +
  theme_classic() +
  theme(text = element_text(size = element_text_size)) +
  scale_fill_grey(start = 0, 
                  end = .9,
                  name = "Growthform")

C <- ggplot(data = niche_data, aes(x = lat_range_sd_g, group = growthform, fill = growthform)) +
  geom_density(alpha = 0.7) +
  xlab("Global latitudinal range (SD)") +
  ylab("Density of species") +
  theme_classic() +
  theme(text = element_text(size = element_text_size)) +
  scale_fill_grey(start = 0, 
                  end = .9,
                  name = "Growthform") +
  coord_cartesian(xlim=c(0, 30))


(cowplot::plot_grid(A,
                   B,
                   C,
                   rel_widths = c(1, 1, 1),
                   ncol = 3, align = "h",
                   labels = c("A", "B", "C"),
                   label_size = 25))

#ggsave("./tmp/final/figure_1.jpg",
#       width = 4000, height = 1500, units = "px")

