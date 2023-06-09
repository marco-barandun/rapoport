library(tidyverse)

setwd("/Users/marco/GitHub/environmental_breadth_final/"); getwd()

# Set global text size options
element_text_size <- 25

# ------ Import data --------------------------------------------------------------------------
niche_data <- read_csv("./3_generated_data/niche_data_final_summarized_v4.csv") %>%
  mutate(e_breadth = (env_breadth*mess)^(1/4)) %>%
  mutate(growthform = ifelse(growthform == "herb", "non-tree", growthform))

# Reading in the list of all non-tree species used
herbs <- read_csv("./3_generated_data/ALLherbs_medianLat.csv") %>% mutate(growthform = "herb")

# Reading in the original tree species dataframe
trees <- read_csv("./1_original_data/ALL_Species_cleaned_occtest_ocean.csv") %>%
  rename(Species = taxonID) %>%
  rename(latitude = Latitude) %>%
  group_by(Species) %>%
  summarise(lat_median = round(median(latitude, na.rm = TRUE), digits = 3),
            lat_median_absolute = round(median(abs(latitude), na.rm = TRUE), digits = 3),
            growthform = "tree") %>%
  mutate(sp = gsub(" ", "_", Species)) %>%
  dplyr::select(sp, lat_median, lat_median_absolute, growthform)

# Generating the excluded species list
species_excluded <- rbind(herbs, trees) %>%
  filter(!sp %in% niche_data$Species) %>%
  mutate(inc = "NO")

# Generating the included species list
species_included <- rbind(herbs, trees) %>%
  filter(sp %in% niche_data$Species) %>%
  mutate(inc = "YES")

all_species <- species_included%>%
  bind_rows(species_excluded)

# ------ Figure S1 A and B -------------

get_legend <- function(plot) {
  tmp <- ggplot_gtable(ggplot_build(plot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  legend
}

nr_lat_median_trees <- ggplot(all_species %>% filter(growthform == "tree"), aes(lat_median, fill = inc)) +
  geom_histogram() +
  scale_fill_manual(values=c("grey60", "grey10")) +
  xlab('Global median latitude') +
  ylab('Number of species') +
  ylim(0, 20000) +
  scale_x_continuous(breaks=c(-75, -50, -25, 0, 25, 50, 75), limits = c(-80, 80)) +
  theme_classic() +
  theme(text = element_text(size = element_text_size),
        legend.position="none")

nr_lat_median_herbs <- ggplot(all_species %>% filter(growthform == "herb"), aes(lat_median, fill = inc)) +
  geom_histogram() +
  xlab('Global median latitude') +
  ylab('Number of species') +
  ylim(0, 20000) +
  scale_x_continuous(breaks=c(-75, -50, -25, 0, 25, 50, 75), limits = c(-80, 80)) +
  theme_classic() +
  theme(text = element_text(size = element_text_size)) +
  scale_fill_manual(values=c("grey60", "grey10"), labels=c('Excluded', 'Included')) +
  guides(fill = guide_legend("Group"))

nr_lat_median_trees_herbs <- cowplot::plot_grid(nr_lat_median_trees, 
                                                nr_lat_median_herbs + theme(legend.position="none"),
                                                labels = c('A', 'B'), label_size = 25)

legend_tree_herbs <- get_legend(
  # create some space to the left of the legend
  nr_lat_median_herbs + theme(legend.box.margin = margin(0, 0, 0, 0)))

(cowplot::plot_grid(nr_lat_median_trees_herbs, legend_tree_herbs, rel_widths = c(3, .4)))

#ggsave("./tmp/final/figure_S1.jpg",
#       width = 3840, height = 2160, units = "px")


# ------ Figure S2 A and B -------------

### Figure S2A & B
(auc_plot <- ggplot(niche_data, aes(auc.val.avg, fill = growthform)) +
   geom_histogram() +
   xlab('Average AUC value') +
   ylab('Number of included species') +
   theme_classic() +
   theme(text = element_text(size = element_text_size),
         legend.text = element_text(size = element_text_size),  # Set legend text size
         legend.position = "none") +
   scale_fill_grey(start = 0.35, end = .9, name = "Growthform")
)

(or_plot <- ggplot(niche_data, aes(or.10p.avg, fill = growthform)) +
    geom_histogram() +
    xlab('Average 10% omission rate') +
    ylab('Number of included species') +
    theme_classic() +
    theme(text = element_text(size = element_text_size),
          legend.text = element_text(size = element_text_size),  # Set legend text size
          legend.position = "none") +
    scale_fill_grey(start = 0.35, end = .9, name = "Growthform")
)

auc_or_plots <- cowplot::plot_grid(auc_plot, or_plot, labels = c('A', 'B'), label_size = element_text_size)

legend_auc_or <- get_legend(
  auc_plot + theme(legend.position = "right", legend.box.margin = margin(0, 0, 0, 0),
                   legend.text = element_text(size = element_text_size))  # Set legend text size
)

# Adjust the left margin of the plot to accommodate the legend title
(plot_with_legend <- cowplot::plot_grid(
    auc_or_plots,
    legend_auc_or,
    rel_widths = c(3, 0.5))
)

#ggsave("./tmp/final/figure_S2.jpeg",
#       width = 3840, height = 2160, units = "px")
 
