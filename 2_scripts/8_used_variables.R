library(tidyverse)

setwd("/Users/marco/GitHub/environmental_breadth_final/")

# ------ Read in the data from the models --------------------------------------------------------------------------

# Getting the list of species
modelled_species <- list.files(path = models_path) %>%
  sub("_models.rds", "", .)

all_used_variables <- data.frame()

for (species in modelled_species) {
  
  sp_model <- readRDS(paste0(models_path, species, "_models.rds"))
  
  # Selecting best model
  results_ordered <- sp_model@results[order(sp_model@results$or.mtp.avg, -sp_model@results$auc.val.avg),]
  best_modelOR <- as.integer(rownames(results_ordered[1,]))
  m <- sp_model@models[[best_modelOR]]
  
  # Extracting the used variables and appending them to the dataframe with the species information
  all_used_variables <- t(as.data.frame(m$betas)) %>%
    as.data.frame(.) %>%
    mutate(species = species) %>%
    bind_rows(all_used_variables, .)

}

# Setting the used variables to 1 and the non-used ones to 0
ds <- all_used_variables %>%
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), 0, 1))) %>%
  rownames_to_column() %>%
  select(-rowname) %>%
  select(sort(names(.)[!startsWith(names(.), "I(")]), sort(names(.)[startsWith(names(.), "I(")])) %>%
  select(species, everything())

#write_csv(all_used_variables %>% mutate(growthform = "tree"), "./3_generated_data/used_variables_bin_trees.csv")

### Generated files
# 1. ./3_generated_data/used_variables_num_trees.csv
# 2. ./3_generated_data/used_variables_bin_trees.csv
# 3. ./3_generated_data/used_variables_num_herbs.csv
# 4. ./3_generated_data/used_variables_bin_herbs.csv

# ------ Plotting --------------------------------------------------------------------------

niche_data <- read_csv("./3_generated_data/niche_data_final_summarized_v4.csv") %>%
  mutate(e_breadth = (env_breadth*mess)^(1/4))

trees <- read_csv("./3_generated_data/used_variables_bin_trees.csv") %>%
  filter(species %in% niche_data$Species) %>%
  mutate_at(vars(-one_of(c("species", "growthform"))), ~ifelse(!is.na(.), 1, NA))
  
herbs <- read_csv("./3_generated_data/used_variables_bin_herbs.csv") %>%
  filter(species %in% niche_data$Species)
  
ds <- herbs

sum_table <- data.frame(Column = colnames(ds %>% dplyr::select(-species, -growthform)), Sum = colSums(ds %>% dplyr::select(-species, -growthform), na.rm = TRUE)) %>%
  arrange(-Sum) %>%
  mutate(type = NULL)
#print(sum_table)

# Create a new column to group the rows
sum_table$Variable <- ifelse(grepl("SG", sum_table$Column), "SG", "CHELSA")

sum_table$type <- ifelse(grepl("^CHELSA_adj_[1-9]$|^CHELSA_adj_1[0-1]$", sum_table$Column), "TEMPERATURE",
                         ifelse(grepl("^I\\(CHELSA_adj_[1-9]\\^2\\)$|^I\\(CHELSA_adj_1[0-1]\\^2\\)$", sum_table$Column), "TEMPERATURE",
                                ifelse(grepl("^CHELSA_adj_1[2-9]$|^CHELSA_adj_1[2-9]$", sum_table$Column), "PRECIPITATION",
                                       ifelse(grepl("^I\\(CHELSA_adj_1[2-9]\\^2\\)$|^I\\(CHELSA_adj_1[2-9]\\^2\\)$", sum_table$Column), "PRECIPITATION",
                                              ifelse(sum_table$Variable == "SG", "SOIL", NA)))))

sum_table$feature <- ifelse(grepl("I\\(", sum_table$Column), "QUADRATIC", "LINEAR")

# Reorder the rows within each group based on the presence of "I("
sum_table$Column <- reorder(sum_table$Column, grepl("CHELSA", sum_table$Column))
sum_table$Column <- reorder(sum_table$Column, grepl("I\\(", sum_table$Column))
sum_table$group <- paste(sum_table$feature, sum_table$type, sep = "-")

(overview <- sum_table %>%
  group_by(group) %>%
  summarize(average_sum = mean(Sum)) %>%
  arrange(desc(average_sum)))


margin_spacer <- function(x) {
  # where x is the column in your dataset
  left_length <- nchar(levels(factor(x)))[1]
  if (left_length > 8) {
    return((left_length - 8) * 4)
  } else {
    return(0) 
  }
}

# Create the bar plot
ggplot(sum_table, aes(x = Column, y = Sum, fill = Variable)) +
  geom_bar(stat = "identity", color = "black") +
  scale_fill_manual(values = c("SG" = "lightgrey", "CHELSA" = "#48494B")) +
  labs(x = "Predictor variable", 
       y = "SDMs including the variable", 
       #caption = "Growthform = herbs",
       fill = "Variable\ngroup") +
  geom_vline(xintercept = sum(grepl("I\\(", sum_table$Column)) + 0.5, linetype = "dashed") +
  annotate("text", x = 12, y = max(sum_table$Sum), label = "Linear", hjust = 0, vjust = 1, size = 6) +
  annotate("text", x = 36, y = max(sum_table$Sum), label = "Quadratic", hjust = 1, vjust = 1, size = 6) +
  theme_classic() +
  coord_cartesian(ylim = c(0, max(sum_table$Sum) * 1.1), expand = FALSE) + # Adjust the y-axis limits and expansion
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title = element_text(size = 16),  # Adjust the overall font size
        axis.text = element_text(size = 12),  # Adjust the overall font size
        plot.margin = margin(l = 0 + margin_spacer(sum_table$Column))
        ) 

#ggsave("./tmp/final/figure_S9_used_variables_non-tree.jpg",
#       width = 3840, height = 2160, units = "px")
  