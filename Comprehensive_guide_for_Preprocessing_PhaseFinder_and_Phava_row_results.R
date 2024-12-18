##################################################################################
# The Comprehensive Guide for Pre-processing Raw Results from PhaseFinder & Phava#
# for Generating Informative Plots. "From Zero to Hero". Author: MSc Alon Kedem. #
##################################################################################

#### First Step ####
# Define a working directory in the location you stored all the result
# from PhaseFinder/ Phava.
# Example:
setwd("~/Documents/R/Technion_NGZ/Tomm/Tomm_December_2024_results")

#### Second Step ####
# Import all required libraries below. If additional libraries are needed later,
# add them to this section for better organization.
# To install libraries simply run "install.packages("name_of_library")"
library(dplyr)
library(ggplot2)
library(stringr)

#### Third Step ####
# Concatenate all the files result's names to one list.
# Notice: the path should be exact as the first stage.
# Notice: you might need to change pattern = "\\.tsv$" to pattern = "\\.txt$", 
#         depends on the extension of the results. For Phava it will be tsv, for
#         PhaseFinder it will be txt.
files <- list.files(
  path = "~/Documents/R/Technion_NGZ/Tomm/Tomm_December_2024_results",
  pattern = "\\.tsv$",
  full.names = TRUE
)

# Combine the row results data
all_data <- do.call(rbind, lapply(files, function(file) {
  data <- read.table(file)
  data$filename <- basename(file)
  return(data)
}))

#### If you ran Phava ####
{
# The V5 column contains the filename (sample identifier).
# Extract the file number only to simplify data manipulation in the following
# process.
all_data$barcode_num <- str_extract(all_data$V5, "\\d+")

# Create another column based on the specification of your samples. 
# notice: EDIT it according your experiment.
# Example:
all_data <- all_data %>%
  mutate(treatment = case_when(
    barcode_num %in% c("96", "95", "94") ~ "PH7",
    barcode_num %in% c("93", "92", "91") ~ "PH5_8",
    barcode_num %in% c("88", "87", "86") ~ "T37",
    barcode_num %in% c("85", "84", "83") ~ "T42",
    TRUE ~ "other"  
  ))
# To add more columns based on binding, you can run this again and change
# "treatment" to a different column like "groups"

# Filter results to retrieve just the important regions that actually invert,
# with sufficient coverage.
all_data_filter <- all_data %>%
  mutate(V4 = if_else(V4 == 10, 1, V4)) %>%
  mutate(reads_count = (V2 + V3)) %>%
  filter(reads_count > 10) %>%
  group_by(V1) %>%
  mutate(id_count = n()) %>%
  filter(id_count > 20) %>% #20 is recommended but you can do 15 as well to get
                            # more results if you do not get much.. 
  mutate(mean_pe = mean(V4)) %>%
  filter(mean_pe > 0.005)
}

#### If you ran PhaseFinder ####
{
# Replace the column names by the first row, which is the actually column names.
colnames(all_data) <- as.character(all_data[1,])
# and remove that row after determinig it as the columns names.
all_data <- all_data[-1,]

# Reset row numbers
rownames(all_data) <- NULL

# Convert the Forward, Reverse andPe_ration to numeric vactors
all_data$Pe_ratio <- as.numeric(all_data$Pe_ratio)
all_data$Pe_F <- as.numeric(all_data$Pe_F)
all_data$Pe_R <- as.numeric(all_data$Pe_R)

# If the values are NA, they are actually should be 0. So we convert them to 0.
all_data$Pe_ratio <- ifelse(is.na(all_data$Pe_ratio), 0, all_data$Pe_ratio) 
all_data$Pe_F <- ifelse(is.na(all_data$Pe_F), 0, all_data$Pe_F)
all_data$Pe_R <- ifelse(is.na(all_data$Pe_R), 0, all_data$Pe_R)

# Filter results to retrieve just the important regions that actually invert,
# with sufficient coverage.
all_data_filter <- all_data %>%
  mutate(reads_count = (Pe_F + Pe_R)) %>%
  filter(reads_count > 20) %>% #20 is recommended but you can do 15 as well to get
                               # more results if you do not get much..
  group_by(ID) %>%
  mutate(id_count = n()) %>%
  filter(id_count > 10) %>%
  mutate(mean_pe = mean(Pe_ratio)) %>%
  filter(mean_pe > 0.005)

# Based on how many times you ran PhaseFinder, if you ran it three times, keep it
# as it is. If you ran it twice so delete the "^out_|_3.ratio.txt$ row". 
# If you ran PhaseFinder once you may delete both ^out_|_2 and ^out_|_3, ignore
# this step totally.
# NOTICE: change the next column's name to what you have have. you can easly find
# it by running "colnames(all_data_filter)", copy paste the col that starts with 
# "out_ERR" for exemple "out_ERR1110309_1.ratio.txt"
all_data_filter$errs <- all_data_filter$out_ERR1110325_1.ratio.txt
all_data_filter$errs <- gsub("^out_|_1.ratio.txt$", "", all_data_filter$errs)
all_data_filter$errs <- gsub("^out_|_2.ratio.txt$", "", all_data_filter$errs)
all_data_filter$errs <- gsub("^out_|_3.ratio.txt$", "", all_data_filter$errs)

# Combine your metadata (aka: age, control, obese, ibd) to your data based on 
# the ERRs of the samples. Usually the column in the metadata will be saved as
# "RunID". the column have all the ERRs of samples.
data_combined <- left_join(all_data_filter, data, by = c("errs" = "RunID"))
}

#### Basic Exploratory Data Analysis ####
{
  # Change "treatment" to your factorial experiment column
  ggplot(all_data_filter, aes(x = as.factor(treatment), y = V4, color = treatment)) +
    geom_boxplot() +
    labs(title = "V4 values by Treatment",
         x = "Treatment",
         y = "V4 value") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    ylim(0, 1)
  
  for (id in unique(all_data_filter$V1)) {
    temp.data <- all_data_filter %>% filter(V1 == id) 
    plot <- ggplot(temp.data, aes(x = as.factor(treatment), y = V4, color = treatment)) +
      geom_point() +
      labs(title = paste("Plot for", id),
           x = "Treatment",
           y = "V4 value") +
      theme_bw()
    print(plot)
  }
}
