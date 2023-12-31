

library(dplyr)
library(tibble)
library(haven)
library(readr)

if (!dir.exists("data") | !dir.exists("data/device-testing")) {
  stop("The data directory or the device-testing directory does not exist.")
}

data_directory <- list.dirs(path = "data/device-testing", full.names = TRUE)
data_directory <- data_directory[data_directory != "data/device-testing"]  # remove the "data/device-testing" directory
timestamps <- basename(data_directory)
latest_timestamp <- max(timestamps)
latest_directory <- file.path("data/device-testing", latest_timestamp)
responses_filepath <- file.path(latest_directory, "Responses.csv")
labels_filepath <- file.path(latest_directory, "Labels.csv")

responses <- read_csv(responses_filepath)
labels <- read_csv(labels_filepath)

# Create a data frame for mapping the variable_name and variable_label
colname_mapping <- left_join(
  tibble(old_names = names(responses)),
  labels, 
  by = c("old_names" = "variable_label")
)

# Handle columns not found in labels
colname_mapping$variable_name <- ifelse(
  is.na(colname_mapping$variable_name),
  colname_mapping$old_names,
  colname_mapping$variable_name
)

# Set the new names to responses data frame
names(responses) <- colname_mapping$variable_name
labelled::var_label(responses) <- labels |> pull(variable_label)

responses <- responses %>% mutate(across(q1:q10, 
                                         ~ case_when(
                                           . == "Strongly disagree" ~ 1,
                                           . == "Somewhat disagree" ~ 2,
                                           . == "Neither disagree nor agree" ~ 3,
                                           . == "Somewhat agree" ~ 4,
                                           . == "Strongly agree" ~ 5,
                                           TRUE ~ NA_real_
                                         ), .names = "{.col}"))

write.csv(responses, "data/survey_responses.csv")
