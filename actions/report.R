# load necessary libraries

print(getwd())
library(dplyr)
library(tibble)

mtcars <- mtcars |> rownames_to_column("car_name")
write.csv(mtcars, "mtcars.csv")


# library(dplyr)
# library(readr)
# library(haven)
#
## get the most recent timestamped directory
# print(getwd())
#
# data_directory <- list.dirs(path = "../data/device-testing", full.names = TRUE)
# latest_directory <- data_directory[which.max(file.info(data_directory)$mtime)]
# responses_filepath <- file.path(latest_directory, "Responses.csv")
# labels_filepath <- file.path(latest_directory, "Labels.csv")
#
# print(data_directory)
# print(latest_directory)
#
## load the responses and labels data
# responses <- read_csv(responses_filepath)
# labels <- read_csv(labels_filepath)
#
## apply labels
# responses_labeled <- responses %>%
#  setNames(labels$variable_label[match(names(responses), labels$variable_name)])
#
## Create a list to store the value labels
# value_labels_list <- list()
#
## Separate the value labels into a list
# labels$value_label <- gsub(" ", "", labels$value_label) # remove spaces
# for(i in 1:nrow(labels)) {
#  value_labels_list[[labels$variable_label[i]]] <- strsplit(labels$value_label[i], ";")[[1]]
# }
#
## Apply value labels to the responses data
# for(i in 1:length(value_labels_list)) {
#  if(!is.null(value_labels_list[[i]])) {
#    for(j in 1:length(value_labels_list[[i]])) {
#      responses_labeled[[i]][responses_labeled[[i]] == as.character(j)] <- value_labels_list[[i]][j]
#    }
#  }
# }
#
## Save the data with labels as an RDS and SPSS file
# write.csv(responses, "responses_test.csv")
# saveRDS(responses_labeled, file = file.path(latest_directory, "responses_labeled.rds"))
# write_sav(responses_labeled, file.path(latest_directory, "responses_labeled.sav"))
#
