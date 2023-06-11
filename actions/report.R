# load necessary libraries

print(getwd())
library(dplyr)
library(tibble)
library(haven)
library(readr)


mtcars <- mtcars |> rownames_to_column("car_name")
write.csv(mtcars, "mtcars.csv")


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
print(data_directory)
print(latest_directory)


responses_filepath
# load the responses and labels data
responses <- read_csv(responses_filepath)
labels <- read_csv(labels_filepath)

# apply labels
responses_labeled <- responses %>%
setNames(labels$variable_label[match(names(responses), labels$variable_name)])

# Create a list to store the value labels
value_labels_list <- list()
#
# Separate the value labels into a list
labels$value_label <- gsub(" ", "", labels$value_label) # remove spaces
for(i in 1:nrow(labels)) {
 value_labels_list[[labels$variable_label[i]]] <- strsplit(labels$value_label[i], ";")[[1]]
}
#
## Apply value labels to the responses data
for(i in 1:length(value_labels_list)) {
  if(!is.null(value_labels_list[[i]])) {
    for(j in 1:length(value_labels_list[[i]])) {
      responses_labeled[[i]][responses_labeled[[i]] == as.character(j)] <- value_labels_list[[i]][j]
    }
  }
}

## Save the data with labels as an RDS and SPSS file

write.csv(responses, file = file.path(latest_directory, "responses_labeled.csv"))
saveRDS(responses_labeled, file = file.path(latest_directory, "responses_labeled.rds"))
#write_sav(responses_labeled, file.path(latest_directory, "responses_labeled.sav"))
#
