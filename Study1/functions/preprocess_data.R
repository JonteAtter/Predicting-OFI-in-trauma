
#####################
## Preprocess data ##
#####################

preprocess_data <- function(data) {
  preprocessed.data <- mikropml::preprocess_data(data, outcome_colname = "ofi", to_numeric = FALSE)
  return (preprocessed.data$dat_transformed)
}
