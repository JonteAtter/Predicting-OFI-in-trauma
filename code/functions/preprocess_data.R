
#####################
## Preprocess data ##
#####################

preprocess_data <- function(data, verbose = FALSE) {
  recipe <- 
    recipe(ofi ~ ., data = data) %>% 
    step_indicate_na(all_predictors()) %>% 
    step_impute_median(all_numeric_predictors()) %>%
    step_unknown(all_nominal_predictors()) %>%
    step_YeoJohnson(all_numeric_predictors()) %>% 
    step_dummy(all_nominal_predictors(), one_hot = TRUE) %>% 
    step_nzv(all_predictors()) %>% 
    prep(verbose = verbose)
  
  return (recipe)
}
