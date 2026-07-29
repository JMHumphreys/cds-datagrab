variable_adapter <- function(config=NULL, variable_id=NULL) get_variable_spec(variable_id %||% config$project$dataset_id, config)
