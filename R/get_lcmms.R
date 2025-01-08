get_lcmms <- function(data, vars) {
  
  # Initialize list
  list_models <- list()
  
  for (i in seq_along(vars)) {
    
    # Define formula
    param <- vars[i]
    formula <- as.formula(paste(param, "day", sep = "~"))
    
    ### Estimate the initial model ----
    model_class_n1 <-
      hlme(
        fixed = {{formula}},
        random = ~ day,
        subject = "id",
        data = data,
        ng = 1
      )
    
    ### Estimate the model with 2 classes ----
    model_class_n2 <- {
      set.seed(123)
      gridsearch(
        hlme(
          fixed = {{formula}},
          random = ~ day,
          subject = "id",
          data = data,
          ng = 2,
          mixture = ~ day
        ),
        rep = 100,
        maxiter = 30,
        minit = model_class_n1
      )
    }
    
    ### Estimate the model with 3 classes ----
    model_class_n3 <- {
      set.seed(123)
      gridsearch(
        hlme(
          fixed = {{formula}},
          random = ~ day,
          subject = "id",
          data = data,
          ng = 3,
          mixture = ~ day
        ),
        rep = 100,
        maxiter = 30,
        minit = model_class_n1
      )
    }
    
    ### Model comparison
    compa_models <-
      summarytable(
        model_class_n1,
        model_class_n2,
        model_class_n3,
        which = c("AIC", "BIC", "entropy", "%class")
      )
    
    ### Assign results to list
    list_models[[param]]$name <- param 
    list_models[[param]]$model_class_n1 <- model_class_n1
    list_models[[param]]$model_class_n2 <- model_class_n2
    list_models[[param]]$model_class_n3 <- model_class_n3
    list_models[[param]]$compa_models <- compa_models
  }
  
    return(list_models)
}
  
  
