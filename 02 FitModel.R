# lib for shiny dashboard: FitModel
# Updated: 220729




# a wrapper function to train a model with train set and calculate the model performance on test set
fit_model <- function(
    formula, df_train, df_test, method, 
    preProcess = c("center", "scale"),
    trControl = trainControl(), 
    tuneGrid = NULL, 
    plot = FALSE, name = method, ... ){
  
  # timer - start
  proc_timer <- proc.time()
  
  # train model
  fit <- train(
    form = formula,
    data = df_train,
    method = method,
    preProcess = c("center", "scale"),
    trControl = trControl,
    tuneGrid = tuneGrid, ...)
  
  # timer - report time used
  timer <- proc.time() - proc_timer
  print(timer)
  
  # print the best tune if there is a tuning parameter
  if(is.null(tuneGrid)){
    print("No tuning parameter")
  } else {
    # print the best tune 
    best_tune <- glue("\t{names(fit$bestTune)} = {fit$bestTune[1,]}")
    print("The best tune is found with:")
    print(best_tune)
    if(plot) plot_modelinfo(fit)
  }
  
  # make prediction on test set
  pred <- predict(fit, newdata = df_test)
  
  # return performance metric or confusion matrix depending on response type
  if(is.numeric(pred)){
    # numeric response
    performance <- postResample(pred, obs = df_test[, 1])
    result <- tibble(
      name = name,
      method = method,
      best_tune = best_tune,
      RMSE = performance["RMSE"],
      kappa = performance["Rsquared"],
      time_elapsed = timer["elapsed"])    
    
    # print performance metrics
    print("Performance metrics:")
    print(performance)
  } else if(is.factor(pred)){
    # categorical response
    performance <- confusionMatrix(df_test[, 1], pred)
    result <- tibble(
      name = name,
      method = method,
      best_tune = best_tune,
      accuracy = performance$overall["Accuracy"],
      kappa = performance$overall["Kappa"],
      time_elapsed = timer["elapsed"])
    
    # print confusion matrix and accuracy
    print("Confusion table:")
    print(performance$table)
    print(glue("Accuracy = {performance$overall['Accuracy']}"))
  }

  # return the fitting
  return(list(result = result, performance = performance, fit = fit, timer = timer))
}


# a helper function to plot the metric vs. the tuning parameter
plot_modelinfo <- function(fit, plot_wrt = 1){
  # get model info
  model <- fit$modelInfo$label
  parameter <- fit$modelInfo$parameters$parameter
  description <- fit$modelInfo$parameters$label
  
  # plot parameter vs metrics
  p <- fit$results %>% 
    select(-setdiff(parameter, names(fit$results)[plot_wrt])) %>% 
    rename_at(1, ~"x") %>% 
    pivot_longer(cols = -1, names_to = "Metric") %>% 
    ggplot(aes(x, value, color = Metric)) +
    geom_point() +
    geom_line() +
    facet_grid(rows = vars(Metric), scales = "free_y") +
    labs(
      title = glue("{model}: Hyperparameter Tuning"),
      x = glue("{parameter} ({description})")
    )
  print(p)
  return(p)
}
