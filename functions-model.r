# Function pr_auc() to return the area under the precision-recall curve
pr_auc <- function(true_classes, pred_pc, score_type, include_curve = FALSE){
  # Depending on pred_type ("raw" or "prob"), pred_pc is either probabilities or classes
  # true_classes are the classes of the observations, "G0" or "F1"
  # scores.weights0 must be numerical values (1 for positive class, 0 for other)
  if(score_type == "prob"){
    # Here pred_pc are the probabilities for the positive class (classification scores)
    # given by the classifier
    PRAUC <- pr.curve(scores.class0 = pred_pc$F1
                      , weights.class0 = ifelse(true_classes == "F1", 1, 0) 
                      , curve = include_curve)
  }
  if(score_type == "raw"){
    # Here pred_pc is the class prediction for all classes
    PRAUC <- pr.curve(scores.class0 = ifelse(pred_pc == "F1", 1, 0)
                      , weights.class0 = ifelse(true_classes == "F1", 1, 0) 
                      , curve = include_curve)
  }
  return(PRAUC)
}


# Function to try each type of model on the formula list, based on caret::train()
# formula_list must be a list of formula with names to work properly
# pred_type can be "prob" or "raw"
run_model <- function(model_object, lab_object)
{
  # print("Calling the base run_model function")
  UseMethod("run_model", model_object)
}


run_model.default <- function(model_object, lab_object)
{
  print("Wrong object type.")
}


run_model.Model <- function(model_object, lab_object){
  # Initiate the three lists which will receive the fits,
  # predictions and performance
  fit.list <- list()
  pred.list <- list()
  time.list <- list()
  
  # Get parameters from model_object
  formula_list <- model_object$Params$FormulaList
  args_caret_train <- model_object$Params$TrainArgs
  pred_type <- model_object$Params$RawProb
  
  # Get general parameters from lab_object
  data_test <- lab_object$Params$TestSet
  data_train <- lab_object$Params$TrainSet
  
  set.seed(lab_object$Params$Seed)
  
  # Set the training dataset and fit the model
  # 1) Without engineered features
  # 2) With balance error features only
  # 3) With all features
  tic("Train 3 models")
  for (i in 1:length(formula_list)){
    # Update args_caret_train with formula and data to run model
    args_train <- c(list("form" = as.formula(formula_list[[i]]))
                    , "data" = list(data_train), args_caret_train)
    
    # In order to allow running caret::train() on different lists of arguments
    # the base::do.call() function is used.
    # Call train on list of arguments args_caret_train
    fit <- do.call(train, args_train)
    
    # Update list of fits with training results
    fit.list[names(formula_list)[i]] <- list(fit)
  }
  time_to_train <- toc()
  time.list["TrainTime"] <- list(
    c("msg" = list(time_to_train$msg)
      , "time.sec" = list(time_to_train$toc-time_to_train$tic))
    )    
  
  # Run the 3 models on the test dataset to get predictions
  # Request probabilities with type = "prob" to make it possible to estimate PRAUC
  # Measure exectuation time and update time.list with elapsed time
  tic("Predict 3 models")
  for (i in 1:length(formula_list)){
    pred.list[names(formula_list)[i]] <- list(
      predict.train(fit.list[[i]], data_test, type = pred_type)
      )
  }
  time_to_predict <- toc()
  time.list["PredTime"] <- list(
    c("msg" = list(time_to_predict$msg)
      , "time.sec" = list(time_to_predict$toc-time_to_predict$tic))
    )
  
  # Update model_object with new attributes
  model_object["ModFits"] <- list(fit.list)
  model_object["Preds"] <- list(pred.list)
  model_object["ExecTimes"] <- list(time.list)
  
  # Return all information as a list
  return (model_object)
}


add_mod_to_lab <- function(model_object, lab_object){
  lab_object[[model_object$Params$ModName]] <- model_object
  return(lab_object)
}


# Define function measure_perf() that returns PR-AUC and the confusion matrix
# of a given model a list of formulas. Performance is measured using the predictions
# calculated based on the test set. Takes into account a classification threshold when 
# predictions are probabilities (as opposed to classes)
measure_perf <- function(model_object, lab_object, threshold = 0.5){
  
  # Get parameters from model_object
  model_name <- model_object$Params$ModName
  formula_list <- model_object$Params$FormulaList
  pred_type <- model_object$Params$RawProb 
  
  # Get parameters from lab_object
  data_test <- lab_object$Params$TestSet
  
  # Initiate performance list
  perf.list <- list()
  
  # Measure performance and update the performance list for each of
  # the three model versions
  for (i in 1:length(formula_list)){
    # 1) Calculate PRAUC performance measure
    PRAUC <- pr_auc(true_classes = data_test$isFraud
                    , pred_pc = model_object$Preds[[i]]
                    , score_type = pred_type
                    , include_curve = TRUE)
    
    # 2) Depending on pred_type, measure performance using either
    # probabilities or class
    if(pred_type == "prob"){
      # Confusion Matrix is calculated from probabilities with the threshold
      # passed as argument
      cm <- confusionMatrix(
        factor(ifelse(model_object$Preds[[i]]$F1 > threshold
                      , "F1"
                      , "G0")
               , levels = c("F1", "G0")
               , labels = c("F1", "G0"))
        , data_test$isFraud
        , positive = "F1"
        )
      Threshold <- threshold
    } else if(pred_type == "raw"){
      # Confusion Matrix is calculated from predicted classes
      cm <- confusionMatrix(model_object$Preds[[i]]
                            , data_test$isFraud
                            , positive = "F1")
      Threshold <- "None (class)"
    } else {
      warning("Error: only 'raw' or 'prob' possible.")
    }
    # Update perf.list with performance for formula i
    perf.list[names(formula_list)[i]] <- list(
      c("PRAUC" = list(PRAUC)
        , "ConfusionMatrix" = list(cm)
        , "Threshold" = list(Threshold))
      )
  }
  # Update model_object with performance
  model_object["Perfs"] <- list(perf.list)
  
  # Return model_object updated with performance
  return (model_object)
}


# Function to display variable importance
# Returns a ggplot graph object
plot_var_imp <- function(model_object,
                         model_version,
                         lab_env){
  var_imp <- cbind(
    Variable = row.names(varImp(model_object$ModFits[[model_version]])[[1]])
    , varImp(model_object$ModFits[[model_version]])[[1]]
    ) %>% 
    arrange(desc(Overall))
  
  row.names(var_imp) <- NULL
  
  # Set the ggplot object
  g <- var_imp %>% 
    ggplot(aes(x = Overall, y = reorder(Variable, Overall))) +
    geom_bar(color = "white", fill="lightblue", stat = "identity") +
    xlab("Importance") +
    ylab("") +
    ggtitle("Variable importance")
  return(g)
}


# Return the performance of a model when different thresholds are used for 
# classification. savePredictions=TRUE is required in trainControl() when 
# training the models since this function re-uses the probabilities computed 
# by train() to save time. These probabilities are saved under in 
# "ModFits[[model_version]]$pred" in the Model object
threshold_effect <- function(model_object, 
                             model_version,
                             lab_env,
                             th.lim = c(0.1, 0.9)){
  
  # Get variables from Model and LabEnv objects:
  model_method = model_object$Params$TrainArgs$method
  nb_repeats <- model_object$Params$TrainArgs$trControl$repeats
  nb_folds <- model_object$Params$TrainArgs$trControl$number
  
  threshold <- seq(from = th.lim[1], to = th.lim[2], by = 0.1)
  df <- data.frame("Threshold" = numeric()
                   , "Type" = factor(levels=c("FP", "FN"))
                   , "Count" = integer()
                   , "Rep_ID" = integer())
  
  # Extract the probabilities for the model and version for all
  # nb_fold and nb_repeats of the cross-validation process
  # Select tunning parameters of bestTune
  
  best_tune <- model_object$ModFits[[model_version]]$bestTune
  
  if(model_method == "rf"){
    best_mtry <- best_tune$mtry
    preds_df <- model_object$ModFits[[model_version]]$pred %>%
      filter(mtry == best_mtry)
  } else if(model_method == "xgbTree"){
    best_nrounds <- best_tune$nrounds
    best_maxdepth <- best_tune$max_depth
    best_eta <- best_tune$eta
    best_gamma <- best_tune$gamma
    best_colsample <- best_tune$colsample_bytree
    best_minchildweight <- best_tune$min_child_weight
    best_subsample <- best_tune$subsample

    preds_df <- model_object$ModFits[[model_version]]$pred %>% 
      filter(nrounds == best_nrounds 
             & max_depth == best_maxdepth 
             & eta == best_eta 
             & gamma == best_gamma 
             & colsample_bytree == best_colsample 
             & min_child_weight == best_minchildweight 
             & subsample == best_subsample)
  } else {
    warning("No acceptable model method found.")
  }
  
  # Create predictions based on probabilities and thresholds for all thresholds
  for(th in threshold){
    rep_ID <- 0
    # Loop through each rep of nb_repeats
    for(rep in seq(1, nb_repeats, 1)){
      rep_ID <- rep_ID + 1
      # Loop through each fold of nb_folds
      for(fold in seq(1, nb_folds, 1)){
        name_resample <- paste("Fold", fold, ".Rep", rep, sep="")
        # Extract probabilities
        F1_prob <- (preds_df %>% filter(Resample == name_resample))$F1
        # Extract observations
        obs <- (preds_df %>% filter(Resample == name_resample))$obs
        # Make predictions based on threshold value and save as factor
        preds_th <- factor(ifelse(F1_prob > th, "F1", "G0") 
                           , levels = c("F1", "G0")
                           , labels = c("F1", "G0"))
        # Binds observations and predictions and compute false positives 
        # and false negatives
        # When using binding, factors are lost, but factor indexes remain
        bind <- data.frame(cbind(preds_th, obs))
        # Save false positives (fp)
        fp <- nrow(bind %>% filter(preds_th == 1 & obs == 2))
        df <- df %>% add_row("Threshold" = th
                             , "Type" = "FP"
                             , "Count" = fp
                             , "Rep_ID" = rep_ID)
        # save false negatives (fn)
        fn <- nrow(bind %>% filter(preds_th == 2 & obs == 1))
        df <- df %>% add_row("Threshold" = th
                             , "Type" = "FN"
                             , "Count" = fn
                             , "Rep_ID" = rep_ID)
      }
    }
  }
  return(df)
}  


# Plot threshold effect
plot_th_effect <- function(dataframe, threshold.lim = c(0.1, 0.8)){
  
  df <- dataframe %>% 
    filter((Threshold >= threshold.lim[1]) 
           & (Threshold <= threshold.lim[2])) %>%
    mutate(Threshold = factor(Threshold))
  
  max_count <- max(df$Count)
  
  df %>% ggplot(aes(x = Threshold, y = Count, fill = Type)) +
    geom_dotplot(binaxis='y'
                 , method = "histodot"
                 , stackdir='center'
                 , binwidth = max_count/30
                 , dotsize= 0.8
                 , position=position_dodge(1)
                 , alpha = 1) +
    xlab("Threshold") +
    ylab("Count") +
    ggtitle("Number of FP and FN for different thresholds") +
    theme(legend.position = "bottom")
}


# Return confusion matrix based on probablity observations and class of reference
# for the specified threshold
preds_to_cm <- function(model_object,
                        model_version,
                        lab_env,
                        threshold){
  # Get the test set
  test_set <- lab_env$Params$TestSet
  
  # Get the predictions
  F1_prob <- model_object$Preds[[model_version]]$F1
  preds_th <- factor(ifelse(F1_prob > threshold, "F1", "G0") 
                     , levels = c("F1", "G0") 
                     , labels = c("F1", "G0"))
  
  obs <- test_set$isFraud
  
  # Using table calculates the confusion matrix
  df <- data.frame("Predictions" = preds_th 
                   , "Reference" = obs
                   , stringsAsFactors = FALSE)
  return(table(df))
}


# Delete the parts of ModFits which are not needed for the version of the
# models that is not "version_keep". Helps manage memory space.
shrink_model <- function(model_object,
                          version_keep){
  model_fits <- model_object$ModFits
  for (i in 1:length(model_fits)){
    if (names(model_fits)[[i]] != version_keep){
      # Remove less important details
      model_object$ModFits <- "Removed to reduce size."
    }
  }
  return(model_object)
}


# Returns the number of reclassifiable false positives for a specific model and version
# based on a specific threshold
get_reclass_FP <- function(model_object, 
                           model_version,
                           lab_env,
                           threshold = 0.5){
  # Get parameters from model and lab_env
  type <- model_object$Params$RawProb
  test_set <- lab_env$Params$TestSet
  
  pred <- model_object$Preds[[model_version]]
  new_df <- cbind(test_set, pred)
  # Let's see how many of the positives are of type payment, debit or cash-in
  if (type == "prob"){
    nfp <- nrow(new_df %>% 
                  filter(F1 > 0.5) %>% 
                  filter(type=="PAYMENT"|type=="CASH_IN" | type=="DEBIT"))
  } else if (type == "raw"){
    nfp <- nrow(new_df %>% 
                  filter(pred == "F1") %>% 
                  filter(type=="PAYMENT"|type=="CASH_IN" | type=="DEBIT"))    
  } else {
    warning(
      "Type argument error in get_reclass_FP(): only 'raw' or 'prob' expected.\n"
      )
  }
  return(nfp)
}


# Function to extract and return performance based on the name of the model
get_performance <- function(model_object, lab_env){
  # Create data frame that will be returned
  return_df <- data.frame("Features" = character() 
                          , "PRAUC" = numeric() 
                          , "Recall" = numeric()
                          , "Precision" = numeric()
                          , "Accuracy" = numeric()
                          , "F1" = numeric() 
                          , stringsAsFactors=FALSE)
  
  # Extract the relevant information from lab_env object
  for (i in 1:length(model_object$Perfs)){
    model_version <- names(model_object$Perfs)[i]
    prauc <- round(
      model_object$Perfs[[i]]$
        PRAUC$auc.integral, 5)
    recall <- round(
      model_object$Perfs[[i]]$
        ConfusionMatrix$byClass["Recall"], 5)
    precision <- round(
      model_object$Perfs[[i]]$
        ConfusionMatrix$byClass["Precision"], 5)
    accuracy <- round(
      model_object$Perfs[[i]]$
        ConfusionMatrix$overall["Accuracy"], 5)    
    F1_perf <- round(
      model_object$Perfs[[i]]$
        ConfusionMatrix$byClass["F1"], 5)
    return_df <- return_df %>%
      add_row(Features = model_version
              , PRAUC = prauc
              , Recall = recall
              , Precision = precision
              , Accuracy = accuracy
              , F1 = F1_perf)
  }
  return(return_df)
}


# Function to extract and return execution time based on the name of the model
# Execution time expressed in hours, minutes, seconds
# Returns a named list
get_exec_time <- function(model_object, lab_env){
  exec_time <- round(model_object$ExecTime$TrainTime$time.sec + 
                       model_object$ExecTime$PredTime$time.sec, 5)
  # Get numbers of hours by using %/% to get quotient
  exec_time_hour <- exec_time %/% 3600 
  # Get numbers of minutes using %% (modulo)
  exec_time_min <- (exec_time - exec_time_hour*3600) %/% 60
  # Get remaining seconds
  exec_time_sec <- (exec_time - exec_time_hour*3600) %% 60
  
  exec_time <- list("hour" = exec_time_hour 
                    , "min" = exec_time_min 
                    , "sec" = exec_time_sec)
  return (exec_time)
}


# Function to extract the confusion matrix based on the name of the model
# and it's option ("base", "err", "all" or "best" - will select the best model)
# Metric for selection of best model can be specified, default value: PRAUC
get_conf_matrix <- function(model_object,
                            model_option, 
                            lab_env, 
                            comparison_metric="PRAUC"){
  best_perf <- 0
  
  # If "best" is passed as model_option this loops finds the best version
  # using comparison_metric for the comparison
  if(model_option == "best"){
    for (i in 1:length(model_object$Perfs)){
      model_version <- names(model_object$Perfs)[i]
      
      # Get performance measure
      if(comparison_metric == "PRAUC"){
        perf_measure <- model_object$Perfs[[i]]$
          PRAUC$auc.integral
      } else if(comparison_metric == "Accuracy"){
        perf_measure <- model_object$Perfs[[i]]$
          ConfusionMatrix$overall[["Accuracy"]]        
      } else {
        perf_measure <- model_object$Perfs[[i]]$
          ConfusionMatrix$byClass[[comparison_metric]]        
      }
      
      # If performance version i better than previous, update model_option
      model_option <- ifelse(perf_measure > best_perf
                             , model_version
                             , model_option)
      # If performance version i better than previous, update performance
      best_perf <- ifelse(perf_measure > best_perf
                          , perf_measure
                          , best_perf)
    }
  }
  
  return (list("Version" = model_option
               , "Metric" = comparison_metric
               , "Table" = model_object$Perfs[[model_option]]$ConfusionMatrix$table))
}


# Function to display the performance of a model
# "comparison_metric" required to compare the versions to get
# the best version
# Returns the best version of the model

display_best_perf <- function(model_object, 
                              lab_env, 
                              comparison_metric){
  # Retrieve performance data
  print(get_performance(model_object, lab_env))
  
  # Dispaly execution time list
  time_list <- get_exec_time(model_object)
  cat("Execution time: " 
      , time_list[[1]], "h "
      , time_list[[2]], "m "
      , time_list[[3]], "s.\n" 
      , sep = "")
  
  # Display confusion matrix for the best version of the
  # specified model
  conf_matrix <- get_conf_matrix(model_object = model_object
                                , model_option = "best"
                                , lab_env = lab_env
                                , comparison_metric = comparison_metric)
  cat(
    "Best version: '"
    , conf_matrix[["Version"]]
    , "' when using '"
    , conf_matrix[["Metric"]]
    , "' as comparison metric.\n"
    , "Confusion matrix of best version: \n"
    , sep = ""
    )
  print(conf_matrix[["Table"]])
  return(conf_matrix[["Version"]])
}
