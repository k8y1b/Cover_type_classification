library(xgboost)
library(tidyverse)
library(data.table)
library(Matrix)

#In this file, we apply gradient boosting using the xgboost library.
#First, we use the default settings and training/holdout performance
#to identify the best candidate.
#Then, we use cross validation on the best candidate to select the best
#paramater set for xgboost, creating the best possible predictor, and evaluate its performacne

#WARNING! This code takes several hours to run


#' K-fold Cross Validation Function for classification tasks, only for xgboost gradient boosting
#' with tree-based multiclass classification. This function must be passed various parameters for the
#' xgboost function to apply gradient boosting.
#' 
#' @description 
#' K-Fold CV for classification with xgboost for multi-class classification
#' 
#' @return 
#' Returns Mean misclassification error, the classifiication rate for each class, and the average non-coverage
#' rate and size of 50 and 80 percent prediction intervals.
#' 
#' @param data Data to be used for fitting 
#' @param estimator takes a name of a function used to fit a model i.e. estimator = RandomForest  or vglm or 
#' @param model_formula formula to be used in model fitting, as a string, for our task defaults to Cover_Type~. 
#' @param kfolds number of folds in CV, default is 5
#' @param resposename name of response variable in data
#' @param nrounds the value for the nround parameter used by xgboost
#' @param eta the value for the eta paramater used by xgboost
#' @param gamma the gamma parameter used by xgboost
#' @param max_depth the max_depth parameter used by xgboost            

kfold_cv_xgb <- function(data, model_formula = "Cover_Type~.-1", kfolds = 5, responsename="Cover_Type",
                     nrounds, eta, gamma, max_depth) {
  
  labels = levels(unlist(data[,responsename]))
  n <- nrow(data) # number of rows in the dataset
  fold.labels = sample(rep(1:kfolds, length.out = n))
  misclaserr = double(kfolds) #Store Misclassification error for each fold. 
  class_misclasserr = matrix(rep(0.0,kfolds*length(labels)),ncol=length(labels)) #Misclassification errors for each class for each fold
  colnames(class_misclasserr) = labels
  size_50 = matrix(rep(0.0,kfolds*length(labels)),ncol=length(labels)) #Average 50% interval size for each class for each fold
  colnames(size_50) = labels
  size_80 = matrix(rep(0.0,kfolds*length(labels)),ncol=length(labels)) #Average 80% interval size for each class for each fold
  colnames(size_80) = labels
  error_50 = matrix(rep(0.0,kfolds*length(labels)),ncol=length(labels)) #50% interval coverage for each class for each fold
  colnames(error_50) = labels
  error_80 = matrix(rep(0.0,kfolds*length(labels)),ncol=length(labels)) #80% interval coverage for each class for each fold
  colnames(error_80) = labels
  
  #actual k-fold evaluation
  for (fold in 1:kfolds) {
    test.rows = which(fold.labels == fold)
    train = data[-test.rows, ]
    test = data[test.rows, ]
    
    train_data = sparse.model.matrix(model_formula,data=train)
    train_label = as.numeric(unlist(train[,responsename]))-1
    test_data = sparse.model.matrix(model_formula,data=test)
    
    #fit model using xgboost
    model_fit = xgboost(data=train_data,
                              label=train_label,
                              nround=nrounds,
                              objective="multi:softprob",
                              num_class=7,
                              eta=eta,
                              gamma=gamma,
                              max_depth=max_depth,
                        verbose = 0,
                        eval_metric="mlogloss"
                        )
    
    
    predictionsMatrix =  t(matrix(predict(model_fit,test_data),nrow=length(labels)))
    predictions = max.col(predictionsMatrix)
    
    test_responses = unlist(test[, responsename])
    
    #see documentation in 01_functions.R
    performance_measures = performance(predictionsMatrix, test_responses)
    
    
    misclaserr[fold] = mean(test_responses != predictions)
    class_misclasserr[fold,] = performance_measures$class_errors
    size_50[fold,] = performance_measures$size_50
    size_80[fold,] = performance_measures$size_80
    error_50[fold,] = performance_measures$error_50
    error_80[fold,] = performance_measures$error_80
    
    print(paste("Fold " , fold, "Misclassification error is: ", misclaserr[fold]))
    
  }
  return(list(
    misclaserr = mean(misclaserr), #misclassification error
    class_misclasserr = colMeans(class_misclasserr), #misclassification error for each class
    size_50 = colMeans(size_50), #50% interval size for each class (average)
    size_80 = colMeans(size_80), #80% interval size for each class (average)
    error_50 = colMeans(error_50), #50% interval error rates 
    error_80 = colMeans(error_80) #80% interval error rates
  ))
  
}

#' A simple function for fitting a multi-class tree-based gradient boosting model using xgboost,
#' and then evaluating its performance using a training/holdout split. Uses the default xgboost function
#' parameters, but with an nround value of 100. Returns performance measures using the performance function,
#' see 01_functions.R for documentation.
#' @description A function for evaluating a multiclass tree-based gradient boosting model with xgboost
#' with default parameters using a training/holdout split.
#' @param formula the formula for the model
#' @param training the training set to fit the model to
#' @param holdout the holdout set to evaluate the model fit
#' @param response the name of the response variable
#' @return the results of the performance function, which includes out of sample misclassification rate for each class
#' and overall average, and the average sizes, frequencies, and non-coverage rates of 50 and 80 percent
#' prediction intervals. See 01_functions.R for documentation.
out_of_sample_evaluation = function(formula,training,holdout,response){
  training_data = training_data = sparse.model.matrix(formula,data=training)
  training_label = as.numeric(unlist(training[,response]))-1
  holdout_data = sparse.model.matrix(formula,data=holdout)
  holdout_label = unlist(holdout[,response])
  
  xgb = xgboost(data=training_data,
                label=training_label,
                nround=100,
                objective="multi:softprob",
                num_class=7,
                verbose=0)
  prob_matrix = t(matrix(predict(xgb,holdout_data),nrow=7))
  return(performance(prob_matrix,holdout_label))
}

set.seed(123456)

#Load in data and required evaluation functions, and create a training/holdout split
load("functions.RData")
data = read_rds("transformed_dataset.rds")

sample_indices=sample(1:nrow(data),nrow(data)/2)
training=data[sample_indices,]
holdout=data[setdiff(1:nrow(data),sample_indices),]

#Here we fit various combinations of transformed/untransformed variables using the default xgboost parameters,
#and evaluate their performance using a training holdout split, in order to identify a possible
#good model

#First, define the formulas

#All features, no transformations (except to soil type)
full_model_formula= Cover_Type ~ Elevation+
  Aspect+Slope+Horizontal_Distance_To_Hydrology+
  Vertical_Distance_To_Hydrology+
  Horizontal_Distance_To_Roadways+
  Hillshade_9am+Hillshade_Noon+
  Hillshade_3pm+Horizontal_Distance_To_Fire_Points+
  Soil_Type_Dropped+Wilderness_Area-1

#Apply binning transformations to distance measurements
binned_distances_formula = Cover_Type ~ Elevation+
  Aspect+Slope+Horizontal_Distance_To_Hydrology_Binned+
  Vertical_Distance_To_Hydrology_Binned+
  Horizontal_Distance_To_Roadways_Binned+
  Hillshade_9am+Hillshade_Noon+
  Hillshade_3pm+Horizontal_Distance_To_Fire_Points_Binned+
  Soil_Type_Dropped+Wilderness_Area -1

#Apply log transformatiosn to distance measurements
log_distances_formula = Cover_Type ~ Elevation+
  Aspect+Slope+log_Horizontal_Distance_To_Hydrology+
  cube_Vertical_Distance_To_Hydrology+
  log_Horizontal_Distance_To_Roadways+
  Hillshade_9am+Hillshade_Noon+
  Hillshade_3pm+log_Horizontal_Distance_To_Fire_Points+
  Soil_Type_Dropped+Wilderness_Area -1

#Take the average of the three hillshade measurements (at 9am, noon, and 3pm) for each observation.
#Replace the three variables with this one average (Hillshade_Average)
hillshade_average_formula = Cover_Type~ Elevation+
  Aspect+Slope+Horizontal_Distance_To_Hydrology+
  Vertical_Distance_To_Hydrology+
  Horizontal_Distance_To_Roadways+
  Hillshade_Average+Horizontal_Distance_To_Fire_Points+
  Soil_Type_Dropped+Wilderness_Area-1

#Bin the aspect variable
aspect_binned_formula = Cover_Type ~ Elevation+
  Aspect_Binned+Slope+Horizontal_Distance_To_Hydrology+
  Vertical_Distance_To_Hydrology+
  Horizontal_Distance_To_Roadways+
  Hillshade_9am+Hillshade_Noon+
  Hillshade_3pm+Horizontal_Distance_To_Fire_Points+
  Soil_Type_Dropped+Wilderness_Area-1

#Apply a "rotation" transformation to the aspect variable, see 02_data_wrangling.R or the report
#for reasons why
aspect_rotated_formula = Cover_Type  ~ Elevation+
  Aspect_Transformed+Slope+Horizontal_Distance_To_Hydrology+
  Vertical_Distance_To_Hydrology+
  Horizontal_Distance_To_Roadways+
  Hillshade_9am+Hillshade_Noon+
  Hillshade_3pm+Horizontal_Distance_To_Fire_Points+
  Soil_Type_Dropped+Wilderness_Area-1

#Then, we fit default xgboost models as describe above, and evaluate out of sample prediction error
full_model = out_of_sample_evaluation(full_model_formula,training = training,holdout=holdout,response = "Cover_Type")
binned_distances = out_of_sample_evaluation(binned_distances_formula,training = training,holdout=holdout,response = "Cover_Type")
log_distances = out_of_sample_evaluation(log_distances_formula,training = training,holdout=holdout,response = "Cover_Type")
hillshade_average = out_of_sample_evaluation(hillshade_average_formula,training = training,holdout=holdout,response = "Cover_Type")
aspect_binned = out_of_sample_evaluation(aspect_binned_formula,training = training,holdout=holdout,response = "Cover_Type")
aspect_rotated = out_of_sample_evaluation(aspect_rotated_formula,training = training,holdout=holdout,response = "Cover_Type")

print(full_model$MSE)
print(binned_distances$MSE)
print(log_distances$MSE)
print(hillshade_average$MSE)
print(aspect_binned$MSE)
print(aspect_rotated$MSE)

#Appears no transformation to distances, taking average of hillshade, and binning aspect work best

#Define the "best" model implied by training/holdout split performance evaluation
best_model_formula = Cover_Type ~ Elevation+
  Aspect_Binned+Slope+Horizontal_Distance_To_Hydrology+
  Vertical_Distance_To_Hydrology+
  Horizontal_Distance_To_Roadways+
  Hillshade_Average+Horizontal_Distance_To_Fire_Points+
  Soil_Type_Dropped+Wilderness_Area-1

#Here, we select the best value for each of the xgboost parameters using cross validation, 
#from a given range
#Not comprehensive, because we are limited by computational power

#The xgboost tuning parameters and the values we will test
etas = c(0.1,0.3,0.5)
gammas = c(0,3,6)
nrounds=c(100,300,500)
max_depths=c(6,9,12)

#Here we fit models with the "best" formula to each combination of the defined parameter values for the xgboost
#function, evaluate their average 5-fold CV prediction error, and the average misclassification error variance
#across classes to select the most evenly-performing model (does not favour a given class)
cv_results = tibble(
  eta=numeric(),
  gamma=numeric(),
  nround=numeric(),
  max_depth=numeric(),
  error=numeric(),
  var_error=numeric()
)
for(eta in etas){
  for(gamma in gammas){
    for(nround in nrounds){
      for(max_depth in max_depths){
        #gett cv measurements
        results = kfold_cv_xgb(data = data,model_formula = best_model_formula,
                                  nrounds = nround, eta=eta,gamma = gamma,max_depth = max_depth)
        #save results
        cv_results <- cv_results %>%
          add_row(
            eta=eta,
            gamma=gamma,
            nround=nround,
            max_depth=max_depth,
            error=results$misclaserr,
            var_error=var(results$class_misclasserr)
          )
      }
    }
  }
}
#write results to a csv file
write_csv(cv_results,"cv_results.csv")

#The best performing model had xgboost parameters:
#nrounds=500,
#eta=0.3,
#gamma=0,
#max_depth=12

#Since we did not save all the performance measurements for each model, we refit
#the "best" model with these xgboost parameters and apply 5-fold cv and training/holdout evaluation in 
#order to gain:
#average cv error
#cv error for each class individually
#average size, non-coverage rate, and frequency (from training/holdout evaluation) of 50 and 80% prediction intervals
#fore each class

set.seed(2354)

#apply 5-fold cv to gain performance measures
best_cv_results = kfold_cv_xgb(data = data,model_formula = best_model_formula,
                               nrounds = 500, eta=0.3,gamma = 0,max_depth = 12)

#apply training/holdout evaluation in order to gain prediction interval frequencies
training_data = training_data = sparse.model.matrix(best_model_formula,data=training)
training_label = as.numeric(unlist(training[,"Cover_Type"]))-1
holdout_data = sparse.model.matrix(best_model_formula,data=holdout)
holdout_label = unlist(holdout[,"Cover_Type"])

best_training_fit = xgboost(data=training_data,
                            label=training_label,
                            nrounds=500,
                            eta=0.3,
                            gamma=0,
                            max_depth=12,
                            verbose = 0,
                            eval_metric="mlogloss",
                            num_class=7,
                            objective="multi:softprob"
                            )
prob_matrix =  t(matrix(predict(best_training_fit,holdout_data),nrow=7))
performance =performance(prob_matrix,holdout$Cover_Type)

#Here we produce a variable importance plot for the "best" xgboost model
importance_matrix = xgb.importance(dimnames(training_data)[[2]],model=best_training_fit)
xgb.plot.importance(importance_matrix,top_n=20)

#save environment for posterity
save.image("xgboost.RData")


