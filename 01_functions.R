#Here we make two functions for evaluating prediction performance
#The first function evaluates out of sample performance by generating prediction interval
#and misclassification statistics
#The second function performs similar analyses, but uses kfold cross validation

#Afterwards, we write the functions to an RData file for sharing.

#' Function for generating 50 and 80 percent prediction interval statistics, including the 
#' table of prediction intervals themselves, their average lengths, their misclassification rates, the 
#' prediction errors of each class, and the overall misclassification error.
#' 
#' @description Function for generating 50 and 80 percent prediction interval statistics
#' 
#' @return The average prediction interval size, the interval misclassification rates,
#' the prediction errors for each class, and the overall misclassification error, for both 50 and 80
#' percent prediction intervals, all encoded as separate values in a list object.
#' @param ProbMatrix A matrix of prediction probabilities. The columns for classes must be in the same order
#' as the labels parameter for class labels
#' @param actual The actual class for each observation in the ProbMatrix
#' @param labels The labels for each class. 
performance <- function(ProbMatrix,actual,labels=as.character(1:7)){
  #code from notes, gets 50 and 80 percent prediction intervals
  ncases=nrow(ProbMatrix)
  pred50=rep(NA,ncases); pred80=rep(NA,ncases)
  for(i in 1:ncases){ 
    p=ProbMatrix[i,]
    ip=order(p)
    pOrdered=p[ip] # increasing order
    labelsOrdered=labels[rev(ip)] # decreasing order
    G=rev(cumsum(c(0,pOrdered))) # cumulative sum from smallest
    k1=min(which(G<=0.5))-1 # 1-level1= 1-0.5=0.5
    k2=min(which(G<=0.2))-1 # 1-level2= 1-0.8=0.2
    pred1=labelsOrdered[1:k1]; pred2=labelsOrdered[1:k2]
    pred50[i]=paste(pred1,collapse="")
    pred80[i]=paste(pred2,collapse="")
  }
  
  
  #Number of observations for interval sizes and interval coverage, for both 50 and 80 percent intervals
  num_50_for_size = rep(0,length(labels))
  num_80_for_size = rep(0,length(labels))
  num_50_for_freq = rep(0,length(labels))
  num_80_for_freq = rep(0,length(labels))
  
  #interval sizes for 50 and 80 percent intervals
  size_50 = rep(0,length(labels))
  size_80 = rep(0,length(labels))
  
  #number correctly classified by 50 and 80 percent intervals
  freq_50 = rep(0,length(labels))
  freq_80 = rep(0,length(labels))
  
  #apply to all observations
  for(i in 1:length(actual)){
    #get components of prediction intervals
    components50=unlist(strsplit(pred50[i],""))
    components80=unlist(strsplit(pred80[i],""))

    indexes50=which(labels %in% components50)
    indexes80=which(labels %in% components80)

    
    index_actual=which(labels == actual[i])
    
    
    num_50_for_freq[index_actual]=num_50_for_freq[index_actual]+1
    num_80_for_freq[index_actual]=num_80_for_freq[index_actual]+1
    num_50_for_size[indexes50]=num_50_for_size[indexes50]+1
    num_80_for_size[indexes80]=num_80_for_size[indexes80]+1
    
    if(!(actual[i] %in% components50)){
      freq_50[index_actual]=freq_50[index_actual]+1
    }
    if(!(actual[i] %in% components80)){
      freq_80[index_actual]=freq_80[index_actual]+1
    }
    
    size_50[indexes50]=size_50[indexes50]+length(indexes50)
    size_80[indexes80]=size_80[indexes80]+length(indexes80)
  }
  
  #Get prediction error
  predictions=apply(ProbMatrix,1,function(X){
    return(labels[which.max(X)])
  })
  MSE=mean(predictions!=actual)
  not_covered=(predictions!=actual)
  class_errors = tapply(not_covered,actual,mean) 

  return(list(
    #Confidence interval tables
    confint_50=table(actual,pred50),
    confint_80=table(actual,pred80),
    #Average interval sizes
    size_50=size_50/num_50_for_size,
    size_80=size_80/num_80_for_size,
    #Error rates for intervals
    error_50=freq_50/num_50_for_freq,
    error_80=freq_80/num_80_for_freq,
    #Mean prediction misclassification
    MSE=MSE,
    #Mean misclassification for each class
    class_errors=class_errors
  ))
}


#' K-fold Cross Validation Function for classification tasks.
#' 
#' @description 
#' K-Fold CV for classification
#' 
#' @return 
#' Returns Mean misclassification error, the classifiication rate for each class, and the average non-coverage
#' rate and size of 50 and 80 percent prediction intervals.
#' 
#' @param data Data to be used for fitting 
#' @param estimator takes a name of a function used to fit a model i.e. estimator = RandomForest  or vglm or 
#' @param model_formula formula to be used in model fitting, as a string, for our task defaults to Cover_Type~. 
#' @param predictor function name to be used for predictions, defaults to predict function
#' @param kfolds number of folds in CV, default is 5
#' @param resposename name of response variable in data
#' @param pred_type type in predictor function
#' @param ... all dditional paramaters to be passed to the appropriate estimator function. 
#'            

kfold_cv <- function(data, estimator, model_formula = "Cover_Type~.", 
  predictor = predict, pred_type, kfolds = 5, responsename="Cover_Type",...) {

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
  
  
  for (fold in 1:kfolds) {
    test.rows = which(fold.labels == fold)
    train = data[-test.rows, ]
    test = data[test.rows, ]
    
    model_fit = estimator(formula = formula(model_formula), data = train, ...)
    
    if (pred_type == "response"){
        predictionsMatrix = predictor(model_fit, newdata = test, type = as.character(pred_type))
        predictions = max.col(predictionsMatrix) 
    }
    else { 
      predictions = predictor(model_fit, newdata = test, type = as.character(pred_type))
      predictionsMatrix = predictor(model_fit, newdata=test,type="prob")
    }
        
    test_responses = unlist(test[, responsename])

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


save.image("functions.RData")