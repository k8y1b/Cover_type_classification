#Warning! This file takes an hour to run
#In this R file, we perform best subset variable selection on log transformed variables and binned transformed variables of the 
#dataset
library(rpart)
library("rpart.plot")

#set random seed
set.seed(251);

#The size of the training set for eventual training/holdout set evaluations
ntrain = 10000;

# create training and holdout set
data = readRDS("transformed_dataset.rds");
idatat = sample(1:nrow(data), ntrain);
datat = data[idatat,];
datah = data[-idatat, ]



#' @description fits an rpart model predicting Cover_Type based on every possible combination of explanatory variables in the provided data set
#' 
#' @return returns the output of the k_foldcv function, fitted at k = 5, for every combination of variables. 
#' I.e. returns Mean misclassification error, the classifiication rate for each class, and the average non-coverage
#' rate and size of 50 and 80 percent prediction intervals, number of explanatory variables included, and formula used 
#' for every combination of variables in the provided data frame.
#' return type is list
#' @param df a dataframe containing Cover_Type and any explanatory variables you wish to include in
#' your analysis

#' ATTN !!! This function will take hours to run on any non-trivial size dataset


varcomb <- function(df){
  #Initialise the dataframe to store kfoldcv results
  cvs <- read.csv(text = "misclaserr,class_misclasserr,size_50,size_80,error_50,error_80,func,numvars");
  
  
  for(i in 1:(length(names(df)) - 1)) {
    #select feature names
    vars = names(df);
    vars = vars[which(vars != "Cover_Type")];
    
    #all possible combination of features
    icomb = combn(vars, i);
    
    for(c in 1:ncol(icomb)){
      #produce a formula for model fitting for each possible combination of features
      f = c("Cover_Type ~ ")
      
      formulavars = icomb[,c];
      
      for(e in formulavars){
        if(length(f) == 1) {
          f = c(f, e);
        } else {
          f = c(f, "+", e);
        }
      }
      
      ff <- formula(paste(f, collapse = " "));
      #get cv results using the generated formula, see 01_functions.R for documentation
      cv <- kfold_cv(df, estimator = rpart, pred_type = "class", responsename = "Cover_Type", kfolds = 5, model_formula = ff);
      
      #save formula and number of included variables
      cv[["func"]] = deparse(ff);
      cv[["numvars"]] = i;
      
      #save cv results to dataframe, along with formula for model fitting and number of included variables
      cvs = rbind(cvs, t(cv));
      
    }
  }
  return(cvs);
}

#Apply all possible log or cube root transformations
nor <- data[, c(1:3, 7:9, 11, 13, 14, 17:20)];
#Get kfold_cv function results for all possible variable combinations
cvs = varcomb(nor);

#Apply all possible binning transformations
bin <- data[, c(1, 3, 7, 8, 9, 11, 13:15, 22:25)];
cvsbin = varcomb(bin)

#By manually examining these nor and bin dataframes, we can extract the best model for CV error as shown below

# Get training/holdout split evaluation for best model, for purposes of prediction interval frequencies.
bestt = rpart(Cover_Type ~ Elevation + Hillshade_Noon + Hillshade_3pm + Wilderness_Area + Aspect_Binned + Horizontal_Distance_To_Hydrology_Binned + Horizontal_Distance_To_Fire_Points_Binned + Horizontal_Distance_To_Roadways_Binned, data = datat);
predbest = predict(bestt, type = "prob", newdata = datah);
performance(predbest, datah$Cover_Type);

#Generate the classification tree for the best model.
rpart.plot(best, cex = 0.42, box.palett = 0, varlen = 16, yshift = -1, space = 0, gap = 0, split.yshift = -1.4)


#pruning experiment. Unfortunately, no pruning is effective.
mses = c();
mses2 = c()
cps = c(1, 0.1, 0.01, 0.001, 0.0001)
cps2 = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)
for(i in cps) {
  pruned = prune.rpart(bestt, i);
  pred = predict(pruned, type = "prob", newdata = datah);
  perf <- performance(pred, datah$Cover_Type);
  mses = c(mses, perf$MSE);
}
plot(log(cps), mses);
for(i in cps2) {
  p = prune.rpart(bestt, i)
  pred = predict(p, type = "prob", newdata = datah)
  perf = performance(pred, datah$Cover_Type)
  mses2 = c(mses2, perf$MSE)
}
plot(cps2, mses2)

