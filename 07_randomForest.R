#Random Forests
#Here we apply random forests models to the dataset

#The code in this file will take about 50 minutes to run

library(randomForest)
library(tidyverse)

set.seed(202122447)

#Load Functions
load("functions.RData")

#load data

data = read_rds("transformed_dataset.rds")



#First we fit a full random Forest model

randomForestFull = randomForest(formula = Cover_Type ~., data = data, importance = TRUE)

#Create list to hold model outcomes
models = list()

#Review variable importance plot to note variables to be used. 
varImpPlot(randomForestFull, main = "Random Forest Model with all variables, Importance")

#With selected variables the subsets are evaluated using 5-fold cross validation

#Form functions based on the variable importance
untransformed = "Cover_Type ~ Elevation + 
                 Soil_Type + 
                 Wilderness_Area + 
                 Horizontal_Distance_To_Roadways + 
                 Horizontal_Distance_To_Fire_Points + 
                 Aspect + 
                 Hillshade_9am + 
                 Hillshade_Noon + 
                 Horizontal_Distance_To_Hydrology + 
                 Vertical_Distance_To_Hydrology + 
                 Hillshade_3pm + 
                 Slope"

models[1] = untransformed

combined = "Cover_Type ~ Elevation + 
                         Aspect_Transformed + 
                         log_Horizontal_Distance_To_Hydrology + 
                         Horizontal_Distance_To_Roadways+ 
                         log_Horizontal_Distance_To_Roadways +  
                         Horizontal_Distance_To_Fire_Points+ 
                         log_Horizontal_Distance_To_Fire_Points + 
                         Soil_Type + Wilderness_Area + Hillshade_9am +
                         Hillshade_3pm + Hillshade_Noon + Slope" 
                         
models[2] = combined
top5 = "Cover_Type ~ Elevation + 
        Soil_Type + 
        Soil_Type_Dropped + 
        Wilderness_Area + 
        Horizontal_Distance_To_Roadways"

models[3] = top5
top10 = "Cover_Type ~ Elevation + 
         Soil_Type + 
         Soil_Type_Dropped + 
         Wilderness_Area + 
         Horizontal_Distance_To_Roadways + 
         log_Horizontal_Distance_To_Roadways +
         Horizontal_Distance_To_Fire_Points + 
         log_Horizontal_Distance_To_Fire_Points + 
         log_Horizontal_Distance_To_Hydrology + 
         Aspect_Transformed"
models[4] = top10

models[5] = "Cover_Type ~."
results = list()


#Fit 5 models based on formulas above and use CV to obtain performance measures
for (i in 1:length(models)) {

    results[[i]] = kfold_cv(data = data, model_formula = formula(models[[i]]), 
                 predictor = predict, estimator = randomForest, pred_type = "class")
  
}


res = cbind(models, results)



#Save results for all models
write_rds(x = results, "randomForestResults.rds")


#Split into train & test sets for final fitting



best = randomForest(formula = formula(top10), data = dataTrain, importance = TRUE)


#Save best performing random forest model
write_rds(x = best, "bestRF.rds")

