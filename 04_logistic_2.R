#Here we fit an additional multinomial logistic regression model.
#This was done to experiment with interpretability, and ended up with
#unexpected high performance.

#WARNING! This file takes about an hour to run, as vglm is very slow

set.seed(24356)
library(tidyverse)
library(VGAM)

#Load data and performance functions, create training/holdout split
load("functions.RData")
data = read_rds("transformed_dataset.rds")

sample_indices=sample(1:nrow(data),nrow(data)/2)
training=data[sample_indices,]
holdout=data[setdiff(1:nrow(data),sample_indices),]

#Fit a vglm model using the suspected model
distances_log_model = vglm(Cover_Type~Elevation+Soil_Type_Dropped+Wilderness_Area+
  log_Horizontal_Distance_To_Roadways+
  log_Horizontal_Distance_To_Hydrology+
  log_Horizontal_Distance_To_Fire_Points+
  cube_Vertical_Distance_To_Hydrology+
    Hillshade_9am+Hillshade_Noon+Hillshade_3pm+
    Aspect_Binned,family=multinomial(),data=training)

#write the model to an rds object
write_rds(distances_log_model,"for_interpretation.rds")

#generate training/holdout evaluations, see 01_functions.R for documentation
predictions_matrix = predict(distances_log_model,newdata=holdout,type="response")
holdout_performance = performance(predictions_matrix,holdout$Cover_Type)

#Training/holdout performance unexpectedly good, so we perform 5-fold cross validation
cv_results = kfold_cv(data,vglm,model_formula= Cover_Type~Elevation+Soil_Type_Dropped+Wilderness_Area+
           log_Horizontal_Distance_To_Roadways+
           log_Horizontal_Distance_To_Hydrology+
           log_Horizontal_Distance_To_Fire_Points+
           cube_Vertical_Distance_To_Hydrology+
           Hillshade_9am+Hillshade_Noon+Hillshade_3pm+
           Aspect_Binned,
         pred_type="response",
         family=multinomial()
         )

#Save environment
save.image("interpret_logit.RData")

