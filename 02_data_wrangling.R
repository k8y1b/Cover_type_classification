#This file reads in the data set provided by kaggle for training,
#applies data wrangling, performs transformations that
#we believe will be beneficial, and writes the new dataset
#to an rds file (transformed_dataset.rds)
library(tidyverse)

#Read in data, apply cosmetic transformations (combine split apart categorical variables into factors)
data = read_csv("train.csv")%>%
  pivot_longer(cols=starts_with("Soil_Type"),
               names_to = "Soil_Type",
               names_prefix="Soil_Type",
               values_to = "Present") %>%
  filter(Present==1)%>%
  select(!Present)%>%
  mutate(Soil_Type=factor(Soil_Type))%>%
  pivot_longer(cols=starts_with("Wilderness_Area"),
               names_to = "Wilderness_Area",
               names_prefix="Wilderness_Area",
               values_to = "Present") %>%
  filter(Present==1)%>%
  select(!Present)%>%
  mutate(Soil_Type=factor(Soil_Type),Cover_Type=factor(Cover_Type),Wilderness_Area=factor(Wilderness_Area)) %>%
  select(!Id)

#Soil Type Transformation
#Originally, the dataset had 40 different soil types
#Here, we keep the top 20 types, and the other types we
#assign to soil type 15 ("unspecified")
top_soil = data %>%
  group_by(Soil_Type)%>%
  summarise(n=n())%>%
  arrange(n)%>%
  top_n(20)

data$Soil_Type_Dropped = factor(data$Soil_Type,levels=1:40)
data$Soil_Type_Dropped[!(data$Soil_Type %in% top_soil$Soil_Type)]=15
data$Soil_Type_Dropped = factor(data$Soil_Type_Dropped,levels=unique(data$Soil_Type_Dropped))

rm(top_soil)

#Here we apply a binning transformation to the aspect variable,
#transforming compass degrees to cardinal directions
data$Aspect_Binned = factor(sapply(data$Aspect,function(X){
  if(X>=360-22.5 | X<22.5){
    return("N")
  }
  if(X>=22.5 & X<67.5){
    return("NE")
  }
  if(X>=67.5 & X<112.5){
    return("E")
  }
  if(X>=112.5 & X<157.5){
    return("SE")
  }
  if(X>=157.5 & X<202.5){
    return("S")
  }
  if(X>=202.5 & X<247.5){
    return("SW")
  }
  if(X>=247.5 & X<292.5){
    return("W")
  }
  if(X>=292.5 & X<337.5){
    return("NW")
  }
}))

#Here we apply a "rotation" to the aspect variable,
#moving the compass degrees so their distribution are less bimodel, and more normal-looking
data = data %>%
  mutate(Aspect_Transformed = Aspect - 270)%>%
  mutate(Aspect_Transformed=if_else(Aspect_Transformed<0,Aspect_Transformed+360,Aspect_Transformed))

#Here we apply log transformations to the horizontal distance variables, and a cube root transformation to
#the vertical distance to hydrology variable
#The vertical distance to hydrology can be negative, so we define a new cube root function that
#preserves the sign of the original value.
#These values also contain 0, so we add 1 to the horizontal distance measurements before taking the
#log transformation
cube_root = function(X){
  sign(X)*abs(X)^(1/3)
}

data = data %>%
  mutate(log_Horizontal_Distance_To_Hydrology=log(Horizontal_Distance_To_Hydrology+1))%>%
  mutate(log_Horizontal_Distance_To_Fire_Points=log(Horizontal_Distance_To_Fire_Points+1))%>%
  mutate(log_Horizontal_Distance_To_Roadways=log(Horizontal_Distance_To_Roadways+1))%>%
  mutate(cube_Vertical_Distance_To_Hydrology=cube_root(Vertical_Distance_To_Hydrology))

#Here we combine the three hillshade variables (measurements at 9am, noon, and 3pm) into 
#one variable, by taking their average, to potentially improve prediction through
#feature reduction
data = data %>%
  rowwise()%>%
  mutate(Hillshade_Average = mean(Hillshade_9am+Hillshade_Noon+Hillshade_3pm))


#Here we apply binning transformations to the distance variables to reduce skew
#We apply the transformations based on the quantiles present in the data, into 5 sections
data$Horizontal_Distance_To_Hydrology_Binned = 
           cut(data$Horizontal_Distance_To_Hydrology,quantile(data$Horizontal_Distance_To_Hydrology,probs=seq(0,1,0.2)),include.lowest=TRUE)
data$Horizontal_Distance_To_Fire_Points_Binned = 
           cut(data$Horizontal_Distance_To_Fire_Points,quantile(data$Horizontal_Distance_To_Fire_Points,probs=seq(0,1,0.2)),include.lowest=TRUE)
data$Horizontal_Distance_To_Roadways_Binned = 
           cut(data$Horizontal_Distance_To_Roadways,quantile(data$Horizontal_Distance_To_Roadways,probs=seq(0,1,0.2)),include.lowest=TRUE)
data$Vertical_Distance_To_Hydrology_Binned = 
         cut(data$Vertical_Distance_To_Hydrology,quantile(data$Vertical_Distance_To_Hydrology,probs=seq(0,1,0.2)),include.lowest=TRUE)

#Write the new dataset with all transformations included to an rds file.
write_rds(data,"transformed_dataset.rds")