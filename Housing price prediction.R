library(stringr)
library(ggplot2)
library(dplyr)
library(mice)
library('reshape2')

# Import file using IMPORT DATASET command on top right panel
file <- read.csv("D:/Data Visualisation/Week 4/Housing Price Prediction/Housing Price prediction.csv")

#View the dataframe
data<-as.data.frame(file)

# to find data types of all columns in the data frame
sapply(data, class)

# to find the missing values in the data frame
apply(is.na(data), 2, which)

#View data
data

#imputing the NA values using mice package 
imp_mice<-mice(Readfile,m=2)
complete(imp_mice)

#convert numerics for coorelation
final_data<-complete(imp_mice)
final_data[,'Street']<-as.numeric(final_data[,'Street'])
final_data[,'Alley']<-as.numeric(final_data[,'Alley'])
final_data[,'LotShape']<-as.numeric(final_data[,'LotShape'])
final_data[,'LandContour']<-as.numeric(final_data[,'LandContour'])
final_data[,'Utilities']<-as.numeric(final_data[,'Utilities'])
final_data[,'LotConfig']<-as.numeric(final_data[,'LotConfig'])
final_data[,'LandSlope']<-as.numeric(final_data[,'LandSlope'])
final_data[,'Neighborhood']<-as.numeric(final_data[,'Neighborhood'])
final_data[,'Condition1']<-as.numeric(final_data[,'Condition1'])
final_data[,'Condition2']<-as.numeric(final_data[,'Condition2'])
final_data[,'BldgType']<-as.numeric(final_data[,'BldgType'])
final_data[,'HouseStyle']<-as.numeric(final_data[,'HouseStyle'])
final_data[,'Fence']<-as.numeric(final_data[,'Fence'])
final_data[,'MiscFeature']<-as.numeric(final_data[,'MiscFeature'])
final_data[,'SaleType']<-as.numeric(final_data[,'SaleType'])
final_data[,'SaleCondition']<-as.numeric(final_data[,'SaleCondition'])

corr <- round(cor(final_data), 1)
melted_corr <- melt(corr)
head(melted_corr)

#[9] ggplot the coorelations
ggplot(data = melted_corr, aes(x=Var1, y=Var2, fill=value)) + geom_tile()

#upper triangle
# Get upper triangle of the correlation matrix
get_upper_tri <- function(corr){
  corr[lower.tri(corr)]<- NA
  return(corr)
}
upper_tri <- get_upper_tri(corr)

# Melt the correlation matrix
library(reshape2)
melted_corr <- melt(upper_tri, na.rm = TRUE)
# Heatmap
library(ggplot2)
ggplot(data = melted_corr, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="NHANES\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()
