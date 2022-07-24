#Install packages
#install.packages("readxl")
#install.packages("ggplot2")
install.packages("neuralnet")
install.packages("Metrics")

#Import libraries
library(readxl)
library(neuralnet)
library(Metrics)
library(ggplot2)

dataset <- read_excel("UoW_load.xlsx")
View(dataset)

dataset$Dates <- as.numeric(dataset$Dates)

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

data_scaled <- as.data.frame(lapply(dataset, normalize))

#Training a NN model
datasetTrain <- data_scaled[1:430,]# the first 400 rows
colnames(datasetTrain) <- c("Dates","Nine","Ten","Eleven")
datasetTest <- data_scaled[431:500,]# the remaining rows
colnames(datasetTest) <- c("Dates","Nine","Ten","Eleven")

model <- neuralnet(Eleven ~ Dates+Nine+Ten, data=datasetTrain, hidden=c(5,2),linear.output=TRUE, threshold=0.01)
plot(model)

#Evaluation model performance
model1Result <- predict(model, datasetTest[1:3])
model1Result

minValue <-min(dataset$`11:00`)
maxValue <-max(dataset$`11:00`)

#Cre ate the reverse of normalised function - renormalized
removeNormalize <- function(x, min, max) {
  return ((max - min)*x + min )
}

deNormalizeData <- removeNormalize(model1Result,minValue,maxValue)
deNormalizeData

rmse(dataset$`11:00`[431:500], deNormalizeData)
mae(dataset$`11:00`[431:500], deNormalizeData)
mape(dataset$`11:00`[431:500], deNormalizeData)

ggplot(datasetTest, aes(x = deNormalizeData,
                        y = dataset$`11:00`[431:500]))+
  geom_point()+
  labs(x = "Predicted")+
  labs(y = "Actual")+
  geom_abline(
    intercept = 0,
    slope = 1,
    color = "red",
    size = 2
  )

ggplot(datasetTest, aes(x=(1:length(dataset$`11:00`[431:500])), y = deNormalizeData))+
  labs(y = "Value")+
  labs(x = "Index")+
  geom_line(color="chartreuse3")+
  geom_line(aes(y = dataset$`11:00`[431:500], group="supp"), color="red")+
  theme(legend.position="bottom")+
  geom_point()
