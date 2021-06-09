library(dplyr)
flight<-read.csv("C:\\Users\\nikhi\\Desktop\\flight.csv")   #input the csv i have uploaded
flight
summary(flight)
origData<-read.csv2("C:\\Users\\nikhi\\Desktop\\flight.csv",sep=",",header=TRUE,stringsAsFactors = FALSE)
airports<-c("ATL","LAX","ORD","DFW","JFK","SFO","CLT","LAS","PHX")
origData<-subset(origData,DEST %in% airports & ORIGIN %in% airports )

head(origData,2)
tail(origData,2)

origData$X<-NULL
cor(origData[c("ORIGIN_AIRPORT_SEQ_ID","ORIGIN_AIRPORT_ID")])
cor(origData[c("DEST_AIRPORT_SEQ_ID","DEST_AIRPORT_ID")])

origData$ORIGIN_AIRPORT_SEQ_ID<-NULL
origData$DEST_AIRPORT_SEQ_ID<-NULL

mismatched<-origData[origData$OP_CARRIER!=origData$OP_UNIQUE_CARRIER,]
nrow(mismatched)

origData$OP_UNIQUE_CARRIER<-NULL

onTimeData<-origData[!is.na(origData$ARR_DEL15)&origData$ARR_DEL15!=""&!is.na(origData$DEP_DEL15)&origData$DEP_DEL15!="",]

nrow(origData)
nrow(onTimeData)

onTimeData$DISTANCE<-as.integer(onTimeData$DISTANCE)
onTimeData$CANCELLED<-as.integer(onTimeData$CANCELLED)
onTimeData$DIVERTED<-as.integer(onTimeData$DIVERTED)

onTimeData$ARR_DEL15<-as.factor(onTimeData$ARR_DEL15)
onTimeData$DEP_DEL15<-as.factor(onTimeData$DEP_DEL15)
onTimeData$DEST_AIRPORT_ID<-as.factor(onTimeData$DEST_AIRPORT_ID)
onTimeData$ORIGIN_AIRPORT_ID<-as.factor(onTimeData$ORIGIN_AIRPORT_ID)
onTimeData$DAY_OF_WEEK<-as.factor(onTimeData$DAY_OF_WEEK)
onTimeData$DEST<-as.factor(onTimeData$DEST)
onTimeData$ORIGIN<-as.factor(onTimeData$ORIGIN)
onTimeData$DEP_TIME_BLK<-as.factor(onTimeData$DEP_TIME_BLK)
onTimeData$OP_CARRIER<-as.factor(onTimeData$OP_CARRIER)

tapply(onTimeData$ARR_DEL15,onTimeData$ARR_DEL15,length)

library(caret)    #library to analyze the delay
set.seed(122515)
featureCols<-c("ARR_DEL15","DAY_OF_WEEK","DEST","ORIGIN","DEP_TIME_BLK")
onTimeDataFiltered<-onTimeData[,featureCols]

inTrainRows<-createDataPartition(onTimeDataFiltered$ARR_DEL15,p=0.7, list= FALSE)
onTimeData
trainDataFiltered<-onTimeDataFiltered[inTrainRows,]
testDataFiltered<-onTimeDataFiltered[-inTrainRows,]

nrow(trainDataFiltered)/(nrow(testDataFiltered)+nrow(trainDataFiltered))
nrow(testDataFiltered)/(nrow(testDataFiltered)+nrow(trainDataFiltered))

logisticRegModel<-train(ARR_DEL15~.,data=trainDataFiltered,method="glm",family="binomial")

logRegPrediction<-predict(logisticRegModel,testDataFiltered)             #logisitc prediciton
logRegConfMat<-confusionMatrix(logRegPrediction,testDataFiltered[,"ARR_DEL15"])
logRegConfMat

library(randomForest)                 #randomized forest               <higher accuracy>
rfModel<-randomForest(trainDataFiltered[-1],trainDataFiltered$ARR_DEL15,proximity=TRUE,important = TRUE)
rfValidation<-predict(rfModel,testDataFiltered)
rfConfMat<-confusionMatrix(rfValidation,testDataFiltered[,"ARR_DEL15"])
rfConfMat
