setwd("~/JHU_PracMachLearn")

library(RCurl)
trainurl <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
testurl <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"

trainlink <- getURL(trainurl)
testlink <- getURL(testurl)

train <- read.csv(textConnection(trainlink))
test <- read.csv(textConnection(testlink))
rm(testlink, testurl, trainlink, trainurl)

library(caret)
library(doMC)
set.seed(0831)
inTrain <- createDataPartition(train$classe, p = 0.7, list = FALSE)
trn <- train[inTrain,]
tst <- train[-inTrain,]
rm(inTrain)

drops <- c("X", "user_name", "raw_timestamp_part_1", "raw_timestamp_part_2"
           , "cvtd_timestamp", "new_window", "num_window")
`%ni%` <- Negate(`%in%`)

trn <- trn[trn$new_window == "no",]
tst <- tst[tst$new_window == "no",] #only rows without the new window rows
trn <- trn[, colSums(is.na(trn)) == 0]
tst <- tst[, colSums(is.na(tst)) == 0] #no columns with NA
trn <- trn[, colSums(trn != "") != 0]
tst <- tst[, colSums(tst != "") != 0] #no columns with blank values
trn <- subset(trn, select = names(trn) %ni% drops)
tst <- subset(tst, select = names(tst) %ni% drops) #drop other non-predictive fields

registerDoMC(4)
set.seed(0831)

rpart <- train(classe ~ ., method = "rpart", data = trn)
confusionMatrix(predict(rpart), trn$classe)

#set.seed(0831)
#rf <- train(classe ~ ., method = "rf", data = trn)
#save(rf, file = "rf.RModel")
load("rf.RModel")

confusionMatrix(predict(rf), trn$classe) #100% accuracy in Training set
tst$predict <- predict(rf, newdata = tst)
confusionMatrix(tst$predict, tst$classe) #99.3% accuracy in Test set

test$predict <- predict(rf, newdata = test)
summary(test$predict)
