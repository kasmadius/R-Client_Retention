suppressMessages(library(gridExtra))
suppressMessages(library(grid))
suppressMessages(library(ggplot2))
suppressMessages(library(lattice))
suppressMessages(library(corrplot))
suppressMessages(library(plyr))
suppressMessages(library(pROC))
suppressMessages(library(caret))
suppressMessages(library(doParallel))
suppressMessages(library(lares))
#path to train data

TRAINDIR = "C:\\Users\\User\\Desktop\\R_Projects\\ClientRetention\\Data\\train_student.csv"

traindata = read.csv(TRAINDIR)

str(traindata)

summary(traindata)
#Subset with no promo to exclude pilot insights
nopilotset = as.data.frame(traindata[traindata$promo == "False",])

summary(nopilotset)

head(nopilotset)

#Create tenure group of customers
CreateGrp <- function(period){
  if (period >= 0 & period <= 12){
    return('0-12')
  }else if(period > 12 & period <= 24){
    return('12-24')
  }else if (period > 24 & period <= 48){
    return('24-48')
  }else if (period > 48 & period <=60){
    return('48-60')
  }else if (period > 60){
    return('> 60')
  }
}
#Factorize the customer by period of months
nopilotset$GrpTenure = sapply(nopilotset$period_id, CreateGrp)
nopilotset$GrpTenure = as.factor(nopilotset$GrpTenure)

summary(nopilotset)

#Remove NA columns
nopilotset$voice_minutes = NULL
nopilotset$time_since_overage = NULL
nopilotset$time_since_data_overage = NULL
nopilotset$time_since_voice_overage = NULL
numericvar = sapply(nopilotset, is.numeric)

nopilotset$time_since_complaints <- ifelse(is.na(nopilotset$time_since_complaints) & !is.na(nopilotset$period_id), 120-as.numeric(nopilotset$period_id), nopilotset$time_since_complaints)
nopilotset$time_since_technical_problems <- ifelse(is.na(nopilotset$time_since_technical_problems) & !is.na(nopilotset$period_id), 120-as.numeric(nopilotset$period_id), nopilotset$time_since_technical_problems)

#Remove unnecessary columns
nopilotset = nopilotset[, -which(names(nopilotset) %in% c("id", "family_id", "promo", "unlimited_text", "active", "period_id", "phone_price"))]

corr_cross(nopilotset, # dataset
           max_pvalue = 0.05, # show only sig. correlations at selected level
           top = 30) # display top 10 correlations, any couples of variables  )

corrmatrix = cor(nopilotset[,numericvar])
corrmatrix
corrplot(corrmatrix, main="\n\nCorrelation Plot for Numerical Variables", method="number")

# check the frequency for each group bin
table(nopilotset$GrpTenure)
barplot(table(nopilotset$GrpTenure), col=c("#d0d0d0"))

# my plots will be very similar lets simplify using a function for it
createplot <- function(dst, column, name) {
  plt <- ggplot(dst, aes(x=column, fill=(churn_in_12))) + 
    ggtitle(name) + 
    xlab(name) +
    ylab("Percentage")  +
    geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.7) + 
    theme_minimal() +
    theme(legend.position="none", axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_fill_manual(values=c("#d0d0d0", "#E69F00"))
  return(plt)
}

# Plot 1 by gender 
p1 <- createplot(nopilotset, nopilotset$gender, "Gender")                      
p3 <- createplot(nopilotset, nopilotset$unlimited_voice, "unlimited_voice")
# plot 4 by plan_type
p4 <- createplot(nopilotset, nopilotset$plan_type, "plan_type")
# plot 5 by workphone
p5 <- createplot(nopilotset, nopilotset$workphone, "workphone")

# draw the plot grid
grid.arrange(p1,p3, p4, p5, ncol=4)

#Percentage of missing data
sapply(nopilotset[,-c(2)], function(x) round((sum(is.na(x))/length(x)*100),2))

#Factorize categories
nopilotset$churn_in_12 = factor(nopilotset$churn_in_12)
nopilotset$plan_type <- factor(nopilotset$plan_type)
nopilotset$gender <- factor(nopilotset$gender)
nopilotset$workphone <- factor(nopilotset$workphone)
nopilotset$unlimited_voice <- factor(nopilotset$unlimited_voice)

str(nopilotset)
summary(nopilotset)

# let's split the dataset into two
TrainTest <- nopilotset
idxSplit <- createDataPartition(nopilotset$churn_in_12, p = 0.75, list=FALSE)
TrainModel <- TrainTest[idxSplit,]
TestModel <- TrainTest[-idxSplit,]

varsToKeep <- c('churn_in_12','plan_type','gender','workphone','GrpTenure',
                'text_consumption','voice_consumption','total_data_consumption','total_voice_consumption',
                'technical_problem','complaints','total_complaints', 'phone_balance',
                'base_monthly_rate_phone', 'base_monthly_rate_plan', 'unlimited_voice',
                'time_since_technical_problems', 'time_since_complaints')


TrainModel <- TrainModel[,varsToKeep]
TestModel <- TestModel[,varsToKeep]
summary(TrainModel)

# Run algorithms using 5-fold cross validation
control <- trainControl(method="cv", number=5, returnResamp='none', summaryFunction = twoClassSummary, classProbs = TRUE)
metric <- "Accuracy"

trainX <- TrainModel[,names(TrainModel) != "churn_in_12"]
preProcValues <- preProcess(x = trainX,method = c("center", "scale"))
preProcValues


cl <- makePSOCKcluster(10)
registerDoParallel(cl)

# Gradient Boost Machine (GBM)
set.seed(7)
fit.gbm <- train(churn_in_12 ~ ., data=TrainModel, method="gbm", 
                 metric=metric, trControl=control, verbose=FALSE, preProc = c("center", "scale"))

stopCluster(cl)

summary(TrainModel)

summary(TestModel)

print(fit.gbm)

AccCalc <- function(TestFit, name) {
  # prediction 
  DstTestModelClean <- TestModel
  DstTestModelClean$churn_in_12 <- NA
  predictedval <- predict(TestFit, newdata=DstTestModelClean, type="prob")
  # summarize results with confusion matrix
  cm <- confusionMatrix(predictedval, TestModel$churn_in_12)
  
  # calculate accuracy of the model
  Accuracy<-round(cm$overall[1],2)
  acc <- as.data.frame(Accuracy)
  
  roc_obj <- roc(TestModel$churn_in_12, as.numeric(predictedval))
  acc$Auc <- auc(roc_obj)
  
  acc$FitName <- name
  return(acc)
}

accAll <-AccCalc(fit.gbm, "gbm")
rownames(accAll) <- c()
arrange(accAll,desc(Accuracy))

#Prediction for dataset
TESTDIR = "C:\\Users\\User\\Desktop\\R_Projects\\ClientRetention\\Data\\score_student_withID.csv"

testdata = read.csv(TESTDIR)
testdata$GrpTenure = sapply(testdata$period_id, CreateGrp)
testdata$GrpTenure = as.factor(testdata$GrpTenure)
testdata$plan_type <- factor(testdata$plan_type)
testdata$gender <- factor(testdata$gender)
testdata$workphone <- factor(testdata$workphone)
testdata$unlimited_voice <- factor(testdata$unlimited_voice)

testdata$time_since_complaints <- ifelse(is.na(testdata$time_since_complaints) & !is.na(testdata$period_id), 120-as.numeric(testdata$period_id), testdata$time_since_complaints)
testdata$time_since_technical_problems <- ifelse(is.na(testdata$time_since_technical_problems) & !is.na(testdata$period_id), 120-as.numeric(testdata$period_id), testdata$time_since_technical_problems)

testdata$churn_hut <- NULL
testdata$churn_hutp <- NULL

#GBM
testdata$churn_hutp <- predict(fit.gbm, newdata = testdata, type="prob")

head(testdata)

#Threshold classification
testdata$churn_hut <- ifelse(testdata$churn_hutp$True > 0.19, "True", "False")
testdata$churn_hut <- factor(testdata$churn_hut)
summary(testdata)

result <- testdata$unique_family[testdata$churn_hut=="True"]
write.csv(result,"C:\\Users\\User\\Desktop\\R_Projects\\ClientRetention\\Data\\Result.csv", row.names = FALSE)
