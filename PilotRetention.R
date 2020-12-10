library(tidyverse)
library(caret)
library(rpart)
library(rpart.plot)
library(ranger)

# Data import
Pilot_data <- read.csv("~/Documents/HEC/Statistical_Learning/Client Retention/train_student.csv")
Realdata <-read.csv("~/Documents/HEC/Statistical_Learning/Client Retention/score_student_withID.csv")

str(Pilot_data)

Pilot_data$unlimited_text <- NULL
Pilot_data$active <- NULL
Realdata$active <- NULL
Realdata$unlimited_text <- NULL

#Period id into categorical variable
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
Pilot_data$GrpTenure = sapply(Pilot_data$period_id, CreateGrp)
Pilot_data$GrpTenure = as.factor(Pilot_data$GrpTenure)
Realdata$GrpTenure = sapply(Realdata$period_id, CreateGrp)
Realdata$GrpTenure = as.factor(Realdata$GrpTenure)
# Target variable as 0 or 1 , I tried as.numeric but it gives back 1 and 2...
Pilot_data$churn_in_12 <- as.numeric(as.logical(toupper(Pilot_data$churn_in_12)))

summary(Pilot_data)

set.seed(123456)
#Remove useless columns
Pilot_data$voice_minutes <- NULL
Pilot_data$time_since_overage <- NULL
Pilot_data$time_since_data_overage <- NULL
Pilot_data$time_since_voice_overage <- NULL
Realdata$voice_minutes <- NULL
Realdata$time_since_overage <- NULL
Realdata$time_since_data_overage <- NULL
Realdata$time_since_voice_overage <- NULL

#Split trainset into train and test set
index_train <- sample(1:nrow(Pilot_data), 4/5 * nrow(Pilot_data))
train_set <- Pilot_data[index_train, ]
test_set <- Pilot_data[-index_train, ]

summary(train_set)
#Impute trainset median for phone price, time_since_technical_problems and time_since_complaints
phone_price_median <- 737.54 
train_set$phone_price <- ifelse(is.na(train_set$phone_price), phone_price_median, train_set$phone_price)
test_set$phone_price <- ifelse(is.na(test_set$phone_price), phone_price_median, test_set$phone_price)
Realdata$phone_price <- ifelse(is.na(Realdata$phone_price), phone_price_median, Realdata$phone_price)

tech_problem_median <- 3 
train_set$time_since_technical_problems <- ifelse(is.na(train_set$time_since_technical_problems), tech_problem_median, train_set$time_since_technical_problems)
test_set$time_since_technical_problems <- ifelse(is.na(test_set$time_since_technical_problems), tech_problem_median, test_set$time_since_technical_problems)
Realdata$time_since_technical_problems <- ifelse(is.na(Realdata$time_since_technical_problems), tech_problem_median, Realdata$time_since_technical_problems)

complaint_median <- 3
train_set$time_since_complaints <- ifelse(is.na(train_set$time_since_complaints), complaint_median, train_set$time_since_complaints)
test_set$time_since_complaints <- ifelse(is.na(test_set$time_since_complaints), complaint_median, test_set$time_since_complaints)
Realdata$time_since_complaints <- ifelse(is.na(Realdata$time_since_complaints), complaint_median, Realdata$time_since_complaints)

summary(train_set)
summary(test_set)

# random forest 
case_weights <- ifelse(train_set$churn_in_12 == 1 , 5 , 1)
RF <- ranger(formula = churn_in_12~ .-unique_id -id -family_id -total_overage_fees, data = train_set , 
              case.weights = case_weights , classification = TRUE , verbose = TRUE , num.trees = 300,
              mtry = 6 , importance = "impurity", write.forest = TRUE , probability = TRUE , 
              min.node.size = 1000 , max.depth = 15 , splitrule = "gini",  )

PredTest <- predict(RF , data = test_set, num.trees = 300 , type = "response" , verbose= T )
PredTest <- PredTest$predictions
PredTest <- PredTest[,2]

source("~/Documents/HEC/Statistical_Learning/Week5-PredBinaries/PredictingBinaries.R")

confusion(test_set$churn_in_12,as.numeric(PredTest>0.5))
roc(test_set$churn_in_12,PredTest,col="blue")$AUC
importance(RF)

#Realdataset prediction for churn probability
RF_Prediction <- predict(RF , data = Realdata , num.trees = 300 , type = "response" , verbose= T )
RF_Prediction <- RF_Prediction$predictions
RF_Prediction<- RF_Prediction[,2]

#Best cut off was 0.5023 trees = 300
Cutoff_up <- 0.51
#Result 
Realdata$INVITATION <- ifelse(RF_Prediction> Cutoff_up, 1, 0)
Realdata$INVITATION = factor(Realdata$INVITATION)
Result <- data.frame(Realdata$unique_family , Realdata$INVITATION)
Index_NoChurn <- which(Result$Realdata.INVITATION==0)
Churn <- Result[-Index_NoChurn,]
nrow(Churn)
write.csv(Churn$Realdata.unique_family , "~/Documents/HEC/Statistical_Learning/Client Retention/Result1.csv",row.names = FALSE)

