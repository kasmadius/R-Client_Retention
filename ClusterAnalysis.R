library(tidyverse)
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization
library(ggplot2)
library(reshape2)
library(ggpubr)
# Data import
Pilot_data <- read.csv("C:/Users/User/Desktop/ClientRetention/train_student.csv")
Realdata <-read.csv("C:/Users/User/Desktop/ClientRetention/score_student_withID.csv")

#Remove useless columns
Pilot_data$unlimited_text <- NULL
Pilot_data$active <- NULL
Realdata$active <- NULL
Realdata$unlimited_text <- NULL
Pilot_data$voice_minutes <- NULL
Pilot_data$time_since_overage <- NULL
Pilot_data$time_since_data_overage <- NULL
Pilot_data$time_since_voice_overage <- NULL
Realdata$voice_minutes <- NULL
Realdata$time_since_overage <- NULL
Realdata$time_since_data_overage <- NULL
Realdata$time_since_voice_overage <- NULL
#Remove NA rows to be able to perform kmeans clustering
Pilot_data <- na.omit(Pilot_data)

summary(Pilot_data)

#Calculate annual profit from a customer

Pilot_data$AnnualProfit <- (Pilot_data$base_monthly_rate_phone + Pilot_data$base_monthly_rate_plan)*12

#Clusterize based on customer balance and usage of services (text, voice and data)
clusterdata <- as.data.frame(cbind(Pilot_data$phone_balance, 
                                   Pilot_data$total_data_consumption, 
                                   Pilot_data$total_text_consumption,
                                   Pilot_data$total_voice_consumption))

clusterdata <- scale(clusterdata)
clusterdata <- as.data.frame(clusterdata)

#set.seed(123)
#gc()
# function to compute total within-cluster sum of square 
#wss <- function(k) {
#  kmeans(clusterdata, k, nstart = 10, iter.max = 100,algorithm="MacQueen" )$tot.withinss
#}
# Compute and plot wss for k = 1 to k = 10
#k.values <- 1:10
# Gain optimal number of clusters
#wss_values <- map_dbl(k.values, wss)
#plot(k.values, wss_values,
#     type="b", pch = 19, frame = FALSE, 
#     xlab="Number of clusters K",
#     ylab="Total within-clusters sum of squares")


# Compute k-means clustering with optimal k = 4
set.seed(123)
final <- kmeans(clusterdata, 4, nstart = 25, iter.max = 300,algorithm="MacQueen")
print(final)

fviz_cluster(final, data = clusterdata)

kmeansREs <- factor(final$cluster)

Pilot_data$cluster <- kmeansREs

summary(Pilot_data)

clusterdata <- cbind.data.frame(phone_balance = Pilot_data$phone_balance, 
                                   total_data_consumption = Pilot_data$total_data_consumption, 
                                   total_text_consumption = Pilot_data$total_text_consumption,
                                   total_voice_consumption = Pilot_data$total_voice_consumption,
                                   age = Pilot_data$age,
                                   period = Pilot_data$period_id,
                                   base_monthly_rate_phone = Pilot_data$base_monthly_rate_phone,
                                   base_monthly_rate_plan = Pilot_data$base_monthly_rate_plan,
                                   phone_price = Pilot_data$phone_price,
                                   data = Pilot_data$data,
                                   annual_profit = Pilot_data$AnnualProfit,
                                   total_complaints = Pilot_data$total_complaints,
                                   total_technical_problems = Pilot_data$total_technical_problems,
                                   cluster =Pilot_data$cluster)
summary(clusterdata)


cluster.agg <- clusterdata %>%
  group_by(cluster) %>%
  summarise_all("median")

cluster.agg <- melt(cluster.agg)

#plots of key metrics per customer group to identify features of each group
cluster.agg
#Phone_balance per cluster
p1 <- ggplot(cluster.agg[cluster.agg$variable=='phone_balance',]) + 
  geom_bar(aes(x=variable, y = value, fill = as.factor(cluster)), 
           position="dodge", stat="identity") + 
  labs(title="Phone Balance", y="USD", x="", fill = "Group")+
  theme(plot.title = element_text(size=7), legend.text = element_text(size = 5), legend.title=element_text(size=5), axis.title.y = element_text(size=5)
        , axis.title.x = element_text(size=5), axis.text = element_text(size=5))


p2 <- ggplot(cluster.agg[cluster.agg$variable=='total_data_consumption',]) + 
  geom_bar(aes(x=variable, y = value, fill = as.factor(cluster)),
           position="dodge", stat="identity") + 
  labs(title="Data Consumption", x="", y = "GB", fill = "Group")+
  theme(plot.title = element_text(size=7),legend.text = element_text(size = 5), legend.title=element_text(size=5), axis.title.y = element_text(size=5)
        , axis.title.x = element_text(size=5), axis.text = element_text(size=5))


p3 <- ggplot(cluster.agg[cluster.agg$variable=='total_text_consumption',]) + 
  geom_bar(aes(x=variable, y = value, fill = as.factor(cluster)), 
           position="dodge", stat="identity") + 
  labs(title="Number of texts", x="", y = "# of SMS", fill = "Group")+
  theme(plot.title = element_text(size=7),legend.text = element_text(size = 5), legend.title=element_text(size=5), axis.title.y = element_text(size=5)
        , axis.title.x = element_text(size=5), axis.text = element_text(size=5))


p4 <- ggplot(cluster.agg[cluster.agg$variable=='total_voice_consumption',]) + 
  geom_bar(aes(x=variable, y = value, fill = as.factor(cluster)), 
           position="dodge", stat="identity") + 
  labs(title="Calls usage", x="", y= "Minutes", fill = "Group")+
  theme(plot.title = element_text(size=7),legend.text = element_text(size = 5), legend.title=element_text(size=5), axis.title.y = element_text(size=5)
        , axis.title.x = element_text(size=5), axis.text = element_text(size=5))


p5 <- ggplot(cluster.agg[cluster.agg$variable=='age',]) + 
  geom_bar(aes(x=variable, y = value, fill = as.factor(cluster)),
           position="dodge", stat="identity") + 
  labs(title="Age", x="", y= "Years", fill = "Group")+
  theme(plot.title = element_text(size=7),legend.text = element_text(size = 5), legend.title=element_text(size=5), axis.title.y = element_text(size=5)
         , axis.title.x = element_text(size=5), axis.text = element_text(size=5))


p6 <- ggplot(cluster.agg[cluster.agg$variable=='period',]) + 
  geom_bar(aes(x=variable, y = value, fill = as.factor(cluster)), 
           position="dodge", stat="identity") + 
  labs(title="Tenure",x="", y="Months", fill = "Group")+  
  theme(plot.title = element_text(size=7),legend.text = element_text(size = 5), legend.title=element_text(size=5), axis.title.y = element_text(size=5)
         , axis.title.x = element_text(size=5), axis.text = element_text(size=5))

p7 <- ggplot(cluster.agg[cluster.agg$variable=='base_monthly_rate_phone',]) + 
  geom_bar(aes(x=variable, y = value, fill = as.factor(cluster)), 
           position="dodge", stat="identity") + 
  labs(title="Monthly phone rate", y="USD", x="", fill = "Group")+
  theme(plot.title = element_text(size=7),legend.text = element_text(size = 5), legend.title=element_text(size=5), axis.title.y = element_text(size=5)
         , axis.title.x = element_text(size=5), axis.text = element_text(size=5))

p8 <- ggplot(cluster.agg[cluster.agg$variable=='base_monthly_rate_plan',]) + 
  geom_bar(aes(x=variable, y = value, fill = as.factor(cluster)), 
           position="dodge", stat="identity") + 
  labs(title="Payment plan", y="USD", x="", fill = "Group")+
  theme(plot.title = element_text(size=7),legend.text = element_text(size = 5), legend.title=element_text(size=5), axis.title.y = element_text(size=5)
         , axis.title.x = element_text(size=5), axis.text = element_text(size=5))

p9 <- ggplot(cluster.agg[cluster.agg$variable=='phone_price',]) + 
  geom_bar(aes(x=variable, y = value, fill = as.factor(cluster)), 
           position="dodge", stat="identity") + 
  labs(title="Phone price", y="USD", x="", fill = "Group")+
  theme(plot.title = element_text(size=7),legend.text = element_text(size = 5), legend.title=element_text(size=5), axis.title.y = element_text(size=5)
         , axis.title.x = element_text(size=5), axis.text = element_text(size=5))

figure1 <- ggarrange(p1, p2, p3, p4, p5, p6, p7, p8, p9, ncol=3, nrow=3)

figure1 <- annotate_figure(figure1,
                           top = text_grob("Figure 1. Key median metrics measurements gained per customer group ", color = "black", face = "bold", size = 10)
                          )


#Plot of median annual profit per customer group
figure2 <- ggplot(cluster.agg[cluster.agg$variable=='annual_profit',]) + 
  geom_bar(aes(x=variable, y = value, fill = as.factor(cluster)), 
           position="dodge", stat="identity") + 
  labs(y="Annual Profit in $", x="", fill = "Group")

figure2 <- annotate_figure(figure2,
                           top = text_grob("Figure 2. Median annual profit per customer group ", color = "black", face = "bold", size = 10)
                           )



#Plot of churn percentage per customer group
figure3 <- ggplot(Pilot_data, aes(x=cluster,fill=churn_in_12))+ 
  geom_bar(position = 'fill')+
  labs(y = "Percent", x="Group", fill="Churn") + scale_fill_ordinal() + 
  theme_minimal()

figure3 <- annotate_figure(figure3,
                           top = text_grob("Figure 3. Churn percentage per customer group ", color = "black", face = "bold", size = 10)
                           )


#Plot of services issues per customer group
p10 <- ggplot(cluster.agg[cluster.agg$variable=='total_technical_problems',]) + 
  geom_bar(aes(x=variable, y = value, fill = as.factor(cluster)), 
           position="dodge", stat="identity") + 
  labs(title="Technical problems", y="Count", x="", fill = "Group")#+
  #theme(plot.title = element_text(size=7),legend.text = element_text(size = 5), legend.title=element_text(size=5), axis.title.y = element_text(size=5)
  #      , axis.title.x = element_text(size=5), axis.text = element_text(size=5))

p11 <- ggplot(cluster.agg[cluster.agg$variable=='total_complaints',]) + 
  geom_bar(aes(x=variable, y = value, fill = as.factor(cluster)), 
           position="dodge", stat="identity") + 
  labs(title="Complaints", y="Count", x="", fill = "Group")#+
  #theme(plot.title = element_text(size=7),legend.text = element_text(size = 5), legend.title=element_text(size=5), axis.title.y = element_text(size=5)
    #    , axis.title.x = element_text(size=5), axis.text = element_text(size=5))

figure4 <- ggarrange(p10, p11, ncol=2, nrow=1)

figure4 <- annotate_figure(figure4,
                           top = text_grob("Figure 4. Service issues per customer group ", color = "black", face = "bold", size = 10))
