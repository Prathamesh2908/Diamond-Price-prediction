library(plyr)
library(gmodels)
library(ggplot2)
library(dplyr)
library(corrplot)
library(ROSE)
library(pROC)
library(ROCR)
library(e1071)
library(Metrics)

diamond_data <- read.csv('C:/Users/prath/Desktop/DMML dataset/diamonds/diamonds.csv')
str(diamond_data)
summary(diamond_data$price)
hist(diamond_data$price, main = "Distribution of Price variable", col = "darkorange")



par(mfrow = c(1,1))

#detecting the outliers and removing outliers
boxplot(diamond_data$carat)
medcarat <- quantile(diamond_data$carat, 0.5)
out <- quantile(diamond_data$carat, 0.95)
diamond_data$carat[diamond_data$carat > out ] <- medcarat
boxplot(diamond_data$carat)

boxplot(diamond_data$depth)
meddepth <- quantile(diamond_data$depth, 0.5)
out1a <- quantile(diamond_data$depth, 0.95)
out1b <- quantile(diamond_data$depth, 0.05)
diamond_data$depth[diamond_data$depth > out1a] <- meddepth
diamond_data$depth[diamond_data$depth < out1b] <- meddepth
boxplot(diamond_data$depth)

boxplot(diamond_data$table)
medtable <- quantile(diamond_data$table, 0.5)
out2a <- quantile(diamond_data$table, 0.95)
out2b <- quantile(diamond_data$table, 0.05)
diamond_data$table[diamond_data$table > out2a ] <- medtable
diamond_data$table[diamond_data$table < out2b ] <- medtable
boxplot(diamond_data$table)




library(psych)
pairs.panels(diamond_data[c("carat", "cut", "color", "clarity", "depth", "table", "price")],
             scale = TRUE,
             hist.col = "grey",
             main = "Correlation Matrix for the Diamonds Dataset")



#dividing the data into test and train. train contains 70% of data
dt = sort(sample(nrow(diamond_data), nrow(diamond_data)*.7))
train_dia <- diamond_data[dt,]
test_dia  <- diamond_data[-dt,]


dim(train_dia)
dim(test_dia)

#linear regression
lm_dia <- lm(price ~., train_dia)
summary(lm_dia)
lm_pred_dia_ <- predict(lm_dia, newdata = test_dia)
write.csv(lm_pred_dia_, file ="C:/Users/prath/Desktop/DMML dataset/diamonds/lm_pred_dia_.csv")
