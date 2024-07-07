#### Table 5.1

# package forecast is required to evaluate performance
library(forecast)

# load file
toyota.corolla.df <- read.csv("ToyotaCorolla.csv")

toyota.corolla.df=toyota.corolla.df[,-c(1,2,8,11)]

# randomly generate training and validation sets
training <- sample(c(1:dim(toyota.corolla.df)[1]), 600)
validation <- sample(setdiff(c(1:dim(toyota.corolla.df)[1]), training), 400)

train.df <- toyota.corolla.df[training, ]
valid.df <- toyota.corolla.df[validation, ]

# run linear regression model
reg <- lm(Price~., data=train.df, subset=training,
          na.action=na.exclude)
pred_t <- predict(reg,train.df,na.action=na.pass)
pred_v <- predict(reg,valid.df,
                  na.action=na.pass)


## evaluate performance
##Mean Error, Root Mean Squared Error,Mean Absolute Error, Mean Percentage Error, Mean Absolute Percentage Error
# training
accuracy(pred_t,train.df$Price)
# validation
accuracy(pred_v, valid.df$Price)

options(scipen=999)

#### Figure 5.2
# load package gains, compute gains (we will use package caret for categorical y later)
library(gains)
gain <- gains(valid.df$Price[!is.na(pred_v)], pred_v[!is.na(pred_v)])

# cumulative lift chart

# we will compute the gain relative to price
price <- valid.df$Price[!is.na(valid.df$Price)]
plot(c(0,gain$cume.pct.of.total*sum(price))~c(0,gain$cume.obs), 
     xlab="# cases", ylab="Cumulative Price", main="Lift Chart", type="l")

# baseline
lines(c(0,sum(price))~c(0,dim(valid.df)[1]), col="gray", lty=2)

# Decile-wise lift chart
barplot(gain$mean.resp/mean(price), names.arg = gain$depth,
        xlab = "Percentile", ylab = "Mean Price", main = "Decile-wise lift chart")

#### Table 5.5
library(caret)
library(e1071)

owner.df <- read.csv("ownerExample.csv")
confusionMatrix(as.factor(ifelse(owner.df$Probability>0.5, 'owner', 'nonowner')), as.factor(owner.df$Class))
confusionMatrix(as.factor(ifelse(owner.df$Probability>0.25, 'owner', 'nonowner')), 
                as.factor(owner.df$Class))
confusionMatrix(as.factor(ifelse(owner.df$Probability>0.75, 'owner', 'nonowner')), 
                as.factor(owner.df$Class))



#### Figure 5.4

df <- read.csv("liftExample.csv")


# create empty accuracy table
accT = c() 

# compute accuracy per cutoff
for (cut in seq(0,1,0.1)){
  cm <- confusionMatrix(as.factor(1 * (df$prob > cut)), as.factor(df$actual))
  accT = c(accT, cm$overall[1])
}

# plot accuracy
plot(accT ~ seq(0,1,0.1), xlab = "Cutoff Value", ylab = "", type = "l", ylim = c(0, 1))
lines(1-accT ~ seq(0,1,0.1), type = "l", lty = 2)
legend("topright",  c("accuracy", "overall error"), lty = c(1, 2), merge = TRUE)


#### Figure 5.5

library(pROC)
r <- roc(df$actual, df$prob)
plot.roc(r)

# compute auc
auc(r)

