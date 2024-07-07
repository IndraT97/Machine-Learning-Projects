#### Table 10.2

library(rpart)
library(rpart.plot)
library(caret)

bank.df <- read.csv("UniversalBank.csv")
bank.df <- bank.df[ , -c(1, 5)]  # Drop ID and zip code columns.
# treat Education as categorical (R will create dummy variables)
bank.df$Education <- factor(bank.df$Education, levels = c(1, 2, 3), 
                            labels = c("Undergrad", "Graduate", "Advanced/Professional"))

# partition data
set.seed(2)
train.index <- sample(c(1:dim(bank.df)[1]), dim(bank.df)[1]*0.6)  
train.df <- bank.df[train.index, ]
valid.df <- bank.df[-train.index, ]

# run logistic regression
# use glm() (general linear model) with family = "binomial" to fit a logistic 
# regression.
logit.reg <- glm(Personal.Loan ~ ., data = train.df, family = "binomial") 
options(scipen=999)
summary(logit.reg)


#### Table 10.3

# use predict() with type = "response" to compute predicted probabilities. 
logit.reg.pred <- predict(logit.reg, valid.df, type = "response")

# first 5 actual and predicted records
data.frame(actual = valid.df$Personal.Loan[1:5], predicted = logit.reg.pred[1:5])

logit.reg.pred.classes <- ifelse(logit.reg.pred > 0.5, 1, 0)
confusionMatrix(as.factor(logit.reg.pred.classes), as.factor(valid.df$Personal.Loan))

# model selection
full.logit.reg <- glm(Personal.Loan ~ ., data = train.df, family = "binomial") 
empty.logit.reg  <- glm(Personal.Loan ~ 1,data = train.df, family= "binomial")
summary(empty.logit.reg)

backwards = step(full.logit.reg)
summary(backwards)

backwards.reg.pred <- predict(backwards, valid.df, type = "response")
backwards.reg.pred.classes <- ifelse(backwards.reg.pred > 0.5, 1, 0)
confusionMatrix(as.factor(backwards.reg.pred.classes), as.factor(valid.df$Personal.Loan))


back2 <- glm(Personal.Loan ~ Income + Education + CD.Account,data = train.df, family= "binomial")
summary(back2)

forwards = step(empty.logit.reg,scope=list(lower=formula(empty.logit.reg),upper=formula(full.logit.reg)), direction="forward",trace=0)
formula(forwards)

stepwise = step(empty.logit.reg,scope=list(lower=formula(empty.logit.reg),upper=formula(full.logit.reg)), direction="both",trace=1)
formula(stepwise)


#### Table 11.2

#NN
nn <- neuralnet(Personal.Loan ~ Income + CD.Account+Family+CreditCard, data = train.df, linear.output = F, hidden = 3)

plot(nn, rep="best")

nn.pred <- predict(nn, valid.df, type = "response")
nn.pred.classes <- ifelse(nn.pred > 0.5, 1, 0)
confusionMatrix(as.factor(nn.pred.classes), as.factor(valid.df$Personal.Loan))

#Another example of NN
df <- read.csv("TinyData.csv")

df$Like <- df$Acceptance=="like"
df$Dislike <- df$Acceptance=="dislike"

set.seed(1)
nn <- neuralnet(Like + Dislike ~ Salt + Fat, data = df, linear.output = F, hidden = 3)

# display weights
nn$weights

# display predictions
prediction(nn)

# plot network
plot(nn, rep="best")

#slow down learning rate
nn <- neuralnet(Like + Dislike ~ Salt + Fat, data = df, linear.output = F, hidden = 4,learningrate=.5)
plot(nn, rep="best")

prediction(nn)
