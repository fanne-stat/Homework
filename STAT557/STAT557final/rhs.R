rhsd <- read.csv("./rhcdata.csv", header = T)
rhsd <- rhsd[,-1]
rhsd$y <- as.factor(rhsd$y)
rhsd$sex <- as.factor(rhsd$sex)
rhsd$race <- as.factor(rhsd$race)
rhsd$insur <- as.factor(rhsd$insur)
rhsd$disease <- as.factor(rhsd$disease)
rhsd$dnr <- as.factor(rhsd$dnr)
rhsd$cancer <- as.factor(rhsd$cancer)
rhsd$resp <- as.factor(rhsd$resp)
rhsd$card <- as.factor(rhsd$card)
rhsd$neuro <- as.factor(rhsd$neuro)
rhsd$gastr <- as.factor(rhsd$gastr)
rhsd$renal <- as.factor(rhsd$renal)
rhsd$meta <- as.factor(rhsd$meta)
rhsd$hema <- as.factor(rhsd$hema)
rhsd$seps <- as.factor(rhsd$seps)
rhsd$trauma <- as.factor(rhsd$trauma)
rhsd$ortho <- as.factor(rhsd$ortho)


library(randomForest)
rforest <- randomForest(formula = y~., data = rhsd, importance = TRUE)

summary(rforest)
mean(rforest$predicted == rhsd$y)
varImpPlot(rforest)

library(corrplot)
cor(rhsd)

model1 <- glm(y ~ ., data = rhsd, family = "binomial")
mean(round(model1$fit) == rhsd$y)

model.step <- step(model1, scope = list(upper = ~.), trace = F)
mean(round(model.step$fit) == rhsd$y)

library(caret)
train_ind <- createDataPartition(rhsd$y, p = 0.5, list = F)
# train_ind
# 
rhsd_train <- rhsd[train_ind,]
# rhsd_test <- rhsd[-train_ind,]
# 
# rhs.rf <- randomForest(y~., data = rhsd_train, importance = T)
# mean(rhs.rf$predicted == rhsd_train$y)
# pred_test <- predict(rhs.rf, newdata = rhsd_test)
# pred_test
# mean(pred_test==rhsd_test$y)
# 
# rhs.model1 <- glm(y~., data = rhsd_train, family = "binomial")
# mean(round(rhs.model1$fit) == rhs.model1$y)
# 
# pred_test.model1 <- round(predict(rhs.model1, newdata = rhsd_test, type = c("response")))
# pred_test.model1
# mean(pred_test.model1 == rhsd_test$y)

# using repeated k-fold to assess the predictive ability
# repeated 5 folds, 10 times 
rcv_ind <- createMultiFolds(rhsd$y, k = 5, times = 10)

# accuracy calculation for random forest model
cv_rf <- function(train_ind){
  train_set <- rhsd[train_ind,]
  cv_set <- rhsd[-train_ind,]
  rf_fit <- randomForest(y~., data = train_set)
  pred_test <- predict(rf_fit, newdata = cv_set)
  accuracy <- mean(pred_test == cv_set$y)
  return(accuracy)
}


# accuracy calculation for model1
cv_model1 <- function(train_ind){
  train_set <- rhsd[train_ind,]
  cv_set <- rhsd[-train_ind,]
  model1_fit <- glm(y~., data = train_set, family = "binomial")
  pred_test <- round(predict(model1_fit, newdata = cv_set, type = c("response")))
  accuracy <- mean(pred_test == cv_set$y)
  return(accuracy)
}

accuracy_rf <- sapply(rcv_ind, cv_rf) 
accuracy_model1 <- sapply(rcv_ind, cv_model1) 


model.step <- step(model1, scope = list(upper = ~.))
formula(model.step)
summary(model.step)
anova(model.step, test = "Chisq")

cv_model.step <- function(train_ind){
  train_set <- rhsd[train_ind,]
  cv_set <- rhsd[-train_ind,]
  model_fit <- glm(formula(model.step), data = train_set, family = "binomial")
  pred_test <- round(predict(model_fit, newdata = cv_set, type = c("response")))
  accuracy <- mean(pred_test == cv_set$y)
  return(accuracy)
}

accuracy_model.setp <- sapply(rcv_ind, cv_model.step)

anova(model.step, test = "Chisq")

model2 <- glm(y ~ edu + insur + disease + dnr + cancer + aps + weight + rrate + hrt + pafi + paco2 + pH + hemat + pot + resp + card + neuro + hema + seps + trauma, data = rhsd, family = "binomial")
summary(model2)
anova(model2, test = "Chisq")

cv_model2 <- function(train_ind){
  train_set <- rhsd[train_ind,]
  cv_set <- rhsd[-train_ind,]
  model_fit <- glm(formula(model2), data = train_set, family = "binomial")
  pred_test <- round(predict(model_fit, newdata = cv_set, type = c("response")))
  accuracy <- mean(pred_test == cv_set$y)
  return(accuracy)
}

accuracy_model2 <- sapply(rcv_ind, cv_model2)



rhsd1 <- rhsd
rhsd1 <- data.frame(rhsd, dr = residuals(model2, type = "deviance"))

library(ggplot2)
ggplot(data = rhsd1, aes(x = weight, y = dr)) + geom_smooth(se = F) + geom_point() + labs(y = "Deviance Residuals", x = "Weight")
ggplot(data = rhsd1, aes(x = hrt, y = dr)) + geom_smooth(se = F) + geom_point() + labs(y = "Deviance Residuals", x = "hrt")
#-------------------------------------------------

model3 <- glm(y ~ edu + insur + disease + dnr + cancer + aps + rrate + pafi + paco2 + pH + hemat + pot + resp + card + neuro + hema + seps + trauma, data = rhsd, family = "binomial")
summary(model3)
anova(model3, test = "Chisq")

cv_model3 <- function(train_ind){
  train_set <- rhsd[train_ind,]
  cv_set <- rhsd[-train_ind,]
  model_fit <- glm(formula(model3), data = train_set, family = "binomial")
  pred_test <- round(predict(model_fit, newdata = cv_set, type = c("response")))
  accuracy <- mean(pred_test == cv_set$y)
  return(accuracy)
}

accuracy_model3 <- sapply(rcv_ind, cv_model3)


model4 <- glm(y ~ . - hrt - weight,data = rhsd, family = "binomial")
summary(model4)
anova(model4, test = "Chisq")

cv_model4 <- function(train_ind){
  train_set <- rhsd[train_ind,]
  cv_set <- rhsd[-train_ind,]
  model_fit <- glm(formula(model4), data = train_set, family = "binomial")
  pred_test <- round(predict(model_fit, newdata = cv_set, type = c("response")))
  accuracy <- mean(pred_test == cv_set$y)
  return(accuracy)
}

accuracy_model4 <- sapply(rcv_ind, cv_model4)

model.step2 <- step(model4, scope = list(upper = formula(model4)), trace = F)
summary(model.step2)
anova(model.step2, test = "Chisq")

cv_model.step2 <- function(train_ind){
  train_set <- rhsd[train_ind,]
  cv_set <- rhsd[-train_ind,]
  model_fit <- glm(formula(model.step2), data = train_set, family = "binomial")
  pred_test <- round(predict(model_fit, newdata = cv_set, type = c("response")))
  accuracy <- mean(pred_test == cv_set$y)
  return(accuracy)
}

accuracy_model.step2 <- sapply(rcv_ind, cv_model.step2)


model5 <- glm(paste(c(formula(model.step2), "- ortho - renal - sod - alb - gastr"), collapse = ""), data = rhsd, family = "binomial")
summary(model5)
anova(model5, test = "Chisq")

AIC(model5)

cv_model5 <- function(train_ind){
  train_set <- rhsd[train_ind,]
  cv_set <- rhsd[-train_ind,]
  model_fit <- glm(formula(model5), data = train_set, family = "binomial")
  pred_test <- round(predict(model_fit, newdata = cv_set, type = c("response")))
  accuracy <- mean(pred_test == cv_set$y)
  return(accuracy)
}

accuracy_model5 <- sapply(rcv_ind, cv_model5)

summary(accuracy_model5)

rocp <- roc(rhsd$y, predict(model1, type = c("response")))
plot(rocp)
#------------------------------------------------------------------------------

library(glmnet)

model_glmnet <- cv.glmnet(x = model.matrix(model1), y = rhsd$y, family = "binomial", type.measure = "class")

summary(model_glmnet)

plot(model_glmnet)

model_glmnet$lambda.min

coef.min <- coef(model_glmnet, s = "lambda.min")

