install.packages('ISLR')
library('ISLR')
library('caret')
library('pROC')

# 1. Importing the Dataset and Exploratory Data Analysis

College <- ISLR :: College

summary(College)
psych::describe(College)

# Scatter Plot of Accept and Enroll
plot(College$Accept, College$Enroll ,
     main = 'Scatter Plot - Accepted VS Enrolled',
     ylab = '# of Acepted',
     xlab = '# of Enrolled')

# Bar Plot of Private VS Non Private Colleges
barplot(table(CollegeDF$Private), ylim = c(0,800),
        main = 'Bar Plot of Private VS Non Private Colleges',
        ylab = 'Number of Colleges')

# 2. Splitting the dataset into train and test set
set.seed(123)
trainIndex <- createDataPartition(College$Private, p=0.7, list = FALSE)
train <- College[trainIndex,]
test <- College[-trainIndex,]

# 3. Logistic Regression model using glm()

RegModel1 <- glm(Private~.,data=train, family = binomial(link = logit)) 
summary(RegModel1)

RegModel2 <- glm(Private ~ Outstate + S.F.Ratio ,data=train, family = binomial(link = logit)) 
summary(RegModel2)

# Regression Coef (Log Odds)
coef(RegModel2)

# Regression Coef (Odds)
exp(coef(RegModel2))

# 4. Confusion matrix for Train Set

probabilities.train <- predict(RegModel2, newdata=train, type="response")

predicted.classes.min <- as.factor(ifelse(probabilities.train>=0.5, "Yes", "No"))

confusionMatrix(predicted.classes.min, train$Private, positive = "Yes") 


# 6. Confusion Matrix for Test Set

probabilities.test = predict(RegModel2, newdata = test, type="response")
predicted.classes.min = as.factor(ifelse(probabilities.test>=0.5, "Yes", "No"))
head(predicted.classes.min)

confusionMatrix(predicted.classes.min, test$Private, positive = "Yes")


# 7. ROC Curve

ROC1 = roc(test$Private, probabilities.test)

plot(ROC1, col="blue", ylab="Sensitivity - TP Rate", xlab= "Specificity - Fp Rate")

# 8. AUC

AUC1 = auc(ROC1)
AUC1
