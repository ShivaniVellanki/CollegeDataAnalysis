# ALY6015 - Initial Analysis Report - Group 2
library("corrplot")
library("car")
library("caret")
library('pROC')

StudentDF <- read.csv("/Users/rohithjoginapally/Downloads/student-mat.csv")
StudentDF

# Changing the values of Yes to 1 and No to 0
StudentDF$schoolsup <- ifelse(StudentDF$schoolsup =="yes", 1,0)
StudentDF
StudentDF$famsup <- ifelse(StudentDF$famsup =="yes", 1,0)

StudentDF$paid <- ifelse(StudentDF$paid == "yes",1,0)

StudentDF$activities <- ifelse(StudentDF$activities == "yes",1,0)

StudentDF$nursery <- ifelse(StudentDF$nursery == "yes",1,0)

StudentDF$higher <- ifelse(StudentDF$higher == "yes",1,0)

StudentDF$internet  <- ifelse(StudentDF$internet == "yes",1,0)

StudentDF$romantic <- ifelse(StudentDF$romantic == "yes",1,0)

StudentDF$FinalGrade <- ifelse(StudentDF$G3 >= 10,1,0)# Creating a new variable where 1 = Pass and 0 = fail
StudentDF$FinalGrade <- as.factor(StudentDF$FinalGrade)

StudentDF

# Descriptive Statistics and EDA
str(StudentDF)
summary(StudentDF)

# Correlation
StudentCor <- cor(StudentDF[unlist(lapply(StudentDF,is.numeric),use.names= FALSE)])
View(StudentCor)

corrplot(StudentCor)

# Scatterplot of G1 - First Period Grade & G3 - Final Grade
scatterplot(G3 ~ G1, data=StudentDF,
            main="Correlation Plot - G1 VS G3",
            xlab="G1 - First Period Grade", ylab="G3 - Final Grade",
            col ='Red')

# Scatterplot of G2 - Second Period Grade & G3 - Final Grade
scatterplot(G3 ~ G2, data=StudentDF,
            main="Correlation Plot - G2 VS G3",
            xlab="G2 - Second Period Grade", ylab="G3 - Final Grade",
            col ='Blue')
cor(StudentDF$G2,StudentDF$G3)

# Scatterplot of G2 - Second Period Grade & G3 - Final Grade
scatterplot(G3 ~ failures, data=StudentDF,
            main="Correlation Plot - failures VS G3",
            xlab="# of Failures", ylab="G3 - Final Grade",
            col ='orange')
cor(StudentDF$failures,StudentDF$G3)

# Splitting the dataset into train and test set
set.seed(123)
trainIndex <- createDataPartition(StudentDF$FinalGrade, p=0.7, list = FALSE)
train <- StudentDF[trainIndex,]
test <- StudentDF[-trainIndex,]

# Logistic Regression using glm()

LogRegModel1 <- glm(FinalGrade ~ G1+G2+failures ,data=train, family = binomial(link = logit))
summary(LogRegModel1)

# Regression Coef (Log Odds)
coef(LogRegModel1)

# Regression Coef (Odds)
exp(coef(LogRegModel1))

# Confusion matrix for Train Set

probabilities.train <- predict(LogRegModel1, newdata=train, type="response")

predicted.classes.min <- as.factor(ifelse(probabilities.train>=0.5, 1, 0))

confusionMatrix(predicted.classes.min, train$FinalGrade)

# Confusion matrix for Test Set

probabilities.test <- predict(LogRegModel1, newdata=test, type="response")

predicted.classes.min <- as.factor(ifelse(probabilities.test>=0.5, 1, 0))

confusionMatrix(predicted.classes.min, test$FinalGrade)


# 7. ROC Curve

ROC1 = roc(test$FinalGrade, probabilities.test)

plot(ROC1, col="blue", ylab="Sensitivity - TP Rate", xlab= "Specificity - Fp Rate")

# 8. AUC

AUC1 = auc(ROC1)
AUC1

# Logistic Regression using all variables as Predictors

LogRegModel2 <- glm(FinalGrade ~ ., data = train, family = binomial(link = logit))
summary(LogRegModel2)

# Regression Coef (Log Odds)
coef(LogRegModel2)

# Regression Coef (Odds)
exp(coef(LogRegModel2))

# Confusion matrix for Train Set
probabilities.train <- predict(LogRegModel2, newdata = train, type = "response")
predicted.classes.min <- as.factor(ifelse(probabilities.train >= 0.5, 1, 0))
confusionMatrix(predicted.classes.min, train$FinalGrade)

# Confusion matrix for Test Set
probabilities.test <- predict(LogRegModel2, newdata = test, type = "response")
predicted.classes.min <- as.factor(ifelse(probabilities.test >= 0.5, 1, 0))
confusionMatrix(predicted.classes.min, test$FinalGrade)

# ROC Curve
ROC2 <- roc(test$FinalGrade, probabilities.test)
plot(ROC2, col = "blue", ylab = "Sensitivity - TP Rate", xlab = "Specificity - FP Rate")

# AUC
AUC2 <- auc(ROC2)
AUC2
