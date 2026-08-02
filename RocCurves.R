# Load Required Libraries

install.packages(c("caret", "randomForest", "e1071", "ggplot2", "pROC"))
library(caret)
library(randomForest)
library(e1071)
library(ggplot2)
library(pROC)


# Load Data

data(iris)


# Split Data

set.seed(123)

trainIndex <- createDataPartition(iris$Species, 
                                  p = 0.6, 
                                  list = FALSE)

trainData <- iris[trainIndex, ]
testData  <- iris[-trainIndex, ]

# Preprocessing

preProcValues <- preProcess(trainData[, -5], method = c("center", "scale"))

trainTransformed <- predict(preProcValues, trainData)
testTransformed  <- predict(preProcValues, testData)


# Train Random Forest Model

control <- trainControl(method = "cv",
                        number = 10,
                        classProbs = TRUE)

model <- train(Species ~ ., 
               data = trainTransformed,
               method = "rf",
               trControl = control)


# Predictions

predictions  <- predict(model, testTransformed)
probabilities <- predict(model, testTransformed, type = "prob")


# Confusion Matrix

confusionMatrix(predictions, testTransformed$Species)


# Histogram of Prediction Percentages

pred_table <- as.data.frame(table(predictions))
colnames(pred_table) <- c("Species", "Count")

pred_table$Percentage <- (pred_table$Count / sum(pred_table$Count)) * 100

ggplot(pred_table, aes(x = Species, y = Percentage, fill = Species)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")), 
            vjust = -0.5, size = 5) +
  labs(title = "Prediction Distribution (%)",
       x = "Predicted Species",
       y = "Percentage (%)") +
  theme_minimal() +
  ylim(0, 100)


# ROC Curves (One-vs-All)


test_labels <- testTransformed$Species

roc_setosa <- roc(as.numeric(test_labels == "setosa"), probabilities$setosa)
roc_versicolor <- roc(as.numeric(test_labels == "versicolor"), probabilities$versicolor)
roc_virginica <- roc(as.numeric(test_labels == "virginica"), probabilities$virginica)

plot(roc_setosa, col = "orange", main = "ROC Curves (One-vs-All)")
plot(roc_versicolor, col = "yellow", add = TRUE)
plot(roc_virginica, col = "purple", add = TRUE)

legend("bottomright",
       legend = c(
         paste("Setosa AUC =", round(auc(roc_setosa), 3)),
         paste("Versicolor AUC =", round(auc(roc_versicolor), 3)),
         paste("Virginica AUC =", round(auc(roc_virginica), 3))
       ),
       col = c("orange", "yellow", "purple"),
       lwd = 2)

auc(roc_setosa)
auc(roc_versicolor)
auc(roc_virginica) 

# Feature Importance

importance <- varImp(model, scale = FALSE)
print(importance)

plot(importance, main = "Feature Importance")

# Actual vs Predicted Plot

results <- data.frame(
  Actual = testTransformed$Species,
  Predicted = predictions
)

ggplot(results, aes(x = Actual, fill = Predicted)) +
  geom_bar(position = "dodge") +
  labs(title = "Actual vs Predicted Species",
       x = "Actual Species",
       y = "Count") +
  theme_minimal()
