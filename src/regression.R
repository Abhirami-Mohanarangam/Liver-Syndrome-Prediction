library(Metrics)
library(caret)

for.prediction <- liver.data[575:583,]
for.model <- liver.data[1:574,]

training.size <- floor(0.85 * nrow(for.model))

set.seed(24)
training.indices <- sample(seq_len(nrow(for.model)), size = training.size)        # sample() from package 'dplyr'

training.data <- liver.data[training.indices,]
test.data <- liver.data[-training.indices,]

formula.all <- with(training.data, (Result ~ Age + Gender + TotBrubin + DirBrubin + AlkPhosph + AlamAmino + AspAmino + TotProt + Albumin + AlbGlobRat))

formula.chosen <- with(training.data, (Result ~ Age + Gender + TotBrubin + AlkPhosph + AlamAmino + TotProt + Albumin + AlbGlobRat))

# ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- #

getResult <- function (prob) {
  if (prob < 0.47) {
    return(1)
  } else {
    return(2)
  }
}


# Logistic Regression Model with all independent variables

logit.model.all <- with(training.data, glm(formula = formula.all, family = "binomial", data = training.data))
print(summary(logit.model.all))
predicted.all <- predict(logit.model.all, test.data, type = "response")

for (i in 1:length(predicted.all)) {
  predicted.all[i] = getResult(predicted.all[i])
}
# print(predicted.all)
print(cat("Root Mean Squared Log Error for model with chosen features: ", red(bold(rmsle(as.numeric(test.data$Result), predicted.all)))))
                              # rmsle() from package
print(cat(blue("Classification Error"), "for model with chosen features: ", red(bold(ce(as.numeric(test.data$Result), as.numeric(predicted.all))))))

temp <- factor(predicted.all, levels=c(1, 2), labels=c("Yes", "No"))

print(ggplot(mapping = aes(1:100)) +
        geom_point(aes(x = 1:length(temp), y = temp, colour = "predicted"), alpha = 0.8) +
        geom_point(aes(x = 1:length(test.data$Result), y = test.data$Result, colour = "actual"), alpha = 0.5) +
        ylab("") + xlab("") +
        ggtitle("Test Data Predictions (Logistic Regression for all features)"))


cnfsn.matrix.all <- confusionMatrix(temp, test.data$Result, positive = "Yes")         # Confusion Matrix form package 'caret'
print(cat(blue(bold(underline("Confusion Matrix")))))
print(cnfsn.matrix.all$table)
print(ggplot(data = data.frame(cnfsn.matrix.all$table), aes(Prediction, Reference, fill=c(cnfsn.matrix.all$table), colour=Prediction)) + geom_tile())

print(cat(blue("Accuracy:"), green(cnfsn.matrix.all[["overall"]][1])))
# 78% of the entire population will be diagnosed correctly
print(cat(blue("Precision:"), green(cnfsn.matrix.all[["byClass"]][5])))
# 80% of the predictions as YES is correct
print(cat(blue("Recall:"), green(cnfsn.matrix.all[["byClass"]][6])))
# 91% of the people with liver disease will be diagnosed correctly


# ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- #


# Logistic Regression Model with selected independent variables

logit.model.chosen <- with(training.data, glm(formula = formula.chosen, family = "binomial", data = training.data))
print(summary(logit.model.chosen))

predicted.chosen <- predict(logit.model.chosen, test.data, type = "response")

for (i in 1:length(predicted.chosen)) {
  predicted.chosen[i] = getResult(predicted.chosen[i])
}
# print(predicted.chosen)
print(cat("Root Mean Squared Log Error for model with chosen features: ", red(bold(rmsle(as.numeric(test.data$Result), predicted.chosen)))))
print(cat(blue("Classification Error"), "for model with chosen features: ", red(bold(ce(as.numeric(test.data$Result), as.numeric(predicted.chosen))))))
# print(cat("Log Loss for model with chosen features: ", red(bold(ll(as.numeric(test.data$Result), as.numeric(predicted.chosen))))))

temp <- factor(predicted.chosen, levels=c(1, 2), labels=c("Yes", "No"))
print(ggplot(mapping = aes(1:100)) +
        geom_point(aes(x = 1:length(temp), y = temp, colour = "Predicted"), alpha = 0.8) +
        geom_point(aes(x = 1:length(test.data$Result), y = test.data$Result, colour = "Actual"), alpha = 0.5) +
        ylab("") + xlab("") +
        ggtitle("Test Data Predictions (Logistic Regression with chosen features)"))

cnfsn.matrix.chosen <- confusionMatrix(temp, test.data$Result, positive = "Yes")
print(cat(blue(bold(underline("Confusion Matrix")))))
print(cnfsn.matrix.chosen$table)
print(ggplot(data = data.frame(cnfsn.matrix.chosen$table), aes(Prediction, Reference, fill=c(cnfsn.matrix.chosen$table), colour=Prediction)) + geom_tile())

print(cat(blue("Accuracy:"), green(cnfsn.matrix.chosen[["overall"]][1])))
# 80% of the entire population will be diagnosed correctly
print(cat(blue("Precision:"), green(cnfsn.matrix.chosen[["byClass"]][5])))
# 81% of the people with liver disease will be diagnosed correctly
print(cat(blue("Recall:"), green(cnfsn.matrix.chosen[["byClass"]][6])))
# 93% of the predictions as YES is correct


# ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- #

print(getResult(predict(logit.model.all, for.prediction[3,][c(1:10)], type="response")))
print(getResult(predict(logit.model.chosen, for.prediction[3,][c(1:3, 5:6, 8:10)], type="response")))


