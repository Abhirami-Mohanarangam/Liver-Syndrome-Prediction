library(randomForest)

rnd.forest.model <- randomForest(formula = formula.chosen, data = training.data, ntree = 500, mtry=8)
print(rnd.forest.model)

predicted.rnd.forest <- predict(rnd.forest.model, test.data)
print(predicted.rnd.forest)

print(cat("Root Mean Squared Log Error for model with chosen features in Random Forest: ", red(bold(rmsle(as.numeric(test.data$Result), as.numeric(predicted.rnd.forest))))))
print(cat(blue("Classification Error"), "for model with chosen features in Random Forest: ", red(bold(ce(as.numeric(test.data$Result), as.numeric(predicted.all))))))

temp <- factor(predicted.rnd.forest)
print(ggplot(mapping = aes(1:100)) +
        geom_point(aes(x = 1:length(temp), y = temp, colour = "Predicted"), alpha = 0.8) +
        geom_point(aes(x = 1:length(test.data$Result), y = test.data$Result, colour = "Actual"), alpha = 0.5) +
        ylab("") + xlab("") +
        ggtitle("Test Data Predictions (Random Forest)"))

cnfsn.matrix.rnd.forest <- confusionMatrix(predicted.rnd.forest, test.data$Result, positive = "Yes")
print(cnfsn.matrix.rnd.forest)
print(ggplot(data = data.frame(cnfsn.matrix.rnd.forest$table), aes(Prediction, Reference, fill=c(cnfsn.matrix.rnd.forest$table), colour=Prediction)) + geom_tile())


print(cat(blue("Accuracy:"), green(cnfsn.matrix.rnd.forest[["overall"]][1])))
                    # 68% of the entire population will be diagnosed correctly
print(cat(blue("Precision:"), green(cnfsn.matrix.rnd.forest[["byClass"]][5])))
                    # 75% of the predictions as YES is correct
print(cat(blue("Recall:"), green(cnfsn.matrix.rnd.forest[["byClass"]][6])))
                    # 84% of the people with liver disease will be diagnosed correctly
