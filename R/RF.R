


#' Randomly split the data into train/test and apply random forest model for prediction
#'
#' @param X predictor matrix
#' @param y response variable
#' @param seed random seed for spliting train/test data
#' @returns a list with the training AUROC, test AUROC and the variable importance
#' @importFrom parallelly availableCores
#' @importFrom parallel makeCluster stopCluster
#' @importFrom doParallel registerDoParallel
#' @importFrom caret createDataPartition train trainControl twoClassSummary varImp
#' @importFrom pROC roc auc
#' @importFrom randomForest randomForest
#' @importFrom stats predict
#' @export
fit_rf <- function(X, y, seed=20){


    cl <- makeCluster(availableCores() - 1)  # use one fewer than available cores
    registerDoParallel(cl)


    control <- trainControl(method = "cv",
                            number = 5,
                            summaryFunction = twoClassSummary,
                            classProbs = TRUE,        # needed for ROC
                            verboseIter = FALSE)
    tuneGrid <- expand.grid(mtry = c(2,3,4,5,6))

    set.seed(seed)
    trainIndex <- createDataPartition(y, p = 0.8, list = FALSE)
    trainData <- X[trainIndex, ]
    testData  <- X[-trainIndex, ]
    train_labels <- y[trainIndex]
    test_labels <- y[-trainIndex]

    model.rf <- train(x=trainData,
                      y=as.factor(train_labels),
                      method = "rf",
                      metric = "ROC",    # optimize for AUROC
                      tuneGrid = tuneGrid,
                      trControl = control)

    vimp_result <- varImp(model.rf)
    var_importance <- vimp_result$importance$Overall

    train_auc <- max(model.rf$results$ROC)
    predProbs <- predict(model.rf, newdata = testData, type = "prob")
    test_auc <- roc(response = test_labels, predictor = predProbs[, 1]) |> auc() |>
        suppressMessages()
    if (test_auc < 0.5) test_auc <- 1-test_auc

    stopCluster(cl)

    return(list(train_auc=train_auc, test_auc=test_auc,
                variable_importance=var_importance))

}
