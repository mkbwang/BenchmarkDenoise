


#' Randomly split the data into train/test and apply random forest model for prediction
#'
#' @param X predictor matrix
#' @param y response variable
#' @param times how many train/test split to try
#' @returns a list with the training AUROC, test AUROC and the variable importance
#' @importFrom parallelly availableCores
#' @importFrom parallel makeCluster stopCluster
#' @importFrom doParallel registerDoParallel
#' @importFrom caret createDataPartition train trainControl twoClassSummary varImp
#' @importFrom pROC roc auc
#' @importFrom randomForest randomForest
#' @importFrom stats predict
#' @export
repeat_rf <- function(X, y, times=20){


    cl <- makeCluster(availableCores() - 1)  # use one fewer than available cores
    registerDoParallel(cl)

    train_roc <- rep(0, times)
    test_roc <- rep(0, times)


    control <- trainControl(method = "cv",
                            number = 5,
                            summaryFunction = twoClassSummary,
                            classProbs = TRUE,        # needed for ROC
                            verboseIter = FALSE)
    tuneGrid <- expand.grid(mtry = c(2,3,4,5,6))
    var_importance_mat <- matrix(0, nrow=times, ncol=ncol(X))
    for (k in 1:times){
        print(k)
        set.seed(k)
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
        var_importance_mat[k, ] <- vimp_result$importance$Overall

        train_roc[k] <- max(model.rf$results$ROC)
        predProbs <- predict(model.rf, newdata = testData, type = "prob")
        roc_obj <- roc(response = test_labels, predictor = predProbs[, 1])
        auroc <- auc(roc_obj)
        if (auroc < 0.5) auroc <- 1-auroc
        test_roc[k] <- auroc

    }

    stopCluster(cl)

    return(list(Train=train_roc, Test=test_roc, Variable_importance=var_importance_mat))

}
