


#' Fit LASSO logistic regression
#'
#' @param input input predictor matrix
#' @param output binary response
#' @param train_prop proportion of data as training
#' @param seed random seed for spliting data into train and test
#'
#' @returns a list with coefficients and train AUC, test AUC
#'
#' @importFrom caret createDataPartition createFolds
#' @importFrom glmnet cv.glmnet
#' @importFrom stats coef predict
#' @importFrom pROC roc auc
#'
#' @export
fit_logistic_lasso <- function(input, output, train_prop=0.7, seed=1){

    set.seed(seed)
    train_indices <- createDataPartition(output, p=train_prop, list=FALSE)

    train_labels <- as.integer(output[train_indices])
    test_labels <- as.integer(output[-train_indices])

    train_data <- input[train_indices, ]
    test_data <- input[-train_indices, ]

    # create cross validation folds
    folds <- createFolds(train_labels, k = 5)
    # Convert list of folds into a numeric vector where each element indicates the fold ID
    foldid <- rep(NA, length(train_labels))
    for(i in seq_along(folds)) {
        foldid[folds[[i]]] <- i
    }

    # cross validation
    cv_fit <- cv.glmnet(x=train_data, y=train_labels, family="binomial",
                        alpha=1, type.measure = "auc",
                        foldid=foldid)
    optimal_coefs <- coef(cv_fit, s = "lambda.min") |> as.vector()

    predicted_probabilities_train <-
        predict(cv_fit, s = "lambda.min", newx = train_data, type = "response") |> as.vector()

    roc_obj <- roc(train_labels, predicted_probabilities_train) |> suppressMessages()
    train_auc_value <- auc(roc_obj) |> as.numeric() |> suppressMessages()

    predicted_probabilities_test <-
        predict(cv_fit, s = "lambda.min", newx = test_data, type = "response") |> as.vector()

    roc_obj <- roc(test_labels, predicted_probabilities_test) |> suppressMessages()
    test_auc_value <- auc(roc_obj) |> as.numeric() |> suppressMessages()

    output <- list(coefs=optimal_coefs, train_auc=train_auc_value, test_auc=test_auc_value)
    return(output)

}



