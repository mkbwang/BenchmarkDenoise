

#' Fit lasso logistic regression
#'
#' @param X predictor matrix
#' @param y binary response vector
#' @param seed train test seed
#' @importFrom glmnet cv.glmnet
#' @importFrom stats rbinom coef predict
#' @importFrom pROC roc auc
fit_logistic_lasso <- function(X, y, seed=1){

    set.seed(seed)
    index_positive <- which(y == TRUE)
    split_mask <- rbinom(n=length(index_positive), 1, prob=0.8)
    train_positive <- index_positive[split_mask == 1]
    test_positive <- index_positive[split_mask == 0]

    index_negative <- which(y == FALSE)
    split_mask <- rbinom(n=length(index_negative), 1, prob=0.8)
    train_negative <- index_negative[split_mask == 1]
    test_negative <- index_negative[split_mask == 0]

    train_labels <- c(y[train_positive], y[train_negative]) |> as.integer()
    test_labels <- c(y[test_positive], y[test_negative]) |> as.integer()

    train_data <- X[c(train_positive, train_negative), ]
    test_data <- X[c(test_positive, test_negative), ]

    cv_fit <- cv.glmnet(x=train_data, y=train_labels, family="binomial",
                        alpha=1, type.measure = "auc", nfolds=5)

    optimal_coefs <- coef(cv_fit, s = "lambda.min") |> as.vector()

    predicted_probabilities_train <-
        predict(cv_fit, s = "lambda.min", newx = train_data, type = "response") |> as.vector()

    roc_obj <- roc(train_labels, predicted_probabilities_train)
    train_auc_value <- auc(roc_obj) |> as.numeric()

    predicted_probabilities_test <-
        predict(cv_fit, s = "lambda.min", newx = test_data, type = "response") |> as.vector()

    roc_obj <- roc(test_labels, predicted_probabilities_test)
    test_auc_value <- auc(roc_obj) |> as.numeric()

    output <- list(coefs=optimal_coefs, train_auc=train_auc_value, test_auc=test_auc_value)
    return(output)

}







