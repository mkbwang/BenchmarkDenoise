


expit <- function(xval) {
    1/(1+exp(-xval))
}


#' Fit coda lasso multiple times with random train/test splitting. Cross validation involved
#' @param y_train response variable of training data
#' @param X_train predictor variable matrix of training data
#' @param y_test response variable of test data
#' @param X_test predictor variable matrix of test data
#' @param lambdas range of lambdas to try
#' @param seed random seed that decides train test split
#' @returns list of training AUC, test AUC and coefficients of all runs
#' @export
fit_codalasso <- function(y_train, X_train, y_test, X_test, lambdas=seq(0.05, 0.55, 0.1), seed=1){

    # set.seed(seed)
    # trainIndex <- createDataPartition(y, p = train_prop, list = FALSE)
    # trainData <- X[trainIndex, ]
    # testData  <- X[-trainIndex, ]
    # train_labels <- y[trainIndex]
    # test_labels <- y[-trainIndex]

    # cross validation to select lambda
    if (length(lambdas) > 1){
        selected_lambda <- codalasso_lambda_tune(y=y_train, X=X_train, nfolds=5,
                                                 lambdas=lambdas, seed=seed)
    } else{
        selected_lambda <- lambdas
    }


    # fit final model
    final_model <- coda_logistic_lasso(y=y_train, X=X_train, lambda=selected_lambda)
    intercept <- final_model$betas[1]
    coefs <- final_model$betas[-1]

    predicted_links_train <- intercept + log(as.matrix(X_train)) %*% coefs
    predicted_probs_train <- expit(predicted_links_train)
    train_auc <- auc(roc(y_train, as.vector(predicted_probs_train))) |> suppressMessages()

    predicted_links_test <- intercept + log(as.matrix(X_test)) %*% coefs
    predicted_probs_test <- expit(predicted_links_test)
    test_auc <- auc(roc(y_test, as.vector(predicted_probs_test))) |> suppressMessages()


    return(list(train_auc=train_auc, test_auc=test_auc, lambda=selected_lambda,
                coefs=coefs))
}

#' split into train and validation multiple times
#'
#' @param y response variable
#' @param X predictor variable matrix
#' @param lambda penalty parameter
#' @param seed random seed for train validation split
#' @param nfolds number of folds for cross validation
#' @importFrom caret createFolds
#' @importFrom pROC auc roc
#' @returns list of training AUC, validation AUC and coefficients of all runs
#' @export
cv.codalasso <- function(y, X, lambda, nfolds = 5, seed=1){

    train_aucs <- rep(0, nfolds)
    test_aucs <- rep(0, nfolds)
    coef_mat <- matrix(0, nrow=nfolds, ncol=ncol(X))

    set.seed(seed)
    fold_indices <- createFolds(y=y, k=nfolds, list=TRUE, returnTrain=FALSE)

    for(j in 1:nfolds){

        train_labels <- y[-fold_indices[[j]]]
        test_labels <- y[fold_indices[[j]]]
        train_features <- X[-fold_indices[[j]], ]
        test_features <- X[fold_indices[[j]], ]

        model <-  coda_logistic_lasso(y=train_labels, X=train_features, lambda=lambda)
        coef_mat[j, ] <- model$betas[-1]

        predicted_links_train <- model$betas[1] +
            log(as.matrix(train_features)) %*% model$betas[-1]
        predicted_probs_train <- expit(predicted_links_train)
        train_aucs[j] <- auc(roc(train_labels, as.vector(predicted_probs_train))) |> suppressMessages()

        predicted_links_test <- model$betas[1] +
            log(as.matrix(test_features)) %*% model$betas[-1]
        predicted_probs_test <- expit(predicted_links_test)
        test_aucs[j] <- auc(roc(test_labels, as.vector(predicted_probs_test))) |> suppressMessages()

    }
    return(list(train_aucs=train_aucs, test_aucs=test_aucs, coef_mat=coef_mat))

}


#' Select best lambda for codalasso based on cross validation
#'
#' @param y response variable
#' @param X predictor variable matrix
#' @param lambdas penalty parameter range
#' @param nfolds number of folds for cross validation
#' @param seed random seed for train validation split
#' @importFrom stats sd
#' @returns optimal lambda
codalasso_lambda_tune <- function(y, X, lambdas=seq(0.05, 0.55, 0.1), nfolds=5,
                                  seed=1){


    train_auc_mean <- rep(0, length(lambdas))
    train_auc_sd <- rep(0, length(lambdas))
    test_auc_mean <- rep(0, length(lambdas))
    test_auc_sd <- rep(0, length(lambdas))
    # num_features_mean <- rep(0, length(lambdas))
    # num_features_sd <- rep(0, length(lambdas))
    # instability <- rep(0, length(lambdas))
    # choice_probs <- matrix(0, nrow=length(lambdas),
    #                        ncol=ncol(X))

    for (j in 1:length(lambdas)){

        result <- cv.codalasso(y=y, X=X, lambda=lambdas[j], nfolds=nfolds, seed=seed)

        train_auc_mean[j] <- mean(result$train_aucs)
        train_auc_sd[j] <- sd(result$train_aucs)
        test_auc_mean[j] <- mean(result$test_aucs)
        test_auc_sd[j] <- sd(result$test_aucs)

        # selection_probability <- colMeans(result$coef_mat != 0)
        # num_features <- rowSums(result$coef_mat != 0)
        # num_features_mean[j] <- mean(num_features)
        # num_features_sd[j] <- sd(num_features)
        #
        # choice_probs[j, ] <- selection_probability
        # instability[j] <- mean(selection_probability * (1-selection_probability))

    }

    best_lambda <- lambdas[which.max(test_auc_mean)]
    # output <- list(lambdas=lambdas,train_auc_mean=train_auc_mean, train_auc_sd=train_auc_sd,
    #                test_auc_mean=test_auc_mean, test_auc_sd=test_auc_sd,
    #                num_features_mean=num_features_mean, num_features_sd=num_features_sd,
    #                instability=instability, choice_probs=choice_probs)

    return(best_lambda)
}


