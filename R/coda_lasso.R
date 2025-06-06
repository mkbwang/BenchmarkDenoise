



#' Log Contrast Model
#'
#' @param y binary response variable
#' @param X predictor matrix
#' @param lambda penalization parameter
#' @param maxiter maximum number of iterations for the optimization loop
#' @param maxiter2 maximum number of iterations for the line search condition.
#' @param r fixed parameter used for the update step.
#' @param tol tolerance for the difference explained deviance in two consecutive steps.
#' @param tol2 tolerance to fulfill the constraint:  sum of betas being 1
#' @param printTime value to print computing time (works any value)
#' @importFrom stats glm deviance binomial
#' @export
coda_logistic_lasso  <-  function(y,X,lambda, maxiter=400, maxiter2=50, r=10,
                                  tol=1.e-4, tol2=1.e-6, printTime=NULL){

    # y: dependent variable, binary, vector of length n.
    # X: matrix of k covariates (positive values, taxa abundances in counts,
    #    proportions, intensities, ...), matrix of dimension n by k.
    # lambda : penalization parameter.
    # maxiter: maximum number of iterations for the optimization loop.
    # maxiter2: maximum number of iterations for the line search condition.
    # r: fixed parameter used for the update step.
    # tol: tolerance for the difference explained deviance in two consecutive steps.
    # tol2: tolerance to fulfill the constraint sum(beta[j])=0, for j>1
    # printTime: value to print computing time (works any value)

    #  library(MASS)  #only if ginv function is needed (not in our case)

    if (!is.numeric(y)){ #Check data
        y  <-  as.numeric(y)-1
    }

    # Transform initial data

    p  <-  ncol(X)   # p: number of covariates after filtering
    n  <-  nrow(X);
    # log transformation Z=log(X)
    z <- log(X)

    # z=matrix of covariates: add a first column of 1's for beta0
    z <- cbind(rep(1,nrow(z)),z)
    z <- as.matrix(z)

    # c=linear constraint sum(betas)=0 (except beta0)
    c <- c(0,rep(1,ncol(X)))
    c <- c/sqrt(normSqr(c))
    c <- as.matrix(c)
    p_c <- c%*%t(c)
    # p_cginv <- ginv(p_c);  #use this only if necessary
    p_c_cginv <- p_c; #p_c%*%p_cginv; #this product in our case is = p_c

    # z transformation (centering) for improvement of optimization
    # this transformation does not affect the estimation since the linear predictor is the same
    z <- (z-(z%*%p_c))
    colnames(z) <- c("beta0",colnames(X))


    # initial values for beta:
    # b0 related to mean(y) and uniform values for the other components
    beta_ini <- c(log(mean(y)/(1-mean(y))),rep(1/p,p))

    #null deviance
    nulldev <- glm(y~1, family=binomial())[[10]]


    #initialization parameters

    k <- 1
    epsilon <- 0.1
    beta_ant <- beta_ini
    beta <- beta_ant
    y_ant <- beta_ant
    y_k <- y_ant
    d_k <- y_ant
    dev_explained_ant <- 0


    # Optimization with constraint
    start_iter = Sys.time();
    #while(((epsilon) > tol) & (k <= maxiter)){
    while((abs(epsilon) > tol) & (k <= maxiter)){

        k0 <- 0
        t_k <- 10
        condition <- 0.1
        while ((condition > 0) & (k0 <= maxiter2)){
            k0 <- k0+1
            gradgyantzyn  <-  grad_g(y_ant, z, y, n)
            d_k  <-  y_ant-t_k*gradgyantzyn;

            # Soft thresholding:
            zproxi <- c(d_k[1],soft_thres(d_k[-1],lambda*t_k))

            # Projection:
            zproj <- projection(zproxi,p_c_cginv)

            # Line search condition
            Gt <- (y_ant-zproj)
            condition <- g(zproj,z, y, n)-g(y_ant,z, y, n)+t(gradgyantzyn)%*%Gt-normSqr(Gt)/(2*t_k)

            t_k <- t_k/2
        }
        beta <- zproj


        y_k <- beta+(k-1)/(k+r-1)*(beta-beta_ant) #updated value

        # dev_explained <- 1-(nrow(X)*2*g(beta,z, y, n)/nulldev)
        scoreb<-z%*%beta
        df<-data.frame(y,z)
        dev_explained<- 1-(deviance(glm(y ~ scoreb, data= df, family = binomial()))/nulldev)

        epsilon <- dev_explained-dev_explained_ant  #present difference deviation value
        # epsilon <- abs(dev_explained-dev_explained_ant)  #present difference deviation value

        y_ant <- y_k
        beta_ant <- beta
        dev_explained_ant <- dev_explained
        k <- k+1
    }

    end_iter = Sys.time();
    if (!is.null(printTime)){
        sprintf("iter time = %f",end_iter-start_iter)
    }

    #Projection of the optimal beta to fulfil the constraint sum(beta[j])=0, for j>1
    indx <- which(abs(zproxi)>tol2)
    if (abs(zproxi[1])>0) indx <- indx[-1]
    c0 <- rep(1,(length(indx)))
    c0 <- c0/sqrt(normSqr(c0))
    p_c0 <- c0%*%t(c0)
    if (ncol(p_c0)*nrow(p_c0)>0){
        p_c0ginv = p_c0 #ginv(p_c0); #use this only if necessary
    }else{
        p_c0ginv=0;
    }
    p_c0_c0ginv=p_c0;
    # p_c0_c0ginv=p_c0%*%p_c0ginv; #use this only if necessary
    #beta1
    beta1 <- as.numeric(projection(beta[indx],p_c0_c0ginv))
    beta_res <- c(beta[1],rep(0,p))
    beta_res[indx] <- beta1


    #dev_explained_beta_res <- 1-(nrow(X)*2*g(beta_res,z, y, n)/nulldev)
    scorebalance<-z%*%beta_res
    df<-data.frame(y,z)
    dev_explained_beta_res <- 1-(deviance(glm(y ~ scorebalance, data= df, family = binomial()))/nulldev)


    idselected<-which(abs(beta_res)>0)

    results <- list(
        "number of iterations" = k,
        "number of selected variables" = sum(abs(beta_res)>0)-1,
        "indices of selected variables" = idselected[-1]-1,
        "name of selected variables" = colnames(z)[idselected[-1]],
        "beta non-zero coefficients" = beta_res[abs(beta_res)>0],
        "proportion of explained deviance" = dev_explained_beta_res,
        "betas" = beta_res)

    return(results)

} # END function coda_logistic_lasso


##-----------------------------------------------------------------

norm1 <- function(x){
    return(sum(abs(x)))
}

normSqr <- function(x){
    return(sum(x^2))
}


mu_beta <- function(x, Z){
    zetabybeta <- Z%*%x
    res <- rep(1,length(zetabybeta))
    indzb <- which(zetabybeta<=100)
    res[indzb] <- exp(zetabybeta[indzb])/(1+exp(zetabybeta[indzb]))
    return(res)
}

g <- function(x,Z,Y,n){
    res <-  (t(Y))%*%Z%*%x
    aux=Z%*%x
    res <- res-sum(log(1+exp(aux)))
    res <-  as.vector((-res)/n)
    return(res)
}

grad_g <- function(x, Z, Y, n){
    res <-  (t(Y-mu_beta(x,Z)))%*%Z
    res <-  as.vector((-res)/n)
    return(res)
}

# Soft thresholding   http://www.simonlucey.com/soft-thresholding/

soft_thres <- function(b, lambda){
    x <- rep(0,length(b))
    # Set the threshold
    th = lambda/2;

    #First find elements that are larger than the threshold
    k  <-  which(b > th)
    x[k]  <-  b[k] - th

    # Next find elements that are less than abs
    k  <- which(abs(b) <= th)
    x[k]  <-  0

    # Finally find elements that are less than -th
    k  <- which(b < -th)
    x[k] = b[k] + th

    return(x)
}


# Projection function

projection <- function(x, M=NULL, Mginv=NULL){
    if (ncol(M)*nrow(M)>0){
        # res <- x-Mginv%*%M%*%x
        res <- x-M%*%x
    } else {res <- rep(0,length(x))}
    return(res)
}


trapezInteg  <-  function(x,y) {
    # Compute AUC using trapezoid numerical integration method
    n = length(x);
    sumArea = 0;
    for (i in 1:(n-1)){
        h=x[i+1]-x[i];
        if (abs(h) > 1.e-7){
            sumArea = sumArea + 0.5*(y[i+1]+y[i])*h;
        }else{
            sumArea = sumArea;
        }
    }
    return(sumArea)
}

lambdaRange_codalasso  <-  function(y,X,lambdaSeq=seq(0,1,0.01)){
    numVarAct=NULL;
    print(c("lambda", "num.selected", "prop.explained.dev"))
    for (lambda in lambdaSeq){
        results  <-  coda_logistic_lasso(y,X,lambda, maxiter = 100);
        numVarAct = c(numVarAct,results[[2]]);
        cat(sprintf("%.4f  %.0f  %.4f\n",lambda, results[[2]],results[[6]]))
    }
    #return(numVarAct);
}

