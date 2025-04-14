
library(simlite)

# following codes are from vignette of simlite
data("gut_experiment_subset", package="simlite")
print(dim(gut_experiment_subset))

# load real count matrix
count_data <- SummarizedExperiment::assay(gut_experiment_subset)
depths <- colSums(count_data)
min_depth <- min(depths)
normalization_factor <- min_depth / t(replicate(nrow(count_data), depths))
count_data_normalized <- count_data * normalization_factor
count_data_normalized <- round(count_data_normalized)
prevalences <- rowMeans(count_data_normalized > 0)

# randomly select 1000 taxa whose prevalence (proportion of nonzero counts) is larger than 50%
subset_features <- which(prevalences > 0.5)
subset_features <- sample(subset_features, 1000)
count_data_subset <- count_data_normalized[subset_features, ]


# fit negative binomial distribution
nb_params <- fitdistr_mat(count_mat=count_data_subset,
                          dist="nb", cutoff=0.97,
                          ncores=4)

# select 400 features for simulation
nfeatures <- 400
nb_means_1 <- nb_params[1:nfeatures, 1]
nb_means_2 <- nb_means_1
nb_means_3 <- nb_means_1


# change abundances of a subset of taxa to set up different phenotypes
foldchange <- function(vals, logfold=1){
  direction <- rbinom(n=length(vals), size=1, prob=0.5)
  direction <- (direction-0.5)*2 # change to 1 and -1

  lower_bound <- quantile(vals, 0.1) # if the original mean is small, always increase
  upper_bound <- quantile(vals, 0.9) # if the original mean is large, always decrease

  direction[vals < lower_bound] <- 1
  direction[vals > upper_bound] <- -1

  folds <- exp(logfold * direction)
  new_vals <- vals * folds
  return(new_vals)
}


nb_means_2[seq(1, 100)] <- foldchange(nb_means_2[seq(1, 100)])
nb_means_3[seq(101, 200)] <- foldchange(nb_means_3[seq(101, 200)])

nb_means_combined <- cbind(nb_means_1, nb_means_2, nb_means_3)


# simulate counts from negative binomial distribution
sim_nb_heterogeneous <- function(nb_means, size, sample_number){
  counts_list <- list()

  for (j in 1:length(sample_number)){
    copula <- gausscopula(n=sample_number[j], Sigma=diag(nrow=nfeatures))
    nb_params <-cbind(nb_means[, j], size)
    colnames(nb_params) <- c("mu", "size")
    nb_counts <- simcountmat(copula=copula, params=nb_params,
                             dist="nb",ncores=4)
    counts_list[[j]] <- nb_counts
  }

  counts_mat <- do.call(cbind, counts_list)
  return(counts_mat)

}


sample_number <- c(80, 60, 20)
nb_counts_mat_10 <- sim_nb_heterogeneous(nb_means=nb_means_combined,
                                      size=10,
                                      sample_number=sample_number)
nb_counts_mat_5 <- sim_nb_heterogeneous(nb_means=nb_means_combined,
                                         size=5,
                                         sample_number=sample_number)
nb_counts_mat_2 <- sim_nb_heterogeneous(nb_means=nb_means_combined,
                                        size=2,
                                        sample_number=sample_number)
nb_counts_mat_1 <- sim_nb_heterogeneous(nb_means=nb_means_combined,
                                        size=1,
                                        sample_number=sample_number)
nb_counts_mat_05 <- sim_nb_heterogeneous(nb_means=nb_means_combined,
                                        size=0.5,
                                        sample_number=sample_number)



# store the true mean,median
true_mean_1 <- replicate(sample_number[1], nb_means_1)
true_mean_2 <- replicate(sample_number[2], nb_means_2)
true_mean_3 <- replicate(sample_number[3], nb_means_3)
true_mean_mat <- cbind(true_mean_1, true_mean_2, true_mean_3)
true_mean_mat_t <- t(true_mean_mat)

nsample <- ncol(true_mean_mat)
nfeature <- nrow(true_mean_mat)

true_median_mat_05 <- matrix(0, nrow=nrow(true_mean_mat), ncol=ncol(true_mean_mat))
true_median_mat_1 <- matrix(0, nrow=nrow(true_mean_mat), ncol=ncol(true_mean_mat))
true_median_mat_2 <- matrix(0, nrow=nrow(true_mean_mat), ncol=ncol(true_mean_mat))
true_median_mat_5 <- matrix(0, nrow=nrow(true_mean_mat), ncol=ncol(true_mean_mat))
true_median_mat_10 <- matrix(0, nrow=nrow(true_mean_mat), ncol=ncol(true_mean_mat))


for(i in 1:nrow(true_mean_mat)){
  for(j in 1:ncol(true_mean_mat)){
    true_median_mat_05[i,j] <- qnbinom(0.5, size=0.5, mu=true_mean_mat[i,j])
    true_median_mat_1[i,j] <- qnbinom(0.5, size=1, mu=true_mean_mat[i,j])
    true_median_mat_2[i,j] <- qnbinom(0.5, size=2, mu=true_mean_mat[i,j])
    true_median_mat_5[i,j] <- qnbinom(0.5, size=5, mu=true_mean_mat[i,j])
    true_median_mat_10[i,j] <- qnbinom(0.5, size=10, mu=true_mean_mat[i,j])
  }
}


savemat <- function(mymat, filename){
  mydf <- data.frame(t(mymat))
  rownames(mydf) <- sprintf("Sample%d", seq(1, nrow(mydf)))
  colnames(mydf) <- sprintf("Feature%d", seq(1, ncol(mydf)))
  write.csv(mydf, sprintf("data/%s", filename), quote=FALSE)
}


savemat(true_median_mat_05, filename="nb_true_median_05.csv")
savemat(true_median_mat_1, filename="nb_true_median_1.csv")
savemat(true_median_mat_2, filename="nb_true_median_2.csv")
savemat(true_median_mat_5, filename="nb_true_median_5.csv")
savemat(true_median_mat_10, filename="nb_true_median_10.csv")

sample_types <- c(rep("Type1", sample_number[1]),
                  rep("Type2", sample_number[2]),
                  rep("Type3", sample_number[3]))
feature_types <- c(rep("Group1", 100),
                   rep("Group2", 100),
                   rep("Group3", 200))
write(sample_types, "data/sample_types.txt")
write(feature_types, "data/feature_types.txt")



savemat(true_mean_mat, filename="nb_true_mean.csv")
savemat(nb_counts_mat_05, filename="nb_count_05.csv")
savemat(nb_counts_mat_1, filename="nb_count_1.csv")
savemat(nb_counts_mat_2, filename="nb_count_2.csv")
savemat(nb_counts_mat_5, filename="nb_count_5.csv")
savemat(nb_counts_mat_10, filename="nb_count_10.csv")




