###############SPM functions###############

pointwise_t_test <- function(data1, data2, var.equal = FALSE, c_level = 0.95) {
  
  
  # For each domain point (column), perform a t-test comparing the two groups
  res <- sapply(1:ncol(data1), function(i) {
    test <- t.test(data1[, i], data2[, i], var.equal = var.equal,
                   conf.level = c_level)
    
    # Return the t-statistic, degrees of freedom, and p-value for this column
    c(statistic = test$statistic,
      df = test$parameter, 
      p_value = test$p.value)
  })
  
  # Transpose and convert to list
  results_list <- as.list(as.data.frame(t(res)))
  names(results_list) <- c("statistic", "df", "p_value")
  
  return(results_list)
}


estimate_fwhm <- function(R) {
  
  # Sum of squares across rows for each column
  ssq <- colSums(R^2)
  
  # Machine epsilon for numerical stability
  eps <- .Machine$double.eps
  
  n_rows <- nrow(R)
  n_cols <- ncol(R)
  
  # Initialize matrix to store approximated derivatives (gradient) along columns
  dx <- matrix(NA, nrow = n_rows, ncol = n_cols)
  
  # Compute gradient along columns for each row:
  for (i in 1:n_rows) {
    # Forward difference for the first column
    dx[i, 1] <- R[i, 2] - R[i, 1]
    # Backward difference for the last column
    dx[i, n_cols] <- R[i, n_cols] - R[i, n_cols - 1]
    # Central differences for interior columns (if available)
    if (n_cols > 2) {
      dx[i, 2:(n_cols - 1)] <- (R[i, 3:n_cols] - R[i, 1:(n_cols - 2)]) / 2
    }
  }
  
  # Sum the squared gradients across rows for each column
  v <- colSums(dx^2)
  
  # Normalize by the column-wise sum of squares (plus eps for stability)
  v <- v / (ssq + eps)
  
  # Remove any NaN values (in case some columns had zero variance)
  v <- v[!is.na(v)]
  
  # Compute resels per node (using the relation with FWHM)
  reselsPerNode <- sqrt(v / (4 * log(2)))
  
  # The global FWHM estimate is the reciprocal of the average resels per node
  FWHM <- 1 / mean(reselsPerNode)
  
  return(FWHM)
}


residuals_data <- function(data1, data2) {
  # Subtract the column-wise means of each group
  r1 <- data1 - matrix(colMeans(data1), nrow = nrow(data1), ncol = ncol(data1), byrow = TRUE)
  r2 <- data2 - matrix(colMeans(data2), nrow = nrow(data2), ncol = ncol(data2), byrow = TRUE)
  return(rbind(r1, r2))
}


SPM <- function(data1, data2, variance.equal = FALSE, Clevel = 0.95){
  
  ss1 <- dim(data1)[1]
  ss2 <- dim(data2)[1]
  Q <- dim(data1)[2]
  
  test_stat <- (pointwise_t_test(data1, data2, var.equal = variance.equal, c_level = Clevel)$statistic)^2
  
  D0 <- 1-pf(q = test_stat, df1 = 1, df2 = ss1+ss2-2)
  
  calculate_expression <- function(z, k, v) {
    FOUR_LOG2 <- 4 * log(2)
    TWO_PI <- 2 * pi
    a <- FOUR_LOG2 / TWO_PI
    b <- lgamma(v / 2) + lgamma(k / 2)
    
    result <- (
      sqrt(a) * exp(lgamma((v + k - 1) / 2) - b) * sqrt(2) *
        (k * z / v) ^ (0.5 * (k - 1)) *
        (1 + k * z / v) ^ (-0.5 * (v + k - 2))
    )
    
    return(result)
  }
  
  D1 <- calculate_expression(test_stat, 1, ss1+ss2-2)
  R1 <- (Q-1)/estimate_fwhm(residuals_data(data1, data2))
  p<- 1-exp(-(D0 + D1 * R1))
  
  #benferroni correction
  p_b <- 1-pf(q = test_stat, df1 = 1, df2 = ss1+ss2-2)
  correct_Q <- unlist(lapply(p_b, function(x) min(x*Q,1)))
  # Ensures P isn't too small by using Q=1 as a lower bound.
  correct_1 <- unlist(lapply(p_b, function(x) min(x*1,1)))
  # compare correct and p elementwise and return min
  p_final_step1 <- unlist(lapply(1:length(p), function(x) min(p[x],correct_Q[x])))
  # Apply Lower Bound
  p_final_step2 <- unlist(lapply(1:length(p), function(x) max(correct_1[x],p_final_step1[x])))
  
  
  return(p_final_step2)
}