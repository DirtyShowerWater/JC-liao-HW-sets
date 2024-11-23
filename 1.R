library(haven)
library(sandwich)
library(lmtest)
library(dplyr)
library(tidyverse)
library(AER)
setwd("C:/Users/Chris/Desktop/HW3/112446-V1")
Data<-read.csv("C:/Users/Chris/Desktop/HW3/DDK2011.csv")
student_test_data <- read_dta("student_test_data.dta")
for_peer_papers <-read_dta("forpeerpapers.dta")
#1.
#迴歸係數
model = lm(stdR_totalscore ~ tracking + bottomhalf + bottomhalf_tracking 
            + girl + percentile + agetest + etpteacher, 
            data= for_peer_papers)

summary(model)
#Total effects, F-statistic, P-value
cl_vcov<-vcovCL(model,cluster=~schoolid)
linearHypothesis(model, c("tracking + bottomhalf_tracking = 0"), vcov. = cl_vcoc)

#2. (未完成)
#bootstrap
set.seed(123)
n_boot <- 1000
# Function to bootstrap the linear model grouped by "schoolid"
bootstrap_lm <- function(data, n_boot = 1000) {
  # Unique school IDs
  unique_schools <- unique(data$schoolid)
  
  # Store bootstrap results
  bootstrap_results <- list()
  
  for (i in 1:n_boot) {
    # Resample entire clusters
    resampled_schools <- sample(unique_schools, replace = TRUE)
    resampled_data <- do.call(rbind, lapply(resampled_schools, function(sid) {
      data[data$schoolid == sid, ]
    }))
    
    # Fit the linear model on the resampled dataset
    fit <- tryCatch(
      lm(stdR_totalscore ~ tracking + bottomhalf + bottomhalf_tracking +
           girl + percentile + agetest + etpteacher, data = resampled_data),
      error = function(e) NULL # Handle errors gracefully
    )
    
    # Extract coefficients if the model fit was successful
    if (!is.null(fit)) {
      bootstrap_results[[i]] <- coef(fit)
    } else {
      bootstrap_results[[i]] <- rep(NA, length(coef(lm(stdR_totalscore ~ 
                                                         tracking + bottomhalf + bottomhalf_tracking + 
                                                         girl + percentile + agetest + etpteacher, 
                                                       data = data))))
    }
  }
  # Convert bootstrap results into a data frame
  bootstrap_df <- do.call(rbind, bootstrap_results)
  colnames(bootstrap_df) <- names(coef(lm(stdR_totalscore ~ 
                                            tracking + bottomhalf + bottomhalf_tracking + 
                                            girl + percentile + agetest + etpteacher, 
                                          data = data)))
  
  return(bootstrap_df)
}

bootstrap_results <- bootstrap_lm(for_peer_papers, n_boot)

# Summary of bootstrap results
summary(bootstrap_results)

# Calculate variance and standard error from bootstrap results
bootstrap_summary <- function(bootstrap_results) {
  # Variance
  variances <- apply(bootstrap_results, 2, var, na.rm = TRUE)
  
  # Standard Error
  std_errors <- sqrt(variances)
  
  # Combine into a data frame for easy interpretation
  summary_df <- data.frame(
    Coefficient = colnames(bootstrap_results),
    Variance = variances,
    Std_Error = std_errors
  )
  
  return(summary_df)
}

# Generate summary for bootstrap results
bootstrap_stats <- bootstrap_summary(bootstrap_results)

# Print the summary
print(bootstrap_stats)

