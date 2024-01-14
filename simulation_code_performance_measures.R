
rm(list=ls())

getwd()

inst_packages<-c("tidyverse","metafor","multcomp","dplyr","tibble","lme4","blme","MuMIn","data.table", "nortest","ggplot2","boot", "readxl")
for(i in inst_packages){
  if(!i %in% rownames(installed.packages())){
    install.packages(i)
  }
  library(i,character.only=T)
}



## setting up the simulation study

results_list <- list()
n_sims <- 3000 # Number of simulations
index <- 1

# before running the loop, please first change specify the parameters in the data. Parameters are 


#run the simulation loop-------



for(i in 1:n_sims){
# function to create a timeseries:
  create_ts <- function(mpn,a,SD1,random_intercept,random_slope){
      ## mpn = number of time points ## a = autocorrelation
    timeseries=c()
    point1=rnorm(1,random_intercept,SD1) #pick 1 pt from stand         normal distribution of 0, sd= 1 
    timeseries=c(timeseries,point1) #run it an you'll see that their is no autocorrelation.   𝑎𝑥(𝑡−1) + 𝜀𝑡
    for (i in 2:mpn){ #for loop
      point=a*timeseries[i-1] + 
        (1-a)*(random_slope + rnorm(1,0,SD1))
      timeseries=c(timeseries,point)
    }

    return(timeseries)
  }
  
  # Generale functions to create the data from the previous function used to create a simple time series.
  
  create_dat <-function(timeseries, SD3int, SD3treat, smd1,smd2,smd3, np_1, np_2,condition1,condition2){
    study_randomIntercept<-rnorm(1,0,SD3int)
    study_randomSlope<-rnorm(1,0,SD3treat)
    phaselength <- np_1 + np_2
    p1 <- timeseries[1:np_1] + smd1+study_randomIntercept
    p2 <- timeseries[(np_1 + 1):phaselength] + smd2+smd3 + study_randomSlope
    labels <- c(rep(condition1, np_1), rep(condition2, np_2))
    dat <- list(labels, c(p1, p2))
    return(dat)
  }
  
  
  create_study_group <- function(n, n_cases, mpn, a, SD1, SD2int, SD2treat, SD3int, SD3treat, np_1, np_2, smd1, smd2, smd3, condition1, condition2) {
    study_group <- list()
    for (study in 1:n) {
      dat_list <- list()
      for (j in 1:n_cases) {
        random_intercept=rnorm(1,0,SD2int)
        random_slope = rnorm(1,0,SD2treat)
        timeseries <- create_ts(mpn, a, SD1, random_intercept, random_slope)
        dat <- create_dat(timeseries, SD3int, SD3treat, smd1, smd2, smd3, np_1, np_2, condition1, condition2)
        dat_list[[j]] <- dat
      }
      study_group[[study]] <- dat_list
    }
    return(study_group)
  }
  
  
  
  #Function to convert the list to data frame
  list_to_df <- function(dat_list, study_no, case_no) {
    df <- data.frame()
    for (k in 1:length(dat_list)){
      study <- dat_list[[k]]
      for (c in seq_along(study)){
        case <- study[[c]]
        df_case <- data.frame(study_id = study_no,
                              case_id = case_no,
                              condition = case[[1]],
                              score = case[[2]])
        df <- rbind(df, df_case)
        case_no <- case_no + 1
      }
      study_no <- study_no + 1
    }
    colnames(df)=c("study_id","case_id", "condition_id","score")
    return(list(df = df, next_study_no = study_no, next_case_no = case_no))
  }
  
  
  #parameter settings=======
  # No1w we can use the create_study_group function to create each study group
l_ab <- create_study_group(n=50, n_cases=5, mpn=20, a=0.2, SD1=0.5, SD2int=2.5, SD2treat=2.7, SD3int=0.9, SD3treat=2.4, np_1=10, np_2=10, smd1=0, smd2=0.9, smd3=0, condition1=0, condition2=1)
  
l_bc <- create_study_group(n=50, n_cases=5, mpn=20, a=0.2, SD1=0.5, SD2int=2.5, SD2treat=2.7, SD3int=0.9, SD3treat=2.4, np_1=10, np_2=10, smd1=0.9, smd2=0, smd3=1.6, condition1=1, condition2=2) 

l_ac <- create_study_group(n=50, n_cases=5, a=0.2, mpn=20, SD1=0.5, SD2int=2.5, SD2treat=2.7, SD3int=0.9, SD3treat=2.4, np_1=10, np_2=10, smd1=0, smd2=1.6, smd3=0, condition1=0, condition2=2)
  
  
  
  
  # initialise study_no and case_no to 1
  study_no<-1
  case_no<-1
  
  # Convert each list to a data frame
  
  list_df_ab<-list_to_df(l_ab,study_no,case_no)
  df_ab <- list_df_ab$df
  study_no<-list_df_ab$next_study_no
  case_no<-list_df_ab$next_case_no
  
  
  list_df_bc <- list_to_df(l_bc, study_no, case_no)
  df_bc <- list_df_bc$df
  study_no <- list_df_bc$next_study_no
  case_no<-list_df_bc$next_case_no
  
  list_df_ac <- list_to_df(l_ac, study_no, case_no)
  df_ac <- list_df_ac$df
  df_ac
  # Add 'design' column to each dataframe
  df_ab$design <- 1
  df_bc$design <- 2
  df_ac$design <- 3
  
  # Combine all data frames
  simple_dat <- rbind(df_ab, df_bc, df_ac)
  
  # split dataframe to change the order of conditions in design == 3 
  
  df_design_3 <- simple_dat %>% filter(design == 3)
  df_other <-simple_dat %>% filter(design != 3)
  
  # Arrange the data frame
  df_design_3 <- df_design_3 %>%
    arrange(case_id, desc(condition_id))
  simple_dat<-rbind(df_other,df_design_3)
  
  
  # 2. NETWORK META-ANALYSIS - Code adapted from Barbosa Mendes et al (2021)-----
  
  
  # calculating the effect sizes by individual cases nested within studies:
  
  # OLS on each case seperately
  out = with(simple_dat,
             by(simple_dat, case_id, function(x) lm(score ~ 1 + condition_id, data=x))) 
  (outsum = lapply(out, summary))
  coefmat = sapply(outsum, coef)
  coeffs = t(coefmat)
  (sigma = sapply(outsum, `[`, 'sigma')) #extract residual standard error
  (sigma = as.data.frame(sigma)) #puts sigma into a dataframe.
  sigma = t(sigma)  #coerces the element into a matrix, so needs to be transformed into dataframe again
  sigma = as.data.frame(sigma)
  coeffs = as_tibble(coeffs, rownames = "case_id", .name_repair = "unique")
  coeffs$sigma=sigma$V1
  coeffs$stdES = coeffs$...2/coeffs$sigma
  
  ### Adding the effect sizes (b1)  to the main dataframe
  metadat = simple_dat %>%
    distinct(case_id, .keep_all = TRUE)
  metadat$stdES = coeffs$stdES
  data_sample_size<-simple_dat %>% 
    group_by(study_id,case_id,condition_id) %>%
    summarise(n = n())
  
  # Define condition names
  data_sample_size$condition_id[data_sample_size$condition_id == 0]<- "condition A"
  data_sample_size$condition_id[data_sample_size$condition_id == 1]<- "condition B"
  data_sample_size$condition_id[data_sample_size$condition_id == 2]<- "condition C"
  
  # Summarize and spread data
  c <- data_sample_size %>% 
    group_by(study_id,case_id,condition_id) %>%
    summarise(num = sum(n))
  c <- c[!(is.na(c$condition_id)),]
  data_wide <- tidyr::spread(c, condition_id, num)
  
  # Merge metadat
  metadat <- merge(data_wide, metadat, by= c("study_id", "case_id"))
  metadat$var<-NA
  
  # Define condition pairs and calculate var and stder
  condition_pairs <- list(c("condition A", "condition B"), c("condition B", "condition C"), c("condition A", "condition C"))
  for(pair in condition_pairs){
    metadat <- metadat %>% mutate(
      var = case_when(
        (!is.na(metadat[[pair[1]]]) & !is.na(metadat[[pair[2]]])) ~ (metadat[[pair[1]]] + metadat[[pair[2]]]) / (metadat[[pair[1]]] * metadat[[pair[2]]]) + (stdES^2 / (2 * (metadat[[pair[1]]] + metadat[[pair[2]]]))),
        TRUE ~ var  # refer to var in the dataframe
      )
    )
  }
  
  metadat$stder <- sqrt(metadat$var)
  
  #### network meta-analysis----
  setDT(metadat)
  
  # #creating the addition netma model
  
  # Initialize your new columns with 0
  metadat[, `:=`("Treatment_A"= 0, "Treatment_B" = 0, "Treatment_C"= 0)]
  
  #coding for condition 0 (A vs B: A is 1, B is -1, C is 0)
  metadat[condition_id==0, `:=`("Treatment_A"= -1, "Treatment_B" = 1, "Treatment_C"= 0)]
  
  #coding for condition 1 (B vs C: B is 1, C is -1, A is 0)
  metadat[condition_id==1, `:=`("Treatment_A"= 0, "Treatment_B" = -1, "Treatment_C"= 1)]
  
  #coding for condition 2 (A vs C: A is 1, C is -1, B is 0)
  metadat[condition_id==2, `:=`("Treatment_A"= -1, "Treatment_B" = 0, "Treatment_C"= 1)]
  
  
  
  
  metadat <- metadat %>%
    mutate(
      phase_1 = case_when(
        condition_id == 0 ~ "condition_A",
        condition_id == 1 ~ "condition_B",
        condition_id == 2 ~ "condition_A"
      ),
      phase_2 = case_when(
        condition_id == 0 ~ "condition_B",
        condition_id == 1 ~ "condition_C",
        condition_id == 2 ~ "condition_C"
      ),
      design = case_when(
        condition_id == 0 ~ paste(phase_1, phase_2, sep = " + "),
        condition_id == 1 ~ paste(phase_1, phase_2, sep = " + "),
        condition_id == 2 ~ paste(phase_1, phase_2, sep = " + ")
      )
    )
  metadat
  metadat$condition_A[metadat$condition_id==0] <- -1
  metadat$condition_B[metadat$condition_id==0] <- 1
  metadat$condition_C[metadat$condition_id==0] <- 0
  metadat$condition_A[metadat$condition_id==1] <- 0
  metadat$condition_B[metadat$condition_id==1] <- -1
  metadat$condition_C[metadat$condition_id==1] <- 1
  metadat$condition_A[metadat$condition_id==2] <- -1
  metadat$condition_B[metadat$condition_id==2] <- 0
  metadat$condition_C[metadat$condition_id==2] <- 1
  
  
  
  
  netma_null <- rma.mv(stdES, 
                       var, 
                       mods = ~ condition_B + condition_C- 1,
                       random = list(~ 1 | study_id/case_id),
                       data = metadat,
                       rho = 1/2,
                       method = "REML",
                       control=list(optimizer="BFGS"))
  netma_null
  
  
  
  #models to test consistency of netma_null
  netma_null0= rma.mv(stdES, 
                      stder, 
                      mods = ~ condition_B + condition_C- 1,
                      random = list(~ 1 | study_id/case_id),
                      data = metadat,
                      rho = 1/2,
                      method = "REML",
                      control=list(optimizer="BFGS"))
  netma_null0
  
  
  netma_null_design= rma.mv(stdES, 
                            stder, 
                            mods = ~ condition_B + condition_C - 1,
                            random = list(~ 1 | design/study_id/case_id),
                            data = metadat,
                            rho = 1/2,
                            method = "REML",
                            control=list(optimizer="BFGS"))
  netma_null_design
  
  #LRT of the results
  anova_results <- anova(netma_null0,netma_null_design)
  anova_results
  # Extract coefficients and store in data frame
  coefs <- coef(summary(netma_null))
  df <- as.data.frame(t(coefs))
  colnames(df) <- c("condition_b", "condition_c")
  
  
  #add the p-values of each condition of the netma_null value to the dataframe
  df$netmanullP_b<-as.numeric(netma_null$pval[1]<0.05)
  df$netmanullP_c<-as.numeric(netma_null$pval[2]<0.05)
  
  # Add individual elements from ANOVA output as new columns
  df$fit_stats_f_AIC <- anova_results$fit.stats.f["AIC"]
  df$fit_stats_r_AIC <- anova_results$fit.stats.r["AIC"]
  df$fit_stats_f_BIC <- anova_results$fit.stats.f["BIC"]
  df$fit_stats_r_BIC <- anova_results$fit.stats.r["BIC"]
  df$LRT <- anova_results$LRT
  df$pval <- anova_results$pval
  
  df$Simulation <- index
  
  
  # Store the results in the list
  results_list[[index]] <- df
  print(index)
  index<-index+1
  
  
}









# AFTER SIMULATION: ASSESSMENT OF STATISTCAL PROPERTIES ---------

    # The example provided below is an example of an assessment 
    # of the assessment of 
      # - the inconsistency ratio
      #


    # the example concerns 20 AB studies, 20 BC studies, 1 AC        study, 1 case per study, 20 measurement points overall, and      10 measurement occasions in the phase A of the study and 10      measurements of phase B

    #It is denoted with the name:
      #AB_BC_AC_CASEPERSTUDY_TOTALmeasurementOCCASIONS_LENGTHofPH       ASEa_LENGTHofPHASEb (pardon the capitals)

  #1 20_20_1_1_20_10_10------------
results_df_20_20_1_1_20_10_10<-do.call(rbind, results_list)
results_df_20_20_1_1_20_10_10
results_df_20_20_1_1_20_10_10<- results_df_20_20_1_1_20_10_10%>%
  mutate(relative_effect = condition_c - condition_b)
subset_df <- results_df_20_20_1_1_20_10_10[c("condition_b", "condition_c","relative_effect","pval")]

#INCONSISTENCY


estimate_rows <- results_df_20_20_1_1_20_10_10[grep("^estimate", rownames(results_df_20_20_1_1_20_10_10)),]  #only the estimate                                               rows



(above_005_count <- sum(estimate_rows$pval > 0.05)) #instances where the p-value is above 0.05 for the comparison of level 4 model and the level 3 model

(below_005_count <- sum(estimate_rows$pval < 0.05)) #instances where the p-value is below 0.05 for the comparison of level 4 model and the level 3 model (the p-value of the design-by-treatment model)

number_consistent<-cat("\nNumber of times pval is above 0.05:", above_005_count)


number_inconsistent<-cat("\nNumber of times pval is below 0.05:", below_005_count)


number_consistent <- paste("\nNumber of times pval is above 0.05:", above_005_count)
number_inconsistent <- paste("\nNumber of times pval is below 0.05:", below_005_count)




#calculate INCONSISTENCY RATIO

if(above_005_count>below_005_count){
  
  ratio_of_inconsistency=below_005_count/(above_005_count+below_005_count)
  
}else{
  
  ratio_of_inconsistency=1-(above_005_count/(above_005_count+ below_005_count))
  
}



ratio_of_inconsistency
below_005_count/3000==ratio_of_inconsistency


#CALCULATING POWER OF ESTIMATE FOR d_AB (effect of b relative to a) and d_AC (the effect of intervention C relative to intervention a)

(power_b<-mean(results_df_20_20_1_1_20_10_10$netmanullP_b)) 
(power_c<-mean(results_df_20_20_1_1_20_10_10$netmanullP_c))





# Extracting rows of interest for the the analysis of confidence intervals-----

ci_lb_rows <- results_df_20_20_1_1_20_10_10[grep("^ci.lb", rownames(results_df_20_20_1_1_20_10_10)), ]

ci_ub_rows <- results_df_20_20_1_1_20_10_10[grep("^ci.ub", rownames(results_df_20_20_1_1_20_10_10)), ]


#PRECISION FOR ESTIMATE B and C

(precision_b_var_estimate <- 1/var(estimate_rows$condition_b))
(precision_c_var_estimate <- 1/var(estimate_rows$condition_c))
(var_b_estimate<-var(estimate_rows$condition_b))
(var_c_estimate<-var(estimate_rows$condition_c))


#BIAS FOR ESTIMATE B and C

# Calculate bias (difference of mean estimates from the true effect size) (for estimate rows only)

(bias_b_estimate <- mean(estimate_rows$condition_b) - 0.9) 
(bias_c_estimate <- mean(estimate_rows$condition_c) -1.6)


# COVERAGE PROBABILITY
## Counters

within_confidence_intervals_b <- 0
outside_confidence_intervals_b <- 0
within_confidence_intervals_c <- 0
outside_confidence_intervals_c <- 0

# True condition estimates
true_condition_b =.9
true_condition_c = 1.6

# Checking for each condition and updating counters
for (i in 1:nrow(estimate_rows)) {
  # Check if the true effect for condition_b is within the confidence interval
  if (true_condition_b >= ci_lb_rows[i, 'condition_b'] && true_condition_b <= ci_ub_rows[i, 'condition_b']) {
    within_confidence_intervals_b <- within_confidence_intervals_b + 1
  } else {
    outside_confidence_intervals_b <- outside_confidence_intervals_b + 1
  }
  ci_lb_rows  
  # Check if the true effect for condition_c is within the confidence interval
  if (true_condition_c >= ci_lb_rows[i, 'condition_c'] && true_condition_c <= ci_ub_rows[i, 'condition_c']) {
    within_confidence_intervals_c <- within_confidence_intervals_c + 1
  } else {
    outside_confidence_intervals_c <- outside_confidence_intervals_c + 1
  }
}
# Print results

(estimates_outside_CI_b <- paste("Number of B estimates outside confidence intervals:", outside_confidence_intervals_b))
(estimates_outside_CI_c <- paste("Number of C estimates outside confidence intervals:", outside_confidence_intervals_c))

(estimates_within_CI_b <- paste("Number of B estimates within confidence intervals:", within_confidence_intervals_b))
(estimates_within_CI_c <- paste("Number of C estimates within confidence intervals:", within_confidence_intervals_c))





# Calculate the total number of estimates
total_estimates_b <- within_confidence_intervals_b + outside_confidence_intervals_b
total_estimates_c <- within_confidence_intervals_c + outside_confidence_intervals_c

# Calculate the coverage probability
(coverage_probability_b <- within_confidence_intervals_b / total_estimates_b)
(coverage_probability_c <- within_confidence_intervals_c / total_estimates_c)





# Calculate MSE for condition_b
(mse_condition_b <- mean((estimate_rows$condition_b - true_condition_b)^2))
# Calculate MSE for condition_c
(mse_condition_c <- mean((estimate_rows$condition_c - true_condition_c)^2))








plots <- list()





for (column in colnames(subset_df)) {
  # Extract the estimates for the current column
  estimate_rows <- grep("^estimate", rownames(subset_df))
  estimates <- subset_df[estimate_rows, column]
  # Print out the estimate rows
  print(paste("Column:", column))
  print(paste("Number of rows used for estimation:", length(estimate_rows)))
  print(paste("Row indices:", paste(estimate_rows, collapse = ", ")))
  
  plot <- ggplot(data.frame(Estimate = estimates), aes(x = Estimate)) +
    
    geom_density() +
    
    labs(title = column, x = "Estimate")
  
  
  
  # Calculate the mean of the estimates
  
  mean_estimate <- mean(estimates, na.rm = TRUE)
  
  
  
  # Add a vertical line at the mean
  plot <- plot +
    
    geom_vline(xintercept = mean_estimate, linetype = "dashed", color = "blue") + annotate("text", x = mean_estimate, y = max(density(estimates)$y, na.rm = TRUE),label = round(mean_estimate, 2), hjust = -0.1, color = "blue")
  
  plots[[column]] <- plot # add plots to list
}



# Access the plot for the "comparison_ab" column
plots[["condition_b"]]
plots[["condition_c"]]
plots[["relative_effect"]]
plots[["pval"]]
unique(results_df_20_20_1_1_20_10_10$Simulation)
# Assuming your dataframe is called "results_df"
matrix_data <- as.matrix(subset_df)

# Select only the rows with rownames starting with "estimate"
estimate_rows <- grep("^estimate", rownames(matrix_data))
estimate_data <- matrix_data[estimate_rows, ]
# Calculate the column means
(means <- colMeans(estimate_data, na.rm = TRUE))



# 0Select only the rows with rownames starting with "ci.ub" and "ci.lb"
ci_ub_rows <- grep("^ci.ub", rownames(matrix_data))
ci_lb_rows <- grep("^ci.lb", rownames(matrix_data))

# Subset for each condition
ci_ub_b_data <- matrix_data[ci_ub_rows, "condition_b", drop = FALSE]
ci_lb_b_data <- matrix_data[ci_lb_rows, "condition_b", drop = FALSE]

ci_ub_c_data <- matrix_data[ci_ub_rows, "condition_c", drop = FALSE]
ci_lb_c_data <- matrix_data[ci_lb_rows, "condition_c", drop = FALSE]

ci_ub_relative_effect_data <- matrix_data[ci_ub_rows, "relative_effect", drop = FALSE]
ci_lb_relative_effect_data <- matrix_data[ci_lb_rows, "relative_effect", drop = FALSE]

# Calculate the column means for each condition
(mean_ci_ub_b <- colMeans(ci_ub_b_data, na.rm = TRUE)
)
(mean_ci_lb_b <- colMeans(ci_lb_b_data, na.rm = TRUE))

(mean_ci_ub_c <- colMeans(ci_ub_c_data, na.rm = TRUE))
(mean_ci_lb_c <- colMeans(ci_lb_c_data, na.rm = TRUE))
(mean_ci_ub_relative_effect <- colMeans(ci_ub_relative_effect_data, na.rm = TRUE))
(mean_ci_lb_relative_effect <- colMeans(ci_lb_relative_effect_data, na.rm = TRUE))

# Calculate CI difference for each condition
(ci_diff_b <- mean_ci_ub_b - mean_ci_lb_b)
(ci_diff_c <- mean_ci_ub_c - mean_ci_lb_c)
(ci_diff_relative_effect <- mean_ci_ub_relative_effect - mean_ci_lb_relative_effect)

# Combine results in a list for easier manipulation
ci_list <- list(
  condition_b = list(upper = mean_ci_ub_b, lower = mean_ci_lb_b, diff = ci_diff_b),
  condition_c = list(upper = mean_ci_ub_c, lower = mean_ci_lb_c, diff = ci_diff_c),
  relative_effect = list(upper = mean_ci_ub_relative_effect, lower = mean_ci_lb_relative_effect, diff = ci_diff_relative_effect)
)

# Print or save the results
for (condition in names(ci_list)) {
  for (boundary in names(ci_list[[condition]])) {
    cat(condition, boundary, ci_list[[condition]][[boundary]], "\n")
    writeLines(paste(condition, boundary, ci_list[[condition]][[boundary]], sep = ": "), paste0(condition, '_', boundary, 'CI_20_20_1_1_20_10_10.txt'))
  }
}

# Extract the specific means 

mean_estimate_b <- means["condition_b"]
mean_estimate_c <- means["condition_c"]
mean_relative_effect <- means["relative_effect"]


# Creating a data frame to store the results

results_summary <- data.frame(
  Number_Consistent = above_005_count,
  Number_Inconsistent = below_005_count,
  Ratio_Inconsistent = ratio_of_inconsistency,
  Power_B = power_b,
  Power_C = power_c,
  Precision_B = precision_b_var_estimate,
  Precision_C = precision_c_var_estimate,
  var_B<-var_b_estimate,
  var_C<-var_c_estimate,
  Bias_B = bias_b_estimate,
  Bias_C = bias_c_estimate,
  Coverage_Prob_B = coverage_probability_b,
  Coverage_Prob_C = coverage_probability_c,
  MSE_B = mse_condition_b,
  MSE_C = mse_condition_c,
  Mean_est_B=mean_estimate_b,
  Mean_est_C=mean_estimate_c,
  Mean_est_RE=mean_relative_effect,
  Lower_Bound_B = mean_ci_lb_b,
  Upper_Bound_B = mean_ci_ub_b,
  Lower_Bound_C = mean_ci_lb_c,
  Upper_Bound_C = mean_ci_ub_c,
  Lower_Bound_Relative_Effect = ci_lb_relative_effect_data,
  Upper_Bound_Relative_Effect = ci_ub_relative_effect_data,
  CI_Diff_B = ci_diff_b,
  CI_Diff_C = ci_diff_c,
  CI_Diff_Relative_Effect = ci_diff_relative_effect
)



# Writing the data frame to an Excel file

write.xlsx(results_summary, "results_summary_20_20_1_1_20_10_10.xlsx")

