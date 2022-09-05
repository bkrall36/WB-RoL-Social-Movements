# Grouping Data by Treatment Status 

# Create a list that contains districts that were visited by chief justice in 2007
treated_districts <- c('Islamabad', 'Rawalpindi', 'Sukkur', 'Hyderabad', 'Peshawar', 'Lahore', 'Abbottabad', 'Faisalabad', 'Multan')

# Create a treated dummy column
clean_pslm_data <- clean_pslm_data %>%
  mutate(
    treated = ifelse(name %in% treated_districts, 1, 0)
  )

# Subset the data for the control groups
pslmdf_control <- clean_pslm_data %>%
  filter(treated == 0)

# Subset the data for the treatment groups
pslmdf_treated <- clean_pslm_data %>%
  filter(treated == 1)

# De-identify districts
pslmdf_treated <- pslmdf_treated[, -c(1:3)]
pslmdf_control <- pslmdf_control[, -c(1:3)]

# Ensure datatype of columns are numeric 
pslmdf_treated[] <- sapply(pslmdf_treated, as.numeric)
pslmdf_control[] <- sapply(pslmdf_control, as.numeric)

# Compute aggregate measures for balance table
control_means <- as.data.frame(colMeans(pslmdf_control))
treated_means <- as.data.frame(colMeans(pslmdf_treated))

# Combine measures and remove unnecessary row observations
means_df <- cbind(control_means, treated_means)
means_df <- means_df %>% filter(row_number() <= n()-4)

# Create p-value vector to store values from for loop 
pval <- c()

# Calculate the p-values baseline differences in means using for loop
for (i in 1:24){
  pval[i] <- t.test(pslmdf_control[,i], pslmdf_treated[,i],
                    alternative = 'two.sided',
                    var.equal = FALSE)$p.val
}

# Create balance table, add significance, and rename columns
balance_tbl <- cbind(means_df, pval)
balance_tbl <- balance_tbl %>%
  mutate(
    'Significance Level' = case_when(
      pval <= 0.01 ~ "***",
      pval <= 0.05 ~ "**",
      pval <= 0.1 ~ "*",
      pval > 0.1 ~ ""
    )
  )


colnames(balance_tbl) <- c("Univisited District Means", "Visited District Means", "P-Value", "Significance Level")

# Save balance table 
table01_balance <- table(balance_tbl)