# Set WD for Outputs
setwd(Outputs)

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
baltable <- data.frame(balance_tbl)
png("balancetable.png", height=600, width=1400)
grid.table(baltable)
dev.off()

# Convert datatypes to numeric 
clean_pslm_data[4:30] <-sapply(clean_pslm_data[4:30], as.numeric)

# Manually run all regression
reg1 <- feols(clean_pslm_data$`POPULATION THAT HAS EVER ATTENDED SCHOOL BY-PROVINCE AND DISTRICTS 2.1` ~ treated | name, clean_pslm_data)
reg2 <- feols(clean_pslm_data$`PERCENTAGE DISTRIBUTION OF POPULATION THAT COMPLETED PRIMARY LEVEL OR HIGHER 2.2` ~ treated | name, clean_pslm_data)
reg3 <- feols(clean_pslm_data$`GROSS ENROLMENT RATIO AT THE PRIMARY LEVEL(AGE 6-10)- BY PROVINCE & DISTRICT(EXCLUDING KATCHI CLASS) 2.3(a)` ~ treated | name, clean_pslm_data)
reg4 <- feols(clean_pslm_data$`GROSS ENROLMENT RATIO AT THE PRIMARY LEVEL(AGE 5-9)- BY PROVINCE & DISTRICT(EXCLUDING KATCHI CLASS) 2.3(b)` ~ treated | name, clean_pslm_data)
reg5 <- feols(clean_pslm_data$`GROSS ENROLMENT RATIO AT THE PRIMARY LEVEL(AGE 4-9)- BY PROVINCE & DISTRICT(INCLUDING KATCHI CLASS) 2.4` ~ treated | name, clean_pslm_data)
reg6 <- feols(clean_pslm_data$`GROSS ENROLMENT RATIO FOR GOVERNMENT PRIMARY SCHOOLS (AGE 5-9) - BY PROVINCE & DISTRICT 2.5` ~ treated | name, clean_pslm_data)
reg7 <- feols(clean_pslm_data$`NET ENROLMENT RATE AT THE PRIMARY LEVEL(AGE 6-10)- BY PROVINCE & DISTRICT(EXCLUDING KATCHI CLASS) 2.6(a)` ~ treated | name, clean_pslm_data)
reg8 <- feols(clean_pslm_data$`NET ENROLMENT RATE AT THE PRIMARY LEVEL(AGE 5-9)- BY PROVINCE & DISTRICT(EXCLUDING KATCHI CLASS) 2.6(b)` ~ treated | name, clean_pslm_data)
reg9 <- feols(clean_pslm_data$`NET ENROLMENT RATE AT THE PRIMARY LEVEL(AGE 4-9)- BY PROVINCE & DISTRICT(INCLUDING KATCHI CLASS) 2.7` ~ treated | name, clean_pslm_data)
reg10 <- feols(clean_pslm_data$`NET ENROLMENT RATE IN GOVERNMENT PRIMARY SCHOOLS (AGE 5-9)- BY PROVINCE & DISTRICT (EXCLUDING KATCHI CLASS) 2.8` ~ treated | name, clean_pslm_data)
reg11 <- feols(clean_pslm_data$`PRIMARY LEVEL ENROLMENT IN GOVERNMENT SCHOOLS A PERCENTAGE OF TOTAL PRIMARY 2.9(a)` ~ treated | name, clean_pslm_data)
reg12 <- feols(clean_pslm_data$`PRIMARY LEVEL ENROLMENT IN GOVERNMENT SCHOOLS A PERCENTAGE OF TOTAL PRIMARY ENROLMENT - BY PROVINCE AND DISTRICT 2.9(b)` ~ treated | name, clean_pslm_data)
reg13 <- feols(clean_pslm_data$`GROSS ENROLMENT RATIO AT THE MIDDLE LEVEL (AGE 11-13) - BY PROVINCE & DISTRICT 2.10(a)` ~ treated | name, clean_pslm_data)
reg14 <- feols(clean_pslm_data$`GROSS ENROLMENT RATIO AT THE MIDDLE LEVEL (AGE 10-12)- BY PROVINCE & DISTRICT 2.10(b)` ~ treated | name, clean_pslm_data)
reg15 <- feols(clean_pslm_data$`NET ENROLMENT RATE AT THE MIDDLE LEVEL (AGE 11-13)- BY PROVINCE & DISTRICT 2.11(a)` ~ treated | name, clean_pslm_data)
reg16 <- feols(clean_pslm_data$`NET ENROLMENT RATE AT THE MIDDLE LEVEL (AGE 10-12)- BY PROVINCE & DISTRICT 2.11(b)` ~ treated | name, clean_pslm_data)
reg17 <- feols(clean_pslm_data$`GROSS ENROLMENT RATIO AT THE MATRIC LEVEL(AGE 14-15)- BY PROVINCE AND DISTRICT 2.12(a)` ~ treated | name, clean_pslm_data)
reg18 <- feols(clean_pslm_data$`GROSS ENROLMENT RATIO AT THE MATRIC LEVEL (AGE 13-14)- BY PROVINCE & DISTRICT 2.12(b)` ~ treated | name, clean_pslm_data)
reg19 <- feols(clean_pslm_data$`NET ENROLMENT RATE AT THE MATRIC LEVEL (AGE 14-15) - BY PROVINCE & REGION 2.13(a)` ~ treated | name, clean_pslm_data)
reg20 <- feols(clean_pslm_data$`NET ENROLMENT RATE AT THE MATRIC LEVEL (AGE 13-14)- BY PROVINCE & DISTRICT 2.13(b)` ~ treated | name, clean_pslm_data)
reg21 <- feols(clean_pslm_data$`IMMUNISATION BASED ON RECALL 3.4(a): AT LEAST 1 IMMUNISATION` ~ treated | name, clean_pslm_data)
reg22 <- feols(clean_pslm_data$`IMMUNISATION BASED ON RECORD 3.4(b): FULLY IMMUNIZED` ~ treated | name, clean_pslm_data)
reg23 <- feols(clean_pslm_data$`IMMUNISATION BASED ON RECALL AND RECORD 3.4(c): FULLY IMMUNIZED` ~ treated | name, clean_pslm_data)
reg24 <- feols(clean_pslm_data$`CHILDREN SUFFERING FROM DIARRHEA IN PAST 15 DAYS 3.7` ~ treated | name, clean_pslm_data)

# Store regression results manually
result1 <- reg1$coeftable
result2 <- reg2$coeftable
result3 <- reg3$coeftable
result4 <- reg4$coeftable
result5 <- reg5$coeftable
result6 <- reg6$coeftable
result7 <- reg7$coeftable
result8 <- reg8$coeftable
result9 <- reg9$coeftable
result10 <- reg10$coeftable
result11 <- reg11$coeftable
result12 <- reg12$coeftable
result13 <- reg13$coeftable
result14 <- reg14$coeftable
result15 <- reg15$coeftable
result16 <- reg16$coeftable
result17 <- reg17$coeftable
result18 <- reg18$coeftable
result19 <- reg19$coeftable
result20 <- reg20$coeftable
result21 <- reg21$coeftable
result22 <- reg22$coeftable
result23 <- reg23$coeftable
result24 <- reg24$coeftable

# Create a balance table
balance_table <- rbind(result1, result2, result3, result4, result5, result6, result7, result8, result9, result10, result11, result12, result13, result14, result15, result16, result17, result18, result19, result20, result21, result22, result23, result24)

# Change row names
colnames <- colnames(clean_pslm_data)
colnames <- colnames[4:27]
row.names(balance_table) <- colnames

# Select specific columns and rename 
balance_table <- balance_table %>% select(c("Estimate", "Std. Error", "Pr(>|t|)"))
colnames(balance_table) <- c("coefficient", "standard_error", "pval")

# Create significance column 
balance_table <- balance_table %>%
  mutate(
    'Significance Level' = case_when(
      pval <= 0.01 ~ "***",
      pval <= 0.05 ~ "**",
      pval <= 0.1 ~ "*",
      pval > 0.1 ~ ""
    )
  )

# Save balance table 
baltable2 <- data.frame(balance_table)
png("balancetable2.png", height=600, width=1400)
grid.table(baltable2)
dev.off()
