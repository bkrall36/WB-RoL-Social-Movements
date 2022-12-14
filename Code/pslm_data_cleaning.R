# Import Data 
pslmdf <- read_excel(paste0(Inputs, "PSLM Data.xlsx"), sheet = 1)

# Subset first three rows and remove them from dataframe 
pslmdf_first3 <- pslmdf[, 1:3]
pslmdf <- pslmdf[, -c(1:3)]

# Store column names and then store columns with names 
cnames <- colnames(pslmdf)
cnames <- cnames[seq(1, length(cnames), 9)]

# Remove row of metadata 
pslmdf_first3 <- pslmdf_first3[-1,]
pslmdf <- pslmdf[-1,]

# Select column observations with totals only 
pslmdf <- pslmdf[seq_len(ncol(pslmdf)) %% 9 == 0]

# Rename total columns manually and using stored column names 
colnames(pslmdf_first3) <- c('year', 'province_id', 'name')
colnames(pslmdf) <- cnames

# Add first three rows back 
clean_pslm_data <- cbind(pslmdf_first3, pslmdf)

# Store Clean Data
write_csv(clean_pslm_data, paste0(
  Outputs, "clean_pslm_data.csv"))

# Data Source: https://www.pbs.gov.pk/publication/pakistan-social-and-living-standards-measurement-survey-pslm-2004-05-provincial