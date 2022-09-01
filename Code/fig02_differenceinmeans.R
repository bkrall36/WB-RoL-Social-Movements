# Set WD for Outputs
setwd(Outputs)

# Difference in Means of Vote Share PML Summary Table
# Summarize and save as a table to graph 
pml_table <- df_02_08 %>% 
  filter((Year == 2002 | 2008), `Party Initials` == 'PML') %>%
  group_by(Year,`Treated before 2008 elections`) %>%
  summarize(
    'Percentage of Vote Share' = mean(Vote_Share.1)
  )

# Reshape the table to wide format
pml_table <- spread(pml_table, key = "Treated before 2008 elections", value = 'Percentage of Vote Share')

# Transpose the table, remove the first row, and save as df object
pml_table <- t(pml_table)
pml_table <- pml_table[-1,]
pml_table <- as.data.frame(pml_table)

# Create a difference column
pml_table <- pml_table %>%
  mutate(
    Difference = pml_table$V2 - pml_table$V1
  )

# Rename columns and rows
colnames(pml_table) <- c(2002, 2008, 'Difference')
rownames(pml_table) <- c("Unvisited Districts", "Visited Districts")

print(pml_table)


# Difference in Means of Vote Share PML-N
# Summarize and save as a table to graph
pmln_table <- df_02_08 %>% 
  filter((Year == 2002 | 2008), `Party Initials` == 'PML-N') %>%
  group_by(Year,`Treated before 2008 elections`) %>%
  summarize(
    'Percentage of Vote Share' = mean(Vote_Share.1)
  )

# Reshape the table to wide format
pmln_table <- spread(pmln_table, key = "Treated before 2008 elections", value = 'Percentage of Vote Share')

# Transpose the table, remove the first row, and save as df object
pmln_table <- t(pmln_table)
pmln_table <- pmln_table[-1,]
pmln_table <- as.data.frame(pmln_table)

# Create a difference column
pmln_table <- pmln_table %>%
  mutate(
    Difference = pmln_table$V2 - pmln_table$V1
  )

# Rename columns and rows
colnames(pmln_table) <- c(2002, 2008, 'Difference')
rownames(pmln_table) <- c("Unvisited Districts", "Visited Districts")

print(pmln_table)

# Difference in Means of Vote Share PML-N
# Summarize and save as a table to graph
pmln_table <- df_02_08 %>% 
  filter((Year == 2002 | 2008), `Party Initials` == 'PML-N') %>%
  group_by(Year,`Treated before 2008 elections`) %>%
  summarize(
    'Percentage of Vote Share' = mean(Vote_Share.1)
  )

# Reshape the table to wide format
pmln_table <- spread(pmln_table, key = "Treated before 2008 elections", value = 'Percentage of Vote Share')

# Transpose the table, remove the first row, and save as df object
pmln_table <- t(pmln_table)
pmln_table <- pmln_table[-1,]
pmln_table <- as.data.frame(pmln_table)

# Create a difference column
pmln_table <- pmln_table %>%
  mutate(
    Difference = pmln_table$V2 - pmln_table$V1
  )

# Rename columns and rows
colnames(pmln_table) <- c(2002, 2008, 'Difference')
rownames(pmln_table) <- c("Unvisited Districts", "Visited Districts")

print(pmln_table)