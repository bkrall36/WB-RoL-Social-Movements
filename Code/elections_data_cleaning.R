# Data Import and Cleaning 

df <- read_excel(paste0(Inputs, "Elections_Final_10.08.xlsx"), sheet = 1)

# Prepare Linear Regression Model Terms and Dummy Variables
# Create Year Fixed-Effect Variable 
df$year_dummy <- factor(df$Year)

# Create Constituency Fixed-Effect Variable 
df$constituency_dummy <- factor(df$`PA ID`)

# Create Treated 2008 and 2013 variable
df <- df %>% mutate(
  treated = ifelse((`Treated before 2008 elections` == 1), 1, 0)
)

# Create vector with list of names of planned and incidental visit districts
planned_districts <- c('Islamabad', 'Rawalpindi', 'Sukkur', 'Hyderabad', 'Peshawar', 'Lahore', 'Abbottabad', 'Faisalabad', 'Multan')
incidental_districts <- c('Moro (Naushehro Feroz)', 'Khairpur', 'Larkana', 'Nawabshah',  'Matiari', 'Attock', 'Gujarat', 'Gujranwala', 'Taxila', 'Haripur', 'Chakwal', 'Pindi Bhattian', 'Chiniot', 'Sahiwal', 'Okara', 'Chichawatni', 'Khanewal')

# Create Incidental Visit Treatment Indicator
df <- df %>% mutate(
  incidental_treatment = ifelse((districts %in% incidental_districts), 1, 0),
  planned_treatment = ifelse((districts %in% planned_districts), 1, 0)
)

# Create Visits 2008 and 2013 variable Counts
#df <- df %>% mutate(
#  treated_2008_n = ifelse((Year == 2008 & Treated == 1), 'Visits before 2008 elections', 0),
#  treated_2013_n = ifelse((Year == 2013 & Treated == 1), 'N_visits', 0)
#)

# Remove observations with NA values for our outcome variable 
df <- df %>% filter(!is.na(Vote_Share))

# Select columns needed for analysis 
df <- df %>% select(c('Year', 'year_dummy', 'PA ID', 'constituency_dummy', 'districts', 'Party Initials', 'Vote_Share.1', 'count', 'treated', 'planned_treatment', 'incidental_treatment', '1st_visit_year'))
# Store Clean Data
write_csv(df, paste0(
  Outputs, "clean_elections_data.csv"))

# Subset Dataset for Regression Analysis
# Restrict analysis to years between 2002 and 2008
df_02_08 <- df %>%
  filter(Year >= 2002 & Year <= 2008)

# Subset data by party affiliation 
df_pml <- df_02_08 %>%
  filter(`Party Initials` == 'PML')

df_pml_n <- df_02_08 %>%
  filter(`Party Initials` == 'PML-N')

df_pppp <- df_02_08 %>%
  filter(`Party Initials` == 'PPPP')

