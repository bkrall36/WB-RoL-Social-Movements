# Set WD for Outputs
setwd(Outputs)

# Event Study 
# Construct a variable that, for treated units, will take the value of the 
# number of years leading up to it (+3,-3). For untreated units, or treated 
# units that will be treated 3 or more years in the future,it takes the value -4
df$time_since_treatment = ifelse(!is.na(df$`1st_visit_year`) & (df$Year-df$`1st_visit_year`>=-3) & (df$Year-df$`1st_visit_year`<=3),df$Year-df$`1st_visit_year`,-4)
# For treated units, after 3 years, this variable will have the value 4
df$time_since_treatment = ifelse(!is.na(df$`1st_visit_year`) & (df$Year-df$`1st_visit_year`>3),4,df$time_since_treatment)

event_study_reg <- lm(Vote_Share.1 ~ factor(time_since_treatment) + constituency_dummy + year_dummy + count, data = df)

# cluster-robust SEs for event study
cl.cov.event <- cluster.vcov(event_study_reg, df$`PA ID`) 
cl.robust.se.event <- sqrt(diag(cl.cov.event))

table03_eventstudy <- stargazer(event_study_reg, 
                                type="text",
                                title = "Table 6 - Effect of CJ visits on vote share for All Parties",
                                dep.var.labels.include = FALSE,
                                style = 'aer',
                                keep=c("time_since_treatment"), 
                                se = list(cl.robust.se.event), 
                                dep.var.caption = '', 
                                notes = c("Robust standard errors appear in brackets (clustered at the district level)."), 
                                notes.align = "l", 
                                keep.stat=c('n', 'adj.rsq', 'f'), 
                                add.lines = list('Year FE' = c('Year FE','Yes'), 'Constituency FE' = c('Constituency FE','Yes'), 'Controls' = c('Controls','Yes'), "Mean" = c("Mean", round(mean(df$Vote_Share.1), 2))),
                                out="eventstudy_regression.html")

