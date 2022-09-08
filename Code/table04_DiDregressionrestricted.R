# Set WD for Outputs
setwd(Outputs)

# Perform Regression Analysis - PML
# OLS 
reg_pml <- lm(Vote_Share.1 ~ treated, data = df_pml_restricted)
# cluster-robust SEs for reg_pml
cl.cov.pml1 <- cluster.vcov(reg_pml, df_pml$`PA ID`) 
cl.robust.se.pml1 <- sqrt(diag(cl.cov.pml1))

# OLS with 'count' variable control
reg_pml2 <- lm(Vote_Share.1 ~ treated + year_dummy, data = df_pml_restricted)
# cluster-robust SEs for reg_pml2
cl.cov.pml2 <- cluster.vcov(reg_pml2, df_pml$`PA ID`) 
cl.robust.se.pml2 <- sqrt(diag(cl.cov.pml2))

# OLS with TWFE 
reg_pml3 <- lm(Vote_Share.1 ~ treated + year_dummy + constituency_dummy, data = df_pml_restricted)
# cluster-robust SEs for reg_pml3
cl.cov.pml3 <- cluster.vcov(reg_pml3, df_pml$`PA ID`) 
cl.robust.se.pml3 <- sqrt(diag(cl.cov.pml3))

# OLS with TWFE + 'count' control variable 
reg_pml4 <- lm(Vote_Share.1 ~ treated + year_dummy + constituency_dummy + count, data = df_pml_restricted)
# cluster-robust SEs for reg_pml4
cl.cov.pml4 <- cluster.vcov(reg_pml4, df_pml$`PA ID`) 
cl.robust.se.pml4 <- sqrt(diag(cl.cov.pml4))

table04_DiDregression <- stargazer(reg_pml,reg_pml2,reg_pml3,reg_pml4, 
                                   type="text",
                                   style = 'aer',
                                   title="Table 2 - Effect of CJ visits on vote share for General Musharraf PML (restricted)", 
                                   keep = c('treated'), 
                                   order=c('treated'), 
                                   covariate.labels=c('CJ Visits'), 
                                   dep.var.caption = '', 
                                   dep.var.labels.include = FALSE, 
                                   se = list(cl.robust.se.pml1, cl.robust.se.pml2, cl.robust.se.pml3, cl.robust.se.pml4), 
                                   notes = c("Robust standard errors appear in brackets (clustered at the district level).",  
                                             "The table presents DiD estimation of the effect of Chief Justice visits to districts of", 
                                             "Pakistan before 2008 elections on the share of votes in favour of PML party in 2008 provincial", 
                                             "and national elections (compare to 2002 elections). Outcome variable is visit to a given",  
                                             "district. Controls include: # of candidates in a district (count)."),
                                   notes.align = "l", 
                                   keep.stat=c('n', 'adj.rsq', 'f'), 
                                   add.lines = list('Year FE' = c('Year FE','No', 'Yes', 'Yes', 'Yes'), 'Constituency FE' = c('Constituency FE','No', 'No', 'Yes', 'Yes'), 'Controls' = c('Controls','No', 'No', 'No', 'Yes'), "Mean" = c("Mean", round(mean(df_pml_restricted$Vote_Share.1), 2), round(mean(df_pml_restricted$Vote_Share.1), 2), round(mean(df_pml_restricted$Vote_Share.1), 2), round(mean(df_pml_restricted$Vote_Share.1), 2))),
                                   out="restrictedregression_pml_2008DiD.html")


# Perform Regression Analysis for Opposition 
## PML-N
# OLS with TWFE 
reg_pmln3 <- lm(Vote_Share.1 ~ treated + year_dummy + constituency_dummy, data = df_pml_n_restricted)
# cluster-robust SEs for reg_pmln3
cl.cov.pmln3 <- cluster.vcov(reg_pmln3, df_pml_n$`PA ID`) 
cl.robust.se.pmln3 <- sqrt(diag(cl.cov.pmln3))

# OLS with TWFE + 'count' control variable 
reg_pmln4 <- lm(Vote_Share.1 ~ treated + year_dummy + constituency_dummy + count, data = df_pml_n_restricted)
# cluster-robust SEs for reg_pmln4
cl.cov.pmln4 <- cluster.vcov(reg_pmln4, df_pml_n$`PA ID`) 
cl.robust.se.pmln4 <- sqrt(diag(cl.cov.pmln4))

## PPP
# OLS with TWFE 
reg_pppp3 <- lm(Vote_Share.1 ~ treated + year_dummy + constituency_dummy, data = df_pppp_restricted)
# cluster-robust SEs for reg_pppp3
cl.cov.pppp3 <- cluster.vcov(reg_pppp3, df_pppp$`PA ID`) 
cl.robust.se.pppp3 <- sqrt(diag(cl.cov.pppp3))

# OLS with TWFE + 'count' control variable 
reg_pppp4 <- lm(Vote_Share.1 ~ treated + year_dummy + constituency_dummy + count, data = df_pppp_restricted)
# cluster-robust SEs for reg_pppp4
cl.cov.pppp4 <- cluster.vcov(reg_pppp4, df_pppp$`PA ID`) 
cl.robust.se.pppp4 <- sqrt(diag(cl.cov.pppp4))

# Create Opposition (PML-N and PPP) Regression Table
table041_DiDregressionOpp <- stargazer(reg_pmln3,reg_pmln4,reg_pppp3,reg_pppp4, 
                                      type="text",
                                      style = 'aer', 
                                      title="Table 3 - Effect of CJ visits on vote share for the Opposition (restricted)", 
                                      keep = c('treated'), 
                                      order=c('treated'), 
                                      covariate.labels=c('CJ Visits'), 
                                      column.labels=c('PML-N', 'PML-N', 'PPP', 'PPP'), 
                                      dep.var.caption = '', 
                                      dep.var.labels.include = FALSE, 
                                      se = list(cl.robust.se.pmln3, cl.robust.se.pmln4, cl.robust.se.pppp3, cl.robust.se.pppp4), 
                                      notes = c("Robust standard errors appear in brackets (clustered at the district level).",
                                                "The table presents DiD estimation of the effect of Chief Justice visits to districts of",
                                                "Pakistan before 2008 elections on the share of votes in favour of opposition parties in 2008",
                                                "provincial and national elections (compare to 2002 elections). Outcome variable is visit to a",
                                                "given district. Controls include: # of candidates in a district (count)."), 
                                      notes.align = "l", 
                                      keep.stat=c('n', 'adj.rsq', 'f'), 
                                      add.lines = list('Year FE' = c('Year FE','Yes', 'Yes', 'Yes', 'Yes'), 'Constituency FE' = c('Constituency FE','Yes', 'Yes', 'Yes', 'Yes'), 'Controls' = c('Controls','No', 'Yes', 'No', 'Yes'), "Mean" = c("Mean", round(mean(df_pml_n_restricted$Vote_Share.1), 2), round(mean(df_pml_n_restricted$Vote_Share.1), 2), round(mean(df_pppp_restricted$Vote_Share.1), 2), round(mean(df_pppp_restricted$Vote_Share.1), 2))),
                                      out="restrictedregression_opp_2008DiD.html")