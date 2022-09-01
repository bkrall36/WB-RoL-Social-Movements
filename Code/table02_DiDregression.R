# Perform Regression Analysis - PML
# OLS 
reg_pml <- lm(Vote_Share.1 ~ treated, data = df_pml)
# cluster-robust SEs for reg_pml
cl.cov.pml1 <- cluster.vcov(reg_pml, df_pml$`PA ID`) 
cl.robust.se.pml1 <- sqrt(diag(cl.cov.pml1))

# OLS with 'count' variable control
reg_pml2 <- lm(Vote_Share.1 ~ treated + year_dummy, data = df_pml)
# cluster-robust SEs for reg_pml2
cl.cov.pml2 <- cluster.vcov(reg_pml2, df_pml$`PA ID`) 
cl.robust.se.pml2 <- sqrt(diag(cl.cov.pml2))

# OLS with TWFE 
reg_pml3 <- lm(Vote_Share.1 ~ treated + year_dummy + constituency_dummy, data = df_pml)
# cluster-robust SEs for reg_pml3
cl.cov.pml3 <- cluster.vcov(reg_pml3, df_pml$`PA ID`) 
cl.robust.se.pml3 <- sqrt(diag(cl.cov.pml3))

# OLS with TWFE + 'count' control variable 
reg_pml4 <- lm(Vote_Share.1 ~ treated + year_dummy + constituency_dummy + count, data = df_pml)
# cluster-robust SEs for reg_pml4
cl.cov.pml4 <- cluster.vcov(reg_pml4, df_pml$`PA ID`) 
cl.robust.se.pml4 <- sqrt(diag(cl.cov.pml4))

table02_DiDregression <- stargazer(reg_pml,reg_pml2,reg_pml3,reg_pml4, 
                                   type="text", 
                                   title="Effect of CJ visits on vote share for General Musharraf PML - Impact on Dicator’s Party", 
                                   keep = c('Constant','treated', 'year_dummy2008', 'count'), 
                                   order=c('Constant','treated'), 
                                   covariate.labels=c('Unvisited Districts', 'Visited Districts','Year Dummy (2008)', 'Controls'), 
                                   column.labels=c('OLS', 'OLS w/ Year FE', 'TWFE', 'TWFE w/ Controls'), 
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
                                   add.lines = list("Mean" = c("Mean", round(mean(df_pml$Vote_Share.1), 2), round(mean(df_pml$Vote_Share.1), 2), round(mean(df_pml$Vote_Share.1), 2), round(mean(df_pml$Vote_Share.1), 2))))


# Perform Regression Analysis for Opposition 
## PML-N
# OLS with TWFE 
reg_pmln3 <- lm(Vote_Share.1 ~ treated + year_dummy + constituency_dummy, data = df_pml_n)
# cluster-robust SEs for reg_pmln3
cl.cov.pmln3 <- cluster.vcov(reg_pmln3, df_pml_n$`PA ID`) 
cl.robust.se.pmln3 <- sqrt(diag(cl.cov.pmln3))

# OLS with TWFE + 'count' control variable 
reg_pmln4 <- lm(Vote_Share.1 ~ treated + year_dummy + constituency_dummy + count, data = df_pml_n)
# cluster-robust SEs for reg_pmln4
cl.cov.pmln4 <- cluster.vcov(reg_pmln4, df_pml_n$`PA ID`) 
cl.robust.se.pmln4 <- sqrt(diag(cl.cov.pmln4))

## PPP
# OLS with TWFE 
reg_pppp3 <- lm(Vote_Share.1 ~ treated + year_dummy + constituency_dummy, data = df_pppp)
# cluster-robust SEs for reg_pppp3
cl.cov.pppp3 <- cluster.vcov(reg_pppp3, df_pppp$`PA ID`) 
cl.robust.se.pppp3 <- sqrt(diag(cl.cov.pppp3))

# OLS with TWFE + 'count' control variable 
reg_pppp4 <- lm(Vote_Share.1 ~ treated + year_dummy + constituency_dummy + count, data = df_pppp)
# cluster-robust SEs for reg_pppp4
cl.cov.pppp4 <- cluster.vcov(reg_pppp4, df_pppp$`PA ID`) 
cl.robust.se.pppp4 <- sqrt(diag(cl.cov.pppp4))

# Create Opposition (PML-N and PPP) Regression Table
table03_DiDregressionOpp <- stargazer(reg_pmln3,reg_pmln4,reg_pppp3,reg_pppp4, 
                                      type="text", 
                                      title="Effect of CJ visits on vote share for the Opposition (PML-N & PPP)", 
                                      keep = c('Constant','treated', 'count', 'year_dummy2008'), order=c('Constant','treated'), 
                                      covariate.labels=c('Unvisited Districts', 'Visited Districts', 'Year Dummy (2008)', 'Controls'), 
                                      column.labels=c('PML-N: TWFE', 'PML-N: TWFE w/ Controls', 'PPP: TWFE', 'PPP: TWFE w/ Controls'), 
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
                                      add.lines = list("Mean" = c("Mean", round(mean(df_pml_n$Vote_Share.1), 2), round(mean(df_pml_n$Vote_Share.1), 2), round(mean(df_pppp$Vote_Share.1), 2), round(mean(df_pppp$Vote_Share.1), 2))))

# Perform Regression Analysis (incidental effects)
# PLM
# OLS with TWFE 
reg_pmlr1 <- lm(Vote_Share.1 ~ incidental_treatment + year_dummy + constituency_dummy, data = df_pml)
# cluster-robust SEs for reg_pml3
cl.cov.pmlr1 <- cluster.vcov(reg_pmlr1, df_pml$`PA ID`) 
cl.robust.se.pmlr1 <- sqrt(diag(cl.cov.pmlr1))

# OLS with TWFE + 'count' control variable 
reg_pmlr2 <- lm(Vote_Share.1 ~ incidental_treatment + year_dummy + constituency_dummy + count, data = df_pml)
# cluster-robust SEs for reg_pml4
cl.cov.pmlr2 <- cluster.vcov(reg_pmlr2, df_pml$`PA ID`) 
cl.robust.se.pmlr2 <- sqrt(diag(cl.cov.pmlr2))

## PML-N
# OLS with TWFE 
reg_pmlnr1 <- lm(Vote_Share.1 ~ incidental_treatment + year_dummy + constituency_dummy, data = df_pml_n)
# cluster-robust SEs for reg_pmln3
cl.cov.pmlnr1 <- cluster.vcov(reg_pmlnr1, df_pml_n$`PA ID`) 
cl.robust.se.pmlnr1 <- sqrt(diag(cl.cov.pmlnr1))

# OLS with TWFE + 'count' control variable 
reg_pmlnr2 <- lm(Vote_Share.1 ~ incidental_treatment + year_dummy + constituency_dummy + count, data = df_pml_n)
# cluster-robust SEs for reg_pmln4
cl.cov.pmlnr2 <- cluster.vcov(reg_pmlnr2, df_pml_n$`PA ID`) 
cl.robust.se.pmlnr2 <- sqrt(diag(cl.cov.pmlnr2))

## PPP
# OLS with TWFE 
reg_pppr1 <- lm(Vote_Share.1 ~ incidental_treatment + year_dummy + constituency_dummy, data = df_pppp)
# cluster-robust SEs for reg_pppp3
cl.cov.pppr1 <- cluster.vcov(reg_pppr1, df_pppp$`PA ID`) 
cl.robust.se.pppr1 <- sqrt(diag(cl.cov.pppr1))

# OLS with TWFE + 'count' control variable 
reg_pppr2 <- lm(Vote_Share.1 ~ incidental_treatment + year_dummy + constituency_dummy + count, data = df_pppp)
# cluster-robust SEs for reg_pppp4
cl.cov.pppr2 <- cluster.vcov(reg_pppr2, df_pppp$`PA ID`) 
cl.robust.se.pppr2 <- sqrt(diag(cl.cov.pppr2))

incidental_regressions <- stargazer(reg_pmlr2, reg_pmlnr2, reg_pppr2,  type="text", title="Effect of Incidental CJ visits on vote share ", keep = c('Constant','incidental_treatment', 'count'), order=c('Constant','incidental_treatment'), covariate.labels=c('Unvisited Districts', 'Incidental Districts', 'Controls'), column.labels=c('PLM', 'PLM-N', 'PPP'), dep.var.caption = '', dep.var.labels.include = FALSE, se = list(cl.robust.se.pmlr2, cl.robust.se.pmlnr2, cl.robust.se.pppr2), notes = c("Robust standard errors appear in brackets (clustered at the district level).",  "The table presents DiD estimation of the effect of Chief Justice visits to districts of", "Pakistan before 2008 elections on the share of votes in 2008 provincial", "and national elections (compare to 2002 elections). Outcome variable is visit to a given",  "district. Controls include: # of candidates in a district (count)."), notes.align = "l", keep.stat=c('n', 'adj.rsq', 'f'), add.lines = list("Mean" = c("Mean", round(mean(df_pml$Vote_Share.1), 2), round(mean(df_pml_n$Vote_Share.1), 2), round(mean(df_pppp$Vote_Share.1), 2))))


# Perform Regression Analysis (planned effects)
# PLM
# OLS with TWFE 
reg_pmlp1 <- lm(Vote_Share.1 ~ planned_treatment + year_dummy + constituency_dummy, data = df_pml)
# cluster-robust SEs for reg_pml3
cl.cov.pmlp1 <- cluster.vcov(reg_pmlp1, df_pml$`PA ID`) 
cl.robust.se.pmlp1 <- sqrt(diag(cl.cov.pmlp1))

# OLS with TWFE + 'count' control variable 
reg_pmlp2 <- lm(Vote_Share.1 ~ planned_treatment + year_dummy + constituency_dummy + count, data = df_pml)
# cluster-robust SEs for reg_pml4
cl.cov.pmlp2 <- cluster.vcov(reg_pmlp2, df_pml$`PA ID`) 
cl.robust.se.pmlp2 <- sqrt(diag(cl.cov.pmlp2))

## PML-N
# OLS with TWFE 
reg_pmlnp1 <- lm(Vote_Share.1 ~ planned_treatment + year_dummy + constituency_dummy, data = df_pml_n)
# cluster-robust SEs for reg_pmln3
cl.cov.pmlnp1 <- cluster.vcov(reg_pmlnp1, df_pml_n$`PA ID`) 
cl.robust.se.pmlnp1 <- sqrt(diag(cl.cov.pmlnp1))

# OLS with TWFE + 'count' control variable 
reg_pmlnp2 <- lm(Vote_Share.1 ~ planned_treatment + year_dummy + constituency_dummy + count, data = df_pml_n)
# cluster-robust SEs for reg_pmln4
cl.cov.pmlnp2 <- cluster.vcov(reg_pmlnp2, df_pml_n$`PA ID`) 
cl.robust.se.pmlnp2 <- sqrt(diag(cl.cov.pmlnp2))

## PPP
# OLS with TWFE 
reg_pppp1 <- lm(Vote_Share.1 ~ planned_treatment + year_dummy + constituency_dummy, data = df_pppp)
# cluster-robust SEs for reg_pppp3
cl.cov.pppp1 <- cluster.vcov(reg_pppp1, df_pppp$`PA ID`) 
cl.robust.se.pppp1 <- sqrt(diag(cl.cov.pppp1))

# OLS with TWFE + 'count' control variable 
reg_pppp2 <- lm(Vote_Share.1 ~ planned_treatment + year_dummy + constituency_dummy + count, data = df_pppp)
# cluster-robust SEs for reg_pppp4
cl.cov.pppr2 <- cluster.vcov(reg_pppp2, df_pppp$`PA ID`) 
cl.robust.se.pppp2 <- sqrt(diag(cl.cov.pppr2))

planned_regressions <- stargazer(reg_pmlp2, reg_pmlnp2, reg_pppp2, type="text", title="Effect of Planned CJ visits on vote share ", keep = c('Constant','planned_treatment', 'count'), order=c('Constant','planned_treatment'), covariate.labels=c('Constant', 'Planned Districts', 'Controls'), column.labels=c('PLM', 'PLM-N', 'PPP'), dep.var.caption = '', dep.var.labels.include = FALSE, se = list(cl.robust.se.pmlp2, cl.robust.se.pmlnp2, cl.robust.se.pppp2), notes = c("Robust standard errors appear in brackets (clustered at the district level).",  "The table presents DiD estimation of the effect of Chief Justice visits to districts of", "Pakistan before 2008 elections on the share of votes in 2008 provincial", "and national elections (compare to 2002 elections). Outcome variable is visit to a given",  "district. Controls include: # of candidates in a district (count)."), notes.align = "l", keep.stat=c('n', 'adj.rsq', 'f'), add.lines = list("Mean" = c("Mean", round(mean(df_pml$Vote_Share.1), 2), round(mean(df_pml_n$Vote_Share.1), 2), round(mean(df_pppp$Vote_Share.1), 2))))
