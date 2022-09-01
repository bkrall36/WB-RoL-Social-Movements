# Set WD for Outputs
setwd(Outputs)

# Event Study Figure
# Store the beta coefficients as X and Y pairs for plotting 
summary <- as.data.frame(event_study_reg$coefficients)
y <- as.data.frame(summary[2:4,])
x <- as.data.frame(c(-1, 0, 1))

# Generate CI at 95% level for time_since_treatment variable
cis <- confint(event_study_reg, level=0.95)
cis <- cis[2:4, ]

# Create dataframe for plotting
combined_df <- cbind(x, y, cis)

fig04_eventstudy <- ggplot(combined_df, aes(x=`c(-1, 0, 1)`, y=`summary[2:4, ]`)) +
  geom_pointrange(aes(ymin=`2.5 %`, ymax=`97.5 %`)) +
  labs(x='Time to Treatment (years)', y='Effect Size (pp of Vote Share)', title='Average Treatment Effect of CJ Visit') +
  geom_line() +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))
ggsave("eventstudy.pdf")