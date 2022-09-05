# Set WD for Outputs
setwd(Outputs)

# Parallel Trend Graphs
# Subset data for ease of manipulation
df_subset <- df %>% select(c(Year, Vote_Share.1, 'Party Initials', treated))

# Rename for clarity
df_subset <- df_subset %>%
  mutate(
    treated = ifelse(treated == 1, 'Visited', 'Never Visted')
  )

# Parallel Trend for PML                    
pml_trends <- df_subset %>%
  filter((`Party Initials` == 'PML') & (Year >= 1997)) %>%
  ggplot(aes(x = Year, y = Vote_Share.1, color = treated)) +
  stat_summary(fun.y = mean, geom = 'line', size = 1.25) +
  theme_bw() +
  labs(x = "Year", y = "% of Vote Share", title = "Evolution of Vote Share - PML (Years 1997 to 2012)") +
  geom_vline(xintercept = 2007) +
  scale_color_manual('Treated', values = c("Visited" = "#2F4F4F", "Never Visited" = "#DCDCDC")) +
  theme(legend.position="bottom", legend.title=element_blank(), plot.title = element_text(hjust = 0.5))
ggsave("linechart_pml_paralleltrends.pdf")


# Parallel Trend for PML-N                   
pmln_trends <- df_subset %>%
  filter((`Party Initials` == 'PML-N') & (Year >= 1997)) %>%
  ggplot(aes(x = Year, y = Vote_Share.1, color = treated)) +
  stat_summary(fun.y = mean, geom = 'line', size = 1.25) +
  theme_bw() +
  labs(x = "Year", y = "% of Vote Share", title = "Evolution of Vote Share - PML-N (Years 1997 to 2012)") +
  geom_vline(xintercept = 2007) +
  scale_color_manual('Treated', values = c("Visited" = "#2F4F4F", "Never Visited" = "#DCDCDC")) +
  theme(legend.position="bottom", legend.title=element_blank(), plot.title = element_text(hjust = 0.5))
ggsave("linechart_pmln_paralleltrends.pdf")

# Parallel Trend for PPPP                    
ppp_trends <- df_subset %>%
  filter(`Party Initials` == 'PPPP') %>%
  ggplot(aes(x = Year, y = Vote_Share.1, color = treated)) +
  stat_summary(fun.y = mean, geom = 'line', size = 1.25) +
  theme_bw() +
  labs(x = "Year", y = "% of Vote Share", title = "Evolution of Vote Share - PPP (Years 2002 to 2012)") +
  geom_vline(xintercept = 2007) +
  scale_color_manual('Treated', values = c("Visited" = "#2F4F4F", "Never Visited" = "#DCDCDC")) +
  theme(legend.position="bottom", legend.title=element_blank(), plot.title = element_text(hjust = 0.5))
ggsave("linechart_ppp_paralleltrends.pdf")