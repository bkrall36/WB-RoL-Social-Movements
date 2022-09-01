# Construct Bar Graph Variables 
df_02_08_bargraphs <- df_02_08 %>%
  # Filter for specific political parties of interest
  filter(`Party Initials` %in% c('PML', 'PML-N', 'PPPP')) %>%
  # Calculate summary statistics used to build confidence intervals 
  group_by(`Party Initials`, Year, `Treated before 2008 elections`) %>%
  summarise( 
    n=n(),
    mean=mean(Vote_Share.1),
    sd=sd(Vote_Share.1)) %>%
  mutate(se=sd/sqrt(n),
         ic=(se * qt((1-0.05)/2 + .5, n-1))) %>%
  # Rename column name and binary variable dummys to characters 
  rename(group = 'Treated before 2008 elections') 

df_02_08_bargraphs$group <- ifelse(df_02_08_bargraphs$group == 1, 'Visited', 'Never Visited')


# Bar Chart of Vote Share PML
# Subset data for PML
pml_bar <- df_02_08_bargraphs %>%
  filter(`Party Initials` == 'PML')

# Create bar chart for PML 
fig01_bargraphPML <- ggplot(data = pml_bar, aes(x=factor(Year), y=mean, groups=group, fill=group)) +
  geom_bar(colour='black', width=0.50, stat='identity', position=position_dodge(0.5)) + 
  geom_errorbar(aes(x=factor(Year), ymin=mean-ic, ymax=mean+ic), width=0.25, colour="#778899", alpha=1, size=1, position=position_dodge(0.5)) +
  ggtitle('Impact of CJ visits on General Musharraf’s Party - PML') + 
  labs(y= "Vote Share (%)", x = '', caption = str_wrap('Note: The figure presents the effect of Chief Justice visits to districts of Pakistan before 2008 elections on the share of votes in # favor of the PML party in 2008 provincial and national elections (compared to 2002 elections). The vertical lines reflect the 95% confidence intervals.')) +
  theme_light() + 
  theme(legend.position="right", legend.title=element_blank(), plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual("group", values = c("Visited" = "#2F4F4F", "Never Visited" = "#DCDCDC"))


# Bar Chart of Vote Share PML-N
# Subset data for PML
pmln_bar <- df_02_08_bargraphs %>%
  filter(`Party Initials` == 'PML-N')

# Create bar chart for PML 
fig02_bargraphPMLN <- ggplot(data = pmln_bar, aes(x=factor(Year), y=mean, groups=group, fill=group)) +
  geom_bar(colour='black', width=0.50, stat='identity', position=position_dodge(0.5)) + 
  geom_errorbar(aes(x=factor(Year), ymin=mean-ic, ymax=mean+ic), width=0.25, colour="#778899", alpha=1, size=1, position=position_dodge(0.5)) +
  ggtitle('Impact of CJ visits on Party Pledging Support for Lawyers Movment - PML-N') + 
  labs(y= "Vote Share (%)", x = '', caption=str_wrap('Note: The figure presents the effect of Chief Justice visits to districts of Pakistan before 2008 elections on the share of votes in # favor of PML-N party in 2008 provincial and national elections (compare to 2002 elections). The vertical lines reflect the 95% confidence intervals.')) +
  theme_light() + 
  theme(legend.position="right", legend.title=element_blank(), plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual("group", values = c("Visited" = "#2F4F4F", "Never Visited" = "#DCDCDC"))


# Bar Chart of Vote Share PPPP
# Subset data for PML
pppp_bar <- df_02_08_bargraphs %>%
  filter(`Party Initials` == 'PPPP')

# Create bar chart for PML 
fig03_bargraphPPP <- ggplot(data = pppp_bar, aes(x=factor(Year), y=mean, groups=group, fill=group)) +
  geom_bar(colour='black', width=0.50, stat='identity', position=position_dodge(0.5)) + 
  geom_errorbar(aes(x=factor(Year), ymin=mean-ic, ymax=mean+ic), width=0.25, colour="#778899", alpha=1, size=1, position=position_dodge(0.5)) +
  ggtitle('Impact of CJ visits on Nuetral Opposition Party - PPP') + 
  labs(y= "Vote Share (%)", x = '', caption = str_wrap('Note: The figure presents the effect of Chief Justice visits to districts of Pakistan before 2008 elections on the share of votes in # favor of PPP party in 2008 provincial and national elections (compared to 2002 elections). The vertical lines reflect the 95% confidence intervals.')) +
  theme_light() + 
  theme(legend.position="right", legend.title=element_blank(), plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual("group", values = c("Visited" = "#2F4F4F", "Never Visited" = "#DCDCDC"))
