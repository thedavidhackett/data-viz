rm(list=ls())

library(tidyverse)
library(scales)
library(covidcast)


state_level = read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv")


state_level %>%
  filter(state == "New Hampshire") %>%
  head()

northeast <- c("New York", "Maine", "Vermont", "New Hampshire", "Massachusetts", 
               "Connecticut", "Rhode Island",)

mask_data <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/mask-use/mask-use-by-county.csv")

county_data <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv")

county_data.filtered <- county_data %>%
  filter(date < "2021-01-01" )

totals <- county_data.filtered %>%
  group_by(fips) %>%
  summarise(total_cases = max(cases))


head(county_census)
head(county_census)

county_cases_masking <- merge(totals, mask_data, by.x="fips", by.y = "COUNTYFP")
county_cases_masking <- merge(county_cases_masking, county_census, by.x="fips", by.y="FIPS")

county_cases_masking$covid_rate <- county_cases_masking$total_cases / county_cases_masking$POPESTIMATE2019
county_cases_masking$high_mask_use <- county_cases_masking$FREQUENTLY + county_cases_masking$ALWAYS

st.reg <- data.frame(State = state.name, Region = state.region)

county_cases_masking <- merge(county_cases_masking, st.reg, by.x="STNAME", by.y="State")


pdf("masking_v_covid.pdf", width = 13, height = 8)
county_cases_masking %>%
  ggplot(aes(x=high_mask_use, y=covid_rate)) +
  geom_point(aes(size=POPESTIMATE2019, color=Region), alpha=0.7) +
  geom_smooth(se=FALSE, color="black") +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_x_continuous(labels = percent_format(accuracy = 1)) +
  scale_size_continuous(breaks = c(0, 1000, 10000, 100000, 1000000), range = c(1,20), labels = comma, name="County Population")+
  ylab("2020 Covid Rate in County")+
  xlab("Percent of County that frequently/always masks") +
  labs(title= "Rate of Covid vs Rate of Masking in 2020") +
  theme_light()

dev.off()

View(state.x77)

