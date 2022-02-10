install.packages("zoo")
install.packages("scales")
library(tidyverse)
library(dplyr)
library(ggplot2)
library(zoo) #used for rollmean
library(scales)

setwd("~/school-projects/data_viz/discussion")


mtcars
# Get First Row
mtcars %>% slice(1)

# Get last row
mtcars %>% slice(n())

# Get 5 through last
mtcars %>% slice(5:n())

# get 5 lowest mpgs
mtcars %>% slice_min(mpg, n=5)

# get 5 largest mpgs
mtcars %>% slice_max(mpg, n=5)

states <- read_csv("covid.csv", 
                   col_types = cols(.default = 'c'))

states.clean <- states %>%
  mutate(date=as.Date(date),
         cases=as.numeric(cases),
         deaths=as.numeric(deaths))

table(states.clean$state)
length(unique(states.clean$state))

USA <- states.clean %>%
  filter(state == 'USA') %>%
  arrange(date) %>%
  mutate(daily_cases = cases - lag(cases),
         roll_average = rollmean(x=daily_cases, k=7, align='right', na.pad = T)) 

#lag gives you the case count from the day before
# align in rollmean positions the current date, right means the current date is last
pdf('USA.pdf')
USACOVID <- USA %>%
  ggplot(aes(x=date, y=roll_average)) +
  geom_bar(stat = "identity", aes(x=date, y=daily_cases), alpha=0.5,)+ #Required, it defaults to just expecting x or y and then counting the number of occurences of that x or y
  geom_line(color='red', size=1)+
  ylab("Daily Case Count")+
  labs(title= "Covid Cases in the United States",
       subtitle = "Plot of Daily New Cases",
       caption = "Data Source: New York Times")+
  scale_x_date(breaks = date_breaks('3 month'), labels = date_format("%b %y"))+
  scale_y_continuous(labels=comma)

options(scipen = 999) #Gets rid of scientific notation for big numbers

print(USACOVID)

dev.off()
