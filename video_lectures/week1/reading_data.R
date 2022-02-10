install.packages("tidyverse")
library(tidyverse)

states <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv",
                   col_types= cols(.default = "c"))

states.clean <- states %>%
  mutate(cases = as.numeric(cases), 
         deaths = as.numeric(deaths), 
         date = as.Date(date))
