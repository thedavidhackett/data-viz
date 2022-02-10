url <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"

global <- read_csv(url, col_types = cols(.default = "c")) %>%
  pivot_longer(cols = 5:727, names_to = "date", values_to = "count") %>%
  select(-Lat, -Long)

global %>%
  group_by(`Country/Region`) %>%
  count()

global %>%
  filter(`Country/Region` == "Australia") %>%
  view()

global.clean <- global %>%
  mutate(date = as.Date(date, tryFormats="%m/%d/%y"), 
         count= as.numeric(count)) %>%
#  separate(date, into= c("month", "day", "year")) %>%
#  mutate(month = str_pad(month, width = 2, pad="0"),
#         day = str_pad(day, width = 2, pad="0"),
#         year = str_c("20", year, sep=""),
#         date =  as.Date(str_c(year, month, day, sep="-"))) %>%
#  select(-month,-day, -year)
  group_by(`Country/Region`, date) %>%
  arrange(date) %>%
  summarise(count = sum(count, na.rm = T)) %>%
  ungroup(`Country/Region`)

global.clean %>%
  filter(`Country/Region` == "Australia") %>%
  view()

interest <- c("US", "United Kingdom", "Spain", "France", "India", "Brazil")

global.interest <- global.clean %>%
  filter(`Country/Region` %in% interest)


global.interest.reindexed <- global.interest %>%
  filter(count >= 10) %>%
  group_by(`Country/Region`) %>%
  mutate(base_date = first(date), #first finds the first date
         diff = date - base_date,
         number_of_days = as.numeric(diff, "days"))  

make_chart <- function(enddate) {

global.interest.reindexed %>%
  ggplot(aes(number_of_days, count, group = `Country/Region`)) +
  geom_line(aes(color = `Country/Region`)) + 
  scale_y_log10() +
  geom_text(data=global.interest.reindexed %>%
              group_by(`Country/Region`) %>%
              slice(n()) %>%
              ungroup(), aes(number_of_days, count, label = `Country/Region`), color="red")

}

