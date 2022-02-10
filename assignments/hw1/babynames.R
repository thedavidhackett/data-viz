rm(list = ls())
install.packages("babynames")
library(babynames)

head(babynames)
head(applicants)
tail(lifetables)


top_names <- babynames %>% 
  group_by(year, sex) %>%
  summarise(highest_prop = max(prop), count = n())

head(top_names)

top_names %>%
  ggplot(aes(x=year, y=count)) +
  geom_line(color='red', size=1)


president_names <- babynames %>%
  filter(name=="Taylor")


wider <- babynames %>%
  pivot_wider(id_cols = c(name, year), 
              names_from = sex, 
              values_from = c(prop, n),
              values_fill = 0)


check_unisex <- function(num_male, num_female) {
  total = num_male + num_female
  return (num_male / total > 0.01 & num_male / total < 0.99)
}

unisex_names <- wider %>%
  group_by(year) %>%
  summarise(unisex = sum(n_F[check_unisex(n_M, n_F)]) +
              sum(n_M[check_unisex(n_M, n_F)]),
            total = sum(n_F) + sum(n_M),
            prop = unisex/total
            )




taylor <- babynames %>%
  filter(name=="Taylor") %>%
  pivot_wider(id_cols=year, values_from = n, names_from = sex, values_fill = 0) %>%
  mutate(prop_m = M / (M + `F`))
  
charlie <- babynames %>%
  filter(name=="Belle") %>%
  pivot_wider(id_cols=year, values_from = n, names_from = sex)


bad_names <- c("Barack", "George", "William", "Ronald", "Jimmy", "Lyndon", "Richard")


bad_names_by_year <- babynames %>%
  filter(name %in% bad_names, sex=="M") %>%
  pivot_wider(id_cols = year, values_from = n, names_from = name)


babynames_change <- babynames %>%
  mutate(endsy = str_sub(name, -1) == "y", 
         endsi = str_sub(name, -1) == "i",
         endsie = str_sub(name, -2, -1) == "ie") %>%
  filter(endsy | endsi | endsie) %>%
  group_by(year) %>%
  summarise(total = sum(n), 
            y = sum(n[endsy]) / total,
            i = sum(n[endsi]) / total,
            ie = sum(n[endsie]) / total
            )


names_2000 <- c('Elsa', 'Tiana', 'Arya', 'Fiona')

babynames_2000 <- babynames %>%
  filter(name %in% names_2000, year > 1997, sex=="F") %>%
  pivot_wider(id_cols = year, values_from = n, names_from = name, values_fill = 0)


years = seq(1998, 2016, 6)

pdf("princess_effect.pdf")
babynames_2000 %>%
  ggplot(aes(x=year, y=n))+
  geom_line(aes(color=name))+
  theme_classic() +
  scale_x_continuous(breaks=years, labels=years)
dev.off()




