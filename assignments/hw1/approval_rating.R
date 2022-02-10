rm(list=ls())
setwd("~/school-projects/data_viz/assignments/hw1")

library(tidyverse)
library(scales)

approval <- read_csv("approval_topline.csv")

approval.filtered <- approval %>%
  filter(subgroup=="All polls") %>%
  mutate(diff= (approve_estimate - disapprove_estimate)/100, modeldate = as.Date(modeldate, format="%m/%d/%Y"))



head(approval.filtered)
pdf("approval.pdf")
approval_chart <- approval.filtered %>%
  ggplot(aes(x=modeldate, y=diff)) +
  geom_bar(stat = "identity", aes(x=modeldate, y=diff, color=ifelse(diff > 0, "red", "blue")), alpha=0.5) +
  theme_light()+
  theme(legend.position = "None") +
  ylab("Net Approval Rating")+
  xlab("Date") +
  labs(title= "Biden's Net Approval Rating") +
  scale_x_date(breaks=date_breaks("2 months"), labels=date_format("%b %Y"))+
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(-0.2, 0.2))

print(approval_chart)
dev.off()
