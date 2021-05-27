# Libraries ---------------------------------------------------------------
# Learning to use C. Wilke's ggtext library

library(tidyverse)
library(ggtext)

# Data Input --------------------------------------------------------------

data <- read.csv("{/path/to/datafile}",
                 header = TRUE,
                 check.names = FALSE,
                 stringsAsFactors = FALSE,
                 na.strings = "NA")
                 
# Data Wrangling ----------------------------------------------------------
# In an epidemiological context, "Nationwide" data point not very useful
# Better to use cases per 100,000

weeks <- data %>% 
  select(States, `TOTAL CASES`, `TOTAL SPORADIC CASES`, Week) %>%
  drop_na() %>% 
  filter(States != "Nationwide") %>%
  arrange(Week, desc(`TOTAL CASES`)) %>% 
  mutate(Week = as.character(paste0("Week ", Week)),
         States = factor(States,
                         levels = unique(States)))
                         
# Bar ---------------------------------------------------------------------

bar <- weeks %>% 
  ggplot(aes(x = States,
             fill = Week)) +
  geom_bar(aes(y = `TOTAL CASES`),
           stat = "identity",
           position = "dodge",
           alpha = 0.45) + 
  geom_bar(aes(y = `TOTAL SPORADIC CASES`),
           stat = "identity",
           position = "dodge") + 
  scale_fill_manual(values = c("#e06809",
                               "#0981e0")) + 
  scale_y_continuous(breaks = c(0, 2000, 4000, 6000, 8000, 10000),
                     labels = c("0", "2000", "4000", "6000", "8000", "10000"),
                     expand = expansion(mult = c(0, 0.1))) + 
  facet_grid(. ~ Week) + 
  labs(y = "Total Cases",
       title = "Proportion of <span style = 'color:#e06809;'>Sporadic Covid-19 Cases</span> to <span style = 'color:#e0680973;'>Total Covid-19 Cases</span> By State",
       subtitle = "Epidemiological Week 18 and 19 between May 2 and May 15, 2021.",
       caption = "Data source: Ministry of Health") + 
  theme_clean() + 
  theme(axis.title.x = element_blank(),
        plot.caption = element_markdown(),
        plot.title = element_markdown(),
        axis.text.x = element_text(angle = 90,
                                   hjust = 1,
                                   vjust = 0.1),
        legend.position = "none")
                 
