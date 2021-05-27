library(tidytuesdayR)
library(tidyverse)
library(ggthemes)

# Download Data -----------------------------------------------------------

data <- tidytuesdayR::tt_load("2021-05-25")

records <- data$records

# Diverging Bars ----------------------------------------------------------
# Messing around with diverging plots

div <- records %>% 
  select(player, shortcut, type) %>% 
  group_by(player, type, shortcut) %>% 
  summarise(counts = n()) %>% 
  add_count(type,
            wt = counts) %>% 
  arrange(desc(n)) %>% 
  mutate(counts = if_else(type == "Single Lap",
                          counts,
                          -counts))

div_plt <- div %>% 
  filter(n > 5) %>% 
  ggplot(.,
         aes(x = fct_reorder(player,
                             desc(n)),
             y = n)) + 
  geom_col(aes(y = counts,
               fill = shortcut)) + 
  geom_hline(yintercept = 0,
             color = "black",
             linetype = 1) + 
  scale_y_continuous(breaks = seq(-175, 200, 25),
                     labels = abs(seq(-175, 200, 25))) + 
  scale_fill_manual(values = c("#0E5150", "#158673", "#905A39", "E1AA5F")) + 
  labs(x = "Players",
       y = "Number of Records",
       title = "Number of Player Records by Single/Three Lap Races and Use of Shortcuts",
       caption = "Only players with more than 5 records captured.") + 
  theme_clean() + 
  theme(axis.text.x = element_blank(),
        axis.line.x.bottom = element_blank(),
        axis.ticks.x.bottom = element_blank())

# Notes: 
# 1. Work on placing x-axis labels at y = 0 and have bars diverge out from that (geom_segment)
# 2. Single/Three Lap labels
# 3. Different colours for Single/Three and their respective shorts/non-shorts runs
