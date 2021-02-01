
# Libraries -------
library(tidytuesdayR)
library(tidyverse)

# Data ------------
tuesdata <- tt_load('2021-01-05')
df <- tuesdata$transit_cost


# Boxplot graph ordered by median -----
df %>%
  group_by(country) %>%
  filter(n() >= 10) %>%
  ggplot(aes(x = reorder(country, cost_km_millions, FUN = median), y = cost_km_millions)) +
  geom_boxplot(fill = '#919c4c') +
  scale_y_continuous(expand = c(0.02,0)) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.background = element_rect(fill = '#EFE1C6'),
        plot.background = element_rect(fill = '#FFFEEA'),
        plot.title = element_text(face = 'bold', size = 20, hjust = 0.5),
        plot.subtitle = element_text(size = 10, hjust = 0.5)) +
  labs(title = 'In the United States, transit projects\nare much more expensive',
       subtitle = 'The median cost of transit projects in the United States is more expensive\nthan the most expensive transit project in the ten other qualifying countries.',
       x = 'Countries with 10 or more transit projects',
       y = 'Cost per KM (millions of USD)',
       caption = 'Source: Transit Costs Project')
  


