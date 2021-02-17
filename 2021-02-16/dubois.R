# Libraries and data -----
library(tidyverse)
library(tidytuesdayR)
library(scales)

tuesdata <- tt_load('2021-02-16')

df$Free <- percent(df$Free / 100, accuracy = 1)


# Challenge 04 -----

df <- tuesdata$freed_slaves

plot <- ggplot(df, aes(x = Year, y = Slave)) +
  geom_area(fill = '#000000') +
  geom_label(data = df[df$Free != '100%',], aes(x = Year, y = Slave, label = Free), 
            position = position_nudge(y = 2, x = 1.5), color = '#000000', fill = '#00aa00',
            label.size = NA, label.padding = unit(0, 'lines'), size = 4, family = 'sans') +
  geom_text(x = 1830, y = 50, label = 'SLAVES\nESCLAVES', 
            color = '#FAFBF5', size = 7, family = 'sans', lineheight = 0.8) +
  geom_text(x = 1830, y = 95, label = 'FREE - LIBRE',
            color = '#000000', size = 5, family = 'sans') +
  scale_x_continuous(expand = c(0,0),
                     position = 'top',
                     breaks = df$Year) +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0, 100)) +
  theme(panel.background = element_rect(fill = '#00aa00'),
        plot.background = element_rect(fill = '#d2b48c'),
        plot.margin = unit(c(1,0.5,0,0.5), 'cm'),
        plot.title = element_text(size = 12, face = 'bold', family = 'sans', hjust = 0.5),
        plot.subtitle = element_text(size = 10, face = 'bold', family = 'sans', hjust = 0.5),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_line(color = '#000000', size = 0.1),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(size = 12, color = '#000000', face = 'bold'),
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  labs(x = element_blank(),
       y = element_blank(),
       title = 'PROPORTION OF FREEMEN AND SLAVES AMONG AMERICAN NEGROES .\n\nPROPORTION DES NÈGRES LIBRES ET DES ESCLAVES EN AMÉRIQUE .\n',
       subtitle = 'DONE BY ATLANTA UNIVERSITY .\n\n\n')

plot

ggsave(plot, filename = '2021-02-16/dubois.png')

# Challenge 07 -----
# Going to have to come back to this...
df <- tuesdata$furniture

names(df) <- c('year', 'dollars')

ggplot(df, aes(x = desc(factor(year)), y = dollars, fill = factor(year))) +
  geom_bar(stat = 'identity', width = 1) +
  coord_polar(theta = 'y') +
  scale_fill_manual(values = c('#ffc0cb', '#0000ff', '#D3D3D3', '#ffd700', '#FAFBF5', '#dc143c')) +
  ylim(0, 1500000) +
  geom_blank()

#+
  
