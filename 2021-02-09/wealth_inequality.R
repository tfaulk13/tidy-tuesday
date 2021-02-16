# Libraries and Data -----
library(tidyverse)
library(tidytuesdayR)
library(scales)
library(patchwork)

tuesdata <- tidytuesdayR::tt_load('2021-02-09')

# Grabbing four datasets for plot
home_owner <- tuesdata$home_owner
race_wealth <- tuesdata$race_wealth
retirement <- tuesdata$retirement
student_debt <- tuesdata$student_debt

# Creating theme for plots -----
tt_theme <- theme(
  plot.background = element_rect(fill = 'lightyellow'),
  panel.background = element_rect(fill = 'lightyellow'),
  panel.grid.minor = element_blank(),
  panel.grid.major.x = element_blank(),
  panel.grid.major.y = element_line(color = 'gray70'),
  plot.title = element_text(hjust = 0.5, face = 'bold'),
  legend.background = element_rect(fill = 'lightyellow'),
  legend.key = element_rect(fill = 'lightyellow', color = NA),
  axis.ticks.y = element_line(color = 'gray70'),
  axis.title = element_blank()
  ) 

# Plots -----

# Race Wealth
p_race_wealth <- race_wealth %>%
  filter(type == 'Median') %>%
  filter(race %in% c('Black', 'Hispanic', 'White')) %>%
  ggplot(aes(x = year, y = wealth_family, color = race)) + 
  geom_line() +
  geom_vline(xintercept = 2007) +
  scale_color_brewer(palette = 'Set1') +
  scale_x_continuous(limits = c(1995, 2016),
                     expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0),
                     labels = comma) +
  tt_theme +
  labs(title = 'Median Household Wealth')

# Home Ownership
p_home_owner <- ggplot(home_owner, aes(x = year, y = home_owner_pct, color = race)) + 
  geom_line() +
  geom_vline(xintercept = 2007) +
  scale_x_continuous(limits = c(1995, 2016)) +
  scale_color_brewer(palette = 'Set1') +
  scale_x_continuous(limits = c(1995, 2016),
                     expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0),
                     labels = percent) +
  tt_theme +
  labs(title = 'Home Ownership Percentage')

# Retirement Savings
p_retirement <- ggplot(retirement, aes(x = year, y = retirement, color = race)) +
  geom_line() +
  geom_vline(xintercept = 2007) +
  scale_x_continuous(limits = c(1995, 2016)) +
  scale_color_brewer(palette = 'Set1') +
  scale_x_continuous(limits = c(1995, 2016),
                     expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0),
                     labels = comma) +
  tt_theme +
  labs(title = 'Average Retirement Savings')

# Avg student loan debt
p_student_debt <- ggplot(student_debt, aes(x = year, y = loan_debt, color = race)) +
  geom_line() +
  geom_vline(xintercept = 2007) +
  scale_color_brewer(palette = 'Set1') +
  scale_x_continuous(limits = c(1995, 2016),
                     expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0),
                     labels = comma) +
  tt_theme +
  labs(title = 'Average Student Loan Debt')

# Patchwork to bring it all together
p_race_wealth + p_retirement + p_home_owner + p_student_debt +
  plot_annotation(title = 'How did the Great Recession Impact Different Races?',
                  subtitle = 'The four graphs below look at key economic metrics before and after 2007.') +
  plot_layout(guides = 'collect') &
  theme(plot.title = element_text(hjust = 0.5, size = 15, face = 'bold'),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        plot.background = element_rect(fill = 'lightyellow'),
        legend.position = 'top',
        legend.title = element_blank(),
        legend.text = element_text(size = 12))
