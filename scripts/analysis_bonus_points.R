# Script for analysis of bonus points
# data from https://github.com/vaastav/Fantasy-Premier-League
# Original version: Neil Rankin 2024-06-07
# Revised for repo: Neil Rankin 2024-06-09


# load libraries
library(tidyverse)

# get data. This data sits withiin this project (I've copied it across from the Vaastav repo)
# original data comes from Vaastav above. At the moment of writing, 23/24 season was missing last few games



merged_gw_24 <- read_csv("data/raw/merged_gw_24.csv") %>% 
  mutate(season = '23/24')

merged_gw_23 <- read_csv("data/raw/merged_gw_23.csv") %>% 
  mutate(season = '22/23') %>% 
  mutate(position = case_when(position == 'GKP' ~ 'GK', 
                              .default = position))
merged_gw_22 <- read_csv("data/raw/merged_gw_22.csv")  %>% 
  mutate(season = '21/22' )%>% 
  mutate(position = case_when(position == 'GKP' ~ 'GK', 
                              .default = position))

merged_gw_21 <- read_csv("data/raw/merged_gw_21.csv")  %>% 
  mutate(season = '20/21') %>% 
  mutate(position = case_when(position == 'GKP' ~ 'GK', 
                              .default = position))

# if you wanted to, one could go back and get data all teh way back to the 2016/17 season from the Vaastav repo



# now bind each season together ('stack' one on top of each other)

merged_gw <- merged_gw_24 %>% 
  bind_rows(merged_gw_23) %>% 
  bind_rows(merged_gw_22) %>% 
  bind_rows(merged_gw_21)


# create a 'bonus' data frame that can be used for the visualisations


bonus <- merged_gw %>% 
  group_by(season, fixture) %>% 
  mutate(score = paste0(team_h_score, '-', team_a_score)) %>% 
  group_by(fixture, score) %>%
  summarise(total_bonus = sum(bonus, na.rm = TRUE)) %>% 
  group_by(score) %>% 
  summarise(total_bonus = sum(total_bonus, na.rm = TRUE), 
            n_obs = n()) %>% 
  mutate(total_bonus_game = total_bonus/n_obs)


# now plot this

bonus %>% 
  ggplot(aes(y = reorder(score, total_bonus_game), x = total_bonus_game)) + 
  geom_col() + 
  ggtitle('Average bonus points per game') +
  labs(caption = "WWFD; @NeilRankinZA", 
       y = "score", 
       x = 'bonus per game')

# rinse and repeat for positions
# by position

bonus_position <- merged_gw %>% 
  group_by(season, fixture) %>% 
  mutate(score = paste0(team_h_score, '-', team_a_score)) %>% 
  group_by(fixture, score, position) %>%
  summarise(total_bonus = sum(bonus, na.rm = TRUE)) %>% 
  group_by(score, position) %>% 
  summarise(total_bonus = sum(total_bonus, na.rm = TRUE), 
            n_obs = n()) %>% 
  mutate(total_bonus_game = total_bonus/n_obs)

# faceting puts it into panels
bonus_position %>% 
  ggplot(aes(y = reorder(score, total_bonus_game), x = total_bonus_game, fill = position)) + 
  geom_col() + 
  ggtitle('Average bonus points per game, by position') +
  labs(caption = "WWFD; @NeilRankinZA", 
       y = "score (ordered by average bonus per game)", 
       x = 'bonus per game') + 
  facet_grid(~position)

# if you don't like the default colour scheme you could try 'ggthemes'
library(ggthemes)


# if you want to give people the impression that this coul dbe done in excel ;)
# and that you were using the older version of excel!

bonus_position %>% 
  ggplot(aes(y = reorder(score, total_bonus_game), x = total_bonus_game, fill = position)) + 
  geom_col() + 
  ggtitle('Average bonus points per game, by position') +
  labs(caption = "WWFD; @NeilRankinZA", 
       y = "score (ordered by average bonus per game)", 
       x = 'bonus per game') + 
  facet_grid(~position) + 
  theme_excel() +
  scale_colour_excel()

# I'm adding a ColorBrewer palette to the wsj base here.
# you'll need the ColorBrewer libarary

library(RColorBrewer)
bonus_position %>% 
  ggplot(aes(y = reorder(score, total_bonus_game), x = total_bonus_game, fill = position)) + 
  geom_col() + 
  ggtitle('Average bonus points per game, by position') +
  labs(caption = "WWFD; @NeilRankinZA", 
       y = "score (ordered by average bonus per game)", 
       x = 'bonus per game') + 
  facet_grid(~position) + 
  theme_wsj(base_size = 8) + 
  scale_fill_brewer(palette = 'Dark2')

# if you've gotten this far, and realised that it's taken you less time to draw these great visuals than install python, 
# then maybe think about giving R a go :)
# a great place to start is https://r4ds.hadley.nz/








