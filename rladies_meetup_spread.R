library(dplyr)
library(lubridate)
library(stringr)
library(ggmap)
library(purrr)
library(tidyr)
library(tibble)
library(maps)
library(ggthemes)
library(plotly)

library(devtools)
#devtools::install_github('thomasp85/gganimate')
library(gganimate)

#devtools::install_github("rladies/meetupr")
library(meetupr)

#Sys.setenv(MEETUP_KEY = "")

api_key <- Sys.getenv("MEETUP_KEY")
rladies_groups <- find_groups(text = "r-ladies", api_key = api_key)%>%
  #dplyr::filter(., grepl("R-Ladies",name))
  dplyr::filter(organizer=="R-Ladies Global")


countries_count <- rladies_groups%>%group_by(country)%>%summarise(n=n())
members_count <- rladies_groups%>%summarise(n=sum(members))
cities_count <- rladies_groups%>%group_by(city)%>%summarise(n=n())


# urlname <- "rladies-helsinki"
# members <- get_members(urlname)


##Idea and original code by https://github.com/d4tagirl/R-Ladies-growth-maps 
#··················
# plotly

world <- ggplot() +
  borders("world", colour = "gray85", fill = "gray80") +
  theme_map()

map <- world +
  geom_point(aes(x = lon, y = lat,
                 text = paste(name,
                              '<br /> created : ', created),
                 size = members),
             data = rladies_groups, colour = 'purple', alpha = .5) +
  scale_size_continuous(range = c(1, 9), breaks = c(250, 500, 750, 1000)) +
  labs(size = 'Followers')

ggplotly(map, tooltip = c('text', 'size'))

#··············
# static map 

map_static <- world +
  geom_point(aes(x = lon, y = lat,
                 size = members),    # add the size aes for later gganimate
             data = rladies_groups, 
             colour = 'purple', alpha = .5) +
  scale_size_continuous(range = c(1, 8), 
                        breaks = c(250, 500, 750, 1000)) +
  labs(size = 'Members')

#··············
# gganimate map 

# init point to show empty map in the beggining
ghost_point <- rladies_groups %>%
  add_row(
    created = as.Date('2011-09-01'),
    members = 0,
    lon = 0,
    lat = 0,
    .before = 1) %>%
  slice(1) %>% 
  mutate(date = format(created, format = '%Y-%m-%d'),
         est_members = 0)

dates <- as_tibble(seq(floor_date(as.Date(min(rladies_groups$created)), 
                                  unit = "month"),
                       today(),
                       by = 'days')) %>%
  filter(day(value) %in% c(1, 10, 20))

rladies_frames <- rladies_groups %>%
  select(name) %>%
  expand(name, date = dates$value) %>%
  right_join(rladies_groups, by = 'name') %>%
  filter(date > created) %>%
  mutate(date = format(date, format = '%Y-%m-%d'),
         age_days = difftime(as.Date('2017-5-15'), created, unit = 'days'),
         age_total = as.numeric(age_days, units = 'days'),
         age_at_date = as.numeric(difftime(date, created, units = 'days'),
                                  units = 'days'),
         est_members = ((members - 1) / age_total) * age_at_date)

rladies_less_frames <- rladies_frames %>%
  filter((day(date) == 1 & month(date) %% 6 == 0) |
           date >= rladies_groups$created[rladies_groups$name == 'R-Ladies London'])

####animated map




