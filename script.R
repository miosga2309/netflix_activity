library(dplyr)
library(tidyr)
library(lubridate)
library(zoo)
library(ggplot2)
library(plotly)

# dates
minetflix <- read.csv("/Users/jonasmiosga/Desktop/netflix_activity/NetflixViewingHistory.csv") 
str(minetflix)
minetflix$Date <- dmy(minetflix$Date)

# splitting columns (title, season, episode)
minetflix_serie <- minetflix %>%
  separate(col = Title, into = c("title", "season", "title_episode"), sep = ': ')
head(minetflix_serie)

# handle NAs
minetflix_serie <- minetflix_serie[!is.na(minetflix_serie$season),]
minetflix_serie <- minetflix_serie[!is.na(minetflix_serie$title_episode),]
head(minetflix_serie)

# counts per day
marathons_minetflix <- minetflix_serie %>%
  count(title, Date)
marathons_minetflix

# analyse marathons
marathons_minetflix <- marathons_minetflix[marathons_minetflix$n >= 6,]
marathons_minetflix
marathons_minetflix <- marathons_minetflix[order(marathons_minetflix$Date),]
marathons_minetflix


# total marathons
marathons_minetflix_total <- marathons_minetflix %>% 
  group_by(title) %>% 
  summarise(n = sum(n)) %>%
  arrange(desc(n))
marathons_minetflix_total

# Top 10 marathon shows
marathons_minetflix_top <- marathons_minetflix_total %>% 
  top_n(10) %>%
  ggplot(aes(x = reorder(title, n), y = n)) +
  geom_col(fill = "#0097d6") +
  coord_flip() +
  ggtitle("Top 10 marathon shows", "4 or more episodes per day") +
  labs(x = "Show on Netflix", y = "Total episodes watched") +
  theme_minimal()
marathons_minetflix_top
ggplotly()


# episodes per day
netflix_episodes_day <- minetflix %>%
  count(Date) %>%
  arrange(desc(n))
head(netflix_episodes_day)

# visual for episodes per day
netflix_episodes_day_plot <- ggplot(aes(x = Date, y = n, color = n), 
                                     data = netflix_episodes_day) +
  geom_col(color = c("#f16727")) +
  theme_minimal() +
  ggtitle("Episodes watched on Netflix per day", "History from 2016 to 2020") +
  labs(x = "Date", y = "Episodes watched") 
netflix_episodes_day_plot
ggplotly()



# calendar view on episodes watched per day
netflix_episodes_day <- netflix_episodes_day[order(netflix_episodes_day$Date),]
netflix_episodes_day$wday <- wday(netflix_episodes_day$Date)
netflix_episodes_day$weekdays <- weekdays(netflix_episodes_day$Date, abbreviate = T)
netflix_episodes_day$months <- months(netflix_episodes_day$Date, abbreviate = T)

netflix_episodes_day$weekdays <-factor(netflix_episodes_day$wday,
                                          levels = rev(1:7),
                                          labels = rev(c("Mon","Tue","Wed","Thu","Fri","Sat","Sun")),
                                          ordered = TRUE) 
netflix_episodes_day$months <- factor(month(netflix_episodes_day$Date),
                                     levels = as.character(1:12), 
                                     labels = c("January","February","March","April","May","June","July","August","September","October","November","December"),
                                     ordered = TRUE)

netflix_episodes_day$yearmon <- factor(as.yearmon(netflix_episodes_day$Date)) 
netflix_episodes_day$weeks <- as.numeric(format(netflix_episodes_day$Date,"%W"))
netflix_episodes_day$weekmon <- ceiling(day(netflix_episodes_day$Date) / 7)

netflix_episodes_day_calendar <- ggplot(netflix_episodes_day, aes(weekmon, weekdays, fill = netflix_episodes_day$n)) + 
  geom_tile(colour = "white") + 
  facet_grid(year(netflix_episodes_day$Date) ~ months) + 
  scale_fill_gradient(low = "#FFD000", high = "#FF1919") + 
  ggtitle("Episodes watched per day", "Heatmap per weekday, month and year") +
  labs(x = "Week number", y = "Weekday") +
  labs(fill = "# Episodes")
netflix_episodes_day_calendar
ggplotly()


# activity frequency on Netflix per day
visits_day <- netflix_episodes_day %>%
  count(weekdays)
visits_day

visits_day_plot <- visits_day %>% 
  ggplot(aes(weekdays, n)) +
  geom_col(fill = "#5b59d6") +
  coord_polar()  +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(face = "bold"),
        plot.title = element_text(size = 16, face = "bold")) +
  ggtitle("Frequency of visits", "Activity per weekday on Netflix")
visits_day_plot

# Activity frequency per month
visits_month <- netflix_episodes_day %>%
  count(months)
visits_month

visits_month_plot <- visits_month %>% 
  ggplot(aes(months, n)) +
  geom_col(fill = "#808000") +
  coord_polar()  +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(face = "bold"),
        plot.title = element_text(size = 18, face = "bold")) +
  ggtitle("Frequency of visits", "Activity per month on Netflix") 
visits_month_plot


# Activity frequency per year
visits_year <- netflix_episodes_day %>%
  count(yearmon)
visits_year

visits_year_plot <- visits_year %>% 
  ggplot(aes(yearmon, n)) +
  geom_col(fill = "#1a954d") +
  coord_polar()  +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(face = "bold"),
        plot.title = element_text(size = 18, face = "bold")) +
  ggtitle("Frequency of visits", "Activity per year and month on Netflix")
visits_year_plot
