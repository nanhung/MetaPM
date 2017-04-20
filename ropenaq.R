# https://ropensci.org/blog/blog/2017/02/21/ropenaq

rm(list = ls())

library("ropenaq")
library("dplyr")
library("import")
library("knitr")
#
#import::from(dplyr, filter)
countries <- aq_countries()
cities <- aq_cities()
#filter(countries, name == "India")
#
in_cities <- aq_cities(country = "IN")
#filter(in_cities, city == "Hyderabad")
#
aq_locations(city = "Hyderabad") %>%
  knitr::kable()

aq_locations(city = "Hyderabad", parameter = "pm25")
#
first_test <- aq_measurements(city = "Hyderabad",
                              date_from = "2016-01-01",
                              date_to = "2016-12-31",
                              parameter = "pm25")
#count <- attr(first_test, "meta")$found
count <- nrow(first_test)
print(count)
#
library("purrr")
allthedata <- (1:ceiling(count/10000)) %>%
  purrr::map(function(page){
    aq_measurements(city = "Hyderabad",
                    date_from = "2016-01-01",
                    date_to = "2016-12-31",
                    parameter = "pm25",
                    page = page,
                    limit = 10000)
  }) %>%
  bind_rows()

allthedata
#
library("ggplot2")
library("viridis")
allthedata %>%
  filter(value != - 999) %>%
  group_by(day = as.Date(dateLocal), location) %>%
  filter(n() > 0) %>%
  summarize(average = mean(value)) %>%
  ggplot() +
  geom_line(aes(x = day, y = average, col = location)) +
  facet_grid(location ~ .) +
  geom_hline(yintercept = 25) +
  scale_color_viridis(discrete = TRUE) +
  theme(legend.position = "none",
        strip.text.y = element_text(angle=0))+
  ylab(expression(paste("Average daily PM2.5 concentration (", mu, "g/",m^3,")"))) +
  xlab("Time (days)")
#
library("emojifont")
library("magick")
library("ggthemes")

load.emojifont('OpenSansEmoji.ttf')
load.emojifont(font = "OpenSansEmoji.ttf")

lima <- aq_measurements(country = "PE", limit = 1000)
lima <- filter(lima, location == "US Diplomatic Post: Lima")
lima <- mutate(lima, label = emoji("surfer"))

figure_onetime <- function(now, lima){
  
  p <- ggplot(lima)+
    geom_area(aes(x = dateLocal,
                  y = value),
              size = 2, fill = "navyblue")+
    geom_text(aes(x = dateLocal,
                  y = value+1,
                  label = label),
              col = "goldenrod",
              family="OpenSansEmoji", size=20,
              data = filter_(lima, ~dateLocal == now))+
    ylab(expression(paste("PM2.5 concentration (", mu, "g/",m^3,")")))+
    xlab('Local date and time, Lima, Peru')+
    ylim(0, 50)+
    ggtitle(as.character(now))+
    theme_hc(bgcolor = "darkunica") +
    scale_colour_hc("darkunica")+
    theme(text = element_text(size=40)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))+
    theme(plot.title=element_text(family="OpenSansEmoji",
                                  face="bold"))+
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank())
  outfil <- gsub("-", "", now)
  outfil <- gsub(" ", "", outfil)
  outfil <- gsub("[:punct:]", "", outfil)
  outfil <- paste0(outfil, ".png")
  ggsave(outfil, p, width=8, height=5)
  
  outfil
}

sort(unique(lima$dateLocal)) %>%
  map(figure_onetime, lima = lima)  %>%
  map(image_read) %>%
  image_join() %>%
  image_animate(fps=2) %>%
  image_write("surf.gif")

###
first_test <- aq_measurements(country = "US",
                              has_geo = TRUE,
                              parameter = "pm25",
                              limit = 10000,
                              date_from = "2016-07-04",
                              date_to = "2016-07-06",
                              value_from = 0)
count <- nrow(first_test)
print(count)

library("purrr")

usdata <- (1:ceiling(count/10000)) %>%
  purrr::map(function(page){
    aq_measurements(country = "US",
                    has_geo = TRUE,
                    parameter = "pm25",
                    limit = 10000,
                    date_from = "2016-07-04",
                    date_to = "2016-07-06",
                    value_from = 0,
                    page = page)
  }) %>%
  bind_rows()

usdata <- usdata %>%
  group_by(hour = update(dateUTC, minute = 0),
           location, longitude, latitude, dateUTC) %>%
  summarize(value = mean(value))

usdata <- usdata %>%
  ungroup() %>%
  mutate(hour = update(hour, hour = lubridate::hour(hour) - 5)) %>%
  mutate(value = ifelse(value > 80, 80, value))
save(usdata, file = "4th_july.RData")

#
devtools::install_github("hrbrmstr/albersusa")
install.packages("unit")
library("albersusa")
load("4th_july.RData")

mintime <- lubridate::ymd_hms("2016 07 04 17 00 00")
maxtime <- lubridate::ymd_hms("2016 07 05 07 00 00")

usdata <- filter(usdata, hour >= mintime)
usdata <- filter(usdata, hour <= maxtime)

us <- usa_composite()
us_map <- fortify(us, region="name")
us_map <- filter(us_map, !id %in% c("Alaska", "Hawaii"))

gg <- ggplot()
gg <- gg + geom_map(data=us_map, map=us_map,
                    aes(long, lat, map_id=id),
                    color="white", size=0.1, fill="black")
gg <- gg + theme_map(base_size = 40)
gg <- gg + theme(plot.title = element_text(color="white"))
gg <- gg + theme(legend.position = "bottom")
gg <- gg + theme(panel.background = element_rect(fill = "black"))
gg <- gg + theme(plot.background=element_rect(fill="black"))
gg <- gg + theme(legend.background= element_rect(fill="black", colour= NA))
gg <- gg + theme(legend.text = element_text(colour="white"))
gg <- gg + theme(legend.title = element_text(colour="white"))

# find the maximal number of data points for the period
lala <- group_by(usdata, location, latitude) %>% summarize(n = n())
# and keep only stations with data for each hour
usdata <- group_by(usdata, location, latitude) %>%
  filter(n() == max(lala$n),
         latitude < 50, longitude > - 130) %>%
  ungroup()

firework_onehour <- function(now, gg, usdata){
  p <- gg+
    geom_point(data = filter_(usdata, ~ hour == now),
               aes(x=longitude,
                   y =latitude,
                   colour = value,
                   size = value))+
    ggtitle(as.character(now)) +
    coord_map()+
    viridis::scale_color_viridis(expression(paste("PM2.5 concentration (", mu, "g/",m^3,")Set to 80 if >80")),
                                 option = "inferno",
                                 limits = c(min(usdata$value),
                                            max(usdata$value))) +
    scale_size(limits = c(min(usdata$value),
                          max(usdata$value)))
  outfil <- gsub("-", "", now)
  outfil <- gsub(" ", "", outfil)
  outfil <- gsub("[:punct:]", "", outfil)
  outfil <- paste0(outfil, "_fireworks.png")
  ggsave(outfil, p, width=12, height=6)
  
  outfil
}

library("mapproj")
sort(unique(usdata$hour)) %>%
  map(firework_onehour, gg = gg, usdata = usdata)  %>%
  map(image_read) %>%
  image_join() %>%
  image_animate(fps=1) %>%
  image_write("fireworks.gif")