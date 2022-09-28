
library("googleAnalyticsR")
library("plyr")
library("tidyverse")
library("maps")
library("ggmap")
library("ggpubr")

register_google(key = "", account_type = "premium", day_limit = 100000)

# Geocode locations
covstat_geocode <- function(df, address){
  coordinates <- geocode(address, source = "google")
  df <- cbind(df, coordinates)
  
  # Add zero to NAs in lat and lon if geocode fails
  message(paste(sum(is.na(df$lat))), " rows with NAs found in lat and long columns", sep = " ")
  if(sum(is.na(df$lat > 0))){
    for (i in 1:length(df$lat)){
      if(is.na(df$lat[i]))
        df$lat[i] <- 0
    }
    for (i in 1:length(df$lon)){
      if(is.na(df$lon[i]))
        df$lon[i] <- 0
    }
  }
  df
}


ga_auth()

ga_list <- ga_account_list()

ga_id <- ga_list$viewId[2]


#///////////////////////////////
## State aggregate visitors ----
#///////////////////////////////

# Dimension filters
dimension1 <- dim_filter("city","PARTIAL","(not set)", not = TRUE)
dimension2 <- dim_filter("country","PARTIAL","States", not = FALSE)

dfilter <- filter_clause_ga4(list(dimension1,dimension2), operator = "AND")

# Metric filters
metric1 <- met_filter("users", "GREATER_THAN", 1, not = FALSE)
metric2 <- met_filter("pageviews", "GREATER_THAN", 1, not = FALSE)

mfilters <- filter_clause_ga4(list(metric1,metric2),operator = "AND")


# Download the data and store them in a dataframe
# Last 100 days of data
ga_results_state <-google_analytics(ga_id,
                               date_range = c("100daysAgo", "yesterday"),
                               metrics = c("users","sessions"),
                               dimensions = c("country","region","deviceCategory", "userType"),
                               dim_filters = dfilter,
                               met_filters = mfilters,
                               anti_sample = FALSE,
                               max = -1)

#keep only mobile, return visitor
state_return_mobile <- ga_results_state %>%
  mutate(region = tolower(region))%>%
  filter(userType == "Returning Visitor" & deviceCategory == "mobile")
  
##Join state ploygon with coordinates and returning mobile users by state for plotting
#Get states polygon
states <- map_data("state")
state_map <- states %>%
  left_join(state_return_mobile, by = "region")  

saveRDS(state_map, file = "state_return_mobile_100days.RDS")
state_map <- readRDS("state_return_mobile_100days.RDS")



#////////////////////////////////////////
## State and City Aggregate visitors ----
#////////////////////////////////////////
# Download the data and store them in a dataframe
# Last 100 days of data
# #Adding city here--with state agg above it showed no visits in Alabama
ga_results_city <-google_analytics(ga_id,
                                   date_range = c("100daysAgo", "yesterday"),
                                   metrics = c("users","sessions"),
                                   dimensions = c("country","region","deviceCategory", "city", "userType"),
                                   dim_filters = dfilter,
                                   met_filters = mfilters,
                                   anti_sample = FALSE,
                                   max = -1)


## create column that abbreviates city name; manual change 'DC' as it does not abbreviate
ga_results_city$state_abrv <- state.abb[match(ga_results_city$region, state.name)]
ga_results_city$state_abrv[is.na(ga_results_city$state_abrv)] <- "DC"

#keep only mobile, returning visitors (not hawaii)
city_return_mobile <- ga_results_city %>%
  mutate(name = paste(city, state_abrv, sep = " "))%>%
  filter(userType == "Returning Visitor" & deviceCategory == "mobile" & region != "Hawaii")

#get lat/lon coordiates for city name and exclude those outside cooridate range
city_return_mobile <- covstat_geocode(city_return_mobile, city_return_mobile$name)
city_return_mobile <- city_return_mobile %>%
  filter(lon < 0)

## save/load data file for mobile, city data
saveRDS(city_return_mobile, file = "city_return_mobile_100days.RDS")
city_return_mobile <- readRDS("city_return_mobile_100days.RDS")

### Create plot with state and city returning visitors using mobile
### last 100 days
state_city_plot <- ggplot(state_map, aes(long, lat, group = group))+
  geom_polygon(aes(fill = users), color = "black")+
  scale_fill_continuous(low = "lightblue", 
                        high = "darkblue",limits = c(0,3000))+
  geom_point(city_return_mobile, mapping=aes(x=lon, y=lat, size = users),  pch=21,
             colour = "black", fill = "red", alpha = 0.6, inherit.aes = FALSE) +
  labs(fill = "Visitors (state)", size = "Visitors (city)")+
  
  labs(title="Returning City Website Visitors Using Mobile Device",
       subtitle = "Last 100 Days") +
  theme_bw()+
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())

ggsave("state_city_mobile.jpg", state_city_plot, width = 950, height = 500, units = "px", dpi = 120)

### Top 10 cities
top10_cities <- city_return_mobile %>%
  #mutate(Place = paste(city, state, sep = ", "))%>%
  #filter(deviceCategory == "mobile" & userType == "New Visitor")%>%
  count(name, wt = users, name = "Users")%>%
  arrange(name,  desc(Users))%>%
  #group_by(userType)%>%
  slice_max(order_by = Users,  n = 10) %>%
  mutate(name = fct_reorder(name, Users)) %>%
  
  ggplot(aes(x=name,y=Users, 
             fill=factor(ifelse(name=="Covington KY", "Highlighted", "Normal"))))+
  geom_bar(stat = 'identity', color = "#7E7E7E")+
  scale_fill_manual(name = "name", values=c("#46b5d2", "#cccccc"))+
  
  coord_flip()+
  scale_y_continuous(label=scales::label_number_si(), limits = c(0, 2000))+
  geom_text(aes(label=scales::comma(Users, accuracy = 1.0)),  hjust = -0.2,  size = 3.5)+
  labs(title="Top 10 Cities for Returning Mobile Device Visitors")+
  theme_bw()+
  theme(legend.position = "none",
        legend.title = element_blank(), 
        axis.title = element_blank(),
        text = element_text(size = 12))

ggsave("top10cities.jpg", top10_cities, width = 700, height = 500, units = "px", dpi = 120)

##//////////////////////////////////////////////////////////
### Time and Device on page--top 3 cities for mobile returns
##//////////////////////////////////////////////////////////

ga_results_city_time <-google_analytics(ga_id,
                                        date_range = c("100daysAgo", "yesterday"),
                                        metrics = c("users", "sessions", "avgTimeOnPage", "avgSessionDuration"),
                                        dimensions = c("city", "region", "mobileDeviceBranding", "userType"),
                                        dim_filters = dfilter,
                                        met_filters = mfilters,
                                        anti_sample = FALSE,
                                        max = -1)

states <- c("New York", "Kentucky", "Ohio")
places <- c("New York", "Covington", "Cincinnati")

top3_mobile_return <- ga_results_city_time %>%
  filter(region %in% states & city %in% places & userType == "Returning Visitor" &
           mobileDeviceBranding != "(not set)" & avgSessionDuration < 500 & avgSessionDuration > 0) %>%
  group_by(city, mobileDeviceBranding)%>%
  mutate(med = median(avgSessionDuration))%>%
  count(mobileDeviceBranding, wt = users, name = "Users", sort = TRUE)

## save/load data file for mobile, city data
saveRDS(top3_mobile_return, file = "top3_mobile_return.RDS")
top3_mobile_return <- readRDS("top3_mobile_return.RDS")


top3_devicebrand <- ggballoonplot(top3_mobile_return, x = "city", y = "mobileDeviceBranding",
              size = "Users", fill = "Users")+
  scale_fill_viridis_c(option = "C", limits = c(0,1500), name = "Visitors")+
  guides(size = "none")+
  theme_bw() +
  labs(title="Mobile Device Brand of Returning Visitors")+
  theme(axis.title =element_blank(),
        text = element_text(size = 12))


ggsave("top3cities_devicebrand.jpg", top3_devicebrand, width = 700, height = 500, units = "px", dpi = 120)

 