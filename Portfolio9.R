pkgs <- c("dplyr", "ggplot2", "readr", "stringr",
          "ggmap", "animation", "plotly")
for(pkg in pkgs) library(pkg, character.only=TRUE)

Starbucks <- read_csv("~/SchoolWork/MA386-StatisticalProgramming/Week9/StarBucksLong.csv")

us.map = ggplot() + borders("state")

## ----USMurder.States, eval=TRUE------------------------------------------
# Obtain Map Data
#  Contains drawing regions in terms of lat/long and state id's.
States.df <- map_data("state")
output <- Starbucks
#Starbucks <- cbind(geocode(as.character(Starbucks2Part1$City)), Starbucks2Part1)
dist = unique(Starbucks$Year)
for(v in dist){
  relativeData <-  filter(Starbucks, Year <= v)
  relativeData$Year = as.integer(v)
  output <- rbind(output,relativeData)
}

ORDERING_CLASSIFIER <- function(x, y){
  #ONTIME - Already ontime
  #Better Improving on lateness
  #WORSE - GETTING MORE LATE
  return (x+y)
}
#Starbucks$Value <- mapply(FUN = ORDERING_CLASSIFIER, Starbucks$Month, Starbucks$Year)
## ----USMurder.Plot, eval=TRUE, fig.cap="Murder rate per capita across the US in 1973."----
# Construct Graphic



Sys.setenv('MAPBOX_TOKEN' = 'pk.eyJ1IjoiYXdvbmcxNCIsImEiOiJjajlycHU3MWI2NmhtMnFwYWJ3ZnlpNjBwIn0.L5_66NIju2-XOOFN0vDR-g')
center.loc <- geocode("USA", output="latlon")
star.map <- output %>%
  plot_mapbox(lat = ~lat,
            lon = ~lon,
            frame = ~Year,
            mode = "markers",
            type = "scattergeo"
            )%>%
            layout(mapbox = list(zoom = 4,center = list(lat = center.loc$lat,
                                     lon = center.loc$lon)))
star.map

p<-plot_ly (
  type = "choropleth",
  locations =
    c( "AZ", "CA", "VT" ) ,
  locationmode = "USA-states" ,
  colorscale = "Viridis" , z = c( 10, 20, 40 )) %>%
  layout ( geo = list ( scope = "usa" ))
