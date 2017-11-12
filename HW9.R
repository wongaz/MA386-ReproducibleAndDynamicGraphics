library(nycflights13)
library(plotly)
library(dplyr)

flight.data = flights
flight.data = filter(flight.data, month == 1)
subset = flight.data[,c(2,3,6,10,13)]
subset$origin = as.factor(subset$origin)
subset$carrier = as.factor(subset$carrier)
subset %>% group_by(day, origin, carrier)%>% summarise()
MINUTES_CLASSIFIER <- function(arg){
  #More than 15
  #0 - True - Late
  #1 - False - On time
  if(arg>15){
    return(1)
  }else{
    return(0)
  }
}
subset$LateClassifier <- sapply(flight.data$,MINUTES_CLASSIFIER)
