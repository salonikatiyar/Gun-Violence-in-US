library(knitr)
library(dplyr)
library(readr)
library(ggplot2)
library(tibble)
library(stringr)
library(gridExtra)
library(scales)
library(lubridate)
library(ggrepel)
library(leaflet)
library(rgdal)
library(plotly)
library(tidyverse)
library(rio)
#Load data

gun <- read.csv("gun-violence-data_01-2013_03-2018.csv")

#Interting missing data --------------------------------------------------
#The Las Vegas mass shooting had to be removed because information about the incident was stored in a PDF
lasvegas <- list(999999, '2017-10-01', 'Nevada', 'Las Vegas', 'Mandalay Bay 3950 Blvd S', 59, 411, 'https://en.wikipedia.org/wiki/2017_Las_Vegas_shooting', 'https://en.wikipedia.org/wiki/2017_Las_Vegas_shooting', NA, NA, NA, NA, NA, 36.095, 'Mandalay Bay Hotel', -115.171667, 47, 'Route 91 Harvest Festiva; concert, open fire from 32nd floor. 47 guns seized; TOTAL:59 kill, 441 inj, number shot TBD,girlfriend Marilou Danley POI', NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
gun <- rbind(gun, lasvegas)
# Data size and structure
glimpse(gun)


# Comparing number of incidents by year, quarter, month, and weekd --------

#reformate the Date variable
gun$date <- ymd(gun$date)
str(gun$date)
summary(gun$date)

#number of daily gun violence incidents ----------------------------------
bydate <- gun %>% group_by(date) %>% summarize(n = n())
bydate$date <- as.Date(bydate$date)
ggplot(bydate, aes(x = date, y = n)) + geom_line() + theme_bw()

# By year -----------------------------------------------------------------

gun$year <- year(gun$date) #extract year from date using the lubridate year() function

gun %>%
       ggplot(aes(x=as.factor(year))) + geom_bar(stat='count', fill='purple3') +
       scale_y_continuous(labels=comma) +
       geom_label(stat = "count", aes(label = ..count.., y = ..count..))+theme_light()

#Seasonal Pattern-----------------------------------------------------------
#Looking at the points there seems to be some periodicity in the timing of events. Lumping the points together into months, we can see if there are seasonal trends in shooting incidents. 
#It certainly looks like we see spikes in violence early in the summer, and an apparent spike at the new year.
bydate %>% mutate(month = format(date, "%m")) %>% group_by(month) %>% ggplot(aes(x = factor(month), y = n)) + geom_boxplot() + theme_bw()

#Adding injuries, it seems that the seasonal pattern holds.
#bydate <- gun %>% group_by(date) %>% summarize(n = n(), killed = sum(n_killed), injured = sum(n_injured)) %>% gather(key = "type", value = "num", killed, injured)
#ggplot(bydate, aes(x = month, y = num, fill = type)) + geom_boxplot() + theme_bw()
bydate <- gun %>% group_by(date) %>% summarize(n = n(), killed = sum(n_killed), injured = sum(n_injured))
bydate$date <- as.Date(bydate$date)
bydate <- bydate %>% mutate(month = format(date, "%m"))

a<-ggplot(bydate, aes(x = month, y = injured,fill = type)) + geom_boxplot() + theme_bw()
b<-ggplot(bydate, aes(x = month, y = killed,fill = type)) + geom_boxplot() + theme_bw()
subplot(a,b)
# By quarter --------------------------------------------------------------

gun$quarter <- quarter(gun$date) #extract Quarters from date

q1 <- gun %>% filter(year=+2013) %>% select(year, quarter) %>% group_by(year) %>% count(quarter) %>%
  ggplot(aes(x=as.factor(quarter), y=n)) + geom_bar(stat='identity', fill='purple3') +
  scale_y_continuous(labels=comma) + facet_grid(.~year) + labs(x='Quarter', y='Number of incidents')+theme_light()

q2 <- gun %>% filter(quarter==1) %>% select(year, quarter) %>%
  group_by(year) %>% count(quarter) %>%
  ggplot(aes(x=as.factor(year), y=n)) + geom_bar(stat='identity', fill='lightpink4') +
  scale_y_continuous(labels=comma) + labs(x='Incidents in Q1 of each year', y='Number of incidents')+theme_light()
q3 <- gun %>% filter( quarter==2) %>% select(year, quarter) %>%
  group_by(year) %>% count(quarter) %>%
  ggplot(aes(x=as.factor(year), y=n)) + geom_bar(stat='identity', fill='lightpink4') +
  scale_y_continuous(labels=comma) + labs(x='Incidents in Q2 of each year', y='Number of incidents')+theme_light()
q4 <- gun %>% filter( quarter==3) %>% select(year, quarter) %>%
  group_by(year) %>% count(quarter) %>%
  ggplot(aes(x=as.factor(year), y=n)) + geom_bar(stat='identity', fill='lightpink4') +
  scale_y_continuous(labels=comma) + labs(x='Incidents in Q3 of each year', y='Number of incidents')+theme_light()
q5 <- gun %>% filter(quarter==4) %>% select(year, quarter) %>%
  group_by(year) %>% count(quarter) %>%
  ggplot(aes(x=as.factor(year), y=n)) + geom_bar(stat='identity', fill='lightpink4') +
  scale_y_continuous(labels=comma) + labs(x='Incidents in Q4 of each year', y='Number of incidents')+theme_light()

q1
grid.arrange(q2,q3,q4,q5)


# By month ----------------------------------------------------------------
gun$month <- month(gun$date, label=TRUE)

#Dates with most incidents
gun$day <- day(gun$date)
gun <- gun %>% mutate(date2=paste(month, day))
kable(gun %>% filter(year!=c(2013, 2018)) %>% count(date2) %>% top_n(10) %>% arrange(desc(n)) %>% rename(date=date2, "total number of incidents"=n))

#only taking the complete years 2014-2017
gun %>% filter(year!=c(2013, 2018)) %>% count(month) %>%
                   ggplot(aes(x=month, y=n)) + geom_bar(stat='identity', fill='olivedrab3')+
                   scale_y_continuous(labels=comma) +
                   labs(x='Month', y='Number of incidents', title='Incidents by Month')+theme_light()

# By weekday --------------------------------------------------------------

gun$weekday <- wday(gun$date, label=TRUE)

gun %>% count(weekday) %>%
  ggplot(aes(x=weekday, y=n)) + geom_bar(stat='identity', fill='olivedrab3') +
  scale_y_continuous(labels=comma) +
  labs(x='Weekday', y='Number of incidents', title='Incidents by Weekday')+theme_light()


# Comparing number of incidents and victims by location -------------------

# Incidents by State ------------------------------------------------------
#to convert State and city_or_county into a factor variable
gun[, c('state', 'city_or_county')] <- lapply(gun[, c('state', 'city_or_county')], as.factor)
str(gun$state)
str(gun$city_or_county)
#plot
plotly::ggplotly(gun %>% count(state) %>%
                   ggplot(aes(x=reorder(state, n), y=n, fill=n, text=state)) +
                   geom_bar(stat='identity', fill='firebrick4') + coord_flip() +
                   labs(x='', y='Number of incidents'),
                 tooltip=c("text", "y"))
#Incidents relative to the State population size----------------------------

statesPop <- read.csv("PopulationUS.csv")
statesPop <- statesPop %>% select(NAME, POPESTIMATE2017)
statesPop <- statesPop %>% filter(!NAME %in% c("United States", "Puerto Rico Commonwealth"))
statesPop <- statesPop %>% rename(state= NAME)
statesPop$state <- as.factor(statesPop$state)

#number of incidents relative to the population of each state

incidentsByState <- gun %>% group_by(state) %>% summarize(stateIncidents=n())
incidentsByState <-left_join(incidentsByState, statesPop, by="state")
incidentsByState$Per100000 <- round((incidentsByState$stateIncidents/incidentsByState$POPESTIMATE2017)*100000)
kable(head(incidentsByState))

#Red means a high danger level in terms of relative numbers of incidents, and green means that a state is relatively safe
plotly::ggplotly(incidentsByState%>% filter(state!="District of Columbia") %>%
                   ggplot(aes(x=reorder(state, Per100000), y=Per100000, fill=Per100000, text=state)) +
                   geom_bar(stat='identity') + coord_flip() +
                   labs(x='', y='Incidents per 100,000 inhabitants') + scale_fill_gradient(low="green", high="red") +
                   theme(legend.position="none"),
                 tooltip=c("text", "y"))

#interactive map of incident by state with Leaflet---------------------------

download.file("http://www2.census.gov/geo/tiger/GENZ2017/shp/cb_2017_us_state_500k.zip" , destfile="C:/Users/SALONI/Desktop/cb_2017_us_state_500k.zip")
states <- readOGR(dsn =path.expand("C:/Users/SALONI/Desktop/cb_2017_us_state_500k") , layer = "cb_2017_us_state_500k", encoding = "UTF-8")
class(states)
#contains in a dataframe
kable(head(states@data))
#joining the Per100000 column
addPer100k <- data.frame(id=states$GEOID, name=states$NAME)
addPer100k <- left_join(addPer100k, incidentsByState %>% select(state, Per100000) %>% rename(name=state), by="name")
addPer100k$Per100000[is.na(addPer100k$Per100000)] <- 0
states$per100k <- addPer100k$Per100000
#create the map
bins <- c(0, 50, 75, 100, 150, Inf)
pal <- colorBin("viridis", domain = states$per100k, bins = bins)

state_popup <- paste0("<strong>State: </strong>", 
                      states$NAME, 
                      "<br><strong>Incidents per 100,000 inhabitants </strong>", 
                      states$per100k) %>% lapply(htmltools::HTML)

leaflet(data = states) %>%
  setView(lng=-96, lat=37.8, zoom=4) %>%
  addProviderTiles("MapBox", options = providerTileOptions(id = "mapbox.light",
                                                           accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'))) %>%
  addPolygons(
    fillColor = ~pal(per100k),
    weight = 2,
    opacity = 1,
    color = "white",
    dashArray = "3",
    fillOpacity = 0.7,
    highlight = highlightOptions(
      weight = 5,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.7,
      bringToFront = TRUE),
    label = state_popup,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto")) %>%
  addLegend(pal = pal, values = ~per100k, opacity = 0.7, title = "Incidents", position = "bottomright")

#Victims by State-------------------------------------------------------
gun$victims <- gun$n_killed + gun$n_injured
VictimsByState <- gun %>% group_by(state) %>% summarize(sumVic=sum(victims), sumInj=sum(n_injured), sumDeath=sum(n_killed), PercDeath=round(sumDeath/sumVic,2), sumIncidents=n(), vicPerInc=round(sumVic/sumIncidents,2))
head(VictimsByState)

VictimsByState %>% filter(vicPerInc>0.8) %>%
  ggplot(aes(x=reorder(state, -vicPerInc), y=vicPerInc))+
  geom_point(size=4,color="orange")+
  geom_segment(aes(x=reorder(state, -vicPerInc),xend=reorder(state, -vicPerInc),y=0,yend=vicPerInc))+
  coord_flip()+
  xlab("States")+ylab("Percent")

#Victims relative to the state population sizes
VictimsByState <-left_join(VictimsByState, statesPop, by="state")
VictimsByState$Per100000 <- round((VictimsByState$sumVic/VictimsByState$POPESTIMATE2017)*100000)


plotly::ggplotly(VictimsByState%>% filter(state!="District of Columbia") %>%
                   ggplot(aes(x=reorder(state, Per100000), y=Per100000, fill=Per100000, text=state)) +
                   geom_bar(stat='identity') + coord_flip() +
                   labs(x='', y='Victims per 100,000 inhabitants') + scale_fill_gradient(low="yellow", high="red") +
                   theme(legend.position="none"),
                 tooltip=c("text", "y"))

#An interactive map of victims by state with Leaflet----------------------
addPer100k <- addPer100k %>% select(id, name)
addPer100k <- left_join(addPer100k, VictimsByState %>% select(state, Per100000) %>% rename(name=state), by="name")
addPer100k$Per100000[is.na(addPer100k$Per100000)] <- 0
states$per100k <- addPer100k$Per100000

bins <- c(0, 25, 50, 75, 100, Inf)
pal <- colorBin("YlOrRd", domain = states$per100k, bins = bins)

state_popup <- paste0("<strong>State: </strong>", 
                      states$NAME, 
                      "<br><strong>Victims per 100,000 inhabitants </strong>", 
                      states$per100k) %>% lapply(htmltools::HTML)

leaflet(data = states) %>%
  setView(lng=-96, lat=37.8, zoom=4) %>%
  addProviderTiles("MapBox", options = providerTileOptions(id = "mapbox.light",
                                                           accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'))) %>%
  addPolygons(
    fillColor = ~pal(per100k),
    weight = 2,
    opacity = 1,
    color = "white",
    dashArray = "3",
    fillOpacity = 0.7,
    highlight = highlightOptions(
      weight = 5,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.7,
      bringToFront = TRUE),
    label = state_popup,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto")) %>%
  addLegend(pal = pal, values = ~per100k, opacity = 0.7, title = "Victims", position = "bottomright")

#Incidents with highest numbers of victims------------------------------
TopMap <- Top10 %>% select(latitude, longitude, Victims, City, Location)

labels <- paste0("<strong>City: </strong>", TopMap$City, 
                 "<br><strong>Location: </strong>", TopMap$Location,
                 "<br><strong>Victims </strong>", TopMap$Victims) %>% lapply(htmltools::HTML)

leaflet(TopMap) %>%
  setView(lng=-96, lat=37.8, zoom=4) %>%
  addTiles() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addCircleMarkers(~longitude, ~latitude, color = "red", radius=~sqrt(Victims), label = labels)

# Incidents by city-------------------------
incidentsByCity <- gun %>% select(city_or_county, state) %>% rename(city=city_or_county) %>% group_by(city, state) %>% summarize(cityIncidents=n())
incidentsByCity[(incidentsByCity$city %in% c('Brooklyn', 'Bronx', 'Queens', 'Staten Island','New York (Manhattan)')) & incidentsByCity$state=='New York',]
sumNewYork <- sum(incidentsByCity$cityIncidents[(incidentsByCity$city %in% c('Brooklyn', 'Bronx', 'Queens', 'Staten Island','New York (Manhattan)')) & incidentsByCity$state=='New York'])

NewYork <- data.frame(city='New York', state='New York', cityIncidents=sumNewYork)
incidentsByCity <- as.tibble(rbind(as.data.frame(incidentsByCity), NewYork))
incidentsByCity %>% top_n(20, wt=cityIncidents) %>%
  ggplot(aes(x=reorder(city, cityIncidents), y=cityIncidents)) + geom_bar(stat='identity', fill='orange') +
  labs(x='City', y='Number of incidents') + coord_flip()
# Comparing the main incident categories by city-----------
#creating a function to create plots by city-------------
cityCats <- function(cityName){
  IncCharac %>% filter(city_or_county==cityName & incident_characteristics %in% overallCats) %>%
    count(incident_characteristics) %>%
    ggplot(aes(x=reorder(incident_characteristics, n), y=n/sum(n), fill=factor(incident_characteristics))) +
    geom_bar(stat='identity', width = 0.5) + scale_fill_manual(values = coloursShot) +
    theme(legend.position="none") + coord_flip(ylim = c(0, 0.8)) + labs(x="", y=cityName) +
    scale_y_continuous(labels=percent)
}

usOverallCats <- usCats(0.8)
baltimoreCats <- cityCats('Baltimore')
washingtonCats <- cityCats('Washington')
chicagoCats <- cityCats('Chicago')

grid.arrange(usOverallCats, baltimoreCats, washingtonCats, chicagoCats, ncol=1)

#Incident categories in the US----------------------------------------
head(gun$incident_characteristics,4)
gun$incident_characteristics <- gsub("\\|\\|", "|", gun$incident_characteristics)
IncCharac <- splitstackshape::cSplit(gun %>% select(incident_id, state, city_or_county, incident_characteristics), 'incident_characteristics', sep =  '|', direction="long")
numCat <- round(nrow(IncCharac)/nrow(gun),1)
cat('On average, there are', numCat, 'incident categories specified per incident')
kable(head(IncCharac,8))
InCharac %>% count(incident_characteristics) %>% top_n(30, wt=n) %>%
  ggplot(aes(x=reorder(incident_characteristics, n), y=n)) +
  geom_bar(stat='identity', fill='violetred4') +
  coord_flip() + labs(x='Incident Category', y='number of incidents')

#Comparing the main incident categories by state------------------------
coloursShot <- c("Shot - Wounded/Injured"="orange", "Shot - Dead (murder, accidental, suicide)"="red", "Non-Shooting Incident"="green", "Shots Fired - No Injuries"="yellow")

#creating a function to vary the x-axis scale (next plot uses same graph with diferent scale)
usCats <- function(fixedX=0.5){
  IncCharac %>% filter(incident_characteristics %in% overallCats) %>%
    count(incident_characteristics) %>%
    ggplot(aes(x=reorder(incident_characteristics, n), y=n/sum(n), fill=factor(incident_characteristics))) +
    geom_bar(stat='identity', width = 0.5) + scale_fill_manual(values = coloursShot) +
    theme(legend.position="none") + coord_flip(ylim = c(0, fixedX)) + labs(x="", y='US overall') +
    scale_y_continuous(labels=percent)
}

#creating a function to create plots by state
stateCats <- function(stateName){
  IncCharac %>% filter(state==stateName & incident_characteristics %in% overallCats) %>%
    count(incident_characteristics) %>%
    ggplot(aes(x=reorder(incident_characteristics, n), y=n/sum(n), fill=factor(incident_characteristics))) +
    geom_bar(stat='identity', width = 0.5)+ scale_fill_manual(values = coloursShot) +
    theme(legend.position="none") + coord_flip(ylim = c(0, 0.5)) + labs(x="", y=stateName) +
    scale_y_continuous(labels=percent)
}
overallCats <- c("Shot - Wounded/Injured", "Shot - Dead (murder, accidental, suicide)", "Non-Shooting Incident", "Shots Fired - No Injuries")
TableOverallCats <- IncCharac %>% filter(incident_characteristics %in% overallCats) %>% count(incident_characteristics)
cat('For', round((sum(TableOverallCats$n)/nrow(gun))*100), 'percent of incidents, an overall category is specified')

usOverallCats <- usCats()
alaskaCats <- stateCats('Alaska')
delawareCats <- stateCats('Delaware')
louisianaCats <- stateCats('Louisiana')

grid.arrange(usOverallCats, alaskaCats, delawareCats, louisianaCats, ncol=1)

# Terrorism, gang involvement, and drug involvement--------------------- 

CatTable <- IncCharac %>% count(incident_characteristics)
kable(CatTable[1:10,])
Involvement <- c("Terrorism Involvement", "Drug involvement", "Gang involvement")
kable(CatTable %>% filter(incident_characteristics %in% Involvement))
Map_1<-IncCharac %>% filter(incident_characteristics=="Terrorism Involvement")
Map_2<-IncCharac %>% filter(incident_characteristics=="Drug involvement")
Map_3<-IncCharac %>% filter(incident_characteristics=="Gang involvement")
Map_4=rbind(Map_1,Map_2)
Mapping=rbind(Map_4,Map_3)
Mapping<- left_join(Mapping, gun %>% select(incident_id, longitude, latitude,victims,location_description), by="incident_id")
Mapping$Characteristics<-0
Mapping$Characteristics[Mapping$incident_characteristics == "Terrorism Involvement"] <- 1
Mapping$Characteristics[Mapping$incident_characteristics == "Drug involvement"] <- 2
Mapping$Characteristics[Mapping$incident_characteristics == "Gang involment"] <- 3
pal <- colorFactor(c("green", "red","navy"), domain = c("1", "2","3"))
mytext <- paste(
  "Incident: ", Mapping$Characteristics, "<br/>", 
  "Victims: ", Mapping$victims, "<br/>", 
  "State:", Mapping$state,"<br/>",
  "City:", Mapping$city_or_county,sep="") %>%
  lapply(htmltools::HTML)

# Final Map
leaflet(Mapping) %>% 
  setView( lat=41.9, lng=-96 , zoom=4) %>%
  addTiles()  %>% 
  addProviderTiles("CartoDB.Positron") %>%
  addCircleMarkers(~longitude, ~latitude,
                   fillColor = ~pal(Characteristics) , fillOpacity = 0.8 , color="white", radius=~sqrt(victims), stroke=FALSE,
                   label = mytext) 

#Exporting dataframe---------------------------
export(VictimsByState, "victims.rds")
export(gun, "gun.rds")
export(addPer100k, "add.rds")
export(Mapping, "Mapping.rds")
export(IncCharac, "Inchar.rds")
export(bydate, "bydate.rds")

