library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(leaflet)
library(shinydashboard)
library(DT)
library(tigris)
library(scales)
# Loading required data----------------------------
guns <- readRDS("guns.rds")
gun<-readRDS("gun.rds")
new<- gun%>% select(year,quarter,month,day,date2,weekday)
guns<-cbind(guns,new)
states <- readRDS("states.rds")
mapping<-readRDS("mapping.rds")
victims<-readRDS("victims.rds")
bydate<-readRDS("bydate.rds")
add<-readRDS("add.rds")
Inchar<- readRDS("Inchar.rds")
sumVic<- gun$victims
guns<-cbind(guns,sumVic)

#Defining components of dashboard--------------------

header <- dashboardHeader(title = "Gun voilence in the US", titleWidth=500)

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem(h4("Dashboard"), tabName = "dashboard"), menuItem(h4("Report"),href = "https://drive.google.com/file/d/1rKBzNuoATvCg_MafykjKGC075Mrn32D2/view?usp=sharing", newtab = TRUE),
    menuItem(h4("Presentation"),href = "https://drive.google.com/file/d/1IBQWCea1BJw6haTQVPJZ3dfrYaGjb7gF/view?usp=sharing", newtab = TRUE),
    hr()
))

body <- dashboardBody( 
  
  tabItems(
    tabItem(tabName= "dashboard",
            fluidRow(
              tabBox(id = "Saloni Katiyar", width=12, height=700,
                     
                     tabPanel("Exploration I",
                              br(),
                              h4("Comparing number of incidents by year and quarter"),
                              fluidRow(plotlyOutput("plot10")),
                              fluidRow(plotlyOutput("plot11"))),
                     tabPanel("Exploration II",
                              br(),
                              h4("Comparing number of incidents by month, date and day"),
                              fluidRow(plotlyOutput("plot12")),
                              fluidRow(plotlyOutput("plot14")),
                              fluidRow(plotlyOutput("plot13"))),
                     tabPanel("Seanonality",
                              br(),
                              h3("What information do we get from the plot below?",align="left"),
                              h4("Looking at the points there seems to be some periodicity in the timing of events. Lumping the points together into months, we can see if there are seasonal trends in shooting incidents.It certainly looks like we see spikes in violence early in the summer, and an apparent spike at the new year.There is a seasonal pattern",align="left"),
                              fluidRow(plotlyOutput("plot8")),
                              fluidRow(plotlyOutput("plot9"))),
                     tabPanel("Incident",
                              br(),
                              h4("Difference in ranking of the state on the basis of number of incidents can be seen after taking population count into account.", align="left"),
                              sliderInput(inputId = "num",
                                          label = "Choose Top N states with most incidents",
                                          value = 10,
                                          min = 1,
                                          max = (n_distinct(guns$state)-1)),
                              fluidRow(plotlyOutput("plot1")),
                              fluidRow(plotlyOutput("plot2"))),
                     tabPanel("Incident Map",
                              br(),
                              fluidRow(h4("Interactive map with Leaflet", align='left')),
                              br(),
                              fluidRow(leafletOutput("usmap", height=700))),
                     tabPanel("Victim",
                              br(),
                              h4("Difference in ranking of the state on the basis of number of victims can be seen after taking population count into account.", align="left"),
                              sliderInput(inputId = "num1",
                                          label = "Choose Top N states with most incidents",
                                          value = 10,
                                          min = 1,
                                          max = (n_distinct(victims$sumVic)-1)),
                              fluidRow(plotlyOutput("plot4")),
                              fluidRow(plotlyOutput("plot3"))),
                     tabPanel("Victim Map",
                              br(),
                              fluidRow(h4("Interactive map with Leaflet", align='left')),
                              br(),
                              
                              fluidRow(leafletOutput("usmap_1", height=700))),
                   
                     tabPanel("Characteristics",
                              br(),
                              h4("Top 30 crimes related to gun violence"),
                              fluidRow(plotlyOutput("plot5"))),
                     tabPanel("Comparision",
                              br(),
                              h4(""),fluidRow(plotlyOutput("plot6")),fluidRow(plotlyOutput("plot7"))),
                   
                      tabPanel("Data Set",
                              br(),
                              fluidRow(h4("The default sort of this table is by descending number of incidents per 100,000 inhabitants in the selected period (column Per100k).", align='left')),
                              br(),
                              fluidRow(DT::dataTableOutput(outputId="table"))))
              
            )
    )
  )
)

# ui--------------------
ui <- dashboardPage(header, sidebar, body, skin="green")

#server------------------
server <- function(input, output) {
  
  gun_select <- reactive({guns %>% filter(state !="District of Columbia") %>%
      group_by(state, population) %>% summarise(incidents = n()) %>% ungroup() %>%
      mutate(Per100k = round((incidents/population)*100000))})
  gun_select1<- reactive({guns %>% filter(state !="District of Columbia") %>%
      group_by(state, population) %>% summarise(sum=sum(sumVic)) %>% ungroup() %>%
      mutate(Per= round((sum/population)*100000))})
  
# Embedding Plots----------  
  output$plot1<-renderPlotly({
    ggplotly(gun_select() %>% top_n(input$num, wt=incidents) %>%
               ggplot(aes(x=reorder(state, incidents), y=incidents, text=state)) +
               geom_bar(stat='identity', fill='olivedrab3') + coord_flip() +
               labs(x='', y='Number of incidents', title="Absolute number of incidents"),
             tooltip=c("text", "y"))
  })
  
  output$plot2<-renderPlotly({
    ggplotly(gun_select() %>% top_n(input$num, wt=Per100k) %>%
               ggplot(aes(x=reorder(state, Per100k), y=Per100k, fill= Per100k, text=state)) +
               geom_bar(stat='identity') + coord_flip() +
               labs(x='', y='Incidents per 100k inhabitants', title="Relative number of incidents") +
               scale_fill_gradient(low="yellow", high="darkgreen") +
               theme(legend.position="none"),
             tooltip=c("text", "y"))
  })
  
  output$plot3<-renderPlotly({
    ggplotly(victims%>% top_n(input$num1, wt=Per100000)%>%filter(state!="District of Columbia") %>%
                       ggplot(aes(x=reorder(state, Per100000), y=Per100000, fill=Per100000, text=state)) +
                       geom_bar(stat='identity') + coord_flip() +
                       labs(x='', y='Victims per 100,000 inhabitants', title = "Relative number of incidents") + scale_fill_gradient(low="yellow", high="darkgreen") +
                       theme(legend.position="none"),
                     tooltip=c("text", "y"))
    
  })
 
  output$plot4<-renderPlotly({
    ggplotly(victims %>% top_n(input$num1, wt=vicPerInc)%>%
      ggplot(aes(x=reorder(state, -vicPerInc), y=vicPerInc)) + geom_bar(stat='identity', fill='olivedrab3') +
      labs(x='State', y='Victims per incidents')+coord_flip()+
              labs(x='States',y='Percent', title="Absolute number of victims")+
              theme(legend.position="none"))
    
  })
  
  output$plot5 <- renderPlotly({Inchar %>% count(incident_characteristics) %>% top_n(30, wt=n) %>%
      ggplot(aes(x=reorder(incident_characteristics, n), y=n)) +
      geom_bar(stat='identity',fill="darkgreen")+
      coord_flip() + labs(x='Incident Category', y='number of incidents', title = "Crime characteristics")
  })
  
  output$plot6  <- renderPlotly({ Inchar %>% count(incident_characteristics) %>%
      ggplot(aes(x=reorder(incident_characteristics, n), y=n/sum(n), fill=factor(incident_characteristics))) +
      geom_bar(stat='identity', width = 0.5) + scale_fill_manual(values = c("lightpink4", "olivedrab3", "yellow4","maroon")) +
      theme(legend.position="none") + coord_flip(ylim = c(0, 0.8)) + labs(x="", y="cityName") +
      scale_y_continuous(labels=percent)
  
  usOverallCats <- usCats(0.8)
  baltimoreCats <- cityCats('Baltimore')
  washingtonCats <- cityCats('Washington')
  chicagoCats <- cityCats('Chicago')+ggtitle("City-Plot for US Overall, Baltimore, Washington, Chicago")
  subplot(usOverallCats, baltimoreCats, washingtonCats, chicagoCats, nrows = 4, shareX = TRUE, titleX = FALSE)
  
  })
  
  output$plot7 <- renderPlotly({ Inchar %>%
      count(incident_characteristics) %>%
      ggplot(aes(x=reorder(incident_characteristics, n), y=n/sum(n), fill=factor(incident_characteristics))) +
      geom_bar(stat='identity', width = 0.5)+ scale_fill_manual(values = c("lightpink4", "olivedrab3", "yellow4","maroon")) +
      theme(legend.position="none") + coord_flip(ylim = c(0, 0.5)) + labs(x="", y="state") +
      scale_y_continuous(labels=percent)
  overallCats <- c("Shot - Wounded/Injured", "Shot - Dead (murder, accidental, suicide)", "Non-Shooting Incident", "Shots Fired - No Injuries")
  cat('For', round((sum(TableOverallCats$n)/nrow(gun))*100), 'percent of incidents, an overall category is specified')
  
  usOverallCats <- usCats(0.8)
  alaskaCats <- stateCats('Alaska')
  delawareCats <- stateCats('Delaware')
  louisianaCats <- stateCats('Louisiana')+ggtitle("State-plot for US Overall, Alaska, Delaware, Louisiana")
  
  subplot(usOverallCats, alaskaCats, delawareCats, louisianaCats, nrows = 4, shareX = TRUE, titleX = FALSE)})
  
  output$plot8 <- renderPlotly({
  ggplot(bydate, aes(x = month, y = injured)) + geom_boxplot(color="olivedrab3") + theme_bw()+xlab("Months")+ylab("Count")+ggtitle("Trend-Injured ")
 
  
})
  
  output$plot9 <- renderPlotly({
    ggplot(bydate, aes(x = month, y = killed)) + geom_boxplot(color="yellow3") + theme_bw()+ggtitle("Trend-Killed")+xlab("Months")+ylab("Count")
    
    
  })
  output$plot10 <- renderPlotly({
    guns %>%
      ggplot(aes(x=as.factor(year))) + geom_bar(stat='count', fill='olivedrab3') +
      scale_y_continuous(labels=comma) +
      ggtitle("Incidents by Year")+xlab("Year")+ylab("Count")+theme_light()
    
    
  })
  output$plot11 <- renderPlotly({
    q1 <- gun %>% filter(year==2013) %>% select(year, quarter) %>% group_by(year) %>% count(quarter) %>%
      ggplot(aes(x=as.factor(quarter), y=n)) + geom_bar(stat='identity', fill='yellow3') +
      scale_y_continuous(labels=comma) + facet_grid(.~year) + labs(x='Quarter', y='Number of incidents')+theme_light()
    
    q2 <- gun %>% filter(quarter==1) %>% select(year, quarter) %>%
      group_by(year) %>% count(quarter) %>%
      ggplot(aes(x=as.factor(year), y=n)) + geom_bar(stat='identity', fill='yellow3') +
      scale_y_continuous(labels=comma) + labs(x='Incidents in Q1 of each year', y='Number of incidents')+theme_light()
    q3 <- gun %>% filter( quarter==2) %>% select(year, quarter) %>%
      group_by(year) %>% count(quarter) %>%
      ggplot(aes(x=as.factor(year), y=n)) + geom_bar(stat='identity', fill='yellow3') +
      scale_y_continuous(labels=comma) + labs(x='Incidents in Q2 of each year', y='Number of incidents')+theme_light()
    q4 <- gun %>% filter( quarter==3) %>% select(year, quarter) %>%
      group_by(year) %>% count(quarter) %>%
      ggplot(aes(x=as.factor(year), y=n)) + geom_bar(stat='identity', fill='yellow3') +
      scale_y_continuous(labels=comma) + labs(x='Incidents in Q3 of each year', y='Number of incidents')+theme_light()
    q5 <- gun %>% filter(quarter==4) %>% select(year, quarter) %>%
      group_by(year) %>% count(quarter) %>%
      ggplot(aes(x=as.factor(year), y=n)) + geom_bar(stat='identity', fill='yellow3') +
      scale_y_continuous(labels=comma) + labs(x='Incidents in Q4 of each year', y='Number of incidents')+theme_light()+ggtitle("Incidents by Quarter")
    
    subplot(q2,q3,q4,q5, nrows =2, margin = 0.05 )
    
    
  })
  output$plot14 <- renderPlotly({gun$day <- day(gun$date)
  gun <- gun %>% mutate(date2=paste(month, day))
  gun  %>% filter(year!=c(2013, 2018)) %>% count(date2)%>% top_n(10) %>% arrange(desc(n))%>%
    ggplot(aes(x=as.factor(date2), y=n)) + geom_bar(stat='identity', fill='olivedrab3') +
    scale_y_continuous(labels=comma) + labs(x='Dates', y='Number of incidents',title='Dates with most number of incidents')+theme_light()})
  
  output$plot12 <- renderPlotly({
    gun %>% filter(year!=c(2013, 2018)) %>% count(month) %>%
    ggplot(aes(x=month, y=n)) + geom_bar(stat='identity', fill='olivedrab3')+
    scale_y_continuous(labels=comma) +
    labs(x='Month', y='Number of incidents', title='Incidents by Month')+theme_light()})
  
  output$plot13 <- renderPlotly({
    gun %>% count(weekday) %>%
      ggplot(aes(x=weekday, y=n)) + geom_bar(stat='identity', fill='yellow3') +
      scale_y_continuous(labels=comma) +
      labs(x='Weekday', y='Number of incidents', title='Incidents by day')+theme_light()
  })
  
   output$table <- DT::renderDataTable(DT::datatable({gun_select() %>% arrange(desc(Per100k))}))
  
  states1 <- reactive({tigris::geo_join(states, gun_select(), "NAME", "state", how="inner")})
  state_popup <- reactive({paste0("<strong>State: </strong>",
                                  states1()$NAME,
                                  "<br><strong>Incidents per 100,000 inhabitants </strong>",
                                  states1()$Per100k) %>%
      lapply(htmltools::HTML)})
  
  pal <- reactive({colorNumeric("Greens", domain=states1()$Per100k)})
  
  output$usmap <- renderLeaflet({states1() %>%
      leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(-98.483330, 38.712046, zoom = 4) %>%
      addPolygons(data = states1(),
                  fillColor = ~pal()(states1()$Per100k),
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
                  label = state_popup(),
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto")) %>%
      addLegend(pal = pal(),
                values = states1()$Per100k,
                position = "bottomright",
                title = "On the basis of incident")})
 
  
  states2 <- reactive({tigris::geo_join(states, gun_select1(), "NAME", "state", how="inner")})
  state_popup2 <- reactive({paste0("<strong>State: </strong>",
                                  states2()$NAME,
                                  "<br><strong>Victims per 100,000 inhabitants </strong>",
                                  states2()$Per) %>%
      lapply(htmltools::HTML)})
  
  pal_1<- reactive({colorNumeric("Greens", domain=states2()$Per)})
  
  output$usmap_1 <- renderLeaflet({states2() %>%
      leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(-98.483330, 38.712046, zoom = 4) %>%
      addPolygons(data = states2(),
                  fillColor = ~pal_1()(states2()$Per),
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
                  label = state_popup2(),
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto")) %>%
                  addLegend(pal = pal(),
                    values = states2()$Per,
                    position = "bottomright",
                    title = "On the basis of victims")})
 
  
}
    
  
#}
# Rshiny App execution----------------
shinyApp(ui = ui, server = server)



