############## 

#Data Location 
###http://www.reefbase.org/gis_maps/datasets.aspx

##################################



#packages<- c("readxl","sqldf","ggplot2","plotly","shiny","ggthemes","leaflet")
#install.packages(packages)

library(readxl)
library(sqldf)
library(ggplot2)
library(ggthemes)
library(shiny)
library(plotly)
library("leaflet")


#######################################################################################

#Data Cleaning

Coral_data <- read_excel("CoralBleaching.xlsx")
corals <- as.data.frame(Coral_data)
colnames(corals)[colnames(corals)=="LAT"] <-'latitude'
colnames(corals)[colnames(corals)=="LON"] <-'longitude'


corals$PERCENTAGE_AFFECTED <- as.numeric(gsub("\\$", "", corals$PERCENTAGE_AFFECTED))


corals$PERCENTAGE_AFFECTED[is.na(corals$PERCENTAGE_AFFECTED)] <- 0
corals$CORAL_SPECIES[is.na(corals$CORAL_SPECIES)] <-'Unknown'

corals <- sqldf("select * from corals where latitude is not null ANd longitude is not null")

corals_severity<- sqldf("
                        select 
                        COUNTRY_CODE, COUNTRY,CAST(YEAR AS CHAR) YEAR,count(*) as corals
                        from 
                        corals 
                        group by COUNTRY_CODE,COUNTRY,YEAR
                        order by COUNTRY,YEAR DESC
                        ")

coral_plot <- sqldf("
                        select 
                        COUNTRY,LOCATION,latitude,longitude
                        ,CORAL_SPECIES
                        ,ROUND(AVG(PERCENTAGE_AFFECTED),0) AS PERCENTAGE_AFFECTED
                        from 
                        corals 
                        group by 
                        COUNTRY,LOCATION,latitude,longitude,CORAL_SPECIES
                        
                        ")
coral_plot$PERCENTAGE_AFFECTED <- paste(coral_plot$PERCENTAGE_AFFECTED,"%", sep = "") 


Colors_DF = data.frame(color = topo.colors(50, alpha = NULL), stringsAsFactors = FALSE)


#######################################################################################

#Shiny App

ui <- fluidPage(
  
  # Application title
  titlePanel("Coral Reef Analysis"),
  
  sidebarPanel(
    h3("Corals Bleaching and Location"),
    # Select Justices name here
    selectizeInput("name",
                   label = "Country Name(s) of Interest",
                   choices = unique(corals_severity$COUNTRY),
                   multiple = F,
                   options = list(placeholder = 'Select a Country'),
                   selected = "Australia"),
    # Term plot
    plotOutput("termPlot", height = 200),
    helpText("You can select maximum of One country to understand the bleaching status of corals 
             in selected Country
             ")
  ),
  
  # Show a plot of the generated distribution
  mainPanel(
    plotlyOutput("trendPlot"),
    leafletOutput("CoralsLocation")
  )
)



server <-function(input, output, session) {
  
  output$trendPlot <- renderPlotly({
    
    if (length(input$name) == 0) {
      print("Please select at least one country")
    } else {
      df_trend <- corals_severity[corals_severity$COUNTRY == input$name, ]
      ggplot(df_trend) +
        geom_line(aes(x = YEAR, y = corals, by = COUNTRY, color = COUNTRY)) +
        labs(x = "YEAR", y = "corals Count", title = "Total corals with High/Medium bleeching") +
        scale_colour_hue("clarity", l = 70, c = 150) + ggthemes::theme_few()
    }
    
  })
  
  output$CoralsLocation <- renderLeaflet({
    
    if (length(input$name) == 0) {
      print("Please select at least one country")
    } else {
      coral_plot <- coral_plot[coral_plot$COUNTRY == input$name, ]
    
    leaflet(coral_plot)%>% 
      addTiles()%>%
      addMarkers(
        lng = ~longitude,
        lat = ~latitude,
        popup = paste("Location",coral_plot$LOCATION , "<br>",
                      "Coral Species", coral_plot$CORAL_SPECIES, "<br>",
                      "Percentage Affected:", coral_plot$PERCENTAGE_AFFECTED)
        
          )
       
    }
    
  })
}

shinyApp(ui, server)

