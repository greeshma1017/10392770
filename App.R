
 #packages<- c("readxl","sqldf","ggplot2","plotly","shiny","ggthemes","leaflet","shinydashboard","shinycustomloader")
#install.packages(packages)
library(readxl)
library(sqldf)
library(ggplot2)
library(ggthemes)
library(shiny)
library(plotly)
library(leaflet)
library(shinydashboard)
library(shinycustomloader)
#setting dashboard theme
theme_set(theme_classic())

#######################################################################################

#Data Cleaning

Coral_data <- read_excel("CoralBleaching.xlsx")
corals <- as.data.frame(Coral_data)
colnames(corals)[colnames(corals)=="LAT"] <-'latitude'
colnames(corals)[colnames(corals)=="LON"] <-'longitude'


corals$PERCENTAGE_AFFECTED <- as.numeric(gsub("\\$", "", corals$PERCENTAGE_AFFECTED))


corals$PERCENTAGE_AFFECTED[is.na(corals$PERCENTAGE_AFFECTED)] <- 0
corals$CORAL_SPECIES[is.na(corals$CORAL_SPECIES)] <-'Unknown'


corals <- sqldf("select * from corals where latitude is not null AND longitude is not null")

corals_severity<- sqldf("
                        select 
                        COUNTRY_CODE, COUNTRY,CAST(YEAR AS CHAR) YEAR,count(*) as corals
                        FROM 
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

CountryList = sort(unique(corals_severity$COUNTRY))

#############################################################################################################################

ui = dashboardPage( skin ="blue",
                    dashboardHeader(
                      title ="Coral Bleaching Analysis",
                      titleWidth =200
                    ), 
                    dashboardSidebar(disable = FALSE,width = 200,
                                     sidebarMenu(
                                       menuItem("Coral Bleaching and Locations",tabName = "menu1")
                                     )
                    ),
                    dashboardBody(
                      
                      #css
                      
                      tags$head(tags$style(HTML('
                                                .content{
                                                padding-top :3px;}
                                                
                                                .dashboardHeaderr .logo {
                                                font-family: "Georgia";
                                                font-weight: 800;
                                                line-height: 1.1;
                                                
                                                color: black;
                                                }
                                                
                                                }
                                                h3{
                                                
                                                font-weight: 800;
                                                font-family: "Georgia";
                                                line-height: 1.1;
                                                color: black;
                                                
                                                }
                                                
                                                .img-local{
                                                align :right;
                                                }
                                                
                                                '))),
                      #creating different tabs 
                      tabItems(
                        tabItem(tabName ="menu1",
                                fluidRow(
                                  
                                  column(width =3,
                                         box(title ="Select a Country", solidHeader = T, status ="primary",width = 100, 
                                             selectInput(inputId = "CountrySelected","Country",
                                                         multiple = F,
                                                         choices = CountryList,selected = "Australia")   
                                         )),
                                  column(width =9, 
                                         box(  width =100,
                                               withLoader(plotOutput("trendPlot"), type='image', loader='Spinner.gif')
                                         ),
                                         box( 
                                           width =200,
                                           withLoader( leafletOutput("CoralsLocation"), type='image', loader='Spinner.gif')
                                         )
                                         
                                  )
                                )
                                
                                
                                
                                
                        )
                        
                      )))






######################################################################################################################################

server<-function(input, output){
  
  output$trendPlot = renderPlot({
      
      if (length(input$CountrySelected) == 0) {
        print("Please select at least one country")
      } else {

        df_trend <- corals_severity[corals_severity$COUNTRY == input$CountrySelected, ]
        ggplot(df_trend) +
          geom_line(aes(x = YEAR, y = corals, by = COUNTRY, color = COUNTRY)) +
          labs(x = "YEAR", y = "corals Count", title = "Total corals with High/Medium bleeching") +
          scale_colour_hue("Country", l = 70, c = 150) 
      }
      
    })
    
    
  
  ######################################################################################################################################    
  output$CoralsLocation = renderLeaflet({
    
    if (length(input$CountrySelected) == 0) {
      print("Please select at least one country")
    } else {
      coral_plot <- coral_plot[coral_plot$COUNTRY == input$CountrySelected, ]
      
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
##############################################################################################################################################
# call shinyApp and launch it
shinyApp(ui=ui, server=server)
