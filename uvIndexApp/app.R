#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(lubridate)
library(rjson)
library(jsonlite)
library(RCurl)
library(shinythemes)

# Define UI for application that draws a histogram

ui <- fluidPage(theme = shinytheme("simplex"),
                shinyUI(navbarPage("UV Index App",
                                   tabPanel("UV Index - Zipcode",  
                                            textInput("zipinput", "Enter your zipcode", "Zipcode"),
                                            div(id = "header", titlePanel("Explore your local UV index")),
                                            plotOutput("zipplot")
                                   ),
                                   tabPanel("UV Index - City,State",
                                            textInput("cityinput", "Enter your city", "City"),
                                            textInput("stateinput", "Enter your state", "State"),
                                            div(id = "header", titlePanel("Explore your local UV index")),
                                            plotOutput("cityplot")),
                                   
                                   tabPanel("Skin Protection",
                                            sidebarPanel("Skincare tips to avoid UV damage"),
                                   ),
                                   tabPanel("Resources",
                                            h3("The Skin Cancer Foundation: https://www.skincancer.org/"), #can someone help me make these hyperlinks?
                                            h3("The National Council on Skin Cancer Prevention, https://skincancerprevention.org/")
                                   ),
                                   tabPanel("Credits")
                )
                ),
                

            

)


# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$value <- renderText({ input$caption })
    
    zipcode <- reactive(input$caption)
    # zipcode <- reactive({
    #     validate(
    #         need(is.character(input$caption) != TRUE, "Please input a zipcode")
    #     )
    # })

    output$zipplot <- renderPlot({
        
        uvrisk <- data.frame(name = c("Low", "Moderate", "High", "Very High"),
                             imin = c(0,3,6,8),
                             imax = c(3,6,8,11),
                             mycolor = c("A", "B", "C", "D")) %>%
            mutate(medy = imin + floor((imax-imin)/2))
        
        zipbase_url <- "https://enviro.epa.gov/enviro/efservice/getEnvirofactsUVHOURLY/ZIP/"
        zipfull_url <- paste0(zipbase_url, zipcode(), "/JSON")
        uvdf <- as.data.frame(fromJSON(readLines(zipfull_url)))
        
        uvdf <- uvdf %>%
            mutate(NEW_DATE = mdy_h(DATE_TIME))

        ggplot() +
            theme_light() +
            geom_rect(data = uvrisk, aes(xmin = c(uvdf$NEW_DATE[1], uvdf$NEW_DATE[1], uvdf$NEW_DATE[1], uvdf$NEW_DATE[1]), xmax = c(uvdf$NEW_DATE[21], uvdf$NEW_DATE[21], uvdf$NEW_DATE[21], uvdf$NEW_DATE[21]), ymin = imin, ymax = imax, fill = mycolor)) +
            geom_text(data = uvrisk, aes(x = uvdf$NEW_DATE[3], y = medy, label = name, size = 3))+
            geom_line(data = uvdf, aes(x = NEW_DATE, y = UV_VALUE)) +
            scale_y_continuous("UV Index", limit = c(-0.1, 11), breaks = c(0,1,2,3,4,5,6,7,8,9,10,11), expand = c(0, 0)) +
            ggtitle("UV Index in the Past Day") +
            theme(axis.title.x = element_blank(),
                  legend.position = "None")
        
    })

    
    city <- reactive(input$cityinput)
    state <- reactive(input$stateinput)
    
    output$cityplot <- renderPlot({
        
        citybase_url <- "https://enviro.epa.gov/enviro/efservice/getEnvirofactsUVHOURLY/CITY/"
        cityfull_url <- paste0(citybase_url, city(), "/STATE/", state(), "/JSON")
        uvdatacity <- fromJSON(readLines(cityfull_url))
        
        uvdatacity <- uvdatacity %>%
            mutate(NEW_DATE = mdy_h(DATE_TIME)) # get new time
        
        ggplot() +
            theme_light() +
            geom_rect(data = uvrisk, aes(xmin = c(uvdatacity$NEW_DATE[1], uvdatacity$NEW_DATE[1], uvdatacity$NEW_DATE[1], uvdatacity$NEW_DATE[1]), 
                                         xmax = c(uvdatacity$NEW_DATE[21], uvdatacity$NEW_DATE[21], uvdatacity$NEW_DATE[21], uvdatacity$NEW_DATE[21]), 
                                         ymin = imin, ymax = imax, fill = mycolor)) +
            geom_text(data = uvrisk, aes(x = uvdatacity$NEW_DATE[3], y = medy, label = name, size = 3))+
            geom_line(data = uvdatacity, aes(x = NEW_DATE, y = UV_VALUE)) +
            scale_y_continuous("UV Index", limit = c(-0.1, 11), breaks = c(0,1,2,3,4,5,6,7,8,9,10,11), expand = c(0, 0)) +
            ggtitle("UV Index in the Past Day") +
            theme(axis.title.x = element_blank(),
                  legend.position = "None")
    })
    
    

}

# Run the application 
shinyApp(ui = ui, server = server)
