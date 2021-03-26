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

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Old Faithful Geyser Data"),
    
    div(id = "header",
        titlePanel("Explore your UV Index")),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("indexplot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)
        
        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
    
    output$indexplot <- renderPlot({
        
        uvrisk <- data.frame(name = c("Low", "Moderate", "High", "Very High"),
                             imin = c(0,3,6,8),
                             imax = c(3,6,8,11),
                             mycolor = c("A", "B", "C", "D")) %>%
            mutate(medy = imin + floor((imax-imin)/2))
        
        zipbase_url <- "https://enviro.epa.gov/enviro/efservice/getEnvirofactsUVHOURLY/ZIP/"
        zipfull_url <- paste0(zipbase_url, "95129/JSON")
        uvdf <- fromJSON(readLines(zipfull_url))
        
        uvdf <- uvdf %>%
            mutate(NEW_DATE = mdy_h(DATE_TIME))

        ggplot() +
            theme_light() +
            geom_rect(data = uvrisk, aes(xmin = c(uvdf$NEW_DATE[1], uvdf$NEW_DATE[1], uvdf$NEW_DATE[1], uvdf$NEW_DATE[1]), xmax = c(uvdf$NEW_DATE[21], uvdf$NEW_DATE[21], uvdf$NEW_DATE[21], uvdf$NEW_DATE[21]), ymin = imin, ymax = imax, fill = mycolor)) +
            geom_text(data = uvrisk, aes(x = uvdf$NEW_DATE[3], y = medy, label = name, size = 3))+
            geom_line(data = uvdf, aes(x = NEW_DATE, y = UV_VALUE)) +
            scale_y_continuous("UV Index", limit = c(-0.1, 11), breaks = c(0,1,2,3,4,5,6,7,8,9,10,11), expand = c(0, 0)) +
            #scale_x_continuous(expand = c(0,0)) +
            ggtitle("UV Index in the Past Day") +
            theme(axis.title.x = element_blank(),
                  legend.position = "None")
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
