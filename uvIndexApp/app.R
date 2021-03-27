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
                shinyUI(navbarPage("ZENITH",
                                   tabPanel("UV Rays",
                                            h2("What is UV and why should you care?"),

                                   ),
                                   tabPanel("UV Index | Zipcode",  

                                            div(id = "header", titlePanel("Explore your local UV index")),
                                            textInput("zipinput", "Enter your zipcode", "33331"),
                                            plotOutput("zipplot"),
                                            hr(),
                                            h6("Our data is from the Envirofacts Data Service API hosted by the", a("United States Environmental Protection Agency.", href = "https://www.epa.gov/enviro/web-services#uvindex"),  "We retrieved the UV Index data by querying hourly forecasts given a city and state or zipcode.")
                                   ),
                                   tabPanel("UV Index | City",
                                            div(id = "header", titlePanel("Explore your local UV index")),
                                            textInput("cityinput", "Enter your city", "san francisco"),
                                            textInput("stateinput", "Enter your state", "ca"),
                                            plotOutput("cityplot"),
                                            hr(),
                                            h6("Our data is from the Envirofacts Data Service API hosted by the", a("United States Environmental Protection Agency.", href = "https://www.epa.gov/enviro/web-services#uvindex"),  "We retrieved the UV Index data by querying hourly forecasts given a city and state or zipcode.")
                                   ),
                                   
                                   tabPanel("Skin Protection", 
                                            titlePanel("Information"),
                                            
                                            navlistPanel(widths = c(4,8),
                                                "Sunscreen",
                                                tabPanel("General sunscreen tips",
                                                         h4("- Apply sunscreen every day! (even when you just stay in the shade most of the day)"),
                                                         h4("- Reapply sunscreen every 2 hours especially when staying outdoors and/or after swimming and sweating."),
                                                         h4("- Remember to wear sunscreen for the lips too! (search for lip balms with SPF)")
                                                         ),
                                                tabPanel("How much sunscreen is enough?",
                                                         h4("- The general rule is to spread a layer of sunscreen over the skin areas that will be exposed to daylight (whether it’s face, neck or body), because not everyone has the same face or body size."),
                                                         h4("- That being said, for some recommended specific amount: ¼ teaspoon for face alone, ½ teaspoon for face + neck and each arm, 1 teaspoon on each leg, the front of the torso and the back of the torso.")
                                                         ),
                                                tabPanel("Things to look for in sunscreen",
                                                         h4("- Broad-spectrum protection (protects against UVA and UVB rays)"),
                                                         h4("- SPF 30 or higher"),
                                                         h4("- Water resistance (up to 40 minutes in water)"),
                                                         h4("- Types of UV filters in the sunscreen"),
                                                         h5("+ Inorganic (physical/mineral) filters: Zinc Oxide, Titanium Dioxide. Also reef-safe. More suitable for people with sensitive skin."),
                                                         h5("+ Organic (chemical) filters: Tinosorb S and M, Mexoryl SX (exclusive filter in L’Oreal sunscreens), Oxybenzone, Octinoxate, Avobenzone, Homosalate, etc."),
                                                         h5("+ Popular UV filters that are FDA-approved: Avobenzone, Homosalate, Octocrylene, Octinoxate, Octisalate, Oxybenzone, Zinc Oxide, Titanium Dioxide"),
                                                         h5("+ Concerns over harmful chemical UV filters: Oxybenzone, Octisalate, Octocrylene, Homosalate - hormone disruption, toxic to coral reefs")
                                                         ),
                                                "Physical Protection",
                                                tabPanel("Clothing",
                                                         h4("Actually, there are a lot of sun protection clothes out there, and the variety is probably more than what you may think! For example,", a("Amazon", href = "https://www.amazon.com/Sun-Protection-Clothes/s?k=Sun+Protection+Clothes"), "offers a wide range of clothes that allow you to enjoy yourself under the sun.")
                                                         ),
                                                tabPanel("Parasols & Umbrellas",
                                                         h4("Don't forget to equip yourself with a gorgeous parasol or umbrella when you go out under the sun! Again,", a("Amazon", href = "https://www.amazon.com/Parasol-Umbrella/s?k=Parasol+Umbrella"), "can be a good place to start finding your favorite.")
                                                         )
                                            )
                                   ),
                                   tabPanel("Resources",
                                            h2("To find out more..."),
                                            hr(),
                                            h3("Skin cancer awareness organizations in the US"),
                                            h4(a("The Skin Cancer Foundation", href = "https://www.skincancer.org/")),
                                            h4(a("The National Council on Skin Cancer Prevention", href = "https://skincancerprevention.org/"))

                                        
                                   )
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
    
    uvrisk <- data.frame(name = c("Low", "Moderate", "High", "Very High"),
                         imin = c(0,3,6,8),
                         imax = c(3,6,8,11),
                         mycolor = c("A", "B", "C", "D")) %>%
        mutate(medy = imin + floor((imax-imin)/2))
    
    output$zipplot <- renderPlot({
        
        zipbase_url <- "https://enviro.epa.gov/enviro/efservice/getEnvirofactsUVHOURLY/ZIP/"
        zipfull_url <- paste0(zipbase_url, zipcode(), "/JSON")
        zipdata <- as.data.frame(fromJSON(readLines(zipfull_url)))
        
        zipdata <- zipdata %>%
            mutate(NEW_DATE = mdy_h(DATE_TIME))
        
        ggplot() +
            theme_light() +
            geom_rect(data = uvrisk, aes(xmin = c(zipdata$NEW_DATE[1], zipdata$NEW_DATE[1], zipdata$NEW_DATE[1], zipdata$NEW_DATE[1]), 
                                         xmax = c(zipdata$NEW_DATE[21], zipdata$NEW_DATE[21], zipdata$NEW_DATE[21], zipdata$NEW_DATE[21]), 
                                         ymin = imin, ymax = imax, fill = mycolor)) +
            geom_text(data = uvrisk, aes(x = zipdata$NEW_DATE[3], y = medy, label = name, size = 3))+
            geom_line(data = zipdata, aes(x = NEW_DATE, y = UV_VALUE)) +
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