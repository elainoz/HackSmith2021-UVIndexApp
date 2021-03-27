# Load packages

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
                                   tabPanel("UV Info",
                                            titlePanel("UV Rays"),
                                            
                                            navlistPanel(widths = c(2,8),
                                                "UV Light Information",
                                                tabPanel("What is Ultraviolet (UV) Light?",
                                                         h4("Ultraviolet (UV) radiation is a type of energy that is produced by the sun. It makes black-light posters glow and is responsible for our summer tans, and"),
                                                         h4("sunburns! Sometimes, too much UV exposure can cause us harm, such as skin cancer and eye damage."),
                                                         div(img(src = "sun.png", height = 700, width = 800), style="text-align: center;")
                                                         ),
                                                tabPanel("Types of Ultraviolet Light",
                                                         h4("There are actually 3 types of UV rays: UVA, UVB and UVC. However, UVC is totally absorbed by the ozone layer and therefore will never reach the earth."),
                                                         h4("That means we are left with 2 types of UV rays to be concerned about:"),
                                                         h5(HTML('&nbsp;')),
                                                         h4(strong("UVA rays (aging rays):")),
                                                         h4("These can prematurely age your skin, causing wrinkles and age spots, and can pass through window glass."),
                                                         h5(HTML('&nbsp;')),
                                                         h4(strong("UVB rays (or burning rays):")),
                                                         h4("These are the primary cause of sunburn and are blocked by window glass."),
                                                         div(img(src = "uv-rays.png", height = 500, width = 700), style="text-align: center;"),
                                                         h6("Source:", a("Apothekari Skincare", href = "https://apothekari.com/all-about-uv-rays/"), align = "center")
                                                         
                                                ),
                                                "UV Index Information",
                                                tabPanel("What is the UV index?",
                                                         h4("The", a("National Weather Service,", href = "https://www.weather.gov/"), "in conjunction with the", a("US Environmental Protection Agency", href = "https://www.epa.gov/"), "published the UV Index in 1994. It’s purpose is to aid Americans"), 
                                                         h4("in deciding and planning their outdoor adventures in hopes of avoiding overexposure to radiation from the sun’s rays, which may lead to long term health"),
                                                         h4("problems."),
                                                         div(img(src = "uv-index-epa.jpg", height = 650, width = 800), style="text-align: center;"),
                                                         h6("Source:", a("Melanoma Research Foundation on Twitter", href = "https://twitter.com/curemelanoma/status/1000019662481035265?lang=hr"), align = "center")
                                                         
                                                ),
                                                tabPanel("Why should I care about the UV index?",
                                                         h4("Overexposure to the sun’s UV radiation can have many effects ranging from sunburns to skin cancers. Paying attention to the UV index can hopefully offer"),
                                                         h4("Americans access to information that can prevent any health problems."),
                                                         div(img(src = "uv-rays-meme.png", height = 700, width = 620), style="text-align: center;")    
                                                )
                                            ),
                                                
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
                                            titlePanel("Skin Protection"),
                                            
                                            navlistPanel(widths = c(2,8),
                                                "Sunscreen",
                                                tabPanel("General sunscreen tips",
                                                         h4("- Apply sunscreen every day! (even when you just stay in the shade most of the day)"),
                                                         h4("- Reapply sunscreen every 2 hours especially when staying outdoors and/or after swimming and sweating."),
                                                         h4("- Remember to wear sunscreen for the lips too! (search for lip balms with SPF)"),
                                                         div(img(src = "uvfilters.jpg", height = 600, width = 900), style="text-align: center;"),
                                                         h6("Source:", a("Gothamista", href = "https://www.gothamista.com/tag/sunscreen/"), align = "center")
                                                         ),
                                                tabPanel("How much sunscreen is enough?",
                                                         h4("- The general rule is to spread a layer of sunscreen over the skin areas that will be exposed to daylight (whether it’s face, neck or body), because not"),
                                                         h4("everyone has the same face or body size."),
                                                         h4("- That being said, there is something called the Teaspoon Rule for you to follow: ¼ teaspoon for face alone, ½ teaspoon for face + neck and each arm, 1"),
                                                         h4("teaspoon on each leg, the front of the torso and the back of the torso."),
                                                         div(img(src = "Sunscreen_graphic-01.jpg", height = 600, width = 870), style="text-align: center;"),
                                                         h6("Source: Schneider J. The Teaspoon Rule of Applying Sunscreen. Arch Dermatol. 2002;138(6):838–839. doi:10.1001/archderm.138.6.838", align = "center")
                                                         ),
                                                tabPanel("Things to look for in sunscreen",
                                                         h4("Look out for these things every time you buy a sunscreen:"),
                                                         h4(strong("- Broad-spectrum protection (protects against UVA and UVB rays)")),
                                                         h4(strong("- SPF 30 or higher")),
                                                         h4(strong("- Water resistance (up to 40 minutes in water)")),
                                                         h4(strong("- Types of UV filters in the sunscreen")),
                                                         h5("+ Inorganic (physical/mineral) filters: Zinc Oxide, Titanium Dioxide. These filters are suitable for sensitive skin and are also reef-safe."),
                                                         h5("+ Organic (chemical) filters: Tinosorb S and M, Mexoryl SX, Oxybenzone, Octinoxate, Avobenzone, Homosalate, etc. However, there are some concerns over certain chemical UV filters, such as"),
                                                         h5("Oxybenzone, Octisalate, Octocrylene and Homosalate, which are claimed to be toxic to coral reefs and cause hormone disruption for humans."),
                                                         h5("+ Some of these filters are FDA-approved: Avobenzone, Homosalate, Octocrylene, Octinoxate, Octisalate, Oxybenzone, Zinc Oxide, Titanium Dioxide; while some are only available in countries outside the"),
                                                         h5("US (such as European countries)"),
                                                         div(img(src = "Inorganic vs Organic UV Filters Diagram.png", height = 500, width = 800), style="text-align: center;"),
                                                         h6("Source:", a("Croda Personal Care", href = "https://www.crodapersonalcare.com/en-gb/discovery-zone/technology-platforms/inorganic-uv-filters"), align = "center")
                                                         ),
                                                "Physical Protection",
                                                tabPanel("Clothing",
                                                         h4("Actually, there are a lot of sun protection clothes out there, and the variety is probably more than what you may think! For example,", a("Amazon", href = "https://www.amazon.com/Sun-Protection-Clothes/s?k=Sun+Protection+Clothes"), "offers a wide"),
                                                         h4("range of clothes that allow you to enjoy yourself under the sun."),
                                                         
                                                         div(img(src = "sun-clothes.jpeg", height = 700, width = 800), style="text-align: center;"),
                                                         h6("Source:", a("Golf Digest", href = "https://www.golfdigest.com/story/golf-style-heres-what-you-need-to-know-about-sun-protection-clothing"), align = "center")
                                                         ),
                                                tabPanel("Parasols & Umbrellas",
                                                         h4("Equip yourself with a gorgeous parasol or umbrella when you go out under the sun! Again,", a("Amazon", href = "https://www.amazon.com/Parasol-Umbrella/s?k=Parasol+Umbrella"), "can be a good place to start finding your favorite."),
                                                         div(img(src = "umbrella.jpg", height = 700, width = 600), style="text-align: center;")
                                                         )
                                            )
                                   ),
                                   tabPanel("Resources",
                                            h1("To find out more..."),
                                            h3("Skin cancer awareness organizations in the US"),
                                            h4(a("The Skin Cancer Foundation", href = "https://www.skincancer.org/")),
                                            h4(a("The National Council on Skin Cancer Prevention", href = "https://skincancerprevention.org/")),
                                            div(img(src = "keep-calm-and-wear-sunscreen-3.png", height = 700, width = 600), style="text-align: center;")
                                            
                                            
                                        
                                   ),
                                   tabPanel("Our Team",
                                            h4("We are all on our first year at Smith and this is our first hackathon."))
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