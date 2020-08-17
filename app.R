

library(shiny)
library(ggplot2)
library(UsingR)

ui <- fluidPage(
    titlePanel("Diamond Price Predication based on Diamonds Dataset"),
    h3(strong("How to use this application:")),
    h4("This application is to estimate the price of a diamond depending on
       the size, cut and color. Diamonds dataset from Cran was used. The linear regression
       with carat, cut and color as predictive variables was the prediction model."),
    h4("Input the size in carat, type of cut and color below."),
    h4("Please see the picture for color coding"),
    
    sidebarLayout(
        sidebarPanel(
            sliderInput("carat",
                        label = "The size of a diamond in Carat:",
                        min = 0.5, max = 5, value = TRUE),
            
            selectInput("cut",
                        label = "Diamond Cut:",
                        choices = c("Fair","Good","Very Good","Premium","Ideal"),
                        selected = "Very Good")),
        
        selectInput("color",
                    label = "Diamond Color: from D (best) to J (worst)",
                    choices = c("D","E","F","G","H","I","J"),
                    selected = "G"),       
    ),
    
   mainPanel(
   tags$image(height = 300, width = 700, src = "Diamond Color.png"),
        h3("Predicted Price for your diamond"),
        h3(strong("$"),strong(textOutput("pred")))
    )
)



server <- function(input, output) {
    fit <- lm(price ~ I(carat-0.2) + cut + color, data = diamonds)
    predicted <- reactive({
        predict(fit, newdata = data.frame(carat = input$carat,
                                          cut = input$cut,
                                          color = input$color))
    })
    
    output$pred <- renderText({
        predicted()
    })

}

# Run the application 
shinyApp(ui = ui, server = server)
