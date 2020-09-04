# PANDA WROTE THIS!!!

library(shiny)
library(tidyverse)
library(colourpicker)

# Define UI 
ui <- fluidPage(
   
   # Application title
   titlePanel("Colour Axis Generator"),
   uiOutput("plott"),
  # plotOutput(outputId = "ggplot", width = "400px", height = "100px"),
   hr(),
   wellPanel(
     fluidRow(
       column(width = 4, 
              numericInput(inputId = "min_val", label = "Enter Minimum Value:",
                                      value = 1),
              numericInput(inputId = "max_val", label = "Enter Maximum Value:",
                           value = 2),
              numericInput(inputId = "inc_val", label = "Enter Increment Value:",
                           value = 0.01),
              numericInput(inputId = "mid_val", label = "Enter Midpoint Value:",
                           value = 1.5)
              ),
       column(width = 4,
              colourInput(inputId = "min_picker", label = "Minimum Colour", value = "#00FFA6"),
              colourInput(inputId = "mid_picker", label = "Midpoint Colour", value = "#FFFF42"),
              colourInput(inputId = "max_picker", label = "Maximum Colour", value = "#FF5300")
              ),
       column(width = 4,
              # numericInput(inputId = "axis_height", label = "Axis Height",
              #              value = 10)
              selectInput(
                inputId = "font_picker",
                label = "Font",
                choices = c( "Helvetica", "Times New Roman","Arial", "Arial Narrow", "Microsoft Sans Serif", "Comic Sans MS")
                  ),
              numericInput(
                inputId = "font_size",
                label = "Font Size",
                value = 12,
               min = 4,
               max = 64
                ),
              checkboxInput(inputId = "show_percent",
                            label = "Show as Percent",
                            value = FALSE),
              numericInput(
                inputId = "plot_width",
                label = "Width (px)",
                value = 500,
                min = 0
              ),
              numericInput(
                inputId = "plot_height",
                label = "Height (px)",
                value = 100,
                min = 0
              )
       )
     )
   )
      


      


)

# Define server logic 
server <- function(input, output) {
   

   output$ggplot <-  renderPlot({
     
     min_val <- input$min_val
     max_val <- input$max_val
     inc_val <- input$inc_val
    # rep_val <- input$axis_height
     
     x <- seq(min_val,max_val,inc_val)
     y <- rep(10, length(x))
     data <- data.frame(cbind(x, y))
     
     min_colour <- input$min_picker
     mid_colour <- input$mid_picker
     max_colour <- input$max_picker
     mid_point <- input$mid_val
     mid_val <- (mid_point - min_val)/(max_val - min_val)
     
     font_size <- input$font_size
     
     plot <- ggplot(data, aes(x, y)) + geom_col(aes(colour = x, size = 10)) +
       scale_color_gradientn(colours = c(min_colour, mid_colour, max_colour), 
                             values = c(0, mid_val, 1)) +
       # scale_color_gradient2(
       #                       low = min_colour, mid = mid_colour, high = max_colour,
       #                       midpoint = mid_point) +
       # scale_x_continuous(labels = scales::percent) +
       theme( #axis.line=element_blank(),
         text = element_text(family = input$font_picker),
         axis.text.x=element_text(angle = 90, vjust = 0.5, size = font_size),
         axis.text.y=element_blank(),axis.ticks=element_blank(),
         axis.title.x=element_blank(),
         axis.title.y=element_blank(),legend.position="none",
         panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
         panel.grid.minor=element_blank(),plot.background=element_blank())
      
     
     if (input$show_percent) {
       return (plot + scale_x_continuous(labels = scales::percent))
     }
     plot
   })

   
   output$plott <- renderUI({
     plotOutput(outputId = "ggplot", width = input$plot_width, height = input$plot_height)
   })

     
}

# Run 
shinyApp(ui = ui, server = server)

