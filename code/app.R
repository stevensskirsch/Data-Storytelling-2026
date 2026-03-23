# Cars Three Variable Scatterplot App 
library(MASS)
library(shiny) 
library(ggplot2)

###########################################################
## Part 1: Defining the attributes of the User Interface ##
###########################################################

ui <- fluidPage(
  
  # headerPanel determines the name of the app
  headerPanel('Cars Three Variable Scatterplot'),
  
  # sidebarPanel is one type of user interface that can include several types of inputs
  sidebarPanel(
    
    # selectInput allows for the selection of categorical inputs for the app
    selectInput(inputId = 'xcol', # the label of the input object  
                label = 'X Variable', # the label of this input as seen by users
                choices = names(Cars93)[-c(1:3, 9, 10, 26, 27)], # options available ot users
                selected = names(Cars93)[7]), # the option that is selected by default
    
    selectInput(inputId = 'ycol', 
                label = 'Y Variable', 
                choices = names(Cars93)[-c(1:3, 9, 10, 26)],
                selected = names(Cars93)[5]),
    
    selectInput('color', 'Z Variable', names(Cars93)[c(3, 9, 10, 26)],
                selected = names(Cars93)[3])
  ),
  
  # mainPanel is the location where our app's output is presented
  mainPanel(
    plotOutput('plot1')
  ) 
)

server <- function(input, output) {

  selectedData <- reactive({
    Cars93[, c(input$xcol, input$ycol, input$color)]
  })

  output$plot1 <- renderPlot({
    ggplot(data = selectedData(), aes(x = eval(as.name(input$xcol)), y = eval(as.name(input$ycol)), color = eval(as.name(input$color)))) +
      geom_point() + 
      labs(x = input$xcol,
           y = input$ycol,
           color = input$color)
  })

}

shinyApp(ui = ui, server = server)
