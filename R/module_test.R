####### Test Module ##########

## hello.r will load required packages needed for this module
source("hello.R")


### Defining a UI
test_ui <- function(id){
  ns <- NS(id)

  tagList(
    actionButton(inputId = ns("test_button"), "Click me"),
    verbatimTextOutput(ns("test_output"))
  )
}


#### Defining the server
test_server <- function(id){
  moduleServer(id, function(input, output, session){
    count_x <- reactiveVal(0)
    observeEvent(input$test_button, {
      count_x(count_x() + 1)
    })

    output$test_output <- renderText({
      count_x()
    })

  })
}

## Merging the UI and the Server
ui <- fluidPage(
  test_ui("module_1")
)

server <- function(input, session, output){
  test_server("module_1")
}


### Calling shinyapp
shinyApp(ui, server)

################## MODULE WORKS $$$$$$$$$$$$$$$$$

