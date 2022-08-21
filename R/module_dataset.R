#### Function to load data


source("hello.R")
source("_functions.R")

dataset_ui <- function(id, dataset){
  ns <- NS(id)

  tagList(
    fileInput(ns("file"), label = "Enter your file"),
    DT::dataTableOutput(ns("table"))
  )
}

dataset_server <- function(id){
  moduleServer(id, function(input, output, session) {
    ## if no file is selected, don't do anything
    userFile <- reactive({
      validate(need(input$file, message = FALSE))
      input$file
    })

    dataframe <- reactive({
      read_csv(userFile()$datapath)
    })

    observe({
      msg <- sprintf("File %s was uploaded", userFile()$name)
      cat(msg, "\n")
    })

    output$table <- DT::renderDataTable({
      geo_data(dataframe())
    })

  })
}


ui <- fluidPage(
  dataset_ui("module_1")
)

server <- function(input, session, output){
  dataset_server("module_1")
}


### Calling shinyapp
shinyApp(ui, server)
