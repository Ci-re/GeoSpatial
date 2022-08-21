source("hello.R")
source("module_dataset.R")
source("_functions.R")
source("cards.R")
source("options_module.R")

options(shiny.autoreload = TRUE)

ui <- dashboardPage(skin = "black",
  dashboardHeader(disable = TRUE),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    tags$style(type = "text/css", "#map {height: calc(100vh - 53px) !important;}"),
    includeCSS("style.css"),
    fluidRow(class = "headerRow",
             column(6, h2("MULTILOC", class = "app-header")),
             column(3, actionButton("settings", "", icon = icon("gear"), class = "btn-primary",
                                    style = "position: absolute; top: 20px; right: 40px")),
             column(3, fileInput(inputId = "location_dataset", accept = c("csv","xlsx"),buttonLabel = "Trial",
                                 label = ""))
    ),
    options_ui("settingsID", trigger = "settings"),
    fluidRow(style = "padding-top:10px;background-color:#cecece; border-radius: 10px; margin: 2px" ,
      column(4, class = "header-row", cards_UI("firstCard")),
      column(4, class = "header-row", cards_UI("secondCard")),
      column(4, class = "header-row", cards_UI("thirdCard"))
    )
  )
)

server <- function(input, output, session) {
  # toggleModal(session, "settingsModal", toggle = "toggle")


  traits <- options_server("settingsID", userFile())

  userFile <- reactive({
    req(input$location_dataset)
    file1 <- input$location_dataset
    validate(need(identical(tools::file_ext(file1$datapath), "csv"),"csv data needed"))
    try(read_csv(file1$datapath))
  })



  observeEvent(traits(),{
    cards_server(id = "firstCard", traitr = traits()[1], icon1 = "chart-pie", color = "teal", dataframe = userFile())
    cards_server(id = "secondCard", traitr = traits()[2], icon1 = "chart-line", color = "blue", dataframe = userFile())
    cards_server(id = "thirdCard", traitr = traits()[3], icon1 = "code-branch", color = "red", dataframe = userFile())
  })
}

shinyApp(ui, server)
