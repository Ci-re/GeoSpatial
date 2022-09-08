# setwd("/home/cirec/Desktop/git_workspace/GeoSpatial/")
source(file = "hello.R", local = TRUE)
source("functions.R")
source("cards.R")
source("options_module.R")
source("visualizer.R")
source("functions2.R")
source("mapsfunction.R")

rApp <- function(...){
  options(shiny.autoreload = TRUE)
  # setwd("~/Desktop/git_workspace/GeoSpatial/R")
  ui <- shinydashboard::dashboardPage(skin = "black",
            shinydashboard::dashboardHeader(title = HTML("MULTILOC"),
                                            disable = FALSE, tags$li(class = "dropdown",
                                             actionButton("settings", "",
                                                          icon = icon("gear"), class = "btn-primary",
                                                          style = "position: absolute; top: 10px; right: 40px"))),
            shinydashboard::dashboardSidebar(disable = TRUE),
            shinydashboard::dashboardBody(
                        tags$style(type = "text/css", "#map {height: calc(100vh - 53px) !important;}"),
                        includeCSS("www/style.css"),
                        # fixedPanel(top = 0,bottom = "20px", width = "100%", draggable = FALSE, right = "20px",left = "20px",
                        #   fluidRow(style = "padding-top:10px;background-color:#000; border-radius: 10px; margin: 90px; z-index: 1; margin-bottom: 800px;",
                        #     column(6, h2("MULTILOC", class = "app-header")),
                        #     column(3, actionButton("settings", "", icon = icon("gear"), class = "btn-primary",
                        #                            style = "position: absolute; top: 20px; right: 40px")),
                        #     column(3, fileInput(inputId = "location_dataset", accept = c("csv","xlsx"), buttonLabel = "Trial",
                        #                         label = ""))
                        #
                        #   )
                        # ),
                        fluidRow(style = "padding-top:10px;background-color:#fcfcee; border-radius: 10px;" ,
                                 options_ui("settingsID", trigger = "settings"),
                                 fluidRow(style = "padding-top:10px;background-color:inherit; padding-bottom: 150px; margin: 2px;",
                                          column(4, class = "header-row", withSpinner(type = 6,cards_UI("firstCard"))),
                                          column(4, class = "header-row", withSpinner(type = 6,cards_UI("secondCard"))),
                                          column(4, class = "header-row", withSpinner(type = 6,cards_UI("thirdCard"))),
                                 ),
                                 visualizer_UI(id = "visualizerID")
                        ),
                      )
  )

  server <- function(input, output, session) {
    # toggleModal(session, "settingsModal", toggle = "toggle")

    # traits$plot()
    # userFile <- reactive({
    #   file1 <- input$location_dataset
    #   validate(need(identical(tools::file_ext(file1$datapath), "xls"),"csv, xls or xlsx data needed"))
    #   return(file1)
    # })
    #
    # touch_data <- reactive({
    #   req(userFile())
    #   # print(read_xls(userFile()$datapath))
    #   SI_data <- get_SI_from_BLUPS(userFile()$datapath)
    #   ENV_data <- get_GXE_data(userFile()$datapath)
    #
    #   SI_data <- wrangle_data(SI_data)
    #   ENV_data <- attach_locs(wrangle_data(ENV_data))
    #   list_of_data <- list("sindex" = SI_data, "env" = ENV_data)
    #   return(list_of_data)
    # })

    traits <- options_server(id = "settingsID")

    observe({
      renderUIs()
    })

    renderUIs <- reactive({
      req(traits())
      cards_server(id = "firstCard", traitr = traits()$selected[1],
                   icon1 = "chart-pie", color = "teal", dataframe = traits()$dataframes$env, plot_choice = traits()$option)
      cards_server(id = "secondCard", traitr = traits()$selected[2],
                   icon1 = "chart-line", color = "blue", dataframe = traits()$dataframes$env, plot_choice = traits()$option)
      cards_server(id = "thirdCard", traitr = traits()$selected[3],
                   icon1 = "code-branch", color = "red", dataframe = traits()$dataframes$env, plot_choice = traits()$option)
      visualizer_SERVER("visualizerID", checks = traits()$checks, sindex_dataframe = traits()$dataframes$sindex,
                        combined_dataframe = traits()$dataframes$env)
    })
  }

  shinyApp(ui, server)

}

rApp()
