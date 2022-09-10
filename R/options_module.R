source("hello.R")
source("functions.R")


options_ui <- function(id, trigger){
  ns <- NS(id)
  tagList(
    shinyBS::bsModal(id = ns("settingsModal"), title = "Settings", trigger = trigger, size = "small",
            fileInput(placeholder = "Please upload BLUPS data...",inputId = ns("location_dataset"), accept = c("csv","xlsx"), buttonLabel = "Trial",
                      label = ""),
            pickerInput(
              inputId = ns("top_traits"),
              label = "Select top traits",
              choices = c("Dry yield" = "dyld", "Fresh Yield" = "fyld", "Dry Matter" = "dm",
                          "Plant Height" = "pltht", "Sprout" = "sprout", "Mosaic" = "mcmds",
                          "Harvest Index" = "hi"),
              multiple = TRUE,
              selected = c("dyld","dm","mcmds"),
              options = pickerOptions(
                title = "Top four traits",
                header = "Top four traits",
                style = "primary",
                maxOptions = 3,
                maxOptionsText = "Please select at most 3 traits"
              )
            ),
            prettyCheckbox(
              inputId = ns("plot_option"), label = "Switch plot",
              status = "success", outline = TRUE,value = TRUE,
            ),
            uiOutput(outputId = ns("accession_range")),
            # switchInput(inputId = ns("plot_option"), label = "Switch", value = TRUE),
            uiOutput(ns("checks_select")),
            # awesomeCheckbox(inputId = ns("select_check_option"),
            #                 label = "Calculate Check Difference",
            #                 value = FALSE, status = "success"),
            actionButton(ns("save"),"Load",icon = icon("chart-pie"),
                         style = "width: 100%", class = "btn-success")
    )
  )
}



 options_server <- function(id) {
  moduleServer(id, function(input, output, session) {




    userFile <- reactive({
      req(input$location_dataset)
      file1 <- input$location_dataset
      validate(need(identical(tools::file_ext(file1$datapath), "xls"),"csv, xls or xlsx data needed"))
      return(file1)
    })

    touch_data <- reactive({
      req(userFile())
      # print(read_xls(userFile()$datapath))
      SI_data <- get_SI_from_BLUPS(userFile()$datapath)
      ENV_ <- get_GXE_data(userFile()$datapath)
      SI_data <- wrangle_data(SI_data)
      ENV_data <- attach_locs(wrangle_data(ENV_))
      list_of_data <- list("sindex" = SI_data, "env" = ENV_data)
      return(list_of_data)
    })

    get_AccessionNames <- reactive({
      req(userFile())
      dataframe <- touch_data()$sindex
      # print(dataframe)
      # print("hittttttttt")
      uyt <- wrangle_data(dataframe)
      # print(uyt)
      return(unique(uyt$accession))
      # return(unique(wrangle_data(read.csv("../../Visualizations/combo.csv"))$accession))
    })

    output$checks_select <- renderUI({
      req(userFile())
      pickerInput(
        inputId = session$ns("checks_select"),
        label = "Select checks",
        choices = c(get_AccessionNames()),
        multiple = TRUE,
        selected = get_checks(get_AccessionNames()),
        options = pickerOptions(
          liveSearch = TRUE,
          style = "btn-primary",
          `action-box` = TRUE,
          size = 10
        )
      )
    })

    output$accession_range <- renderUI({
      maxObs <- length(get_AccessionNames())
      minObs <-  1
      sliderInput(
        inputId = session$ns("accession_range"),
        label = h3("Range of Observation"),
        min = 1,
        max = maxObs,
        value = c(1, 4),
        step = 1
      )
    })

    observeEvent(input$save, {
      toggleModal(session, "settingsModal",toggle = "toggle")
    })

    list_of_topTraits <- eventReactive(input$save, {
      # req(input$save)
      # plot_option()
      # print(input$plot_option)

      original_list <- c("dyld","fyld","dm","pltht","mcmds","sprout","hi")
      top_traits <- input$top_traits
      opt <- ""
      if(input$plot_option == TRUE){
        opt <- TRUE
      } else {
        opt <- FALSE
      }
      selected <- fix_listOfTop_Traits(original_list = original_list, top_traits =  top_traits)
      dataframes <- touch_data()
      accession_range <- input$accession_range
      # print(input$checks_select)
      list_return <- list(option = opt, selected = selected, checks = input$checks_select, dataframes = dataframes, range_acc = accession_range)
      return(list_return)
    })
    return(list_of_topTraits)

  })
}

#
# ui <- fluidPage(options_ui("module_2", trigger = "settings"))
# server <- function(input, output, session){
#   options_server("module_2")
# }
#
# shinyApp(ui , server)


