source("hello.R")
source("_functions.R")


options_ui <- function(id, trigger){
  ns <- NS(id)

  tagList(
    bsModal(id = ns("settingsModal"), title = "Settings", trigger = trigger, size = "small",
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
            uiOutput(ns("checks_select")),
            actionButton(ns("save"),"Save",icon = icon("chart-pie"), style = "width: 100%", class = "btn-success")
    )
  )
}


options_server <- function(id, dataframe) {
  moduleServer(id, function(input, output, session) {
    # toggleModal(session, "settingsModal",toggle = "toggle")

    list_of_topTraits <- eventReactive(input$save, {
      # req(input$save)
      original_list <- c("dyld","fyld","dm","pltht","mcmds","sprout","hi")
      top_traits <- input$top_traits
      selected <- fix_listOfTop_Traits(original_list = original_list, top_traits =  top_traits)
      return(selected)
    })

    get_AccessionNames <- reactive({
      req(dataframe)
      print("hittttttttt")
      uyt <- dataframe
      return(unique(uyt$Accession))

      # return(unique(wrangle_data(read.csv("../../Visualizations/combo.csv"))$accession))
    })

    output$checks_select <- renderUI({
      pickerInput(
        inputId = session$ns("check_select"),
        label = "Select checks",
        choices = c(get_AccessionNames()),
        multiple = TRUE,
        # selected = NULL,
        options = pickerOptions(
          liveSearch = TRUE,
          style = "btn-primary",
          `action-box` = TRUE,
          size = 5
        )
      )
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

