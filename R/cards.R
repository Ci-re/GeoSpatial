# source("hello.R")
# source("functions.R")
# source("options_module.R")

#' @importFrom shiny NS tagList

cards_UI <- function(id){

  ns <- NS(id)
  tagList(
    infoBoxOutput(outputId = ns("toptraits"), width = 12),
    plotOutput(outputId = ns("lineplot"), width = "100%", height = "200px")
  )
}

cards_server <- function(id, traitr, color, dataframe, icon1, plot_choice){
  moduleServer(id, function(input, output, session) {

    getValues <- eventReactive(dataframe, {
      req(dataframe)
      # print(dataframe)
      datageh <- dataframe %>% filter(trait == toupper(traitr))
      if(toupper(traitr) == "MCMDS"){
        datageh <- datageh %>% arrange(combined)
      }else {
        datageh <- datageh %>% arrange(desc(combined))
      }
      # print(datageh)
      keyvals <- list("accession" = datageh$accession[1], "value" = datageh$combined[1])
      # print(keyvals)
      return(keyvals)
    })

    output$toptraits <- renderInfoBox({
      req(getValues())
      vals <- getValues()
      full_names <- list("MCMDS" = "Cassava Mosaic", "HI" = "Harvest Index", "DM" = "Dry matter",
                         "SPROUT" = "Sprout", "PLTHT" = "Plant Height",
                         "DYLD" = "Dry yield", "FYLD" = "Fresh yield")
      infoBox(title = h5(vals["accession"], class = "clone-name"), value = vals["value"],
              subtitle = full_names[toupper(traitr)], icon = icon(icon1), color = color, fill = TRUE)
    })

    output$lineplot <- renderPlot({
      req(getValues())
      if(plot_choice == TRUE){
        uytdata <- linePlot(trait_to_plot = toupper(traitr), imported_data = dataframe)
      } else if(plot_choice == FALSE) {
        uytdata <- linePlot_environment(trait_to_plot = toupper(traitr), imported_data = dataframe)
      }
      uytdata
    })
  })
}

# ui <- dashboardPage(
#   dashboardHeader(),
#   dashboardSidebar(),
#   dashboardBody(
#     includeCSS("style.css"),
#     cards_UI("uid")
#   )
# )
# server <- function(input, output, session){
#   dat <- read_csv("../../Visualizations/combo.csv")
#   cards_server(id = "uid",traitr = "DYLD", color = "red", dataframe = dat)
# }

# shinyApp(ui  , server)
