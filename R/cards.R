source("hello.R")
source("_functions.R")
source("options_module.R")


cards_UI <- function(id){

  ns <- NS(id)
  tagList(
    infoBoxOutput(outputId = ns("toptraits"), width = 12),
    plotOutput(outputId = ns("lineplot"))
  )
}

cards_server <- function(id, traitr, color, dataframe, icon1){
  moduleServer(id, function(input, output, session) {

    getValues <- eventReactive(dataframe, {
      req(dataframe)
      # print(dataframe)
      datageh <- wrangle_data(dataframe) %>% filter(trait == toupper(traitr))
      datageh <- datageh %>% arrange(desc(combined))
      print(datageh)

      keyvals <- list("accession" = datageh$accession[1], "value" = datageh$combined[1])
      print(keyvals)
      return(keyvals)
    })


    output$toptraits <- renderInfoBox({
      vals <- getValues()
      full_names <- list("MCMDS" = "Cassava Mosaic", "HI" = "Harvest Index", "DM" = "Dry matter",
                         "SPROUT" = "Sprout", "PLTHT" = "Plant Height",
                         "DYLD" = "Dry yield", "FYLD" = "Fresh yield")
      infoBox(title = h5(vals["accession"], class = "clone-name"), value = vals["value"],
              subtitle = full_names[toupper(traitr)], icon = icon(icon1), color = color, fill = TRUE)
    })

    output$lineplot <- renderPlot({
      req(getValues())
      uytdata <- linePlot(trait_to_plot = toupper(traitr), imported_data = dataframe)
      uytdata
    }, width = 400,  height = 150)
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

shinyApp(ui  , server)
