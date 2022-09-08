

visualizer_UI <- function(id){
  ns <- NS(id)
  tagList(
    chooseSliderSkin("Flat", color = "black"),
    tabBox(
      width = 12,
      title = "Hello World",
      # The id lets us use input$tabset1 on the server to find the current tab
      id = "GxSindex",
      tabPanel(title = "Genotype X sindex", icon = icon("bar-chart"), fluidRow(
        box(width = 12,
          column(4, withSpinner(uiOutput(ns("sort_sindex")))),
          column(4, withSpinner(uiOutput(ns("select_top")))),
          column(4, withSpinner(uiOutput(ns("color_scales")))),
        )
      ), fluidRow(
        box(width = 6, withSpinner(DT::dataTableOutput(ns("raw_distribution")))),
        box(width = 6, withSpinner(DT::dataTableOutput(ns("check_mean_distribution")))),
        box(width = 9, withSpinner(plotlyOutput(ns("correlation"), height = "800px", width = "100%"))),
        box(width = 3),
        box(width = 9, withSpinner(plotOutput(ns("heatmaps"), width = "100%", height = "1200px"))),
        box(width = 3, column(3, uiOutput(ns("heatmap_hint")))),
        box(width = 9, withSpinner(plotOutput(ns("check_diff_plot"), width = "100%", height = "1500px"))),
        box(width = 3, uiOutput(ns("top_frac")))
      )),
      tabPanel(title = "Genotype X Env", icon = icon("bar-chart"), fluidRow(
        box(width = 12,
          column(4, uiOutput(ns("sort_combined"))),
          column(4, uiOutput(ns("select_top_env"))),
          column(4, uiOutput(ns("trait"))),
        )
      ), fluidRow(
        box(width = 6, withSpinner(DT::dataTableOutput(ns("raw_distribution_env")))),
        box(width = 6, withSpinner(DT::dataTableOutput(ns("check_mean_distribution_env")))),

        box(width = 9, plotlyOutput(ns("correlation_env"), width = "100%", height = "800px")),
        box(width = 3),
        box(width = 9, withSpinner(plotOutput(ns("heatmaps_env"), width = "100%", height = "1200px"))),
        box(width = 3, withSpinner(uiOutput(ns("corr_heat_env")))),
        box(width = 9, withSpinner(plotOutput(ns("check_diff_plot_env"), width = "100%", height = "1500px"))),
        box(width = 3, uiOutput(ns("top_frac_env")))
      )),

      tabPanel(title = "Geographical Vix", icon = icon("chart"), fluidRow(
        box(width = 12,
          column(3, uiOutput(ns("select_trait"))),
          column(3, uiOutput(ns("select_accession"))),
          column(3, uiOutput(ns("select_checks"))),
          column(3, uiOutput(ns("weather")))
        )
      ), fluidRow(
        box(
          uiOutput(ns("check_difference_toggler")),
          width = 12, withSpinner(plotlyOutput(ns("accession_map"), width = "100%", height = "800px"))),
        # box(width = 3, ),
        # box(width = 12, plotlyOutput(ns("check_map"))),
        box(width = 9, withSpinner(leafletOutput(outputId = ns("live_map"), width = "100%", "900px"))),
        box(width = 3, radioButtons(inputId = ns("weather2"), label = "Real time view:",
                                     c("Rain" = "rainClassic",
                                      "Temperature" = "temperature",
                                      "Precipitation" = "precipitationClassic",
                                      "Clouds" = "cloudsClassic",
                                      "Pressure" = "pressure",
                                      "Wind" = "wind"),
                                    selected = "precipitationClassic")
            )
      )),

      tabPanel(title = "Trait X Env", icon = icon("chart"), fluidRow(
        box(width = 12,
            column(6, withSpinner(uiOutput(ns("select_accessions_trt")))),
            column(6, withSpinner(uiOutput(ns("select_checks_trt"))))
        )
      ), fluidRow(
        box(width = 12, withSpinner(plotlyOutput(ns("tile_plot"), width = "100%", height = "1000px"))),
      ))
    )
  )
}

visualizer_SERVER <- function(id, sindex_dataframe, combined_dataframe, checks){
  moduleServer(id, function(input, output, session) {

    ################################# SINDEX PART #########################################
    output$sort_sindex <- renderUI({
      pickerInput(
        inputId = session$ns("sort_sindex"),
        label = "Sort data",
        choices = c("Best to Good", "Good to Best"),
        selected = "Best to Worse",
        multiple = FALSE
      )
    })

    output$select_top <- renderUI({
      max_obs <- length(sindex_dataframe$accession)
      mid_obs <- median(1:max_obs)

      sliderInput(
        inputId = session$ns("select_top"),
        label = "Select top.." ,
        min = 1,
        step = 1,
        max = max_obs,
        value = mid_obs
      )
    })

    output$color_scales <- renderUI({
      pickerInput(
        inputId = session$ns("color_scales"),
        label = "Select color scale",
        choices = c("Red to Blue", "Red to Green", "default"),
        selected = "default",
        multiple = FALSE
      )
    })

    output$heatmap_hint <- renderUI({
      prettyCheckbox(
        inputId = session$ns("corr_heat"), label = "Switch plot",
        status = "success", outline = TRUE,value = FALSE,
      )
    })

    output$top_frac <- renderUI({
      max_obs <- length(sindex_dataframe$accession)
      mid_obs <- median(1:max_obs)

      sliderInput(
        inputId = session$ns("top_frac"),
        label = "Select topfrac.." ,
        min = 1,
        step = 1,
        max = max_obs,
        value = 4,
      )
    })


    output$raw_distribution <- DT::renderDataTable({
      req(input$select_top)
      dataframe <- sindex_dataframe %>% dplyr::select(-selected)
      DT::datatable(dataframe[1:input$select_top,], options = list(pageLength = 5, scrollX = TRUE, scrollY = TRUE))
    })

    output$check_mean_distribution <- DT::renderDataTable({
      req(input$select_top)
      dat <- sindex_dataframe %>% dplyr::select(-selected)
      dataframe <- calc_checkMean(dataframe = dat,checks = checks)
      DT::datatable(dataframe[1:input$select_top,], options = list(pageLength = 5, scrollX = TRUE, scrollY = TRUE))
    })

    output$correlation <- renderPlotly({
      req(input$select_top)
      dat <- sindex_dataframe %>% dplyr::select(-selected)
      dat <- sindex_corrplot(dataframe = dat[1:input$select_top,])
      ggplotly(dat)
    })

    output$heatmaps <- renderPlot({
      req(input$select_top)
      # print(class(input$corr_heat))
      if(input$corr_heat == TRUE){
        dat <- sindex_dataframe %>% dplyr::select(-selected)
        dat <- sup_heat_corr(dataframe = dat[1:input$select_top,], checks)
        return(dat)
      }else{
        dat <- sindex_dataframe %>% dplyr::select(-selected)
        dat <- sindex_heatmap(dataframe = dat[1:input$select_top,])
        return(dat)
      }
    })

    output$check_diff_plot <- renderPlot({
      req(input$select_top)
      dat <- sindex_dataframe %>% dplyr::select(-selected)
      dataframe <- calc_checkMean(dataframe = dat,checks = checks)
      barplot <- barplot_checkdiff(import_data = dataframe[1:input$top_frac,])
      return(barplot)
    })

############################################## ENVIRONMENT PART ############################################
    output$sort_combined <- renderUI({
      pickerInput(
        inputId = session$ns("sort_combined"),
        label = "Sort data...",
        choices = c("Best to Good", "Good to Best"),
        selected = "Best to Worse",
        multiple = FALSE
      )
    })

    output$select_top_env <- renderUI({
      max_obs <- length(unique(combined_dataframe$accession))
      mid_obs <- median(1:max_obs)
      chooseSliderSkin("Flat", color = "black")
      sliderInput(
        inputId = session$ns("select_top_env"),
        label = "Select top..." ,
        min = 1,
        max = max_obs,
        value = mid_obs,
        step = 1
      )
    })

    output$trait <- renderUI({
      pickerInput(
        inputId = session$ns("trait"),
        label = "Select trait",
        choices = c("Dry yield" = "dyld", "Fresh Yield" = "fyld", "Dry Matter" = "dm",
                    "Plant Height" = "pltht", "Sprout" = "sprout", "Mosaic" = "mcmds",
                    "Harvest Index" = "hi"),
        selected = "dyld",
        multiple = FALSE
      )
    })

    output$top_frac_env <- renderUI({
      max_obs <- length(sindex_dataframe$accession)
      mid_obs <- median(1:max_obs)

      sliderInput(
        inputId = session$ns("top_frac_env"),
        label = "Select topfrac.." ,
        min = 1,
        step = 1,
        max = max_obs,
        value = 10,
      )
    })

    output$corr_heat_env <- renderUI({
      prettyCheckbox(
        inputId = session$ns("corr_heat_env"), label = "Switch plot",
        status = "success", outline = TRUE,value = FALSE,
      )
    })


    output$raw_distribution_env <- DT::renderDataTable({
      req(input$trait)
      # print(input$trait)
      # print(combined_dataframe)
      dat <- pivot_wider_function(combined_dataframe)
      dataframe <- dat %>% filter(trait == toupper(input$trait))
      # print(dataframe)
      DT::datatable(dataframe[1:input$select_top_env,], options = list(pageLength = 5, scrollX = TRUE, scrollY = TRUE))
    })

    output$check_mean_distribution_env <- DT::renderDataTable({
      req(input$trait)
      # print(input$trait)
      # print(combined_dataframe)
      dat <- pivot_wider_function(combined_dataframe)
      dataframe <- dat %>% filter(trait == toupper(input$trait))
      datr <- calculate_checkmean(dataframe, checks = checks)
      # print(dataframe)
      DT::datatable(datr[1:input$select_top_env,], options = list(pageLength = 5, scrollX = TRUE, scrollY = TRUE))
    })

    output$correlation_env <- renderPlotly({
      req(input$trait)
      dataframe <- combined_dataframe %>% filter(trait == toupper(input$trait))
      not_all_na <- function(x) any(!is.na(x))
      dat <- pivot_wider_function(dataframe) %>% dplyr::select(where(not_all_na))
      corr_plt <- env_correlation(dat)
      ggplotly(corr_plt)
    })

    output$heatmaps_env <- renderPlot({
      req(input$trait)
      dat <- pivot_wider_function(combined_dataframe)
      not_all_na <- function(x) any(!is.na(x))
      dataframe <- dat %>% filter(trait == toupper(input$trait)) %>% dplyr::select(-trait) %>%
        dplyr::select(where(not_all_na))
      if(input$corr_heat_env == TRUE){
        heat_plt <- env_heatmap(dataframe[1:input$select_top_env,])
        return(heat_plt)
      }else{
        heat_plt <- env_sup_heat_corr(dataframe[1:input$select_top_env,], checks)
        return(heat_plt)
      }
    })

    output$check_diff_plot_env <- renderPlot({
      req(input$trait)
      dat <- pivot_wider_function(combined_dataframe)
      not_all_na <- function(x) any(!is.na(x))


      dataframe <- dat %>% filter(trait == toupper(input$trait)) %>% dplyr::select(where(not_all_na))

      dat <- calculate_checkmean(dataframe, checks = checks)
      env_barplot_checkdiff <- env_barplot_checkdiff(dat[1:input$top_frac_env,])
      return(env_barplot_checkdiff)
    })


    ##################################### GEOGRAPHICAL PARTITION ########################################

    output$select_trait <- renderUI({
      pickerInput(
        inputId = session$ns("select_trait"),
        label = "Select trait",
        choices = c("Dry matter" = "dm", "Dry Yield" = "dyld",
                    "Fresh yield" = "fyld", "Sprout" = "sprout",
                    "Mosaic" = "mcmds", "Plant height" = "pltht"),
        selected = "dm",
        multiple = FALSE
      )
    })

    output$select_checks <- renderUI({
      req(sindex_dataframe)
      pickerInput(
        inputId = session$ns("select_check"),
        label = "Checks..",
        choices = unique(combined_dataframe$accession),
        selected = combined_dataframe$accession[1:3],
        multiple = TRUE,
        options = pickerOptions(
          liveSearch = TRUE,
          style = "btn-primary",
          `action-box` = TRUE,
          size = 10
        )
      )
    })
    output$select_accession <- renderUI({
      req(sindex_dataframe)
      pickerInput(
        inputId = session$ns("select_accession"),
        label = "Genotype..",
        choices = unique(combined_dataframe$accession),
        selected = combined_dataframe$accession[1],
        multiple = TRUE,
        options = pickerOptions(
          liveSearch = TRUE,
          style = "btn-primary",
          `action-box` = TRUE,
          size = 10
        )
      )
    })
    output$weather <- renderUI({
      pickerInput(
        inputId = session$ns("weather"),
        label = "Weather..",
        choices = c("Rainfall", "Temperature"),
        selected = "Rainfall",
        multiple = FALSE
      )
    })

    output$check_difference_toggler <- renderUI({
      prettyCheckbox(
        inputId = session$ns("check_difference_toggler"),
        label = "Toggle check-difference",
        status = "info", outline = TRUE,value = FALSE,
      )
    })

    output$accession_map <- renderPlotly({
      req(input$select_accession)
      req(input$select_check)
      req(input$weather)

      genotype <- input$select_accession
      weather_data <- input$weather
      checks <- input$select_check
      trait <- input$select_trait
      switchs <- input$check_difference_toggler
      rendered_map <- render_maps(dataframe = combined_dataframe, checks = checks, trait = trait,
                                  accession = genotype, weather = weather_data, switch = switchs)
      ggplotly(rendered_map)
    })

    output$live_map <- renderLeaflet({
      req(input$select_accession)
      genotype <- input$select_accession
      weather_data <- input$weather2
      checks <- input$select_check
      trait <- input$select_trait
      switchs <- input$check_difference_toggler
      render_leaflet_map <- render_leaflet_maps(dataframe = combined_dataframe, checks = checks, trait = trait,
                                  accession = genotype, weather = weather_data, switch = switchs)
      return(render_leaflet_map)
    })

    ################################# GGTiles ##########################################3
    output$select_accessions_trt <- renderUI({
      req(combined_dataframe)
      pickerInput(
        inputId = session$ns("select_accession_trt"),
        label = "Genotype..",
        choices = unique(combined_dataframe$accession),
        selected = combined_dataframe$accession[1],
        multiple = TRUE,
        options = pickerOptions(
          liveSearch = TRUE,
          style = "btn-primary",
          `action-box` = TRUE,
          size = 10
        )
      )
    })

    output$select_checks_trt <- renderUI({
      req(combined_dataframe)
      # sorted_combined <- combined_dataframe %>% arrange(desc(combined))
      print(combined_dataframe)
      pickerInput(
        inputId = session$ns("select_checks_trt"),
        label = "Checks..",
        choices = unique(combined_dataframe$accession),
        selected = combined_dataframe$accession[1],
        multiple = TRUE,
        options = pickerOptions(
          liveSearch = TRUE,
          style = "btn-primary",
          `action-box` = TRUE,
          size = 10
        )
      )
    })

    output$tile_plot <- renderPlotly({
      req(input$select_accession_trt)
      checksr <- input$select_checks_trt
      accession <- input$select_accession_trt
      combined_diff <- general_check_diff(combined_dataframe, checksr)
      plt <- tilesplot_trait_env(combined_diff, accession)
      return(ggplotly(plt))
    })
  })
}


# shinyApp(
#   ui = dashboardPage(
#     dashboardHeader(title = "tabBoxes"),
#     dashboardSidebar(),
#     dashboardBody(
#       includeCSS("style.css"),
#       visualizer_UI("viz")
#     )
#   ),
#   server = function(input, output) {
#     # The currently selected tab from the first box
#     visualizer_SERVER("viz")
#   }
# )
