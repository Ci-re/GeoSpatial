source("hello.R")
source("_functions.R")
source("_2function.R")
source("options_module.R")


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
          column(4, uiOutput(ns("sort_sindex"))),
          column(4, uiOutput(ns("select_top"))),
          column(4, uiOutput(ns("color_scales"))),
        )
      ), fluidRow(
        box(width = 6, DT::dataTableOutput(ns("raw_distribution"))),
        box(width = 6, DT::dataTableOutput(ns("check_mean_distribution"))),
        box(width = 9, plotlyOutput(ns("correlation"), height = "800px", width = "100%")),
        box(width = 3),
        box(width = 9, plotOutput(ns("heatmaps"), width = "100%", height = "1200px")),
        box(width = 3, column(3, uiOutput(ns("heatmap_hint")))),
        box(width = 9, plotOutput(ns("check_diff_plot"), width = "100%", height = "1500px")),
        box(width = 3, uiOutput(ns("top_frac")))
      )),
      tabPanel(title = "Genotype X Env", icon = icon("bar-chart"), fluidRow(
        box(width = 12,
          column(4, uiOutput(ns("sort_combined"))),
          column(4, uiOutput(ns("select_top_env"))),
          column(4, uiOutput(ns("trait"))),
        )
      ), fluidRow(
        box(width = 6, DT::dataTableOutput(ns("raw_distribution_env"))),
        box(width = 6, DT::dataTableOutput(ns("check_mean_distribution_env"))),

        box(width = 9, plotlyOutput(ns("correlation_env"), width = "100%", height = "800px")),
        box(width = 3),
        box(width = 9, plotOutput(ns("heatmaps_env"), width = "100%", height = "1200px")),
        box(width = 3, uiOutput(ns("corr_heat_env"))),
        box(width = 9, plotOutput(ns("check_diff_plot_env"), width = "100%", height = "1500px")),
        box(width = 3, uiOutput(ns("top_frac_env")))
      )),

      tabPanel(title = "Geographical Vix", icon = icon("chart"), fluidRow(
        box(width = 12,
          column(3, uiOutput(ns("select_trait"))),
          column(3, uiOutput(ns("custom_check"))),
          column(3, uiOutput(ns("select_accession"))),
          column(3, uiOutput(ns("hint_map")))
        )
      ), fluidRow(
        column(6, box(width = 12, plotlyOutput(ns("accession_map")))),
        column(6, box(width = 12, plotlyOutput(ns("check_map")))),
        leafletOutput(ns("live_map"))
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
      dataframe <- sindex_dataframe %>% select(-selected)
      DT::datatable(dataframe[1:input$select_top,], options = list(pageLength = 5, scrollX = TRUE, scrollY = TRUE))
    })

    output$check_mean_distribution <- DT::renderDataTable({
      req(input$select_top)
      dat <- sindex_dataframe %>% select(-selected)
      dataframe <- calc_checkMean(dataframe = dat,checks = checks)
      DT::datatable(dataframe[1:input$select_top,], options = list(pageLength = 5, scrollX = TRUE, scrollY = TRUE))
    })

    output$correlation <- renderPlotly({
      req(input$select_top)
      dat <- sindex_dataframe %>% select(-selected)
      dat <- sindex_corrplot(dataframe = dat[1:input$select_top,])
      ggplotly(dat)
    })

    output$heatmaps <- renderPlot({
      req(input$select_top)
      # print(class(input$corr_heat))
      if(input$corr_heat == TRUE){
        dat <- sindex_dataframe %>% select(-selected)
        dat <- sup_heat_corr(dataframe = dat[1:input$select_top,], checks)
        return(dat)
      }else{
        dat <- sindex_dataframe %>% select(-selected)
        dat <- sindex_heatmap(dataframe = dat[1:input$select_top,])
        return(dat)
      }
    })

    output$check_diff_plot <- renderPlot({
      req(input$select_top)
      dat <- sindex_dataframe %>% select(-selected)
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
        value = 4,
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
      dat <- pivot_wider_function(dataframe) %>% select(where(not_all_na))
      corr_plt <- env_correlation(dat)
      ggplotly(corr_plt)
    })

    output$heatmaps_env <- renderPlot({
      req(input$trait)
      dat <- pivot_wider_function(combined_dataframe)
      not_all_na <- function(x) any(!is.na(x))
      dataframe <- dat %>% filter(trait == toupper(input$trait)) %>% select(-trait) %>%
        select(where(not_all_na))
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


      dataframe <- dat %>% filter(trait == toupper(input$trait)) %>% select(where(not_all_na))

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
        selected = "default",
        multiple = FALSE
      )
    })

    output$custom_check <- renderUI({
      req(sindex_dataframe)
      pickerInput(
        inputId = session$ns("custom_check"),
        label = "Select checks",
        choices = unique(sindex_dataframe$accession),
        selected = "default",
        multiple = TRUE
      )
    })
    output$select_accession <- renderUI({
      req(sindex_dataframe)
      pickerInput(
        inputId = session$ns("select_accession"),
        label = "Select checks",
        choices = unique(sindex_dataframe$accession),
        selected = "default",
        multiple = TRUE
      )
    })
    output$hint_map <- renderUI({
      actionButton(
        inputId = session$ns("hint_map"),
        label = "",
        icon = icon("circle-question")
      )
    })

    output$loc_map <- renderPlotly({
      req(input$acc_select)
      selected <- input$acc_select
      trait_sel <- ""

      piv2 <- piv2 %>% filter(accession %in% selected)






      if(input$switch == FALSE){

      } else {
        checks <- input$checks_select
        dat <- geo_data() %>% janitor::clean_names() %>% arrange(desc(combined))
        # dat %>% View()

        if(length(checks) >= 0){
          piv2_difference <- calc_checkmean(dat)
          piv2_difference <- piv2_difference %>% filter(accession %in% selected)

          tf <- ggplot() +
            # geom_sf(data = lev1, show.legend = TRUE) +

            geom_sf(data = lev1, colour = "white", fill = "grey", size = .1) +
            geom_text(data = piv2_difference, aes(x = long, y = lat, label = location),
                      nudge_x = .2, nudge_y = .3, check_overlap = FALSE) +
            geom_point(data = piv2_difference, mapping = aes(x = long, y = lat, size = values, fill = values, color = category,
                                                             text = paste0("<b> trait: ",trait,"</b> \n",
                                                                           "<b> accession: ", accession, "</b> \n",
                                                                           "<b>",trait_sel ,":" ,values,"</b>")))+
            # scale_color_viridis_c() +
            scale_fill_gradient2(low = "red", midpoint = 0, mid = "yellow", high = "green") +
            scale_color_manual(values = c("darkblue", "blue")) +
            # coord_sf(xlim = c(2, 6), ylim = c(6, 10), expand = FALSE) +
            # geom_sf_text(data = lev1, aes(label = statename)) +
            # theme(panel.grid.major = element_line(color = gray(0.5), linetype = "dashed",
            #                                       size = 0.5), panel.background = element_rect(fill = "aliceblue")) +
            facet_wrap(~fct_inorder(accession), ncol = 2) +
            theme_gray()
          return(plotly::ggplotly(tf, tooltip = "text"))
        }
      }
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
