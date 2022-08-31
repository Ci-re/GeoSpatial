lev1 <- st_read("NGA_population_v1_2_admin/NGA_population_v1_2_admin_level2_boundaries.shp")

render_maps <- function(dataframe, checks){

  if(input$trait_select == "DYLD"){
    trait_sel <- "Dry yield"
  }else if(input$trait_select == "FYLD"){
    trait_sel <- "Fresh Yield"
  }else if(input$trait_select == "DM"){
    trait_sel <- "Dry matter"
  }else if(input$trait_select == "PLTHT"){
    trait_sel <- "Plant height"
  }else if(input$trait_select == "MCMDS"){
    trait_sel <- "Cassava mosaic"
  }else {
    trait_sel <- input$trait_select
  }

  tf <- ggplot() +
    # geom_sf(data = lev1, show.legend = TRUE) +
    geom_sf(data = lev1, colour = "white", fill = "grey", size = .1) +
    geom_text(data = piv2, aes(x = long, y = lat, label = location), nudge_x = .2, nudge_y = .3, check_overlap = FALSE) +
    geom_point(data = piv2, mapping = aes(x = long, y = lat, size = values, fill = values, color = category,
                                          text = paste0("<b> trait: ",trait,"</b> \n",
                                                        "<b> accession: ", accession, "</b> \n",
                                                        "<b>",trait_sel ,":",values,"</b>")))+
    # scale_color_viridis_c() +
    scale_fill_gradient2(low = "red", midpoint = 6.357, mid = "yellow", high = "green") +
    scale_color_manual(values = c("darkblue", "blue")) +
    # coord_sf(xlim = c(2, 6), ylim = c(6, 10), expand = FALSE) +
    # geom_sf_text(data = lev1, aes(label = statename)) +
    # theme(panel.grid.major = element_line(color = gray(0.5), linetype = "dashed",
    #                                       size = 0.5), panel.background = element_rect(fill = "aliceblue")) +
    facet_wrap(~fct_inorder(accession), ncol = 2) +
    theme_gray()
  return(plotly::ggplotly(tf, tooltip = "text"))
}
