source("hello.R")
options(ggrepel.max.overlaps = Inf)

geo_data <- function(dataset){

}


fix_listOfTop_Traits <- function(original_list, top_traits){
  filt <- function(x){
    !(x %in% top_traits)
  }
  not_in_list <- original_list[sapply(original_list, filt) == TRUE]
  length_of_top_traits <- length(top_traits)
  more_to_add <- 3 - length_of_top_traits
  if(more_to_add > 0){
    for(i in 1:more_to_add){
      top_traits[length_of_top_traits + i] <- not_in_list[i]
    }
  }
  return(top_traits)
}

wrangle_data <- function(imported_data){
  # imported_data <- read_csv("../../Visualizations/combo.csv")
  imported_data <- janitor::clean_names(imported_data)
  x <- colnames(imported_data)
  # print(x)
  ind <- which(str_detect(x,"ssion"))
  colnames(imported_data)[ind] <- "accession"
  # print(colnames(imported_data))
  # imported_data
  # dat <- janitor::clean_names(imported_data)
  # print(imported_data)
  #%>% select_if(~!any(is.na(.)))
  return(imported_data)
}


attach_locs <- function(dat){
  piv <- dat %>% pivot_longer(cols = -c(trait, accession, combined), names_to = "location", values_to = "values")
  locs <- data.frame(location = c("Ibadan", "Mokwa", "Ago-owu", "Onne", "Otobi", "Ubiaja"), lat = c(7.3775, 9.2928, 7.2519, 4.7238, 7.1079, 6.6493),
                     long = c(3.9470,5.0547,4.3258,7.1516,8.0897,6.3918))
  piv2 <- piv %>%  mutate(
    location = case_when(
      stringr::str_detect(location, pattern = regex("ag"))        ~ "Ago-owu",
      stringr::str_detect(location, pattern = regex("ib"))        ~ "Ibadan",
      stringr::str_detect(location, paste(c("mk", "mok"), collapse = '|'))        ~ "Mokwa",
      stringr::str_detect(location, pattern = regex("on"))        ~ "Onne",
      stringr::str_detect(location, pattern = regex("ot"))        ~ "Otobi",
      stringr::str_detect(location, pattern = regex("ub"))        ~ "Ubiaja",
      stringr::str_detect(location, pattern = regex("ik"))        ~ "Ikenne"
    )
  ) %>%
    mutate(
      long = case_when(
        location == "Ago-owu"      ~ locs %>% filter(location=="Ago-owu") %>% dplyr::select(long) %>% as.numeric(),
        location == "Ibadan"      ~ locs %>% filter(location=="Ibadan") %>% dplyr::select(long) %>% as.numeric(),
        location == "Mokwa"      ~ locs %>% filter(location=="Mokwa") %>% dplyr::select(long) %>% as.numeric(),
        location == "Onne"      ~ locs %>% filter(location=="Onne") %>% dplyr::select(long) %>% as.numeric(),
        location == "Otobi"      ~ locs %>% filter(location=="Otobi") %>% dplyr::select(long) %>% as.numeric(),
        location == "Ubiaja"      ~ locs %>% filter(location=="Ubiaja") %>% dplyr::select(long) %>% as.numeric()
      )
    ) %>%
    mutate(
      lat = case_when(
        location == "Ago-owu"      ~ locs %>% filter(location=="Ago-owu") %>% dplyr::select(lat) %>% as.numeric(),
        location == "Ibadan"      ~ locs %>% filter(location=="Ibadan") %>% dplyr::select(lat) %>% as.numeric(),
        location == "Mokwa"      ~ locs %>% filter(location=="Mokwa") %>% dplyr::select(lat) %>% as.numeric(),
        location == "Onne"      ~ locs %>% filter(location=="Onne") %>% dplyr::select(lat) %>% as.numeric(),
        location == "Otobi"      ~ locs %>% filter(location=="Otobi") %>% dplyr::select(lat) %>% as.numeric(),
        location == "Ubiaja"      ~ locs %>% filter(location=="Ubiaja") %>% dplyr::select(lat) %>% as.numeric()
      )
    )
  return(piv2)
}


pivot_wider_function <- function(imported_data){
  uytdata <- wrangle_data(imported_data) %>% dplyr::select(-c(long, lat))
  uytdata <- uytdata %>% pivot_wider(names_from = "location", values_from = "values")
  return(uytdata)
}

linePlot <- function(trait_to_plot, imported_data){
  # listr <- c("dyld", "fyld", "")

  # imported_data <- read_csv("../../Visualizations/combo.csv")
  # trait_to_plot <- "MCMDS"
  uytdata <- pivot_wider_function(imported_data) %>% filter(trait == trait_to_plot)
  # uytdata %>% View()
  # ?top_frac

  # class(trait_to_plot)

  if(trait_to_plot == "MCMDS"){
    uytdata_arr <- uytdata %>% arrange(combined)
  }else {
    uytdata_arr <- uytdata %>% arrange(desc(combined))
  }

  if(trait_to_plot == "SPROUT" || trait_to_plot == "MCMDS"){
    uytdatax <- uytdata_arr[1:4,]
  }else{
    uytdatax <- uytdata_arr %>% top_frac(.1, combined)
  }


  x <- uytdatax %>% pivot_longer(-c(trait, accession),
                                 names_to = "location", values_to = "values")
  full_names <- list("MCMDS" = "Cassava Mosaic", "HI" = "Harvest Index", "DM" = "Dry Matter",
                "SPROUT" = "Sprout", "DYLD" = "Dry yield", "FYLD" = "Fresh yield", "PLTHT" = "Plant Height")

  p <- x %>% ggplot(aes(location, values, group = accession, colour = accession)) +
    geom_point() +
    geom_line()  +
    theme_grey() +
    theme(legend.position = 'none',
          plot.background = element_rect(fill = "#fcfcee",colour = "#cecece")) +
          # plot.caption = element_text("Hello",face = "italics")) +
    labs(x = "Location", y = full_names[trait_to_plot]) +
    # guides(fill = "none") +
    geom_text_repel(max.overlaps = Inf, box.padding = 0.5,
                    aes(group = accession, label = if_else(location == "Mokwa",accession,""),
                        max.overlaps = 5))

  p

  return(p)
}

linePlot_environment <- function(trait_to_plot, imported_data){

  # imported_data <- read_csv("../../Visualizations/combo.csv")
  # trait_to_plot <- "FYLD"
  uytdata <- pivot_wider_function(imported_data) %>% filter(trait == trait_to_plot)
  # uytdata %>% View()

  summary(uytdata)
  location_mean <- uytdata %>% add_row(accession = "mean",trait = trait_to_plot,
                                       summarise(., across(where(is.numeric), mean)))
  # location_mean %>% View()
  # ?top_frac


  # class(trait_to_plot)

  # if(trait_to_plot == "MCMDS"){
  #   uytdata_arr <- uytdata %>% arrange(combined)
  # }else {
  #   uytdata_arr <- uytdata %>% arrange(desc(combined))
  # }
  #
  # if(trait_to_plot == "SPROUT" || trait_to_plot == "MCMDS"){
  #   uytdatax <- uytdata_arr[1:4,]
  # }else{
  #   uytdatax <- uytdata_arr %>% top_frac(.1, combined)
  # }


  overall_mean <- location_mean %>% filter(accession == "mean")

  loc_mean <- location_mean[-41,]
  uytdata_long <- loc_mean %>% pivot_longer(-c(trait, accession, combined),
                                 names_to = "location", values_to = "values") %>%
    mutate(acc_average = if_else(location == "Ibadan",overall_mean$Ibadan,
                          if_else(location == "Ago-owu", overall_mean$`Ago-owu`,
                            if_else(location == "Onne", overall_mean$Onne,
                             if_else(location == "Otobi",overall_mean$Otobi,
                              if_else(location == "Mokwa", overall_mean$Mokwa,
                                if_else(location == "Ubiaja", overall_mean$Ubiaja,overall_mean$combined)))))))
  # uytdata_long %>% View()
  uyt <- uytdata_long %>% group_by(accession) %>% arrange(desc(acc_average))


  full_names <- list("MCMDS" = "Cassava Mosaic", "HI" = "Harvest Index", "DM" = "Dry Matter",
                "SPROUT" = "Sprout", "DYLD" = "Dry yield", "FYLD" = "Fresh yield", "PLTHT" = "Plant Height")

  combined_df <- uytdata_long %>% filter(location == "combined")
  combined_Val <- as.numeric(combined_df$values)
  # x <- uytdata_long %>%  mutate(location = reorder(acc_average))
  # uyt %>% View()
  p <- uyt %>% ggplot(aes(acc_average, values)) +
    # scale_color_manual(name="values",values=c("red", "darkblue")) +
    geom_point(aes(color = accession, alpha = values), shape = 1) +
    # geom_smooth(method = "lm", se = FALSE) +
    # geom_line(aes(group = accession))  +
    theme_minimal_hgrid() +
    theme(legend.position = 'none',
          plot.background = element_rect(fill = "#fcfcee",colour = "#cecece"),
          axis.text.x = element_text(angle = 90)) +
    geom_abline(intercept = 0, slope = 1, linetype = "dotted") +
    # scale_x_discrete(limit = unique(uyt$location)) +
    scale_x_continuous(breaks = unique(uyt$acc_average), labels = unique(uyt$location)) +
          # plot.caption = element_text("Hello",face = "italics")) +
    labs(x = "Location", y = trait_to_plot)
    # coord_flip()

  # guides(fill = "none") +
    # geom_text_repel(max.overlaps = Inf, box.padding = 0.5,
    #                 aes(group = trait, label = if_else(location == "Mokwa",accession,""),
    #                     max.overlaps = 5))



  return(p)

}


calc_checkMean <- function(dataframe, checks){
  # checks <- c("IITA-TMS-IBA000070","TMEB419","TMS13F1160P0004","IITA-TMS-IBA30572","IITA-TMS-IBA980581")

  # calculate for checks average
  checks_mean <- dataframe %>%
    filter(accession %in% checks) %>%
    add_row(accession = "check_mean", summarise(., across(where(is.numeric), mean))) %>%
    filter(accession == "check_mean")

  # insert check mean into dataset
  dataframe <- bind_rows(dataframe,checks_mean)

  # dataset for percentage difference against checks average
  dataframe_checkdiff <- dataframe %>%
    dplyr::select(-index) %>%
    mutate(across(where(is.numeric), .fns = ~((./.[accession == "check_mean"]-1)*100))) %>%
    mutate(index=dataframe$index) %>%
    mutate(rank = factor(row_number()))
  return(dataframe_checkdiff)
}


get_GXE_data <- function(imported_data){
  # imported_data <- "../../Visualizations/BLUPS-UYT-40.xls"

  imported_data <- as.character(imported_data)
  mysheets_fromexcel <- list()
  # print("HITTT")
  mysheetlist <- excel_sheets(imported_data)
  # print(mysheetlist)
  # class(mysheetlist)
  mysheetlist <- lapply(mysheetlist, tolower)
  mysheetlist <- as.character(mysheetlist)
  list_of_traits <- c("dm", "dyld", "fyld", "pltht", "mcmds", "mcmdi", "sprout", "hi", "rtno", "rtwt", "shtwt")

  for (i in 1:length(mysheetlist)){
    if(mysheetlist[i]  %in% list_of_traits){
      tempdf <- read_excel(path=imported_data, sheet = i)
      mysheets_fromexcel[[i]] <- tempdf
      assign(mysheetlist[i], tempdf)
    }
  }
  list_of_excels <- mysheets_fromexcel %>% discard(is.null)

  stacked_data <- list_of_excels[1]
  for(i in 2:length(list_of_excels)){
    stacked_data <- bind_rows(stacked_data, list_of_excels[[i]])
  }
  return(stacked_data)
}

get_SI_from_BLUPS <- function(imported_data) {
  # imported_data <- "../../Visualizations/BLUPS-UYT-40.xls"
  mysheets_fromexcel <- list()
  mysheetlist <- excel_sheets(imported_data)
  # class(mysheetlist)
  mysheetlist <- lapply(mysheetlist, tolower)
  mysheetlist <- as.character(mysheetlist)
  list_of_traits <- c("sindex","index","si","selection index")

  for (i in 1:length(mysheetlist)){
    if(mysheetlist[i]  %in% list_of_traits){
      tempdf <- read_excel(path=imported_data, sheet = i)
      mysheets_fromexcel[[i]] <- tempdf
      assign(mysheetlist[i], tempdf)
    }
  }
  list_of_excels <- mysheets_fromexcel %>% discard(is.null)
  list_of_excels <- list_of_excels[[1]]
  print(class(list_of_excels))
  return(as.data.frame(list_of_excels))
}

# ?shinyWidgets::awesomeCheckbox
sindex_corrplot <- function(dataframe, color_scale){
  corr <- round(cor(dataframe[,-1], use = "pairwise.complete.obs"), 1)
  p.mat <- cor_pmat((dataframe[,-1]), use = "pairwise.complete.obs")

  corrr_plot <- ggcorrplot(corr, method = c("square"), type = c("lower"),
             ggtheme = ggplot2::theme_minimal, title = "",
             show.legend = TRUE, legend.title = "Correlation", show.diag = FALSE,
             colors = c("blue", "white", "red"), outline.color = "gray",
             hc.order = TRUE, hc.method = "complete", lab = TRUE,
             lab_col = "black", lab_size = 4, p.mat = NULL, sig.level = 0.05,
             insig = c("pch", "blank"), pch = 4, pch.col = "black",
             pch.cex = 5, tl.cex = 10, tl.col = "black", tl.srt = 45,
             digits = 2)
  return(corrr_plot)
}


sindex_heatmap <- function(dataframe, color_scale){
  u4h <- dataframe %>%
    # arrange(desc(sindex)) %>%
    column_to_rownames("accession")

  dm <- paste(u4h$dm, "dm")
  index <- paste(u4h$index, "index")

  # set the text colors
  # identify all scaled values that fall below -0.3
  ayt20.col <- scale(u4h) < -0.3
  # set all values that satisfy the condition to "white"
  ayt20.col <- gsub("TRUE", "white", ayt20.col)
  # set all values that do not satisfy the condition to "black"
  ayt20.col <- gsub("FALSE", "black", ayt20.col)
  # convert to matrix
  ayt20.col <- matrix(ayt20.col, ncol = ncol(u4h))

  ayt20.size <- scale(u4h) + 1.2


  set.seed(2016113)
  sup_heat <- superheat(u4h,
            # retain original order of rows/cols
            # pretty.order.rows = TRUE,
            # pretty.order.cols = TRUE,
            # scale the matrix columns
            scale = TRUE,
            # order the rows by selection index
            # order.rows = order(dataframe$index),

            # change the color
            # heat.col.scheme = "blue",
            # change the color (#b35806 = brown and #542788 = purple)
            heat.pal = c("red", "white", "darkgreen"),
            # Color transitions
            # heat.pal.values = c(0, 0.5, 1),
            # # color limits
            # heat.lim = c(-1, 2),
            # heat.na.col = "white", # na values
            # add row dendrogram
            row.dendrogram = TRUE,
            # add colun dendrogram
            col.dendrogram = TRUE,

            # clustering methods
            # clustering.method = "hierarchical",
            # generate column clusters
            # n.clusters.rows = 4,
            # left.label = 'variable'

            # cluster by index
            # membership.rows = fct_inorder(index)

            # plot title
            title = "Superheat for UYT 40 \n selection index",
            title.size = 5,
            # row title
            row.title = "accession",
            row.title.size = 5,
            # col title
            column.title = "traits",
            column.title.size = 5,

            # adjacent plots
            # # add selection index as a scatterplot next to the rows
            # yr = u4h$index,
            # yr.axis.name = "selection index"

            # add text matrix
            # X.text = round(as.matrix(u4h), 1),
            # X.text.col = ayt20.col,
            # #X.text.size = 4,
            # X.text.size = ayt20.size,
            # X.text.angle = 12,

            # change the size of the labels
            left.label.size = 0.3,
            bottom.label.size = 0.24,

            # change the size of the label text
            left.label.text.size = 6,
            bottom.label.text.size = 6,

            # # change the color of the labels
            # left.label.col = "white",
            # bottom.label.col = c("#b3e2cd","#fdcdac","#e5d8bd"),

            # change the color of the label text
            left.label.text.col = "black",
            bottom.label.text.col = "black",

            # change the angle of the label text
            bottom.label.text.angle = 90,
            left.label.text.alignment = "center",
            bottom.label.text.alignment = "center",

            # # remove the grid
            # grid.hline = FALSE,
            # grid.vline = FALSE,

            # # change the grid color and size
            # grid.hline.col = "white",
            # grid.vline.col = "white",
            # grid.hline.size = 2,
            # grid.vline.size = 2,

            # # cluster the heatmap
            # n.clusters.rows = 3,
            # left.label = "variable",
            # n.clusters.cols = 2,
            # bottom.label = "variable"

            # # remove the legend
            # legend = FALSE,

            #  # make the legend bigger
            # legend.height = 0.5,
            # legend.width = 2,
            # legend.text.size = 20,

            # # cluster by gears
            # membership.rows = index,
            #
            # # place each variable in its own cluster
            # membership.cols = 1:ncol(u4h),
            # bottom.label = "variable",
            #
            # # smooth the heatmap within clusters
            # smooth.heat = TRUE
  )
  return(sup_heat)
}

sup_heat_corr <- function(dataframe, checks){
  u4h <- dataframe %>%
    # arrange(desc(sindex)) %>%
    column_to_rownames("accession")
  print_col_checks <- u4h %>%
    mutate(row_number = row_number()) %>%
    .[checks,] #%>%
  #  select(row_number) %>%
  #  as.vector()


  point.col <- rep("wheat3", nrow(u4h))
  # color checks
  point.col[print_col_checks[c(1:ncol(print_col_checks)),13]] <- "red"
  sup_heat_corr <- superheat(dplyr::select(u4h, -index),
            # scale the variables/columns
            scale = T,
            # order the rows by selection index
            order.rows = order(dataframe$index),

            # # add selection index as a scatterplot next to the rows
            # yr = u4h$sindex,
            # yr.axis.name = "selection index",
            # # change the color of the points
            # yr.obs.col = point.col,
            # yr.point.size = 4

            # # add selection index as a line plot next to the rows
            # yr =u4h$sindex,
            # yr.axis.name = "selection index",
            # yr.plot.type = "line",
            # # order the rows by mpg
            # order.rows = order(u4h$sindex)

            # # loess curve
            # # add selection index as a smoothed line plot next to the rows
            # yr = u4h$sindex,
            # yr.axis.name = "selection index",
            # yr.plot.type = "smooth",
            # # change the line thickness and color
            # yr.line.size = 4,
            # yr.line.col = "red4",
            # # order the rows by b
            # order.rows = order(u4h$b)

            # # linear regression line
            # # add selection index as a smoothed line plot  next to the rows
            # yr = u4h$sindex,
            # yr.axis.name = "selection index",
            # yr.plot.type = "smooth",
            # smoothing.method = "lm",
            # # change the line thickness and color
            # yr.line.size = 4,
            # yr.line.col = "plum4",
            # # order the rows by b
            # order.rows = order(u4h$b)

            # # scatterplot with connecting line plot
            # # add selection index as a scatter line plot next to the rows
            # yr = u4h$sindex,
            # yr.axis.name = "selection index",
            # yr.plot.type = "scatterline",
            # # change the line color
            # yr.line.col = "tomato3",
            # yr.obs.col = rep("orange", nrow(u4h)),
            # yr.point.size = 4,
            # # order the rows by b
            # order.rows = order(u4h$b)

            # # scatterplot with smooth line
            # # add selection index as a scatter smoothed plot next to the rows
            # yr = u4h$sindex,
            # yr.axis.name = "selection index",
            # yr.plot.type = "scattersmooth",
            # # change the line color
            # yr.line.col = "tomato3",
            # yr.obs.col = rep("orange", nrow(u4h)),
            # # order the rows by b
            # order.rows = order(u4h$b)

            # # add selection index as a barplot next to the rows
            # yr = u4h$sindex,
            # yr.axis.name = "selection index",
            # yr.plot.type = "bar",
            # # set bar colors
            # yr.bar.col = "black",
            # yr.cluster.col = c("beige", "white", "beige")

            # change the size of the label text
            left.label.text.size = 6,
            bottom.label.text.size = 6,

            # change the size of the labels
            left.label.size = 0.3,
            # bottom.label.size = 0.24,

            # add selection index as a scatterplot next to the rows
            yr = u4h$index,
            yr.axis.name = "selection index",
            yr.plot.size = 0.2,
            # yr.lim = c(0, 60),
            # change the color of the points
            yr.obs.col = point.col,
            yr.point.size = 4,
            # add correlation between each variable and selection index
            yt = cor(u4h)[-12,"index"],
            yt.plot.type = "bar",
            yt.axis.name = "Correlation with \n selection index",
            #yt.lim = c(-1.5, 1)
            # yt.axis.size = 14,
            yt.axis.name.size = 8,
            yt.plot.size = 0.35
  )
  return(sup_heat_corr)
}

barplot_checkdiff <- function(import_data){
  print(import_data)
  barplot_checkdiff <- import_data %>%
    # filter(accession == acc_names[i]) %>%
    dplyr::select(-c(rtwt, mcmds, mcmdi)) %>%
    dplyr::select(accession,fyld,dyld,dm,shtwt,everything()) %>%
    pivot_longer(-c(accession,rank,index), names_to = "traits", values_to = "values") %>%

    # group_by(traits) %>%
    # top_frac(.1,sindex) %>%
    #ungroup() %>%

    # mutate(traits = fct_reorder(traits, values)) %>%
    ggplot(aes(fct_inorder(traits), values)) +
    geom_col(aes(fill = values), stat = "identity", color=alpha("black",.3)) +
    # facet_grid( vars(fct_inorder(traits)), vars(accession), scales = "free", space = "free") +
    # geom_hline(aes(yintercept = 0, alpha = 0.1, color="blue")) +
    geom_text(aes(label= round(values,2)), position = position_stack(vjust = 0.5), size = 7) +
    # geom_text(aes(label=round(values, 2)), vjust=0) +
    scale_fill_gradient2(low = "red", mid = "white", high = "darkgreen", midpoint= 0) +
    scale_y_continuous(limits = c(-100,100), labels = function(x) paste0(x, "%")) +
    facet_wrap(~accession, ncol = 1, strip.position = "right", scales = "free") +
    labs(x = "Traits", y = "Percentage Difference", title = "Difference of Accessions X Checks") +
    theme_bw(base_size = 20)
  return(barplot_checkdiff)
}

