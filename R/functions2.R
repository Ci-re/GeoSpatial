
source("hello.R")

##### calculate check mean for combined data

calculate_checkmean <- function(dataframe, checks){
  # checks <- c("IITA-TMS-IBA000070","TMEB419","TMS13F1160P0004","IITA-TMS-IBA30572","IITA-TMS-IBA980581")
  # dataframe <- read.csv("../../Visualizations/combo.csv")

  dataframe <- dataframe %>% janitor::clean_names()
  # calculate for checks average
  checks_mean <- dataframe %>%
    filter(accession %in% checks) %>%
    add_row(accession = "check_mean", summarise(., across(where(is.numeric), mean))) %>%
    filter(accession == "check_mean")
checks_mean
  # insert check mean into dataset
  dataframe <- bind_rows(dataframe,checks_mean)


  # dataset for percentage difference against checks average
  dataframe_checkdiff <- dataframe %>%
    dplyr::select(-combined) %>%
    mutate(across(where(is.numeric), .fns = ~((./.[accession == "check_mean"]-1)*100))) %>%
    mutate(combined=dataframe$combined)
    # mutate(rank = factor(row_number()))
  return(dataframe_checkdiff)
}


env_correlation <- function(dataframe){
  # x <- read.csv("../../Visualizations/combo.csv") %>% janitor::clean_names()
  #
  # dataframe <- x %>% filter(trait == "PLTHT") %>% select(where(not_all_na))
  dataframe <- dataframe %>% dplyr::select(where(is.numeric))
  corr <- round(cor(dataframe, use = "pairwise.complete.obs"), 1)
  p.mat <- cor_pmat(dataframe, use = "pairwise.complete.obs")

  env_corr <- ggcorrplot(corr, method = c("square"), type = c("lower"),
             ggtheme = ggplot2::theme_dark(), title = "",
             show.legend = TRUE, legend.title = "Correlation", show.diag = FALSE,
             colors = c("blue", "white", "red"), outline.color = "gray",
             hc.order = TRUE, hc.method = "complete", lab = TRUE,
             lab_col = "black", lab_size = 4, p.mat = NULL, sig.level = 0.05,
             insig = c("pch", "blank"), pch = 4, pch.col = "black",
             pch.cex = 5, tl.cex = 10, tl.col = "black", tl.srt = 45,
             digits = 2)
  return(env_corr)

}

env_heatmap <- function(dataframe){
  u4h <- dataframe %>%
    # arrange(desc(sindex)) %>%
    column_to_rownames("accession")

  # dm <- paste(u4h$dm, "dm")
  combined <- paste(u4h$combined, "combined")

  # set the text colors
  # identify all scaled values that fall below -0.3
  # ayt20.col <- scale(u4h) < -0.3
  # set all values that satisfy the condition to "white"
  # ayt20.col <- gsub("TRUE", "white", ayt20.col)
  # set all values that do not satisfy the condition to "black"
  # ayt20.col <- gsub("FALSE", "black", ayt20.col)
  # convert to matrix
  # ayt20.col <- matrix(ayt20.col, ncol = ncol(u4h))

  # ayt20.size <- scale(u4h) + 1.2


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
                        title = "Superheat for UYT 40 \n Environment",
                        title.size = 5,
                        # row title
                        row.title = "accession",
                        row.title.size = 5,
                        # col title
                        column.title = "Environments",
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

env_sup_heat_corr <- function(dataframe, checks){
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
  point.col[print_col_checks[c(1:ncol(print_col_checks)),length(colnames(dataframe))]] <- "red"
  sup_heat_corr <- superheat(dplyr::select(u4h, -combined),
                             # scale the variables/columns
                             scale = T,
                             # order the rows by selection index
                             order.rows = order(dataframe$combined),

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
                             yr = u4h$combined,
                             yr.axis.name = "Combined",
                             yr.plot.size = 0.2,
                             # yr.lim = c(0, 60),
                             # change the color of the points
                             yr.obs.col = point.col,
                             yr.point.size = 4,
                             # add correlation between each variable and selection index
                             yt = cor(u4h)[-(length(colnames(u4h))),"combined"],
                             yt.plot.type = "bar",
                             yt.axis.name = "Correlation with \n Combined Environment",
                             #yt.lim = c(-1.5, 1)
                             # yt.axis.size = 14,
                             yt.axis.name.size = 8,
                             yt.plot.size = 0.35
  )
  return(sup_heat_corr)
}

env_barplot_checkdiff <- function(import_data, checks){
  barplot_checkdiff <- import_data %>%
    # filter(accession == acc_names[i]) %>%
    dplyr::select(-trait) %>%
    dplyr::select(accession,everything()) %>%
    pivot_longer(-c(accession, combined), names_to = "traits", values_to = "values") %>%
    arrange(desc(combined)) %>%

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
    labs(x = "Location", y = "Percentage Difference", title = "Difference of Accessions X Checks") +
    theme_bw(base_size = 20)
  return(barplot_checkdiff)
}

general_check_diff <- function(dataframe, checks){
  # dataframe <- read_excel("../Visualizations/BLUPS-UYT-40.xls")
  # print(checks)
  dataframe <- pivot_wider_function(dataframe)
  # print(dataframe)
  combined_check_mean <- dataframe %>%
    filter(accession %in% checks) %>%
    group_by(trait) %>%
    summarise(across(where(is.numeric),.fns = ~ mean(.,na.rm =T))) %>%
    # mutate(category = "check_mean") %>%
    # unite("trait",trait,category) %>%
    mutate(accession = "check_mean", .after = trait)

  combined_trait_checkmean <- bind_rows(dataframe ,combined_check_mean)
  traits_all2 <- combined_trait_checkmean %>%
    # select(-c(trait,accession)) %>%
    group_by(trait) %>%
    mutate(across(where(is.numeric), .fns = ~((./.[accession == "check_mean"]-1)*100)))
  trt <- traits_all2 %>% janitor::clean_names() %>% attach_locs() %>% janitor::clean_names()
  print(trt)
  return(trt)
}

tilesplot_trait_env <- function(dataframe, accessions){
  # print(dataframe)
  plt <- dataframe %>%
    filter(
      accession  %in%  accessions,
      trait  %in%  c("DYLD","FYLD","DM","PLTHT","SHTWT")
    ) %>%
    count(accession, location, trait, combined, values) %>%
    ggplot(mapping = aes(x = location, y = reorder(trait,combined))) +
    geom_tile(mapping = aes(fill = values),color= "#FFFFFF") +
    facet_wrap(~accession, ncol=2) +
    scale_fill_gradient2(low = "red", mid = "white", high = "green", midpoint= 0)
    # geom_point(aes(colour = combined, size = value)) +
    # scale_color_gradient(low = "yellow", high = "red") +
    # geom_text(aes(label = round(value, 1)), size = 10, color ="white") +
    # theme(legend.position = "bottom") +
    # theme(legend.key.size=unit(0.2, "cm")) +
    # theme(legend.key.width=unit(0.2, "cm"))
    # ggfittext::geom_fit_text(aes(label = round(values, 1) ),reflow = TRUE, grow = TRUE, contrast = TRUE)
  return(plt)
}





