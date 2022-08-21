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
  # datageh <- read_csv("../../Visualizations/combo.csv")
  datageh <- imported_data
  dat <- janitor::clean_names(datageh)  #%>% select_if(~!any(is.na(.)))


  # dat %>% View()
  piv <- dat %>% pivot_longer(cols = -c(trait, accession, combined), names_to = "location", values_to = "values")
  locs <- data.frame(location = c("Ibadan", "Mokwa", "Ago-owu", "Onne", "Otobi", "Ubiaja"), lat = c(7.3775, 9.2928, 7.2519, 4.7238, 7.1079, 6.6493),
                     long = c(3.9470,5.0547,4.3258,7.1516,8.0897,6.3918))
  piv2 <- piv %>%  mutate(
    location = case_when(
      stringr::str_detect(location, pattern = regex("ag"))        ~ "Ago-owu",
      stringr::str_detect(location, pattern = regex("ib"))        ~ "Ibadan",
      stringr::str_detect(location, pattern = regex("mk"))        ~ "Mokwa",
      stringr::str_detect(location, pattern = regex("on"))        ~ "Onne",
      stringr::str_detect(location, pattern = regex("ot"))        ~ "Otobi",
      stringr::str_detect(location, pattern = regex("ub"))        ~ "Ubiaja",
      stringr::str_detect(location, pattern = regex("ik"))        ~ "Ikenne"
    )
  ) %>%
    mutate(
      long = case_when(
        location == "Ago-owu"      ~ locs %>% filter(location=="Ago-owu") %>% select(long) %>% as.numeric(),
        location == "Ibadan"      ~ locs %>% filter(location=="Ibadan") %>% select(long) %>% as.numeric(),
        location == "Mokwa"      ~ locs %>% filter(location=="Mokwa") %>% select(long) %>% as.numeric(),
        location == "Onne"      ~ locs %>% filter(location=="Onne") %>% select(long) %>% as.numeric(),
        location == "Otobi"      ~ locs %>% filter(location=="Otobi") %>% select(long) %>% as.numeric(),
        location == "Ubiaja"      ~ locs %>% filter(location=="Ubiaja") %>% select(long) %>% as.numeric()
      )
    ) %>%
    mutate(
      lat = case_when(
        location == "Ago-owu"      ~ locs %>% filter(location=="Ago-owu") %>% select(lat) %>% as.numeric(),
        location == "Ibadan"      ~ locs %>% filter(location=="Ibadan") %>% select(lat) %>% as.numeric(),
        location == "Mokwa"      ~ locs %>% filter(location=="Mokwa") %>% select(lat) %>% as.numeric(),
        location == "Onne"      ~ locs %>% filter(location=="Onne") %>% select(lat) %>% as.numeric(),
        location == "Otobi"      ~ locs %>% filter(location=="Otobi") %>% select(lat) %>% as.numeric(),
        location == "Ubiaja"      ~ locs %>% filter(location=="Ubiaja") %>% select(lat) %>% as.numeric()
      )
    )
  return(piv2)
}


pivot_wider_function <- function(imported_data){
  uytdata <- wrangle_data(imported_data) %>% select(-c(long, lat))
  uytdata <- uytdata %>% pivot_wider(names_from = "location", values_from = "values")
  return(uytdata)
}

linePlot <- function(trait_to_plot, imported_data){
  # listr <- c("dyld", "fyld", "")

  imported_data <- read_csv("../../Visualizations/combo.csv")
  trait_to_plot <- "SPROUT"
  uytdata <- pivot_wider_function(imported_data) %>% filter(trait == trait_to_plot)
  uytdatax <- uytdata %>% arrange(desc(combined)) %>% top_frac(wt = combined, .1)
  uytdatax %>% View()
  x <- uytdatax %>% pivot_longer(-c(trait, accession),
                                 names_to = "location", values_to = "values")
  full_names <- list("MCMDS" = "Cassava Mosaic", "HI" = "Harvest Index", "DM" = "Dry Matter",
                "SPROUT" = "Sprout", "DYLD" = "Dry yield", "FYLD" = "Fresh yield", "PLTHT" = "Plant Height")

  p <- x %>% ggplot(aes(location, values, group = accession, colour = accession)) +
    geom_line()  +
    theme_grey() +
    theme(legend.position = 'none',
          plot.background = element_rect(fill = "#cecece",colour = "#cecece")) +
          # plot.caption = element_text("Hello",face = "italics")) +
    labs(x = "Location", y = full_names[trait_to_plot]) +
    # guides(fill = "none") +
    geom_text_repel(max.overlaps = Inf, box.padding = 0.5, aes(group = accession, label = if_else(location == "Mokwa",accession,""), max.overlaps = 5))

  p

  return(p)
}
