library(readxl)
get_GXE_data <- function(imported_data){
  # imported_data <- "../../Visualizations/BLUPS-UYT-40.xls"
  mysheets_fromexcel <- list()
  mysheetlist <- excel_sheets(imported_data)
  # class(mysheetlist)
  mysheetlist <- lapply(mysheetlist, tolower)
  mysheetlist <- as.character(mysheetlist)
  list_of_traits <- c("dm", "dyld", "fyld", "pltht", "mcmds", "mcmdi", "sprout", "hi", "rtno", "rtwt", "shtwt")

  for (i in 1:length(mysheetlist)){
    if(mysheetlist[i]  %in% list_of_traits){
      tempdf <- read_excel(path=xlsx_file, sheet = i)
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
      tempdf <- read_excel(path=xlsx_file, sheet = i)
      mysheets_fromexcel[[i]] <- tempdf
      assign(mysheetlist[i], tempdf)
    }
  }
  list_of_excels <- mysheets_fromexcel %>% discard(is.null)
  return(list_of_excels)
}





















### ** Examples

which(LETTERS == "R")
which(ll <- c(TRUE, FALSE, TRUE, NA, FALSE, FALSE, TRUE)) #> 1 3 7
names(ll) <- letters[seq(ll)]
which(ll)
which((1:12)%%2 == 0) # which are even?
which(1:10 > 3, arr.ind = TRUE)
( m <- matrix(1:12, 3, 4) )

div.3 <- m %% 3 == 0
which(div.3)

which(div.3, arr.ind = TRUE)

rownames(m) <- paste("Case", 1:3, sep = "_")
which(m %% 5 == 0, arr.ind = TRUE)

dim(m) <- c(2, 2, 3);

which(div.3, arr.ind = FALSE)

which(div.3, arr.ind = TRUE)


vm <- c(m)
dim(vm) <- length(vm) #-- funny thing with  length(dim(...)) == 1
which(div.3, arr.ind = TRUE)


## Don't show:
dimnames(m) <- list(X = c("U", "V"), Z = c("y","z"), three = LETTERS[1:3])
wm <- which(m %% 3 == 0, arr.ind = TRUE)
vn <- vm; dimnames(vn) <- list(LETTERS[1:12])
wv <- which(vn %% 3 == 0, arr.ind = TRUE)

stopifnot(identical(wv, array(3L*(1:4), dim = c(4, 1),
                              dimnames = list(c("C", "F", "I", "L"), "dim1"))),
          identical(wm, array(c(1:2, 1:2, 2:1, 1:2, 1:3, 3L),
                              dim = 4:3,
                              dimnames = list(rep(c("U","V"),2),
                                              c("X", "Z", "three"))))
)
## End(Don't show)






















############# This function attaches the location and coordinates the dataset ####################



attach_locs <- function(dat){
  # piv <- dat %>% pivot_longer(cols = -c(trait, accession, combined), names_to = "location", values_to = "values")
  ### Declare a dataframe with each required location and their coordianates
  locs <- data.frame(location = c("Ibadan", "Mokwa", "Ago-owu", "Onne", "Otobi", "Ubiaja"), lat = c(7.3775, 9.2928, 7.2519, 4.7238, 7.1079, 6.6493),
                     long = c(3.9470,5.0547,4.3258,7.1516,8.0897,6.3918))

  ### Rename your locations using regex and stringr package for detecting patterns
  piv2 <- dat %>%  mutate(
    location = case_when(
      stringr::str_detect(location, pattern = regex("ag"))        ~ "Ago-owu",
      stringr::str_detect(location, pattern = regex("Ib"))        ~ "Ibadan",
      stringr::str_detect(location, paste(c("mk", "Mok"), collapse = '|'))        ~ "Mokwa",
      stringr::str_detect(location, pattern = regex("On"))        ~ "Onne",
      stringr::str_detect(location, pattern = regex("ot"))        ~ "Otobi",
      stringr::str_detect(location, pattern = regex("ub"))        ~ "Ubiaja",
      stringr::str_detect(location, pattern = regex("Ik"))        ~ "Ikenne"
    )
  ) %>%
    #### bind the coordinates to the location using the detect for detecting a regex in your location column for both longitude and latitude
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









#### Read excel file BLUPS data
datageh <- read_excel("AYT28_35.xls") %>% janitor::clean_names()

### Rename the dataset columns
colnames(datageh) <- c("trial", "location","accession", "tc","fyld","dyld","dm")

### Select the trial needed, in this case, AYT28
### Pivot longer and add a column to identify both checks and treatments, in this case, genotypes
### Attach locations and their coordinates using our function, then filter the trait to plot

piv2 <- datageh %>% filter(str_detect(trial , "20AYT28")) %>%
  pivot_longer(cols = -c(trial, location, accession), names_to = "traits", values_to = "values") %>% select(-trial) %>%
  mutate(category = if_else(as.numeric(str_extract(accession, "\\d{2}+")) <= 15, "Check", "Genotype")) %>% attach_locs() %>%
  filter(traits == "dyld")




##### Plotting into the 'P object using GGPLOT
#### plot shape files using geom_sf, in our case using level 2 Nigeria map shape file.
#### Add Jitter for the accession
### Label the shape file using geom_sf_text

p <- ggplot() +
  # geom_sf(data = lev1, show.legend = TRUE) +
  geom_sf(data = lev1, colour = "black", fill = "white", size = .1) +
  geom_text(data = piv2, aes(x = long, y = lat, label = location), nudge_x = .2, nudge_y = .9, check_overlap = FALSE) +
  geom_jitter(data = piv2, width = 0.75, height = 0.70, mapping = aes(x = long, y = lat, color = values, shape = category, label = accession), size = 5)+
                                        # text = paste0("<b> trait: ",trait,"</b> \n",
                                        #               "<b> accession: ", accession, "</b> \n",
                                        #               "<b>",trait_sel ,":",values,"</b>")))+
  # geom_text_repel(data = piv2, mapping = aes(x = long, y = lat, label = accession)) +
  scale_color_viridis_c() +
  scale_shape_manual(values = c(18, 16))+
  # scale_fill_gradient2(low = "red", midpoint = 6.357, mid = "yellow", high = "green") +
  # scale_color_manual(values = c("darkblue", "red")) +
  # coord_sf(xlim = c(2, 6), ylim = c(6, 10), expand = FALSE) +
  geom_sf_text(data = lev1, aes(label = statename, alpha = 0.9)) +

  labs(x = "", y = "")+
  # theme(panel.grid.major = element_line(color = gray(0.5), linetype = "dashed",
  #                                       size = 0.5), panel.background = element_rect(fill = "aliceblue")) +
  # facet_wrap(~fct_inorder(traits), ncol = 2) +
  theme_void() +
  ggtitle(element_text("Dry yield")) +
  theme(legend.position = "bottom", axis.line=element_blank(),
        axis.text = element_blank(),
        plot.title = element_text("Total Carotene"),
        axis.ticks=element_blank(), title = element_text("Total Carotene"))
p


#### Saving file as a png image
ggsave(p, filename = "~/Desktop/Dry_matter.png")





### For the barplot check difference

## Read data and clean names using janitor
## change the first column name from genotype to accession, specify index if not applicable

dataframe <- read_excel("AYT28.xlsx") %>% janitor::clean_names()
colnames(dataframe)[1] <- "accession"

### Identify checks and treatments
dataframe <- dataframe %>% mutate(category = if_else(as.numeric(str_extract(accession, "\\d{2}+")) <= 15, "Check", "Genotype"))
  # dataframe <- read_excel("../Visualizations/BLUPS-UYT-40.xls")
  # print(checks)
  # dataframe <- pivot_wider_function(dataframe)
  # print(dataframe)


#### Filter the checks out
  combined_check_mean <- dataframe %>%
    filter(category == "Check") %>%
    # group_by(trait) %>%


#### Summarise and calculate the mean across
    summarise(across(where(is.numeric),.fns = ~ mean(.,na.rm =T))) %>%
    # mutate(tcRank = "check_mean") %>%
    # unite("trait",trait,category) %>%
    mutate(accession = "check_mean")

### Bind the checkmean data and the original data
  combined_trait_checkmean <- bind_rows(dataframe ,combined_check_mean)

  #### Calculate the difference with the checks
  traits_all2 <- combined_trait_checkmean %>%
    # select(-c(trait,accession)) %>%
    # group_by(trait) %>%
    mutate(across(where(is.numeric), .fns = ~((./.[accession == "check_mean"]-1)*100))) %>%
    mutate(tcRank = tcichkc) %>%
    mutate(dyldRank = dyld) %>%
    mutate(dmRank = dm) %>% arrange(-tcichkc, -dyldRank, -dmRank)
  # trt <- traits_all2 %>% janitor::clean_names() %>% attach_locs() %>% janitor::clean_names()



#### Plot your data into the barplot_checkdiff object
  plot_data <- traits_all2
  barplot_checkdiff <- plot_data[1:6,] %>%
    # filter(accession == acc_names[i]) %>%
    # dplyr::select(-c(tcRank, dyldRank, fyldRank)) %>%
    dplyr::select(accession,everything()) %>%
    pivot_longer(-c(accession, category, tcRank, dyldRank, dmRank), names_to = "traits", values_to = "values") %>%
    arrange(-tcRank, -dyldRank, -dmRank) %>%

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
    scale_y_continuous(labels = function(x) paste0(x, "%")) +
    facet_wrap(~accession, ncol = 1, strip.position = "right", scales = "free") +
    labs(x = "Trait", y = "Percentage Difference", title = "Difference of Accessions X Checks") +
    theme_bw(base_size = 35)
  barplot_checkdiff
  ggsave(barplot_checkdiff, filename = "~/Desktop/plot_images/barplot.jpg", height = 30, width = 20, limitsize = FALSE)

