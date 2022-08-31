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
