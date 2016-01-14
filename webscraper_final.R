################################# HEADER #########################

## Script created by Mihaly Garamvolgyi
## 2016/01/14
## R version 3.1.2 (2014-10-31) Pumpkin Helmet

# uses rvest webscraping library
# CSS selectorgadget

################################# HEADER #########################

# Automatikus package telepítés
packages <- function(x){
  x <- as.character(match.call()[[2]])
  if (!require(x,character.only=TRUE)){
    install.packages(pkgs=x,repos="http://cran.r-project.org")
    require(x,character.only=TRUE)
  }
}

# packages betöltése
packages(RCurl)
packages(rvest)
packages(reshape2)

# set working directory
setwd("~/Repositories/Webscraper")

# set initial url w/o page number
my_url <-"http://ingatlan.com/lista/elado+nyaralo?page="

# set minimum/maximum page number
min_page = 1
max_page = 337

my_data <- NULL

  # starting loop
  for (current_page in min_page:max_page) {
    
    current_url <- paste0(my_url, current_page, collapse=NULL) 
    
    # download data
    print(paste("downloading page", current_page))
    raw_data <- read_html(current_url, encoding = "UTF-8")
   
    # address only
    # data <- try_data  %>% html_nodes(".address-highlighted") %>%  html_text()
    
    # address plus details
    selected_data <- raw_data  %>% html_nodes(".rowClickCSS") %>%  html_text()
    
    # create row headers
    my_rows <- c('Telepules', 'Ar', 'Ingatlanmeret', 'Telekmeret', 'Szobaszam', 'Ures')
    my_rows <- rep(my_rows, 20)
    selected_data <- cbind(my_rows, selected_data) 
    
    # create IDs
    from <- (current_page - 1) * 20 + 1
    to <-  (current_page - 1) * 20 + 20
    IDs <- as.numeric(rep(seq(from:to), 6))
    IDs <- IDs[order(as.numeric(rep(seq(from:to), 6)))]
    selected_data <- cbind(IDs, selected_data)
    
    # create frame
    selected_data <- data.frame(selected_data)
    
    # reshape to wide format
    selected_data_wide <-dcast(selected_data, IDs ~ my_rows, value.var="selected_data")
    
    # save results
    my_data <- rbind(my_data, selected_data_wide)
    write.csv(my_data, file = "data_20160114.csv")
    
  }

write.csv(my_data, file = "file.csv")

print("FINSHED!")

# Octal Escape Sequence 