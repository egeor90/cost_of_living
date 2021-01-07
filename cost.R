#!/usr/local/bin/Rscript
system("clear");

cat("Please wait! The process is ongoing...\n");

invisible(Sys.setlocale("LC_ALL", 'en_US.UTF-8'))
options(warn=-1)

pck_ <- c("rvest","stringr","data.table","corpus","qdapRegex","countrycode","priceR")

pck <- pck_[!(pck_ %in% installed.packages()[,"Package"])]
if(length(pck)){
  cat(paste0("Installing: ", pck, "\n"))
  install.packages(pck, repos = 'https://cran.rstudio.com/')
}

suppressWarnings(suppressMessages(invisible(lapply(pck_, require, character.only = TRUE))))

all_cities <- fread("data/cities.csv") %>% as.data.frame()

system("clear");

cat("Enter the city: ");
city_1 <- readLines("stdin",n=1);

if(!is.na(grep(str_to_title(city_1), all_cities$city) || grep(str_to_title(city_1), all_cities$alternative))){
  if(length(grep(str_to_title(city_1), all_cities$city)) && is.na(all_cities[grep(str_to_title(city_1), all_cities$city), "alternative"])){
    city_link_1 <- str_to_title(city_1)
    country_1 <- all_cities[grep(pattern=str_to_title(city_1), all_cities$city),"country"] %>% trimws()
  }else if(length(grep(str_to_title(city_1),all_cities$city)) > 0 && !is.na(all_cities[grep(str_to_title(city_1),all_cities$city), "alternative"])){
    city_link_1 <- all_cities[grep(pattern=str_to_title(city_1), all_cities$city),]
    city_link_1 <- paste0(trimws(city_link_1[,1]),"+%28",trimws(city_link_1[,2]),"%29&")
    country_1 <- all_cities[grep(pattern=str_to_title(city_1), all_cities$city),"country"] %>% trimws()
  }else if(length(grep(str_to_title(city_1),all_cities$city)) == 0 && length(grep(str_to_title(city_1),all_cities$alternative)) > 0){
    city_link_1 <- all_cities[grep(pattern=str_to_title(city_1), all_cities$alternative),]
    city_link_1 <- paste0(trimws(city_link_1[,1]),"+%28",trimws(city_link_1[,2]),"%29&")
    country_1 <- all_cities[grep(pattern=str_to_title(city_1), all_cities$alternative),"country"] %>% trimws()
  }else{
    stop("An error occurred. Try again!")
  }
}else{
  stop("This city is not available. Please enter another city!")
}

currency_ <- countrycode(country_1,origin = 'country.name',destination = "iso4217c")

if(length(strsplit(city_link_1, " ")[[1]]) > 1){
  city_link_1 <- paste0(strsplit(city_link_1, " ")[[1]], collapse = "+")
}else{
  city_link_1 <- city_link_1
}

if(length(strsplit(country_1, " ")[[1]]) > 1){
  country_1 <- paste0(strsplit(country_1, " ")[[1]], collapse = "+")
}else{
  country_1 <- country_1
}

if(country_1 == "United+States"){
  city_link_1 <- paste0(city_link_1,"%2C+",all_cities[grep(pattern=str_to_title(city_1), all_cities$city),"state"] %>% trimws())
}else{
  city_link_1 <- city_link_1
}

cat(paste0("Enter the amount to compare (in ", currency_, "): "));
amount_ <- readLines("stdin",n=1);
cat("Enter the city to compare: ");
city_2 <- readLines("stdin",n=1);

if(!is.na(grep(str_to_title(city_2), all_cities$city) || grep(str_to_title(city_2), all_cities$alternative))){
  if(length(grep(str_to_title(city_2), all_cities$city)) && is.na(all_cities[grep(str_to_title(city_2), all_cities$city), "alternative"])){
    city_link_2 <- str_to_title(city_2)
    country_2 <- all_cities[grep(pattern=str_to_title(city_2), all_cities$city),"country"] %>% trimws()
  }else if(length(grep(str_to_title(city_2),all_cities$city)) > 0 && !is.na(all_cities[grep(str_to_title(city_2),all_cities$city), "alternative"])){
    city_link_2 <- all_cities[grep(pattern=str_to_title(city_2), all_cities$city),]
    city_link_2 <- paste0(trimws(city_link_2[,1]),"+%28",trimws(city_link_2[,2]),"%29&")
    country_2 <- all_cities[grep(pattern=str_to_title(city_2), all_cities$city),"country"] %>% trimws()
  }else if(length(grep(str_to_title(city_2),all_cities$city)) == 0 && length(grep(str_to_title(city_2),all_cities$alternative)) > 0){
    city_link_2 <- all_cities[grep(pattern=str_to_title(city_2), all_cities$alternative),]
    city_link_2 <- paste0(trimws(city_link_2[,1]),"+%28",trimws(city_link_2[,2]),"%29&")
    country_2 <- all_cities[grep(pattern=str_to_title(city_2), all_cities$alternative),"country"] %>% trimws()
  }else{
    stop("An error occurred. Try again!")
  }
}else{
  stop("This city is not available. Please enter another city!")
}
if(length(strsplit(city_link_2, " ")[[1]]) > 1){
  city_link_2 <- paste0(strsplit(city_link_2, " ")[[1]], collapse = "+")
}else{
  city_link_2 <- city_link_2
}

if(country_2 == "United+States"){
  city_link_2 <- paste0(city_link_2,"%2C+",all_cities[grep(pattern=str_to_title(city_2), all_cities$city),"state"] %>% trimws())
}else{
  city_link_2 <- city_link_2
}

if(length(strsplit(country_2, " ")[[1]]) > 1){
  country_2 <- paste0(strsplit(country_2, " ")[[1]], collapse = "+")
}else{
  country_2 <- country_2
}

start_ <- Sys.time()

url_ <- paste0("https://www.numbeo.com/cost-of-living/compare_cities.jsp?country1=",country_1,"&city1=",city_link_1,"&country2=",country_2,"&city2=",city_link_2,"&amount=",amount_,"&displayCurrency=",currency_)
html <- read_html(url_)

sum_ <- as.character(text_split(html %>% html_nodes('.summary_big') %>% html_text() %>% str_trim() %>% unlist(), "sentences")[1,3])

if(country_1 != country_2){
  sum_price <- rm_between(rm_between(sum_, 'would need around', 'in', extract=TRUE)[[1]], '(', ')', extract=TRUE)[[1]]
  sum_price <- gsub(",","",sum_price)
}else{
  sum_price <- rm_between(sum_, 'would need around', 'in', extract=TRUE)[[1]]
  sum_price <- gsub(",","",sum_price)
}

sum_price <- ifelse(substr(sum_price,nchar(sum_price),nchar(sum_price)) == ".",
                    substr(sum_price,1,nchar(sum_price)-1),
                    sum_price)

price_2 <- as.numeric(gsub('[^-0-9.]', '', sum_price))

currency_2 <- countrycode(country_2,origin = 'country.name',destination = "iso4217c")

invisible(capture.output(price2_inbase <- suppressWarnings(suppressMessages(invisible(exchange_rate_latest(currency = currency_2))))))
invisible(capture.output(price2_inbase <- exchange_rate_latest(currency = currency_2)))
price2_inbase <- round(price_2*(price2_inbase[which(price2_inbase$currency == currency_),2]),2)

invisible(capture.output(fx_usd <- exchange_rate_latest()))
price1_usd <- as.numeric(fx_usd[which(fx_usd$currency == currency_),2])
price2_usd <- as.numeric(fx_usd[which(fx_usd$currency == currency_2),2])



system("clear");

if(country_1 == country_2){
  cat(" Date: ", format(as.Date(Sys.Date(),origin="1970-01-01")),"\n",
      "Source city: ", str_to_title(city_1),"\n",
      "Compared city: ", str_to_title(city_2),"\n",
      "Amount: ", amount_, currency_,"in",str_to_title(city_1),"\n",
      paste0("\nYou need to have ", paste(price_2, currency_2), " in ", str_to_title(city_2)," to maintain the same life in ",str_to_title(city_1)," with ",amount_," ",currency_,"."),"\n",
      "\nEarning rate (City 2 / City 1): ", round((price_2/price2_usd)/(as.numeric(amount_)/as.numeric(price1_usd)),2),"\n")
}else{
  cat(" Date: ", format(as.Date(Sys.Date(),origin="1970-01-01")),"\n",
      "Source city: ", str_to_title(city_1),"\n",
      "Compared city: ", str_to_title(city_2),"\n",
      "Amount: ", amount_, currency_,"in",str_to_title(city_1),"\n",
      "\nYou need to have", paste0(paste(price_2, currency_2), " (",paste(price2_inbase, currency_), ") in ", str_to_title(city_2)," to maintain the same life in ",str_to_title(city_1)," with ",amount_," ",currency_),"\n",
      "\nEarning rate (City 2 / City 1): ", round((price_2/price2_usd)/(as.numeric(amount_)/as.numeric(price1_usd)),2),"\n")
}

end_ <- Sys.time()
time_ <- end_ - start_

cat("\n\nProcess time: ",round(time_,2), " seconds.\n\n")
