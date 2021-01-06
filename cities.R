# Obtain all cities

city_url <- "https://www.numbeo.com/cost-of-living/prices_by_city.jsp?displayCurrency=USD&itemId=27"
html <- read_html(city_url)
cities_ <- html %>% html_nodes('td') %>% html_text() %>% str_trim() %>% unlist()
cities_ <- cities_[7:length(cities_)]
cities_ <- cities_[seq(from=1,to=length(cities_),3)] %>% as.data.frame()

cities_usa <- cities_[grep(pattern = "United States", cities_[,1]),] %>% as.data.frame()
cities_nousa <- cities_[-grep(pattern = "United States", cities_[,1]),] %>% as.data.frame()


cities_nousa <- as.data.frame(str_split_fixed(cities_nousa[,1], "\\,", 2))
colnames(cities_nousa) <- c("city", "country")
cities_nousa2 <- as.data.frame(str_split_fixed(cities_nousa[,1], "\\(", 2))
cities_nousa2$V2 <- gsub(")","",cities_nousa2$V2)

cities_nousa <- cbind(as.character(cities_nousa2$V1),as.character(cities_nousa2$V2),as.character(cities_nousa$country))
cities_nousa <- cities_nousa %>% as.data.frame()
colnames(cities_nousa) <- c("city", "alternative", "country")
rm(cities_nousa2)

cities_usa <- as.data.frame(str_split_fixed(cities_usa[,1], "\\,", 3))
colnames(cities_usa) <- c("city", "state", "country")

all_cities <- bind_rows(cities_nousa,cities_usa)
all_cities <- all_cities[,c("city","alternative","state","country")]
all_cities <- all_cities[order(all_cities[,"city"]),]

all_cities <- apply(all_cities, 2, function(x) gsub("^$|^ $", NA, x)) %>% as.data.frame()

write.csv(file = "data/cities.csv", all_cities,row.names = FALSE,col.names = TRUE)
