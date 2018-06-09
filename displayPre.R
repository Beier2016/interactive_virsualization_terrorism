#loading library
list.of.packages <- c("prettydoc", "dplyr", "tidyr", "DT", "highcharter", "leaflet", "corpus", "tm", "wordcloud", "tidytext", "shiny")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
library(shiny)
library(prettydoc)
library(dplyr)
library(tidyr)
library(DT)
library(highcharter)
library(leaflet)
library(corpus)
library(tm)
library(wordcloud)


#if there is encoding problem, uncomment the code below, and run it
#Sys.setlocale(category = "LC_ALL", locale = "us")


#loading data and wrangling
terror <- read.csv('globalterrorismdb_0617dist.csv', stringsAsFactors = FALSE)
terror <- select(terror, iyear, attacktype1_txt, 
                 gname, nkill, nwound, region_txt, country_txt, city, success,
                 latitude, longitude, iday, imonth, summary, weaptype1_txt, 
                 ransomnote, motive, summary) %>%
  filter(as.numeric(iyear) >= 2006, as.numeric(iyear) <= 2016)
terror <- terror[complete.cases(terror), ]

#attack type
attack_type <- filter(terror, !is.na(attacktype1_txt)) %>%
  group_by(attacktype1_txt) %>%
  summarise(Times = n()) %>%
  arrange(desc(Times))
attack_type <- attack_type$attacktype1_txt
attack_type <- c("All", attack_type)

#top terror group
top30_group <- filter(terror, !is.na(gname)) %>%
  group_by(gname) %>%
  summarise(Times = n()) %>%
  arrange(desc(Times))
top30_group <- top30_group$gname[2:31]
top30_group <- c("All", top30_group)

#top 30 city
city_name_list <- filter(terror, city != "Unknown") %>% 
  group_by(city) %>%
  summarise(Times = n()) %>%
  arrange(desc(Times))
city_name_list <- city_name_list[1:30,1]
city_name_list <- city_name_list$city
city_name_list <- c("All", city_name_list)

#1 Tree map for region
g_treemap_region <- function() {
  #summarize, select, filter data
  killed_regions <- filter(terror, is.na(nkill) == FALSE) %>% 
    group_by(region_txt) %>% 
    summarise(Total = sum(as.numeric(nkill))) %>% 
    arrange(desc(Total))%>% 
    head(30)
  
  #building treemap
  hchart(killed_regions,"treemap", hcaes(x = region_txt, value = Total, color = Total)) %>%
    hc_colorAxis(minColor = "#F1948A", maxColor = "#7B241C") %>%
    hc_title(text = "Number of People Killed (Region) ") %>%
    hc_add_theme(hc_theme_google()) %>%
    hc_credits(enabled = TRUE, text = "Sources: The Global Terrorism Database (GTD)", style = list(fontSize = "10px"))
}

#1 Tree map for country
g_treemap_country <- function(){
  #summarize, select, filter data
  countries_killed <- filter(terror, is.na(nkill) == FALSE) %>% 
    group_by(country_txt) %>% 
    summarise(Total = round(sum(as.numeric(nkill)))) %>% 
    arrange(desc(Total)) %>% 
    head(30)
  #building hchart
  hchart(countries_killed, "treemap", hcaes(x = country_txt, value = Total, color = Total)) %>%
    hc_colorAxis(minColor = "#D7BDE2", maxColor = "#5B2C6F") %>%
    hc_title(text = "Number of People Killed (Country)") %>%
    hc_add_theme(hc_theme_google()) %>%
    hc_credits(enabled = TRUE, text = "Sources: The Global Terrorism Database (GTD)", style = list(fontSize = "10px"))
}

#1 Tree map for city
g_treemap_city <- function(){
  #summarize, select, filter data
  cities_killed <- filter(terror, city != "Unknown", is.na(nkill) == FALSE)%>% 
    group_by(city) %>% 
    summarise(Total = round(sum(as.numeric(nkill)))) %>% 
    arrange(desc(Total)) %>% 
    head(30)
  #building hchart
  hchart(cities_killed, "treemap", hcaes(x = city, value = Total, color = Total)) %>%
    hc_colorAxis(minColor = "#D5F5E3", maxColor = "#0B5345") %>%
    hc_title(text = "Number of People Killed (City)") %>%
    hc_add_theme(hc_theme_google()) %>%
    hc_credits(enabled = TRUE, text = "Sources: The Global Terrorism Database (GTD)", style = list(fontSize = "10px"))
}

#1 Tree map for year
g_treemap_year <- function(){
  #filter data
  years_killed <- filter(terror, is.na(nkill) == FALSE) %>% 
    group_by(iyear) %>% 
    summarise(Total = round(sum(as.numeric(nkill)))) %>% 
    arrange(desc(Total)) %>% 
    head(30)
  years_killed$iyear <- as.character(years_killed$iyear)
  #building hchart
  hchart(years_killed, "treemap", hcaes(x = iyear, value = Total, color = Total)) %>%
    hc_colorAxis(minColor = "#D5DBDB", maxColor = "#212F3C") %>%
    hc_title(text = "Number of People Killed (Year)") %>%
    hc_credits(enabled = TRUE, text = "Sources: The Global Terrorism Database (GTD)", style = list(fontSize = "10px")) %>%
    hc_add_theme(hc_theme_google())
}

#1 tree map controller, type_name:refion, country, city, year
g_treemap <- function(type_name){
  buffer <- NA
  if (type_name == "Region") {
    buffer <- g_treemap_region()
  }
  if (type_name == "Country") {
    buffer <- g_treemap_country()
  }
  if (type_name == "City") {
    buffer <- g_treemap_city()
  }
  if (type_name == "Year") {
    buffer <- g_treemap_year()
  }
  buffer
}

#2 map for attack events
#years: c(1994, 2014)
#succ: 0 / 1, attack_type
#city_name: top30 dangerous city
#map used data
map_used_data <- select(terror, latitude, longitude, success, iyear, city, imonth, iday, country_txt, summary, nkill, nwound, attacktype1_txt)
g_map_attack <- function(years, succ, city_name){
  succ <- ifelse(succ == "Yes", "1", "0")
  year_from <- years[1]
  year_to <- years[2]
  buffer <- data.frame()
  #select non-na value, and match the year
  if (city_name == "All") {
    buffer <- filter(map_used_data, success == as.character(succ),
                     city != "Unknown",
                     as.numeric(iyear) >= year_from, 
                     as.numeric(iyear) <= year_to)
  } else {
    buffer <- filter(map_used_data, success == as.character(succ), 
                     city ==  city_name,
                     city != "Unknown",
                     as.numeric(iyear) >= year_from, 
                     as.numeric(iyear) <= year_to)
  }

  
  
  #building the map
  leaflet(data = buffer) %>%
    addTiles() %>%
    addMarkers(lat = as.numeric(buffer$latitude), 
               lng = as.numeric(buffer$longitude), 
               clusterOptions = markerClusterOptions(),
               popup= paste("<strong>Date: </strong>", buffer$iday,"/",buffer$imonth,"/", buffer$iyear,
                            "<br><strong>Place: </strong>", buffer$city,"-",buffer$country_txt,
                            "<br><strong>Attack Type: </strong>", buffer$attacktype1_txt,
                            "<br><strong>Killed: </strong>", buffer$nkill,
                            "<br><strong>Wounded: </strong>", buffer$nwound,
                            "<br><strong>Summary: </strong>", buffer$summary))
}

#3 terror organization statistics table and search function
g_table_terrorGroup <- function(){
  #filter data
  group_name <- filter(terror, gname != "Unknown", !is.na(iyear))
  #fill na by 0, otherwise sum will be NA
  group_name$nkill[is.na(group_name$nkill)] <- 0
  group_name$nwound[is.na(group_name$nwound)] <- 0
  #summarize
  group_name <- group_by(group_name, gname) %>% 
    summarise(Event_Times = n(), 
              Killed = sum(as.numeric(nkill)), 
              Wounded = sum(as.numeric(nwound))) %>% 
    arrange(desc(Event_Times)) %>%
    rename("Organization_Name" = gname)
  #generate tabele
  datatable(group_name)
}

#4 terror organization detail for top 30
# attack number 2D bar chart
# terror_organization_name: "All" "Taliban", "Al-Shabaab", "Zwai Tribe"
g_2Dbarchart_attack_number <- function(terror_organization_name) {
  #filter data
  attack_num <- data.frame()
  if (terror_organization_name == "All") {
    attack_num <- filter(terror, is.na(iyear) == FALSE)
  } else {
    attack_num <- filter(terror, is.na(iyear) == FALSE, 
                         gname == as.character(terror_organization_name))
  }
  attack_num$iyear <- as.numeric(attack_num$iyear)
  #success times
  attack_num1 <- filter(attack_num, success == 1)   %>%
    group_by(iyear) %>%
    summarise(succ = n())
  #failure times and full join
  attack_num <- filter(attack_num, success == 0)   %>%
    group_by(iyear) %>%
    summarise(fail = n()) %>% 
    full_join(attack_num1, by = "iyear") %>%
    #fill all NA by 0
    mutate_all(funs(replace(., is.na(.), 0))) %>%
    arrange(iyear)
  #building 2D bar chart
  attack_by_group <- highchart() %>%
    hc_xAxis(categories = attack_num$iyear) %>%
    hc_add_series(name = "Success", data = attack_num$succ) %>%
    hc_add_series(name = "Failure", data = attack_num$fail) %>% 
    hc_chart(type = "column") %>%
    hc_yAxis(title = list(text = "Attack Number")) %>% 
    hc_xAxis(title = list(text = "Year")) %>%
    # Titles and credits
    hc_title(text = paste("Attack Number by", terror_organization_name, collapse = " ")) %>%
    hc_credits(enabled = TRUE, text = "Sources: The Global Terrorism Database (GTD)", style = list(fontSize = "10px"))
  print(attack_by_group)
}

#5 pie chart of attack type for top 30
# terror_organization_name: "All", "Taliban", "Al-Shabaab", "Zwai Tribe"
g_piechart_attack_type <- function(terror_organization_name){
  #filter data
  attack_type <- data.frame()
  if (terror_organization_name == "All") {
    attack_type <- filter(terror) %>% 
      group_by(attacktype1_txt) %>%
      summarise(times = n())
  } else {
    attack_type <- filter(terror, gname == as.character(terror_organization_name)) %>% 
      group_by(attacktype1_txt) %>%
      summarise(times = n())
  }
  #building the pie chart
  attack_type_piechart <- highchart() %>%
    hc_title(text = "Attack Type",
             style = list(fontSize = "25px")) %>% 
    hc_chart(type = "column",
             polar = TRUE) %>% 
    hc_xAxis(categories = as.character(attack_type$attacktype1_txt)) %>% 
    hc_add_series(attack_type$times, name = "Times", showInLegend = FALSE) %>%
    hc_add_theme(hc_theme_smpl()) %>%
    hc_credits(enabled = TRUE, text = "Sources: The Global Terrorism Database (GTD)", style = list(fontSize = "10px"))
  print(attack_type_piechart)
}

#6 icon virsualization for top 30
# terror_organization_name: "All", "Taliban", "Al-Shabaab", "Zwai Tribe"
g_iconvir_weapon <- function(terror_organization_name){
  # attack weapon by icon virsulization
  weapon <- data.frame()
  if (terror_organization_name == "All") {
    weapon <- filter(terror) %>% 
      group_by(weaptype1_txt) %>%
      summarise(times = n()) %>%
      arrange(desc(times))
  } else {
    weapon <- filter(terror, gname == as.character(terror_organization_name)) %>% 
      group_by(weaptype1_txt) %>%
      summarise(times = n()) %>%
      arrange(desc(times))
  }
  
  #create icon weapon match table
  weapon_icon_table <- filter(terror) %>% 
    group_by(weaptype1_txt) %>%
    summarise(times = n()) %>%
    arrange(desc(times))
  buffer_icon <- c("bomb", "bullseye", "question", "fire", "users", "flask", "truck", "comments", "medkit", "bell-slash","signal")
  weapon_icon_table <- data.frame(weapon_icon_table, icon = buffer_icon)
  
  #building icon virsulazation
  icon_insert <- c()
  for (x in 1:dim(weapon)[1]) {
    for (y in 1:dim(weapon_icon_table)[1]) {
      if (weapon[x,1] == weapon_icon_table[y,1]) {
        icon_insert <- c(icon_insert, as.character(weapon_icon_table[y,3]))
        break()
      }
    }
  }
  weapon <- data.frame(weapon, icon = icon_insert)
  weapon$times <- round(weapon$times/sum(weapon$times) * 100)
  weapon <- filter(weapon, times != 0)
  buffer <- hciconarray(weapon$weaptype1_txt, weapon$times, icons = weapon$icon) %>%
    hc_add_theme(
      hc_theme_merge(
        hc_theme_flatdark(),
        hc_theme_null(chart = list(backgroundColor = "#ffffff"))
      )
    )
  print(buffer)
}

#7 text analysis for ransom
g_textAnalysis_ransom <- function(){
  #text wrangling
  text_analysis <- filter(terror, !is.na(ransomnote), ransomnote != "")
  text <- text_analysis$ransomnote
  bufferCorpus <- Corpus(VectorSource(text))
  bufferCorpus = tm_map(bufferCorpus, content_transformer(tolower))
  bufferCorpus = tm_map(bufferCorpus, removePunctuation)
  bufferCorpus = tm_map(bufferCorpus, removeNumbers)
  bufferCorpus = tm_map(bufferCorpus, removeWords,c(stopwords("english"), stopwords("SMART")))
  
  #building word cloud
  myDtm = TermDocumentMatrix(bufferCorpus,control = list(minWordLength = 1))
  freqTerms <- findFreqTerms(myDtm, lowfreq=1)
  m <- as.matrix(myDtm)
  v <- sort(rowSums(m), decreasing=TRUE)
  myNames <- names(v)
  data <- data.frame(word=myNames, freq=v)
  wc <-wordcloud(data$word, data$freq, min.freq=3, colors=brewer.pal(9,"Set1"), scale = c(5,.3))
}
#7 text analysis for motive
g_textAnalysis_motive <- function(){
  #text wrangling
  text_analysis <- filter(terror, !is.na(motive), motive != "")
  text <- sample(text_analysis$motive, nrow(text_analysis)/60)
  #unwanted words
  specificWords <- c("The", "Unknown", "attack", "specific", "motive", "sources", "unknown", "claimed", "targeted",
                     "carried", "noted", "incident", "stated", "responsibility", "the")
  text<-sapply(text, function(x) gsub("\n"," ",x))
  bufferCorpus<-VCorpus(VectorSource(text))
  bufferCorpusClean <- bufferCorpus %>% 
    tm_map(content_transformer(removeNumbers)) %>% 
    tm_map(content_transformer(removePunctuation)) %>%
    tm_map(content_transformer(removeWords),tidytext::stop_words$word) %>%
    tm_map(content_transformer(removeWords),specificWords)
  #building the world cloud
  myDtm = TermDocumentMatrix(bufferCorpusClean, control = list(minWordLength = 3))
  freqTerms <- findFreqTerms(myDtm, lowfreq=1)
  m <- as.matrix(myDtm)
  v <- sort(rowSums(m), decreasing=TRUE)
  myNames <- names(v)
  data <- data.frame(word=myNames, freq=v)
  wc <-wordcloud(data$word, data$freq, min.freq=9, colors=brewer.pal(9,"Set1"), scale = c(5,.3))
}
#7 text analysis for summary
g_textAnalysis_summary <- function(){
  #text wrangling
  text_analysis <- filter(terror, !is.na(summary), summary != "")
  text <- sample(text_analysis$summary, nrow(text_analysis)/100)
  bufferCorpus <- Corpus(VectorSource(text))
  bufferCorpus = tm_map(bufferCorpus, removePunctuation)
  bufferCorpus = tm_map(bufferCorpus, removeNumbers)
  bufferCorpus = tm_map(bufferCorpus, removeWords,c(stopwords("english"), stopwords("SMART"), "the"))
  myDtm = TermDocumentMatrix(bufferCorpus,control = list(minWordLength = 3))
  freqTerms <- findFreqTerms(myDtm, lowfreq=1)
  
  #building the word cloud
  m <- as.matrix(myDtm)
  v <- sort(rowSums(m), decreasing=TRUE)
  myNames <- names(v)
  data <- data.frame(word=myNames, freq=v)
  wc <-wordcloud(data$word, data$freq, min.freq=50, colors=brewer.pal(9,"Set1"), scale = c(5,.3))
}