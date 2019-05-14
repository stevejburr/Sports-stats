library(here)
library(tidyverse)
library(XML)
library(RCurl)


getCricketData <- function(page){
  Sys.sleep(1)
  url <- paste0("http://stats.espncricinfo.com/ci/engine/stats/index.html?class=1;page=",page,";template=results;type=batting")
  tables <-readHTMLTable(url, stringsAsFactors = F)
  tables$`Overall figures`
}

map_df(1:60,getCricketData) -> allCricketData

write_csv(allCricketData,"cricketData.csv")

# allCricketData[,c(1:11)]%>%
#   mutate(Ave=as.numeric(Ave),
#          Mat=as.numeric(Mat))%>%
#   filter(!is.na(Ave)) %>%
#   filter(Mat>=10) %>%
#   mutate(`Scaled Ave`=(Ave-mean(Ave))/sd(Ave)) %>%
#   arrange(desc(`Scaled Ave`))
