library(here)
library(tidyverse)
library(XML)
library(RCurl)



getLeagueStats<- function(url,page){
    Sys.sleep(1)
    url <- paste0(url,page,"/")
    print(url)
    data <- getURL(url)
    tables <-readHTMLTable(data, stringsAsFactors = F)
    keep(tables,is.data.frame) %>% as.data.frame()
}

#get english football scorers:
url <- "https://www.worldfootball.net/alltime_goalgetter/eng-premier-league/tore/"
england <- map_df(1:20,~getLeagueStats(url,.x))
colnames(england) <- c("rank","player","teams","matches","goals","penalties","rate")



#get champions league scorers:
url<- "https://www.worldfootball.net/alltime_goalgetter/champions-league/tore/"
ucl <- map_df(1:20,~getLeagueStats(url,.x))
colnames(ucl) <- c("rank","player","teams","matches","goals","penalties","rate")


#get europa league scorers:
url <- "https://www.worldfootball.net/alltime_goalgetter/europa-league/tore/"
europa <- map_df(1:20,~getLeagueStats(url,.x))
colnames(europa) <- c("rank","player","teams","matches","goals","penalties","rate")

#get cup winners cup:
url <- "https://www.worldfootball.net/alltime_goalgetter/ec-der-pokalsieger/tore/"
cwc <- map_df(1:20,~getLeagueStats(url,.x))
colnames(cwc) <- c("rank","player","teams","matches","goals","penalties","rate")


#france:
url <- "https://www.worldfootball.net/alltime_goalgetter/fra-ligue-1/tore/"
france <- map_df(1:20, ~getLeagueStats(url,.x))
colnames(france) <- c("rank","player","teams","matches","goals","penalties","rate")

#germany:
url <- "https://www.worldfootball.net/alltime_goalgetter/bundesliga/tore/"
germany <- map_df(1:20, ~getLeagueStats(url,.x))
colnames(germany) <- c("rank","player","teams","matches","goals","penalties","rate")

#italy:
url <- "https://www.worldfootball.net/alltime_goalgetter/ita-serie-a/tore/"
italy <- map_df(1:20, ~getLeagueStats(url,.x))
colnames(italy) <- c("rank","player","teams","matches","goals","penalties","rate")

#spain:
url <- "https://www.worldfootball.net/alltime_goalgetter/esp-primera-division/tore/"
spain <- map_df(1:20, ~getLeagueStats(url,.x))
colnames(spain) <- c("rank","player","teams","matches","goals","penalties","rate")


#save to .csv files:
write_csv(cwc,"cwc.csv")
write_csv(england,"england.csv")
write_csv(europa,"europa.csv")
write_csv(france,"france.csv")
write_csv(ucl,"ucl.csv")
write_csv(germany,"germany.csv")
write_csv(italy,"italy.csv")
write_csv(spain,"spain.csv")


#append all files together into one master files
#assume that player name as listed in the tables is a good enough unique identifier...
#then can summarise + sum up by player and recalculate rate from matches + goals (get inc/exc penalties)

rbind(cwc,england,europa,france,ucl,germany,italy,spain) %>%
  select(-c(rank,teams,rate)) %>%
  mutate_at(c("matches","goals","penalties"),as.numeric) %>%
  filter(!is.na(player)) %>%
  group_by(player) %>%
  summarise(matches=sum(matches),
            goals=sum(goals),
            penalties=sum(penalties)) %>%
  mutate(rateExcPen=(goals-penalties)/matches,
         rateIncPen=goals/matches) %>%
  filter(matches>30) %>%
  arrange(desc(rateIncPen)) -> allTopScores

write_csv(allTopScores,"allTopScores.csv")

