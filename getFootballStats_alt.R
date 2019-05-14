#alternative approach to getting football stats:
#use pages like this: https://www.worldfootball.net/goalgetter/esp-primera-division-2018-2019/
#go back to 1990 or 1996?


years <- c(1990:2019)

cbind(years[1:29],years[2:30]) %>%
  data.frame() %>%
  mutate(seasons=paste0(X1,"-",X2)) %>%
  pull(seasons) -> seasons


urls <- c("https://www.worldfootball.net/goalgetter/esp-primera-division",
          "https://www.worldfootball.net/goalgetter/eng-premier-league",
          "https://www.worldfootball.net/goalgetter/champions-league",
          "https://www.worldfootball.net/goalgetter/europa-league",
          "https://www.worldfootball.net/goalgetter/ita-serie-a",
          "https://www.worldfootball.net/goalgetter/fra-ligue-1",
          "https://www.worldfootball.net/goalgetter/bundesliga")

expand.grid(url=urls,season=seasons) %>%
  mutate(url=paste0(url,"-",season,"/")) %>%
  pull(url) -> urlsList


getFootballData <- function (url){
  Sys.sleep(1)
  data <- try({getURL(url)})
  if(class(data)!="try-error"){
  tables <-readHTMLTable(data, stringsAsFactors = F)
  keep(tables,is.data.frame) %>% as.data.frame() -> data
  data <- data[,c(2,6)]
  colnames(data) <- c("Player","Goals")
  data %>% 
    mutate(Penalty=str_extract(Goals,"(?<=\\()[0-9]+"),
           Goals = str_extract(Goals,"^[0-9]+"))
  }
}

map_df(urlsList,getFootballData) %>% 
  group_by(Player) %>%
  mutate(Penalty=as.numeric(Penalty),
         Goals = as.numeric(Goals)) %>%
  summarise(Penalty=sum(Penalty),
            Goals=sum(Goals)) -> allFootballData

write_csv(allFootballData,"allTopScores_alt.csv")
     