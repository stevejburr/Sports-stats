library(here)
library(tidyverse)
library(XML)
library(RCurl)


url <- "https://en.wikipedia.org/wiki/List_of_Formula_One_polesitters"

data <- getURL(url)
tables <-readHTMLTable(data, stringsAsFactors = F)
poles <- tables$`Formula One polesitters\n`[-1,c(2,4,5)] 
colnames(poles) <- c("Driver","Poles","Races")

poles %>% 
  mutate(Poles=as.numeric(Poles),
         Races=as.numeric(Races),
         rate=Poles/Races) -> poles

write_csv(poles,"poles.csv")

# poles$stdRate <- scale(poles$rate)
# poles

url <- "https://en.wikipedia.org/wiki/List_of_Formula_One_drivers"
data <- getURL(url)
tables <-readHTMLTable(data, stringsAsFactors = F)
all <- tables[[3]]
all <- all[-1,c(1,5,7,8,10)]
colnames(all) <- c("Driver","Races","Poles","Wins","Fastest")

#remove anything contained in square brackets:

all %>%
  mutate(Driver=str_remove(Driver,"[*^~.,]+"),
          Races=str_remove(Races,"\\[.+\\]"),
         Poles=str_remove(Poles,"\\[.+\\]"),
         Wins=str_remove(Wins,"\\[.+\\]"),
         Fastest=str_remove(Fastest,"\\[.+\\]")) %>%
  filter(Driver!="Name") %>%
  mutate_at(c("Races","Poles","Wins","Fastest"),as.numeric) -> all


all %>%
  as.tibble() %>%
  mutate(`Pole Rate`=Poles/Races,
         `Win Rate`=Wins/Races,
         `Fastest Rate`=Fastest/Races,
         `Avg Rate`=(`Pole Rate`+`Win Rate`+`Fastest Rate`)/3) %>%
  filter(Races>=10) %>%
  arrange(desc(`Avg Rate`)) -> all

#create "standardised" rates
all[,c(6,7,8,9)] %>% scale() -> scaled

#append "Scaled" to the front of the variable names
colnames(scaled) %>% map_chr(function(x){paste0("Scaled ",x)}) -> colnames(scaled)

cbind(all,scaled) -> all

all %>%
  write_csv("allF1Data.csv")

#basic viz idea:
# all %>%
#   ggplot(aes(y=`Pole Rate`))+
#   geom_violin(aes(x=1)) +
#   geom_point(data=(all %>% filter(1:n()<=6)),aes(x=1)) +
#   geom_text(data=(all %>% filter(1:n()<=6)),aes(label=Driver,x=1.25))
