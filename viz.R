library(tidyverse)
library(ggbeeswarm)
library(ggrepel)

#start with jbm football dataset and flag messi:
# from https://johnburnmurdoch.github.io/projects/goal-lines/all-comps/
football_jbm <- read_csv("football_johnburnmurdoch.csv")

football_jbm %>%
  group_by(id,name) %>%
  summarise(games=max(played),
            goals=max(G),
            penalties=max(G)-max(NPG)) %>%
  mutate(rate=goals/games,
         rateExcPen=(goals-penalties)/games)%>%
  ungroup() %>%
  mutate(scaledRate=(rate-mean(rate))/sd(rate),
         scaledRateExcPen=(rate-mean(rateExcPen))/sd(rate)) %>%
  arrange(desc(scaledRate))-> football_jbm

football_jbm %>%
  ggplot(aes(y=scaledRate))+
  geom_violin(aes(x=1)) +
  geom_point(data=(football_jbm %>% filter(1:n()<=6)),aes(x=1)) +
  geom_text_repel(data=(football_jbm %>% filter(1:n()<=6)),
                  aes(label=name,x=1.25),
                  direction="y",
                  segment.size=0,
                  segment.alpha=0)


#repeat with my full football dataset:
football_sjb <- read_csv("allTopScores.csv")

football_sjb %>%
  mutate(scaledRate=(rateIncPen-mean(rateIncPen))/sd(rateIncPen),
         scaledRateExcPen=(rateExcPen-mean(rateExcPen))/sd(rateExcPen)) %>%
  arrange(desc(scaledRate))-> football_sjb


football_sjb %>%
  ggplot(aes(y=scaledRate))+
  geom_violin(aes(x=1)) +
  geom_point(data=(football_sjb %>% filter(1:n()<=10)),aes(x=1)) +
  geom_text_repel(data=(football_sjb %>% filter(1:n()<=10)),
                  aes(label=player,x=1.25),
                  direction="y",
                  segment.size=0,
                  segment.alpha=0)

#compare to f1:
f1<- read_csv("allF1Data.csv")

f1 %>%
  filter(Poles+Wins+Fastest>1) %>%
  gather(key=key,value=value,-Driver) -> f1_temp
  
f1_temp %>%
  filter(key %in% c("Scaled Pole Rate","Scaled Win Rate","Scaled Fastest Rate")) %>%
  group_by(key) %>%
  arrange(key,desc(value)) %>%
  mutate(rank=1:n()) %>%
  mutate(label=if_else(rank<=6,Driver,"")) %>%
  rename(scaled="value") %>%
  ungroup() %>%
  mutate(key=str_remove(key,"Scaled "))-> f1_trans


f1_temp %>%
  filter(key %in% c("Pole Rate","Win Rate","Fastest Rate")) %>%
  select(Driver,key,value) %>%
  left_join(f1_trans) %>%
  arrange(key,desc(value)) %>%
  mutate(label=if_else(label=="","",
                       paste0(label,
                              " (",
                              scales::percent_format(accuracy=1)(value),
                              ")"))) -> f1_trans


f1_trans %>%
  ggplot(aes(y=scaled,x=key))+
  geom_violin() +
  geom_point(data=(f1_trans %>% filter(rank<=6))) +
  geom_text_repel(aes(label=label))

#to do - add %ages to labels to show rates on original scale for noteable players

#tennis

tennis_clay <- readxl::read_xlsx("Tennis Data.xlsx")

tennis_clay %>%
  mutate(Wins=as.numeric(Wins),
         Losses=as.numeric(Losses),
         rate=Wins/(Wins+Losses),
         scaledRate=(rate-mean(rate))/sd(rate)) -> tennis_clay


tennis_clay %>%
  ggplot(aes(y=scaledRate))+
  geom_violin(aes(x=1)) +
  geom_point(data=(tennis_clay %>% filter(1:n()<=6)),aes(x=1)) +
  geom_text_repel(data=(tennis_clay %>% filter(1:n()<=6)),
                  aes(label=Player,x=1.25),
                  direction="y",
                  segment.size=0,
                  segment.alpha=0)



tennis_grass <- readxl::read_xlsx("Tennis Data.xlsx",sheet="Grass")

tennis_grass %>%
  mutate(Wins=as.numeric(Wins),
         Losses=as.numeric(Losses),
         rate=Wins/(Wins+Losses),
         scaledRate=(rate-mean(rate))/sd(rate)) %>%
  arrange(desc(rate))-> tennis_grass


tennis_grass %>%
  ggplot(aes(y=scaledRate))+
  geom_violin(aes(x=1)) +
  geom_point(data=(tennis_grass %>% filter(1:n()<=6)),aes(x=1)) +
  geom_text_repel(data=(tennis_grass %>% filter(1:n()<=6)),
                  aes(label=Player,x=1.25),
                  direction="y",
                  segment.size=0,
                  segment.alpha=0)

#fun fact - Andy Murray has a better win rate on grass than Pete Sampras and Novak Djokovic


tennis_all <- readxl::read_xlsx("Tennis Data.xlsx",sheet="All")

tennis_all %>%
  mutate(Wins=as.numeric(Wins),
         Losses=as.numeric(Losses),
         rate=Wins/(Wins+Losses),
         scaledRate=(rate-mean(rate))/sd(rate)) %>%
  arrange(desc(rate))-> tennis_all


tennis_all %>%
  ggplot(aes(y=scaledRate))+
  geom_violin(aes(x=1)) +
  geom_point(data=(tennis_all %>% filter(1:n()<=6)),aes(x=1)) +
  geom_text_repel(data=(tennis_all %>% filter(1:n()<=6)),
                  aes(label=Player,x=1.25),
                  segment.size=0,
                  segment.alpha=0)


tennis_all %>%
  select(Player,rate,scaledRate) %>%
  mutate(rank=1:n()) %>%
  mutate(label=if_else(rank<=6,
                       paste0(Player," (",
                              scales::percent_format(accuracy=1)(rate),
                              ")"),"")) %>%
  mutate(key="Tennis - Win Rate (all)") -> tennis_all_t


tennis_clay %>%
  select(Player,rate,scaledRate) %>%
  mutate(rank=1:n()) %>%
  mutate(label=if_else(rank<=6,
                       paste0(Player," (",
                              scales::percent_format(accuracy=1)(rate),
                              ")"),"")) %>%
  mutate(key="Tennis - Win Rate (clay)") -> tennis_clay_t

tennis_grass %>%
  select(Player,rate,scaledRate) %>%
  mutate(rank=1:n()) %>%
  mutate(label=if_else(rank<=6,
                       paste0(Player," (",
                              scales::percent_format(accuracy=1)(rate),
                              ")"),"")) %>%
  mutate(key="Tennis - Win Rate (grass)") -> tennis_grass_t


tennis_comb_t <- rbind(tennis_all_t,tennis_clay_t,tennis_grass_t)


#cricket:

cricket <- read_csv("cricketData.csv")

cricket %>%
  filter(Mat>=10) %>%
  filter(!is.na(Ave)) %>%
  select(Player, Ave) %>%
  mutate(rate=Ave,
         scaledRate=(Ave-mean(Ave))/sd(Ave)) %>% 
  select(-Ave) %>%
  arrange(desc(rate)) %>%
  mutate(rank=1:n()) %>%
  mutate(Player=str_remove(Player,"\\s\\([a-zA-Z0-9]+\\)")) %>%
  mutate(label=if_else(rank<=6,paste0(Player," (",rate,")"),"")) %>%
  mutate(key="Cricket - Test average")-> cricket

#need a version for all sports which looks like:
#player - key - value (scaled) - value (real) - rank - label (for top 6 + actual measure in brackets)
colnames(f1_trans) <- c("Player","key","rate","scaledRate","rank","label")
f1_trans %>%
  mutate(key=case_when(
    key=="Fastest Rate" ~ "F1 - Fastest Lap %",
    key=="Pole Rate" ~ "F1 - Pole Position %",
    key=="Win Rate" ~ "F1 - Win %"
  )) -> f1_trans

football_sjb %>%
  rename(Player="player",
         rate="rateIncPen") %>%
  select(Player,rate,scaledRate) %>%
  arrange(desc(rate)) %>%
  mutate(rank=1:n()) %>%
  mutate(label=if_else(rank<=6,
                       paste0(Player," (",
                              scales::number_format(accuracy=0.1)(rate),
                              ")"),"")) %>%
  mutate(key="Football - Goal rate (all time)") -> football_sjb_t


football_jbm %>%
  rename(Player="name") %>%
  select(Player,rate,scaledRate) %>%
  arrange(desc(rate)) %>%
  mutate(rank=1:n()) %>%
  mutate(label=if_else(rank<=6,
                       paste0(Player," (",
                              scales::number_format(accuracy=0.1)(rate),
                              ")"),"")) %>%
  mutate(key="Football - Goal rate (top scorers since 1990)") -> football_jbm_t


rbind(cricket,tennis_comb_t,f1_trans,football_sjb_t,football_jbm_t) -> chartData

write_csv(chartData,"chartData.csv")

chartData <- read_csv("chartData.csv")

chartData %>%
  ggplot(aes(y=scaledRate,x=key))+
  geom_violin() +
  geom_point(data=(chartData %>% filter(rank<=4))) +
  geom_text_repel(data=(chartData %>% filter(rank<=4)),aes(label=label)) +
  coord_flip() +
  labs(title="Bradman is further from his nearest peer in his sport than anyone else",
       subtitle="This is despite not being as an extreme outlier as others in scale")
  
#filter down main vis + format

#note that these aren't perfect measures and some are capped at certain values...

#do seperate viz for F1 (x3 measures)
#do seperate viz for Tennis (x3 measures)