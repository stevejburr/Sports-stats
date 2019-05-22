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
  mutate(sport=str_extract(key,"[a-zA-Z0-9]+ -"),
           sport=str_remove(sport," -")) -> chartData

chartData %>%
  ggplot(aes(y=scaledRate,x=key,fill=sport,colour=sport))+
  geom_violin(show.legend = F) +
  geom_point(data=(chartData %>% filter(rank<=4)),colour="grey50",show.legend = F) +
  geom_text_repel(data=(chartData %>% filter(rank<=4)),
                  aes(label=label),
                  colour="grey50") +
  coord_flip() +
  scale_x_discrete("")+
  scale_y_continuous("Performance vs Average (Std.Dev. from Mean)")+
  scale_color_manual("Sport",
                     values=c("#9CFFFA","#F5FFC6","#AB87FF","#FFACE4"),
                     aesthetics = c("colour","fill"))+
  labs(title="Sports biggest outliers",
       subtitle="Bradman's test cricket average is the most peerless record in sport.\nOther records are more extreme in scale, but no one else has such huge gap to No.2.",
       caption="Viz by @stevejburr - Data from espncricinfo, atptour.com, Wikipedia and worldfootball.net") +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        text=element_text(colour="grey50",size=16),
        axis.text=element_text(colour="grey50"),
        axis.title=element_text(colour="grey50")) -> all


#left align all titles
g1 <- ggplotGrob(all)
g1$layout
g1$layout$l[g1$layout$name == "title"] <- 2
g1$layout$l[g1$layout$name == "subtitle"] <- 2


ggsave("full.png",plot=g1,units="in",dpi=72,width=700/72,height=700/72)


mainKeys <- c("Tennis - Win Rate (clay)","Cricket - Test average","F1 - Pole Position %","Football - Goal rate (top scorers since 1990)")

chartData %>%
  filter(key %in% mainKeys)%>%
  ggplot(aes(y=scaledRate,x=key,fill=sport,colour=sport))+
  geom_violin(show.legend = F) +
  geom_point(data=(chartData %>% filter(rank<=4) %>% filter(key %in% mainKeys)),colour="grey50",show.legend = F) +
  geom_text_repel(data=(chartData %>% filter(rank<=4) %>% filter(key %in% mainKeys)),
                  aes(label=label),
                  colour="grey50") +
  coord_flip() +
  scale_x_discrete("")+
  scale_y_continuous("Performance vs Average (Std.Dev. from Mean)")+
  scale_color_manual("Sport",
                     values=c("#9CFFFA","#F5FFC6","#AB87FF","#FFACE4"),
                     aesthetics = c("colour","fill"))+
  labs(title="Sports biggest outliers",
       subtitle="Bradman's test cricket average is the most peerless record in sport.\nOther records are more extreme in scale, but no one else has such huge gap to No.2.",
       caption="Viz by @stevejburr - Data from espncricinfo, atptour.com, Wikipedia and worldfootball.net") +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        text=element_text(colour="grey50",size=16),
        axis.text=element_text(colour="grey50"),
        axis.title=element_text(colour="grey50")) -> main


#left align all titles
g2 <- ggplotGrob(main)
g2$layout
g2$layout$l[g2$layout$name == "title"] <- 2
g2$layout$l[g2$layout$name == "subtitle"] <- 2


ggsave("main.png",plot=g2,units="in",dpi=72,width=700/72,height=700/72)



chartData %>%
  filter(sport=="Tennis")%>%
  ggplot(aes(y=scaledRate,x=key,fill=sport,colour=sport))+
  geom_violin(show.legend = F) +
  geom_point(data=(chartData %>% filter(rank<=6) %>% filter(sport=="Tennis")),colour="grey50",show.legend = F) +
  geom_text_repel(data=(chartData %>% filter(rank<=6) %>% filter(sport=="Tennis")),
                  aes(label=label),
                  colour="grey50") +
  coord_flip() +
  scale_x_discrete("")+
  scale_y_continuous("Performance vs Average (Std.Dev. from Mean)")+
  scale_color_manual("Sport",
                     values=c("#9CFFFA","#F5FFC6","#AB87FF","#FFACE4"),
                     aesthetics = c("colour","fill"))+
  labs(title="Nadal is exceptional on clay, but overall all the greats have similar win rates",
       subtitle="",
       caption="Viz by @stevejburr - Data from atptour.com") +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        text=element_text(colour="grey50",size=16),
        axis.text=element_text(colour="grey50"),
        axis.title=element_text(colour="grey50")) -> tennis


#left align all titles
g3 <- ggplotGrob(tennis)
g3$layout
g3$layout$l[g3$layout$name == "title"] <- 2
g3$layout$l[g3$layout$name == "subtitle"] <- 2


ggsave("tennis.png",plot=g3,units="in",dpi=72,width=700/72,height=700/72)






chartData %>%
  filter(sport=="F1")%>%
  ggplot(aes(y=scaledRate,x=key,fill=sport,colour=sport))+
  geom_violin(show.legend = F) +
  geom_point(data=(chartData %>% filter(rank<=6) %>% filter(sport=="F1")),colour="grey50",show.legend = F) +
  geom_text_repel(data=(chartData %>% filter(rank<=6) %>% filter(sport=="F1")),
                  aes(label=label),
                  colour="grey50") +
  coord_flip() +
  scale_x_discrete("")+
  scale_y_continuous("Performance vs Average (Std.Dev. from Mean)")+
  scale_color_manual("Sport",
                     values=c("#9CFFFA","#F5FFC6","#AB87FF","#FFACE4"),
                     aesthetics = c("colour","fill"))+
  labs(title="Lewis Hamilton is one of the few modern drivers to get near the records set by the\nearly F1 greats",
       subtitle="",
       caption="Viz by @stevejburr - Data from Wikipedia") +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        text=element_text(colour="grey50",size=16),
        axis.text=element_text(colour="grey50"),
        axis.title=element_text(colour="grey50")) -> f1


#left align all titles
g4 <- ggplotGrob(f1)
g4$layout
g4$layout$l[g4$layout$name == "title"] <- 2
g4$layout$l[g4$layout$name == "subtitle"] <- 2


ggsave("f1.png",plot=g4,units="in",dpi=72,width=700/72,height=700/72)







chartData %>%
  filter(sport=="Football")%>%
  ggplot(aes(y=scaledRate,x=key,fill=sport,colour=sport))+
  geom_violin(show.legend = F) +
  geom_point(data=(chartData %>% filter(rank<=6) %>% filter(sport=="Football")),colour="grey50",show.legend = F) +
  geom_text_repel(data=(chartData %>% filter(rank<=6) %>% filter(sport=="Football")),
                  aes(label=label),
                  colour="grey50") +
  coord_flip() +
  scale_x_discrete("")+
  scale_y_continuous("Performance vs Average (Std.Dev. from Mean)")+
  scale_color_manual("Sport",
                     values=c("#9CFFFA","#F5FFC6","#AB87FF","#FFACE4"),
                     aesthetics = c("colour","fill"))+
  labs(title="Messi has an outstanding scoring rate for the modern game, but Ronaldo is also\nwell clear of the chasing pack.\nTheir feats stand out less in the longer historical context.",
       subtitle="Top 5 European Leagues and European Competitions",
       caption="Viz by @stevejburr - Data from worldfootball.net") +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        text=element_text(colour="grey50",size=16),
        axis.text=element_text(colour="grey50"),
        axis.title=element_text(colour="grey50")) -> football


#left align all titles
g5 <- ggplotGrob(football)
g5$layout
g5$layout$l[g5$layout$name == "title"] <- 2
g5$layout$l[g5$layout$name == "subtitle"] <- 2


ggsave("football.png",plot=g5,units="in",dpi=72,width=700/72,height=700/72)






chartData %>%
  filter(sport=="Cricket")%>%
  ggplot(aes(y=scaledRate,x=key,fill=sport,colour=sport))+
  geom_violin(show.legend = F) +
  geom_point(data=(chartData %>% filter(rank<=6) %>% filter(sport=="Cricket")),colour="grey50",show.legend = F) +
  geom_text_repel(data=(chartData %>% filter(rank<=6) %>% filter(sport=="Cricket")),
                  aes(label=label),
                  colour="grey50") +
  coord_flip() +
  scale_x_discrete("")+
  scale_y_continuous("Performance vs Average (Std.Dev. from Mean)")+
  scale_color_manual("Sport",
                     values=c("#9CFFFA","#F5FFC6","#AB87FF","#FFACE4"),
                     aesthetics = c("colour","fill"))+
  labs(title="Bradman's test batting average (99.9) stands alone, the five closest competitors\nare in the 60s.",
       subtitle="",
       caption="Viz by @stevejburr - Data from espncricinfo") +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        text=element_text(colour="grey50",size=16),
        axis.text=element_text(colour="grey50"),
        axis.title=element_text(colour="grey50")) -> cricket


#left align all titles
g6 <- ggplotGrob(cricket)
g6$layout
g6$layout$l[g6$layout$name == "title"] <- 2
g6$layout$l[g6$layout$name == "subtitle"] <- 2


ggsave("cricket.png",plot=g6,units="in",dpi=72,width=700/72,height=700/72)
