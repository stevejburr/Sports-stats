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
  gather(key=key,value=value,-Driver) %>%
  filter(key %in% c("Scaled Pole Rate","Scaled Win Rate","Scaled Fastest Rate")) %>%
  group_by(key) %>%
  mutate(rank=1:n()) %>%
  mutate(label=if_else(rank<=6,Driver,"")) -> f1_trans

f1_trans %>%
  ggplot(aes(y=value,x=key))+
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
  geom_point(data=(tennis_clay %>% filter(1:n()<=10)),aes(x=1)) +
  geom_text_repel(data=(tennis_clay %>% filter(1:n()<=10)),
                  aes(label=Player,x=1.25),
                  direction="y",
                  segment.size=0,
                  segment.alpha=0)
