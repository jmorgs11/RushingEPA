#library(devtools)
#devtools::install_github("mrcaseb/nflfastR")
library(nflfastR)
library(dplyr)
library(tidyverse)
library(nflscrapR)
library(RCurl)
library(XML)
library(ggplot2)
library(randomcoloR)
library(ggrepel)
library(stringr)


NFL2019pbp <- scrape_season_play_by_play(2019,"reg")

read_pbp_rds <- function(year){
  readRDS(url(glue::glue('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_{year}.rds')))
}

NFL_pbp2019 <- 2019 %>% 
  purrr::map_dfr(read_pbp_rds)


NFL_pbp_19 <- all_pbp %>% filter(game_date >= "2019-05-05")

tail(NFL_pbp2019, n = 10)

NFL_pbp2019<-NFL_pbp2019 %>% filter(game_date <= "2020-01-01")




NFL_pbp2019  %>% select(posteam, play_type, desc, epa) %>% filter(play_type == "run")

EPA_TeamRush<- function(team){
  data = NFL_pbp2019  %>% select(posteam, desc, epa, play_type) %>% filter(play_type == "run", posteam == team)
  data = na.omit(data)
  return(as.numeric(sum(data[,3])))
}
EPA_TeamRush("BAL")

unique(NFL_pbp2019$home_team)

TeamRushingEPA<-data.frame(unique(NFL_pbp2019$home_team))
TeamRushingEPA$Team<-TeamRushingEPA$unique.NFL_pbp2019.home_team.
TeamRushingEPA<-TeamRushingEPA %>% select(Team)


TeamRushingEPA$Total_Rushing_EPA<-capture.output(for (i in TeamRushingEPA$Team) {
  cat(EPA_TeamRush(i),"\n")
})

TeamRushingEPA$Total_Rushing_EPA<-as.numeric(TeamRushingEPA$Total_Rushing_EPA)



EPA_TeamPass<- function(team){
  data = NFL_pbp2019  %>% select(posteam, desc, epa, play_type) %>% filter(play_type == "pass", posteam == team)
  data = na.omit(data)
  return(as.numeric(sum(data[,3])))
}

TeamRushingEPA$Total_Passing_EPA<-capture.output(for (i in TeamRushingEPA$Team) {
  cat(EPA_TeamPass(i),"\n")
})

TeamRushingEPA$Total_Passing_EPA<-as.numeric(TeamRushingEPA$Total_Passing_EPA)
TeamEPA<-TeamRushingEPA

TeamEPARushAtt<- function(team){
  data = NFL_pbp2019  %>% select(posteam, desc, epa, play_type) %>% filter(play_type == "run", posteam == team)
  data = na.omit(data)
  return(as.numeric(sum(data[,3]))/(as.numeric(nrow(data))))
}
TeamEPARushAtt("BAL")


TeamEPA$EPA_per_RushAtt<-capture.output(for (i in TeamEPA$Team) {
  cat(TeamEPARushAtt(i),"\n")
})
TeamEPA$EPA_per_RushAtt<-as.numeric(TeamEPA$EPA_per_RushAtt)

TeamEPAPassAtt<- function(team){
  data = NFL_pbp2019  %>% select(posteam, desc, epa, play_type) %>% filter(play_type == "pass", posteam == team)
  data = na.omit(data)
  return(as.numeric(sum(data[,3]))/(as.numeric(nrow(data))))
}


TeamEPA$EPA_per_PassAtt<-capture.output(for (i in TeamEPA$Team) {
  cat(TeamEPAPassAtt(i),"\n")
})
TeamEPA$EPA_per_PassAtt<-as.numeric(TeamEPA$EPA_per_PassAtt)


ggplot(TeamEPA, aes(EPA_per_RushAtt,EPA_per_PassAtt, label = Team, color = Playoff))+
  geom_text(size = 3) +
  geom_vline(xintercept = 0)+
  geom_hline(yintercept = 0)+
  xlab("EPA per Rush Attempt")+
  ylab("EPA per Pass Attempt")

TeamEPA$Playoff[is.na(TeamEPA$Playoff)]<-0


###############################################################
RusherEPA<-data.frame(unique(NFL_pbp2019$rusher))
RusherEPA$Name<-RusherEPA$unique.NFL_pbp2019.rusher.
RusherEPA<-RusherEPA[-1,]
RusherEPA <- RusherEPA %>% select(Name)


NFL_pbp2019 %>% select(posteam, desc, epa, play_type, rusher) %>% filter(play_type == "run", rusher == "A.Jones")



RusherEPAfunc<-function(name){
  data = NFL_pbp2019 %>% select(posteam, desc, epa, play_type, rusher) %>% filter(str_detect(desc, name)) %>% filter(play_type=="run") %>% filter(rusher == name)
  data = na.omit(data)
  return(as.numeric(sum(data[,3]))/(as.numeric(nrow(data))))
}

TotalRusherEPA<-function(name){
  data = NFL_pbp2019 %>% select(posteam, desc, epa, play_type, rusher) %>% filter(str_detect(desc, name)) %>% filter(play_type=="run") %>% filter(rusher == name)
  data = na.omit(data)
  return(as.numeric(sum(data[,3])))
}

Rushers<-function(name){
  data = NFL_pbp2019 %>% select(posteam, desc, epa, play_type, rusher) %>% filter(str_detect(desc, name)) %>% filter(play_type=="run") %>% filter(rusher == name)

  return((as.numeric(nrow(data))))
}



RusherEPAfunc("J.Williams")
Rushers("A.Jones")
RusherEPA$EPA_per_Rush<-as.numeric(RusherEPA$EPA_per_Rush)

RusherEPA$EPA_per_Rush<-NA
RusherEPA$EPA_per_Rush<-capture.output(for (i in RusherEPA$Name) {
  cat(RusherEPAfunc(i),"\n")
})

RusherEPA$Rush_Att<-capture.output(for (i in RusherEPA$Name) {
  cat(Rushers(i),"\n")
})

RusherEPA$Rush_Att<-as.numeric(RusherEPA$Rush_Att)
Rushers("M.Trubisky")
RusherEPA<-RusherEPA %>% filter(Rush_Att >= 50)

RusherEPA$Rush_Att<-as.numeric(RusherEPA$Rush_Att)
RusherEPA$EPA_per_Rush<-as.numeric(RusherEPA$EPA_per_Rush)


RusherEPAdata<-function(name){
  data = NFL_pbp2019 %>% select(posteam, desc, epa, play_type, rusher) %>% filter(str_detect(desc, name)) %>% filter(play_type=="run") 
  data = na.omit(data)
  data$Num <- seq.int(nrow(data))
  data$cumsum<-cumsum(data$epa)
  return(data)
}
JWillPack = RusherEPAdata("25-D.Johnson")

RusherEPAfunc("M.Ingram")

RushEPAPlot<-ggplot(RusherEPAdata("B.Scott"),aes(Num,cumsum))+
  geom_line()  +
  geom_vline(xintercept = 0)+
  geom_hline(yintercept = 0)+
  geom_text(data=. %>% 
              arrange(desc(Num)) %>% 
              group_by(rusher) %>% 
              slice(1), 
            aes(label=rusher), 
            size = 2.5,
            position=position_nudge(0.1), hjust=0, show.legend=FALSE)



for (i in RusherEPA$Name){
  RushEPAPlot<-RushEPAPlot + geom_line(data = RusherEPAdata(i), color = randomColor(luminosity = "dark")) +   geom_text(data=RusherEPAdata(i) %>%
                                                                                                                          arrange(desc(Num)) %>% 
                                                                                                                          group_by(rusher) %>% 
                                                                                                                          slice(1), 
                                                                                                                        aes(label=rusher), 
                                                                                                                        size = 2.75,
                                                                                                                        position=position_nudge(0.1), hjust=0, show.legend=FALSE) 
}

geom_text(data = RusherEPAdata(i), aes(label = i, x = Num, y = cumsum, color = i))

RushEPAPlot + xlab("Rush Attempts") + ylab("Cumulative EPA") +ggtitle("2019 Rushing EPA")
RushEPAPlot + continuous_scale()


TotalRusherEPA("M.Ingram")


RusherEPA$Total_Rush_EPA<-capture.output(for (i in RusherEPA$Name) {
  cat(TotalRusherEPA(i),"\n")
})
RusherEPA$Total_Rush_EPA<-as.numeric(RusherEPA$Total_Rush_EPA)



tail(RusherEPAdata("L.Jackson"))[6,7]

install.packages("viridis")  # Install
library(viridis) 

ggplot(RusherEPA, aes(EPA_per_Rush,Total_Rush_EPA, label = Name, color = Rush_Att))+
  geom_point()+
  geom_text(size = 2.5, nudge_y =1.5) +
  geom_vline(xintercept = 0)+
  geom_hline(yintercept = 0)+
  xlab("EPA per Rush Attempt")+
  ylab("Total Rushing EPA")+
  scale_color_gradient(low = "orange", high = "purple", na.value = NA)+
  theme_bw()

scale_col

scale_colo






NFL_pbp2019 %>% select(posteam, desc, epa, play_type, rusher) %>% filter(str_detect(desc, "A.Jones")) %>% filter(play_type=="run")



