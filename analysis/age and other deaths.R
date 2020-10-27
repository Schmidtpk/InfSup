library(tidyverse)
library(Covid)

df <- rki_new%>%
  filter(Meldedatum<as.Date("2020-09-01"))%>%
  mutate(month=lubridate::month(Meldedatum,label=TRUE))%>%
  group_by(age,month)%>%
  summarise(
    dead = sum(AnzahlTodesfall[Neuer.Todesfall%in%c(0,1)])
  ) %>%
  complete(month,age,fill=list(dead=0))%>%
  left_join(deaths%>%filter(name=="Deutschland"))%>%
  mutate(
    ratio = 12*dead/deaths*100
  )
ggplot(df%>%filter(!age %in% c("A00-A04","A05-A14"))%>%
         mutate(age=gsub("A","",age),
                age=gsub("-"," - ",age),
                age= paste(age,"years")),
       aes(x=month,y=ratio,group=age))+
  geom_line(size=1.5,col="red")+
  geom_point(size=2)+
  facet_grid(age~.,scales = "free_y")+
  theme_bw()+
  xlim(c("Feb","Mar","Apr","May","Jun","Jul","Aug"))+
  theme(axis.title.x = element_blank(),
        strip.text.y = element_text(angle = 0),
        axis.title.y = element_text(size=10),
        plot.subtitle=element_text(size=10)
        )+
  ylab("Covid deaths as percentage of all deaths")+
  labs(subtitle = "Confirmed Covid-19 deaths as percentage of total deaths (from 2018) by age group",
       caption = "Data source: https://npgeo-corona-npgeo-de.hub.arcgis.com, www.gbe-bund.de")
ggsave("./analysis/death.jpeg",height = 4,width = 6)


ggplot(df%>%filter(!age %in% c("A00-A04","A05-A14"))%>%
         mutate(age=gsub("A","",age),
                age=gsub("-"," - ",age),
                age= paste(age,"years")),
       aes(x=month,y=ratio,group=age,col=age,shape=age))+
  geom_line(alpha=.5)+
  geom_point()+
  theme_bw()+
  xlim(c("Feb","Mar","Apr","May","Jun","Jul","Aug"))+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size=10),
        plot.subtitle=element_text(size=10)
  )+
  #scale_y_log10()+
  ylab("Covid deaths as percentage of all deaths")+
  labs(subtitle = "Confirmed Covid-19 deaths as percentage of total deaths (from 2018) by age group",
       caption = "Data source: https://npgeo-corona-npgeo-de.hub.arcgis.com, www.gbe-bund.de")
ggsave("./analysis/deathall.jpeg",height = 4,width = 6)


# ftr ---------------------------------------------------------------------

onset_t <- rki_new %>%
  group_by(Refdatum,age,gender)%>%
  summarise(
    pos.new = sum(AnzahlFall[Neuer.Fall%in%c(0,1)]),
    dead.new = sum(AnzahlTodesfall[Neuer.Todesfall%in%c(0,1)]))%>%
  rename(date=Refdatum)

onset_t %>%
  filter(date<as.Date("2020-08-15"),
         date>=as.Date("2020-03-01")) %>%
  mutate(
    week = lubridate::isoweek(date),
    month = lubridate::month(date)) %>%
  group_by(age,month,gender) %>%
  summarise(
    date = min(date),
    ftr = sum(dead.new)/sum(pos.new)*100,
    sd = ftr*(1-ftr)/sqrt(sum(pos.new)))%>%
  filter(!age %in% c("A00-A04","A05-A14","A15-A34"))%>%
  ungroup()%>%
  mutate(
    eftr=ifr_estimate(age,weights = TRUE)
  )%>%
  ggplot(aes(x=date,y=ftr,col=gender))+
  geom_line()+geom_point()+
  #geom_errorbar(aes(ymin=ftr-2.57*sd,ymax=ftr+2.57*sd))+
  facet_wrap(vars(age),scales="free_y")+
  geom_hline(yintercept = 0)+ylab("symptomatic cfr")+
  theme_classic()+
  geom_hline(aes(yintercept = eftr))


report_t <- rki_new %>%
  group_by(Meldedatum,age,gender)%>%
  summarise(
    pos.new = sum(AnzahlFall[Neuer.Fall%in%c(0,1)]),
    dead.new = sum(AnzahlTodesfall[Neuer.Todesfall%in%c(0,1)]))%>%
  rename(date=Meldedatum)

report_t %>%
  filter(date<as.Date("2020-08-01"),
         date>=as.Date("2020-03-01")) %>%
  mutate(
    week = lubridate::isoweek(date),
    month = lubridate::month(date)) %>%
  group_by(age,month,gender) %>%
  summarise(
    date = min(date),
    ftr = sum(dead.new)/sum(pos.new)*100,
    sd = ftr*(1-ftr)/sqrt(sum(pos.new)))%>%
  filter(!age %in% c("A00-A04","A05-A14"))%>%
  ungroup()%>%
  mutate(
    eftr=ifr_estimate(age,weights = TRUE)
  )%>%
  ggplot(aes(x=date,y=ftr,col=gender))+
  geom_hline(aes(yintercept = eftr),size=1.5)+
  geom_line()+geom_point()+
  #geom_errorbar(aes(ymin=ftr-2.57*sd,ymax=ftr+2.57*sd))+
  facet_wrap(vars(age),scales="free_y")+
  geom_hline(yintercept = 0)+ylab("cfr/ifr (in %)")+
  theme_classic()+
  theme(axis.title.x = element_blank())+
  labs(subtitle = "Case fatality rate and estimated infection fatility rate (horizontal line)",
       caption = "Data source: https://npgeo-corona-npgeo-de.hub.arcgis.com, Regionaldatenbank")
ggsave("./analysis/cfr.jpeg",height = 4,width = 6)
