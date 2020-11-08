library(Covid)
library(RCovModel)
library(tidyverse)
library(latex2exp)

start.date <- as.Date("2020-02-15")
end.date1 <- as.Date("2020-05-15")
end.date2 <- as.Date("2020-09-01")

res.path <- "./jags models/submission/save/draft_heinsbergXX11-06.RData"


# number of symptomatic cases
rki_new%>%
  filter(Meldedatum<=end.date1)%>%
  summarise(sum(AnzahlFall[Neuer.Fall%in% c(0,1)]))

rki_new%>%
  filter(!is.na(Refdatum),
         Meldedatum<=end.date1)%>%
  summarise(sum(AnzahlFall[Neuer.Fall%in% c(0,1)]))

df.cur <- rki_new%>%
  filter(!is.na(Refdatum),
         Refdatum<=end.date1)%>%
  group_by(age)%>%
  summarise(sum(AnzahlFall[Neuer.Fall%in% c(0,1)]))

sum(df.cur[1:2,2])/sum(df.cur[,2])

# +++ RESULTS BASE +++ -----------------------------------------------------------------

age.cur <-  NULL#c("A15-A34","A35-A59","A60-A79","A80+")
total.age <- FALSE
shape.cur <- c(2,0,1,8,16)
CI.large.cur <- TRUE

theme_set(theme_bw())

load(res.path)
cov.main <- setdiff(c(macro$cov,setdiff(macro$dummies,macro$dummies.FE)),
                    c(#"sports",
                      "shops2",
                      "holidays",
                      "events",
                      "bars",
                      "eventslimited",
                      "symptomatic testing",
                      "speechMerkel",
                      "masks recommended"))


macro$notes
show_basics()

# number of symptomatic cases
data%>%
  summarise(sum(pos.new),
            sum(dead.new))

# number of counties in data
data$name%>%unique%>%length()

# number of counties in states with intervetions
rki_new%>%filter(Bundesland %in% unique(interventions %>%
                                           filter(all_interventions_exist)%>%
                                           pull(Bundesland)))%>%pull(Landkreis)%>%unique

# all effects -------------------------------------------------------------
show_effects_sample(
  cov = c(macro$cov,
          setdiff(macro$dummies,macro$dummies.FE)),
  filter_age = age.cur,
  background_prior = TRUE,
  ncol=2)+
  scale_shape_manual(values=shape.cur)+
  scale_x_discrete(label = my_labeller)+
  scale_y_continuous(labels = function(x) paste0(x,"%"))
  ggsave("./analysis/tex/plotsnew/res_all.pdf",
       width = 6,height=8)

show_effects_sample(cov = cov.main,
                    filter_age = age.cur,
                    average.age = TRUE,
                    order = TRUE,
                    background_prior = FALSE,
                    CI_large = TRUE,
                    ncol=2,width_errorbar = .5)+
  guides(shape=F)+
  scale_y_continuous(name="change in transmission",
                     labels = function(x) paste0(x,"%"))+
  ggtitle("Intervention/covariate")+
  theme(plot.title = element_text(size = 9,hjust=-1.6,face="bold"))

ggsave("./analysis/tex/plotsnew/res.pdf",
       width = 4,height=4)

show_effects_sample(c(macro$dummies.FE),
                    filter_age = age.cur,
                    background_prior = FALSE,
                    CI_large = TRUE,
                    ncol=2)+
  scale_shape_manual(values=shape.cur)+
  scale_y_continuous(name="effect estimate",
                     labels = function(x) paste0(x,"%"))+
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank()
  )
ggsave("./analysis/tex/plotsnew/res_wday.pdf",
       width = 4,height=5)


# effects table -----------------------------------------------------------

df.eff <- extract_effects(cov.main)


my_rounding <- function(x) round(x,digits = 2)

# average
df.eff %>%
  group_by(m) %>%
  left_join(Covid::regionaldatenbank%>%
              filter(adm.level==1)%>%
              select(age,total)) %>%
  group_by(m,iteration)%>%
  summarise(
    draw = weighted.mean(draw,w = total),
    age = "average"
  ) %>%
  mutate(draw=draw*100)%>%
  summarise(
    mean = my_rounding(mean(draw)),
    up = my_rounding(quantile(draw,0.975)),
    down = my_rounding(quantile(draw, 0.025)),
    out = paste0(mean, "[",down,",",up,"]")
  )%>%select(m,
             #age,
             out)%>%
  arrange(m
          #,age
          )%>%
  mutate(#age=age_labels(age),
         m=my_labeller2(as.character(m)))%>%
  #pivot_wider(names_from = age,values_from=out)%>%
  rename(covariate=m)#%>%
  #xtable::xtable()%>%print(include.rownames=FALSE)

# by age
df.eff %>%
  group_by(age,
    m) %>%
  mutate(draw=draw*100)%>%
  summarise(
    mean = my_rounding(mean(draw)),
    up = my_rounding(quantile(draw,0.975)),
    down = my_rounding(quantile(draw, 0.025)),
    out = paste0(mean, "[",down,",",up,"]")
  )%>%select(m,
             age,
             out)%>%
  arrange(m
          ,age
  )%>%
  mutate(age=age_labels(age),
    m=my_labeller2(as.character(m)))%>%
  #pivot_wider(names_from = age,values_from=out)%>%
  rename(covariate=m)#%>%
  #xtable::xtable()%>%print(include.rownames=FALSE)


# total weather predictions -----------------------------------------------------------------
p.weather <- show_total_effect(
                  choose_age = total.age,
                  average_age = TRUE,
                  forecast = TRUE,
                  smooth = 14)+
  scale_x_date(date_labels = "%b",
               breaks='2 months')+
  guides(color=F)+
  theme_bw()+
  theme(axis.title.x=element_blank())+
  facet_grid(age~.)+
  scale_y_continuous(name="seasonal effect",
                     labels = function(x) paste0(x,"%"))+
  theme(
    strip.background = element_blank(),
    strip.text.y = element_blank(),
    axis.title.x=element_blank())
p.weather


# total traced -----------------------------------------------------------------
p.traced <- show_total_effect(average_c = TRUE,
                              average_age = TRUE,
                  choose_age = total.age,
                  covariates = c("traced"))+
  scale_y_continuous(name="test & trace effect",
                     labels = function(x) paste0(x,"%"))+
  theme(
    strip.background = element_blank(),
    strip.text.y = element_blank(),
    axis.title.x=element_blank())
p.traced


# total cumsum ------------------------------------------------------------
data %>% filter(age==unique(age)[1])%>%
  group_by(name) %>% filter(date== max(date)) %>% select(name,cumsum_incidence100) %>% ungroup() %>%
  filter(cumsum_incidence100==max(cumsum_incidence100))
show_total_effect(choose_c = "LK Tirschenreuth",
                  choose_age = total.age,
                  average_age = TRUE,
                  covariates = c("cumsum_incidence100"))+
  scale_y_continuous(name="effect of immunity",
                     labels = function(x) paste0(x,"%"))+
  theme(
    strip.background = element_blank(),
    strip.text.y = element_blank(),
    axis.title.x=element_blank())
ggsave("./analysis/tex/plotsnew/cumsum_Tirschenreuth.pdf",
       width = 5,height=5)





# total info ------------------------------------------------------------
show_total_effect(choose_c = "LK Tirschenreuth",
                  choose_age = total.age,
                  average_age = TRUE,
                  covariates = c("info_lincidence"))+
  scale_y_continuous(name="information effect",
                     labels = function(x) paste0(x,"%"))+
  theme(
    strip.background = element_blank(),
    strip.text.y = element_blank(),
    axis.title.x=element_blank())
ggsave("./analysis/tex/plotsnew/info_Tirschen.pdf",
       width = 5,height=5)

p.info <- show_total_effect(average_c = TRUE,
                  choose_age = total.age,
                  average_age = TRUE,
                  covariates = c("info_incidence",
                                 "info_lincidence"))+
  scale_y_continuous(name="information effect",
                     labels = function(x) paste0(x,"%"))+
  theme(
    strip.background = element_blank(),
    strip.text.y = element_blank(),
    axis.title.x=element_blank())
p.info
ggsave("./analysis/tex/plotsnew/info_average.pdf",
       width = 5,height=5)

# total all ---------------------------------------------------------------
ggpubr::ggarrange(ggpubr::ggarrange(p.traced,p.info,labels = c("A","B")),
                  p.weather,labels = c("","C"),
                  ncol=1)
ggsave("./analysis/tex/plotsnew/total_all.pdf",
       width = 4,height=4)
# totoal diff info -----------------------------------------------------------


# nothing to max
max.inc <- data%>%
  group_by(date)%>%
  summarise(
    info = mean(info_incidence)
  )%>%pull(info)%>%max()
total_diff_effect(rbind(
  #data.frame(m="info_incidence", value1 = 50/10e6,value2=0),
  data.frame(m="info_lincidence",
             value1 = log(max.inc+1,base=10),
             value2= log(1,base=10))),
  average = TRUE
)


# nothing to 50/100k
total_diff_effect(rbind(
  #data.frame(m="info_incidence", value1 = 50/10e6,value2=0),
  data.frame(m="info_lincidence",
             value1 = log(50+1,base=10),
             value2= log(1,base=10))),
  average = TRUE
)

# nothing to 300/100k
total_diff_effect(rbind(
  #data.frame(m="info_incidence", value1 = 50/10e6,value2=0),
  data.frame(m="info_lincidence",
             value1 = log(300+1,base=10),
             value2= log(1,base=10))),
  average = TRUE
)

# nothing to 1000/100k
total_diff_effect(rbind(
  #data.frame(m="info_incidence", value1 = 50/10e6,value2=0),
  data.frame(m="info_lincidence",
             value1 = log(1000+1,base=10),
             value2= log(1,base=10))),
  average = TRUE
)

### implications of 500/100k in deaths
c(300,1000)*800*2*ifr_estimate(age = 1:80,weights = TRUE)/100

# total diff weather ------------------------------------------------------

num.month1 <- 7
num.month2 <- 1
df.diff.weather<-  rbind(
  data.frame(m="UPM.Relative_Feuchte",
             value1 = weather_dwd%>%filter(month(date)%in%num.month1)%>%pull(UPM.Relative_Feuchte)%>%mean(na.rm=TRUE),
             value2 = weather_dwd%>%filter(month(date)%in%num.month2)%>%pull(UPM.Relative_Feuchte)%>%mean(na.rm=TRUE)),
  data.frame(m="TMK.Lufttemperatur",
             value1 = weather_dwd%>%filter(month(date)%in%num.month1)%>%pull(TMK.Lufttemperatur)%>%mean(na.rm=TRUE),
             value2 = weather_dwd%>%filter(month(date)%in%num.month2)%>%pull(TMK.Lufttemperatur)%>%mean(na.rm=TRUE)))

total_diff_effect(
  df.diff.weather,
  average = TRUE
)



# total diff traced -------------------------------------------------------

# observed effect in data
total_diff_effect(
  data.frame(m="traced",
             value1 = 0,
             value2 = data%>%
               filter(month(date)==5)%>%
               summarise(
                 traced = mean(traced)
               )%>%
               pull(traced)),
  average = TRUE
)

# observed effect in data for old
total_diff_effect(
  data.frame(m="traced",
             value1 = 0,
             value2 = data%>%
               filter(month(date)==5)%>%
               summarise(
                 traced = mean(traced)
               )%>%
               pull(traced)),
  choose_age = c("A80+")
)


# all with reporting rate
total_diff_effect(
  data.frame(m="traced",
             value1 = 0,
             value2 = 4),
  average = TRUE
)


# young
total_diff_effect(
  data.frame(m="traced",
             value1 = 0,
             value2 = 4),
  choose_age = c("A15-A34","A35-A59"),
  average = TRUE
)


# old
total_diff_effect(
  data.frame(m="traced",
             value1 = 0,
             value2 = 1),
  choose_age = c("A60-A79","A80+"),
  average = TRUE
)

# total cumsum -------------------------------------------------------
total_diff_effect(
  data.frame(m="cumsum_incidence100",
             value1 = 0,
             value2 = 1.2),average = TRUE)


total_diff_effect(
  data.frame(m="cumsum_incidence100",
             value1 = 0,
             value2 = 1),choose_age = c("A80+"))



# transmission  --------------------------------------------------------------


# + distribution plots ----------------------------------------------------
breaks.cur <- (-3:14)
### SI distribution
df.si <- RCovModel::pull_mcmc_sample("SI_dist")
p1 <- ggplot(data.frame(onset = df.si),
       aes(x=onset,y=..density..))+
  geom_histogram(breaks = breaks.cur,fill="white",col="black")+
  geom_vline(xintercept = mean(df.si),col="red")+
  xlab("incubation period distribution")+
  theme(axis.title.y = element_blank())


### generation distribution
df.trans <- RCovModel::pull_mcmc_sample("transmission_dist")
p2 <- ggplot(data.frame(trans = df.trans),
       aes(x=trans,y=..density..))+
  geom_histogram(breaks = breaks.cur,fill="white",col="black")+
  geom_vline(xintercept = mean(df.trans),col="red")+
  xlab("generation time distribution")+
  theme(axis.title.y = element_blank())


df.serial.interval <- data.frame(trans=df.trans,onset = df.si)%>%
  mutate(onset2 = sample(onset),
         si = trans+onset-onset2,
         before = trans<onset)
p3 <- ggplot(df.serial.interval,
             aes(x=si,y=..density..))+
  geom_histogram(breaks = breaks.cur,fill="white",col="black")+
  geom_vline(xintercept = mean(df.serial.interval$si),col="red")+
  xlab("serial interval distribution")+
  theme(axis.title.y = element_blank())


ggpubr::ggarrange(p1,p2,p3,ncol = 1)
ggsave("./analysis/tex/plotsnew/dist.pdf",
       width = 4,height=4)


hist(df.serial.interval$si)
mean(df.serial.interval$si)
quantile(df.serial.interval$si,c(0.025,0.975))

mean(df.serial.interval$si<0)
mean(df.serial.interval$before)

# generation time distribution mean
df.m.trans <- pull_mcmc_sample("mean_transmission")
mean(df.m.trans)
quantile(df.m.trans,c(0.025,0.975))

# incubation period distribution mean
df.m.inc <- pull_mcmc_sample("mean_SI")
mean(df.m.inc)
quantile(df.m.inc,c(0.025,0.975))


### dispersion
disp_sample <- extract_mcmc(name = "i_disp",
                            dim_names = c("age"),
                            quantiles = FALSE,samples = TRUE)
disp_sample$prob.zero <- sapply(disp_sample$i_disp,
                                function(x) mean(rnbinom(200,mu=1,size=x)==0))
df.zero.secondary <- disp_sample %>% group_by(age) %>%
  summarise(
    Meanzero = 1-mean(prob.zero),
    SDzero = sd(prob.zero)
  )

disp_sample$prob20 <- sapply(disp_sample$i_disp,
                                function(x) compute_dispersion_percentage(x,R=1))
df.prob20 <- disp_sample %>%
  select(age,prob20)%>%
  group_by(age) %>%
  summarise(
    Meanzero = mean(prob20),
    SDzero = sd(prob20)
  )%>%rename(
    Meanp20 = Meanzero,
    SDp20 = SDzero
  )

# summary
df.disp <- extract_mcmc("i_disp","age")

### tau (initial infections)
df.tau.a <- extract_mcmc("tau", c("name","age"),samples = TRUE)%>%
  group_by(age) %>%
  summarise(
    Mean = mean(tau),
    SD = sd(tau)
  )

df.tau <- extract_mcmc("tau", c("name","age"),samples = TRUE)%>%
  group_by(name) %>%
  summarise(
    Mean = mean(tau),
    SD = sd(tau)
  )



### R0
df.R0 <- extract_mcmc("Rzero", c("name","age"),samples = TRUE)%>%
  group_by(age,name) %>%
  summarise(
    Mean = mean(Rzero),
    SD = sd(Rzero)
  )

df.R0.a <- df.R0 %>% group_by(age)%>%
  summarise(
    Mean = mean(Mean),
    SD = mean(SD)
  )


# + table -----------------------------------------------------------------
print(
  xtable::xtable(
    df.R0.a %>%
      left_join(
        df.disp%>%select(age,Mean,SD),
        by = "age",
        suffix = c("R0","disp"))%>%
      left_join(
        df.zero.secondary%>%select(age,Meanzero,SDzero),
        by = "age")%>%
      left_join(
        df.prob20%>%select(age,Meanp20,SDp20),
        by = "age")%>%
      # left_join(
      #   df.tau.a%>%select(age,Mean,SD),
      #   by = "age",
      #   suffix = c("","tau"))%>%
      mutate(age = age_labels(age))),
    include.rownames=FALSE)


# + average dispersion ----------------------------------------------------



### equivalent dispersion to mean/variance ratio of age weighted distribution of sec. inf
df.cur <- disp_sample%>%
  left_join(
    regionaldatenbank%>%
      filter(adm.level==1)%>%
      mutate(ratio=total/sum(total))%>%
      select(age,ratio))%>%
  mutate(ratio = ratio / max(ratio))

df.cur$sample <- NA
for(i in 1:nrow(df.cur)){
  df.cur$sample[i] <-
    list(rnbinom(df.cur$ratio[i]*1e4,
            mu = 1,
            size = df.cur$i_disp[i]))
}

df.cur %>%
  group_by(iteration)%>%
  summarise(
    mean = mean(unlist(sample)),
    var = var(unlist(sample)),
    meanvar = mean/var,
    disp = mean^2/(var-mean)
  ) %>% ungroup()%>%
  summarise(
    meandisp = mean(disp),
    q025disp = quantile(disp,probs = c(0.025)),
    q975disp = quantile(disp,probs = c(0.975)),
    meanmeanvar = mean(meanvar),
    q025meanvar = quantile(meanvar,probs = c(0.025)),
    q975meanvar = quantile(meanvar,probs = c(0.975)))


# + dispersion example ------------------------------------------------------


data %>% group_by(name,date)%>%
  summarise(
    pos.new = sum(pos.new)
  )%>%
  group_by(name)%>%
  arrange(date)%>%
  mutate(
    posl = dplyr::lag(x = pos.new,n = 7),
    growth = pos.new/posl
  ) %>% select(name,date,posl,growth,pos.new)%>%
  filter(#pos.new>=10,
    posl>=10)%>%
  ggplot(aes(y=growth,x=posl))+geom_point(alpha=.2)+
  geom_hline(yintercept = 1,col="red")+
  scale_x_log10()+
  xlab("number initial cases")+
  ylab("7-day growth rate")


# + meta regression -------------------------------------------------------

df.R0_name <- df.R0 %>%
  group_by(name)%>%
  summarise(Mean = mean(Mean))%>%
  left_join(data %>%
              group_by(name) %>%
              slice(1),
            by = c("name"), suffix = c("","name")) %>%
  mutate(
    rural = grepl(pattern = "LK",name,ignore.case = F)
  ) %>%
  mutate(
    easternGer = Bundesland %in% c("Berlin",
                                   "Brandenburg",
                                   "Thüringen",
                                   "Sachsen",
                                   "Sachsen-Anhalt",
                                   "Mecklenburg-Vorpommern")
  )%>% left_join(
    data%>%group_by(name,age)%>%slice(1)%>%
      group_by(name)%>%
      summarise(
        children = sum(pop[age %in% c("A00-A04","A05-A14")])/sum(pop),
        young = sum(pop[age == "A15-A34"])/sum(pop),
        middle = sum(pop[age == "A35-A59"])/sum(pop),
        old = sum(pop[age %in% c("A60-A79","A80+")])/sum(pop),
      )) %>%
  mutate(
    latitude = as.numeric(latitude),
    longitude = as.numeric(longitude)
    )

lm <-lm(Mean ~ easternGer+
             standardise(density)
           +rural+
          #standardise(children)+
          standardise(young)+
          standardise(middle)
        # +
        #   standardise(latitude)+
        #   standardise(longitude)
           , df.R0_name)


lm2 <- lm(first ~ easternGer+
            standardise(density)
          +rural+
            #standardise(children)+
            standardise(young)+
            standardise(middle)
          #+standardise(latitude)+
           # standardise(longitude)
           ,
          df.R0_name %>% left_join(data.frame(first = dat$Ti, name = names(dat$Ti))))


lm3 <- lm(Mean ~ easternGer+
            standardise(density)
          +rural+
            #standardise(children)+
            standardise(young)+
            standardise(middle)
          #+standardise(latitude)+
          # standardise(longitude)
          ,
          df.tau%>%
            left_join(data %>%
                        group_by(name) %>%
                        slice(1),
                      by = c("name"), suffix = c("","name")) %>%
            mutate(
              rural = grepl(pattern = "LK",name,ignore.case = F)
            ) %>%
            mutate(
              easternGer = Bundesland %in% c("Berlin",
                                             "Brandenburg",
                                             "Thüringen",
                                             "Sachsen",
                                             "Sachsen-Anhalt",
                                             "Mecklenburg-Vorpommern")
            )%>% left_join(
              data%>%group_by(name,age)%>%slice(1)%>%
                group_by(name)%>%
                summarise(
                  children = sum(pop[age %in% c("A00-A04","A05-A14")])/sum(pop),
                  young = sum(pop[age == "A15-A34"])/sum(pop),
                  middle = sum(pop[age == "A35-A59"])/sum(pop),
                  old = sum(pop[age %in% c("A60-A79","A80+")])/sum(pop),
                )) %>%
            mutate(
              latitude = as.numeric(latitude),
              longitude = as.numeric(longitude)
            ))

stargazer::stargazer(lm,lm2,lm3,
                     covariate.labels = c("Eastern Germany",
                                          "population density",
                                          "rural (no city)",
                                          #"ratio age group 0-14",
                                          "ratio age group 15-34",
                                          "ratio age group 35-59",
                                          # "latitude",
                                          # "longitude",
                                          "average"),
                     omit.stat = c("f","adj.rsq"))



# week days ---------------------------------------------------------------
extract_effects(c("FEMon","FETue","FEWed","FEThu","FEFri","FESat","FESun"))%>%
  group_by(age,m)%>%
  summarise(mean = mean(draw))%>%
  pivot_wider(names_from = m,values_from = mean)


# correlation estimates -------------------------------------------------------------

### average effect across age
effects <- extract_effects(c(macro$cov,setdiff(macro$dummies,macro$dummies.FE)))
effects %>%
  left_join(Covid::regionaldatenbank%>%
              filter(adm.level==1)%>%
              select(age,total))%>%
  group_by(iteration,m)%>%
  summarise(draw = weighted.mean(draw,w = total)) %>%
  mutate(age="average")%>%
  filter(m %in% cov.main)%>%
  pivot_wider(names_from = m,values_from = draw) %>%
  select(-c(age,iteration))%>%
  illustrate_corr()+
  scale_y_discrete(label = my_labeller2)+
  scale_x_discrete(label = my_labeller2)
ggsave("./analysis/tex/plotsnew/estimates_corr_ave.pdf",
       width = 8,height=8)

effects %>%
  left_join(Covid::regionaldatenbank%>%
              filter(adm.level==1)%>%
              select(age,total))%>%
  group_by(iteration,m)%>%
  summarise(draw = weighted.mean(draw,w = total)) %>%
  mutate(age="average")%>%
  pivot_wider(names_from = m,values_from = draw) %>%
  select(-c(age,iteration))%>%
  illustrate_corr()+
  scale_y_discrete(label = my_labeller)+
  scale_x_discrete(label = my_labeller)
ggsave("./analysis/tex/plotsnew/estimates_corr_all_ave.pdf",
       width = 9,height=9)


### age group specific
cor.age <- "A35-A59"
effects <- extract_effects(c(macro$cov,setdiff(macro$dummies,macro$dummies.FE)))
effects %>%
  filter(m %in% cov.main)%>%
  filter(age %in% cor.age)%>%
  pivot_wider(names_from = m,values_from = draw) %>%
  select(-c(age,iteration))%>%
  illustrate_corr()+
  scale_y_discrete(label = my_labeller2)+
  scale_x_discrete(label = my_labeller2)
ggsave("./analysis/tex/plotsnew/estimates_corr.pdf",
       width = 8,height=8)


effects %>%
  pivot_wider(names_from = m,values_from = draw) %>%
  filter(age %in% cor.age)%>%
  select(-c(age,iteration))%>%
  illustrate_corr()+
  scale_y_discrete(label = my_labeller)+
  scale_x_discrete(label = my_labeller)
ggsave("./analysis/tex/plotsnew/estimates_corr_all.pdf",
       width = 9,height=9)


# R estimate --------------------------------------------------------------

p <- show_total_effect(c(macro$cov,macro$dummies.interventions),
                       draws_max = 100,
                       average_c = TRUE,
                       absoluteR = TRUE)


dat.interventions <- data %>%
  filter(age == unique(age)[1])%>%
  pivot_longer(setdiff(cov.main,macro$cov), names_to = "cov") %>%
  group_by(cov,date)%>%
  summarise(ratio = mean(value))%>%
  filter(ratio>.5)%>%
  group_by(cov)%>%
  summarise(date = min(date))%>%
  select(date,cov)

p+
  geom_vline(data = dat.interventions,
             aes(xintercept = date))+
  scale_x_date(breaks = unique(dat.interventions$date),
               labels = sapply(unique(dat.interventions$date), function(x) {
                 dat.interventions%>%filter(date==x)%>%pull(cov)%>%paste0(collapse = ",")}))+
  theme(axis.text.x = element_text(angle=90))


# +++ DATA +++ --------------------------------------------------------------------


# heinsberg ---------------------------------------------------------------

# heinsberg important at beginning
rki_new %>%
  mutate(date = Refdatum,
         heins = Landkreis=="LK Heinsberg")%>%
  group_by(date,heins)%>%
  summarise(
    pos = sum(AnzahlFall[Neuer.Fall%in%c(0,1)]))%>%
  # group_by(date)%>%
  # summarise(ratio = pos[heins]/sum(pos))%>%
  ggplot(aes(x=date,y=pos,col=heins))+geom_line()+
  #scale_y_log10()+
  xlim(c(as.Date("2020-02-21"),as.Date("2020-03-15")))+
  ylim(c(0,100))


rki_new %>%
  mutate(date = Refdatum,
         heins = Landkreis=="LK Heinsberg")%>%
  filter(month(date)==2)%>%
  group_by(heins)%>%
  summarise(
    pos = sum(AnzahlFall[Neuer.Fall%in%c(0,1)]))
343/727


# reporting vs symptom onset in Heinsberg
dfheins.s <- rki_new %>%
  mutate(date = Refdatum)%>%
  filter(Landkreis=="LK Heinsberg")%>%
  group_by(date)%>%
  summarise(
    pos = sum(AnzahlFall[Neuer.Fall%in%c(0,1)]))
dfheins.r  <- rki_new %>%
  mutate(date = Meldedatum)%>%
  filter(Landkreis=="LK Heinsberg")%>%
  group_by(date)%>%
  summarise(
    pos = sum(AnzahlFall[Neuer.Fall%in%c(0,1)]))
dfheins <- full_join(dfheins.r,dfheins.s,
                     by="date",
                     suffix=c(".r",".s"))

ggplot(dfheins%>%pivot_longer(c(pos.r,pos.s),
                              names_to = "type"),
       aes(x=date,y=value))+
  geom_line(aes(group = type,col=type))+
  geom_point(aes(col=type))+
  scale_x_date(name = "date",
               date_labels = "%d-%m",
               date_breaks = "2 days",
               limits = c(as.Date("2020-02-15"),as.Date("2020-03-07")))+
  scale_y_continuous(limits = c(0,75),name = "new cases")+
  scale_color_discrete(name = "", labels = c("by reporting date","by symptom onset"))+
  geom_vline(xintercept = as.Date(c("2020-02-26","2020-02-19","2020-02-22")))+
  geom_label(data = data.frame(date=as.Date(c("2020-02-26","2020-02-19","2020-02-22")),
                               intervention = c("Interventions start",
                                                "carneval start",
                                                "carneval saturday"),
                               yposition = c(70,50,60)),
             aes(x=date,y=yposition,label=intervention))+
  theme(legend.position = "bottom",
        axis.title.x = element_blank())
ggsave("./analysis/tex/plotsnew/heinsberg.pdf",
       width = 6,height=4)




# by age
dfheins.s <- rki_new %>%
  mutate(date = Refdatum)%>%
  filter(Landkreis=="LK Heinsberg")%>%
  group_by(date,age)%>%
  summarise(
    pos = sum(AnzahlFall[Neuer.Fall%in%c(0,1)]))
dfheins.r  <- rki_new %>%
  mutate(date = Meldedatum
         )%>%
  filter(Landkreis=="LK Heinsberg")%>%
  group_by(date,age)%>%
  summarise(
    pos = sum(AnzahlFall[Neuer.Fall%in%c(0,1)]))
dfheins <- full_join(dfheins.r,dfheins.s,by=c("date","age"),suffix=c(".r",".s"))
ggplot(dfheins%>%pivot_longer(c(pos.r,pos.s),
                              names_to = "type"),
       aes(x=date,y=value,col=type))+
  geom_histogram(stat="identity")+
  facet_grid(age~type)+
  scale_x_date(name = "date",
               date_labels = "%d-%m",
               date_breaks = "2 days",
               limits = c(as.Date("2020-02-15"),as.Date("2020-03-05")))+
  scale_y_continuous(limits = c(0,40),name = "number of cases")+
  scale_color_discrete(name = "", labels = c("by reporting date","by symptom onset"))+
  guides(color=F)




# table summary -----------------------------------------------------------

cur.dat <- data %>% filter(date>= start.date,
                date<= end.date1)%>%
  select(name,date,all_of(cov.main))%>%
  pivot_longer(cov.main,names_to = "cov")%>%
  group_by(cov)%>%
  summarise(
    label = my_labeller2(unique(cov)),
    min = min(value),
    q05 = quantile(value,.05),
    mean = mean(value),
    q95 = quantile(value,.95),
    max = max(value),
    vart = max(0,compute_variation_in(value,date)),
    varl = max(0,compute_variation_in(value,name)),
    type = ifelse(all(value%in%c(0,1)), "binary", "real"),
    first = min(date[value==1],na.rm = TRUE),first = as.character(first),
    last = max(date[value==1],na.rm = TRUE),last = as.character(last),
    "total locations" = length(unique(name[value==1])),
    "total days" = length(unique(date[value==1]))
  )
cur.dat %>%
  filter(type=="binary")%>%
  arrange(first)%>%
  select(label,first,'total days','total locations',vart,varl)%>%
  xtable::xtable()%>%
  print(include.rownames=FALSE)


cur.dat %>%
  filter(type=="real")%>%
  left_join(macro$df.standardize %>% select(cov,sd))%>%
  mutate(standardized = !is.na(sd))%>%
  select(-c(sd))%>%
  select(label,min,q05,mean,q95,max,vart,varl)%>%
  xtable::xtable()%>%
  print(include.rownames=FALSE)


# table counties -----------------------------------------------------------

data %>% group_by(name) %>%
  summarise(
    state = substr(unique(Bundesland),1,3),
    cases = sum(pos.new),
    deaths = sum(dead.new),
    population = unique(pop_c)/1e3,
    incidence = cases/unique(pop_c)*1e5,
    scfr = deaths/cases*1000
  )%>%xtable::xtable(digits = 0)%>%print(include.rownames=FALSE)

# asymptomatic cases -------------------------------------------------------

### Ratio of asymptomatic cases by age group
rki_new %>%
  filter(Meldedatum<end.date1) %>%
  group_by(age) %>%
  summarise(asymptomatic = mean(is.na(Refdatum)),
            sd = sd(is.na(Refdatum)),
            n = length(Refdatum),
            se = sd/sqrt(n) * qt(.9/2+.5,n-1),
            .groups = 'drop')%>%
  ggplot(aes(x=age,y=asymptomatic))+
  geom_bar(stat = "identity",position = position_dodge())+theme_classic()#+
  #geom_errorbar(aes(ymin=asymptomatic-se,ymax= asymptomatic+se),position = position_dodge())
ggsave("./analysis/tex/plotsnew/asymptomatic.pdf",width = 5,height=2)



rki_new %>%
  filter(age %in% unique(data$age),
         Meldedatum<end.date2,
         Meldedatum>as.Date("2020-03-01")) %>%
  mutate(month = lubridate::month(Meldedatum,label=TRUE))%>%
  group_by(age,month) %>%
  summarise(asymptomatic = mean(is.na(Refdatum)),
            sd = sd(is.na(Refdatum)),
            .groups = 'drop')%>%
  ggplot(aes(x=month,y=asymptomatic, shape=age,group=age))+
  geom_line()+geom_point()+theme_classic()+
  theme(
    axis.title.x = element_blank()
  )+
scale_shape_manual(values=shape.cur,labels = age_labels)

ggsave("./analysis/tex/plotsnew/asymptomatic_on_time.pdf",width = 5,height=2)


# 7-day incidence by age group. -------------------------------------------
rki_new %>%
  filter(!is.na(Refdatum),
         Refdatum>start.date,
         Refdatum<end.date2) %>%
  mutate(date=Refdatum)%>%
  group_by(date,age)%>%
  summarise(
    pos.new = sum(AnzahlFall[Neuer.Fall%in%c(0,1)])
  ) %>%
  left_join(regionaldatenbank %>% filter(adm.level==1))%>%
  group_by(age)%>%
  arrange(date)%>%
  mutate(
    pos7 = zoo::rollapplyr(pos.new, width = 7, FUN = sum, partial = TRUE)
  )%>%
  mutate(incidence = pos7/total)%>%
  ggplot(aes(x=date,
             y=incidence,#y=pos.new,
             color=age
             #,shape=age
             ))+
  scale_y_log10()+ylab("incidence")+
  geom_point(size=.1)+
  geom_line()+theme_classic()
ggsave("./analysis/tex/plotsnew/incidence_by_age.pdf",width = 5,height=2)


# growth rate daily --------------------------------------------------------------

seq.dates <-
  seq.Date(min(rki_new$Meldedatum),max(rki_new$Meldedatum),by="day")

dat.cur <- rki_new %>%
  filter(Landkreis %in% data$name)

dfs <- dat.cur %>%
  filter(!is.na(Refdatum))%>%
  group_by(Refdatum,
           age)%>%
  summarise(
    pos = sum(AnzahlFall[Neuer.Fall%in%c(0,1)]),
    dead = sum(AnzahlTodesfall[Neuer.Todesfall%in%c(0,1)])
  ) %>%
  rename(
    date=Refdatum,
  )%>%
  ungroup()%>%
  complete(date=seq.dates,age,
           fill = list(pos = 0, dead =0))%>%
  group_by(age)%>%
  arrange(date) %>%
  mutate(pos7 = zoo::rollapply(pos,width=7,align="center",partial=TRUE,FUN=mean),
         lpos7 = lag(pos7,7),
         growth = pos7/lpos7)


dfr <- dat.cur %>%
  group_by(Meldedatum,
           age)%>%
  summarise(
    pos = sum(AnzahlFall[Neuer.Fall%in%c(0,1)]),
    dead = sum(AnzahlTodesfall[Neuer.Todesfall%in%c(0,1)])
  ) %>%
  rename(
    date=Meldedatum,
  )%>%
  ungroup()%>%
  complete(date=seq.dates,age,
           fill = list(pos = 0, dead =0))%>%
  group_by(age)%>%
  arrange(date) %>%
  mutate(pos7 = zoo::rollapply(pos,width=7,align="center",partial=TRUE,FUN=mean),
         lpos7 = lag(pos7,7),
         growth = pos7/lpos7)


df_all <- rbind(
  dfs%>%mutate(type = "symptom onset",
               date = date - as.difftime(5,units = "days")),
  dfr%>%mutate(type = "reporting date",
               date = date - as.difftime(5,units = "days")))%>%ungroup()

start.date3 <- as.Date("2020-03-01")
end.date3 <- as.Date("2020-05-15")
ggplot(df_all%>%
         filter(date >= start.date3,
                date < end.date3)%>%
         #filter(age %in% unique(dfs_all$age)[2:5])%>%
         mutate(age=age_labels(age)))+
  geom_point(aes(x=date,y=growth),alpha=.2)+
  geom_line(aes(x=date,y=growth),alpha=.2)+
  scale_y_log10()+
  #coord_cartesian(ylim = c(0,6))+
  geom_hline(yintercept = 1)+
  theme_bw()+
  theme(
    axis.text.x = element_text(angle = 90,vjust = .4),
    axis.title.x = element_blank()
  )+facet_grid(age~type)+
  ylab("growth rate")+
  scale_x_date(date_breaks = "1 week",
               date_labels = "%d %b")
ggsave("./analysis/tex/plotsnew/growth_days.pdf",
       width = 5,height=6)



# 7-day incidence grid -------------------------------------------
plot.dat <- rki_new %>%
  filter(!is.na(Refdatum),
         Refdatum>start.date,
         Refdatum<end.date2) %>%
  mutate(date=Refdatum)%>%
  group_by(date,age)%>%
  summarise(
    pos.new = sum(AnzahlFall[Neuer.Fall%in%c(0,1)])
  ) %>%
  left_join(regionaldatenbank %>% filter(adm.level==1))%>%
  group_by(age)%>%
  arrange(date)%>%
  mutate(
    pos7 = zoo::rollapplyr(pos.new, width = 7, FUN = sum, partial = TRUE),
    growth = pos7/lag(pos7)
  )%>%
  group_by(age)%>%
  mutate(incidence = 1e5*pos7/total,
         meani=mean(incidence),
         medi = median(incidence),
         sdi = sd(incidence))

  ggplot(plot.dat)+
  geom_hline(aes(yintercept = meani))+
  geom_line(aes(x=date,
                y=incidence))+
    #scale_y_log10()+
    theme_bw()+
    guides(color=F)+
  ylab("7-day incidence in 100.000")+
    facet_wrap(age~.)
ggsave("./analysis/tex/plotsnew/incidence_by_age2.pdf",
       width = 5,height=4)


# incidence tiles (first wave) ---------------------------------------------------------
plot.dat <- rki_new %>%
  filter(!is.na(Refdatum)
         ) %>%
  mutate(Bundesland = as.character(Bundesland),
         Bundesland = if_else(Bundesland=="ThÃ¼ringen","Thüringen",Bundesland),
         Bundesland = if_else(Bundesland=="Baden-WÃ¼rttemberg","Baden-Württemberg",Bundesland),
         date=Refdatum)%>%
  group_by(date,age,Bundesland)%>%
  summarise(
    pos.new = sum(AnzahlFall[Neuer.Fall%in%c(0,1)])
  ) %>%
  rename(name=Bundesland)%>%
  left_join(regionaldatenbank  %>%
              mutate(
                name = as.character(name),
                name = if_else(name=="Baden-Württemberg,Land","Baden-Württemberg",name)
                ),
            suffix = c("",".rdb"))%>%
  group_by(age,name)%>%
  arrange(date)%>%
  mutate(
    pos7 = zoo::rollapplyr(pos.new, width = 7, FUN = sum, partial = TRUE),
    growth = pos7/lag(pos7)
  )%>%
  filter(

    # Refdatum>start.date,
    # Refdatum<end.date1
    date>as.Date("2020-03-7"),
    date<as.Date("2020-04-21")
  )%>%
  group_by(age,name)%>%
  mutate(incidence = 1e5*pos7/total,
         lincidence = log(incidence),
         meani=mean(incidence),
         meanli=mean(lincidence),
         medi = median(incidence),
         sdi = sd(incidence),
         sdli = sd(lincidence))%>%
  group_by(name)%>%
  mutate(totalB = sum(unique(total),na.rm = TRUE),
         incidenceB = sum(pos.new,na.rm = TRUE)/totalB)%>%
  ungroup()%>%
  filter(incidenceB>= quantile(incidenceB, probs = .6, na.rm=TRUE))%>%
  mutate(
         age = gsub("A0","",age),
         age = gsub("A","",age),
         age = gsub("-"," - ",age),
         age = factor(age,levels = c("0 - 4","5 - 14","15 - 34","35 - 59","60 - 79","80+")))

p1 <- ggplot(plot.dat%>%
       complete(nesting(name,meani,sdi,age),date,fill = list (incidence=0))
       )+
  geom_tile(aes(x=date,
                y=age,
                fill = (incidence-meani)/sdi))+
  scale_fill_gradientn(
    values = scales::rescale(c(-1,0,1)),
    colors = c("green","yellow","red"))+
  theme_bw()+
  theme(
    strip.background.x = element_blank(),
    plot.background = element_blank(),    # Background of the entire plot
    panel.background = element_blank(),
    panel.grid = element_blank(),
    legend.position = "bottom")+
  facet_wrap(name~.)+
  labs(fill = "standardized incidence", x = element_blank(),y = element_blank())
p1
ggsave("./analysis/tex/plotsnew/incidence_age_heatmap.pdf",width = 8,height=3)
#ggsave("./analysis/tex/plotsnew/incidence_age_heatmap.jpeg",width = 8,height=3)



# incidence tiles (second wave) ---------------------------------------------------------
plot.dat <- rki_new %>%
  filter(!is.na(Refdatum)) %>%
  mutate(Bundesland = as.character(Bundesland),
         Bundesland = if_else(Bundesland=="ThÃ¼ringen","Thüringen",Bundesland),
         Bundesland = if_else(Bundesland=="Baden-WÃ¼rttemberg","Baden-Württemberg",Bundesland),
         date=Refdatum)%>%
  group_by(date,age,Bundesland)%>%
  summarise(
    pos.new = sum(AnzahlFall[Neuer.Fall%in%c(0,1)])
  ) %>%
  rename(name=Bundesland)%>%
  left_join(regionaldatenbank  %>%
              mutate(
                name = as.character(name),
                name = if_else(name=="Baden-Württemberg,Land","Baden-Württemberg",name)
              ),
            suffix = c("",".rdb"))%>%
  group_by(age,name)%>%
  arrange(date)%>%
  mutate(
    pos7 = zoo::rollapplyr(pos.new, width = 7, FUN = sum, partial = TRUE),
    growth = pos7/lag(pos7)
  )%>%
  filter(
    date>as.Date("2020-05-01"),
    date<end.date2
  )%>%
  group_by(age,name)%>%
  mutate(incidence = 1e5*pos7/total,
         lincidence = log(incidence),
         meani=mean(incidence),
         meanli=mean(lincidence),
         medi = median(incidence),
         sdi = sd(incidence),
         sdli = sd(lincidence))%>%
  group_by(name)%>%
  mutate(totalB = sum(unique(total),na.rm = TRUE),
         incidenceB = sum(pos.new,na.rm = TRUE)/totalB)%>%
  ungroup()%>%
  filter(incidenceB>= quantile(incidenceB, probs = .55, na.rm=TRUE))%>%
  mutate(
    age = gsub("A0","",age),
    age = gsub("A","",age),
    age = gsub("-"," - ",age),
    age = factor(age,levels = c("0 - 4","5 - 14","15 - 34","35 - 59","60 - 79","80+")))

p2 <- ggplot(plot.dat%>%
         complete(nesting(name,age),date,fill = list(incidence=-2,meani=0,sdi=1))
)+
  geom_tile(aes(x=date,
                y=age,
                fill = (incidence-meani)/sdi))+
  scale_fill_gradientn(
    #values = scales::rescale(c(-1,0,1)),
    colors = c("green","yellow","red"))+
  theme_bw()+
  theme(
    strip.background.x = element_blank(),
    plot.background = element_blank(),    # Background of the entire plot
    panel.background = element_blank(),
    panel.grid = element_blank(),
    legend.position = "bottom")+
  facet_wrap(name~.)+
  labs(fill = "standardized incidence", x = element_blank())+
  scale_x_date(date_labels = "%b %d")+
  labs(fill = "standardized incidence", x = element_blank(),y = element_blank())
p2
ggsave("./analysis/tex/plotsnew/incidence_age_heatmap_late.pdf",width = 9,height=3)

#join both plots
ggpubr::ggarrange(p1,p2,ncol = 1)
ggsave("./analysis/tex/plotsnew/incidence_age_heatmaps.pdf",width = 9,height=6)
#ggsave("./analysis/tex/plotsnew/incidence_age_heatmaps.jpg",width = 9,height=6)


# info and delay -----------------------------------------------------------------
load(res.path)
cov.cur <- c("info_incidence","incidence","traced")

data %>%
  filter(date > start.date)%>%
  select(cov.cur,date,age,name,Bundesland) %>%
  filter(Bundesland %in% c("Bayern","Hamburg"))%>%
  group_by(Bundesland,date)%>%
  summarise_at(vars(cov.cur),mean, na.rm=TRUE)%>%
  pivot_longer(matches(cov.cur),
               names_to = "covariate",
               values_to = "value") %>%
  group_by(covariate)%>%
  mutate(
    value2 = (value-mean(value))/sd(value)
  )%>%
  ggplot(aes(x=date,y=value))+
  geom_point()+
  geom_line(aes(group=Bundesland))+
  facet_wrap(covariate~Bundesland,scales = "free_y",ncol = 2)+
  theme_classic()
ggsave("./analysis/tex/plotsnew/info_example.pdf",width = 5,height=3)

# delay general -----------------------------------------------------------

### table
rki_new %>%
  filter(age %in% unique(data$age),
         Landkreis %in% unique(data$name),
         Refdatum<=end.date2,
         Refdatum > start.date)%>%
  mutate(name = Landkreis,
         delay_ind = as.numeric(Meldedatum-Refdatum),
         delay_ind = if_else(delay_ind>14,14,delay_ind),
         delay_ind = if_else(delay_ind<(-7),-7,delay_ind))%>%
  group_by(lubridate::month(Refdatum,label=TRUE))%>%
  summarise(mean=mean(delay_ind),
            median=median(delay_ind),
            sd=sd(delay_ind),
            delay3n = mean(delay_ind<=-3,na.rm = TRUE),
            delay0 = mean(delay_ind<=0,na.rm = TRUE),
            delay3 = mean(delay_ind<=3,na.rm = TRUE),
            delay6 = mean(delay_ind<=6,na.rm = TRUE),
            delay9 = mean(delay_ind<=9,na.rm = TRUE))


# mean and quantiles
rki_new %>%
  filter(Refdatum>=start.date,
         Refdatum<=end.date2
         )%>%
  mutate(
    delay_ind = as.numeric(Meldedatum-Refdatum),
    delay_ind = if_else(delay_ind>14,14,delay_ind),
    delay_ind = if_else(delay_ind<(-7),-7,delay_ind)) %>%
  #group_by(Refdatum,Bundesland)%>%
  group_by(Refdatum)%>%
  summarise(
    pos = sum(AnzahlFall[Neuer.Fall%in%c(0,1)]),
    delay = mean(delay_ind,na.rm=TRUE),
    delay1 = quantile(delay_ind,na.rm=TRUE,probs = .1),
    delay9 = quantile(delay_ind,na.rm=TRUE,probs = .9))%>%
  rename(date = Refdatum)%>%
  arrange(date)%>%
  mutate(
    delay = zoo::rollapplyr(delay, width = 7, FUN = mean, partial = TRUE),
    delay1 = zoo::rollapplyr(delay1, width = 7, FUN = mean, partial = TRUE),
    delay9 = zoo::rollapplyr(delay9, width = 7, FUN = mean, partial = TRUE))%>%
  ggplot(aes(x=date,y=delay))+
  geom_ribbon(aes(ymin=delay1,ymax=delay9),alpha=.2)+
  geom_line()+
  theme_bw()+
  geom_hline(yintercept = c(-1,6))+
  theme(axis.title.x = element_blank())+
  scale_x_date(date_labels = "%b %d")
ggsave("./analysis/tex/plotsnew/delay_on_time.pdf",
       width = 5,height=2)


# traced ------------------------------------------------------------

data %>%
  mutate(week = isoweek(date))%>%
  group_by(week)%>%
  summarise(date = min(date),
            ratio_med = median(traced, na.rm = TRUE),
            ratio_q10 = quantile(traced,probs = .1, na.rm = TRUE),
            ratio_q90 = quantile(traced,probs = .9, na.rm = TRUE)) %>%
  ggplot(aes(x=date,y=ratio_med))+
  geom_point()+
  geom_errorbar(aes(ymin=ratio_q10,ymax=ratio_q90))+
  theme_bw()+
  theme(axis.title.x = element_blank())+
  ylab("ratio of traced infectiuous")
ggsave("./analysis/tex/plotsnew/traced_on_time.pdf",
       width = 5,height=2)

Covid::traced%>%
  filter(date >= start.date,
           date <= end.date2)%>%
  mutate(week = isoweek(date))%>%
  group_by(week)%>%
  summarise(date = min(date),
            ratio_med = median(traced, na.rm = TRUE),
            ratio_q10 = quantile(traced,probs = .1, na.rm = TRUE),
            ratio_q90 = quantile(traced,probs = .9, na.rm = TRUE),
            ratio_q25 = quantile(traced,probs = .25, na.rm = TRUE),
            ratio_q75 = quantile(traced,probs = .75, na.rm = TRUE)) %>%
  ggplot(aes(x=date,y=ratio_med))+
  geom_point()+
  geom_errorbar(aes(ymin=ratio_q25,ymax=ratio_q75))+
  theme_bw()+
  theme(axis.title.x = element_blank())+
  ylab("ratio of traced infectious")
ggsave("./analysis/tex/plotsnew/traced_on_time2.pdf",
       width = 5,height=2)


# tests -------------------------------------------------------------------

#safe cases
cur.cases <-rki_new %>%
  filter(Refdatum>start.date,
         Refdatum<=end.date2,
         Neuer.Fall %in% c(0,1)) %>%
  mutate(week = lubridate::isoweek(Refdatum))%>%
  group_by(week)%>%
  summarise(
    pos = sum(AnzahlFall),
    date=median(Refdatum)
  )

Covid::tests %>% select(date,new.tests) %>%
  mutate(week = lubridate::isoweek(date)) %>%
  left_join(cur.cases,by="week")%>%
  mutate(
    testsprocase = new.tests/pos
  ) %>%
  ggplot(
    aes(x=date.y,
        y=testsprocase
    ))+
  geom_line()+
  geom_point()+
  scale_y_log10()+
  theme_classic()+
  ylab("Tests for one symptomatic case")+
  theme(axis.title.x = element_blank())
ggsave("./analysis/tex/plotsnew/testsprocase.pdf",
       width = 5,height=4)



ggplot(tests,aes(x=date,y=new.tests/1000))+
  geom_point()+
  geom_line()+
  ylim(c(0,1e3))+theme_classic()+ylab("weekly tests (in thousand)")
ggsave("./analysis/tex/plotsnew/tests.pdf",
       width = 5,height=4)



# ftr ---------------------------------------------------------------------

###Case fatality rate (ftr) by age in Germany.
report_t <- rki_new %>%
  filter(Meldedatum<=end.date1)%>%
  group_by(Meldedatum,age,gender)%>%
  summarise(
    pos.new = sum(AnzahlFall[Neuer.Fall%in%c(0,1)]),
    dead.new = sum(AnzahlTodesfall[Neuer.Todesfall%in%c(0,1)]))%>%
  rename(date=Meldedatum)

print(xtable::xtable(digits = 2,
                     report_t %>%
                       filter(date<as.Date("2020-08-01"),
                              date>as.Date("2020-02-15")) %>%
                       group_by(age) %>%
                       summarise(
                         ftr = sum(dead.new)/sum(pos.new)*100,
                         deaths = sum(dead.new),
                         cases = sum(pos.new),
                       ))
      ,include.rownames=FALSE)



### symptomatic case fatility rate over time
onset_t <- rki_new %>%
  group_by(Refdatum,age,gender)%>%
  summarise(
    pos.new = sum(AnzahlFall[Neuer.Fall%in%c(0,1)]),
    dead.new = sum(AnzahlTodesfall[Neuer.Todesfall%in%c(0,1)]))%>%
  rename(date=Refdatum)

onset_t %>%
  filter(date<as.Date("2020-05-31"),
         date>=as.Date("2020-03-01")) %>%
  mutate(
    week = lubridate::isoweek(date),
    month = lubridate::month(date),
    biweek = ceiling(lubridate::isoweek(date)/2)) %>%
  group_by(age,month) %>%
  summarise(
    date = min(date),
    sum.pos = sum(pos.new),
    sum.dead = sum(dead.new),
    ftr = sum(dead.new)/sum.pos)%>%
  filter(!age %in% c("A00-A04","A05-A14"))%>%
  mutate(
    eftr=ifr_estimate(as.character(age),weights = TRUE)/100,
    sd = eftr*(1-eftr)/sqrt(sum.pos),
    age = age_labels(age)
  )%>%
  ggplot(aes(x=date,y=ftr))+
  geom_line()+geom_point()+
  #geom_errorbar(aes(ymin=eftr-2.57*sd,ymax=eftr+2.57*sd))+
  facet_wrap(vars(age),scales="free_y")+
  geom_hline(yintercept = 0)+ylab("symptomatic cfr")+
  theme_classic()+
  theme(
    axis.title.x = element_blank()
  )+
  scale_x_date(date_labels = "%b",
               limits = as.Date(c("2020-02-22","2020-05-8")),
               breaks='1 months')+
  geom_hline(aes(yintercept = eftr),linetype=2)+
  geom_text(aes(label=paste0("n = ",sum.dead),y=.5*eftr),size=3)
ggsave("./analysis/tex/plotsnew/scfr_time.pdf",width = 5,height=3)


### case fatility rate over time
report_t <- rki_new %>%
  group_by(Meldedatum,age,gender)%>%
  summarise(
    pos.new = sum(AnzahlFall[Neuer.Fall%in%c(0,1)]),
    dead.new = sum(AnzahlTodesfall[Neuer.Todesfall%in%c(0,1)]))%>%
  rename(date=Meldedatum)

report_t %>%
  filter(date<end.date1,
         date>=as.Date("2020-03-01")) %>%
  mutate(
    week = lubridate::isoweek(date),
    month = lubridate::month(date)) %>%
  group_by(age,month) %>%
  summarise(
    date = min(date),
    sum.pos = sum(pos.new),
    sum.dead = sum(dead.new),
    ftr = sum.dead/sum.pos)%>%
  filter(!age %in% c("A00-A04","A05-A14"))%>%
  mutate(
    eftr=ifr_estimate(as.character(age),weights = TRUE)/100,
    sd = eftr*(1-eftr)/sqrt(sum.pos),
    age = age_labels(age)
  )%>%
  ggplot(aes(x=date,y=ftr))+
  geom_line()+geom_point()+
  #geom_errorbar(aes(ymin=eftr-2.57*sd,ymax=eftr+2.57*sd))+
  facet_wrap(vars(age),scales="free_y")+
  geom_hline(yintercept = 0)+
  ylab("case fatality rate")+
  theme_classic()+
  geom_hline(aes(yintercept = eftr),linetype=2)+
  theme(
    axis.title.x = element_blank()
  )+
  scale_x_date(date_labels = "%b",
               limits = as.Date(c("2020-02-22","2020-05-8")),
                  breaks='1 months')+
  geom_text(aes(label=paste0("n = ",sum.dead),y=.5*eftr),size=3)
ggsave("./analysis/tex/plotsnew/cfr_main.pdf",width = 5,height=2.5)


report_t %>%
  filter(date<end.date2,
         date>=as.Date("2020-03-01")) %>%
  mutate(
    week = lubridate::isoweek(date),
    month = lubridate::month(date)) %>%
  group_by(age,month) %>%
  summarise(
    date = min(date),
    sum.pos = sum(pos.new),
    ftr = sum(dead.new)/sum.pos)%>%
  filter(!age %in% c("A00-A04","A05-A14"))%>%
  mutate(
    eftr=ifr_estimate(as.character(age),weights = TRUE)/100,
    sd = eftr*(1-eftr)/sqrt(sum.pos),
    age = age_labels(age)
  )%>%
  ggplot(aes(x=date,y=ftr))+
  geom_line()+geom_point()+
  #geom_errorbar(aes(ymin=eftr-2.57*sd,ymax=eftr+2.57*sd))+
  facet_wrap(vars(age),scales="free_y")+
  geom_hline(yintercept = 0)+ylab("cfr")+
  theme_classic()+
  geom_hline(aes(yintercept = eftr),linetype=2)
ggsave("./analysis/tex/plotsnew/cfr_time.pdf",width = 5,height=3)



# weather -----------------------------------------------------------------

load(res.path)
cov.cur <- macro$cov.weather[1:2]
data %>%
  select(cov.cur,date,age,name,Bundesland) %>%
  filter(Bundesland %in% c("Bayern","Hamburg"))%>%
  group_by(Bundesland,date)%>%
  summarise_at(vars(cov.cur),mean, na.rm=TRUE)%>%
  pivot_longer(matches(cov.cur),
               names_to = "covariate",
               values_to = "value") %>%
  group_by(covariate)%>%
  mutate(
    value2 = (value-mean(value))/sd(value)
  )%>%
  ungroup()%>%
  mutate(covariate = my_labeller(covariate))%>%
  ggplot(aes(x=date,y=value))+
  geom_point(size=1)+
  geom_line(alpha=.3)+
  facet_wrap(Bundesland~covariate,scales = "free_y")+
  theme_classic()+
  theme(axis.title.y=element_blank(),
        axis.title.x=element_blank())
ggsave("./analysis/tex/plotsnew/weather_example.pdf",
       width = 5,height=4)


# interventions -----------------------------------------------------------
load(res.path)

data %>%
  mutate(time=t)%>%
  select(setdiff(c(macro$dummies,"time"),c(macro$dummies.FE,"sports open"))) %>%
  illustrate_corr()
ggsave("./analysis/tex/plotsnew/interventions_cor.pdf",
       width = 10,height=10)

library(ggplot2)
ggplot(data%>%
         mutate(unit = if_else(name %in% interventions$unit,
                               name, Bundesland))%>%
         group_by(unit,date)%>%slice(1)%>%
         select(unit, macro$dummies.interventions)%>%
         filter(date > start.date,
                date<end.date1)%>%
         pivot_longer(cols = -c(date,unit))%>%
         mutate(name = my_labeller(name)),
       aes(x=date,y=name,col=value))+
  geom_point()+facet_wrap(vars(unit))+
  theme(axis.title = element_blank(),
        legend.position = "bottom",
        legend.title=element_blank())+
  scale_y_discrete(label = my_labeller)+
  scale_color_discrete(labels=c("inactive","active"))
ggsave("./analysis/tex/plotsnew/interventions_by_unit.jpeg",
       width = 10,height=10)



ggplot(data%>%
         mutate(unit = if_else(name %in% interventions$unit,
                               name, Bundesland))%>%
         group_by(unit,date)%>%slice(1)%>%
         select(unit, macro$dummies.interventions)%>%
         filter(date > start.date,
                date<end.date1)%>%
         pivot_longer(cols = -c(date,unit))%>%
         mutate(name = my_labeller(name)),
       aes(x=date,y=unit,col=value))+
  geom_point()+facet_wrap(vars(name))+
  theme(axis.title = element_blank(),
        legend.position = "bottom",
        legend.title=element_blank())+
  scale_color_discrete(labels=c("inactive","active"))
ggsave("./analysis/tex/plotsnew/interventions_by_name.jpeg",
       width = 10,height=10)



# + timeline interventions --------------------------------------------------

plot.dat <- intervention.list %>%
  filter(adm.level!=3,
         adm.level==1 |
           Bundesland %in% unique(data$Bundesland),
         intervention %in% c(cov.main,"schools","kitas"))%>%
  group_by(intervention) %>%
  summarise(
    first = min(start,na.rm = TRUE),
    median = median(start, na.rm = TRUE)
  )
plot.dat$intervention = my_labeller(plot.dat$intervention)
plot.dat <- plot.dat %>%
  arrange(first) %>%
  mutate(pos = nrow(plot.dat):1,
         pos = if_else(pos>=unique(pos[intervention=="narrow testing"]),pos-4,as.numeric(pos)),
         pos = pos/2)

ggplot() +
  geom_segment(aes(x = first,y = pos,xend = first),data=plot.dat,yend = 0,alpha=.1) +
  geom_segment(aes(x = min(plot.dat$first),y = 0,xend =  max(plot.dat$first),yend = 0),data=plot.dat,arrow = arrow(length = unit(x = 0.2,units = 'cm'),type = 'closed')) +
  ggrepel::geom_text_repel(aes(x = first,y = pos,label = intervention),direction = "x",
            data=plot.dat
            ) +
  geom_point(aes(x = first,y = pos),data=plot.dat) +
  scale_x_date(breaks = unique(plot.dat$first),date_labels = "%d-%m")+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(angle = 90,vjust = .4),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())
ggsave("./analysis/tex/plotsnew/timeline.pdf",
       width = 7,height=3)


# correlation covariates ---------------------------------------------------------

### of all variables
data %>%
  filter(t >= min(dat$Ti),
         t <= max(dat$T))%>%
  mutate(time=t)%>%
  select(setdiff(c(macro$cov,"time",macro$dummies),
                 c(macro$dummies.FE))) %>%
  illustrate_corr()+
  scale_y_discrete(label = my_labeller)+
  scale_x_discrete(label = my_labeller)

ggsave("./analysis/tex/plotsnew/correlation_all.pdf",
       width = 10,height=10)


### of main covariates
data %>%
  filter(t >= min(dat$Ti),
         t <= max(dat$T))%>%
  mutate(time=t)%>%
  select(c(cov.main,"time")) %>%
  illustrate_corr()+
  scale_y_discrete(label = my_labeller2)+
  scale_x_discrete(label = my_labeller2)

ggsave("./analysis/tex/plotsnew/correlation_main.pdf",
       width = 8,height=8)


# growth rate emprirics ---------------------------------------------------

### load data
df <- rki_new %>%
  filter(!is.na(Refdatum))%>%
  mutate(name = Landkreis) %>%
  group_by(Refdatum,name,
           age,
           Bundesland)%>%
  summarise(
    pos.new = sum(AnzahlFall[Neuer.Fall%in%c(0,1)]),
    dead.new = sum(AnzahlTodesfall[Neuer.Todesfall%in%c(0,1)])
  ) %>%
  rename(
    date=Refdatum,
  )%>%
  ungroup()

### plot changes over time
df_all <- df %>%
  complete(nesting(name,Bundesland),age,date = seq.Date(min(df$date),max(df$date),by = 1),fill = list(pos.new=0,dead.new=0))%>%
  mutate(week = isoweek(date))%>%
  group_by(name,week,Bundesland)%>%
  summarise(date = min(date),
            pos.new = sum(pos.new),
            dead.new = sum(dead.new))%>%
  group_by(name,Bundesland)%>%
  arrange(week) %>%
  mutate(lpos = lag(pos.new),
         growth = pos.new/lpos)%>%
  ungroup()


res <- df_all %>%
  filter(lpos>10)%>%
  group_by(week,Bundesland) %>%
  mutate(
    R = mean(growth,na.rm = TRUE),
    dev = growth-R)%>%
  mutate(month = month(date))%>%
  group_by(month)%>%
  mutate(
    var = mean(dev^2,na.rm = TRUE),
    var.sd = sd(dev,na.rm = TRUE),
    n = n(),
    disp = R^2/(lpos*var-R),
    varmin = var-1.96*var.sd/sqrt(n),
    varmax = var+1.96*var.sd/sqrt(n),
    dispmin = pmin(R^2/(lpos*varmin-R),R^2/(lpos*var-R),R^2/(lpos*varmax-R),na.rm = TRUE),
    dispmax = pmax(R^2/(lpos*varmin-R),R^2/(lpos*var-R),R^2/(lpos*varmax-R),na.rm = TRUE)
  )

res <- res %>%
  group_by(month)%>%
  summarise(
    date = min(date),
    R = mean(R,na.rm = TRUE),
    disp = mean(disp,na.rm = TRUE),
    dispmin = mean(dispmin,na.rm = TRUE),
    dispmax = mean(dispmax,na.rm = TRUE)
  )
ggplot(res%>%
         filter(
           date > as.Date("2020-03-10"),
           date < as.Date("2020-09-01")),
       aes(x=date))+
  geom_point(aes(y=disp))+
  geom_errorbar(aes(ymin=dispmin,ymax=dispmax),width=7)+
  theme_bw()+
  theme(axis.title.x = element_blank())+
  ylab(latex2exp::TeX("dispersion $\\Psi$"))
ggsave("./analysis/tex/plotsnew/disp_reduced.pdf",
       width = 6,height=3)


# plot all growth rates
R <- 2.5
disp <- .2
disp2 <-.2*df_all%>%filter(date>= start.date,
                           date<= end.date1)%>%pull(lpos)%>%mean(na.rm=TRUE)
ggplot(df_all%>%
         filter(lpos>1,
                date>= start.date,
                date<= end.date1)%>%
         group_by(lpos)%>%
         summarise(
           mean = mean((1-growth)^2),
           pred1 = R*(R+disp)/(unique(lpos)*disp),
           pred2 = R/unique(lpos) + R^2/disp2,
           sd = sd((1-growth)^2),
           n = n()),
       aes(x=lpos,y=mean))+
  geom_point(alpha=.25)+
  geom_errorbar(aes(ymin=mean-1.96*sd/sqrt(n),ymax=mean+1.96*sd/sqrt(n)),alpha=.25)+
  geom_line(aes(y=pred1),col="black",size=1)+
  geom_line(aes(y=pred2),col="red",size=1)+
  scale_x_log10()+
  coord_cartesian(ylim=c(0,20))+
  theme_bw()+
  ylab("Variance of growth rate")+
  xlab("infections in previous week")
ggsave("./analysis/tex/plotsnew/grate_ilag.pdf",
       width = 6,height=3)



# Priors ------------------------------------------------------------------

### Lancet
### Evolving epidemiology and transmission dynamics of coronavirus disease 2019

#incubation  5·2 days (95%: 1·8–12·4)
quantile(rnorm(1000,5.2,sd=2), probs = c(.025,.975) )
quantile(plot_gamma_dist(5.1,2), probs = c(.025,.975))

### Science
### Quantifying SARS-CoV-2 transmission suggests

# generation time
# with mean and median equal to
# 5.0 days and standard deviation of 1.9 days


