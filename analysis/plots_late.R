library(Covid)
library(RCovModel)
library(tidyverse)
library(latex2exp)

start.date <- as.Date("2020-02-15")
end.date1 <- as.Date("2020-05-15")
end.date2 <- as.Date("2020-09-01")



res.path <- "./jags models/submission/save/draft_late10-25.RData"
if(!file.exists(res.path))
  stop("Download file and post to path! Source: https://drive.google.com/file/d/1fwAp5YqBRVR0AqHFE2HRyqoeq9rT7m4Z/view?usp=sharing")

# +++ RESULTS BASE +++ -----------------------------------------------------------------

age.cur <-  NULL#c("A15-A34","A35-A59","A60-A79","A80+")
total.age <- 2:3
shape.cur <- c(2,0,1,8,16)
CI.large.cur <- TRUE

theme_set(theme_bw())

load(res.path)
cov.main <-c(macro$cov,setdiff(macro$dummies,macro$dummies.FE))


macro$notes
show_basics()

# number of symptomatic cases
data%>%
  summarise(sum(pos.new),
            sum(dead.new))

data %>% group_by(age)%>%
  summarise(pos = sum(pos.new),
            sum(dead.new))%>%ungroup()%>%
  summarise(pos[age == "A05-A14"]/sum(pos))


data$name%>%unique()%>%length()

# all effects -------------------------------------------------------------

show_effects_sample(cov = cov.main,
                    filter_age = age.cur,
                    order = TRUE,
                    CI_large = TRUE,
                    width_errorbar = 3)+
  scale_shape_manual(values=shape.cur,labels = age_labels)+
  scale_y_continuous(name="change in transmission",
                     breaks = c(-.5,-.25,0,.25)*100,
                     labels = function(x) paste0(x,"%"))+
  ggtitle("Intervention/covariate")+
  theme(plot.title = element_text(size = 9,hjust=-0.5,face="bold"))
ggsave("./analysis/tex/plots/res_late.pdf",
       width = 6,height=4.5)



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



# total info ------------------------------------------------------------

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

# total all ---------------------------------------------------------------
ggpubr::ggarrange(ggpubr::ggarrange(p.traced,p.info,labels = c("A","B")),
                  p.weather,labels = c("","C"),
                  ncol=1)
ggsave("./analysis/tex/plots/total_all_late.pdf",
       width = 4,height=4)
# totoal diff info -----------------------------------------------------------



# nothing to 50/100k
total_diff_effect(rbind(
  #data.frame(m="info_incidence", value1 = 50/10e6,value2=0),
  data.frame(m="info_lincidence",
             value1 = log(50+1,base=10),
             value2= log(1,base=10))),
  #choose_age = 2:3,
  average = TRUE
)

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
  #choose_age = 2:3,
  average = TRUE
)



# total diff traced -------------------------------------------------------
total_diff_effect(
  data.frame(m="traced",
             value1 = 0,
             value2 = 1),
  choose_age = c("A80+","A60-A79"),
  average = TRUE
)

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
disp_sample_age <- disp_sample%>%
  left_join(
    regionaldatenbank%>%
      filter(adm.level==1)%>%
      mutate(ratio=total/sum(total))%>%
      select(age,ratio))%>%
  mutate(ratio = ratio / max(ratio))%>%
  group_by(age) %>%
  sample_frac(ratio)
sample.sec.inf <-
  as.numeric(sapply(
    disp_sample_age$i_disp,
    function(x) rnbinom(100,mu=1,size=x)))
var.cur <- var(sample.sec.inf)
mean.cur <- mean(sample.sec.inf)
mean.cur/var.cur
mean.cur^2/(var.cur-mean.cur)


# + meta regression -------------------------------------------------------

df.R0 <- df.R0 %>%
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
           , df.R0)


lm2 <- lm(first ~ easternGer+
            standardise(density)
          +rural+
            #standardise(children)+
            standardise(young)+
            standardise(middle)
          #+standardise(latitude)+
           # standardise(longitude)
           ,
          df.R0 %>% left_join(data.frame(first = dat$Ti, name = names(dat$Ti))))


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
ggsave("./analysis/tex/plots/estimates_corr_late.pdf",
       width = 8,height=8)

effects %>%
  pivot_wider(names_from = m,values_from = draw) %>%
  filter(age %in% cor.age)%>%
  select(-c(age,iteration))%>%
  illustrate_corr()+
  scale_y_discrete(label = my_labeller)+
  scale_x_discrete(label = my_labeller)
ggsave("./analysis/tex/plots/estimates_corr_all_late.pdf",
       width = 9,height=9)




