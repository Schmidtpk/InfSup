library(tidyverse)
library(mymcmc)
library(Covid)
library(tidybayes)

load("./jags models/submission/save/submission.RData")
load("./jags models/submission/simulate/simdat.RData")

qmax <- .9
qmin <- .1

unique(data$name)

dat.first <- as.Date("2020-03-13")
dat.last <- as.Date("2020-05-15")



# all by age
res.all %>%
  filter(date>as.Date("2020-03-13"))%>%
  group_by(date,piter,age)%>%
  summarise(value=sum(value))%>%
  group_by(date,age)%>%
  summarise(vmax=quantile(value,qmax,type=4),vmin=quantile(value,qmin,type=4))%>%
  left_join(data%>%group_by(date,age)%>%summarise(pos.new=sum(pos.new)))%>%
  ggplot()+
  geom_ribbon(aes(x=date,ymin=vmin,ymax=vmax),alpha=.3)+
  geom_line(aes(x=date,y=pos.new),col="red")+
  facet_wrap(vars(age))+
  scale_y_log10()+
  theme_bw()+
  ylab("cases")
ggsave("./analysis/tex/plotsnew/oos_predictions.pdf",
       width = 6,height=5)

age.cur <- data$age%>%unique()

dfres <- res.all %>%
  filter(date<dat.last,date>dat.first)%>%
  filter(age %in% age.cur)%>%
  left_join(data,by=c("name","date","age"))%>%
  group_by(date,name,age)%>%
  summarise(
    qmin = quantile(value,probs=.1),
    qmax = quantile(value,probs=.9),
    coverage = pos.new <= qmax & pos.new >= qmin,
    covexact = pos.new ==qmax | pos.new == qmin
  )


#coverage
dfres %>%
  ungroup()%>%
  summarise(cov = mean(coverage)-mean(covexact)*.2)

dfres %>%
  group_by(age)%>%
  summarise(cov = mean(coverage)-mean(covexact)*.2)


dfres %>%
  group_by(date)%>%
  summarise(cov = mean(coverage)-mean(covexact)*.2)%>%
  ggplot(aes(x=date))+geom_point(aes(y=cov))+
  geom_hline(yintercept = .8,col="darkgrey")


