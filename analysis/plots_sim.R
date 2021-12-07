library(Covid)
library(RCovModel)
library(tidybayes)
library(latex2exp)
library(tidyverse)


load("./jags models/submission/save/submission.RData")
res.path <- "./jags models/submission/sim and est/save/"
number <- '12b'
theme_set(theme_bw())

cov.choice <- cov.main <- setdiff(c(macro$cov,setdiff(macro$dummies,macro$dummies.FE)),
                                  c(#"sports",
                                    "shops2",
                                    "holidays",
                                    "events",
                                    "bars",
                                    "eventslimited",
                                    "symptomatic testing",
                                    "speechMerkel",
                                    "masks recommended"))

file.list <- list.files(res.path)
file.list <- grep(paste0("sim",number,"_"),file.list,value = TRUE)
all <- extract_effects(cov.choice)%>%
  mutate(run = 0)
print(samps%>%gather_draws(i_disp[age])%>%mean_qi)
print(samps%>%gather_draws(mean_transmission)%>%mean_qi)
for(file.cur in file.list){
  load(paste0("./jags models/submission/sim and est/save/",file.cur))

  show_basics()

  # plot(
  #   mymcmc::array_to_df(reports.sim)%>%filter(X2=='Berlin')%>%
  #   ggplot(aes(x=X1,group=X3,col=X3,y=value))+
  #   geom_line()+
  #   facet_wrap(vars(X2))+
  #   scale_y_log10()+
  #   ggtitle(file.cur))

  all <- rbind(all,
               extract_effects(cov.choice = cov.choice)%>%
                 mutate(run = which(file.list==file.cur)))

  print(samps%>%gather_draws(i_disp[age])%>%mean_qi)
  print(samps%>%gather_draws(mean_transmission)%>%mean_qi)

}



# + without age -------------------------------------------------------------
res <- all%>%
  group_by(m,run)%>%
  summarise(mean = mean(draw),
            .lower = quantile(draw,0.05),
            .upper = quantile(draw,0.95))%>%
  group_by(m)%>%
  mutate(true = run==0,
         m = as.factor(m),
         orderby = -mean[true])%>%
  ungroup()%>%
  mutate(
    m = fct_reorder(m,orderby)
  )


ggplot(mapping = aes(y=m,x=mean,xmax=.upper,xmin=.lower))+
  geom_vline(xintercept = 0, linetype = 2)+
  geom_errorbar(data = res%>%filter(true==FALSE),
                position = position_dodge2(width = 0.3),
                width=.3,
                alpha=1)+
  geom_point(data = res%>%filter(true==TRUE),shape=4,size=3,stroke=2,color="red")+
  scale_y_discrete(label = my_labeller2)





# + with age --------------------------------------------------------------
res <- all%>%
  group_by(m,run,age)%>%
  summarise(mean = mean(draw),
            .lower = quantile(draw,0.05),
            .upper = quantile(draw,0.95))%>%
  group_by(m,age)%>%
  mutate(true = run==0,
         m = as.factor(m),
         orderby = -mean[true])%>%
  ungroup()%>%
  mutate(
    m = fct_reorder(m,orderby),
    age = age_labels(age)
  )


ggplot(mapping = aes(y=m,x=mean,xmax=.upper,xmin=.lower))+
  geom_vline(xintercept = 0, linetype = 2)+
  geom_errorbar(data = res%>%filter(true==FALSE),
                position = position_dodge2(width = 0.3),
                width=.3,
                alpha=1)+
  geom_point(data = res%>%filter(true==TRUE),
             shape=4,size=2,stroke=1.5,color="red")+
  scale_y_discrete(label = my_labeller2)+
  facet_wrap(vars(age))+
  theme(
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
  )
ggsave("./analysis/tex/plotsnew/res_sim_age.pdf",
       width = 6,height=8)

