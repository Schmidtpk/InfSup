
# load data ---------------------------------------------------------------

library(CovidGer)
library(InfSup)
library(tidybayes)
library(latex2exp)
library(tidyverse)


# + simulation from model ---------------------------------------------------

load("./jags models/submission/save/submission.RData")
res.path <- "./jags models/submission/sim and est/save/"
number <- '12b'
theme_set(theme_bw())

cov.choice <- cov.main <- c(macro$cov,setdiff(macro$dummies,macro$dummies.FE))

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

all.noage <- all


# + simulation w age ------------------------------------------------------


load("./jags models/submission/save/submission.RData")
res.path <- "./jags models/sim age/save/"
number <- '1b'
theme_set(theme_bw())


file.list <- list.files(res.path)
file.list <- grep(paste0("sim_age_",number),file.list,value = TRUE)
all <- extract_effects(cov.choice)%>%
  mutate(run = 0,
         Rhatmax = max(resMCMC$Rhat))
print(samps%>%gather_draws(i_disp[age])%>%mean_qi)
print(samps%>%gather_draws(mean_transmission)%>%mean_qi)
for(file.cur in file.list){
  load(paste0("./jags models/sim age/save/",file.cur))

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
                 mutate(run = which(file.list==file.cur),
                        Rhatmax = max(resMCMC$Rhat)))

  print(samps%>%gather_draws(i_disp[age])%>%mean_qi)
  print(samps%>%gather_draws(mean_transmission)%>%mean_qi)

}
all.age <- all



# + sample from prior -----------------------------------------------------
### prior for age specifc effects
sample.prior <- rnorm(1e5,0,dat$effect_sd)
sample.prior <- sample.prior[sample.prior>-1]
prior.down <- quantile(sample.prior,probs = 0.025)
prior.up <- quantile(sample.prior,probs = 0.975)
dat.prior <- data.frame(prior.up=prior.up,prior.down=prior.down)
### prior for marginal effects
sample.prior <- rnorm(1e5,0,sqrt(1/dat$na)*dat$effect_sd)
sample.prior <- sample.prior[sample.prior>-1]
prior.down <- quantile(sample.prior,probs = 0.025)
prior.up <- quantile(sample.prior,probs = 0.975)
dat.prior2 <- data.frame(prior.up=prior.up,prior.down=prior.down)


# plot average effects -------------------------------------------------------------

### no age simulation
res.noage <- all.noage %>%
  left_join(Covid::regionaldatenbank%>%
              dplyr::filter(adm.level==1)%>%
              select(age,total)) %>%
  group_by(m,iteration,run)%>%
  summarise(
    draw = weighted.mean(draw,w = total),
    age = "average"
  ) |>
  group_by(m,run)%>%
  summarise(mean = mean(draw),
            .lower = quantile(draw,0.025),
            .upper = quantile(draw,0.975))%>%
  group_by(m)%>%
  mutate(true = run==0,
         m = as.factor(m),
         orderby = -mean[true])%>%
  ungroup()%>%
  mutate(
    m = fct_reorder(m,orderby)
  )

p.noage <- ggplot()+
  geom_rect(data= dat.prior2,
            aes(xmin=prior.down, xmax=prior.up, ymin=-Inf, ymax=Inf),
            alpha=.1)+
  geom_vline(xintercept = 0, linetype = 2)+
  geom_errorbar(data = res.noage%>%filter(true==FALSE),
                aes(y=m,x=mean,xmax=.upper,xmin=.lower),
                position = position_dodge2(width = 0.3),
                width=.3,
                alpha=1)+
  geom_point(data = res.noage%>%filter(true==TRUE),
             aes(y=m,x=mean,xmax=.upper,xmin=.lower),
             shape=4,size=3,stroke=1.5,color="red")+
  scale_y_discrete(label = my_labeller3)+
  scale_x_continuous(name="change in transmission",
                     labels = function(x) paste0(100*x,"%"))+
  theme(axis.title.y = element_blank())


### age simulation
res.age <- all.age %>%
  filter(Rhatmax < quantile(Rhatmax,probs=1)) |>
  left_join(Covid::regionaldatenbank%>%
              dplyr::filter(adm.level==1)%>%
              select(age,total)) %>%
  group_by(m,iteration,run)%>%
  summarise(
    draw = weighted.mean(draw,w = total),
    age = "average"
  ) |>
  group_by(m,run)%>%
  summarise(mean = mean(draw),
            .lower = quantile(draw,0.025),
            .upper = quantile(draw,0.975))%>%
  group_by(m)%>%
  mutate(true = run==0,
         m = as.factor(m),
         orderby = -mean[true])%>%
  ungroup()%>%
  mutate(
    m = fct_reorder(m,orderby)
  )

p.age <- ggplot()+
  geom_rect(data= dat.prior2,
            aes(xmin=prior.down, xmax=prior.up, ymin=-Inf, ymax=Inf),
            alpha=.1)+
  geom_vline(xintercept = 0, linetype = 2)+
  geom_errorbar(data = res.age%>%filter(true==FALSE),
                aes(y=m,x=mean,xmax=.upper,xmin=.lower),
                position = position_dodge2(width = 0.3),
                width=.3,
                alpha=1)+
  geom_point(data = res.age%>%filter(true==TRUE),
             aes(y=m,x=mean,xmax=.upper,xmin=.lower),
             shape=4,size=3,stroke=1.5,color="red")+
  scale_y_discrete(label = my_labeller3)+
  scale_x_continuous(name="change in transmission",
                     labels = function(x) paste0(100*x,"%"))+
  theme(axis.title.y = element_blank())

ggpubr::ggarrange(p.noage,p.age,ncol = 2,labels = c("A","B"),label.y = 1.015)
ggsave("./analysis/tex/plotsnew/res_sim_averageR.pdf",
       width = 8,height=4)
ggsave("./analysis/tex/plotsnew/res_sim_averageR.tif",
       width = 8,height=4, dpi = 800, device="tiff")

# + with age --------------------------------------------------------------
res <- all%>%
  group_by(m,run,age)%>%
  summarise(mean = mean(draw),
            .lower = quantile(draw,0.025),
            .upper = quantile(draw,0.975))%>%
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
  geom_rect(data= res |> group_by(age) |> slice(1) |> cbind(dat.prior),
            aes(xmin=prior.down, xmax=prior.up, ymin=-Inf, ymax=Inf),
            alpha=.1)+
  scale_y_discrete(label = my_labeller3)+
  facet_wrap(vars(age))+
  theme(
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
  )
ggsave("./analysis/tex/plotsnew/res_sim_ageR.pdf",
       width = 6,height=8)


# coverage
res |> filter(run!=0) |> select(m,mean,.lower,.upper,run,age) |>
  left_join(
    res |> filter(run==0) |> select(m,mean) |> rename(value = mean)
  ) |>
  group_by(age) |>
  summarise(coverage = mean(value<=.upper & value>= .lower),
            bias = mean(mean-value),
            absbias = mean(abs(mean)-abs(value)))
