library(RCovModel)
library(tidyverse)
library(latex2exp)
library(tidybayes)

vec.path <- c(
  "./jags models/submission/save/submission.RData",
  "./jags models/submission/save/robust group101-01.RData",
  "./jags models/submission/save/robust group201-01.RData"
)
names(vec.path)<-c("main",
                  "one group",
                  "two groups")

theme_set(theme_bw())

load("./jags models/submission/save/submission.RData")


cov.choice <-
  c(macro$cov,
    setdiff(macro$dummies,macro$dummies.FE),
  "group1",
  "group2")

# CI length ---------------------------------------------------------------


# effects table -----------------------------------------------------------


### with age
for(path.cur in vec.path){

  load(path.cur)

  # show_basics()

  df.eff <- extract_effects(cov.choice)

  # average
  df.eff <- df.eff %>%
    group_by(m,age) %>%
    summarise(
      mean = mean(draw),
      up = quantile(draw,0.975),
      down = quantile(draw, 0.025),
      lengthCI = up-down
    )

  cat(path.cur,'\n')

  if(path.cur==vec.path[1])
    save.CI <- df.eff %>% pull(lengthCI)%>%mean()
  else
    cat("comparison:",df.eff %>% pull(lengthCI)%>%mean()/save.CI,'\n')

  cat(df.eff %>% pull(lengthCI)%>%mean(),'\n','\n')
}


# effects plot ------------------------------------------------------------




all <- tibble()
for(file.cur in vec.path){
  load(file.cur)

  cat(vec.path[vec.path==file.cur]%>%names()," with max Rhat: ",max(resMCMC$Rhat),"and running time ",macro$time.passed,units(macro$time.passed), "\n")
  cat('dispersion: ', resMCMC$mean[grepl('disp',rownames(resMCMC))], "\n", "\n")

  all <- rbind(all,
               extract_effects(cov.choice = cov.choice)%>%
                 mutate(run = vec.path[vec.path==file.cur]%>%names()))



}


res <- all%>%
  group_by(m,run,iteration)%>%
  summarise(average = mean(draw))%>%
  summarise(mean = mean(average),
            .lower = quantile(average,0.025),
            .upper = quantile(average,0.975))%>%
  group_by(m)%>%
  mutate(true = run=="main",
         m = as.factor(m),
         orderby = ifelse(n()<=2,-Inf,-mean[true]))%>%
  ungroup()%>%
  mutate(
    m = fct_reorder(m,orderby)
  )



limitsx <- c(-0.8,.5)

# sample from prior
sample.prior <- rnorm(1e5,0,sqrt(1/dat$na)*dat$effect_sd)
sample.prior <- sample.prior[sample.prior>-1]
prior.down <- quantile(sample.prior,probs = 0.025)
prior.up <- quantile(sample.prior,probs = 0.975)
dat.prior <- data.frame(prior.up=prior.up,prior.down=prior.down)

pd <- position_dodge(width = .7)

ggplot(res |>
         group_by(m) |>
         filter(n()==3|grepl("group",m))
         )+
  geom_rect(data= res |> slice(1) |> cbind(dat.prior),
            aes(xmin=prior.down, xmax=prior.up, ymin=-Inf, ymax=Inf),
            alpha=.1)+
  geom_vline(xintercept = 0, linetype = 2)+
  geom_errorbar(aes(y=m,x=mean,xmax=.upper,xmin=.lower,shape=run),
                position = pd)+
  geom_point(aes(y=m,x=mean,shape=run),
             position = pd)+
  scale_y_discrete(label = my_labeller2)+
  scale_shape_manual('model',values = c("main" = 4,"one group"=1,"two groups"=2))+
  theme(
    axis.title.y = element_blank()
  )+
  coord_cartesian(xlim=limitsx)
ggsave("./analysis/tex/plotsnew/res_robustR2_groups.pdf",
      width = 6,height=6)

