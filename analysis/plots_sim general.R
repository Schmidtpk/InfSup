library(tidyverse)

df<- readRDS("./jags models/sim age/save/sim_diff_easy.RData")

# adapt i0 numbers. Generated i0 over 10 days, which equals i0/10 per day
df <- df |>
  mutate(i0 = i0/10) |>
  rename(qlow = "2.5%",
         qhigh = "97.5%")

# regressions ------------------------------------------------------------

df |> mutate(out =  qhigh<0 | qlow>0,
             out = abs(true.beta-mean)) |>
  select(true.beta,i_disp,i0,c,days.diff,
         out) |>
  lm(formula = out ~ .) |> stargazer::stargazer(type="text")


# average infections in county during study period
load("./jags models/submission/save/submission.RData")
data |> group_by(date) |> filter(date>as.Date("2020-03-12"),
                                 date<as.Date("2020-05-01")) |>
  summarise(mpos=sum(pos.new)*4/111) |> pull() |> summary()
# -> 20 to 60
# days difference * locations is between 100 and 2000
# minimal value of days * locations * infections is 100*20, maximum is 2000*60


data |> group_by(date) |> filter(date>as.Date("2020-03-12"),
                                 date<as.Date("2020-05-01")) |>
  summarise(mpos=sum(pos.new)/111) |> pull() |> summary()
# -> 5 to 15
# reporting rate 25% --> 20 to 60
# days difference * locations is between 100 and 2000
# minimal value of days * locations * infections is 100*20, maximum is 2000*60
# --> 2000 to 120 000

# plots -------------------------------------------------------------------

# infection-days power
df |>
  rename(dispersion = i_disp,
         effect = true.beta) |>
  group_by(c,days.diff,i0,effect,dispersion) |>
  summarise(out = mean(qhigh<0 | qlow>0)) |>
  mutate(infectiondays = c*days.diff*i0) |>
  ggplot()+
  geom_hline(yintercept = c(0,1))+
  geom_rect(data = data.frame(xmin=20*100,xmax=60*2000,ymin=-1,ymax=2),
            aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax),alpha=.2)+
  geom_point(aes(infectiondays,out,shape=factor(days.diff),size=factor(c),col=factor(i0)),
             alpha=.5)+
  scale_color_discrete(name="daily infections")+
  scale_size_discrete(name="locations")+
  scale_shape_discrete(name="days")+
  facet_grid(effect~dispersion,labeller = label_both)+
  xlab("infections")+
  ylab("rejection rate")+
  theme_bw()+
  scale_x_log10()+
  coord_cartesian(ylim=c(0,1))
ggsave("./analysis/tex/plotsnew/sim_diff_power.pdf",width = 10,height = 5)
ggsave("./analysis/tex/plotsnew/sim_diff_power.tif",width = 10,height = 5, dpi = 800, device="tiff")


# infection-days mae
df |>
  rename(dispersion = i_disp,
         effect = true.beta) |>
  group_by(c,days.diff,i0,effect,dispersion) |>
  summarise(out = mean(abs(mean-effect))) |>
  mutate(infectiondays = c*days.diff*i0) |>
  ggplot()+
  geom_hline(yintercept = 0)+
  geom_rect(data = data.frame(xmin=20*100,xmax=60*2000,ymin=-1,ymax=2),
            aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax),alpha=.2)+
  geom_point(aes(infectiondays,out,shape=factor(days.diff),size=factor(c),col=factor(i0)),
             alpha=.5)+
  scale_color_discrete(name="daily infections")+
  scale_size_discrete(name="locations")+
  scale_shape_discrete(name="days")+
  facet_grid(effect~dispersion, labeller = label_both)+
  xlab("infections")+
  ylab("mean absolute error")+
  theme_bw()+
  scale_x_log10()+
  coord_cartesian(ylim=c(0,.3))
ggsave("./analysis/tex/plotsnew/sim_diff_mae.pdf",width = 10,height = 5)


