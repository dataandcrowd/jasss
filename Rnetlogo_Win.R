### RNetlogo Package ###

# --Windows Version--#
library(RNetLogo)
library(tidyverse)
nl.path <- "c:/Program Files/NetLogo 6.0.4/app"
NLStart(nl.path, gui=F, nl.obj=NULL, is3d=FALSE, nl.jarname='netlogo-6.0.4.jar')
model.path.win <- "d:/github/jasss/Gangnam_v6_macro.nlogo"
NLLoadModel(model.path.win)

new.col.names <- c( "riskpop", "d_sinsa", "d_nonhyun1", "d_nonhyun2",
                    "d_samsung1", "d_samsung2","d_daechi1","d_daechi4","d_yeoksam1",
                    "d_yeoksam2","d_dogok1","d_dogok2","d_gaepo1","d_gaepo4",
                    "d_ilwon","d_ilwon1","d_ilwon2","d_suseo", "d_ap","d_chungdam",
                    "d_daechi2","d_gaepo2","d_segok",
                    "a_u15","a_btw1564","a_ov65","e_high","e_low")

for (i in 1:10){
  NLCommand("setup")
  NLCommand (paste('set AC', 100))
  NLCommand (paste('set Scenario', '"BAU"'))
  NLCommand (paste('set scenario-percent', '"inc-sce"'))
  NLCommand (paste('set PM10-parameters', 100))
  
  simulation <- paste("model100",i, sep = ".")
  assign(simulation, NLDoReportWhile("ticks < 8764" , "go",
                                     c("%riskpop", "d_sinsa", "d_nonhyun1", "d_nonhyun2",
                                       "d_samsung1", "d_samsung2","d_daechi1","d_daechi4","d_yeoksam1",
                                       "d_yeoksam2","d_dogok1","d_dogok2","d_gaepo1","d_gaepo4",
                                       "d_ilwon","d_ilwon1","d_ilwon2","d_suseo", "d_ap","d_chungdam",
                                       "d_daechi2","d_gaepo2","d_segok",
                                       "a_u15","a_btw1564","a_ov65","e_high","e_low"), df.col.names= new.col.names,as.data.frame = T, max.minutes=150)
         
  )
  simulation %>% mutate(ticks = 1:8764, iteration = i)
  h <- paste("health100",i, sep = ".")
  assign(h, NLGetAgentSet(c("who",  "homename", "destinationName", "age", "health"), "people")) %>% mutate(iteration = i)
}

for (i in 1:10){
  NLCommand("setup")
  NLCommand (paste('set AC', 150))
  NLCommand (paste('set Scenario', '"BAU"'))
  NLCommand (paste('set scenario-percent', '"inc-sce"'))
  NLCommand (paste('set PM10-parameters', 100))
  
  simulation <- paste("model150",i, sep = ".")
  assign(simulation, NLDoReportWhile("ticks < 8764" , "go",
                                     c("%riskpop", "d_sinsa", "d_nonhyun1", "d_nonhyun2",
                                       "d_samsung1", "d_samsung2","d_daechi1","d_daechi4","d_yeoksam1",
                                       "d_yeoksam2","d_dogok1","d_dogok2","d_gaepo1","d_gaepo4",
                                       "d_ilwon","d_ilwon1","d_ilwon2","d_suseo", "d_ap","d_chungdam",
                                       "d_daechi2","d_gaepo2","d_segok",
                                       "a_u15","a_btw1564","a_ov65","e_high","e_low"), df.col.names= new.col.names,as.data.frame = T, max.minutes=150)
         
  )
  simulation %>% mutate(ticks = 1:8764, iteration = i)
  h <- paste("health100",i, sep = ".")
  assign(h, NLGetAgentSet(c("who",  "homename", "destinationName", "age", "health"), "people")) %>% mutate(iteration = i)
}

for (i in 1:10){
  NLCommand("setup")
  NLCommand (paste('set AC', 200))
  NLCommand (paste('set Scenario', '"BAU"'))
  NLCommand (paste('set scenario-percent', '"inc-sce"'))
  NLCommand (paste('set PM10-parameters', 100))
  
  simulation <- paste("model200",i, sep = ".")
  assign(simulation, NLDoReportWhile("ticks < 8764" , "go",
                                     c("%riskpop", "d_sinsa", "d_nonhyun1", "d_nonhyun2",
                                       "d_samsung1", "d_samsung2","d_daechi1","d_daechi4","d_yeoksam1",
                                       "d_yeoksam2","d_dogok1","d_dogok2","d_gaepo1","d_gaepo4",
                                       "d_ilwon","d_ilwon1","d_ilwon2","d_suseo", "d_ap","d_chungdam",
                                       "d_daechi2","d_gaepo2","d_segok",
                                       "a_u15","a_btw1564","a_ov65","e_high","e_low"), df.col.names= new.col.names,as.data.frame = T, max.minutes=150)
         
  )
  simulation %>% mutate(ticks = 1:8764, iteration = i)
  h <- paste("health100",i, sep = ".")
  assign(h, NLGetAgentSet(c("who",  "homename", "destinationName", "age", "health"), "people")) %>% mutate(iteration = i)
}

#save.image(file = "d:/github/jasss/jasss.Rdata")

model100 <- bind_rows(model100.1, model100.2, model100.3,model100.4,model100.5,model100.6,model100.7,model100.8,model100.9,model100.10) %>%
            group_by(ticks) %>%
            summarise_all(funs(mean))



#############################################
library(tidyverse)
library(reshape2)
library(gghighlight)

#risk <- total %>% select(riskpop,ticks)
#dong <- total %>% select(matches('d_'),ticks)
#age  <- total %>% select(matches('a_'),ticks)
#edu  <- total %>% select(matches('e_'),ticks)
#pm10 <- total %>% select(matches('pm10-'),ticks)

##
risk100 <- model100 %>%
           select(riskpop,ticks) %>% 
           melt(id = "ticks", variable.name = "total", value.name = "percent") 

risk100 %>% 
  ggplot(aes(ticks, percent, group = total)) + 
  geom_line(size = 1) +
  ylim(0,20) +
  theme_bw() +
  theme(axis.text=element_text(size=20),
        axis.title=element_text(size=15,face="bold")) +  
  xlab("ticks") +
  ylab("Percent of risk population(%)") 

##
dong100 <- model100 %>% 
        select(matches('d_'),ticks) %>%
        melt(id = "ticks", variable.name = "dongs", value.name = "percent")

ggplot(dong100) +
  geom_line(aes(ticks, percent, color = dongs), size = 1) +
  gghighlight(max(percent) > 8,
              label_params = list(size = 8)) +
  ylim(0,20) +
  theme_bw() +
  theme(legend.position="bottom",
        axis.text=element_text(size=20),
        axis.title=element_text(size=15,face="bold")) +  
  xlab("Days") +
  ylab("Percent of risk population(%)") 

##
age100 <- model100 %>% select(matches('a_'),ticks) %>% 
  melt(id = "ticks", variable.name = "age", value.name = "percent") 

age100 %>% 
  ggplot(aes(ticks, percent, group = age, color = age)) + 
  geom_line(size = 1)+
  gghighlight(max(percent) > 0,
              label_params = list(size = 8)) +
  ylim(0,20) +
  theme_bw() +
  theme(axis.text=element_text(size=20),
        axis.title=element_text(size=15,face="bold"),
        legend.position="none") +  
  xlab("Days") +
  ylab("Percent of risk population(%)") 


edu100 <- model100 %>% select(matches('e_'),ticks) %>%
          melt(id = "ticks", variable.name = "edu", value.name = "percent") 

edu100 %>% 
  ggplot(aes(ticks, percent, group = edu, color = edu)) + 
  geom_line(size = 1)+
  gghighlight(max(percent) > 0,
              label_params = list(size = 8)) +
  ylim(0,20) +
  theme_bw() +
  theme(axis.text=element_text(size=20),
        axis.title=element_text(size=15,face="bold"),
        legend.position="none") +  
  xlab("Days") +
  ylab("Percent of risk population(%)") 


options("scipen"=100, "digits"=4) ##Coz of decimal points


# health distribution: density plot!
healthy <- NLGetAgentSet(c("who",  "homename", "destinationName", "age", "health"), "people")
health <- healthy %>% filter(destinationName != "others") # remove agents destinated to others

health$health[health$health <= 0] <- 0

ggplot(health, aes(health, fill = destinationName)) + 
  geom_density(alpha = 0.4) +
  theme_bw() +
  theme(legend.title = element_text(size=20, face="bold"),
        legend.text = element_text(size=15),
        legend.position = c(0.2, 0.8),
        axis.text=element_text(size=20),
        axis.title=element_text(size=15,face="bold")
  )

#
healthzero <- health %>% filter(health==0)
ggplot(healthzero, aes(homename, fill = age)) + 
  geom_bar(stat = "count", position = "dodge")+
  theme_bw()


healthzero %>% 
  count(homename, age) %>% 
  ggplot(aes(x = reorder(homename, n, sum), y = n, fill = age)) + 
  geom_col() +
  xlab("") +
  ylab("Counts") +
  theme_bw() +
  theme(legend.title = element_text(size=20, face="bold"),
        legend.text = element_text(size=15),
        legend.position = c(0.1, 0.9),
        axis.text=element_text(size=13),
        axis.text.x = element_text(angle = 90, hjust = 1),
        axis.title=element_text(size=15,face="bold")
  )

