library(tidyverse)
library(gghighlight)

#gn.bau <- read.csv("behaviourspace/Gangnam_v6_macro BAU-table.csv")
#gn.inc <- read.csv("behaviourspace/Gangnam_v6_macro INC-table.csv")
#gn.dec <- read.csv("behaviourspace/Gangnam_v6_macro DEC-table.csv")
#gw.bau <- read.csv("behaviourspace/Gwanak_v6_macro BAU-table.csv")
#gw.inc <- read.csv("behaviourspace/Gwanak_v6_macro INC-table.csv")
#gw.dec <- read.csv("behaviourspace/Gwanak_v6_macro DEC-table.csv")

files  <- list.files(pattern= "BAU.*.csv")
tables <- lapply(files, read.csv, header = TRUE)#, fileEncoding="CP949", encoding="UTF-8") # MacOSX
bau <- do.call(rbind, tables)  
  
gn.col.names <- c("iteration", "scenario","AC","percent",	"ticks",
                  "riskpop", "d_sinsa", "d_nonhyun1", "d_nonhyun2",
                  "d_samsung1", "d_samsung2","d_daechi1","d_daechi4","d_yeoksam1",
                  "d_yeoksam2","d_dogok1","d_dogok2","d_gaepo1","d_gaepo4",
                  "d_ilwon","d_ilwon1","d_ilwon2","d_suseo", "d_ap","d_chungdam",
                  "d_daechi2","d_gaepo2","d_segok",
                  "age_u15","age_btw1564","age_ov65","edu_high","edu_low")

gw.col.names <- c("iteration", "scenario","AC","percent",	"ticks",
                  "riskpop", "d_boramae", "d_chnim", "d_hengun", "d_inheon", "d_nak", 
                  "d_jung-ang", "d_namhyeon", "d_seowon", "d_sinwon", "d_seorim", "d_sinsa", 
                  "d_sillim", "d_nanhyang", "d_jowon", "d_daehak", "d_euncheon", "d_sunghyun", 
                  "d_chungryong", "d_nangok", "d_samsung", "d_miseong", 
                  "age_u15","age_btw1564","age_ov65","edu_high","edu_low")
colnames(bau) <- gn.col.names
#colnames(gn.inc) <- gn.col.names
#colnames(gn.dec) <- gn.col.names
#colnames(gw.bau) <- gw.col.names
#colnames(gw.inc) <- gw.col.names
#colnames(gw.dec) <- gw.col.names



bau<- bind_rows(gn.bau %>% select(ticks, riskpop, AC, scenario, age_u15,age_btw1564,age_ov65,edu_high,edu_low) %>% mutate(District= "Gangnam"),
                  gw.bau %>% select(ticks, riskpop, AC, scenario, age_u15,age_btw1564,age_ov65,edu_high,edu_low) %>% mutate(District= "Gwanak")) %>% 
        group_by(District, scenario, AC, ticks) %>%
        summarise_all(funs(mean))

bau$AC[bau$AC == 100] <- "AC100"
bau$AC[bau$AC == 150] <- "AC150"
bau$AC[bau$AC == 200] <- "AC200"

bau.risk <- bau %>%
    reshape2::melt(id = c("ticks", "District", "scenario","AC"), variable.name = "type", value.name = "percent") 

bau.risk %>% 
  ggplot(aes(ticks, percent, group = District, colour = District)) + 
  geom_line(size = 1) +
  ylim(0,50) +
  xlab("ticks") +
  ylab("Percent of risk population(%)")  +
  facet_grid(scenario + AC ~ type) +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.title=element_text(size=12),
        legend.text=element_text(size=12),
        axis.text=element_text(size=15),
        axis.title=element_text(size=15),
        strip.text = element_text(size = 15))

######################
#Subdistrict analysis#
######################
#gn.dong.bau <- gn.bau %>% select(-c(riskpop, iteration,percent, age_u15, age_btw1564, age_ov65, edu_high, edu_low)) %>% 
#  group_by(scenario, AC, ticks) %>%
#  summarise_all(funs(mean))
#gn.dong.inc <- gn.inc %>% select(-c(riskpop, iteration,percent, age_u15, age_btw1564, age_ov65, edu_high, edu_low)) %>%
#  group_by(scenario, AC, ticks) %>%
#  summarise_all(funs(mean))
#gn.dong.dec <- gn.dec %>% select(-c(riskpop, iteration,percent, age_u15, age_btw1564, age_ov65, edu_high, edu_low)) %>% 
#  group_by(scenario, AC, ticks) %>%
#  summarise_all(funs(mean))
#gn <- bind_rows(gn.dong.bau, gn.dong.inc, gn.dong.dec)
library(gridExtra)
library(gghighlight)
library(tidyverse)
library(ggpubr)
library(directlabels)
library(tidyquant)
library(cowplot)

gn <- read.csv("behaviourspace/gn_dong.csv")
gn$AC[gn$AC == 100] <- "AC100"
gn$AC[gn$AC == 150] <- "AC150"
gn$AC[gn$AC == 200] <- "AC200"

gn.melt <- gn  %>% 
  reshape2::melt(id = c("ticks","scenario","AC") , variable.name = "dongs", value.name = "percent")
gn.melt$dongs <- gsub("d_", "", gn.melt$dongs) # delete d_


########
bb100 <- subset(gn.melt, AC=="AC100" & scenario == "BAU" & ticks == 8763)
bb150 <- subset(gn.melt, AC=="AC150" & scenario == "BAU" & ticks == 8763)
bb200 <- subset(gn.melt, AC=="AC200" & scenario == "BAU" & ticks == 8763)
#for (i in 1:length(test$dongs)){
#  test$dongs <- capitalize(test$dongs[i])
#}
plotbb100 <- bb100 %>%
  arrange(percent) %>%
  mutate(dongs=factor(dongs, levels=dongs)) %>%
  ggplot( aes(x=dongs, y=percent)) +
  geom_segment( aes(xend= dongs, yend=0)) +
  geom_point( size=2, color="orange") +
  coord_flip() +
  xlab("Districts") +
  ylab("Percent of risk population(%)")  +
  theme_bw()+
  theme(legend.position = "bottom",
        legend.title=element_text(size=9),
        legend.text=element_text(size=9),
        axis.text=element_text(size=9),
        axis.title=element_text(size=9),
        strip.text = element_text(size = 9))

bl100 <- gn.melt %>% filter(scenario == "BAU" & AC == "AC100") %>% 
  ggplot(aes(x=ticks, y=percent, group = dongs, color = dongs)) +
  geom_line(size = 0.8, alpha = 0.1) +
  geom_ma(ma_fun = SMA, n = 500, linetype = "solid", size = 1) +
  ylim(0,50) +
  xlim(0,10000) +
  xlab("ticks") +
  ylab("Percent of risk population(%)")  +
  #annotate("text", x = 4382, y = 19, label = "BAU x AC100", size = 7, fontface="bold") +
  theme_bw() +
  theme(legend.position = "",
        legend.title=element_text(size=12),
        legend.text=element_text(size=12),
        axis.text=element_text(size=12),
        axis.title=element_text(size=12),
        strip.text = element_text(size =12)) 
plotbl100 <- direct.label(bl100,list(dl.trans(x=x+0.1), "last.qp",cex=.9, vjust=-1))

plotbb150 <- bb150 %>%
  arrange(percent) %>%
  mutate(dongs=factor(dongs, levels=dongs)) %>%
  ggplot( aes(x=dongs, y=percent)) +
  geom_segment( aes(xend= dongs, yend=0)) +
  geom_point( size=2, color="orange") +
  coord_flip() +
  xlab("Districts") +
  ylab("Percent of risk population(%)")  +
  theme_bw()+
  theme(legend.position = "bottom",
        legend.title=element_text(size=9),
        legend.text=element_text(size=9),
        axis.text=element_text(size=9),
        axis.title=element_text(size=9),
        strip.text = element_text(size = 9))

bl150 <- gn.melt %>% filter(scenario == "BAU" & AC == "AC150") %>% 
  ggplot(aes(x=ticks, y=percent, group = dongs, color = dongs)) +
  geom_line(size = 0.8, alpha = 0.1) +
  geom_ma(ma_fun = SMA, n = 500, linetype = "solid", size = 1) +
  ylim(0,50) +
  xlim(0,10000) +
  xlab("ticks") +
  ylab("Percent of risk population(%)")  +
  #annotate("text", x = 4382, y = 19, label = "BAU x AC100", size = 7, fontface="bold") +
  theme_bw() +
  theme(legend.position = "",
        legend.title=element_text(size=12),
        legend.text=element_text(size=12),
        axis.text=element_text(size=12),
        axis.title=element_text(size=12),
        strip.text = element_text(size =12)) 
plotbl150 <- direct.label(bl150,list(dl.trans(x=x+0.1), "last.qp",cex=.9, vjust=-1))


plotbb200 <- bb200 %>%
  arrange(percent) %>%
  mutate(dongs=factor(dongs, levels=dongs)) %>%
  ggplot( aes(x=dongs, y=percent)) +
  geom_segment( aes(xend= dongs, yend=0)) +
  geom_point( size=2, color="orange") +
  coord_flip() +
  xlab("Districts") +
  ylab("Percent of risk population(%)")  +
  theme_bw()+
  theme(legend.position = "bottom",
        legend.title=element_text(size=9),
        legend.text=element_text(size=9),
        axis.text=element_text(size=9),
        axis.title=element_text(size=9),
        strip.text = element_text(size = 9))

bl200 <- gn.melt %>% filter(scenario == "BAU" & AC == "AC200") %>% 
  ggplot(aes(x=ticks, y=percent, group = dongs, color = dongs)) +
  geom_line(size = 0.8, alpha = 0.1) +
  geom_ma(ma_fun = SMA, n = 500, linetype = "solid", size = 1) +
  ylim(0,50) +
  xlim(0,10000) +
  xlab("ticks") +
  ylab("Percent of risk population(%)")  +
  #annotate("text", x = 4382, y = 19, label = "BAU x AC100", size = 7, fontface="bold") +
  theme_bw() +
  theme(legend.position = "",
        legend.title=element_text(size=12),
        legend.text=element_text(size=12),
        axis.text=element_text(size=12),
        axis.title=element_text(size=12),
        strip.text = element_text(size =12)) 
plotbl200 <- direct.label(bl200,list(dl.trans(x=x+0.1), "last.qp",cex=.9, vjust=-1))

#plot_grid(bl100, bl150, bl200, bb100, bb150, bb200, labels = c("a", "b", "c", "a'", "b'", "c'"), align = "h")


############
##INC
############
ib100 <- subset(gn.melt, AC=="AC100" & scenario == "INC" & ticks == 8763)
ib150 <- subset(gn.melt, AC=="AC150" & scenario == "INC" & ticks == 8763)
ib200 <- subset(gn.melt, AC=="AC200" & scenario == "INC" & ticks == 8763)
#for (i in 1:length(test$dongs)){
#  test$dongs <- capitalize(test$dongs[i])
#}
plotib100 <- ib100 %>%
  arrange(percent) %>%
  mutate(dongs=factor(dongs, levels=dongs)) %>%
  ggplot( aes(x=dongs, y=percent)) +
  geom_segment( aes(xend= dongs, yend=0)) +
  geom_point( size=2, color="red") +
  coord_flip() +
  xlab("Districts") +
  ylab("Percent of risk population(%)")  +
  theme_bw()+
  theme(legend.position = "bottom",
        legend.title=element_text(size=9),
        legend.text=element_text(size=9),
        axis.text=element_text(size=9),
        axis.title=element_text(size=9),
        strip.text = element_text(size = 9))

il100 <- gn.melt %>% filter(scenario == "INC" & AC == "AC100") %>% 
  ggplot(aes(x=ticks, y=percent, group = dongs, color = dongs)) +
  geom_line(size = 0.8, alpha = 0.1) +
  geom_ma(ma_fun = SMA, n = 500, linetype = "solid", size = 1) +
  ylim(0,50) +
  xlim(0,10000) +
  xlab("ticks") +
  ylab("Percent of risk population(%)")  +
  #annotate("text", x = 4382, y = 19, label = "INC x AC100", size = 7, fontface="bold") +
  theme_bw() +
  theme(legend.position = "",
        legend.title=element_text(size=12),
        legend.text=element_text(size=12),
        axis.text=element_text(size=12),
        axis.title=element_text(size=12),
        strip.text = element_text(size =12)) 
plotil100 <- direct.label(il100,list(dl.trans(x=x+0.1), "last.qp",cex=.9, vjust=-1))

plotib150 <- ib150 %>%
  arrange(percent) %>%
  mutate(dongs=factor(dongs, levels=dongs)) %>%
  ggplot( aes(x=dongs, y=percent)) +
  geom_segment( aes(xend= dongs, yend=0)) +
  geom_point( size=2, color="red") +
  coord_flip() +
  xlab("Districts") +
  ylab("Percent of risk population(%)")  +
  theme_bw()+
  theme(legend.position = "bottom",
        legend.title=element_text(size=9),
        legend.text=element_text(size=9),
        axis.text=element_text(size=9),
        axis.title=element_text(size=9),
        strip.text = element_text(size = 9))

il150 <- gn.melt %>% filter(scenario == "INC" & AC == "AC150") %>% 
  ggplot(aes(x=ticks, y=percent, group = dongs, color = dongs)) +
  geom_line(size = 0.8, alpha = 0.1) +
  geom_ma(ma_fun = SMA, n = 500, linetype = "solid", size = 1) +
  ylim(0,50) +
  xlim(0,10000) +
  xlab("ticks") +
  ylab("Percent of risk population(%)")  +
  #annotate("text", x = 4382, y = 19, label = "INC x AC100", size = 7, fontface="bold") +
  theme_bw() +
  theme(legend.position = "",
        legend.title=element_text(size=12),
        legend.text=element_text(size=12),
        axis.text=element_text(size=12),
        axis.title=element_text(size=12),
        strip.text = element_text(size =12)) 
plotil150 <- direct.label(il150,list(dl.trans(x=x+0.1), "last.qp",cex=.9, vjust=-1))


plotib200 <- ib200 %>%
  arrange(percent) %>%
  mutate(dongs=factor(dongs, levels=dongs)) %>%
  ggplot( aes(x=dongs, y=percent)) +
  geom_segment( aes(xend= dongs, yend=0)) +
  geom_point( size=2, color="red") +
  coord_flip() +
  xlab("Districts") +
  ylab("Percent of risk population(%)")  +
  theme_bw()+
  theme(legend.position = "bottom",
        legend.title=element_text(size=9),
        legend.text=element_text(size=9),
        axis.text=element_text(size=9),
        axis.title=element_text(size=9),
        strip.text = element_text(size = 9))

il200 <- gn.melt %>% filter(scenario == "INC" & AC == "AC200") %>% 
  ggplot(aes(x=ticks, y=percent, group = dongs, color = dongs)) +
  geom_line(size = 0.8, alpha = 0.1) +
  geom_ma(ma_fun = SMA, n = 500, linetype = "solid", size = 1) +
  ylim(0,50) +
  xlim(0,10000) +
  xlab("ticks") +
  ylab("Percent of risk population(%)")  +
  #annotate("text", x = 4382, y = 19, label = "INC x AC100", size = 7, fontface="bold") +
  theme_bw() +
  theme(legend.position = "",
        legend.title=element_text(size=12),
        legend.text=element_text(size=12),
        axis.text=element_text(size=12),
        axis.title=element_text(size=12),
        strip.text = element_text(size =12)) 
plotil200 <- direct.label(il200,list(dl.trans(x=x+0.1), "last.qp",cex=.9, vjust=-1))

############
##DEC
#########
########
db100 <- subset(gn.melt, AC=="AC100" & scenario == "DEC" & ticks == 8763)
db150 <- subset(gn.melt, AC=="AC150" & scenario == "DEC" & ticks == 8763)
db200 <- subset(gn.melt, AC=="AC200" & scenario == "DEC" & ticks == 8763)
#for (i in 1:length(test$dongs)){
#  test$dongs <- capitalize(test$dongs[i])
#}
plotdb100 <- db100 %>%
  arrange(percent) %>%
  mutate(dongs=factor(dongs, levels=dongs)) %>%
  ggplot( aes(x=dongs, y=percent)) +
  geom_segment( aes(xend= dongs, yend=0)) +
  geom_point( size=2, color="blue") +
  coord_flip() +
  xlab("Districts") +
  ylab("Percent of risk population(%)")  +
  theme_bw()+
  theme(legend.position = "bottom",
        legend.title=element_text(size=9),
        legend.text=element_text(size=9),
        axis.text=element_text(size=9),
        axis.title=element_text(size=9),
        strip.text = element_text(size = 9))

dl100 <- gn.melt %>% filter(scenario == "DEC" & AC == "AC100") %>% 
  ggplot(aes(x=ticks, y=percent, group = dongs, color = dongs)) +
  geom_line(size = 0.8, alpha = 0.1) +
  geom_ma(ma_fun = SMA, n = 500, linetype = "solid", size = 1) +
  ylim(0,50) +
  xlim(0,10000) +
  xlab("ticks") +
  ylab("Percent of risk population(%)")  +
  #annotate("text", x = 4382, y = 19, label = "DEC x AC100", size = 7, fontface="bold") +
  theme_bw() +
  theme(legend.position = "",
        legend.title=element_text(size=12),
        legend.text=element_text(size=12),
        axis.text=element_text(size=12),
        axis.title=element_text(size=12),
        strip.text = element_text(size =12)) 
plotdl100 <- direct.label(dl100,list(dl.trans(x=x+0.1), "last.qp",cex=.9, vjust=-1))

plotdb150 <- db150 %>%
  arrange(percent) %>%
  mutate(dongs=factor(dongs, levels=dongs)) %>%
  ggplot( aes(x=dongs, y=percent)) +
  geom_segment( aes(xend= dongs, yend=0)) +
  geom_point( size=2, color="blue") +
  coord_flip() +
  xlab("Districts") +
  ylab("Percent of risk population(%)")  +
  theme_bw()+
  theme(legend.position = "bottom",
        legend.title=element_text(size=9),
        legend.text=element_text(size=9),
        axis.text=element_text(size=9),
        axis.title=element_text(size=9),
        strip.text = element_text(size = 9))

dl150 <- gn.melt %>% filter(scenario == "DEC" & AC == "AC150") %>% 
  ggplot(aes(x=ticks, y=percent, group = dongs, color = dongs)) +
  geom_line(size = 0.8, alpha = 0.1) +
  geom_ma(ma_fun = SMA, n = 500, linetype = "solid", size = 1) +
  ylim(0,50) +
  xlim(0,10000) +
  xlab("ticks") +
  ylab("Percent of risk population(%)")  +
  #annotate("text", x = 4382, y = 19, label = "DEC x AC100", size = 7, fontface="bold") +
  theme_bw() +
  theme(legend.position = "",
        legend.title=element_text(size=12),
        legend.text=element_text(size=12),
        axis.text=element_text(size=12),
        axis.title=element_text(size=12),
        strip.text = element_text(size =12)) 
plotdl150 <- direct.label(dl150,list(dl.trans(x=x+0.1), "last.qp",cex=.9, vjust=-1))


plotdb200 <- db200 %>%
  arrange(percent) %>%
  mutate(dongs=factor(dongs, levels=dongs)) %>%
  ggplot( aes(x=dongs, y=percent)) +
  geom_segment( aes(xend= dongs, yend=0)) +
  geom_point( size=2, color="blue") +
  coord_flip() +
  xlab("Districts") +
  ylab("Percent of risk population(%)")  +
  theme_bw()+
  theme(legend.position = "bottom",
        legend.title=element_text(size=9),
        legend.text=element_text(size=9),
        axis.text=element_text(size=9),
        axis.title=element_text(size=9),
        strip.text = element_text(size = 9))

dl200 <- gn.melt %>% filter(scenario == "DEC" & AC == "AC200") %>% 
  ggplot(aes(x=ticks, y=percent, group = dongs, color = dongs)) +
  geom_line(size = 0.8, alpha = 0.1) +
  geom_ma(ma_fun = SMA, n = 500, linetype = "solid", size = 1) +
  ylim(0,50) +
  xlim(0,10000) +
  xlab("ticks") +
  ylab("Percent of risk population(%)")  +
  #annotate("text", x = 4382, y = 19, label = "DEC x AC100", size = 7, fontface="bold") +
  theme_bw() +
  theme(legend.position = "",
        legend.title=element_text(size=12),
        legend.text=element_text(size=12),
        axis.text=element_text(size=12),
        axis.title=element_text(size=12),
        strip.text = element_text(size =12)) 
plotdl200 <- direct.label(dl200,list(dl.trans(x=x+0.1), "last.qp",cex=.9, vjust=-1))


b100 <- plotbl100 + annotation_custom(grob = ggplotGrob(plotbb100), xmin = 100, xmax = 7000, ymin = 15, ymax = 50)
b150 <- plotbl150 + annotation_custom(grob = ggplotGrob(plotbb150), xmin = 100, xmax = 7000, ymin = 15, ymax = 50)
b200 <- plotbl200 + annotation_custom(grob = ggplotGrob(plotbb200), xmin = 100, xmax = 7000, ymin = 15, ymax = 50)

i100 <- plotil100 + annotation_custom(grob = ggplotGrob(plotib100), xmin = 100, xmax = 7000, ymin = 15, ymax = 50)
i150 <- plotil150 + annotation_custom(grob = ggplotGrob(plotib100), xmin = 100, xmax = 7000, ymin = 15, ymax = 50)
i200 <- plotil200 + annotation_custom(grob = ggplotGrob(plotib100), xmin = 100, xmax = 7000, ymin = 15, ymax = 50)

d100 <- plotdl100 + annotation_custom(grob = ggplotGrob(plotdb100), xmin = 100, xmax = 7000, ymin = 15, ymax = 50)
d150 <- plotdl150 + annotation_custom(grob = ggplotGrob(plotdb150), xmin = 100, xmax = 7000, ymin = 15, ymax = 50)
d200 <- plotdl200 + annotation_custom(grob = ggplotGrob(plotdb200), xmin = 100, xmax = 7000, ymin = 15, ymax = 50)


plot_grid(b100, b150, b200, i100, i150, i200, d100, d150, d200, 
          labels = c("A", "B", "C", "D", "E", "F","G", "H", "I"), align = "h")

