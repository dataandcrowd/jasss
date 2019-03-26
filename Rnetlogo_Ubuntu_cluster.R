### RNetlogo Package ###

# --Linux Version--#
library(RNetLogo)
library(doParallel)

nl.path <- "/usr/local/Cluster-Apps/netlogo/6.0.4/app"
NLStart(nl.path, gui=F, nl.obj=NULL, is3d=FALSE, nl.jarname='netlogo-6.0.4.jar')
model.path <- "/home/hs621/github/jasss/Gangnam_v6_macro.nlogo"
NLLoadModel(model.path)

new.col.names <- c( "riskpop", "d_sinsa", "d_nonhyun1", "d_nonhyun2",
                    "d_samsung1", "d_samsung2","d_daechi1","d_daechi4","d_yeoksam1",
                    "d_yeoksam2","d_dogok1","d_dogok2","d_gaepo1","d_gaepo4",
                    "d_ilwon","d_ilwon1","d_ilwon2","d_suseo", "d_ap","d_chungdam",
                    "d_daechi2","d_gaepo2","d_segok",
                    "a_u15","a_btw1564","a_ov65","e_high","e_low")

init <- Sys.time()
foreach (i = 1:50) %dopar% {
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
  h <- paste("health100",i, sep = ".")
  assign(h, NLGetAgentSet(c("who",  "homename", "destinationName", "age", "health"), "people")) 
}
Sys.time() - init


init <- Sys.time()
foreach (i = 1:50) %dopar% {
  NLCommand("setup")
  NLCommand (paste('set AC', 150))
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
  h <- paste("health100",i, sep = ".")
  assign(h, NLGetAgentSet(c("who",  "homename", "destinationName", "age", "health"), "people")) 
}
Sys.time() - init

init <- Sys.time()
foreach (i = 1:50) %dopar% {
  NLCommand("setup")
  NLCommand (paste('set AC', 200))
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
  h <- paste("health100",i, sep = ".")
  assign(h, NLGetAgentSet(c("who",  "homename", "destinationName", "age", "health"), "people")) 
}
Sys.time() - init


#######################################
#
save.image(file = "/home/hs621/github/jasss/cluster.Rdata")
stopCluster(cl)