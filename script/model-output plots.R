#Spread of antimalaria drug resistance with IPTp
#Created by Deus Thindwa
#Created on 20/03/2018

malaria.res.packages <- c("plyr", "tidyverse", "data.table","gridExtra", "grid","gtools", "here" )
lapply(malaria.res.packages, library, character.only=TRUE)

#==========SIMULATED SCENARIOS TO EQUILIBRIA (FIGURE 2)==========

#load and wrangle to get datasets in right shape
simu.m <- read.table(here("data", "simu.mod.txt"), header = TRUE)
simu.m$Prev.1 <- simu.m$Prev.2 <- NULL
simu.m1 <- simu.m[c(1,2)]
simu.m2 <- simu.m[c(1,6)]
simu.m3 <- simu.m[c(1,10)]
simu.m4 <- simu.m[c(1,11)]
setnames(simu.m1, old=c("R1.1"), new=c("R"))
simu.m1$Scenario <- "Resistance to ART alone"
simu.m1$Infection <- simu.m4$Prev.3
setnames(simu.m2, old=c("R2.2"), new=c("R"))
simu.m2$Scenario <- "Resistance to PQP alone"
simu.m2$Infection <- simu.m4$Prev.3
setnames(simu.m3, old=c("R3.3"), new=c("R"))
simu.m3$Scenario <- "Resistance to ART+PQP"
simu.m3$Infection <- simu.m4$Prev.3
simu.m <- bind_rows(simu.m1, simu.m2, simu.m3)
simu.m$Prevalence <- "B"

simu.h <- read.table(here("data", "simu.high.txt"), header = TRUE)
simu.h$Prev.1 <- simu.h$Prev.2 <- NULL
simu.h1 <- simu.h[c(1,2)]
simu.h2 <- simu.h[c(1,6)]
simu.h3 <- simu.h[c(1,10)]
simu.h4 <- simu.h[c(1,11)]
setnames(simu.h1, old=c("R1.1"), new=c("R"))
simu.h1$Scenario <- "Resistance to ART alone"
simu.h1$Infection <- simu.h4$Prev.3
setnames(simu.h2, old=c("R2.2"), new=c("R"))
simu.h2$Scenario <- "Resistance to PQP alone"
simu.h2$Infection <- simu.h4$Prev.3
setnames(simu.h3, old=c("R3.3"), new=c("R"))
simu.h3$Scenario <- "Resistance to ART+PQP"
simu.h3$Infection <- simu.h4$Prev.3
simu.h <- bind_rows(simu.h1, simu.h2, simu.h3)
simu.h$Prevalence <- "A"

res.equilibrium <- bind_rows(simu.m, simu.h)
res.equilibrium$TIME <- res.equilibrium$TIME/365.25
res.equilibrium <- subset(res.equilibrium, TIME>=10 & TIME<=40)
res.equilibrium$R <- res.equilibrium$R*100
res.equilibrium$Infection <- res.equilibrium$Infection*100
rm(list = ls()[grep("^simu", ls())])

#Plot the time-resistance dynamics
ggplot(data = subset(res.equilibrium)) + 
  theme_bw() + 
  coord_cartesian(ylim=c(0,100)) + 
  geom_line(aes(x=TIME, y=Infection, lty = "Infection prevalence"), size = 1) + 
  geom_line(aes(x=TIME, y=R/0.1, color=Scenario), size = 1) + 
  scale_y_continuous(sec.axis = sec_axis(~.*0.1, name = "% Resistant")) +
  scale_color_manual(values = c("springgreen3","red3","royalblue3")) + 
  facet_grid(.~ Prevalence, scales="free_y") + 
  geom_vline(xintercept = 10, linetype="dashed", color = "black", size=0.6) + 
  labs(title="", x ="Year", y = "% Infection prevalence") + 
  theme(strip.text.x = element_text(face="bold", size = 12, color="black")) +
  theme(axis.title.x = element_text(size = 11)) + 
  theme(axis.title.y = element_text(size = 11)) +
  theme(axis.text.x = element_text(face="bold", size=10), axis.text.y = element_text(face="bold", size=10)) + 
  theme(legend.justification=c(0.5,0), legend.position = "right", legend.text = element_text(size = 11), legend.title = element_text(face="bold", size=0)) + 
  theme(legend.key.height=unit(1.5,"line")) + 
  theme(legend.key.width=unit(2,"line")) 

#==========IPTp vs CASE MANAGEMENT AND SPREAD OF RESISTANCE (FIGURE 3)==========

#load and wrangle to get datasets in right shape
cmiptp.highA <- read.table(here("data", "cmiptp.high1.txt"), header = TRUE)
cmiptp.high1 <- cmiptp.highA[c(1,2)]
cmiptp.high2 <- cmiptp.highA[c(1,5)]
cmiptp.high3 <- cmiptp.highA[c(1,8)]
setnames(cmiptp.high1, old=c("R1.1"), new=c("R"))
setnames(cmiptp.high2, old=c("R1.2"), new=c("R"))
setnames(cmiptp.high3, old=c("R1.3"), new=c("R"))
cmiptp.high1$Policy <- "CM & IPTp"
cmiptp.high2$Policy <- "CM only"
cmiptp.high3$Policy <- "IPTp only"
cmiptp.high1$Scenario <- "B"
cmiptp.high2$Scenario <- "B"
cmiptp.high3$Scenario <- "B"
cmA <- bind_rows(cmiptp.high1, cmiptp.high2, cmiptp.high3)

cmiptp.highB <- read.table(here("data", "cmiptp.high2.txt"), header = TRUE)
cmiptp.highB$R2.3 <- cmiptp.highB$R2.1
cmiptp.high1 <- cmiptp.highB[c(1,3)]
cmiptp.high2 <- cmiptp.highB[c(1,6)]
cmiptp.high3 <- cmiptp.highB[c(1,8)]
setnames(cmiptp.high1, old=c("R2.1"), new=c("R"))
setnames(cmiptp.high2, old=c("R2.2"), new=c("R"))
setnames(cmiptp.high3, old=c("R2.3"), new=c("R"))
cmiptp.high1$Policy <- "CM & IPTp"
cmiptp.high2$Policy <- "CM only"
cmiptp.high3$Policy <- "IPTp only"
cmiptp.high1$Scenario <- "C"
cmiptp.high2$Scenario <- "C"
cmiptp.high3$Scenario <- "C"
cmB <- bind_rows(cmiptp.high1, cmiptp.high2, cmiptp.high3)

cmiptp.highC <- read.table(here("data", "cmiptp.high3.txt"), header = TRUE)
cmiptp.high1 <- cmiptp.highC[c(1,4)]
cmiptp.high2 <- cmiptp.highC[c(1,7)]
cmiptp.high3 <- cmiptp.highC[c(1,10)]
setnames(cmiptp.high1, old=c("R3.1"), new=c("R"))
setnames(cmiptp.high2, old=c("R3.2"), new=c("R"))
setnames(cmiptp.high3, old=c("R3.3"), new=c("R"))
cmiptp.high1$Policy <- "CM & IPTp"
cmiptp.high2$Policy <- "CM only"
cmiptp.high3$Policy <- "IPTp only"
cmiptp.high1$Scenario <- "A"
cmiptp.high2$Scenario <- "A"
cmiptp.high3$Scenario <- "A"
cmC <- bind_rows(cmiptp.high1, cmiptp.high2, cmiptp.high3)

res.policy <- bind_rows(cmA, cmB, cmC)
res.policy$TIME <- res.policy$TIME/365.25
res.policy <- subset(res.policy, TIME>10 & TIME<=40)
res.policy$R[res.policy$TIME <12] <- 0.0101
res.policy$R <- res.policy$R*100
rm(list = ls()[grep("^cm", ls())])

#Plot the time-resistance dynamics
ggplot(data = res.policy) + 
  theme_bw() +
  geom_line(aes(x=TIME, y=R, lty=Policy, color=Scenario), size = 1, position = position_dodge(width = 2.5)) + 
  guides(colour=FALSE) + 
  coord_cartesian(ylim=c(0,4)) + 
  scale_color_manual(values = c("red3","springgreen3","royalblue3")) + 
  labs(title="", x ="Year", y = "% Resistant") + 
  scale_x_continuous(breaks=c(10, 15, 20, 25, 30, 35, 40)) + 
  facet_grid( .~ Scenario, scales="free_y") + 
  theme(strip.text.x = element_text(face="bold", size = 12, color="black")) +
  theme(axis.title.x = element_text(size = 11)) + 
  theme(axis.title.y = element_text(size = 11)) + 
  theme(axis.text.x = element_text(face="bold", size=10), axis.text.y = element_text(face="bold", size=10)) + 
  theme(legend.justification=c(0.5,0), legend.position ="right", legend.text = element_text(size = 11), legend.title = element_text(size=12)) + 
  guides(lty=guide_legend(title="Treatment policy")) +
  theme(legend.key.height=unit(1.5,"line")) + 
  theme(legend.key.width=unit(2,"line")) 

#==========IPTp COVERAGE, FITNESS COST AND SPREAD OF RESISTANCE (FIGURE 4)==========

covcostiptp <- read.table(here("data", "cost.high.txt"), header = TRUE)
covcostiptp1 <- covcostiptp[c(1,2)]
covcostiptp2 <- covcostiptp[c(1,3)]
covcostiptp3 <- covcostiptp[c(1,4)]
covcostiptp4 <- covcostiptp[c(1,5)]
covcostiptp5 <- covcostiptp[c(1,6)]
covcostiptp6 <- covcostiptp[c(1,7)]
covcostiptp7 <- covcostiptp[c(1,8)]
setnames(covcostiptp1, old=c("R3.1"), new=c("R"))
setnames(covcostiptp2, old=c("R3.2"), new=c("R"))
setnames(covcostiptp3, old=c("R3.3"), new=c("R"))
setnames(covcostiptp4, old=c("R3.4"), new=c("R"))
setnames(covcostiptp5, old=c("R3.5"), new=c("R"))
setnames(covcostiptp6, old=c("R3.6"), new=c("R"))
setnames(covcostiptp7, old=c("R3.7"), new=c("R"))
covcostiptp1$Cost <- "  None"
covcostiptp2$Cost <- " 4%"
covcostiptp3$Cost <- " 8%"
covcostiptp4$Cost <- "12%"
covcostiptp5$Cost <- "16%"
covcostiptp6$Cost <- "20%"
covcostiptp7$Cost <- "24%"
covA <- bind_rows(covcostiptp1, covcostiptp2, covcostiptp3, covcostiptp4, covcostiptp5, covcostiptp6, covcostiptp7)
covA$Scenario <- "A"

covcostiptp <- read.table(here("data", "cost.high.txt"), header = TRUE)
covcostiptp8 <- covcostiptp[c(1,9)]
covcostiptp9 <- covcostiptp[c(1,10)]
covcostiptp10 <- covcostiptp[c(1,11)]
covcostiptp11 <- covcostiptp[c(1,12)]
covcostiptp12 <- covcostiptp[c(1,13)]
covcostiptp13 <- covcostiptp[c(1,14)]
covcostiptp14 <- covcostiptp[c(1,15)]
setnames(covcostiptp8, old=c("R3.8"), new=c("R"))
setnames(covcostiptp9, old=c("R3.9"), new=c("R"))
setnames(covcostiptp10, old=c("R3.10"), new=c("R"))
setnames(covcostiptp11, old=c("R3.11"), new=c("R"))
setnames(covcostiptp12, old=c("R3.12"), new=c("R"))
setnames(covcostiptp13, old=c("R3.13"), new=c("R"))
setnames(covcostiptp14, old=c("R3.14"), new=c("R"))
covcostiptp8$Cost <- "  None"
covcostiptp9$Cost <- " 4%"
covcostiptp10$Cost <- " 8%"
covcostiptp11$Cost <- "12%"
covcostiptp12$Cost <- "16%"
covcostiptp13$Cost <- "20%"
covcostiptp14$Cost <- "24%"
covB <- bind_rows(covcostiptp8, covcostiptp9, covcostiptp10, covcostiptp11, covcostiptp12, covcostiptp13, covcostiptp14)
covB$Scenario <- "B"

res.fitness <- bind_rows(covA, covB)
res.fitness$TIME <- res.fitness$TIME/365.25
res.fitness$R[res.fitness$TIME <12] <- 0.0101
res.fitness$R <- res.fitness$R*100
res.fitness <- subset(res.fitness, TIME>=10 & TIME<=40)
rm(list = ls()[grep("^cov", ls())])

ggplot(res.fitness, aes(x=TIME, y=R, fill=Cost)) + 
  #geom_bar(stat = 'identity') + 
  geom_area(stat = 'identity', position = position_dodge()) + 
  guides(colour=FALSE) + 
  coord_cartesian(ylim=c(0,4)) +
  scale_color_manual(values = c("red3", "red3", "red3", "red3")) + 
  theme_bw() + 
  scale_x_continuous(breaks=c(10, 15, 20, 25, 30, 35, 40)) + 
  labs(title="", x ="Year", y = "% of double mutant") + 
  facet_grid(. ~ Scenario, scales="free_y") + 
  theme(strip.text.x = element_text(face="bold", size = 12, color="black")) +
  theme(axis.title.x = element_text(size = 11)) + 
  theme(axis.title.y = element_text(size = 11)) + 
  theme(axis.text.x = element_text(face="bold", size=10), axis.text.y = element_text(face="bold", size=10)) + 
  theme(legend.justification=c(0.5,0), legend.position ="right", legend.text = element_text(size = 11), legend.title = element_text(size=12)) + 
  guides(fill=guide_legend(title="Fitness cost")) +
  theme(legend.key.height=unit(1,"line")) + 
  theme(legend.key.width=unit(1,"line")) 

#==========BIRTH RATE AND SPREAD OF RESISTANCE (FIGURE 5)==========

preg <- read.table(here("data", "preg.high.txt"), header = TRUE)
preg1 <- preg[c(1,2)]
preg2 <- preg[c(1,3)]
setnames(preg1, old=c("R3.final..2"), new=c("R"))
preg1$Policy <- "CM & IPTp"
setnames(preg2, old=c("R3.final..3"), new=c("R"))
preg2$Policy <- "CM only"
res.fertility <- bind_rows(preg1, preg2)
res.fertility$R <- res.fertility$R*100
res.fertility$sigmaf <- res.fertility$sigmaf*365.25*1000
rm(list = ls()[grep("^preg", ls())])

ggplot(res.fertility) + 
  geom_line(aes(x=sigmaf, y=R, lty=Policy), color="red3", size=1) + 
  theme_bw() + 
  coord_cartesian(ylim=c(0,4)) + 
  scale_x_continuous(breaks=c(20, 25, 30, 35, 40, 45, 50)) + 
  labs(title="", x ="Number of pregnancies per 1000 mid-year total population", y = "% of double mutant") + 
  theme(strip.text.x = element_text(size = 11, color="black")) +
  theme(axis.title.x = element_text(size = 11)) + 
  theme(axis.title.y = element_text(size = 11)) + 
  theme(axis.text.x = element_text(face="bold", size=10), axis.text.y = element_text(face="bold", size=10)) + 
  theme(legend.justification=c(0.5,0), legend.position ="right", legend.text = element_text(size = 11), legend.title = element_text(size = 12)) + 
  guides(lty=guide_legend(title="Treatment policy")) +
  theme(legend.key.height=unit(1.5,"line")) + 
  theme(legend.key.width=unit(2,"line")) 

#==========WORST CASE TREATMENT FAILURE (FIGURE 6)==========

#load pregnancy dataset
preg.ws <- read.table(here("data", "preg.high.ws.txt"), header = TRUE)
preg.ws1 <- preg.ws[c(1,2)]
preg.ws2 <- preg.ws[c(1,3)]
setnames(preg.ws1, old=c("R3.final..3"), new=c("R"))
setnames(preg.ws2, old=c("R3.final..4"), new=c("R"))
preg.ws1$Policy <- "CM & IPTp"
preg.ws2$Policy <- "CM only"
res.fertility.ws <- bind_rows(preg.ws1, preg.ws2)
res.fertility.ws$sigmaf <- round(res.fertility.ws$sigmaf*365.25*1000)
res.fertility.ws$R <- res.fertility.ws$R*100
res.fertility.ws$Scenario <- "B"
rm(list = ls()[grep("^preg", ls())])

#load IPTp v case management dataset
cmiptp.high <- read.table(here("data", "cmiptp.high.ws.txt"), header = TRUE)
cmiptp.high1 <- cmiptp.high[c(1,2)]
cmiptp.high2 <- cmiptp.high[c(1,3)]
cmiptp.high3 <- cmiptp.high[c(1,4)]
setnames(cmiptp.high1, old=c("R3.1"), new=c("R"))
setnames(cmiptp.high2, old=c("R3.2"), new=c("R"))
setnames(cmiptp.high3, old=c("R3.3"), new=c("R"))
cmiptp.high1$Policy <- "CM & IPTp"
cmiptp.high2$Policy <- "CM only"
cmiptp.high3$Policy <- "IPTp only"
res.policy.ws <- bind_rows(cmiptp.high1, cmiptp.high2, cmiptp.high3)
res.policy.ws$Scenario <- "A"
res.policy.ws$TIME <- res.policy.ws$TIME/365.25
res.policy.ws <- subset(res.policy.ws, TIME>=10 & TIME<=40)
res.policy.ws$R <- res.policy.ws$R*100
rm(list = ls()[grep("^cm", ls())])

covcostiptp <- read.table(here("data", "cost.high.ws.txt"), header = TRUE)
covcostiptp8 <- covcostiptp[c(1,9)]
covcostiptp9 <- covcostiptp[c(1,10)]
covcostiptp10 <- covcostiptp[c(1,11)]
covcostiptp11 <- covcostiptp[c(1,12)]
covcostiptp12 <- covcostiptp[c(1,13)]
covcostiptp13 <- covcostiptp[c(1,14)]
covcostiptp14 <- covcostiptp[c(1,15)]
setnames(covcostiptp8, old=c("R3.final..12"), new=c("R"))
setnames(covcostiptp9, old=c("R3.final..13"), new=c("R"))
setnames(covcostiptp10, old=c("R3.final..14"), new=c("R"))
setnames(covcostiptp11, old=c("R3.final..15"), new=c("R"))
setnames(covcostiptp12, old=c("R3.final..16"), new=c("R"))
setnames(covcostiptp13, old=c("R3.final..17"), new=c("R"))
setnames(covcostiptp14, old=c("R3.final..18"), new=c("R"))
covcostiptp8$Cost <- "  None"
covcostiptp9$Cost <- " 8%"
covcostiptp10$Cost <- "16%"
covcostiptp11$Cost <- "24%"
covcostiptp12$Cost <- "32%"
covcostiptp13$Cost <- "40%"
covcostiptp14$Cost <- "48%"
covB <- bind_rows(covcostiptp8, covcostiptp9, covcostiptp10, covcostiptp11, covcostiptp12, covcostiptp13, covcostiptp14)
covB$Scenario <- "D"

#load fitness cost of resistance dataset
covcostiptp <- read.table(here("data", "cost.high.ws.txt"), header = TRUE)
covcostiptp1 <- covcostiptp[c(1,2)]
covcostiptp2 <- covcostiptp[c(1,3)]
covcostiptp3 <- covcostiptp[c(1,4)]
covcostiptp4 <- covcostiptp[c(1,5)]
covcostiptp5 <- covcostiptp[c(1,6)]
covcostiptp6 <- covcostiptp[c(1,7)]
covcostiptp7 <- covcostiptp[c(1,8)]
setnames(covcostiptp1, old=c("R3.final..5"), new=c("R"))
setnames(covcostiptp2, old=c("R3.final..6"), new=c("R"))
setnames(covcostiptp3, old=c("R3.final..7"), new=c("R"))
setnames(covcostiptp4, old=c("R3.final..8"), new=c("R"))
setnames(covcostiptp5, old=c("R3.final..9"), new=c("R"))
setnames(covcostiptp6, old=c("R3.final..10"), new=c("R"))
setnames(covcostiptp7, old=c("R3.final..11"), new=c("R"))
covcostiptp1$Cost <- "  None"
covcostiptp2$Cost <- " 8%"
covcostiptp3$Cost <- "16%"
covcostiptp4$Cost <- "24%"
covcostiptp5$Cost <- "32%"
covcostiptp6$Cost <- "40%"
covcostiptp7$Cost <- "48%"
covA <- bind_rows(covcostiptp1, covcostiptp2, covcostiptp3, covcostiptp4, covcostiptp5, covcostiptp6, covcostiptp7)
covA$Scenario <- "C"

res.fitness.ws <- bind_rows(covA, covB)
res.fitness.ws$beta <- res.fitness.ws$beta*100
res.fitness.ws$R <- res.fitness.ws$R*100
rm(list = ls()[grep("^cov", ls())])

#ggplot
WS1 <- ggplot(data = res.policy.ws) + 
  theme_bw() +
  geom_line(aes(x=TIME, y=R, lty=Policy), color = "red3", size = 1) + 
  labs(title="", x ="Years", y = "% double mutant") + 
  scale_x_continuous(breaks=c(10, 15, 20, 25, 30, 35, 40)) + 
  coord_cartesian(ylim=c(0,100)) + 
  facet_grid( .~ Scenario, scales="free_y") + 
  theme(strip.text.x = element_text(face="bold", size = 12, color="black")) +
  theme(axis.title.x = element_text(size = 11)) + 
  theme(axis.title.y = element_text(size = 11)) + 
  theme(axis.text.x = element_text(face="bold", size=10), axis.text.y = element_text(face="bold", size=10)) + 
  theme(legend.justification=c(0.5,0), legend.position ="none", legend.text = element_text(size = 11), legend.title = element_text(size=12)) + 
  guides(lty=guide_legend(title="Treatment policy")) +
  theme(legend.key.height=unit(1,"line")) + 
  theme(legend.key.width=unit(2,"line")) 


WS2 <- ggplot(res.fertility.ws) + 
  geom_line(aes(x=sigmaf, y=R, lty=Policy), color="red3", size=1) + 
  theme_bw() + 
  coord_cartesian(ylim=c(0,100)) + 
  scale_x_continuous(breaks=c(20, 25, 30, 35, 40, 45, 50)) + 
  labs(title="", x ="Pregnancies /1000 humans /year", y = "") + 
  facet_grid(. ~ Scenario, scales="free_y") + 
  theme(strip.text.x = element_text(face="bold", size = 12, color="black")) +
  theme(axis.title.x = element_text(size = 11)) + 
  theme(axis.title.y = element_text(size = 11)) + 
  theme(axis.text.x = element_text(face="bold", size=10), axis.text.y = element_text(face="bold", size=10)) + 
  theme(legend.justification=c(0.5,0), legend.position ="right", legend.text = element_text(size = 11), legend.title = element_text(size = 12)) + 
  guides(lty=guide_legend(title="Treatment \n policy")) +
  theme(legend.key.height=unit(1,"line")) + 
  theme(legend.key.width=unit(1,"line"))  

WS3 <- ggplot(subset(res.fitness.ws, Scenario=="C"), aes(x=beta, y=R, fill=Cost)) + 
  geom_area(stat = 'identity', position = position_dodge()) + 
  coord_cartesian(ylim=c(0,100)) +
  theme_bw() + 
  labs(title="", x ="% IPTp coverage", y = "% of double mutant") + 
  facet_grid(. ~ Scenario, scales="free_y") + 
  theme(strip.text.x = element_text(face="bold", size = 12, color="black")) +
  theme(axis.title.x = element_text(size = 11)) + 
  theme(axis.title.y = element_text(size = 11)) + 
  theme(axis.text.x = element_text(face="bold", size=10), axis.text.y = element_text(face="bold", size=10)) + 
  theme(legend.justification=c(0.5,0), legend.position ="none", legend.text = element_text(size = 11), legend.title = element_text(size=12)) + 
  guides(fill=guide_legend(title="Fitness cost")) +
  theme(legend.key.height=unit(1.5,"line")) + 
  theme(legend.key.width=unit(1,"line")) 

WS4 <- ggplot(subset(res.fitness.ws, Scenario=="D"), aes(x=beta, y=R, fill=Cost)) + 
  geom_area(stat = 'identity', position = position_dodge()) + 
  coord_cartesian(ylim=c(0,100)) +
  theme_bw() + 
  labs(title="", x ="% IPTp coverage", y = "") + 
  facet_grid(. ~ Scenario, scales="free_y") + 
  theme(strip.text.x = element_text(face="bold", size = 12, color="black")) +
  theme(axis.title.x = element_text(size = 11)) + 
  theme(axis.title.y = element_text(size = 11)) + 
  theme(axis.text.x = element_text(face="bold", size=10), axis.text.y = element_text(face="bold", size=10)) + 
  theme(legend.justification=c(0.5,0), legend.position ="right", legend.text = element_text(size = 11), legend.title = element_text(size=12)) + 
  guides(fill=guide_legend(title="Fitness cost")) +
  theme(legend.key.height=unit(0.8,"line")) + 
  theme(legend.key.width=unit(1,"line")) 

grid.arrange(grobs=list(WS1, WS2, WS3, WS4), ncol=2, nrow=2)


#=========SEXUAL RECOMBINATION AND RESISTANCE DYNAMICS==========

#calculate mean multiplicity of infection and prob of (clones) k>1 given PCR prevalence
pcr_prev = 0.832
p_det = c(0.29275440, 0.56904048)
p_pop = c(0.03, 0.97)
slide_prev= weighted.mean(p_det*pcr_prev, p_pop)
m.moi = round(exp(1.082 + 0.185*logit(slide_prev)))#3
pr_multiclonal = 1-(dpois(0,m.moi)+dpois(1,m.moi))#0.801

#loading recombination dataset for observed treatment failure
res.recomb <- read.table(here("data", "recombination.os.txt"), header = TRUE)

res.recombA <- res.recomb[c(1,2)]; res.recombA$Res <- "B"
res.recombP <- res.recomb[c(1,3)]; res.recombP$Res <- "C"
res.recombAP <- res.recomb[c(1,4)]; res.recombAP$Res <- "A"
setnames(res.recombA, "R1.NR","R");setnames(res.recombP, "R2.NR","R");setnames(res.recombAP,"R3.NR","R")
res.recomb1 <- bind_rows(res.recombAP,res.recombA,res.recombP)
res.recomb1$Rec <- "No"

res.recombA <- res.recomb[c(1,5)]; res.recombA$Res <- "B"
res.recombP <- res.recomb[c(1,6)]; res.recombP$Res <- "C"
res.recombAP <- res.recomb[c(1,7)]; res.recombAP$Res <- "A"
setnames(res.recombA, "R1.R","R");setnames(res.recombP, "R2.R","R");setnames(res.recombAP,"R3.R","R")
res.recomb2 <- bind_rows(res.recombAP,res.recombA,res.recombP)
res.recomb2$Rec <- "Yes"

res.recomb <- bind_rows(res.recomb1,res.recomb2)
res.recomb$TIME <- res.recomb$TIME/365.25
res.recomb <- subset(res.recomb, TIME>=10 & TIME<=40)
res.recomb$R <- res.recomb$R*100
remove(res.recomb1,res.recomb2,res.recombA,res.recombP,res.recombAP)

#ggplot for observed treatment failure
rec.A <- ggplot(data=res.recomb) + 
  theme_bw() + 
  coord_cartesian(ylim=c(0,4)) + 
  geom_line(aes(x=TIME, y=R, color=Res, lty=Rec), size = 1) + 
  scale_color_manual(values = c("red3", "springgreen3","royalblue3")) + 
  facet_grid(.~ Res, scales="free_y") + 
  labs(title="", x="Year", y="% Resistance") + 
  theme(strip.text.x = element_text(face="bold", size = 12, color="black")) +
  theme(axis.title.x = element_text(size = 11)) + 
  theme(axis.title.y = element_text(size = 11)) +
  theme(axis.text.x = element_text(face="bold", size=10), axis.text.y = element_text(face="bold", size=10)) + 
  theme(legend.justification=c(0.5,0), legend.position = "right", legend.text = element_text(size = 11), legend.title = element_text(face="bold", size=11)) + 
  guides(color=FALSE) +
  guides(lty=guide_legend(title="Recombination under \n observed scenario")) +
  theme(legend.key.height=unit(1.2,"line")) + 
  theme(legend.key.width=unit(2,"line")) 

#loading recombination dataset for worst-case treatment failure
res.recomb.ws <- read.table(here("data", "recombination.ws.txt"), header = TRUE)

res.recombA <- res.recomb.ws[c(1,2)]; res.recombA$Res <- "E"
res.recombP <- res.recomb.ws[c(1,3)]; res.recombP$Res <- "F"
res.recombAP <- res.recomb.ws[c(1,4)]; res.recombAP$Res <- "D"
setnames(res.recombA, "R1.NR","R");setnames(res.recombP, "R2.NR","R");setnames(res.recombAP,"R3.NR","R")
res.recomb1 <- bind_rows(res.recombAP,res.recombA,res.recombP)
res.recomb1$Rec <- "No"

res.recombA <- res.recomb.ws[c(1,5)]; res.recombA$Res <- "E"
res.recombP <- res.recomb.ws[c(1,6)]; res.recombP$Res <- "F"
res.recombAP <- res.recomb.ws[c(1,7)]; res.recombAP$Res <- "D"
setnames(res.recombA, "R1.R","R");setnames(res.recombP, "R2.R","R");setnames(res.recombAP,"R3.R","R")
res.recomb2 <- bind_rows(res.recombAP,res.recombA,res.recombP)
res.recomb2$Rec <- "Yes"

res.recomb.ws <- bind_rows(res.recomb1,res.recomb2)
res.recomb.ws$TIME <- res.recomb.ws$TIME/365.25
res.recomb.ws <- subset(res.recomb.ws, TIME>=10 & TIME<=40)
res.recomb.ws$R <- res.recomb.ws$R*100
remove(res.recomb1,res.recomb2,res.recombA,res.recombP,res.recombAP)

#ggplot for worst-case treatment failure
rec.B <- ggplot(data=res.recomb.ws) + 
  theme_bw() + 
  coord_cartesian(ylim=c(0,100)) + 
  geom_line(aes(x=TIME, y=R, color=Res, lty=Rec), size = 1) + 
  scale_color_manual(values = c("red3", "springgreen3","royalblue3")) + 
  facet_grid(.~ Res, scales="free_y") + 
  labs(title="", x="Year", y="% Resistance") + 
  theme(strip.text.x = element_text(face="bold", size = 12, color="black")) +
  theme(axis.title.x = element_text(size = 11)) + 
  theme(axis.title.y = element_text(size = 11)) +
  theme(axis.text.x = element_text(face="bold", size=10), axis.text.y = element_text(face="bold", size=10)) + 
  theme(legend.justification=c(0.5,0), legend.position = "right", legend.text = element_text(size = 11), legend.title = element_text(face="bold", size=11)) + 
  guides(color=FALSE) +
  guides(lty=guide_legend(title="Recombination under \n worst-case scenario")) +
  theme(legend.key.height=unit(1.2,"line")) + 
  theme(legend.key.width=unit(2,"line")) 

grid.arrange(grobs=list(rec.A,rec.B), ncol=1, nrow=2)

#=========SENSITIVITY ANALYSIS OF MODEL PARAMETERS==========

#load treatment failure dataset (S1Fig)
tf.s1 <- read.table(here("data", "treatmentF2.sa.txt"), header = TRUE)
setnames(tf.s1, old=c("R2.final..2"), new=c("R"))
setnames(tf.s1, old=c("Tf2"), new=c("Tf"))
tf.s1$Policy <- "A"
tf.s2 <- read.table(here("data", "treatmentF4.sa.txt"), header = TRUE)
setnames(tf.s2, old=c("R1.final..2"), new=c("R"))
setnames(tf.s2, old=c("Tf1"), new=c("Tf"))
tf.s2$Policy <- "B"

res.treatment.sa<- bind_rows(tf.s1, tf.s2)
res.treatment.sa$Tf <- res.treatment.sa$Tf*100
res.treatment.sa$R <- res.treatment.sa$R*100
rm(list = ls()[grep("^tf", ls())])

#ggplot
ggplot(res.treatment.sa) + 
  geom_line(aes(x=Tf, y=R, lty=Policy), size = 1) + 
  theme_bw() + 
  scale_x_continuous(breaks=c(0, 20, 40, 60, 80, 100)) +
  facet_grid(. ~ Policy, scales="free_y") + 
  coord_cartesian(ylim = c(0,100)) + 
  theme(axis.text.x = element_text(face="bold", size=9), axis.text.y = element_text(face="bold", size=9)) + 
  xlab(bquote('% Treatment failure ('*lambda[jk]~')')) + 
  ylab(bquote('% Resistant')) +
  theme(strip.text.x = element_text(face="bold", size = 12, color="black")) +
  theme(axis.title.x = element_text(size = 11)) + 
  theme(axis.title.y = element_text(size = 11)) + 
  theme(axis.text.x = element_text(face="bold", size=10), axis.text.y = element_text(face="bold", size=10)) + 
  theme(legend.justification=c(0.5,0), legend.box = "none", legend.position ="none", legend.text = element_text(size = 10), legend.title = element_text(size=11))

#load biting preference dataset (S2Fig)
pb.sa <- read.table(here("data", "prefR.sa1.txt"), header = TRUE)
pb.sa1 <- pb.sa[c(1,2)]
pb.sa2 <- pb.sa[c(1,3)]
pb.sa3 <- pb.sa[c(1,4)]
pb.sa4 <- pb.sa[c(1,5)]
setnames(pb.sa1, old=c("R3.final..17"), new=c("R"))
setnames(pb.sa2, old=c("R3.final..18"), new=c("R"))
setnames(pb.sa3, old=c("R3.final..19"), new=c("R"))
setnames(pb.sa4, old=c("R3.final..20"), new=c("R"))
pb.sa1$Prevalence <- "High"
pb.sa1$Scenario <- "CM & IPTp"
pb.sa2$Prevalence <- "High"
pb.sa2$Scenario <- "CM only"
pb.sa3$Prevalence <- "Moderate"
pb.sa3$Scenario <- "CM & IPTp"
pb.sa4$Prevalence <- "Moderate"
pb.sa4$Scenario <- "CM only"
pb.A <- bind_rows(pb.sa1, pb.sa2, pb.sa3, pb.sa4)
pb.A$Section <- "A"

pb.sa <- read.table(here("data", "prefR.sa2.txt"), header = TRUE)
pb.sa1 <- pb.sa[c(1,2)]
pb.sa2 <- pb.sa[c(1,3)]
pb.sa3 <- pb.sa[c(1,4)]
pb.sa4 <- pb.sa[c(1,5)]
setnames(pb.sa1, old=c("R3.final..11"), new=c("R"))
setnames(pb.sa2, old=c("R3.final..12"), new=c("R"))
setnames(pb.sa3, old=c("R3.final..13"), new=c("R"))
setnames(pb.sa4, old=c("R3.final..14"), new=c("R"))
pb.sa1$Prevalence <- "High"
pb.sa1$Scenario <- "CM & IPTp"
pb.sa2$Prevalence <- "High"
pb.sa2$Scenario <- "CM only"
pb.sa3$Prevalence <- "Moderate"
pb.sa3$Scenario <- "CM & IPTp"
pb.sa4$Prevalence <- "Moderate"
pb.sa4$Scenario <- "CM only"
pb.B <- bind_rows(pb.sa1, pb.sa2, pb.sa3, pb.sa4)
pb.B$Section <- "B"

res.preference.sa <- bind_rows(pb.A, pb.B)
res.preference.sa$R <- res.preference.sa$R*100
rm(list = ls()[grep("^pb", ls())])

#ggplot
SA1 <- ggplot(subset(res.preference.sa, Section=="A")) + 
  geom_line(aes(x=pref_ratio, y=R, color=Scenario, lty=Prevalence), size = 1) + 
  theme_bw() + 
  coord_cartesian(ylim=c(0,4)) + 
  theme(axis.text.x = element_text(face="bold", size=9), axis.text.y = element_text(face="bold", size=9)) + 
  scale_x_continuous(breaks=c(0,2,4,6,8,10,12)) + 
  facet_grid(. ~ Section, scales="free_y") + 
  xlab(bquote('Biting exposure ratio ('*alpha[2]~'/' *alpha[1]~')')) + 
  ylab(bquote('% Resistant')) +
  theme(strip.text.x = element_text(face="bold", size = 12, color="black")) +
  theme(axis.title.x = element_text(size = 11)) + 
  theme(axis.title.y = element_text(size = 11)) + 
  theme(axis.text.x = element_text(face="bold", size=10), axis.text.y = element_text(face="bold", size=10)) + 
  theme(legend.justification=c(0.5,0), legend.box = "vertical", legend.position ="right", legend.text = element_text(size = 10), legend.title = element_text(size=11)) + 
  guides(lty=guide_legend(title="Infection prevalence")) +
  guides(color=guide_legend(title="Treatment policy")) +
  theme(legend.key.height=unit(-2,"line")) + 
  theme(legend.key.width=unit(2,"line")) 

SA2 <- ggplot(subset(res.preference.sa, Section=="B")) + 
  geom_line(aes(x=pref_ratio, y=R, color=Scenario, lty=Prevalence), size = 1) + 
  theme_bw() + 
  coord_cartesian(ylim=c(0,100)) +
  theme(axis.text.x = element_text(face="bold", size=9), axis.text.y = element_text(face="bold", size=9)) + 
  scale_x_continuous(breaks=c(0,2,4,6,8,10,12)) + 
  facet_grid(. ~ Section, scales="free_y") + 
  xlab(bquote('Biting exposure ratio ('*alpha[2]~'/' *alpha[1]~')')) + 
  ylab(bquote('% Resistant')) +
  theme(strip.text.x = element_text(face="bold", size = 12, color="black")) +
  theme(axis.title.x = element_text(size = 11)) + 
  theme(axis.title.y = element_text(size = 11)) + 
  theme(axis.text.x = element_text(face="bold", size=10), axis.text.y = element_text(face="bold", size=10)) + 
  theme(legend.justification=c(0.5,0), legend.box = "vertical", legend.position ="right", legend.text = element_text(size = 10), legend.title = element_text(size=11)) + 
  guides(lty=guide_legend(title="Infection prevalence")) +
  guides(color=guide_legend(title="Treatment policy")) +
  theme(legend.key.height=unit(-2,"line")) + 
  theme(legend.key.width=unit(2,"line")) 

grid.arrange(grobs=list(SA1, SA2), ncol=2, nrow=1)
