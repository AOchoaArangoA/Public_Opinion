#En este script se realiza las graficas del estudio de eventos
install.packages("arm")
install.packages("iplots")
install.packages("coefplot")
install.packages("car")
install.packages("writexl")

library(writexl)
library(car)
library(dplyr)
library(arm)
library(iplots)
library(coefplot)

table(t$t0)

##### VARIABLES ECONOMIA ######
#### DATA ####
library(readxl)
#dat <- read_excel("Downloads/indice_eco.xlsx")
dat <- read_excel("Economia.xlsx")
dat <- na.omit(dat)
dat <- dat %>% filter(dat$T_C == 1)
write_xlsx(t,"Downloads/coefplotdat.xlsx")

#### VARIABLES POR PERIODO EN DATA ####
t.4 <- c(ifelse(dat$Fecha_num==16,1,0))
t.3 <- c(ifelse(dat$Fecha_num==17,1,0))
t.2 <- c(ifelse(dat$Fecha_num==18,1,0))
t.1<- c(ifelse(dat$Fecha_num==19,1,0))
t0 <- c(ifelse(dat$Fecha_num==20,0,0))
t1 <- c(ifelse(dat$Fecha_num==21,1,0))
t2 <- c(ifelse(dat$Fecha_num==22,1,0))
t3 <- c(ifelse(dat$Fecha_num==23,1,0))
t4 <- c(ifelse(dat$Fecha_num==24,1,0))
t5 <- c(ifelse(dat$Fecha_num==25,1,0))
t6 <- c(ifelse(dat$Fecha_num==26,1,0))
ind <- (dat$P6STGBSC + dat$P8STIC + dat$P13STGBS.B)/3
t<- data_frame(
  t.4 ,t.3,t.2,t.1,t0,t1,t2 ,t3 ,t4,t5,t6
)
colnames(t) <- c('IA', 'IIB', 'IIIC','IIIID', 't0', 't1', 't2', 't3',
                 't4','t5', 't6')
dat <- cbind(P6STGBSC,P8STIC,P13STGBS.B,t,ind)
attach(dat)


#### reg1 ####
reg1 <- lm(P6STGBSC ~
             IA+
             IIB +
             IIIC + 
             IIIID +
             t0 +
             t1 +
             t2 +
             t3 +
             t4 +
             t5 +
             t6 )


#### reg2 ####
reg2 <- lm(P8STIC ~ 
             IA+
             IIB +
             IIIC + 
             IIIID +
             t0 +
             t1 +
             t2 +
             t3 +
             t4 +
             t5 +
             t6 )

#### reg3####
reg3 <- lm(P13STGBS.B ~ 
             IA+
             IIB +
             IIIC + 
             IIIID +
             t0 +
             t1 +
             t2 +
             t3 +
             t4 +
             t5 +
             t6 )
#### reg4####
reg4 <- lm(ind ~ 
             IA+
             IIB +
             IIIC + 
             IIIID +
             t0 +
             t1 +
             t2 +
             t3 +
             t4 +
             t5 +
             t6)
##### graficos ####

coefplot(reg1,title="Situacion Economica Actual",interactive=F,intercept = F, color = "blue",horizontal=TRUE)
coefplot(reg2,title="Situacion Futura de la Economia",interactive=FALSE,intercept = FALSE, color = "red",horizontal=TRUE)
coefplot(reg3,title="Satisfaccion con el Funcionamiento de la Economia",interactive=FALSE,intercept = FALSE, color = "purple",horizontal=TRUE)
coefplot(reg4,title="Indicador Agregado de Economia",interactive=FALSE,intercept = FALSE, color = "brown",horizontal=TRUE)
##### VARIABLES GOV #####
#### DATA ####
dat <- read_excel("Downloads/Gobierno.xlsx")
dat <- na.omit(dat)
dat <- dat %>% filter(dat$T_C == 1)




m1 = lm(mpg ~ wt + cyl + carb, data=mtcars)




plot_order <- c('lead4', 'lead3', 'lead2','lead1', 't', 't1', 't2', 't3',
                't4','t5', 't6')
label = c( -4, -3, -2, -1, 1,2,3,4,5,6)
#confint(reg1,level= 0.95)
mean = c(coef(reg1)[plot_order], 0)
#cero = c(0,0,"t0")

#coefs = rbind(coefs, cero)
coefs = as.data.frame(summary(reg1)$coefficients[-1,1:2])
names(coefs)[2] = "se" 
coefs$vars = rownames(coefs)
rownames(coefs) <- NULL  
cero = c(as.numeric(0),as.numeric(0),"t0")
coefs = rbind(coefs, cero)

coefs$vars2 <- factor(coefs$vars, levels = coefs$vars)


ggplot(coefs, aes(x = vars, Estimate)) + 
  geom_hline(yintercept=0, lty=2, lwd=1, colour="grey50") +
  geom_errorbar(aes(ymin=Estimate - 1.96*se, ymax=Estimate + 1.96*se), 
                lwd=1, colour="maroon", width=0) +
  geom_errorbar(aes(ymin=Estimate - se, ymax=Estimate + se), 
                lwd=2.5, colour="darkblue", width=0) +
  geom_point(size=4, pch=21, fill="yellow") + 
  ggtitle("Estudio de Eventos Situaci?n econ?mica actual del pa?s", ) +
  xlab("Periodo del Evento") + ylab("Coeficiente de la Estimacion")+
  theme_bw()






coefs = as.data.frame(summary(reg2)$coefficients[-1,1:2])
names(coefs)[2] = "se" 
coefs$vars =rownames(coefs)


ggplot(coefs, aes(x = vars, Estimate)) + 
  geom_hline(yintercept=0, lty=2, lwd=1, colour="grey50") +
  geom_errorbar(aes(ymin=Estimate - 1.96*se, ymax=Estimate + 1.96*se), 
                lwd=1, colour="maroon", width=0) +
  geom_errorbar(aes(ymin=Estimate - se, ymax=Estimate + se), 
                lwd=2.5, colour="darkblue", width=0) +
  geom_point(size=4, pch=21, fill="yellow") + 
  ggtitle("Estudio de Eventos Situaci?n econ?mica futura del pa?s", ) +
  xlab("Periodo del Evento") + ylab("Coeficiente de la Estimacion")+
  theme_bw()



coefs = as.data.frame(summary(reg3)$coefficients[-1,1:2])
names(coefs)[2] = "se" 
coefs$vars =rownames(coefs)


ggplot(coefs, aes(x = vars, Estimate)) + 
  geom_hline(yintercept=0, lty=2, lwd=1, colour="grey50") +
  geom_errorbar(aes(ymin=Estimate - 1.96*se, ymax=Estimate + 1.96*se), 
                lwd=1, colour="maroon", width=0) +
  geom_errorbar(aes(ymin=Estimate - se, ymax=Estimate + se), 
                lwd=2.5, colour="darkblue", width=0) +
  geom_point(size=4, pch=21, fill="yellow") + 
  ggtitle("Estudio de Eventos Satisfacci?n con el funcionamiento de la econom?a", ) +
  xlab("Periodo del Evento") + ylab("Coeficiente de la Estimacion")+
  theme_bw()



coefs = as.data.frame(summary(reg4)$coefficients[-1,1:2])
names(coefs)[2] = "se" 
coefs$vars =rownames(coefs)


ggplot(coefs, aes(x = vars, Estimate)) + 
  geom_hline(yintercept=0, lty=2, lwd=1, colour="grey50") +
  geom_errorbar(aes(ymin=Estimate - 1.96*se, ymax=Estimate + 1.96*se), 
                lwd=1, colour="maroon", width=0) +
  geom_errorbar(aes(ymin=Estimate - se, ymax=Estimate + se), 
                lwd=2.5, colour="darkblue", width=0) +
  geom_point(size=4, pch=21, fill="yellow") + 
  ggtitle("Estudio de Eventos ?ndice OP Econom?a", ) +
  xlab("Periodo del Evento") + ylab("Coeficiente de la Estimacion")+
  theme_bw()





write.csv(Your DataFrame,"Downloads/coefplotdat.csv", row.names = FALSE)


#### VARIABLES POR PERIODO EN DATA ####
t.4 <- c(ifelse(dat$Fecha_num==16,1,0))
t.3 <- c(ifelse(dat$Fecha_num==17,1,0))
t.2 <- c(ifelse(dat$Fecha_num==18,1,0))
t.1<- c(ifelse(dat$Fecha_num==19,1,0))
t0 <- c(ifelse(dat$Fecha_num==20,1,0))
t1 <- c(ifelse(dat$Fecha_num==21,1,0))
t2 <- c(ifelse(dat$Fecha_num==22,1,0))
t3 <- c(ifelse(dat$Fecha_num==23,1,0))
t4 <- c(ifelse(dat$Fecha_num==24,1,0))
t5 <- c(ifelse(dat$Fecha_num==25,1,0))
t6 <- c(ifelse(dat$Fecha_num==26,1,0))
ind <- (dat$P20STGBSC+dat$P15STGBSC.G+dat$P15STGBSC.F+dat$P15STGBSC.A)/4
t<- data_frame(
  t.4 ,t.3,t.2,t.1,t0,t1,t2 ,t3 ,t4,t5,t6
)
colnames(t) <- c('t-4', 't-3', 't-2','t-1', 't', 't1', 't2', 't3',
                 't4','t5', 't6')
dat <- cbind(P20STGBSC,P15STGBSC.G,P15STGBSC.F,P15STGBSC.A,t,ind)
attach(dat)

#### reg1 ####
regov1 <- lm(dat$P20STGBSC ~ `t-4`+
               `t-3` +
               `t-2` + 
               `t-1` +
               t0 +
               t1 +
               t2 +
               t3 +
               t4 +
               t5 +
               t6 )
#### reg2 ####
regov2 <- lm(dat$P15STGBSC.G ~ `t-4`+
               `t-3` +
               `t-2` + 
               `t-1` +
               t0 +
               t1 +
               t2 +
               t3 +
               t4 +
               t5 +
               t6 )
#### reg3####
regov3 <- lm(dat$P15STGBSC.F ~ `t-4`+
               `t-3` +
               `t-2` + 
               `t-1` +
               t0 +
               t1 +
               t2 +
               t3 +
               t4 +
               t5 +
               t6 )
#### reg4####
regov4 <- lm(dat$P15STGBSC.A ~ `t-4`+
               `t-3` +
               `t-2` + 
               `t-1` +
               t0 +
               t1 +
               t2 +
               t3 +
               t4 +
               t5 +
               t6 )
#### reg5####
regov5 <- lm(dat$ind ~ `t-4`+
               `t-3` +
               `t-2` + 
               `t-1` +
               t0 +
               t1 +
               t2 +
               t3 +
               t4 +
               t5 +
               t6 )

##### graficos ####
coefplot(regov1,title="Aprovacion del Gobierno",interactive=FALSE,intercept = FALSE, color = "blue",horizontal=TRUE)
coefplot(regov2,title="Confianza en los Partidos Pol?ticos",interactive=FALSE,intercept = FALSE, color = "red",horizontal=TRUE)
coefplot(regov3,title="Confianza en el Poder Judicial",interactive=FALSE,intercept = FALSE, color = "purple",horizontal=TRUE)
coefplot(regov4,title="Confianza en las Fuerzas Armadas",interactive=FALSE,intercept = FALSE, color = "green",horizontal=TRUE)
coefplot(regov5,title="Indicador Agregado de Govierno",interactive=FALSE,intercept = FALSE, color = "orange",horizontal=TRUE, xline=t0)




