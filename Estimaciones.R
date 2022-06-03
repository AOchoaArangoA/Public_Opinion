#En este script se realizan las estimaciones del control sintetico 
#install.packages("Synth")
#install.packages("ggplot2")
library("ggplot2")

library("Synth")
??Synth

library(readr)
library(readxl)
data <- read_excel("Base_Datos_01042022.xlsx")

data = as.data.frame(data)
data$IDENPA = as.character(data$IDENPA)
data$IDENPA_num = as.numeric(data$IDENPA_num)


#########  CAUSAL IMPACT ###############


##### PREGUNTAS  ######

preguntas = c('P15STGBSC.A',
              'P15STGBSC.B',
              'P15STGBSC.C',
              'P15STGBSC.D',
              'P15STGBSC.E',
              'P15STGBSC.F',
              'P15STGBSC.G',
              'P15STGBSC.H',
              'P16NC.F',
              'P20STGBSC',
              'P6STGBSC',
              'P8STIC',
              'P11STGBS',
              'P13STGBS.B',
              'P39ST.B',
              'P52N.C')
## SINTETIC CONTROL ####

#Buscando tener robustez en la estimacion. Intentamos realizar un control sintentico
#En muchas de las ocaciones Peru, representaba cerca del 50% de la varianza. 


predictors_gov <- c("EDAD","SEXO","S11","S10","S26")

dataprep_preguntas <- dataprep(foo = data,
                             predictors = predictors_gov,
                             predictors.op = "mean",
                             time.predictors.prior = 8:20,
                             special.predictors = list(
                               list("EDAD", c(8:19), "mean"),
                               list("SEXO", c(8:19), "mean"),
                               list("S11", c(8:19), "mean"),
                               list("S10", c(8:19), "mean"),
                               list("S26", c(8:19), "mean")),
                             dependent = preguntas[10],
                             unit.variable = c("IDENPA_num"),
                             unit.names.variable = "IDENPA",
                             time.variable = "Fecha_num",
                             treatment.identifier = 1,
                             controls.identifier = c(2,3,4),
                             time.optimize.ssr =8:24,
                             time.plot =15:23
                            
                             
)
#P20STGBSC


synth_out_pre <- synth(data.prep.obj = dataprep_preguntas)

## GRAPH ##
grap_ind_pre <- path.plot(synth_out_pre, dataprep_preguntas,
                          Legend=c("Treate P20STGBSC","Synthetic P20STGBSC"))
grap_dif_pre <- gaps.plot(synth_out_pre, dataprep_preguntas,
                          )
## TABLES ##

synth.tables.p<- synth.tab(
  dataprep.res = dataprep_preguntas,
  synth.res = synth_out_pre)

print(synth.tables.p)


## DIFF - DIFF ####
#data_s = as.data.frame(dataprep_preguntas[["Y0plot"]])

data_s = as.data.frame((dataprep_preguntas$Y0plot %*% synth_out_pre$solution.w))
data_s$Fecha = row.names(data_s)
row.names(data_s) = NULL
data_s$Id = 0
colnames(t) <- c('t-4', 't-3', 't-2','t-1', 't', 't1', 't2', 't3',
                 't4','t5', 't6')



data_t = as.data.frame(dataprep_preguntas[["Y1plot"]])
data_t$Fecha = row.names(data_t)
row.names(data_s) = NULL
data_t$Id = 1
colnames(data_t) <- c('Nuevo', 'Fecha', 'Id')

dfp = merge(data_t, data_s, all = TRUE)

dfp$time = ifelse(dfp$Fecha >=20, 1, 0)

dfp$diff = dfp$time * dfp$Id
didreg = lm( Nuevo~ dfp$time * dfp$Id, data =dfp)
summary(didreg)



dhom =(P52N_C$T_C*P52N_C$B_A*P52N_C$hombre)
dmuj =(P52N_C$T_C*P52N_C$B_A*P52N_C$mujer)
P52N_C = data.frame(P52N_C, dhom , dmuj)

table(P52N_C$Fecha_num)

didreg1 = lm(P39ST_B$P39ST.B ~ T_C*B_A + , data = P39ST_B)
summary(didreg1)


####### TEST MEDIAS ##########

#Estimacion de las medias para el balanceo de los grupos de control y tratamiento
library(readxl)
medias <- read_excel("Prueba_Medias.xlsx")
??t.test

t.test(EDAD~Id, data = medias)
wilcox.test(EDAD~Id, data = medias)

t.test(S10~Id, data = medias)
wilcox.test(S10~Id, data = medias)

t.test(S11~Id, data = medias, var.equal = T)
t.test(SEXO~Id, data = medias)
wilcox.test(SEXO~Id, data = medias)
#t.test(P31NI~Id, data = medias)

t.test(S23~Id, data = medias)
wilcox.test(S23~Id, data = medias)

t.test(S14A~Id, data = medias)
wilcox.test(S14A~Id, data = medias)

t.test(S1~Id, data = medias)
wilcox.test(S1~Id, data = medias)

t.test(S26~Id, data = medias)
wilcox.test(S26~Id, data = medias)

t.test(S25~Id, data = medias)
wilcox.test(S25~Id, data = medias)

t.test(Sup~Id, data = medias)
wilcox.test(sup~Id, data = medias)

t.test(S16~Id, data = medias)
wilcox.test(S16~Id, data = medias)

t.test(S8~Id, data = medias)
wilcox.test(S16~Id, data = medias)


####### Medias sin NAs ##########


library(readxl)
ig = read_excel("indice_gov.xlsx")
attach(ig)
t.test(EDAD~IDENPA, data = ig)
wilcox.test(EDAD~Id, data = ig)

t.test(S10~IDENPA, data = ig)
wilcox.test(S10~Id, data = medias)

t.test(S11~IDENPA, data = ig, var.equal = T)
t.test(SEXO~IDENPA, data = ig)
wilcox.test(SEXO~Id, data = medias)
#t.test(P31NI~Id, data = medias)

t.test(S23~IDENPA, data = ig)
wilcox.test(S23~Id, data = medias)

t.test(S14A~IDENPA, data = ig)
wilcox.test(S14A~Id, data = medias)

t.test(S1~IDENPA, data = ig)
wilcox.test(S1~Id, data = medias)

t.test(S26~IDENPA, data = ig)
wilcox.test(S26~Id, data = medias)

t.test(S25~IDENPA, data = ig)
wilcox.test(S25~Id, data = medias)

t.test(Sup~IDENPA, data = ig)
wilcox.test(sup~Id, data = medias)

t.test(S16~IDENPA, data = ig)
wilcox.test(S16~Id, data = medias)

t.test(S8~IDENPA, data = ig)
wilcox.test(S16~Id, data = medias)
######### Estimacion Todas las observaciones #######

#En esta parte se realizaron las estimacion del impacto pregunta por pregunta.
#Se intento buscar las preguntas que hayan presentado una perturbacion significativa
#para poder crear el índice
library(readxl)
p1 = read_excel("./Bases_Dif_n1/P52N.C.xlsx")
attach(p1)
p2 = read_excel("./Bases_Dif_n1/P39ST.B.xlsx")
attach(p2)
p3 = read_excel("./Bases_Dif_n1/P20STGBSC.xlsx")
attach(p3)
p4 = read_excel("./Bases_Dif_n1/P16NC.F.xlsx")
attach(p4)

p5 = read_excel("./Bases_Dif_n1/P15STGBSC.H.xlsx")
attach(p5)
p6 = read_excel("./Bases_Dif_n1/P15STGBSC.G.xlsx")
attach(p6)
p7 = read_excel("./Bases_Dif_n1/P15STGBSC.F.xlsx")
attach(p7)
p8 = read_excel("./Bases_Dif_n1/P15STGBSC.E.xlsx")
attach(p8)
p9 = read_excel("./Bases_Dif_n1/P15STGBSC.D.xlsx")
attach(p9)
p10 = read_excel("./Bases_Dif_n1/P15STGBSC.C.xlsx")
attach(p10)
p11 = read_excel("./Bases_Dif_n1/P15STGBSC.B.xlsx")
attach(p11)
p12 = read_excel("./Bases_Dif_n1/P15STGBSC.A.xlsx")
attach(p12)

p13 = read_excel("./Bases_Dif_n1/P13STGBS.B.xlsx")
attach(p13)

p14 = read_excel("./Bases_Dif_n1/P11STGBS.xlsx")
attach(p14)

p15 = read_excel("./Bases_Dif_n1/P8STIC.xlsx")
attach(p15)

p16 = read_excel("./Bases_Dif_n1/P6STGBSC.xlsx")
attach(p16)

regp1 = lm(p1$P52N.C ~ T_C*B_A , data = p1)
summary(regp1)

regp2 = lm(p2$P39ST.B ~ T_C*B_A , data = p2)
summary(regp2)

regp3 = lm(p3$P20STGBSC ~ T_C*B_A , data = p3)
summary(regp3)

regp4 = lm(p4$P16NC.F ~ T_C*B_A , data = p4)
summary(regp4)

regp5 = lm(p5$P15STGBSC.H ~ T_C*B_A , data = p5)
summary(regp5)

regp6 = lm(p6$P15STGBSC.G ~ T_C*B_A , data = p6)
summary(regp6)
#ggplot(p6, aes(x = p6$Fecha_lar, y = p6$P15STGBSC.G, group = p6$T_C, fill =p6$T_C ))+geom_point()+geom_smooth(se=FALSE)
#ggplot(p6, aes(x = p6$Fecha_lar, y = p6$P15STGBSC.G))+geom_point()+geom_smooth(method = lm)

regp7 = lm(p7$P15STGBSC.F ~ T_C*B_A , data = p7)
summary(regp7)

regp8 = lm(p8$P15STGBSC.E ~ T_C*B_A , data = p8)
summary(regp8)

regp9 = lm(p9$P15STGBSC.D ~ T_C*B_A , data = p9)
summary(regp9)

regp10 = lm(p10$P15STGBSC.C ~ T_C*B_A , data = p10)
summary(regp10)

regp11 = lm(p11$P15STGBSC.B ~ T_C*B_A , data = p11)
summary(regp11)

regp12 = lm(p12$P15STGBSC.A ~ T_C*B_A , data = p12)
summary(regp12)

regp13 = lm(p13$P13STGBS.B ~ T_C*B_A , data = p13)
summary(regp13)

regp14 = lm(p14$P11STGBS ~ T_C*B_A , data = p14)
summary(regp14)

regp15 = lm(p15$P8STIC ~ T_C*B_A , data = p15)
summary(regp15)

regp16 = lm(p16$P6STGBSC ~ T_C*B_A , data = p16)
summary(regp16)


####### INDICE ######
#Las variables que se van utilizar para en indice. Variables que son relevantes para la estimacion
library(readxl)
indice_gov = read_excel("indice_gov.xlsx")
regindice = lm(indice_gov$Gov ~ T_C*B_A, data = indice_gov)
summary(regindice)

indice_eco = read_excel("indice_eco.xlsx")

regeco = lm(indice_eco$Eco ~ T_C*B_A, data = indice_eco)
summary(regeco)


###### ESTIMACION CON CONTROLES #######
indice_gov_co = read_excel("indice_gov_co.xlsx")


regcon_gov_co = lm(indice_gov_co$Gov ~ T_C*B_A+ SEXO+ EDAD+S10+S23+S14A+Sup+S8+S26, data = indice_gov_co)
summary(regcon_gov_co)

indice_eco_co = read_excel("indice_eco_co.xlsx")
regcon_eco_co = lm(indice_eco_co$Eco ~ T_C*B_A +SEXO+ EDAD+S10+S23+S14A+Sup, data = indice_eco_co)
summary(regcon_eco_co)

####### ESTIMACION CON EFECTOS########

regcon_gov_co = lm(indice_gov_co$Gov ~ T_C*B_A*Sup+ EDAD+S10+S23+S14A+S8+S26 , data = indice_gov_co)
summary(regcon_gov_co)

regcon_eco_co = lm(indice_eco_co$Eco ~ T_C*B_A*Sup+ EDAD+S10+S23+S14A+SEXO, data = indice_eco_co)
summary(regcon_eco_co)

regcon_gov_co = lm(indice_gov_co$Gov ~ T_C*B_A*sec+ EDAD+S10+S23+S14A+SEXO , data = indice_gov_co)
summary(regcon_gov_co)

regcon_eco_co = lm(indice_eco_co$Eco ~ T_C*B_A*sec+ EDAD+S10+S23+S14A+SEXO, data = indice_eco_co)
summary(regcon_eco_co)

regcon_gov_co = lm(indice_gov_co$Gov ~ T_C*B_A*Estrato+ EDAD+S10+S23+S14A +SEXO, data = indice_gov_co)
summary(regcon_gov_co)

regcon_eco_co = lm(indice_eco_co$Eco ~ T_C*B_A*Estrato+ EDAD+S10+S23+S14A+SEXO, data = indice_eco_co)
summary(regcon_eco_co)

regcon_gov_co = lm(indice_gov_co$Gov ~ T_C*B_A*S25 + EDAD+S10+S23+S14A+ SEXO, data = indice_gov_co)
summary(regcon_gov_co)

regcon_eco_co = lm(indice_eco_co$Eco ~ T_C*B_A*S25+ EDAD+S10+S23+S14A+SEXO, data = indice_eco_co)
summary(regcon_eco_co)

regcon_gov_co = lm(indice_gov_co$Gov ~ T_C*B_A*SEXO + EDAD+S10+S23+S14A+S8+S26+ Sup, data = indice_gov_co)
summary(regcon_gov_co)

regcon_eco_co = lm(indice_eco_co$Eco ~ T_C*B_A*SEXO+ EDAD+S10+S23+S14A, data = indice_eco_co)
summary(regcon_eco_co)


regcon_gov_co = lm(indice_gov_co$Gov ~ T_C*B_A*EDAD +S10+S23+S14A, data = indice_gov_co)
summary(regcon_gov_co)

regcon_eco_co = lm(indice_eco_co$Eco ~ T_C*B_A*EDAD+ EDAD+S10+S23+S14A, data = indice_eco_co)
summary(regcon_eco_co)

regcon_gov_co = lm(indice_gov_co$Gov ~ T_C*B_A*S10 + EDAD+S10+S23+S14A, data = indice_gov_co)
summary(regcon_gov_co)

regcon_eco_co = lm(indice_eco_co$Eco ~ T_C*B_A*S10+ EDAD+S10+S23+S14A, data = indice_eco_co)
summary(regcon_eco_co)

regcon_gov_co = lm(indice_gov_co$Gov ~ T_C*B_A*S10 + EDAD+S10+S23+S14A, data = indice_gov_co)
summary(regcon_gov_co)

regcon_eco_co = lm(indice_eco_co$Eco ~ T_C*B_A*S10+ EDAD+S10+S23+S14A, data = indice_eco_co)
summary(regcon_eco_co)


regcon_gov_co = lm(indice_gov_co$Gov ~ T_C*B_A*S14A+ SEXO+ EDAD+S10+S23+Sup+S8+S26, data = indice_gov_co)
summary(regcon_gov_co)

###### TENDENCIAS PARALELAS #####

#En esta parte se realiza la grafica de tendencias paralelas
library(readxl)
col_gov = read_excel("col_gov.xlsx")
per_gov = read_excel("per_gov.xlsx")

plot(col_gov$Fecha_num, col_gov$Gov, type = "p",
     main= "", 
     xlab = "", ylab= "",pch =4, col = "darkblue", bg = "yellow", 
     cex = 1, lwd = 2.5)

title(main = "Gráfico Tendencias Índice C.Gobierno", sub = "",
      xlab = "Período", ylab = "Valor Índice (0-1)",
      cex.main = 2,   font.main= 2, col.main= "black",
      cex.sub = 0.75, font.sub = 3, col.sub = "black",
      col.lab ="black", cex.lab = 1.4
)


lines(lowess(col_gov$Fecha_num, col_gov$Gov), col = "darkblue", lwd = 3, pch= 4,type = "b", cex = 2, lty = "dotted")


points(col_gov$Fecha_num, per_gov$Gov, type = "p",
       pch = 16, col = "maroon",
       cex = 1, lwd=1.1)

lines(lowess(col_gov$Fecha_num, per_gov$Gov), col = "maroon", lwd = 3 , type = "b", cex = 2 , lty = "dotted")
abline(v = 20,  lty = "solid", lwd = 2, col = "dimgray")

legend("topleft",                                       # Add legend to plot
       legend = c("Colombia", "Peru"),
       col = c("darkblue", "maroon"),
       cex = c(1.5,1.5),
       pch = c(4, 16))


#### Graficas Indice Economia
col_eco = read_excel("col_eco.xlsx")
per_eco = read_excel("per_eco.xlsx")




plot(col_eco$Fecha_num, col_eco$Eco, type = "p",
     main= "", 
     xlab = "", ylab= "",pch =4, col = "darkblue", bg = "yellow", 
     cex = 1, lwd = 2.5, 
     ylim = c(0.45,0.7))

title(main = "Gráfico Tendencias Índice C.Economía", sub = "",
      xlab = "Período", ylab = "Valor Índice (0-1)",
      cex.main = 2,   font.main= 2, col.main= "black",
      cex.sub = 0.75, font.sub = 3, col.sub = "black",
      col.lab ="black", cex.lab = 1.4
)


lines(lowess(col_eco$Fecha_num, col_eco$Eco), col = "darkblue", lwd = 3, pch= 4,type = "b", cex = 2, lty = "dotted")


points(col_eco$Fecha_num, per_eco$Eco, type = "p",
       pch = 16, col = "maroon",
       cex = 1, lwd=1.1)

lines(lowess(col_eco$Fecha_num, per_eco$Eco), col = "maroon", lwd = 3 , type = "b", cex = 2 , lty = "dotted")
abline(v = 20,  lty = "solid", lwd = 2, col = "dimgray")

legend("topleft",                                       # Add legend to plot
       legend = c("Colombia", "Peru"),
       col = c("darkblue", "maroon"),
       cex = c(1.5,1.5),
       pch = c(4, 16))

###### Prueba medias #######

data_mean_col = subset(data_mean, IDENPA == 170, select = c(IDENPA, EDAD))

mean(data_mean_col$EDAD)


