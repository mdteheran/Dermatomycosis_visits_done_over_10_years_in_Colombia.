###Database curation##
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
packages <- c("actuar","AICcmodavg","agricolae","apaTables","aod","arm","astsa",
              "boot","broom","car","carData","caret","cmprsk","corrr","corrplot",
              "cowplot","correlationfunnel","explore","explor","DescTools","DALEX",
              "DataExplorer","datasets","DataEditR","data.table","dplyr","dynlm",
              "dlookr","editData","ellipse","easystats","esquisse","effects","effectsize",
              "faraway","fdth","fable","flexmix","flexsurv","forcats","forecast","grafify",
              "foreign","gapminder","GGally","ggcorrplot","ggalt","ggpubr","ggstatsplot",
              "ggrepel","gmodels","gtsummary","ggridges","ggfortify","ggplot2","ggview",
              "gplots","gridExtra","ggthemes","ggrain","ggdist","ggVennDiagram","ggflowchart","grid",
              "glue","haven","HH","Hmisc","hrbrthemes","ISLR","ISLR2","interactions","igraph",
              "htmlTable","janitor","jtools","kableExtra","KMsurv","lubridate","lsr",
              "lmtest","lmSupport","MASS","meta","magrittr","missForest","modelr",
              "modelsummary","modelStudio","pls","mFilter","MLmetrics","mstate",
              "multcompView","multcomp","nlme","oddsratio","parameters","PairedData",
              "palmerpenguins","pwr","PerformanceAnalytics","PASWR","picante",
              "psych","plotly","psychometric","QuantPsyc","rlang","reshape2","rattle","rgl",
              "ranger","RColorBrewer","readr","rmarkdown","rcompanion","riskRegression",
              "readxl","rstatix","randomForest","rpivotTable","RVAideMemoire","splines",
              "skimr","SMPracticals","stats","sjstats","ssym","summarytools","scatterD3",
              "survMisc","survminer","survival","texreg","tidyverse","timsac","tidyverse",
              "timetk","tsdl","tsbox","tsibble","tseries","TSstudio","tibble","tidyr",
              "vars","viridis","vioplot","wesanderson","xts","xtable","yarrr")
ipak(packages)
library(Gmisc, quietly = TRUE)

attach(dermat_sispro)
attach(sispro_final)

##Tabla 1##
visits<-apply(sispro_final[,5], 2, sum)
table1<-apply(sispro_final[,8:34], 2, sum)
data.frame(table1)

age<-apply(sispro_final[,10:15], 2, sum)
data.frame((age/visits)*100)
sex<-apply(sispro_final[,8:9], 2, sum)
data.frame((sex/visits)*100)
geo_zone<-apply(sispro_final[,18:20], 2, sum)
data.frame((geo_zone/visits)*100)
phys<-apply(sispro_final[,16:17], 2, sum)
data.frame((phys/visits)*100)
ICD<-apply(sispro_final[,21:34], 2, sum)
data.frame((ICD/visits)*100)

##Tabla 2##
visits<-apply(dermat_sispro[,5], 2, sum)
people<-apply(dermat_sispro[,6], 2, sum)
summary(visits/people)

atenc_region<-tapply(Atenciones, Region, sum, simplify = T)
as.data.frame(atenc_region)
person_region<-tapply(Personas, Region, sum, simplify = T)
as.data.frame(person_region)
intens_region<-tapply(Intensidad, Region, summary)
as.data.frame(intens_region)

visit_region<-tapply(`Visits 2010-2019`, Region, sum, simplify = T)
as.data.frame(visit_region)
tasa_reg<-(atenc_region/visit_region)*1000
as.data.frame(tasa_reg)

##Figura 1##
años_sum<- list(a2010=`2010`,a2011=`2011`,a2012=`2012`,a2013=`2013`,a2014=`2014`,
            a2015=`2015`,a2016=`2016`,a2017=`2017`,a2018=`2018`,a2019=`2019`)
data.frame(lapply(años, sum))

años_median<- data.frame(`2010`,`2011`,`2012`,`2013`,`2014`,
                `2015`,`2016`,`2017`,`2018`,`2019`)
apply(años_median,2,median)


par(mfrow=c(2, 5))
boxplot(`2010`, xlab = substitute(paste(bold('Mediana: 6.630; n: 362.398'))),
        ylab="Atenciones",cex.lab=2, main="Año 2010", cex.axis=1, notch=T, cex.main = 2,
        cex=2, col="white", pch=24, bg="brown")
axis(2, font=2)
boxplot(`2011`, xlab = substitute(paste(bold('Mediana: 8.789; n: 425.892'))),
        ylab="Atenciones",cex.lab=2, main="Año 2011", cex.axis=1, notch=T, cex.main = 2,
        cex=2, col="white", pch=24, bg="brown")
axis(2, font=2)
boxplot(`2012`, xlab = substitute(paste(bold('Mediana: 12.478; n: 564.019'))),
        ylab="Atenciones",cex.lab=2, main="Año 2012", cex.axis=1, notch=T, cex.main = 2,
        cex=2, col="darkgray", pch=24, bg="brown")
axis(2, font=2)
boxplot(`2013`, xlab = substitute(paste(bold('Mediana: 9.596; n: 461.462'))),
        ylab="Atenciones",cex.lab=2, main="Año 2013", cex.axis=1, notch=T, cex.main = 2,
        cex=2, col="white", pch=24, bg="brown")
axis(2, font=2)
boxplot(`2014`, xlab = substitute(paste(bold('Mediana: 11.756; n: 553.149'))),
        ylab="Atenciones",cex.lab=2, main="Año 2014", cex.axis=1, notch=T, cex.main = 2,
        cex=2, col="white", pch=24, bg="brown")
axis(2, font=2)
boxplot(`2015`, xlab = substitute(paste(bold('Mediana: 6.967; n: 434.397'))),
        ylab="Atenciones",cex.lab=2, main="Año 2015", cex.axis=1, notch=T, cex.main = 2,
        cex=2, col="white", pch=24, bg="brown")
axis(2, font=2)
boxplot(`2016`, xlab = substitute(paste(bold('Mediana: 4.199; n: 297.643'))),
        ylab="Atenciones",cex.lab=2, main="Año 2016", cex.axis=1, notch=T, cex.main = 2,
        cex=2, col="white", pch=24, bg="brown")
axis(2, font=2)
boxplot(`2017`, xlab = substitute(paste(bold('Mediana: 7.683; n: 379.456'))),
        ylab="Atenciones",cex.lab=2, main="Año 2017", cex.axis=1, notch=T, cex.main = 2,
        cex=2, col="white", pch=24, bg="brown")
axis(2, font=2)
boxplot(`2018`, xlab = substitute(paste(bold('Mediana: 9.320; n: 507.280'))),
        ylab="Atenciones",cex.lab=2, main="Año 2018", cex.axis=1, notch=T, cex.main = 2,
        cex=2, col="white", pch=24, bg="brown")
axis(2, font=2)
boxplot(`2019`, xlab = substitute(paste(bold('Mediana: 10.023; n: 584.897'))),
        ylab="Atenciones",cex.lab=2, main="Año 2019", cex.axis=1, notch=T, cex.main = 2,
        cex=2, col="lightblue", pch=24, bg="brown")
axis(2, font=2)

##Figura 2##
ggplot(dermat_sispro,aes(x= reorder(`ISO-3166`,-Intensidad),Intensidad))+
  labs(x = "Departamentos de Colombia [ISO-3166]",y = "Intensidad [atenciones por persona]")+
  theme_economist () +
  coord_flip(ylim=c(1.1,2)) +
  theme(axis.title.y = element_text(size=20,color="black",face="bold")) +
  theme(axis.text.y = element_text(size = 12,color="#04395E")) +
  theme(axis.text.x = element_text(size = 16,color="#04395E")) +
  theme(axis.title.x = element_text(size=20,color="black",face="bold")) +
  theme(axis.text = element_text(face="bold")) +
  geom_bar(position= 'dodge', stat ="identity", fill="#04395E", width=0.7) +
  geom_text(aes(label= round(Intensidad, digits = 2)),
            position=position_dodge(width=0.9),
            color="white", hjust= 1.5, size=6)

ggplot(dermat_sispro,aes(x= reorder(`ISO-3166`,-Tasa_atenciones),Tasa_atenciones))+
  labs(x = "Departamentos de Colombia [ISO-3166]",y = "Tasa de atenciones [por 1.000 atenciones prestadas]")+
  theme_economist () +
  coord_flip(ylim=c(0.6,2.4)) +
  theme(axis.title.y = element_text(size=20,color="black",face="bold")) +
  theme(axis.text.y = element_text(size = 12,color="#04395E")) +
  theme(axis.text.x = element_text(size = 16,color="#04395E")) +
  theme(axis.title.x = element_text(size=20,color="black",face="bold")) +
  theme(axis.text = element_text(face="bold")) +
  geom_bar(position= 'dodge', stat ="identity", fill="#04395E", width=0.7) +
  geom_text(aes(label= round(Tasa_atenciones, digits = 2)),
            position=position_dodge(width=0.9),
            color="white", hjust= 1.5, size=6)

