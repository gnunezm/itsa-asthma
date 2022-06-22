### AVPP por asma

#Cargando librerias necesarias
librerias<-c('tidyverse','ggplot2','lubridate','forecast','readxl','stringr','writexl',
             'haven','stargazer','pander','zoo','broom')
ipak <- function(pkg){
    new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
    if (length(new.pkg))
        install.packages(new.pkg, dependencies = TRUE)
    sapply(pkg, require, character.only = TRUE)
}

ipak(librerias)

#Cargando archivos

listado_def<-list.files(path="/Users/Gonzalo/Documents/BD Defunciones Chile/",pattern="*.dta")
defun=data.frame()
temp=0
for (i in listado_def) {
    temp=0
    print(paste('Analizando',i))
    temp <-haven::read_dta(paste0("/Users/Gonzalo/Documents/BD Defunciones Chile/",i))
    colnames(temp)[grep("def_ano",colnames(temp))]<-"ano_def"
    colnames(temp)[grep("def_mes",colnames(temp))]<-"mes_def"
    colnames(temp)[grep("def_dia",colnames(temp))]<-"dia_def"
    colnames(temp)[grep("def_local",colnames(temp))]<-"lugar_def"
    colnames(temp)[grep("local_def",colnames(temp))]<-"lugar_def"
    colnames(temp)[grep("ANO_DEF",colnames(temp))]<-"ano_def"
    colnames(temp)[grep("MES_DEF",colnames(temp))]<-"mes_def"
    colnames(temp)[grep("DIA_DEF",colnames(temp))]<-"dia_def"
    colnames(temp)[grep("LOCAL_DEF",colnames(temp))]<-"lugar_def"
    colnames(temp)[grep("DIAG1",colnames(temp))]<-"diag1"
    colnames(temp)[grep("DIAG2",colnames(temp))]<-"diag2"
    colnames(temp)[grep("EDAD_TIPO",colnames(temp))]<-"edad_tipo"
    colnames(temp)[grep("EDAD_CANT",colnames(temp))]<-"edad_cant"
    colnames(temp)[grep("DIAG2",colnames(temp))]<-"diag2"
    colnames(temp)[grep("SEXO",colnames(temp))]<-"sexo"
    colnames(temp)[grep("COD_COMUNA",colnames(temp))]<-"nocomuna"
    colnames(temp)[grep("COMUNA",colnames(temp))]<-"comuna"
    temp$fecha_def<-as.Date(paste(temp$ano_def,temp$mes_def,temp$dia_def,sep="-"))
    temp$año<-temp$ano_def
    temp<-temp %>% select(año,fecha_def,sexo,edad_tipo,edad_cant,comuna,diag1,diag2,lugar_def)
    defun=rbind(defun,temp)
    }

listado2<-list.files(path="/Users/Gonzalo/Documents/BD Defunciones Chile/",pattern="*.csv")
defun2=read_csv2(paste0("/Users/Gonzalo/Documents/BD Defunciones Chile/",listado2),col_names = FALSE)

#Modificando y crando variables
defun2<-defun2 %>% select("X1","X2","X3","X4","X5","X6","X9","X18","X27")
colnames(defun2)<-c('año','fecha_def','sexo','edad_tipo','edad_cant','comuna','diag1','diag2','lugar_def')
defun2 <- defun2 %>% filter(año>=2018 & año<2022)
defun<-rbind(defun,defun2)
defun$diagn1<-substr(defun$diag1,1,3)
asma_def<-subset(defun,defun$diagn1=="J45" | defun$diagn1=="J46")
asma_def$trimestre<-as.yearqtr(asma_def$fecha_def)
asma_def<-subset(asma_def,asma_def$edad_cant>=15)
asma_def$EVN<-ifelse(asma_def$año<=2005 & (asma_def$sexo==1 | asma_def$sexo=='Hombre'),74.8,80.8)
asma_def$EVN<-ifelse(asma_def$año>2005 & asma_def$año<=2010 & (asma_def$sexo==1 | asma_def$sexo=='Hombre'),75.49,81.53)
asma_def$EVN<-ifelse(asma_def$año>2010 & asma_def$año<=2015 & (asma_def$sexo==1 | asma_def$sexo=='Hombre'),76.12,82.20)
asma_def$EVN<-ifelse(asma_def$año>2015 & asma_def$año<=2020 & (asma_def$sexo==1 | asma_def$sexo=='Hombre'),76.68,82.81)
asma_def$EVN<-ifelse(asma_def$año>2020 & asma_def$año<=2025 & (asma_def$sexo==1 | asma_def$sexo=='Hombre'),77.19,83.36)

#ITSA Asma por número de fallecidos
asma_def_st_año<-asma_def %>%
  filter(año>=2000) %>%
  group_by(año) %>%
  summarise(total=n())

asma_def_st_año$int1<-ifelse(asma_def_st_año$año<2008,0,1)
asma_def_st_año$int2<-ifelse(asma_def_st_año$año<2013,0,1)
asma_def_st_año$trat1<-ifelse(asma_def_st_año$año<2008,0,ifelse(asma_def_st_año$año>2012,0,1))
asma_def_st_año$trat2<-ifelse(asma_def_st_año$año<2013,0,1)

ts_año_def <- lm( total ~ año + int1 + int2 + trat, data=asma_def_st_año )

#ts_año_def <- lm( total ~ año + int1 + trat, data=asma_def_st_año )

asma_def_st_año$predict<-ts_año$fitted.values
summary(ts_año_def)
asma_def_sin_intervención<-asma_def_st_año
asma_def_sin_intervención$int1<-0
asma_def_sin_intervención$int2<-0
asma_def_sin_intervención$trat1<-0
asma_def_sin_intervención$trat2<-0
asma_def_sin_intervención$predict<-predict(ts_año_def,asma_def_sin_intervención)

a_def<-glance(ts_año_def)

ggplot(asma_def_st_año, aes(año,total)) +
  geom_point(color='brown') +
  geom_line(data=asma_def_sin_intervención,aes(y=predict,color="Contrafactual"),linetype=2) +
  geom_line(aes(y=predict,color='Tendencia Casos'),linetype=2) +
  labs(x ="Tiempo (años)", y = "Defunciones Anuales por Asma\n(CIE-10 J45 y J46)") +
  geom_vline(xintercept = c(2008,2013),linetype=4,color='red') +
  scale_y_continuous(limits=c(0,290),expand = c(0,0),breaks = seq(0,290,60)) +
  scale_color_manual(values=c('darkorange','steelblue')) +
  scale_x_continuous(expand=c(0,0),limits=c(2000,2021),breaks = seq(2000,2021,3)) +
  guides(color=guide_legend(title="Leyenda")) +
  annotate("text", x = 2004, y = 240, label = "Tendencia previa\na intervención") +
  annotate("text", x = 2010.5, y = 180, label = "Tendencia\nPosterior\na Inicio\nGES") +
  annotate("text", x = 2017, y = 210, label = "Tendencia\nPosterior\na actualización\nGuía GES") +
  annotate("text", x = 2017, y = 60, label = paste0("Tendencia Global\nal alza con\np-valor: ",round(a_def$p.value,5))) +
  theme_minimal()
ggsave('tendencia_asma_def.pdf',device = 'pdf')

#ITSA Asma según AVPP (YPLL)
asma_def_st_avpp<-asma_def %>%
  filter(año>=2000) %>%
  group_by(año) %>%
  summarise(total=sum(EVN-edad_cant))

asma_def_st_avpp$int1<-ifelse(asma_def_st_avpp$año<2008,0,1)
asma_def_st_avpp$int2<-ifelse(asma_def_st_avpp$año<2014,0,1)
asma_def_st_avpp$trat1<-ifelse(asma_def_st_avpp$año<2008,0,1)
asma_def_st_avpp$trat2<-ifelse(asma_def_st_avpp$año<2014,0,1)

ts_año_def_avpp<- lm( total ~ año + int1 + int2 + trat1 + trat2, data=asma_def_st_avpp )


asma_def_st_avpp$predict<-ts_año_def_avpp$fitted.values
summary(ts_año_def_avpp)
asma_def_sin_intervención_avpp<-asma_def_st_avpp
asma_def_sin_intervención_avpp$int1<-0
asma_def_sin_intervención_avpp$int2<-0
asma_def_sin_intervención_avpp$trat1<-0
asma_def_sin_intervención_avpp$trat2<-0
asma_def_sin_intervención_avpp$predict<-predict(ts_año_def_avpp,asma_def_sin_intervención_avpp)

a_def_avpp<-glance(ts_año_def_avpp)

ggplot(asma_def_st_avpp, aes(año,total)) +
  geom_point(color='brown') +
  geom_line(data=asma_def_sin_intervención_avpp,aes(y=predict,color="Contrafactual"),linetype=2) +
  geom_line(aes(y=predict,color='Tendencia Casos'),linetype=2) +
  labs(x ="Tiempo (años)", y = "Años de Vida Potencialmente Perdidos\npor Asma (CIE-10 J45 y J46)") +
  geom_vline(xintercept = c(2008,2013),linetype=4,color='red') +
  scale_y_continuous(limits=c(0,2550),expand = c(0,0),breaks = seq(0,2550,300)) +
  scale_color_manual(values=c('darkorange','steelblue')) +
  scale_x_continuous(expand=c(0,0),limits=c(2000,2021),breaks = seq(2000,2021,3)) +
  guides(color=guide_legend(title="Leyenda")) +
  annotate("text", x = 2004, y = 700, label = "Tendencia previa\na intervención") +
  annotate("text", x = 2010.5, y = 700, label = paste0("Tendencia\nPosterior\na Inicio\nGES\np-valor: ",round(summary(ts_año_def_avpp)$coefficients[,4][3],3))) +
  annotate("text", x = 2017, y = 700, label = paste0("Tendencia\nPosterior\na actualización\nGuía GES\np-valor: ",round(summary(ts_año_def_avpp)$coefficients[,4][4],3))) +
  annotate("text", x = 2017, y = 300, label = paste0("Tendencia Global\nen disminución con\np-valor: ",round(a_def_avpp$p.value,5))) +
  theme_minimal()
ggsave('tendencia_asma_def_avpp.pdf',device = 'pdf')



#ITSA Asma por trimestre casos totales (en construccion)

asma_def_st_trimestre<-asma_def %>%
    group_by(trimestre) %>%
    summarise(total=n())



asma_def_st_trimestre$int1<-ifelse(asma_def_st_trimestre$trimestre<"2008 Q1",0,1)
asma_def_st_trimestre$int2<-ifelse(asma_def_st_trimestre$trimestre<"2013 Q4",0,1)
asma_def_st_trimestre$trat<-ifelse(asma_def_st_trimestre$trimestre<"2008 Q1",1,ifelse(asma_def_st_trimestre$trimestre<"2014 Q1",2,3))

ts <- lm( total ~ trimestre + int1 + int2 + trat, data=asma_def_st_trimestre )
summary(ts)
asma_def_sin_intervención<-asma_def_st_trimestre
asma_def_st_trimestre$predict<-ts$fitted.values
asma_def_sin_intervención$int1<-0
asma_def_sin_intervención$int2<-0
asma_def_sin_intervención$trat<-0
asma_def_sin_intervención$predict<-predict(ts,asma_def_sin_intervención)

ggplot(asma_def_st_trimestre, aes(trimestre,total)) +
  geom_point(color='gray') +
  geom_line(data=asma_def_sin_intervención,aes(y=predict,color="Contrafactual"),linetype=2) +
  geom_line(aes(y=predict,color='Tendencia Casos'),linetype=2) +
  labs(x ="Tiempo (años)", y = "Defunciones por Asma") +
  geom_vline(xintercept = c(as.numeric(asma_def_st$trimestre[45]),as.numeric(asma_def_st$trimestre[68])),linetype=4,color='red') +
  scale_y_continuous(limits=c(0,115),expand = c(0,0)) +
  scale_color_manual(values=c('darkorange','steelblue')) +
  scale_x_yearqtr(format = "%YQ%q",expand=c(0,0),limits=c("1997Q1","2021Q4")) +
  theme_minimal()

plot( asma_def_st$trimestre, asma_def_st$total,
      bty="n", pch=19, col="gray",
      xlab = "Tiempo (años)", 
      ylab = "Defunciones por Asma")
lines(asma_def_st$trimestre, ts$fitted.values, col="steelblue", lwd=2 )
abline( v=2008, col="firebrick", lty=2 )
abline(v=2013,col="firebrick", lty=2 )
text( 2008, 300, "Inicio GES", col="firebrick", cex=1.3, pos=4 )
text( 2013, 300, "Actualización GES", col="firebrick", cex=1.3, pos=4 )

