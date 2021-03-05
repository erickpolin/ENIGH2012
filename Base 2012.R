###################  Creación de los deciles 2012 #####################

#ENIGH 2012
library(foreign)
library(survey)
library(doBy)
library(reldist)
library(tidyverse)
options(survey.lonely.psu="adjust")

#reading the data
setwd("C:/Users/Erick/onedrive/GIC/GITHUB2018/GIC/ENIGH_2012/ENIGH2012/")
Conc<-read.dbf("NCV_concentrado_2012_concil_2010.dbf",as.is = T)

#Keeping Variables of interest

Conc<-Conc%>%
  select("folioviv"=FOLIOVIV,"gasto"=GASTO_MON,"foliohog"=FOLIOHOG, "tot_integ"=TOT_INTEG,"ing_cor"=ING_COR,"ingtrab"=INGTRAB,
         "trabajo"=TRABAJO,"negocio"=NEGOCIO,"otros_trab"=OTROS_TRAB,"rentas"=RENTAS,"utilidad"=UTILIDAD,
         "arrenda"= ARRENDA,"transfer"=TRANSFER,"jubilacion"=JUBILACION,"becas"=BECAS,"donativos"=DONATIVOS,
         "remesas"= REMESAS,"bene_gob"=BENE_GOB,"transf_hog"=TRANSF_HOG,"trans_inst"=TRANS_INST,
         "estim_alqu"=ESTIM_ALQU,"otros_ing"=OTROS_ING,"factor"=FACTOR_HOG,"upm"=UPM,"est_dis"=EST_DIS,"Tam_loc"=TAM_LOC)

Conc<-Conc%>%
  mutate(Small=ifelse(Tam_loc==4,1,0))

prop.table(table(Conc$Small))


################ DEfinir hogares in?genas 
Poblacion<-read.dbf("ncv_poblacion_2012_concil_2010_dbf.dbf",as.is = T)

Poblacion<-Poblacion%>%
  select(folioviv,foliohog,numren,parentesco,hablaind,comprenind,etnia)



#El concepto de hogar ind?gena se ha definido como aquel donde el jefe(a), 
#su c?nyuge o alguno de los ascendientes (madre o padre, madrastra o padrastro, abuelo(a),
#bisabuelo(a), tatarabuelo(a), suegro(a)) declararon hablar alguna lengua ind?gena.
parentescos<-c(101,102,201,202,203,204,205,601,602,606,607,608,615,616)

Poblacion<- Poblacion%>%
  mutate(HogarIndigena=ifelse(parentesco%in%parentescos&hablaind==1|parentesco%in%parentescos&comprenind==1|parentesco%in%parentescos&etnia==1,1,0))

HogaresIndigenas<-Poblacion %>%
  group_by(folioviv,foliohog)%>%
  summarize(HogarIndigena=mean(HogarIndigena))

HogaresIndigenas<-data.frame(HogaresIndigenas)

HogaresIndigenas<-HogaresIndigenas%>%
  mutate(HogarIndigena=ifelse(HogarIndigena>0,1,0))

prop.table(table(HogaresIndigenas$HogarIndigena))

Conc<-merge(Conc,HogaresIndigenas,by=c("folioviv","foliohog"))

########ya est?n los hogares ind?genas 

#the fist two digits of "folioviv" makes reference to the state
#Let?s create a variable called entidad that contains thos two first digits of the "folioviv" variable
Conc$entidad<-substr(Conc$folioviv,1,2)

############vamos a deflactar 
entidad<-c("01","02","03","04","05","06","07","08","09",
           "10","11","12","13","14","15","16","17","18","19","20",
           "21","22","23","24","25","26","27","28","29","30","31","32")

Deflactores<-c(79.83511852,78.76790086,81.34614419,77.71979475,79.43163136,79.45078962,78.07433841,
               80.58111794,77.34617233,77.74269473,77.78287166,79.01629746,76.66184735,77.90465055,
               77.97328156,78.82694321,80.55795457,79.13869647,81.50247101,80.15418451,79.40416714,
               76.27483257,81.4477534,79.2973544,83.32204201,80.5745756,79.6903822,81.13511586,78.92802579,
               79.17115265,79.1511371,79.0081984)

entidades<-c("Aguascalientes","Baja California","Baja California Sur","Campeche","Coahuila de Zaragoza",
             "Colima","Chiapas","Chihuahua","Ciudad de México","Durango","Guanajuato","Guerrero","Hidalgo",
             "Jalisco","México","Michoacán de Ocampo","Morelos","Nayarit","Nuevo León","Oaxaca","Puebla",
             "Querétaro","Quintana Roo","San Luis Potosí","Sinaloa","Sonora","Tabasco","Tamaulipas","Tlaxcala","Veracruz de Ignacio de la Llave","Yucatán","Zacatecas")

Deflactores2012<-data.frame(entidad,entidades,Deflactores)

Conc<-merge(Conc,Deflactores2012,by=c("entidad"))

Conc <- Conc%>%
 mutate(ing_cor=(ing_cor/Deflactores)*100, ingtrab=(ingtrab/Deflactores)*100, trabajo=(trabajo/Deflactores)*100, 
         negocio=(negocio/Deflactores)*100, otros_trab=(otros_trab/Deflactores)*100, rentas=(rentas/Deflactores)*100,
         utilidad=(utilidad/Deflactores)*100,arrenda=(arrenda/Deflactores)*100, transfer=(transfer/Deflactores)*100,
         jubilacion=(jubilacion/Deflactores)*100, becas=(becas/Deflactores)*100, donativos=(donativos/Deflactores)*100,
         remesas=(remesas/Deflactores)*100, bene_gob=(bene_gob/Deflactores)*100, transf_hog=(transf_hog/Deflactores)*100, 
         trans_inst=(trans_inst/Deflactores)*100,estim_alqu=(estim_alqu/Deflactores)*100, otros_ing=(otros_ing/Deflactores)*100,
        gasto=(gasto/Deflactores)*100)



#apparently this is a "flag", IDK what is this shit yet
Conc$Nhog <- 1

########################################## DECILES 

#Attaching the data frame
attach(Conc) #this is for not writing the name of the data frame and the $ ever time

#Sort Conc according to ing_cor, folioviv, foliohog
Conc<- orderBy (~+ing_cor+folioviv+foliohog, data=Conc) #this give us the households sorted by total income

#Adding the values of the expansion factor. The sum is 34 million, which is the number of households in the country.
tot_hogares<-sum(Conc$factor,to.data.frame=TRUE)

#Dividing the number of households into 10 without decimals
tam_dec<-trunc(tot_hogares/10) #the result is 3.5 million housholds per decile

#Adding a variable to the data frame with this value
Conc$tam_dec<-tam_dec

############################# Creating Deciles of Income 

Conc$MAXT<-Conc$ing_cor #vamos a crear en esta base la variable MAXT que es una copia de la columna de ingresos

Conc<-Conc[with(Conc, order(rank(MAXT))),]  #lo que hicimos aqu? fue reordenar la base con respecto a MAXT. Cosa que en realidad, ya estaba.

Conc$ACUMULA<-cumsum(Conc$factor) #aqu? creamos una variable de suma acumulada del factor de viviendas.



############################Ahora viene la creaci?n de los deciles

#no se que es esto de a1 y b1. Pero s? e simportante. Los resultados cmabian por poquito si no lo haces 
for(i in 1:9)
{
  a1<-Conc[dim(Conc[Conc$ACUMULA<tam_dec*i,])[1]+1,]$factor
  Conc<-rbind(Conc[1:(dim(Conc[Conc$ACUMULA<tam_dec*i,])[1]+1),],
              Conc[(dim(Conc[Conc$ACUMULA<tam_dec*i,])[1]+1):dim(Conc[1])[1],])
  b1<-tam_dec*i-Conc[dim(Conc[Conc$ACUMULA<tam_dec*i,])[1],]$ACUMULA
  Conc[(dim(Conc[Conc$ACUMULA<tam_dec*i,])[1]+1),]$factor<-b1
  Conc[(dim(Conc[Conc$ACUMULA<tam_dec*i,])[1]+2),]$factor<-(a1-b1)
}

#aqu? estamos creando otra variable de suma acumulada del n?mero de hogares
Conc$ACUMULA2<-cumsum(Conc$factor)

#aqu? estamos creando una variable que se llama decil que solo tiene ceros
Conc$DECIL<-0

#recordemos que el tama?o de cada decil es de 3,474,481. 
#loq ue hicimos aqu? es pedirle que ponga un uno a los primeros hogares menores al tama?o de decil. 
#es decir, que el primer decil tiene ya UNOS
Conc[(Conc$ACUMULA2<=tam_dec),]$DECIL<-1

#para una sucesi?n del 1 al 9, cuando la variable acumulado2 sea mayor que el tama?o de decil multiplicado por
#1, 2, 3... pone en la variable decil el n?mero i+1
for(i in 1:9)
{
  Conc[((Conc$ACUMULA2>tam_dec*i)&(Conc$ACUMULA2<=tam_dec*(i+1))),]$DECIL<-(i+1)
}

# a lo que le qued? cero (que es la ?ltima observaci?n), ponle el decil 10
Conc[Conc$DECIL%in%"0",]$DECIL<-10

#Ahora vamos a ahcerlos per capita

Conc<-Conc%>%
  mutate(ing_cor=ing_cor/tot_integ,
         ingtrab=ingtrab/tot_integ,
         trabajo=trabajo/tot_integ,
         negocio=negocio/tot_integ,
         otros_trab=otros_trab/tot_integ,
         rentas=rentas/tot_integ,
         utilidad=utilidad/tot_integ,
         arrenda=arrenda/tot_integ,
         transfer=transfer/tot_integ,
         jubilacion=jubilacion/tot_integ,
         becas=becas/tot_integ,
         donativos=donativos/tot_integ,
         remesas=remesas/tot_integ,
         bene_gob=bene_gob/tot_integ,
         transf_hog=transf_hog/tot_integ,
         trans_inst=trans_inst/tot_integ,
         estim_alqu=estim_alqu/tot_integ,
         otros_ing=otros_ing/tot_integ,
         gasto=gasto/tot_integ)

#Conc<-Conc%>%
 # mutate(prueba=ingtrab+rentas+transfer+estim_alqu+otros_ing)

#all.equal(Conc$ing_cor,Conc$prueba)

Conc<-Conc%>%
  mutate(Bottom_40=ifelse(DECIL<5,1,0))



write.dbf(Conc,file="Conc_2012.dbf")

# TOTAL HOGARES
x<-tapply(Conc$factor,Conc$Nhog,sum)
# DECILES
y<-tapply(Conc$factor,Conc$DECIL,sum)
# se calcula el promedio (ingreso entre los hogares) tanto para el total como para cada uno de los deciles
ing_cormed_t<-tapply(Conc$factor*Conc$ing_cor,Conc$Nhog,sum)/x
ing_cormed_d<-tapply(Conc$factor*Conc$ing_cor,Conc$DECIL,sum)/y
########################## C U A D R O S 
# guardamos los resultados en un data frame
prom_rub <- data.frame (c(ing_cormed_t,ing_cormed_d))
# agregamos el nombre a las filas
Numdec<-c("Total", "I", "II", "III","IV", "V", "VI", "VII", "VIII", "IX","X")
row.names(prom_rub)<-Numdec

# GINI Nacional (sobre los 10 deciles) por hogar usando el promedio del ingreso corriente (ingcor)
deciles_hog_ingcor <- data.frame(hogaresxdecil=c(x,x,x,x,x,x,x,x,x,x),
                                 ingreso=c(ing_cormed_d[1],ing_cormed_d[2],ing_cormed_d[3],
                                           ing_cormed_d[4],ing_cormed_d[5],ing_cormed_d[6],
                                           ing_cormed_d[7],ing_cormed_d[8],ing_cormed_d[9],
                                           ing_cormed_d[10]))
# se efectua la función Gini y se guarda en nuestro vector a.
a<-gini(deciles_hog_ingcor$ingreso,weights=deciles_hog_ingcor$hogares)
# se renombran las variables (columnas)
names(prom_rub)=c("INGRESO CORRIENTE")
names(a)="GINI"
##### Mostramos el resultado en pantalla 
round(prom_rub)
round(a,3)
rm(list = ls())


################## Creadicón de las tablas de ingreso por fuente por decil #############
library(foreign)
library(survey)
library(doBy)
library(reldist)
library(tidyverse)
options(survey.lonely.psu="adjust")

#reading the data
setwd("C:/Users/Erick/onedrive/GIC/GITHUB2018/GIC/ENIGH_2012/ENIGH2012/")
Conc2012<-read.dbf("Conc_2012.dbf",as.is = T)


names(Conc2012)<-c("ENTIDAD","FOLIOVIV","FOLIOHOG","GASTO","TOT_INTEG","INGCOR",
"INGTRAB","TRABAJO","NEGOCIO","OTROS_TRAB","RENTAS","UTILIDAD","ARRENDA",
"TRANSFER","JUBILACION","BECAS","DONATIVOS","REMESAS","BENE_GOB",
"TRANSF_HOG","TRANS_INST","ESTIM_ALQU","OTROS_ING","FACTOR","UPM",
"EST_DIS","TAM_LOC","SMALL","HOGARINDIG","ENTIDADES","DEFLACTORE",
"NHOG","TAM_DEC","MAXT","ACUMULA","ACUMULA2","DECIL","BOTTOM_40")

mydesign <- svydesign(id=~UPM,strata=~EST_DIS,data=Conc2012,weights=~FACTOR)

#vamos por el ingreso corriente total del pa?s
# ing_ cor se define como La suma de las variables ingtrab, rentas, transfer, estim_alqu y otros_ing.
#te sale que el ingreso trimestra promedio en Mexico es de 49,610.
#notes? que esto no es otra cosa que el ing_cor*factor/34744819
Ming_corTot <- svyratio(~INGCOR,denominator=~NHOG,mydesign) 

#ahora, vamos a hacer lo mismo por decil
#aqu? cmabia la funci?n a svyby, en by va el decil que creamos.
#y al final va la funci?n que queremos
Ming_corDECIL <- svyby(~INGCOR,denominator=~NHOG,by=~DECIL,mydesign,svyratio)


#     Trabajo
#
#El trabajo se divide en tres clasificaciones: subordinado, independiente y otros.
### ingreso del trabajo total###
MingtrabTot <- svyratio(~INGTRAB,denominator=~NHOG,mydesign) # Total promedio
MingtrabDECIL <- svyby(~INGTRAB,denominator=~NHOG,by=~DECIL ,mydesign,svyratio) # por decil
###### ingreso del trabajo subordinado
MtrabajoTot <- svyratio(~TRABAJO,denominator=~NHOG,mydesign) # Total promedio
MtrabajoDECIL <- svyby(~TRABAJO,denominator=~NHOG,by=~DECIL ,mydesign,svyratio) # por decil
###### ingreso del trabajo independiente
MnegocioTot <- svyratio(~NEGOCIO,denominator=~NHOG,mydesign) # Total promedio
MnegocioDECIL <- svyby(~NEGOCIO,denominator=~NHOG,by=~DECIL ,mydesign,svyratio) # por decil
###### ingreso de otros trabajos
Motros_trabTot <- svyratio(~OTROS_TRAB,denominator=~NHOG,mydesign) # Total promedio
Motros_trabDECIL<- svyby(~OTROS_TRAB,denominator=~NHOG,by=~DECIL,mydesign,svyratio) # por decil


###################################        Rentas de la propiedad 

#la renta de la propiedad se divide en: ingresos de sociedades y arrendamientos.

#ingresos totales por renta de la porpiedad
MrentasTot <- svyratio(~RENTAS,denominator=~NHOG,mydesign) # Total promedio
MrentasDECIL <- svyby(~RENTAS,denominator=~NHOG,by=~DECIL ,mydesign,svyratio) #Por decil
###### ingresos de sociedades
MutilidadTot <- svyratio(~UTILIDAD,denominator=~NHOG,mydesign) # Total promedio
MutilidadDECIL <- svyby(~UTILIDAD,denominator=~NHOG,by=~DECIL ,mydesign,svyratio) # por decil
###### arrendamiento
MarrendaTot <- svyratio(~ARRENDA,denominator=~NHOG,mydesign) # Total promedio
MarrendaDECIL <- svyby(~ARRENDA,denominator=~NHOG,by=~DECIL ,mydesign,svyratio) # Por decil


###################################        Transferencias   

#las transferencias totales se definen como la suma de jubilacion, becas, donativos, remesas, bene_gob, transf_hog y trans_inst.

MtransferTot <- svyratio(~TRANSFER,denominator=~NHOG,mydesign) # Total promedio
MtransferDECIL <- svyby(~TRANSFER,denominator=~NHOG,by=~DECIL ,mydesign,svyratio) # DECIL

###### jubilacion se define como Jubilaciones, pensiones e indemnizaciones por accidente de trabajo despido y retiro voluntario.
#En el cuestionario solo se les pregunta si recibi? jubilaciones. As? que puede ser p?blicas o privadas.

MjubilacionTot <- svyratio(~JUBILA,denominator=~NHOG,mydesign) # Total promedio
MjubilacionDECIL <- svyby(~JUBILA,denominator=~NHOG,by=~DECIL ,mydesign,svyratio) # decil

###### becas que pueden ser, de nuevo, p?blicas privadas. 
MbecasTot <- svyratio(~BECA,denominator=~NHOG,mydesign) # Total promedio
MbecasDECIL <- svyby(~BECA,denominator=~NHOG,by=~DECIL ,mydesign,svyratio) # decil

###### donativos que tambi?n pueden ser p?blicos o privados.
MdonativosTot <- svyratio(~DONATIVO,denominator=~NHOG,mydesign) # Total promedio
MdonativosDECIL <- svyby(~DONATIVO,denominator=~NHOG,by=~DECIL ,mydesign,svyratio) # DECIL

###### remesas se definen como ingresos provenientes d eotros paises. As? de manera gen?rica.
MremesasTot <- svyratio(~REMESA,denominator=~NHOG,mydesign) # Total promedio
MremesasDECIL <- svyby(~REMESA,denominator=~NHOG,by=~DECIL ,mydesign,svyratio) # DECIL

###### bene_gob:  aqu? estna los programas p?blicos. Prospera, procampo, 65 y m?s, adultos mayores, sin hambre, empleo tempora y Otros.
Mbene_gobTot <- svyratio(~BENE_GOB,denominator=~NHOG,mydesign) # Total promedio
Mbene_gobDECIL <- svyby(~BENE_GOB,denominator=~NHOG,by=~DECIL ,mydesign,svyratio) # decil

###### transf_hog:  Esto es lo que transfiere otro hogar.
Mtransf_hogTot <- svyratio(~ESP_HOG,denominator=~NHOG,mydesign) # Total promedio
Mtransf_hogDECIL <- svyby(~ESP_HOG,denominator=~NHOG,by=~DECIL ,mydesign,svyratio) #decil

###### trans_inst: puede venir de institucione sp?blicas o privadas.
Mtrans_instTot <- svyratio(~ESP_INST,denominator=~NHOG,mydesign) # Total promedio
Mtrans_instDECIL <- svyby(~ESP_INST,denominator=~NHOG,by=~DECIL ,mydesign,svyratio) # DECIL


### estim_alqu ### Aparentemente se le pregunta al entrevistado cu?nto constar?a la renta del lugar donde vive.
Mestim_alquTot <- svyratio(~ESTI,denominator=~NHOG,mydesign) # Total promedio
Mestim_alquDECIL <- svyby(~ESTI,denominator=~NHOG,by=~DECIL ,mydesign,svyratio) # decil


### otros_ing ### es literalmente ?algo m?s?
Motros_ingTot <- svyratio(~OTROS,denominator=~NHOG,mydesign) # Total promedio
Motros_ingDECIL <- svyby(~OTROS,denominator=~NHOG,by=~DECIL ,mydesign,svyratio) # Decil


######################################### Estimaciones 

ES_Ming_corTot <- Ming_corTot[[1]] #lo que estoy haciendo aqu? es extraer el valor de la primera columa que corresponde al c?lculo.
ES_Ming_corDECIL <- Ming_corDECIL[[2]] #En el caso de las entidades, los c?lculos quedaron en la segunda columna

ES_MingtrabTot <- MingtrabTot[[1]]
ES_MingtrabDECIL <- MingtrabDECIL[[2]]

ES_MtrabajoTot <- MtrabajoTot[[1]]
ES_MtrabajoDECIL <- MtrabajoDECIL[[2]]

ES_MnegocioTot <- MnegocioTot[[1]]
ES_MnegocioDECIL <- MnegocioDECIL[[2]]

ES_Motros_trabTot <- Motros_trabTot [[1]]
ES_Motros_trabDECIL <- Motros_trabDECIL [[2]]

ES_MrentasTot <- MrentasTot [[1]]
ES_MrentasDECIL <- MrentasDECIL [[2]]

ES_MutilidadTot <- MutilidadTot [[1]]
ES_MutilidadDECIL <- MutilidadDECIL [[2]]

ES_MarrendaTot <- MarrendaTot [[1]]
ES_MarrendaDECIL <- MarrendaDECIL [[2]]

ES_MtransferTot <- MtransferTot[[1]]
ES_MtransferDECIL <- MtransferDECIL[[2]]

ES_MjubilacionTot <- MjubilacionTot [[1]]
ES_MjubilacionDECIL <- MjubilacionDECIL [[2]]

ES_MbecasTot <- MbecasTot [[1]]
ES_MbecasDECIL <- MbecasDECIL [[2]]

ES_MdonativosTot <- MdonativosTot[[1]]
ES_MdonativosDECIL <- MdonativosDECIL[[2]]

ES_MremesasTot <- MremesasTot[[1]]
ES_MremesasDECIL <- MremesasDECIL[[2]]

ES_Mbene_gobTot <- Mbene_gobTot [[1]]
ES_Mbene_gobDECIL <- Mbene_gobDECIL [[2]]

ES_Mtransf_hogTot <- Mtransf_hogTot [[1]]
ES_Mtransf_hogDECIL <- Mtransf_hogDECIL [[2]]

ES_Mtrans_instTot <- Mtrans_instTot[[1]]
ES_Mtrans_instDECIL <- Mtrans_instDECIL[[2]]

ES_Mestim_alquTot <- Mestim_alquTot [[1]]
ES_Mestim_alquDECIL <- Mestim_alquDECIL [[2]]

ES_Motros_ingTot <- Motros_ingTot [[1]]
ES_Motros_ingDECIL <- Motros_ingDECIL [[2]]

########## Error Est?ndar 
SE_Ming_corTot <- SE (Ming_corTot)
SE_Ming_corDECIL <- SE (Ming_corDECIL)

SE_MingtrabTot <- SE (MingtrabTot)
SE_MingtrabDECIL <- SE (MingtrabDECIL)

SE_MtrabajoTot <- SE (MtrabajoTot)
SE_MtrabajoDECIL <- SE (MtrabajoDECIL)

SE_MnegocioTot <- SE (MnegocioTot)
SE_MnegocioDECIL <- SE (MnegocioDECIL)

SE_Motros_trabTot <- SE (Motros_trabTot)
SE_Motros_trabDECIL <- SE (Motros_trabDECIL)

SE_MrentasTot <- SE (MrentasTot)
SE_MrentasDECIL <- SE (MrentasDECIL)

SE_MutilidadTot <- SE (MutilidadTot)
SE_MutilidadDECIL <- SE (MutilidadDECIL)

SE_MarrendaTot <- SE (MarrendaTot)
SE_MarrendaDECIL <- SE (MarrendaDECIL)

SE_MtransferTot <- SE (MtransferTot)
SE_MtransferDECIL <- SE (MtransferDECIL)

SE_MjubilacionTot <- SE (MjubilacionTot)
SE_MjubilacionDECIL <- SE (MjubilacionDECIL)

SE_MbecasTot <- SE (MbecasTot)
SE_MbecasDECIL <- SE (MbecasDECIL)

SE_MdonativosTot <- SE (MdonativosTot)
SE_MdonativosDECIL <- SE (MdonativosDECIL)

SE_MremesasTot <- SE (MremesasTot)
SE_MremesasDECIL <- SE (MremesasDECIL)

SE_Mbene_gobTot <- SE (Mbene_gobTot)
SE_Mbene_gobDECIL <- SE (Mbene_gobDECIL)

SE_Mtransf_hogTot <- SE (Mtransf_hogTot)
SE_Mtransf_hogDECIL <- SE (Mtransf_hogDECIL)

SE_Mtrans_instTot <- SE (Mtrans_instTot)
SE_Mtrans_instDECIL <- SE (Mtrans_instDECIL)

SE_Mestim_alquTot <- SE (Mestim_alquTot)
SE_Mestim_alquDECIL <- SE (Mestim_alquDECIL)

SE_Motros_ingTot <- SE (Motros_ingTot)
SE_Motros_ingDECIL <- SE (Motros_ingDECIL)

########## Coeficiente de variaci?n 
CV_Ming_corTot <- cv(Ming_corTot)
CV_Ming_corDECIL <- cv(Ming_corDECIL)

CV_MingtrabTot <- cv(MingtrabTot)
CV_MingtrabDECIL <- cv(MingtrabDECIL)

CV_MtrabajoTot <- cv(MtrabajoTot)
CV_MtrabajoDECIL <- cv(MtrabajoDECIL)

CV_MnegocioTot <- cv(MnegocioTot)
CV_MnegocioDECIL <- cv(MnegocioDECIL)

CV_Motros_trabTot <- cv(Motros_trabTot)
CV_Motros_trabDECIL <- cv(Motros_trabDECIL)

CV_MrentasTot <- cv(MrentasTot)
CV_MrentasDECIL <- cv(MrentasDECIL)

CV_MutilidadTot <- cv(MutilidadTot)
CV_MutilidadDECIL <- cv(MutilidadDECIL)

CV_MarrendaTot <- cv(MarrendaTot)
CV_MarrendaDECIL <- cv(MarrendaDECIL)

CV_MtransferTot <- cv(MtransferTot)
CV_MtransferDECIL <- cv(MtransferDECIL)

CV_MjubilacionTot <- cv(MjubilacionTot)
CV_MjubilacionDECIL <- cv(MjubilacionDECIL)

CV_MbecasTot <- cv(MbecasTot)
CV_MbecasDECIL <- cv(MbecasDECIL)

CV_MdonativosTot <- cv(MdonativosTot)
CV_MdonativosDECIL <- cv(MdonativosDECIL)

CV_MremesasTot <- cv(MremesasTot)
CV_MremesasDECIL <- cv(MremesasDECIL)

CV_Mbene_gobTot <- cv(Mbene_gobTot)
CV_Mbene_gobDECIL <- cv(Mbene_gobDECIL)

CV_Mtransf_hogTot <- cv(Mtransf_hogTot)
CV_Mtransf_hogDECIL <- cv(Mtransf_hogDECIL)

CV_Mtrans_instTot <- cv(Mtrans_instTot)
CV_Mtrans_instDECIL <- cv(Mtrans_instDECIL)

CV_Mestim_alquTot <- cv(Mestim_alquTot)
CV_Mestim_alquDECIL <- cv(Mestim_alquDECIL)

CV_Motros_ingTot <- cv(Motros_ingTot)
CV_Motros_ingDECIL <- cv(Motros_ingDECIL)

########## Limite inferior 
LI_Ming_corTot <- confint(Ming_corTot,level=0.90)[,1]
LI_Ming_corDECIL <- confint(Ming_corDECIL,level=0.90)[,1]

LI_MingtrabTot <- confint(MingtrabTot,level=0.90)[,1]
LI_MingtrabDECIL <- confint(MingtrabDECIL,level=0.90)[,1]

LI_MtrabajoTot <- confint(MtrabajoTot,level=0.90)[,1]
LI_MtrabajoDECIL <- confint(MtrabajoDECIL,level=0.90)[,1]

LI_MnegocioTot <- confint(MnegocioTot,level=0.90)[,1]
LI_MnegocioDECIL <- confint(MnegocioDECIL,level=0.90)[,1]

LI_Motros_trabTot <- confint(Motros_trabTot,level=0.90)[,1]
LI_Motros_trabDECIL <- confint(Motros_trabDECIL,level=0.90)[,1]

LI_MrentasTot <- confint(MrentasTot,level=0.90)[,1]
LI_MrentasDECIL <- confint(MrentasDECIL,level=0.90)[,1]

LI_MutilidadTot <- confint(MutilidadTot,level=0.90)[,1]
LI_MutilidadDECIL <- confint(MutilidadDECIL,level=0.90)[,1]

LI_MarrendaTot <- confint(MarrendaTot,level=0.90)[,1]
LI_MarrendaDECIL <- confint(MarrendaDECIL,level=0.90)[,1]

LI_MtransferTot <- confint(MtransferTot,level=0.90)[,1]
LI_MtransferDECIL <- confint(MtransferDECIL,level=0.90)[,1]

LI_MjubilacionTot <- confint(MjubilacionTot,level=0.90)[,1]
LI_MjubilacionDECIL <- confint(MjubilacionDECIL,level=0.90)[,1]

LI_MbecasTot <- confint(MbecasTot,level=0.90)[,1]
LI_MbecasDECIL <- confint(MbecasDECIL,level=0.90)[,1]

LI_MdonativosTot <- confint(MdonativosTot,level=0.90)[,1]
LI_MdonativosDECIL <- confint(MdonativosDECIL,level=0.90)[,1]

LI_MremesasTot <- confint(MremesasTot,level=0.90)[,1]
LI_MremesasDECIL <- confint(MremesasDECIL,level=0.90)[,1]

LI_Mbene_gobTot <- confint(Mbene_gobTot,level=0.90)[,1]
LI_Mbene_gobDECIL <- confint(Mbene_gobDECIL,level=0.90)[,1]

LI_Mtransf_hogTot <- confint(Mtransf_hogTot,level=0.90)[,1]
LI_Mtransf_hogDECIL <- confint(Mtransf_hogDECIL,level=0.90)[,1]

LI_Mtrans_instTot <- confint(Mtrans_instTot,level=0.90)[,1]
LI_Mtrans_instDECIL <- confint(Mtrans_instDECIL,level=0.90)[,1]

LI_Mestim_alquTot <- confint(Mestim_alquTot,level=0.90)[,1]
LI_Mestim_alquDECIL <- confint(Mestim_alquDECIL,level=0.90)[,1
]
LI_Motros_ingTot <- confint(Motros_ingTot,level=0.90)[,1]
LI_Motros_ingDECIL <- confint(Motros_ingDECIL ,level=0.90)[,1]

########## Limite superior 
LS_Ming_corTot <- confint(Ming_corTot,level=0.90)[,2]
LS_Ming_corDECIL <- confint(Ming_corDECIL,level=0.90)[,2]

LS_MingtrabTot <- confint(MingtrabTot,level=0.90)[,2]
LS_MingtrabDECIL <- confint(MingtrabDECIL,level=0.90)[,2]

LS_MtrabajoTot <- confint(MtrabajoTot,level=0.90)[,2]
LS_MtrabajoDECIL <- confint(MtrabajoDECIL,level=0.90)[,2]

LS_MnegocioTot <- confint(MnegocioTot,level=0.90)[,2]
LS_MnegocioDECIL <- confint(MnegocioDECIL,level=0.90)[,2]

LS_Motros_trabTot <- confint(Motros_trabTot,level=0.90)[,2]
LS_Motros_trabDECIL <- confint(Motros_trabDECIL,level=0.90)[,2]

LS_MrentasTot <- confint(MrentasTot,level=0.90)[,2]
LS_MrentasDECIL <- confint(MrentasDECIL,level=0.90)[,2]

LS_MutilidadTot <- confint(MutilidadTot,level=0.90)[,2]
LS_MutilidadDECIL <- confint(MutilidadDECIL,level=0.90)[,2]

LS_MarrendaTot <- confint(MarrendaTot,level=0.90)[,2]
LS_MarrendaDECIL <- confint(MarrendaDECIL,level=0.90)[,2]

LS_MtransferTot <- confint(MtransferTot,level=0.90)[,2]
LS_MtransferDECIL <- confint(MtransferDECIL,level=0.90)[,2]

LS_MjubilacionTot <- confint(MjubilacionTot,level=0.90)[,2]
LS_MjubilacionDECIL <- confint(MjubilacionDECIL,level=0.90)[,2]

LS_MbecasTot <- confint(MbecasTot,level=0.90)[,2]
LS_MbecasDECIL <- confint(MbecasDECIL,level=0.90)[,2]

LS_MdonativosTot <- confint(MdonativosTot,level=0.90)[,2]
LS_MdonativosDECIL <- confint(MdonativosDECIL,level=0.90)[,2]

LS_MremesasTot <- confint(MremesasTot,level=0.90)[,2]
LS_MremesasDECIL <- confint(MremesasDECIL,level=0.90)[,2]

LS_Mbene_gobTot <- confint(Mbene_gobTot,level=0.90)[,2]
LS_Mbene_gobDECIL <- confint(Mbene_gobDECIL,level=0.90)[,2]

LS_Mtransf_hogTot <- confint(Mtransf_hogTot,level=0.90)[,2]
LS_Mtransf_hogDECIL <- confint(Mtransf_hogDECIL,level=0.90)[,2]

LS_Mtrans_instTot <- confint(Mtrans_instTot,level=0.90)[,2]
LS_Mtrans_instDECIL <- confint(Mtrans_instDECIL,level=0.90)[,2]

LS_Mestim_alquTot <- confint(Mestim_alquTot,level=0.90)[,2]
LS_Mestim_alquDECIL <- confint(Mestim_alquDECIL,level=0.90)[,2]

LS_Motros_ingTot <- confint(Motros_ingTot,level=0.90)[,2]
LS_Motros_ingDECIL <- confint(Motros_ingDECIL,level=0.90)[,2]

#############################      Cuadros   
#este cuadro, lo ?nico que tiene son todas la estimaciones.
#son 10 filas y 18 columnas.
c_DECIL_ES <-
  data.frame(c(ES_Ming_corTot,ES_Ming_corDECIL),c(ES_MingtrabTot,ES_MingtrabDECIL),c(ES_MtrabajoTot,ES_MtrabajoDECIL),c(ES_MnegocioTot,ES_MnegocioDECIL)
             ,c(ES_Motros_trabTot,ES_Motros_trabDECIL),c(ES_MrentasTot,ES_MrentasDECIL),c(ES_MutilidadTot,ES_MutilidadDECIL)
             ,c(ES_MarrendaTot,ES_MarrendaDECIL),c(ES_MtransferTot,ES_MtransferDECIL),c(ES_MjubilacionTot,ES_MjubilacionDECIL),c(ES_MbecasTot,ES_MbecasDECIL),
             c(ES_MdonativosTot,ES_MdonativosDECIL),c(ES_MremesasTot,ES_MremesasDECIL),c(ES_Mbene_gobTot,ES_Mbene_gobDECIL),c(ES_Mtransf_hogTot,ES_Mtransf_hogDECIL)
             ,c(ES_Mtrans_instTot,ES_Mtrans_instDECIL),c(ES_Mestim_alquTot,ES_Mestim_alquDECIL),c(ES_Motros_ingTot,ES_Motros_ingDECIL))
##### ERROR ESTANDAR
c_DECIL_SE <-
  data.frame(c(SE_Ming_corTot,SE_Ming_corDECIL),c(SE_MingtrabTot,SE_MingtrabDECIL),c(SE_MtrabajoTot,SE_MtrabajoDECIL),c(SE_MnegocioTot,SE_MnegocioDECIL)
             ,c(SE_Motros_trabTot,SE_Motros_trabDECIL),c(SE_MrentasTot,SE_MrentasDECIL),c(SE_MutilidadTot,SE_MutilidadDECIL)
             ,c(SE_MarrendaTot,SE_MarrendaDECIL),c(SE_MtransferTot,SE_MtransferDECIL),c(SE_MjubilacionTot,SE_MjubilacionDECIL),c(SE_MbecasTot,SE_MbecasDECIL),
             c(SE_MdonativosTot,SE_MdonativosDECIL),c(SE_MremesasTot,SE_MremesasDECIL),c(SE_Mbene_gobTot,SE_Mbene_gobDECIL),c(SE_Mtransf_hogTot,SE_Mtransf_hogDECIL),c(SE_Mtrans_instTot,SE_Mtrans_instDECIL)
             ,c(SE_Mestim_alquTot,SE_Mestim_alquDECIL),c(SE_Motros_ingTot,SE_Motros_ingDECIL))

##### COEFICIENTE DE VARIACION
c_DECIL_CV <-
  data.frame(c(CV_Ming_corTot,CV_Ming_corDECIL),c(CV_MingtrabTot,CV_MingtrabDECIL),c(CV_MtrabajoTot,CV_MtrabajoDECIL),c(CV_MnegocioTot,CV_MnegocioDECIL)
             ,c(CV_Motros_trabTot,CV_Motros_trabDECIL),c(CV_MrentasTot,CV_MrentasDECIL),c(CV_MutilidadTot,CV_MutilidadDECIL),
             c(CV_MarrendaTot,CV_MarrendaDECIL),c(CV_MtransferTot,CV_MtransferDECIL),c(CV_MjubilacionTot,CV_MjubilacionDECIL),c(CV_MbecasTot,CV_MbecasDECIL)
             ,c(CV_MdonativosTot,CV_MdonativosDECIL),c(CV_MremesasTot,CV_MremesasDECIL),c(CV_Mbene_gobTot,CV_Mbene_gobDECIL),c(CV_Mtransf_hogTot,CV_Mtransf_hogDECIL),c(CV_Mtrans_instTot,CV_Mtrans_instDECIL)
             ,c(CV_Mestim_alquTot,CV_Mestim_alquDECIL),c(CV_Motros_ingTot,CV_Motros_ingDECIL))

##### LIMITE INFERIOR AL 90%
c_DECIL_LI <-
  data.frame(c(LI_Ming_corTot,LI_Ming_corDECIL),c(LI_MingtrabTot,LI_MingtrabDECIL),c(LI_MtrabajoTot,LI_MtrabajoDECIL),
             c(LI_MnegocioTot,LI_MnegocioDECIL),c(LI_Motros_trabTot,LI_Motros_trabDECIL),c(LI_MrentasTot,LI_MrentasDECIL),c(LI_MutilidadTot,LI_MutilidadDECIL),c(LI_MarrendaTot,LI_MarrendaDECIL)
             ,c(LI_MtransferTot,LI_MtransferDECIL),c(LI_MjubilacionTot,LI_MjubilacionDECIL),c(LI_MbecasTot,LI_MbecasDECIL),c(LI_MdonativosTot,LI_MdonativosDECIL)
             ,c(LI_MremesasTot,LI_MremesasDECIL),c(LI_Mbene_gobTot,LI_Mbene_gobDECIL),c(LI_Mtransf_hogTot,LI_Mtransf_hogDECIL),c(LI_Mtrans_instTot,LI_Mtrans_instDECIL)
             ,c(LI_Mestim_alquTot,LI_Mestim_alquDECIL),c(LI_Motros_ingTot,LI_Motros_ingDECIL))

### LIMITE SUPERIOR AL 90%
c_DECIL_LS <-
  data.frame(c(LS_Ming_corTot,LS_Ming_corDECIL),c(LS_MingtrabTot,LS_MingtrabDECIL),c(LS_MtrabajoTot,LS_MtrabajoDECIL),c(LS_MnegocioTot,LS_MnegocioDECIL)
             ,c(LS_Motros_trabTot,LS_Motros_trabDECIL),c(LS_MrentasTot,LS_MrentasDECIL),c(LS_MutilidadTot,LS_MutilidadDECIL),
             c(LS_MarrendaTot,LS_MarrendaDECIL),c(LS_MtransferTot,LS_MtransferDECIL),c(LS_MjubilacionTot,LS_MjubilacionDECIL),c(LS_MbecasTot,LS_MbecasDECIL),
             c(LS_MdonativosTot,LS_MdonativosDECIL),c(LS_MremesasTot,LS_MremesasDECIL),c(LS_Mbene_gobTot,LS_Mbene_gobDECIL),c(LS_Mtransf_hogTot,LS_Mtransf_hogDECIL),c(LS_Mtrans_instTot,LS_Mtrans_instDECIL)
             ,c(LS_Mestim_alquTot,LS_Mestim_alquDECIL),c(LS_Motros_ingTot,LS_Motros_ingDECIL))

# se agregan los nombres de las entidades a las filas
#esta cadena est? bien loca, no?
DECILES<-c("PROMEDIO", "I", "II", "III","IV", "V", "VI", "VII", "VIII", "IX","X")
row.names(c_DECIL_ES)<-row.names(c_DECIL_SE)<-row.names(c_DECIL_CV)<-row.names(c_DECIL_LI)<-row.names(c_DECIL_LS)<-DECILES

#ahora vamos a ponerle nombre a las columnas
names(c_DECIL_ES)=c("ING COR2012", "TRABAJO2012", "SUBORDINADO2012", "NEGOCIOS2012","OTROS TRAB2012", "RENTAS2012","UTILIDAD2012", "ARRENDA2012", "TRANSFER2012","JUBILACION2012", "BECAS2012", "DONATIVOS2012", "REMESAS2012", "BENEGOBIERNO2012", "TRANS HOG2012", "TRANS INST2012", "ESTIM ALQU2012", "OTROS INGRESOS2012")

names(c_DECIL_SE)=c("ING COR2012", "TRABAJO2012", "SUBORDINADO2012", "NEGOCIOS2012","OTROS TRAB2012", "RENTAS2012","UTILIDAD2012", "ARRENDA2012", "TRANSFER2012","JUBILACION2012", "BECAS2012", "DONATIVOS2012", "REMESAS2012", "BENEGOBIERNO2012", "TRANS HOG2012", "TRANS INST2012", "ESTIM ALQU2012", "OTROS INGRESOS2012")

names(c_DECIL_CV)=c("ING COR2012", "TRABAJO2012", "SUBORDINADO2012", "NEGOCIOS2012","OTROS TRAB2012", "RENTAS2012","UTILIDAD2012", "ARRENDA2012", "TRANSFER2012","JUBILACION2012", "BECAS2012", "DONATIVOS2012", "REMESAS2012", "BENEGOBIERNO2012", "TRANS HOG2012", "TRANS INST2012", "ESTIM ALQU2012", "OTROS INGRESOS2012")

names(c_DECIL_LI)=c("ING COR2012", "TRABAJO2012", "SUBORDINADO2012", "NEGOCIOS2012","OTROS TRAB2012", "RENTAS2012","UTILIDAD2012", "ARRENDA2012", "TRANSFER2012","JUBILACION2012", "BECAS2012", "DONATIVOS2012", "REMESAS2012", "BENEGOBIERNO2012", "TRANS HOG2012", "TRANS INST2012", "ESTIM ALQU2012", "OTROS INGRESOS2012")

names(c_DECIL_LS)=c("ING COR", "TRABAJO", "SUBORDINADO", "NEGOCIOS","OTROS TRAB", "RENTAS","UTILIDAD", "ARRENDA", "TRANSFER","JUBILACION", "BECAS", "DONATIVOS", "REMESAS", "BENEGOBIERNO", "TRANS HOG", "TRANS INST", "ESTIM ALQU", "OTROS INGRESOS")

#ahora, lo que podemos hacer es mostrar los cuadros en la consola redondeados
# el comando round, redondea las cifra para mostrar, en el caso del coeficiente de variaci?n redondea a 4 decimales y luego multiplica por cien.
# Mostramos el resultado en pantalla
round(c_DECIL_ES)
round(c_DECIL_SE)
round(c_DECIL_CV,4)*100
round(c_DECIL_LI)
round(c_DECIL_LS)

prueba<-c_DECIL_ES%>%
  mutate(`ING COR2012`=`ING COR2012`, prueba=TRABAJO2012+RENTAS2012+JUBILACION2012+BECAS2012+DONATIVOS2012+REMESAS2012+BENEGOBIERNO2012+`TRANS HOG2012`+`TRANS INST2012`+`ESTIM ALQU2012`+`OTROS INGRESOS2012`)

all.equal(prueba$`ING COR2012`,prueba$prueba)

########## consumo por decil ###########



Consumo_por_DECIL <- svyby(~GASTO,denominator=~NHOG,by=~DECIL,mydesign,svyratio)

Consumo_promedio <- svyratio(~GASTO,denominator=~NHOG,mydesign) 

SE_consumo_Tot <- SE (Consumo_promedio)
SE_consumo_DECIL <- SE (Consumo_por_DECIL)

Consumo_por_DECIL <- Consumo_por_DECIL[[2]] 
Consumo_promedio <- Consumo_promedio[[1]]

Consumo<-data.frame(c(Consumo_promedio,Consumo_por_DECIL))

DECILES<-c("PROMEDIO", "I", "II", "III","IV", "V", "VI", "VII", "VIII", "IX","X")
row.names(Consumo)<-DECILES

Consumo_SE<-data.frame(c(SE_consumo_Tot,SE_consumo_DECIL))
row.names(Consumo_SE)<-DECILES





write.dbf(Consumo,file = "Nacional Consumo  por DECIL 2012.dbf")
write.dbf(Consumo_SE,file="Nacional Consumo  por DECIL 2012 SE.dbf")
write.dbf(c_DECIL_ES,file = "Nacional por fuente por DECIL estimaciones 2012.dbf")
write.dbf(c_DECIL_SE,file = "Nacional por fuente por DECIL errores standard 2012.dbf")
write.dbf(c_DECIL_CV,file = "Nacional por fuente por DECIL CV 2012.dbf")
write.dbf(c_DECIL_LI,file = "Nacional por fuente por DECIL LI 2012.dbf")
write.dbf(c_DECIL_ES,file = "Nacional por fuente por DECIL LS 2012.dbf")

rm(list = ls())




################## Bottom 40 por ingreso #############
library(foreign)
library(survey)
library(doBy)
library(reldist)
library(tidyverse)
options(survey.lonely.psu="adjust")


setwd("C:/Users/Erick/onedrive/GIC/GITHUB2018/GIC/ENIGH_2012/ENIGH2012/")
Conc2012<-read.dbf("Conc_2012.dbf",as.is = T)


names(Conc2012)<-c("ENTIDAD","FOLIOVIV","FOLIOHOG","GASTO","TOT_INTEG","INGCOR","INGTRAB","TRABAJO","NEGOCIO","OTROS_TRAB",
                   "RENTAS","UTILIDAD","ARRENDA","TRANSFER","JUBILA","BECA","DONATIVO","REMESA","BENE_GOB",
                   "ESP_HOG","ESP_INST","ESTI","OTROS","FACTOR","UPM","EST_DIS","tam_localidad","Small","HOGARINDIG","NOMBRE_ENT",
                   "DEFLACTORES","Nhog","TAM_DECIL","MAXT","ACUMULA","ACUMULA2","DECIL","Bottom_40")

mydesign <- svydesign(id=~UPM,strata=~EST_DIS,data=Conc2012,weights=~FACTOR)

#vamos por el ingreso corriente total del pa?s
# ing_ cor se define como La suma de las variables ingtrab, rentas, transfer, estim_alqu y otros_ing.
#te sale que el ingreso trimestra promedio en Mexico es de 49,610.
#notes? que esto no es otra cosa que el ing_cor*factor/34744819
Ming_corTot <- svyratio(~INGCOR,denominator=~Nhog,mydesign) 

#ahora, vamos a hacer lo mismo por Bottom_40
#aqu? cmabia la funci?n a svyby, en by va el Bottom_40 que creamos.
#y al final va la funci?n que queremos
Ming_corBottom_40 <- svyby(~INGCOR,denominator=~Nhog,by=~Bottom_40,mydesign,svyratio)


#     Trabajo
#
#El trabajo se divide en tres clasificaciones: subordinado, independiente y otros.
### ingreso del trabajo total###
MingtrabTot <- svyratio(~INGTRAB,denominator=~Nhog,mydesign) # Total promedio
MingtrabBottom_40 <- svyby(~INGTRAB,denominator=~Nhog,by=~Bottom_40 ,mydesign,svyratio) # por Bottom_40
###### ingreso del trabajo subordinado
MtrabajoTot <- svyratio(~TRABAJO,denominator=~Nhog,mydesign) # Total promedio
MtrabajoBottom_40 <- svyby(~TRABAJO,denominator=~Nhog,by=~Bottom_40 ,mydesign,svyratio) # por Bottom_40
###### ingreso del trabajo independiente
MnegocioTot <- svyratio(~NEGOCIO,denominator=~Nhog,mydesign) # Total promedio
MnegocioBottom_40 <- svyby(~NEGOCIO,denominator=~Nhog,by=~Bottom_40 ,mydesign,svyratio) # por Bottom_40
###### ingreso de otros trabajos
Motros_trabTot <- svyratio(~OTROS_TRAB,denominator=~Nhog,mydesign) # Total promedio
Motros_trabBottom_40<- svyby(~OTROS_TRAB,denominator=~Nhog,by=~Bottom_40,mydesign,svyratio) # por Bottom_40


###################################        Rentas de la propiedad 

#la renta de la propiedad se divide en: ingresos de sociedades y arrendamientos.

#ingresos totales por renta de la porpiedad
MrentasTot <- svyratio(~RENTAS,denominator=~Nhog,mydesign) # Total promedio
MrentasBottom_40 <- svyby(~RENTAS,denominator=~Nhog,by=~Bottom_40 ,mydesign,svyratio) #Por Bottom_40
###### ingresos de sociedades
MutilidadTot <- svyratio(~UTILIDAD,denominator=~Nhog,mydesign) # Total promedio
MutilidadBottom_40 <- svyby(~UTILIDAD,denominator=~Nhog,by=~Bottom_40 ,mydesign,svyratio) # por Bottom_40
###### arrendamiento
MarrendaTot <- svyratio(~ARRENDA,denominator=~Nhog,mydesign) # Total promedio
MarrendaBottom_40 <- svyby(~ARRENDA,denominator=~Nhog,by=~Bottom_40 ,mydesign,svyratio) # Por Bottom_40


###################################        Transferencias   

#las transferencias totales se definen como la suma de jubilacion, becas, donativos, remesas, bene_gob, transf_hog y trans_inst.

MtransferTot <- svyratio(~TRANSFER,denominator=~Nhog,mydesign) # Total promedio
MtransferBottom_40 <- svyby(~TRANSFER,denominator=~Nhog,by=~Bottom_40 ,mydesign,svyratio) # Bottom_40

###### jubilacion se define como Jubilaciones, pensiones e indemnizaciones por accidente de trabajo despido y retiro voluntario.
#En el cuestionario solo se les pregunta si recibi? jubilaciones. As? que puede ser p?blicas o privadas.

MjubilacionTot <- svyratio(~JUBILA,denominator=~Nhog,mydesign) # Total promedio
MjubilacionBottom_40 <- svyby(~JUBILA,denominator=~Nhog,by=~Bottom_40 ,mydesign,svyratio) # Bottom_40

###### becas que pueden ser, de nuevo, p?blicas privadas. 
MbecasTot <- svyratio(~BECA,denominator=~Nhog,mydesign) # Total promedio
MbecasBottom_40 <- svyby(~BECA,denominator=~Nhog,by=~Bottom_40 ,mydesign,svyratio) # Bottom_40

###### donativos que tambi?n pueden ser p?blicos o privados.
MdonativosTot <- svyratio(~DONATIVO,denominator=~Nhog,mydesign) # Total promedio
MdonativosBottom_40 <- svyby(~DONATIVO,denominator=~Nhog,by=~Bottom_40 ,mydesign,svyratio) # Bottom_40

###### remesas se definen como ingresos provenientes d eotros paises. As? de manera gen?rica.
MremesasTot <- svyratio(~REMESA,denominator=~Nhog,mydesign) # Total promedio
MremesasBottom_40 <- svyby(~REMESA,denominator=~Nhog,by=~Bottom_40 ,mydesign,svyratio) # Bottom_40

###### bene_gob:  aqu? estna los programas p?blicos. Prospera, procampo, 65 y m?s, adultos mayores, sin hambre, empleo tempora y Otros.
Mbene_gobTot <- svyratio(~BENE_GOB,denominator=~Nhog,mydesign) # Total promedio
Mbene_gobBottom_40 <- svyby(~BENE_GOB,denominator=~Nhog,by=~Bottom_40 ,mydesign,svyratio) # Bottom_40

###### transf_hog:  Esto es lo que transfiere otro hogar.
Mtransf_hogTot <- svyratio(~ESP_HOG,denominator=~Nhog,mydesign) # Total promedio
Mtransf_hogBottom_40 <- svyby(~ESP_HOG,denominator=~Nhog,by=~Bottom_40 ,mydesign,svyratio) #Bottom_40

###### trans_inst: puede venir de institucione sp?blicas o privadas.
Mtrans_instTot <- svyratio(~ESP_INST,denominator=~Nhog,mydesign) # Total promedio
Mtrans_instBottom_40 <- svyby(~ESP_INST,denominator=~Nhog,by=~Bottom_40 ,mydesign,svyratio) # Bottom_40


### estim_alqu ### Aparentemente se le pregunta al entrevistado cu?nto constar?a la renta del lugar donde vive.
Mestim_alquTot <- svyratio(~ESTI,denominator=~Nhog,mydesign) # Total promedio
Mestim_alquBottom_40 <- svyby(~ESTI,denominator=~Nhog,by=~Bottom_40 ,mydesign,svyratio) # Bottom_40


### otros_ing ### es literalmente ?algo m?s?
Motros_ingTot <- svyratio(~OTROS,denominator=~Nhog,mydesign) # Total promedio
Motros_ingBottom_40 <- svyby(~OTROS,denominator=~Nhog,by=~Bottom_40 ,mydesign,svyratio) # Bottom_40


######################################### Estimaciones 

ES_Ming_corTot <- Ming_corTot[[1]] #lo que estoy haciendo aqu? es extraer el valor de la primera columa que corresponde al c?lculo.
ES_Ming_corBottom_40 <- Ming_corBottom_40[[2]] #En el caso de las entidades, los c?lculos quedaron en la segunda columna

ES_MingtrabTot <- MingtrabTot[[1]]
ES_MingtrabBottom_40 <- MingtrabBottom_40[[2]]

ES_MtrabajoTot <- MtrabajoTot[[1]]
ES_MtrabajoBottom_40 <- MtrabajoBottom_40[[2]]

ES_MnegocioTot <- MnegocioTot[[1]]
ES_MnegocioBottom_40 <- MnegocioBottom_40[[2]]

ES_Motros_trabTot <- Motros_trabTot [[1]]
ES_Motros_trabBottom_40 <- Motros_trabBottom_40 [[2]]

ES_MrentasTot <- MrentasTot [[1]]
ES_MrentasBottom_40 <- MrentasBottom_40 [[2]]

ES_MutilidadTot <- MutilidadTot [[1]]
ES_MutilidadBottom_40 <- MutilidadBottom_40 [[2]]

ES_MarrendaTot <- MarrendaTot [[1]]
ES_MarrendaBottom_40 <- MarrendaBottom_40 [[2]]

ES_MtransferTot <- MtransferTot[[1]]
ES_MtransferBottom_40 <- MtransferBottom_40[[2]]

ES_MjubilacionTot <- MjubilacionTot [[1]]
ES_MjubilacionBottom_40 <- MjubilacionBottom_40 [[2]]

ES_MbecasTot <- MbecasTot [[1]]
ES_MbecasBottom_40 <- MbecasBottom_40 [[2]]

ES_MdonativosTot <- MdonativosTot[[1]]
ES_MdonativosBottom_40 <- MdonativosBottom_40[[2]]

ES_MremesasTot <- MremesasTot[[1]]
ES_MremesasBottom_40 <- MremesasBottom_40[[2]]

ES_Mbene_gobTot <- Mbene_gobTot [[1]]
ES_Mbene_gobBottom_40 <- Mbene_gobBottom_40 [[2]]

ES_Mtransf_hogTot <- Mtransf_hogTot [[1]]
ES_Mtransf_hogBottom_40 <- Mtransf_hogBottom_40 [[2]]

ES_Mtrans_instTot <- Mtrans_instTot[[1]]
ES_Mtrans_instBottom_40 <- Mtrans_instBottom_40[[2]]

ES_Mestim_alquTot <- Mestim_alquTot [[1]]
ES_Mestim_alquBottom_40 <- Mestim_alquBottom_40 [[2]]

ES_Motros_ingTot <- Motros_ingTot [[1]]
ES_Motros_ingBottom_40 <- Motros_ingBottom_40 [[2]]

########## Error Est?ndar 
SE_Ming_corTot <- SE (Ming_corTot)
SE_Ming_corBottom_40 <- SE (Ming_corBottom_40)

SE_MingtrabTot <- SE (MingtrabTot)
SE_MingtrabBottom_40 <- SE (MingtrabBottom_40)

SE_MtrabajoTot <- SE (MtrabajoTot)
SE_MtrabajoBottom_40 <- SE (MtrabajoBottom_40)

SE_MnegocioTot <- SE (MnegocioTot)
SE_MnegocioBottom_40 <- SE (MnegocioBottom_40)

SE_Motros_trabTot <- SE (Motros_trabTot)
SE_Motros_trabBottom_40 <- SE (Motros_trabBottom_40)

SE_MrentasTot <- SE (MrentasTot)
SE_MrentasBottom_40 <- SE (MrentasBottom_40)

SE_MutilidadTot <- SE (MutilidadTot)
SE_MutilidadBottom_40 <- SE (MutilidadBottom_40)

SE_MarrendaTot <- SE (MarrendaTot)
SE_MarrendaBottom_40 <- SE (MarrendaBottom_40)

SE_MtransferTot <- SE (MtransferTot)
SE_MtransferBottom_40 <- SE (MtransferBottom_40)

SE_MjubilacionTot <- SE (MjubilacionTot)
SE_MjubilacionBottom_40 <- SE (MjubilacionBottom_40)

SE_MbecasTot <- SE (MbecasTot)
SE_MbecasBottom_40 <- SE (MbecasBottom_40)

SE_MdonativosTot <- SE (MdonativosTot)
SE_MdonativosBottom_40 <- SE (MdonativosBottom_40)

SE_MremesasTot <- SE (MremesasTot)
SE_MremesasBottom_40 <- SE (MremesasBottom_40)

SE_Mbene_gobTot <- SE (Mbene_gobTot)
SE_Mbene_gobBottom_40 <- SE (Mbene_gobBottom_40)

SE_Mtransf_hogTot <- SE (Mtransf_hogTot)
SE_Mtransf_hogBottom_40 <- SE (Mtransf_hogBottom_40)

SE_Mtrans_instTot <- SE (Mtrans_instTot)
SE_Mtrans_instBottom_40 <- SE (Mtrans_instBottom_40)

SE_Mestim_alquTot <- SE (Mestim_alquTot)
SE_Mestim_alquBottom_40 <- SE (Mestim_alquBottom_40)

SE_Motros_ingTot <- SE (Motros_ingTot)
SE_Motros_ingBottom_40 <- SE (Motros_ingBottom_40)

########## Coeficiente de variaci?n 
CV_Ming_corTot <- cv(Ming_corTot)
CV_Ming_corBottom_40 <- cv(Ming_corBottom_40)

CV_MingtrabTot <- cv(MingtrabTot)
CV_MingtrabBottom_40 <- cv(MingtrabBottom_40)

CV_MtrabajoTot <- cv(MtrabajoTot)
CV_MtrabajoBottom_40 <- cv(MtrabajoBottom_40)

CV_MnegocioTot <- cv(MnegocioTot)
CV_MnegocioBottom_40 <- cv(MnegocioBottom_40)

CV_Motros_trabTot <- cv(Motros_trabTot)
CV_Motros_trabBottom_40 <- cv(Motros_trabBottom_40)

CV_MrentasTot <- cv(MrentasTot)
CV_MrentasBottom_40 <- cv(MrentasBottom_40)

CV_MutilidadTot <- cv(MutilidadTot)
CV_MutilidadBottom_40 <- cv(MutilidadBottom_40)

CV_MarrendaTot <- cv(MarrendaTot)
CV_MarrendaBottom_40 <- cv(MarrendaBottom_40)

CV_MtransferTot <- cv(MtransferTot)
CV_MtransferBottom_40 <- cv(MtransferBottom_40)

CV_MjubilacionTot <- cv(MjubilacionTot)
CV_MjubilacionBottom_40 <- cv(MjubilacionBottom_40)

CV_MbecasTot <- cv(MbecasTot)
CV_MbecasBottom_40 <- cv(MbecasBottom_40)

CV_MdonativosTot <- cv(MdonativosTot)
CV_MdonativosBottom_40 <- cv(MdonativosBottom_40)

CV_MremesasTot <- cv(MremesasTot)
CV_MremesasBottom_40 <- cv(MremesasBottom_40)

CV_Mbene_gobTot <- cv(Mbene_gobTot)
CV_Mbene_gobBottom_40 <- cv(Mbene_gobBottom_40)

CV_Mtransf_hogTot <- cv(Mtransf_hogTot)
CV_Mtransf_hogBottom_40 <- cv(Mtransf_hogBottom_40)

CV_Mtrans_instTot <- cv(Mtrans_instTot)
CV_Mtrans_instBottom_40 <- cv(Mtrans_instBottom_40)

CV_Mestim_alquTot <- cv(Mestim_alquTot)
CV_Mestim_alquBottom_40 <- cv(Mestim_alquBottom_40)

CV_Motros_ingTot <- cv(Motros_ingTot)
CV_Motros_ingBottom_40 <- cv(Motros_ingBottom_40)

########## Limite inferior 
LI_Ming_corTot <- confint(Ming_corTot,level=0.90)[,1]
LI_Ming_corBottom_40 <- confint(Ming_corBottom_40,level=0.90)[,1]

LI_MingtrabTot <- confint(MingtrabTot,level=0.90)[,1]
LI_MingtrabBottom_40 <- confint(MingtrabBottom_40,level=0.90)[,1]

LI_MtrabajoTot <- confint(MtrabajoTot,level=0.90)[,1]
LI_MtrabajoBottom_40 <- confint(MtrabajoBottom_40,level=0.90)[,1]

LI_MnegocioTot <- confint(MnegocioTot,level=0.90)[,1]
LI_MnegocioBottom_40 <- confint(MnegocioBottom_40,level=0.90)[,1]

LI_Motros_trabTot <- confint(Motros_trabTot,level=0.90)[,1]
LI_Motros_trabBottom_40 <- confint(Motros_trabBottom_40,level=0.90)[,1]

LI_MrentasTot <- confint(MrentasTot,level=0.90)[,1]
LI_MrentasBottom_40 <- confint(MrentasBottom_40,level=0.90)[,1]

LI_MutilidadTot <- confint(MutilidadTot,level=0.90)[,1]
LI_MutilidadBottom_40 <- confint(MutilidadBottom_40,level=0.90)[,1]

LI_MarrendaTot <- confint(MarrendaTot,level=0.90)[,1]
LI_MarrendaBottom_40 <- confint(MarrendaBottom_40,level=0.90)[,1]

LI_MtransferTot <- confint(MtransferTot,level=0.90)[,1]
LI_MtransferBottom_40 <- confint(MtransferBottom_40,level=0.90)[,1]

LI_MjubilacionTot <- confint(MjubilacionTot,level=0.90)[,1]
LI_MjubilacionBottom_40 <- confint(MjubilacionBottom_40,level=0.90)[,1]

LI_MbecasTot <- confint(MbecasTot,level=0.90)[,1]
LI_MbecasBottom_40 <- confint(MbecasBottom_40,level=0.90)[,1]

LI_MdonativosTot <- confint(MdonativosTot,level=0.90)[,1]
LI_MdonativosBottom_40 <- confint(MdonativosBottom_40,level=0.90)[,1]

LI_MremesasTot <- confint(MremesasTot,level=0.90)[,1]
LI_MremesasBottom_40 <- confint(MremesasBottom_40,level=0.90)[,1]

LI_Mbene_gobTot <- confint(Mbene_gobTot,level=0.90)[,1]
LI_Mbene_gobBottom_40 <- confint(Mbene_gobBottom_40,level=0.90)[,1]

LI_Mtransf_hogTot <- confint(Mtransf_hogTot,level=0.90)[,1]
LI_Mtransf_hogBottom_40 <- confint(Mtransf_hogBottom_40,level=0.90)[,1]

LI_Mtrans_instTot <- confint(Mtrans_instTot,level=0.90)[,1]
LI_Mtrans_instBottom_40 <- confint(Mtrans_instBottom_40,level=0.90)[,1]

LI_Mestim_alquTot <- confint(Mestim_alquTot,level=0.90)[,1]
LI_Mestim_alquBottom_40 <- confint(Mestim_alquBottom_40,level=0.90)[,1]

LI_Motros_ingTot <- confint(Motros_ingTot,level=0.90)[,1]
LI_Motros_ingBottom_40 <- confint(Motros_ingBottom_40 ,level=0.90)[,1]

########## Limite superior 
LS_Ming_corTot <- confint(Ming_corTot,level=0.90)[,2]
LS_Ming_corBottom_40 <- confint(Ming_corBottom_40,level=0.90)[,2]

LS_MingtrabTot <- confint(MingtrabTot,level=0.90)[,2]
LS_MingtrabBottom_40 <- confint(MingtrabBottom_40,level=0.90)[,2]

LS_MtrabajoTot <- confint(MtrabajoTot,level=0.90)[,2]
LS_MtrabajoBottom_40 <- confint(MtrabajoBottom_40,level=0.90)[,2]

LS_MnegocioTot <- confint(MnegocioTot,level=0.90)[,2]
LS_MnegocioBottom_40 <- confint(MnegocioBottom_40,level=0.90)[,2]

LS_Motros_trabTot <- confint(Motros_trabTot,level=0.90)[,2]
LS_Motros_trabBottom_40 <- confint(Motros_trabBottom_40,level=0.90)[,2]

LS_MrentasTot <- confint(MrentasTot,level=0.90)[,2]
LS_MrentasBottom_40 <- confint(MrentasBottom_40,level=0.90)[,2]

LS_MutilidadTot <- confint(MutilidadTot,level=0.90)[,2]
LS_MutilidadBottom_40 <- confint(MutilidadBottom_40,level=0.90)[,2]

LS_MarrendaTot <- confint(MarrendaTot,level=0.90)[,2]
LS_MarrendaBottom_40 <- confint(MarrendaBottom_40,level=0.90)[,2]

LS_MtransferTot <- confint(MtransferTot,level=0.90)[,2]
LS_MtransferBottom_40 <- confint(MtransferBottom_40,level=0.90)[,2]

LS_MjubilacionTot <- confint(MjubilacionTot,level=0.90)[,2]
LS_MjubilacionBottom_40 <- confint(MjubilacionBottom_40,level=0.90)[,2]

LS_MbecasTot <- confint(MbecasTot,level=0.90)[,2]
LS_MbecasBottom_40 <- confint(MbecasBottom_40,level=0.90)[,2]

LS_MdonativosTot <- confint(MdonativosTot,level=0.90)[,2]
LS_MdonativosBottom_40 <- confint(MdonativosBottom_40,level=0.90)[,2]

LS_MremesasTot <- confint(MremesasTot,level=0.90)[,2]
LS_MremesasBottom_40 <- confint(MremesasBottom_40,level=0.90)[,2]

LS_Mbene_gobTot <- confint(Mbene_gobTot,level=0.90)[,2]
LS_Mbene_gobBottom_40 <- confint(Mbene_gobBottom_40,level=0.90)[,2]

LS_Mtransf_hogTot <- confint(Mtransf_hogTot,level=0.90)[,2]
LS_Mtransf_hogBottom_40 <- confint(Mtransf_hogBottom_40,level=0.90)[,2]

LS_Mtrans_instTot <- confint(Mtrans_instTot,level=0.90)[,2]
LS_Mtrans_instBottom_40 <- confint(Mtrans_instBottom_40,level=0.90)[,2]

LS_Mestim_alquTot <- confint(Mestim_alquTot,level=0.90)[,2]
LS_Mestim_alquBottom_40 <- confint(Mestim_alquBottom_40,level=0.90)[,2]

LS_Motros_ingTot <- confint(Motros_ingTot,level=0.90)[,2]
LS_Motros_ingBottom_40 <- confint(Motros_ingBottom_40,level=0.90)[,2]

#############################      Cuadros   
#este cuadro, lo ?nico que tiene son todas la estimaciones.
#son 10 filas y 18 columnas.
c_Bottom_40_ES <-
  data.frame(c(ES_Ming_corTot,ES_Ming_corBottom_40),c(ES_MingtrabTot,ES_MingtrabBottom_40),c(ES_MtrabajoTot,ES_MtrabajoBottom_40),c(ES_MnegocioTot,ES_MnegocioBottom_40)
             ,c(ES_Motros_trabTot,ES_Motros_trabBottom_40),c(ES_MrentasTot,ES_MrentasBottom_40),c(ES_MutilidadTot,ES_MutilidadBottom_40)
             ,c(ES_MarrendaTot,ES_MarrendaBottom_40),c(ES_MtransferTot,ES_MtransferBottom_40),c(ES_MjubilacionTot,ES_MjubilacionBottom_40),c(ES_MbecasTot,ES_MbecasBottom_40),
             c(ES_MdonativosTot,ES_MdonativosBottom_40),c(ES_MremesasTot,ES_MremesasBottom_40),c(ES_Mbene_gobTot,ES_Mbene_gobBottom_40),c(ES_Mtransf_hogTot,ES_Mtransf_hogBottom_40)
             ,c(ES_Mtrans_instTot,ES_Mtrans_instBottom_40),c(ES_Mestim_alquTot,ES_Mestim_alquBottom_40),c(ES_Motros_ingTot,ES_Motros_ingBottom_40))
##### ERROR ESTANDAR
c_Bottom_40_SE <-
  data.frame(c(SE_Ming_corTot,SE_Ming_corBottom_40),c(SE_MingtrabTot,SE_MingtrabBottom_40),c(SE_MtrabajoTot,SE_MtrabajoBottom_40),c(SE_MnegocioTot,SE_MnegocioBottom_40)
             ,c(SE_Motros_trabTot,SE_Motros_trabBottom_40),c(SE_MrentasTot,SE_MrentasBottom_40),c(SE_MutilidadTot,SE_MutilidadBottom_40)
             ,c(SE_MarrendaTot,SE_MarrendaBottom_40),c(SE_MtransferTot,SE_MtransferBottom_40),c(SE_MjubilacionTot,SE_MjubilacionBottom_40),c(SE_MbecasTot,SE_MbecasBottom_40),
             c(SE_MdonativosTot,SE_MdonativosBottom_40),c(SE_MremesasTot,SE_MremesasBottom_40),c(SE_Mbene_gobTot,SE_Mbene_gobBottom_40),c(SE_Mtransf_hogTot,SE_Mtransf_hogBottom_40),c(SE_Mtrans_instTot,SE_Mtrans_instBottom_40)
             ,c(SE_Mestim_alquTot,SE_Mestim_alquBottom_40),c(SE_Motros_ingTot,SE_Motros_ingBottom_40))

##### COEFICIENTE DE VARIACION
c_Bottom_40_CV <-
  data.frame(c(CV_Ming_corTot,CV_Ming_corBottom_40),c(CV_MingtrabTot,CV_MingtrabBottom_40),c(CV_MtrabajoTot,CV_MtrabajoBottom_40),c(CV_MnegocioTot,CV_MnegocioBottom_40)
             ,c(CV_Motros_trabTot,CV_Motros_trabBottom_40),c(CV_MrentasTot,CV_MrentasBottom_40),c(CV_MutilidadTot,CV_MutilidadBottom_40),
             c(CV_MarrendaTot,CV_MarrendaBottom_40),c(CV_MtransferTot,CV_MtransferBottom_40),c(CV_MjubilacionTot,CV_MjubilacionBottom_40),c(CV_MbecasTot,CV_MbecasBottom_40)
             ,c(CV_MdonativosTot,CV_MdonativosBottom_40),c(CV_MremesasTot,CV_MremesasBottom_40),c(CV_Mbene_gobTot,CV_Mbene_gobBottom_40),c(CV_Mtransf_hogTot,CV_Mtransf_hogBottom_40),c(CV_Mtrans_instTot,CV_Mtrans_instBottom_40)
             ,c(CV_Mestim_alquTot,CV_Mestim_alquBottom_40),c(CV_Motros_ingTot,CV_Motros_ingBottom_40))

##### LIMITE INFERIOR AL 90%
c_Bottom_40_LI <-
  data.frame(c(LI_Ming_corTot,LI_Ming_corBottom_40),c(LI_MingtrabTot,LI_MingtrabBottom_40),c(LI_MtrabajoTot,LI_MtrabajoBottom_40),
             c(LI_MnegocioTot,LI_MnegocioBottom_40),c(LI_Motros_trabTot,LI_Motros_trabBottom_40),c(LI_MrentasTot,LI_MrentasBottom_40),c(LI_MutilidadTot,LI_MutilidadBottom_40),c(LI_MarrendaTot,LI_MarrendaBottom_40)
             ,c(LI_MtransferTot,LI_MtransferBottom_40),c(LI_MjubilacionTot,LI_MjubilacionBottom_40),c(LI_MbecasTot,LI_MbecasBottom_40),c(LI_MdonativosTot,LI_MdonativosBottom_40)
             ,c(LI_MremesasTot,LI_MremesasBottom_40),c(LI_Mbene_gobTot,LI_Mbene_gobBottom_40),c(LI_Mtransf_hogTot,LI_Mtransf_hogBottom_40),c(LI_Mtrans_instTot,LI_Mtrans_instBottom_40)
             ,c(LI_Mestim_alquTot,LI_Mestim_alquBottom_40),c(LI_Motros_ingTot,LI_Motros_ingBottom_40))

### LIMITE SUPERIOR AL 90%
c_Bottom_40_LS <-
  data.frame(c(LS_Ming_corTot,LS_Ming_corBottom_40),c(LS_MingtrabTot,LS_MingtrabBottom_40),c(LS_MtrabajoTot,LS_MtrabajoBottom_40),c(LS_MnegocioTot,LS_MnegocioBottom_40)
             ,c(LS_Motros_trabTot,LS_Motros_trabBottom_40),c(LS_MrentasTot,LS_MrentasBottom_40),c(LS_MutilidadTot,LS_MutilidadBottom_40),
             c(LS_MarrendaTot,LS_MarrendaBottom_40),c(LS_MtransferTot,LS_MtransferBottom_40),c(LS_MjubilacionTot,LS_MjubilacionBottom_40),c(LS_MbecasTot,LS_MbecasBottom_40),
             c(LS_MdonativosTot,LS_MdonativosBottom_40),c(LS_MremesasTot,LS_MremesasBottom_40),c(LS_Mbene_gobTot,LS_Mbene_gobBottom_40),c(LS_Mtransf_hogTot,LS_Mtransf_hogBottom_40),c(LS_Mtrans_instTot,LS_Mtrans_instBottom_40)
             ,c(LS_Mestim_alquTot,LS_Mestim_alquBottom_40),c(LS_Motros_ingTot,LS_Motros_ingBottom_40))

# se agregan los nombres de las entidades a las filas
#esta cadena est? bien loca, no?
Bottom_40ES<-c("PROMEDIO", "Upper60","Bottom40")
row.names(c_Bottom_40_ES)<-row.names(c_Bottom_40_SE)<-row.names(c_Bottom_40_CV)<-row.names(c_Bottom_40_LI)<-row.names(c_Bottom_40_LS)<-Bottom_40ES

#ahora vamos a ponerle nombre a las columnas
names(c_Bottom_40_ES)=c("ING COR2012", "TRABAJO2012", "SUBORDINADO2012", "NEGOCIOS2012","OTROS TRAB2012", "RENTAS2012","UTILIDAD2012", "ARRENDA2012", "TRANSFER2012","JUBILACION2012", "BECAS2012", "DONATIVOS2012", "REMESAS2012", "BENEGOBIERNO2012", "TRANS HOG2012", "TRANS INST2012", "ESTIM ALQU2012", "OTROS INGRESOS2012")

names(c_Bottom_40_SE)=c("ING COR2012", "TRABAJO2012", "SUBORDINADO2012", "NEGOCIOS2012","OTROS TRAB2012", "RENTAS2012","UTILIDAD2012", "ARRENDA2012", "TRANSFER2012","JUBILACION2012", "BECAS2012", "DONATIVOS2012", "REMESAS2012", "BENEGOBIERNO2012", "TRANS HOG2012", "TRANS INST2012", "ESTIM ALQU2012", "OTROS INGRESOS2012")

names(c_Bottom_40_CV)=c("ING COR2012", "TRABAJO2012", "SUBORDINADO2012", "NEGOCIOS2012","OTROS TRAB2012", "RENTAS2012","UTILIDAD2012", "ARRENDA2012", "TRANSFER2012","JUBILACION2012", "BECAS2012", "DONATIVOS2012", "REMESAS2012", "BENEGOBIERNO2012", "TRANS HOG2012", "TRANS INST2012", "ESTIM ALQU2012", "OTROS INGRESOS2012")

names(c_Bottom_40_LI)=c("ING COR2012", "TRABAJO2012", "SUBORDINADO2012", "NEGOCIOS2012","OTROS TRAB2012", "RENTAS2012","UTILIDAD2012", "ARRENDA2012", "TRANSFER2012","JUBILACION2012", "BECAS2012", "DONATIVOS2012", "REMESAS2012", "BENEGOBIERNO2012", "TRANS HOG2012", "TRANS INST2012", "ESTIM ALQU2012", "OTROS INGRESOS2012")

names(c_Bottom_40_LS)=c("ING COR", "TRABAJO", "SUBORDINADO", "NEGOCIOS","OTROS TRAB", "RENTAS","UTILIDAD", "ARRENDA", "TRANSFER","JUBILACION", "BECAS", "DONATIVOS", "REMESAS", "BENEGOBIERNO", "TRANS HOG", "TRANS INST", "ESTIM ALQU", "OTROS INGRESOS")

#ahora, lo que podemos hacer es mostrar los cuadros en la consola redondeados
# el comando round, redondea las cifra para mostrar, en el caso del coeficiente de variaci?n redondea a 4 decimales y luego multiplica por cien.
# Mostramos el resultado en pantalla
round(c_Bottom_40_ES)
round(c_Bottom_40_SE)
round(c_Bottom_40_CV,4)*100
round(c_Bottom_40_LI)
round(c_Bottom_40_LS)

prueba<-c_Bottom_40_ES%>%
  mutate(`ING COR2012`=`ING COR2012`, prueba=TRABAJO2012+RENTAS2012+JUBILACION2012+BECAS2012+DONATIVOS2012+REMESAS2012+BENEGOBIERNO2012+`TRANS HOG2012`+`TRANS INST2012`+`ESTIM ALQU2012`+`OTROS INGRESOS2012`)

all.equal(prueba$`ING COR2012`,prueba$prueba)

########## consumo por bottom ###########



Consumo_por_Bottom_40 <- svyby(~GASTO,denominator=~Nhog,by=~Bottom_40,mydesign,svyratio)

Consumo_promedio <- svyratio(~GASTO,denominator=~Nhog,mydesign) 

SE_consumo_Tot <- SE (Consumo_promedio)
SE_consumo_Bottom_40 <- SE (Consumo_por_Bottom_40)

Consumo_por_Bottom_40 <- Consumo_por_Bottom_40[[2]] 
Consumo_promedio <- Consumo_promedio[[1]]

Consumo<-data.frame(c(Consumo_promedio,Consumo_por_Bottom_40))

Bottom_40ES<-c("PROMEDIO", "Upper60","Bottom40")
row.names(Consumo)<-Bottom_40ES

Consumo_SE<-data.frame(c(SE_consumo_Tot,SE_consumo_Bottom_40))
row.names(Consumo_SE)<-Bottom_40ES





write.dbf(Consumo,file = "Nacional Consumo  por Bottom_40 2012.dbf")
write.dbf(Consumo_SE,file="Nacional Consumo  por Bottom_40 2012 SE.dbf")
write.dbf(c_Bottom_40_ES,file = "Nacional por fuente por Bottom_40 estimaciones 2012.dbf")
write.dbf(c_Bottom_40_SE,file = "Nacional por fuente por Bottom_40 errores standard 2012.dbf")
write.dbf(c_Bottom_40_CV,file = "Nacional por fuente por Bottom_40 CV 2012.dbf")
write.dbf(c_Bottom_40_LI,file = "Nacional por fuente por Bottom_40 LI 2012.dbf")
write.dbf(c_Bottom_40_ES,file = "Nacional por fuente por Bottom_40 LS 2012.dbf")

rm(list = ls())











################# ingreso por estado MEDIA ######################
library(foreign)
library(survey)
library(doBy)
library(reldist)
library(tidyverse)
options(survey.lonely.psu="adjust")

#reading the data
setwd("C:/Users/Erick/onedrive/GIC/GITHUB2018/GIC/ENIGH_2012/ENIGH2012")
Conc2012<-read.dbf("Conc_2012.dbf",as.is = T)


names(Conc2012)<-c("ENTIDAD","FOLIOVIV","FOLIOHOG","GASTO","TOT_INTEG","INGCOR","INGTRAB","TRABAJO","NEGOCIO","OTROS_TRAB",
                   "RENTAS","UTILIDAD","ARRENDA","TRANSFER","JUBILA","BECA","DONATIVO","REMESA","BENE_GOB",
                   "ESP_HOG","ESP_INST","ESTI","OTROS","FACTOR","UPM","EST_DIS","tam_localidad","Small","HOGARINDIG","NOMBRE_ENT",
                   "DEFLACTORES","Nhog","TAM_DECIL","MAXT","ACUMULA","ACUMULA2","DECIL","Bottom_40")

mydesign <- svydesign(id=~UPM,strata=~EST_DIS,data=Conc2012,weights=~FACTOR)

#vamos por el ingreso corriente total del pa?s
# ing_ cor se define como La suma de las variables ingtrab, rentas, transfer, estim_alqu y otros_ing.
#te sale que el ingreso trimestra promedio en Mexico es de 49,610.
#notes? que esto no es otra cosa que el ing_cor*factor/34744819
Ming_corTot <- svyratio(~INGCOR,denominator=~Nhog,mydesign) 

#ahora, vamos a hacer lo mismo por ENTIDAD
#aqu? cmabia la funci?n a svyby, en by va el ENTIDAD que creamos.
#y al final va la funci?n que queremos
Ming_corENTIDAD <- svyby(~INGCOR,denominator=~Nhog,by=~ENTIDAD,mydesign,svyratio)


#     Trabajo
#
#El trabajo se divide en tres clasificaciones: subordinado, independiente y otros.
### ingreso del trabajo total###
MingtrabTot <- svyratio(~INGTRAB,denominator=~Nhog,mydesign) # Total promedio
MingtrabENTIDAD <- svyby(~INGTRAB,denominator=~Nhog,by=~ENTIDAD ,mydesign,svyratio) # por ENTIDAD
###### ingreso del trabajo subordinado
MtrabajoTot <- svyratio(~TRABAJO,denominator=~Nhog,mydesign) # Total promedio
MtrabajoENTIDAD <- svyby(~TRABAJO,denominator=~Nhog,by=~ENTIDAD ,mydesign,svyratio) # por ENTIDAD
###### ingreso del trabajo independiente
MnegocioTot <- svyratio(~NEGOCIO,denominator=~Nhog,mydesign) # Total promedio
MnegocioENTIDAD <- svyby(~NEGOCIO,denominator=~Nhog,by=~ENTIDAD ,mydesign,svyratio) # por ENTIDAD
###### ingreso de otros trabajos
Motros_trabTot <- svyratio(~OTROS_TRAB,denominator=~Nhog,mydesign) # Total promedio
Motros_trabENTIDAD<- svyby(~OTROS_TRAB,denominator=~Nhog,by=~ENTIDAD,mydesign,svyratio) # por ENTIDAD


###################################        Rentas de la propiedad 

#la renta de la propiedad se divide en: ingresos de sociedades y arrendamientos.

#ingresos totales por renta de la porpiedad
MrentasTot <- svyratio(~RENTAS,denominator=~Nhog,mydesign) # Total promedio
MrentasENTIDAD <- svyby(~RENTAS,denominator=~Nhog,by=~ENTIDAD ,mydesign,svyratio) #Por ENTIDAD
###### ingresos de sociedades
MutilidadTot <- svyratio(~UTILIDAD,denominator=~Nhog,mydesign) # Total promedio
MutilidadENTIDAD <- svyby(~UTILIDAD,denominator=~Nhog,by=~ENTIDAD ,mydesign,svyratio) # por ENTIDAD
###### arrendamiento
MarrendaTot <- svyratio(~ARRENDA,denominator=~Nhog,mydesign) # Total promedio
MarrendaENTIDAD <- svyby(~ARRENDA,denominator=~Nhog,by=~ENTIDAD ,mydesign,svyratio) # Por ENTIDAD


###################################        Transferencias   

#las transferencias totales se definen como la suma de jubilacion, becas, donativos, remesas, bene_gob, transf_hog y trans_inst.

MtransferTot <- svyratio(~TRANSFER,denominator=~Nhog,mydesign) # Total promedio
MtransferENTIDAD <- svyby(~TRANSFER,denominator=~Nhog,by=~ENTIDAD ,mydesign,svyratio) # ENTIDAD

###### jubilacion se define como Jubilaciones, pensiones e indemnizaciones por accidente de trabajo despido y retiro voluntario.
#En el cuestionario solo se les pregunta si recibi? jubilaciones. As? que puede ser p?blicas o privadas.

MjubilacionTot <- svyratio(~JUBILA,denominator=~Nhog,mydesign) # Total promedio
MjubilacionENTIDAD <- svyby(~JUBILA,denominator=~Nhog,by=~ENTIDAD ,mydesign,svyratio) # ENTIDAD

###### becas que pueden ser, de nuevo, p?blicas privadas. 
MbecasTot <- svyratio(~BECA,denominator=~Nhog,mydesign) # Total promedio
MbecasENTIDAD <- svyby(~BECA,denominator=~Nhog,by=~ENTIDAD ,mydesign,svyratio) # ENTIDAD

###### donativos que tambi?n pueden ser p?blicos o privados.
MdonativosTot <- svyratio(~DONATIVO,denominator=~Nhog,mydesign) # Total promedio
MdonativosENTIDAD <- svyby(~DONATIVO,denominator=~Nhog,by=~ENTIDAD ,mydesign,svyratio) # ENTIDAD

###### remesas se definen como ingresos provenientes d eotros paises. As? de manera gen?rica.
MremesasTot <- svyratio(~REMESA,denominator=~Nhog,mydesign) # Total promedio
MremesasENTIDAD <- svyby(~REMESA,denominator=~Nhog,by=~ENTIDAD ,mydesign,svyratio) # ENTIDAD

###### bene_gob:  aqu? estna los programas p?blicos. Prospera, procampo, 65 y m?s, adultos mayores, sin hambre, empleo tempora y Otros.
Mbene_gobTot <- svyratio(~BENE_GOB,denominator=~Nhog,mydesign) # Total promedio
Mbene_gobENTIDAD <- svyby(~BENE_GOB,denominator=~Nhog,by=~ENTIDAD ,mydesign,svyratio) # ENTIDAD

###### transf_hog:  Esto es lo que transfiere otro hogar.
Mtransf_hogTot <- svyratio(~ESP_HOG,denominator=~Nhog,mydesign) # Total promedio
Mtransf_hogENTIDAD <- svyby(~ESP_HOG,denominator=~Nhog,by=~ENTIDAD ,mydesign,svyratio) #ENTIDAD

###### trans_inst: puede venir de institucione sp?blicas o privadas.
Mtrans_instTot <- svyratio(~ESP_INST,denominator=~Nhog,mydesign) # Total promedio
Mtrans_instENTIDAD <- svyby(~ESP_INST,denominator=~Nhog,by=~ENTIDAD ,mydesign,svyratio) # ENTIDAD


### estim_alqu ### Aparentemente se le pregunta al entrevistado cu?nto constar?a la renta del lugar donde vive.
Mestim_alquTot <- svyratio(~ESTI,denominator=~Nhog,mydesign) # Total promedio
Mestim_alquENTIDAD <- svyby(~ESTI,denominator=~Nhog,by=~ENTIDAD ,mydesign,svyratio) # ENTIDAD


### otros_ing ### es literalmente ?algo m?s?
Motros_ingTot <- svyratio(~OTROS,denominator=~Nhog,mydesign) # Total promedio
Motros_ingENTIDAD <- svyby(~OTROS,denominator=~Nhog,by=~ENTIDAD ,mydesign,svyratio) # ENTIDAD


######################################### Estimaciones 

ES_Ming_corTot <- Ming_corTot[[1]] #lo que estoy haciendo aqu? es extraer el valor de la primera columa que corresponde al c?lculo.
ES_Ming_corENTIDAD <- Ming_corENTIDAD[[2]] #En el caso de las entidades, los c?lculos quedaron en la segunda columna

ES_MingtrabTot <- MingtrabTot[[1]]
ES_MingtrabENTIDAD <- MingtrabENTIDAD[[2]]

ES_MtrabajoTot <- MtrabajoTot[[1]]
ES_MtrabajoENTIDAD <- MtrabajoENTIDAD[[2]]

ES_MnegocioTot <- MnegocioTot[[1]]
ES_MnegocioENTIDAD <- MnegocioENTIDAD[[2]]

ES_Motros_trabTot <- Motros_trabTot [[1]]
ES_Motros_trabENTIDAD <- Motros_trabENTIDAD [[2]]

ES_MrentasTot <- MrentasTot [[1]]
ES_MrentasENTIDAD <- MrentasENTIDAD [[2]]

ES_MutilidadTot <- MutilidadTot [[1]]
ES_MutilidadENTIDAD <- MutilidadENTIDAD [[2]]

ES_MarrendaTot <- MarrendaTot [[1]]
ES_MarrendaENTIDAD <- MarrendaENTIDAD [[2]]

ES_MtransferTot <- MtransferTot[[1]]
ES_MtransferENTIDAD <- MtransferENTIDAD[[2]]

ES_MjubilacionTot <- MjubilacionTot [[1]]
ES_MjubilacionENTIDAD <- MjubilacionENTIDAD [[2]]

ES_MbecasTot <- MbecasTot [[1]]
ES_MbecasENTIDAD <- MbecasENTIDAD [[2]]

ES_MdonativosTot <- MdonativosTot[[1]]
ES_MdonativosENTIDAD <- MdonativosENTIDAD[[2]]

ES_MremesasTot <- MremesasTot[[1]]
ES_MremesasENTIDAD <- MremesasENTIDAD[[2]]

ES_Mbene_gobTot <- Mbene_gobTot [[1]]
ES_Mbene_gobENTIDAD <- Mbene_gobENTIDAD [[2]]

ES_Mtransf_hogTot <- Mtransf_hogTot [[1]]
ES_Mtransf_hogENTIDAD <- Mtransf_hogENTIDAD [[2]]

ES_Mtrans_instTot <- Mtrans_instTot[[1]]
ES_Mtrans_instENTIDAD <- Mtrans_instENTIDAD[[2]]

ES_Mestim_alquTot <- Mestim_alquTot [[1]]
ES_Mestim_alquENTIDAD <- Mestim_alquENTIDAD [[2]]

ES_Motros_ingTot <- Motros_ingTot [[1]]
ES_Motros_ingENTIDAD <- Motros_ingENTIDAD [[2]]

########## Error Est?ndar 
SE_Ming_corTot <- SE (Ming_corTot)
SE_Ming_corENTIDAD <- SE (Ming_corENTIDAD)

SE_MingtrabTot <- SE (MingtrabTot)
SE_MingtrabENTIDAD <- SE (MingtrabENTIDAD)

SE_MtrabajoTot <- SE (MtrabajoTot)
SE_MtrabajoENTIDAD <- SE (MtrabajoENTIDAD)

SE_MnegocioTot <- SE (MnegocioTot)
SE_MnegocioENTIDAD <- SE (MnegocioENTIDAD)

SE_Motros_trabTot <- SE (Motros_trabTot)
SE_Motros_trabENTIDAD <- SE (Motros_trabENTIDAD)

SE_MrentasTot <- SE (MrentasTot)
SE_MrentasENTIDAD <- SE (MrentasENTIDAD)

SE_MutilidadTot <- SE (MutilidadTot)
SE_MutilidadENTIDAD <- SE (MutilidadENTIDAD)

SE_MarrendaTot <- SE (MarrendaTot)
SE_MarrendaENTIDAD <- SE (MarrendaENTIDAD)

SE_MtransferTot <- SE (MtransferTot)
SE_MtransferENTIDAD <- SE (MtransferENTIDAD)

SE_MjubilacionTot <- SE (MjubilacionTot)
SE_MjubilacionENTIDAD <- SE (MjubilacionENTIDAD)

SE_MbecasTot <- SE (MbecasTot)
SE_MbecasENTIDAD <- SE (MbecasENTIDAD)

SE_MdonativosTot <- SE (MdonativosTot)
SE_MdonativosENTIDAD <- SE (MdonativosENTIDAD)

SE_MremesasTot <- SE (MremesasTot)
SE_MremesasENTIDAD <- SE (MremesasENTIDAD)

SE_Mbene_gobTot <- SE (Mbene_gobTot)
SE_Mbene_gobENTIDAD <- SE (Mbene_gobENTIDAD)

SE_Mtransf_hogTot <- SE (Mtransf_hogTot)
SE_Mtransf_hogENTIDAD <- SE (Mtransf_hogENTIDAD)

SE_Mtrans_instTot <- SE (Mtrans_instTot)
SE_Mtrans_instENTIDAD <- SE (Mtrans_instENTIDAD)

SE_Mestim_alquTot <- SE (Mestim_alquTot)
SE_Mestim_alquENTIDAD <- SE (Mestim_alquENTIDAD)

SE_Motros_ingTot <- SE (Motros_ingTot)
SE_Motros_ingENTIDAD <- SE (Motros_ingENTIDAD)

########## Coeficiente de variaci?n 
CV_Ming_corTot <- cv(Ming_corTot)
CV_Ming_corENTIDAD <- cv(Ming_corENTIDAD)

CV_MingtrabTot <- cv(MingtrabTot)
CV_MingtrabENTIDAD <- cv(MingtrabENTIDAD)

CV_MtrabajoTot <- cv(MtrabajoTot)
CV_MtrabajoENTIDAD <- cv(MtrabajoENTIDAD)

CV_MnegocioTot <- cv(MnegocioTot)
CV_MnegocioENTIDAD <- cv(MnegocioENTIDAD)

CV_Motros_trabTot <- cv(Motros_trabTot)
CV_Motros_trabENTIDAD <- cv(Motros_trabENTIDAD)

CV_MrentasTot <- cv(MrentasTot)
CV_MrentasENTIDAD <- cv(MrentasENTIDAD)

CV_MutilidadTot <- cv(MutilidadTot)
CV_MutilidadENTIDAD <- cv(MutilidadENTIDAD)

CV_MarrendaTot <- cv(MarrendaTot)
CV_MarrendaENTIDAD <- cv(MarrendaENTIDAD)

CV_MtransferTot <- cv(MtransferTot)
CV_MtransferENTIDAD <- cv(MtransferENTIDAD)

CV_MjubilacionTot <- cv(MjubilacionTot)
CV_MjubilacionENTIDAD <- cv(MjubilacionENTIDAD)

CV_MbecasTot <- cv(MbecasTot)
CV_MbecasENTIDAD <- cv(MbecasENTIDAD)

CV_MdonativosTot <- cv(MdonativosTot)
CV_MdonativosENTIDAD <- cv(MdonativosENTIDAD)

CV_MremesasTot <- cv(MremesasTot)
CV_MremesasENTIDAD <- cv(MremesasENTIDAD)

CV_Mbene_gobTot <- cv(Mbene_gobTot)
CV_Mbene_gobENTIDAD <- cv(Mbene_gobENTIDAD)

CV_Mtransf_hogTot <- cv(Mtransf_hogTot)
CV_Mtransf_hogENTIDAD <- cv(Mtransf_hogENTIDAD)

CV_Mtrans_instTot <- cv(Mtrans_instTot)
CV_Mtrans_instENTIDAD <- cv(Mtrans_instENTIDAD)

CV_Mestim_alquTot <- cv(Mestim_alquTot)
CV_Mestim_alquENTIDAD <- cv(Mestim_alquENTIDAD)

CV_Motros_ingTot <- cv(Motros_ingTot)
CV_Motros_ingENTIDAD <- cv(Motros_ingENTIDAD)

########## Limite inferior 
LI_Ming_corTot <- confint(Ming_corTot,level=0.90)[,1]
LI_Ming_corENTIDAD <- confint(Ming_corENTIDAD,level=0.90)[,1]

LI_MingtrabTot <- confint(MingtrabTot,level=0.90)[,1]
LI_MingtrabENTIDAD <- confint(MingtrabENTIDAD,level=0.90)[,1]

LI_MtrabajoTot <- confint(MtrabajoTot,level=0.90)[,1]
LI_MtrabajoENTIDAD <- confint(MtrabajoENTIDAD,level=0.90)[,1]

LI_MnegocioTot <- confint(MnegocioTot,level=0.90)[,1]
LI_MnegocioENTIDAD <- confint(MnegocioENTIDAD,level=0.90)[,1]

LI_Motros_trabTot <- confint(Motros_trabTot,level=0.90)[,1]
LI_Motros_trabENTIDAD <- confint(Motros_trabENTIDAD,level=0.90)[,1]

LI_MrentasTot <- confint(MrentasTot,level=0.90)[,1]
LI_MrentasENTIDAD <- confint(MrentasENTIDAD,level=0.90)[,1]

LI_MutilidadTot <- confint(MutilidadTot,level=0.90)[,1]
LI_MutilidadENTIDAD <- confint(MutilidadENTIDAD,level=0.90)[,1]

LI_MarrendaTot <- confint(MarrendaTot,level=0.90)[,1]
LI_MarrendaENTIDAD <- confint(MarrendaENTIDAD,level=0.90)[,1]

LI_MtransferTot <- confint(MtransferTot,level=0.90)[,1]
LI_MtransferENTIDAD <- confint(MtransferENTIDAD,level=0.90)[,1]

LI_MjubilacionTot <- confint(MjubilacionTot,level=0.90)[,1]
LI_MjubilacionENTIDAD <- confint(MjubilacionENTIDAD,level=0.90)[,1]

LI_MbecasTot <- confint(MbecasTot,level=0.90)[,1]
LI_MbecasENTIDAD <- confint(MbecasENTIDAD,level=0.90)[,1]

LI_MdonativosTot <- confint(MdonativosTot,level=0.90)[,1]
LI_MdonativosENTIDAD <- confint(MdonativosENTIDAD,level=0.90)[,1]

LI_MremesasTot <- confint(MremesasTot,level=0.90)[,1]
LI_MremesasENTIDAD <- confint(MremesasENTIDAD,level=0.90)[,1]

LI_Mbene_gobTot <- confint(Mbene_gobTot,level=0.90)[,1]
LI_Mbene_gobENTIDAD <- confint(Mbene_gobENTIDAD,level=0.90)[,1]

LI_Mtransf_hogTot <- confint(Mtransf_hogTot,level=0.90)[,1]
LI_Mtransf_hogENTIDAD <- confint(Mtransf_hogENTIDAD,level=0.90)[,1]

LI_Mtrans_instTot <- confint(Mtrans_instTot,level=0.90)[,1]
LI_Mtrans_instENTIDAD <- confint(Mtrans_instENTIDAD,level=0.90)[,1]

LI_Mestim_alquTot <- confint(Mestim_alquTot,level=0.90)[,1]
LI_Mestim_alquENTIDAD <- confint(Mestim_alquENTIDAD,level=0.90)[,1
]
LI_Motros_ingTot <- confint(Motros_ingTot,level=0.90)[,1]
LI_Motros_ingENTIDAD <- confint(Motros_ingENTIDAD ,level=0.90)[,1]

########## Limite superior 
LS_Ming_corTot <- confint(Ming_corTot,level=0.90)[,2]
LS_Ming_corENTIDAD <- confint(Ming_corENTIDAD,level=0.90)[,2]

LS_MingtrabTot <- confint(MingtrabTot,level=0.90)[,2]
LS_MingtrabENTIDAD <- confint(MingtrabENTIDAD,level=0.90)[,2]

LS_MtrabajoTot <- confint(MtrabajoTot,level=0.90)[,2]
LS_MtrabajoENTIDAD <- confint(MtrabajoENTIDAD,level=0.90)[,2]

LS_MnegocioTot <- confint(MnegocioTot,level=0.90)[,2]
LS_MnegocioENTIDAD <- confint(MnegocioENTIDAD,level=0.90)[,2]

LS_Motros_trabTot <- confint(Motros_trabTot,level=0.90)[,2]
LS_Motros_trabENTIDAD <- confint(Motros_trabENTIDAD,level=0.90)[,2]

LS_MrentasTot <- confint(MrentasTot,level=0.90)[,2]
LS_MrentasENTIDAD <- confint(MrentasENTIDAD,level=0.90)[,2]

LS_MutilidadTot <- confint(MutilidadTot,level=0.90)[,2]
LS_MutilidadENTIDAD <- confint(MutilidadENTIDAD,level=0.90)[,2]

LS_MarrendaTot <- confint(MarrendaTot,level=0.90)[,2]
LS_MarrendaENTIDAD <- confint(MarrendaENTIDAD,level=0.90)[,2]

LS_MtransferTot <- confint(MtransferTot,level=0.90)[,2]
LS_MtransferENTIDAD <- confint(MtransferENTIDAD,level=0.90)[,2]

LS_MjubilacionTot <- confint(MjubilacionTot,level=0.90)[,2]
LS_MjubilacionENTIDAD <- confint(MjubilacionENTIDAD,level=0.90)[,2]

LS_MbecasTot <- confint(MbecasTot,level=0.90)[,2]
LS_MbecasENTIDAD <- confint(MbecasENTIDAD,level=0.90)[,2]

LS_MdonativosTot <- confint(MdonativosTot,level=0.90)[,2]
LS_MdonativosENTIDAD <- confint(MdonativosENTIDAD,level=0.90)[,2]

LS_MremesasTot <- confint(MremesasTot,level=0.90)[,2]
LS_MremesasENTIDAD <- confint(MremesasENTIDAD,level=0.90)[,2]

LS_Mbene_gobTot <- confint(Mbene_gobTot,level=0.90)[,2]
LS_Mbene_gobENTIDAD <- confint(Mbene_gobENTIDAD,level=0.90)[,2]

LS_Mtransf_hogTot <- confint(Mtransf_hogTot,level=0.90)[,2]
LS_Mtransf_hogENTIDAD <- confint(Mtransf_hogENTIDAD,level=0.90)[,2]

LS_Mtrans_instTot <- confint(Mtrans_instTot,level=0.90)[,2]
LS_Mtrans_instENTIDAD <- confint(Mtrans_instENTIDAD,level=0.90)[,2]

LS_Mestim_alquTot <- confint(Mestim_alquTot,level=0.90)[,2]
LS_Mestim_alquENTIDAD <- confint(Mestim_alquENTIDAD,level=0.90)[,2]

LS_Motros_ingTot <- confint(Motros_ingTot,level=0.90)[,2]
LS_Motros_ingENTIDAD <- confint(Motros_ingENTIDAD,level=0.90)[,2]

#############################      Cuadros   
#este cuadro, lo ?nico que tiene son todas la estimaciones.
#son 10 filas y 18 columnas.
c_ENTIDAD_ES <-
  data.frame(c(ES_Ming_corTot,ES_Ming_corENTIDAD),c(ES_MingtrabTot,ES_MingtrabENTIDAD),c(ES_MtrabajoTot,ES_MtrabajoENTIDAD),c(ES_MnegocioTot,ES_MnegocioENTIDAD)
             ,c(ES_Motros_trabTot,ES_Motros_trabENTIDAD),c(ES_MrentasTot,ES_MrentasENTIDAD),c(ES_MutilidadTot,ES_MutilidadENTIDAD)
             ,c(ES_MarrendaTot,ES_MarrendaENTIDAD),c(ES_MtransferTot,ES_MtransferENTIDAD),c(ES_MjubilacionTot,ES_MjubilacionENTIDAD),c(ES_MbecasTot,ES_MbecasENTIDAD),
             c(ES_MdonativosTot,ES_MdonativosENTIDAD),c(ES_MremesasTot,ES_MremesasENTIDAD),c(ES_Mbene_gobTot,ES_Mbene_gobENTIDAD),c(ES_Mtransf_hogTot,ES_Mtransf_hogENTIDAD)
             ,c(ES_Mtrans_instTot,ES_Mtrans_instENTIDAD),c(ES_Mestim_alquTot,ES_Mestim_alquENTIDAD),c(ES_Motros_ingTot,ES_Motros_ingENTIDAD))
##### ERROR ESTANDAR
c_ENTIDAD_SE <-
  data.frame(c(SE_Ming_corTot,SE_Ming_corENTIDAD),c(SE_MingtrabTot,SE_MingtrabENTIDAD),c(SE_MtrabajoTot,SE_MtrabajoENTIDAD),c(SE_MnegocioTot,SE_MnegocioENTIDAD)
             ,c(SE_Motros_trabTot,SE_Motros_trabENTIDAD),c(SE_MrentasTot,SE_MrentasENTIDAD),c(SE_MutilidadTot,SE_MutilidadENTIDAD)
             ,c(SE_MarrendaTot,SE_MarrendaENTIDAD),c(SE_MtransferTot,SE_MtransferENTIDAD),c(SE_MjubilacionTot,SE_MjubilacionENTIDAD),c(SE_MbecasTot,SE_MbecasENTIDAD),
             c(SE_MdonativosTot,SE_MdonativosENTIDAD),c(SE_MremesasTot,SE_MremesasENTIDAD),c(SE_Mbene_gobTot,SE_Mbene_gobENTIDAD),c(SE_Mtransf_hogTot,SE_Mtransf_hogENTIDAD),c(SE_Mtrans_instTot,SE_Mtrans_instENTIDAD)
             ,c(SE_Mestim_alquTot,SE_Mestim_alquENTIDAD),c(SE_Motros_ingTot,SE_Motros_ingENTIDAD))

##### COEFICIENTE DE VARIACION
c_ENTIDAD_CV <-
  data.frame(c(CV_Ming_corTot,CV_Ming_corENTIDAD),c(CV_MingtrabTot,CV_MingtrabENTIDAD),c(CV_MtrabajoTot,CV_MtrabajoENTIDAD),c(CV_MnegocioTot,CV_MnegocioENTIDAD)
             ,c(CV_Motros_trabTot,CV_Motros_trabENTIDAD),c(CV_MrentasTot,CV_MrentasENTIDAD),c(CV_MutilidadTot,CV_MutilidadENTIDAD),
             c(CV_MarrendaTot,CV_MarrendaENTIDAD),c(CV_MtransferTot,CV_MtransferENTIDAD),c(CV_MjubilacionTot,CV_MjubilacionENTIDAD),c(CV_MbecasTot,CV_MbecasENTIDAD)
             ,c(CV_MdonativosTot,CV_MdonativosENTIDAD),c(CV_MremesasTot,CV_MremesasENTIDAD),c(CV_Mbene_gobTot,CV_Mbene_gobENTIDAD),c(CV_Mtransf_hogTot,CV_Mtransf_hogENTIDAD),c(CV_Mtrans_instTot,CV_Mtrans_instENTIDAD)
             ,c(CV_Mestim_alquTot,CV_Mestim_alquENTIDAD),c(CV_Motros_ingTot,CV_Motros_ingENTIDAD))

##### LIMITE INFERIOR AL 90%
c_ENTIDAD_LI <-
  data.frame(c(LI_Ming_corTot,LI_Ming_corENTIDAD),c(LI_MingtrabTot,LI_MingtrabENTIDAD),c(LI_MtrabajoTot,LI_MtrabajoENTIDAD),
             c(LI_MnegocioTot,LI_MnegocioENTIDAD),c(LI_Motros_trabTot,LI_Motros_trabENTIDAD),c(LI_MrentasTot,LI_MrentasENTIDAD),c(LI_MutilidadTot,LI_MutilidadENTIDAD),c(LI_MarrendaTot,LI_MarrendaENTIDAD)
             ,c(LI_MtransferTot,LI_MtransferENTIDAD),c(LI_MjubilacionTot,LI_MjubilacionENTIDAD),c(LI_MbecasTot,LI_MbecasENTIDAD),c(LI_MdonativosTot,LI_MdonativosENTIDAD)
             ,c(LI_MremesasTot,LI_MremesasENTIDAD),c(LI_Mbene_gobTot,LI_Mbene_gobENTIDAD),c(LI_Mtransf_hogTot,LI_Mtransf_hogENTIDAD),c(LI_Mtrans_instTot,LI_Mtrans_instENTIDAD)
             ,c(LI_Mestim_alquTot,LI_Mestim_alquENTIDAD),c(LI_Motros_ingTot,LI_Motros_ingENTIDAD))

### LIMITE SUPERIOR AL 90%
c_ENTIDAD_LS <-
  data.frame(c(LS_Ming_corTot,LS_Ming_corENTIDAD),c(LS_MingtrabTot,LS_MingtrabENTIDAD),c(LS_MtrabajoTot,LS_MtrabajoENTIDAD),c(LS_MnegocioTot,LS_MnegocioENTIDAD)
             ,c(LS_Motros_trabTot,LS_Motros_trabENTIDAD),c(LS_MrentasTot,LS_MrentasENTIDAD),c(LS_MutilidadTot,LS_MutilidadENTIDAD),
             c(LS_MarrendaTot,LS_MarrendaENTIDAD),c(LS_MtransferTot,LS_MtransferENTIDAD),c(LS_MjubilacionTot,LS_MjubilacionENTIDAD),c(LS_MbecasTot,LS_MbecasENTIDAD),
             c(LS_MdonativosTot,LS_MdonativosENTIDAD),c(LS_MremesasTot,LS_MremesasENTIDAD),c(LS_Mbene_gobTot,LS_Mbene_gobENTIDAD),c(LS_Mtransf_hogTot,LS_Mtransf_hogENTIDAD),c(LS_Mtrans_instTot,LS_Mtrans_instENTIDAD)
             ,c(LS_Mestim_alquTot,LS_Mestim_alquENTIDAD),c(LS_Motros_ingTot,LS_Motros_ingENTIDAD))

# se agregan los nombres de las entidades a las filas
#esta cadena est? bien loca, no?
ENTIDADES<-c(entidades<-c("MExico","Aguascalientes","Baja California","Baja California Sur","Campeche","Coahuila de Zaragoza",
                          "Colima","Chiapas","Chihuahua","Ciudad de Mexico","Durango","Guanajuato","Guerrero","Hidalgo",
                          "Jalisco","Mexico","Michoaca¡n de Ocampo","Morelos","Nayarit","Nuevo Leon","Oaxaca","Puebla",
                          "Queretaro","Quintana Roo","San Luis Potosi","Sinaloa","Sonora","Tabasco","Tamaulipas","Tlaxcala","Veracruz de Ignacio de la Llave","Yucatan","Zacatecas"))

row.names(c_ENTIDAD_ES)<-row.names(c_ENTIDAD_SE)<-row.names(c_ENTIDAD_CV)<-row.names(c_ENTIDAD_LI)<-row.names(c_ENTIDAD_LS)<-ENTIDADES

#ahora vamos a ponerle nombre a las columnas
names(c_ENTIDAD_ES)=c("ING COR2012", "TRABAJO2012", "SUBORDINADO2012", "NEGOCIOS2012","OTROS TRAB2012", "RENTAS2012","UTILIDAD2012", "ARRENDA2012", "TRANSFER2012","JUBILACION2012", "BECAS2012", "DONATIVOS2012", "REMESAS2012", "BENEGOBIERNO2012", "TRANS HOG2012", "TRANS INST2012", "ESTIM ALQU2012", "OTROS INGRESOS2012")

names(c_ENTIDAD_SE)=c("ING COR2012", "TRABAJO2012", "SUBORDINADO2012", "NEGOCIOS2012","OTROS TRAB2012", "RENTAS2012","UTILIDAD2012", "ARRENDA2012", "TRANSFER2012","JUBILACION2012", "BECAS2012", "DONATIVOS2012", "REMESAS2012", "BENEGOBIERNO2012", "TRANS HOG2012", "TRANS INST2012", "ESTIM ALQU2012", "OTROS INGRESOS2012")

names(c_ENTIDAD_CV)=c("ING COR2012", "TRABAJO2012", "SUBORDINADO2012", "NEGOCIOS2012","OTROS TRAB2012", "RENTAS2012","UTILIDAD2012", "ARRENDA2012", "TRANSFER2012","JUBILACION2012", "BECAS2012", "DONATIVOS2012", "REMESAS2012", "BENEGOBIERNO2012", "TRANS HOG2012", "TRANS INST2012", "ESTIM ALQU2012", "OTROS INGRESOS2012")

names(c_ENTIDAD_LI)=c("ING COR2012", "TRABAJO2012", "SUBORDINADO2012", "NEGOCIOS2012","OTROS TRAB2012", "RENTAS2012","UTILIDAD2012", "ARRENDA2012", "TRANSFER2012","JUBILACION2012", "BECAS2012", "DONATIVOS2012", "REMESAS2012", "BENEGOBIERNO2012", "TRANS HOG2012", "TRANS INST2012", "ESTIM ALQU2012", "OTROS INGRESOS2012")

names(c_ENTIDAD_LS)=c("ING COR", "TRABAJO", "SUBORDINADO", "NEGOCIOS","OTROS TRAB", "RENTAS","UTILIDAD", "ARRENDA", "TRANSFER","JUBILACION", "BECAS", "DONATIVOS", "REMESAS", "BENEGOBIERNO", "TRANS HOG", "TRANS INST", "ESTIM ALQU", "OTROS INGRESOS")

#ahora, lo que podemos hacer es mostrar los cuadros en la consola redondeados
# el comando round, redondea las cifra para mostrar, en el caso del coeficiente de variaci?n redondea a 4 decimales y luego multiplica por cien.
# Mostramos el resultado en pantalla
round(c_ENTIDAD_ES)
round(c_ENTIDAD_SE)
round(c_ENTIDAD_CV,4)*100
round(c_ENTIDAD_LI)
round(c_ENTIDAD_LS)

prueba<-c_ENTIDAD_ES%>%
  mutate(`ING COR2012`=`ING COR2012`, prueba=TRABAJO2012+RENTAS2012+JUBILACION2012+BECAS2012+DONATIVOS2012+REMESAS2012+BENEGOBIERNO2012+`TRANS HOG2012`+`TRANS INST2012`+`ESTIM ALQU2012`+`OTROS INGRESOS2012`)

all.equal(prueba$`ING COR2012`,prueba$prueba)

########## consumo por ENTIDAD 



Consumo_por_ENTIDAD <- svyby(~GASTO,denominator=~Nhog,by=~ENTIDAD,mydesign,svyratio)

Consumo_promedio <- svyratio(~GASTO,denominator=~Nhog,mydesign) 

SE_consumo_Tot <- SE (Consumo_promedio)
SE_consumo_ENTIDAD <- SE (Consumo_por_ENTIDAD)

Consumo_por_ENTIDAD <- Consumo_por_ENTIDAD[[2]] 
Consumo_promedio <- Consumo_promedio[[1]]

Consumo<-data.frame(c(Consumo_promedio,Consumo_por_ENTIDAD))

ENTIDADES<-c(entidades<-c("MExico","Aguascalientes","Baja California","Baja California Sur","Campeche","Coahuila de Zaragoza",
                          "Colima","Chiapas","Chihuahua","Ciudad de Mexico","Durango","Guanajuato","Guerrero","Hidalgo",
                          "Jalisco","Mexico","Michoaca¡n de Ocampo","Morelos","Nayarit","Nuevo Leon","Oaxaca","Puebla",
                          "Queretaro","Quintana Roo","San Luis Potosi","Sinaloa","Sonora","Tabasco","Tamaulipas","Tlaxcala","Veracruz de Ignacio de la Llave","Yucatan","Zacatecas"))

row.names(Consumo)<-ENTIDADES

Consumo_SE<-data.frame(c(SE_consumo_Tot,SE_consumo_ENTIDAD))
row.names(Consumo_SE)<-ENTIDADES





write.dbf(Consumo,file = "ESTADOS MEAN Consumo 2012.dbf")
write.dbf(Consumo_SE,file="ESTADOS MEAN 2012 SE.dbf")
write.dbf(c_ENTIDAD_ES,file = "ESTADOS MEAN por fuente por estimaciones 2012.dbf")
write.dbf(c_ENTIDAD_SE,file = "ESTADOS MEAN por fuente por errores standard 2012.dbf")
write.dbf(c_ENTIDAD_CV,file = "ESTADOS MEAN por fuente por CV 2012.dbf")
write.dbf(c_ENTIDAD_LI,file = "ESTADOS MEAN por fuente por LI 2012.dbf")
write.dbf(c_ENTIDAD_ES,file = "ESTADOS MEAN por fuente por LS 2012.dbf")

rm(list = ls())



################# ingreso por estado bottom 40 ######################
library(foreign)
library(survey)
library(doBy)
library(reldist)
library(tidyverse)
options(survey.lonely.psu="adjust")

#reading the data
setwd("C:/Users/Erick/onedrive/GIC/GITHUB2018/GIC/ENIGH_2012/ENIGH2012")
Conc2012<-read.dbf("Conc_2012.dbf",as.is = T)


names(Conc2012)<-c("ENTIDAD","FOLIOVIV","FOLIOHOG","GASTO","TOT_INTEG","INGCOR","INGTRAB","TRABAJO","NEGOCIO","OTROS_TRAB",
                   "RENTAS","UTILIDAD","ARRENDA","TRANSFER","JUBILA","BECA","DONATIVO","REMESA","BENE_GOB",
                   "ESP_HOG","ESP_INST","ESTI","OTROS","FACTOR","UPM","EST_DIS","tam_localidad","Small","HOGARINDIG","NOMBRE_ENT",
                   "DEFLACTORES","Nhog","TAM_DECIL","MAXT","ACUMULA","ACUMULA2","DECIL","Bottom_40")

Conc2012<-Conc2012%>%
  filter(Bottom_40==1)




mydesign <- svydesign(id=~UPM,strata=~EST_DIS,data=Conc2012,weights=~FACTOR)

#vamos por el ingreso corriente total del pa?s
# ing_ cor se define como La suma de las variables ingtrab, rentas, transfer, estim_alqu y otros_ing.
#te sale que el ingreso trimestra promedio en Mexico es de 49,610.
#notes? que esto no es otra cosa que el ing_cor*factor/34744819
bottom40_Ming_corTot <- svyratio(~INGCOR,denominator=~Nhog,mydesign) 

#ahora, vamos a hacer lo mismo por ENTIDAD
#aqu? cmabia la funci?n a svyby, en by va el ENTIDAD que creamos.
#y al final va la funci?n que queremos
bottom40_Ming_corENTIDAD <- svyby(~INGCOR,denominator=~Nhog,by=~ENTIDAD,mydesign,svyratio)


#     Trabajo
#
#El trabajo se divide en tres clasificaciones: subordinado, independiente y otros.
### ingreso del trabajo total###
bottom40_MingtrabTot <- svyratio(~INGTRAB,denominator=~Nhog,mydesign) # Total promedio
bottom40_MingtrabENTIDAD <- svyby(~INGTRAB,denominator=~Nhog,by=~ENTIDAD ,mydesign,svyratio) # por ENTIDAD
###### ingreso del trabajo subordinado
bottom40_MtrabajoTot <- svyratio(~TRABAJO,denominator=~Nhog,mydesign) # Total promedio
bottom40_MtrabajoENTIDAD <- svyby(~TRABAJO,denominator=~Nhog,by=~ENTIDAD ,mydesign,svyratio) # por ENTIDAD
###### ingreso del trabajo independiente
bottom40_MnegocioTot <- svyratio(~NEGOCIO,denominator=~Nhog,mydesign) # Total promedio
bottom40_MnegocioENTIDAD <- svyby(~NEGOCIO,denominator=~Nhog,by=~ENTIDAD ,mydesign,svyratio) # por ENTIDAD
###### ingreso de otros trabajos
bottom40_Motros_trabTot <- svyratio(~OTROS_TRAB,denominator=~Nhog,mydesign) # Total promedio
bottom40_Motros_trabENTIDAD<- svyby(~OTROS_TRAB,denominator=~Nhog,by=~ENTIDAD,mydesign,svyratio) # por ENTIDAD


###################################        Rentas de la propiedad 

#la renta de la propiedad se divide en: ingresos de sociedades y arrendamientos.

#ingresos totales por renta de la porpiedad
bottom40_MrentasTot <- svyratio(~RENTAS,denominator=~Nhog,mydesign) # Total promedio
bottom40_MrentasENTIDAD <- svyby(~RENTAS,denominator=~Nhog,by=~ENTIDAD ,mydesign,svyratio) #Por ENTIDAD
###### ingresos de sociedades
bottom40_MutilidadTot <- svyratio(~UTILIDAD,denominator=~Nhog,mydesign) # Total promedio
bottom40_MutilidadENTIDAD <- svyby(~UTILIDAD,denominator=~Nhog,by=~ENTIDAD ,mydesign,svyratio) # por ENTIDAD
###### arrendamiento
bottom40_MarrendaTot <- svyratio(~ARRENDA,denominator=~Nhog,mydesign) # Total promedio
bottom40_MarrendaENTIDAD <- svyby(~ARRENDA,denominator=~Nhog,by=~ENTIDAD ,mydesign,svyratio) # Por ENTIDAD


###################################        Transferencias   

#las transferencias totales se definen como la suma de jubilacion, becas, donativos, remesas, bene_gob, transf_hog y trans_inst.

bottom40_MtransferTot <- svyratio(~TRANSFER,denominator=~Nhog,mydesign) # Total promedio
bottom40_MtransferENTIDAD <- svyby(~TRANSFER,denominator=~Nhog,by=~ENTIDAD ,mydesign,svyratio) # ENTIDAD

###### jubilacion se define como Jubilaciones, pensiones e indemnizaciones por accidente de trabajo despido y retiro voluntario.
#En el cuestionario solo se les pregunta si recibi? jubilaciones. As? que puede ser p?blicas o privadas.

bottom40_MjubilacionTot <- svyratio(~JUBILA,denominator=~Nhog,mydesign) # Total promedio
bottom40_MjubilacionENTIDAD <- svyby(~JUBILA,denominator=~Nhog,by=~ENTIDAD ,mydesign,svyratio) # ENTIDAD

###### becas que pueden ser, de nuevo, p?blicas privadas. 
bottom40_MbecasTot <- svyratio(~BECA,denominator=~Nhog,mydesign) # Total promedio
bottom40_MbecasENTIDAD <- svyby(~BECA,denominator=~Nhog,by=~ENTIDAD ,mydesign,svyratio) # ENTIDAD

###### donativos que tambi?n pueden ser p?blicos o privados.
bottom40_MdonativosTot <- svyratio(~DONATIVO,denominator=~Nhog,mydesign) # Total promedio
bottom40_MdonativosENTIDAD <- svyby(~DONATIVO,denominator=~Nhog,by=~ENTIDAD ,mydesign,svyratio) # ENTIDAD

###### remesas se definen como ingresos provenientes d eotros paises. As? de manera gen?rica.
bottom40_MremesasTot <- svyratio(~REMESA,denominator=~Nhog,mydesign) # Total promedio
bottom40_MremesasENTIDAD <- svyby(~REMESA,denominator=~Nhog,by=~ENTIDAD ,mydesign,svyratio) # ENTIDAD

###### bene_gob:  aqu? estna los programas p?blicos. Prospera, procampo, 65 y m?s, adultos mayores, sin hambre, empleo tempora y Otros.
bottom40_Mbene_gobTot <- svyratio(~BENE_GOB,denominator=~Nhog,mydesign) # Total promedio
bottom40_Mbene_gobENTIDAD <- svyby(~BENE_GOB,denominator=~Nhog,by=~ENTIDAD ,mydesign,svyratio) # ENTIDAD

###### transf_hog:  Esto es lo que transfiere otro hogar.
bottom40_Mtransf_hogTot <- svyratio(~ESP_HOG,denominator=~Nhog,mydesign) # Total promedio
bottom40_Mtransf_hogENTIDAD <- svyby(~ESP_HOG,denominator=~Nhog,by=~ENTIDAD ,mydesign,svyratio) #ENTIDAD

###### trans_inst: puede venir de institucione sp?blicas o privadas.
bottom40_Mtrans_instTot <- svyratio(~ESP_INST,denominator=~Nhog,mydesign) # Total promedio
bottom40_Mtrans_instENTIDAD <- svyby(~ESP_INST,denominator=~Nhog,by=~ENTIDAD ,mydesign,svyratio) # ENTIDAD


### estim_alqu ### Aparentemente se le pregunta al entrevistado cu?nto constar?a la renta del lugar donde vive.
bottom40_Mestim_alquTot <- svyratio(~ESTI,denominator=~Nhog,mydesign) # Total promedio
bottom40_Mestim_alquENTIDAD <- svyby(~ESTI,denominator=~Nhog,by=~ENTIDAD ,mydesign,svyratio) # ENTIDAD


### otros_ing ### es literalmente ?algo m?s?
bottom40_Motros_ingTot <- svyratio(~OTROS,denominator=~Nhog,mydesign) # Total promedio
bottom40_Motros_ingENTIDAD <- svyby(~OTROS,denominator=~Nhog,by=~ENTIDAD ,mydesign,svyratio) # ENTIDAD


######################################### Estimaciones 

ES_Ming_corTot <- bottom40_Ming_corTot[[1]] #lo que estoy haciendo aqu? es extraer el valor de la primera columa que corresponde al c?lculo.
ES_Ming_corENTIDAD <- bottom40_Ming_corENTIDAD[[2]] #En el caso de las entidades, los c?lculos quedaron en la segunda columna

ES_MingtrabTot <- bottom40_MingtrabTot[[1]]
ES_MingtrabENTIDAD <- bottom40_MingtrabENTIDAD[[2]]

ES_MtrabajoTot <- bottom40_MtrabajoTot[[1]]
ES_MtrabajoENTIDAD <- bottom40_MtrabajoENTIDAD[[2]]

ES_MnegocioTot <- bottom40_MnegocioTot[[1]]
ES_MnegocioENTIDAD <- bottom40_MnegocioENTIDAD[[2]]

ES_Motros_trabTot <- bottom40_Motros_trabTot [[1]]
ES_Motros_trabENTIDAD <- bottom40_Motros_trabENTIDAD [[2]]

ES_MrentasTot <- bottom40_MrentasTot [[1]]
ES_MrentasENTIDAD <- bottom40_MrentasENTIDAD [[2]]

ES_MutilidadTot <- bottom40_MutilidadTot [[1]]
ES_MutilidadENTIDAD <- bottom40_MutilidadENTIDAD [[2]]

ES_MarrendaTot <- bottom40_MarrendaTot [[1]]
ES_MarrendaENTIDAD <- bottom40_MarrendaENTIDAD [[2]]

ES_MtransferTot <- bottom40_MtransferTot[[1]]
ES_MtransferENTIDAD <- bottom40_MtransferENTIDAD[[2]]

ES_MjubilacionTot <- bottom40_MjubilacionTot [[1]]
ES_MjubilacionENTIDAD <- bottom40_MjubilacionENTIDAD [[2]]

ES_MbecasTot <- bottom40_MbecasTot [[1]]
ES_MbecasENTIDAD <- bottom40_MbecasENTIDAD [[2]]

ES_MdonativosTot <- bottom40_MdonativosTot[[1]]
ES_MdonativosENTIDAD <- bottom40_MdonativosENTIDAD[[2]]

ES_MremesasTot <- bottom40_MremesasTot[[1]]
ES_MremesasENTIDAD <- bottom40_MremesasENTIDAD[[2]]

ES_Mbene_gobTot <- bottom40_Mbene_gobTot [[1]]
ES_Mbene_gobENTIDAD <- bottom40_Mbene_gobENTIDAD [[2]]

ES_Mtransf_hogTot <- bottom40_Mtransf_hogTot [[1]]
ES_Mtransf_hogENTIDAD <- bottom40_Mtransf_hogENTIDAD [[2]]

ES_Mtrans_instTot <- bottom40_Mtrans_instTot[[1]]
ES_Mtrans_instENTIDAD <- bottom40_Mtrans_instENTIDAD[[2]]

ES_Mestim_alquTot <- bottom40_Mestim_alquTot [[1]]
ES_Mestim_alquENTIDAD <- bottom40_Mestim_alquENTIDAD [[2]]

ES_Motros_ingTot <- bottom40_Motros_ingTot [[1]]
ES_Motros_ingENTIDAD <- bottom40_Motros_ingENTIDAD [[2]]

########## Error Est?ndar 
SE_Ming_corTot <- SE (bottom40_Ming_corTot)
SE_Ming_corENTIDAD <- SE (bottom40_Ming_corENTIDAD)

SE_MingtrabTot <- SE (bottom40_MingtrabTot)
SE_MingtrabENTIDAD <- SE (bottom40_MingtrabENTIDAD)

SE_MtrabajoTot <- SE (bottom40_MtrabajoTot)
SE_MtrabajoENTIDAD <- SE (bottom40_MtrabajoENTIDAD)

SE_MnegocioTot <- SE (bottom40_MnegocioTot)
SE_MnegocioENTIDAD <- SE (bottom40_MnegocioENTIDAD)

SE_Motros_trabTot <- SE (bottom40_Motros_trabTot)
SE_Motros_trabENTIDAD <- SE (bottom40_Motros_trabENTIDAD)

SE_MrentasTot <- SE (bottom40_MrentasTot)
SE_MrentasENTIDAD <- SE (bottom40_MrentasENTIDAD)

SE_MutilidadTot <- SE (bottom40_MutilidadTot)
SE_MutilidadENTIDAD <- SE (bottom40_MutilidadENTIDAD)

SE_MarrendaTot <- SE (bottom40_MarrendaTot)
SE_MarrendaENTIDAD <- SE (bottom40_MarrendaENTIDAD)

SE_MtransferTot <- SE (bottom40_MtransferTot)
SE_MtransferENTIDAD <- SE (bottom40_MtransferENTIDAD)

SE_MjubilacionTot <- SE (bottom40_MjubilacionTot)
SE_MjubilacionENTIDAD <- SE (bottom40_MjubilacionENTIDAD)

SE_MbecasTot <- SE (bottom40_MbecasTot)
SE_MbecasENTIDAD <- SE (bottom40_MbecasENTIDAD)

SE_MdonativosTot <- SE (bottom40_MdonativosTot)
SE_MdonativosENTIDAD <- SE (bottom40_MdonativosENTIDAD)

SE_MremesasTot <- SE (bottom40_MremesasTot)
SE_MremesasENTIDAD <- SE (bottom40_MremesasENTIDAD)

SE_Mbene_gobTot <- SE (bottom40_Mbene_gobTot)
SE_Mbene_gobENTIDAD <- SE (bottom40_Mbene_gobENTIDAD)

SE_Mtransf_hogTot <- SE (bottom40_Mtransf_hogTot)
SE_Mtransf_hogENTIDAD <- SE (bottom40_Mtransf_hogENTIDAD)

SE_Mtrans_instTot <- SE (bottom40_Mtrans_instTot)
SE_Mtrans_instENTIDAD <- SE (bottom40_Mtrans_instENTIDAD)

SE_Mestim_alquTot <- SE (bottom40_Mestim_alquTot)
SE_Mestim_alquENTIDAD <- SE (bottom40_Mestim_alquENTIDAD)

SE_Motros_ingTot <- SE (bottom40_Motros_ingTot)
SE_Motros_ingENTIDAD <- SE (bottom40_Motros_ingENTIDAD)


#############################      Cuadros   
#este cuadro, lo ?nico que tiene son todas la estimaciones.
#son 10 filas y 18 columnas.
c_ENTIDAD_ES <-
  data.frame(c(ES_Ming_corTot,ES_Ming_corENTIDAD),c(ES_MingtrabTot,ES_MingtrabENTIDAD),c(ES_MtrabajoTot,ES_MtrabajoENTIDAD),c(ES_MnegocioTot,ES_MnegocioENTIDAD)
             ,c(ES_Motros_trabTot,ES_Motros_trabENTIDAD),c(ES_MrentasTot,ES_MrentasENTIDAD),c(ES_MutilidadTot,ES_MutilidadENTIDAD)
             ,c(ES_MarrendaTot,ES_MarrendaENTIDAD),c(ES_MtransferTot,ES_MtransferENTIDAD),c(ES_MjubilacionTot,ES_MjubilacionENTIDAD),c(ES_MbecasTot,ES_MbecasENTIDAD),
             c(ES_MdonativosTot,ES_MdonativosENTIDAD),c(ES_MremesasTot,ES_MremesasENTIDAD),c(ES_Mbene_gobTot,ES_Mbene_gobENTIDAD),c(ES_Mtransf_hogTot,ES_Mtransf_hogENTIDAD)
             ,c(ES_Mtrans_instTot,ES_Mtrans_instENTIDAD),c(ES_Mestim_alquTot,ES_Mestim_alquENTIDAD),c(ES_Motros_ingTot,ES_Motros_ingENTIDAD))
##### ERROR ESTANDAR
c_ENTIDAD_SE <-
  data.frame(c(SE_Ming_corTot,SE_Ming_corENTIDAD),c(SE_MingtrabTot,SE_MingtrabENTIDAD),c(SE_MtrabajoTot,SE_MtrabajoENTIDAD),c(SE_MnegocioTot,SE_MnegocioENTIDAD)
             ,c(SE_Motros_trabTot,SE_Motros_trabENTIDAD),c(SE_MrentasTot,SE_MrentasENTIDAD),c(SE_MutilidadTot,SE_MutilidadENTIDAD)
             ,c(SE_MarrendaTot,SE_MarrendaENTIDAD),c(SE_MtransferTot,SE_MtransferENTIDAD),c(SE_MjubilacionTot,SE_MjubilacionENTIDAD),c(SE_MbecasTot,SE_MbecasENTIDAD),
             c(SE_MdonativosTot,SE_MdonativosENTIDAD),c(SE_MremesasTot,SE_MremesasENTIDAD),c(SE_Mbene_gobTot,SE_Mbene_gobENTIDAD),c(SE_Mtransf_hogTot,SE_Mtransf_hogENTIDAD),c(SE_Mtrans_instTot,SE_Mtrans_instENTIDAD)
             ,c(SE_Mestim_alquTot,SE_Mestim_alquENTIDAD),c(SE_Motros_ingTot,SE_Motros_ingENTIDAD))



# se agregan los nombres de las entidades a las filas
#esta cadena est? bien loca, no?
ENTIDADES<-c(entidades<-c("MExico","Aguascalientes","Baja California","Baja California Sur","Campeche","Coahuila de Zaragoza",
                          "Colima","Chiapas","Chihuahua","Ciudad de Mexico","Durango","Guanajuato","Guerrero","Hidalgo",
                          "Jalisco","Mexico","Michoaca¡n de Ocampo","Morelos","Nayarit","Nuevo Leon","Oaxaca","Puebla",
                          "Queretaro","Quintana Roo","San Luis Potosi","Sinaloa","Sonora","Tabasco","Tamaulipas","Tlaxcala","Veracruz de Ignacio de la Llave","Yucatan","Zacatecas"))

row.names(c_ENTIDAD_ES)<-row.names(c_ENTIDAD_SE)<-ENTIDADES

#ahora vamos a ponerle nombre a las columnas
names(c_ENTIDAD_ES)=c("ING COR2012", "TRABAJO2012", "SUBORDINADO2012", "NEGOCIOS2012","OTROS TRAB2012", "RENTAS2012","UTILIDAD2012", "ARRENDA2012", "TRANSFER2012","JUBILACION2012", "BECAS2012", "DONATIVOS2012", "REMESAS2012", "BENEGOBIERNO2012", "TRANS HOG2012", "TRANS INST2012", "ESTIM ALQU2012", "OTROS INGRESOS2012")

names(c_ENTIDAD_SE)=c("ING COR2012", "TRABAJO2012", "SUBORDINADO2012", "NEGOCIOS2012","OTROS TRAB2012", "RENTAS2012","UTILIDAD2012", "ARRENDA2012", "TRANSFER2012","JUBILACION2012", "BECAS2012", "DONATIVOS2012", "REMESAS2012", "BENEGOBIERNO2012", "TRANS HOG2012", "TRANS INST2012", "ESTIM ALQU2012", "OTROS INGRESOS2012")


#ahora, lo que podemos hacer es mostrar los cuadros en la consola redondeados
# el comando round, redondea las cifra para mostrar, en el caso del coeficiente de variaci?n redondea a 4 decimales y luego multiplica por cien.
# Mostramos el resultado en pantalla
round(c_ENTIDAD_ES)
round(c_ENTIDAD_SE)


prueba<-c_ENTIDAD_ES%>%
  mutate(`ING COR2012`=`ING COR2012`, prueba=TRABAJO2012+RENTAS2012+JUBILACION2012+BECAS2012+DONATIVOS2012+REMESAS2012+BENEGOBIERNO2012+`TRANS HOG2012`+`TRANS INST2012`+`ESTIM ALQU2012`+`OTROS INGRESOS2012`)

all.equal(prueba$`ING COR2012`,prueba$prueba)

########## consumo por ENTIDAD 



Consumo_por_ENTIDAD <- svyby(~GASTO,denominator=~Nhog,by=~ENTIDAD,mydesign,svyratio)

Consumo_promedio <- svyratio(~GASTO,denominator=~Nhog,mydesign) 

SE_consumo_Tot <- SE (Consumo_promedio)
SE_consumo_ENTIDAD <- SE (Consumo_por_ENTIDAD)

Consumo_por_ENTIDAD <- Consumo_por_ENTIDAD[[2]] 
Consumo_promedio <- Consumo_promedio[[1]]

Consumo<-data.frame(c(Consumo_promedio,Consumo_por_ENTIDAD))

ENTIDADES<-c(entidades<-c("MExico","Aguascalientes","Baja California","Baja California Sur","Campeche","Coahuila de Zaragoza",
                          "Colima","Chiapas","Chihuahua","Ciudad de Mexico","Durango","Guanajuato","Guerrero","Hidalgo",
                          "Jalisco","Mexico","Michoaca¡n de Ocampo","Morelos","Nayarit","Nuevo Leon","Oaxaca","Puebla",
                          "Queretaro","Quintana Roo","San Luis Potosi","Sinaloa","Sonora","Tabasco","Tamaulipas","Tlaxcala","Veracruz de Ignacio de la Llave","Yucatan","Zacatecas"))

row.names(Consumo)<-ENTIDADES

Consumo_SE<-data.frame(c(SE_consumo_Tot,SE_consumo_ENTIDAD))
row.names(Consumo_SE)<-ENTIDADES





write.dbf(Consumo,file = "ESTADOS Consumo bottom 40 2012.dbf")
write.dbf(Consumo_SE,file="ESTADOS Consumo bottom 40 2012 SE.dbf")
write.dbf(c_ENTIDAD_ES,file = "ESTADOS bottom 40 por fuente por ENTIDAD estimaciones 2012.dbf")
write.dbf(c_ENTIDAD_SE,file = "ESTADOS bottom 40 fuente por ENTIDAD errores standard 2012.dbf")

rm(list = ls())



################# ingreso por estado upper 60 ######################
library(foreign)
library(survey)
library(doBy)
library(reldist)
library(tidyverse)
options(survey.lonely.psu="adjust")

#reading the data
setwd("C:/Users/Erick/onedrive/GIC/GITHUB2018/GIC/ENIGH_2012/ENIGH2012")
Conc2012<-read.dbf("Conc_2012.dbf",as.is = T)


names(Conc2012)<-c("ENTIDAD","FOLIOVIV","FOLIOHOG","GASTO","TOT_INTEG","INGCOR","INGTRAB","TRABAJO","NEGOCIO","OTROS_TRAB",
                   "RENTAS","UTILIDAD","ARRENDA","TRANSFER","JUBILA","BECA","DONATIVO","REMESA","BENE_GOB",
                   "ESP_HOG","ESP_INST","ESTI","OTROS","FACTOR","UPM","EST_DIS","tam_localidad","Small","HOGARINDIG","NOMBRE_ENT",
                   "DEFLACTORES","Nhog","TAM_DECIL","MAXT","ACUMULA","ACUMULA2","DECIL","Bottom_40")

Conc2012<-Conc2012%>%
  filter(Bottom_40==0)




mydesign <- svydesign(id=~UPM,strata=~EST_DIS,data=Conc2012,weights=~FACTOR)

#vamos por el ingreso corriente total del pa?s
# ing_ cor se define como La suma de las variables ingtrab, rentas, transfer, estim_alqu y otros_ing.
#te sale que el ingreso trimestra promedio en Mexico es de 49,610.
#notes? que esto no es otra cosa que el ing_cor*factor/34744819
upper60_Ming_corTot <- svyratio(~INGCOR,denominator=~Nhog,mydesign) 

#ahora, vamos a hacer lo mismo por ENTIDAD
#aqu? cmabia la funci?n a svyby, en by va el ENTIDAD que creamos.
#y al final va la funci?n que queremos
upper60_Ming_corENTIDAD <- svyby(~INGCOR,denominator=~Nhog,by=~ENTIDAD,mydesign,svyratio)


#     Trabajo
#
#El trabajo se divide en tres clasificaciones: subordinado, independiente y otros.
### ingreso del trabajo total###
upper60_MingtrabTot <- svyratio(~INGTRAB,denominator=~Nhog,mydesign) # Total promedio
upper60_MingtrabENTIDAD <- svyby(~INGTRAB,denominator=~Nhog,by=~ENTIDAD ,mydesign,svyratio) # por ENTIDAD
###### ingreso del trabajo subordinado
upper60_MtrabajoTot <- svyratio(~TRABAJO,denominator=~Nhog,mydesign) # Total promedio
upper60_MtrabajoENTIDAD <- svyby(~TRABAJO,denominator=~Nhog,by=~ENTIDAD ,mydesign,svyratio) # por ENTIDAD
###### ingreso del trabajo independiente
upper60_MnegocioTot <- svyratio(~NEGOCIO,denominator=~Nhog,mydesign) # Total promedio
upper60_MnegocioENTIDAD <- svyby(~NEGOCIO,denominator=~Nhog,by=~ENTIDAD ,mydesign,svyratio) # por ENTIDAD
###### ingreso de otros trabajos
upper60_Motros_trabTot <- svyratio(~OTROS_TRAB,denominator=~Nhog,mydesign) # Total promedio
upper60_Motros_trabENTIDAD<- svyby(~OTROS_TRAB,denominator=~Nhog,by=~ENTIDAD,mydesign,svyratio) # por ENTIDAD


###################################        Rentas de la propiedad 

#la renta de la propiedad se divide en: ingresos de sociedades y arrendamientos.

#ingresos totales por renta de la porpiedad
upper60_MrentasTot <- svyratio(~RENTAS,denominator=~Nhog,mydesign) # Total promedio
upper60_MrentasENTIDAD <- svyby(~RENTAS,denominator=~Nhog,by=~ENTIDAD ,mydesign,svyratio) #Por ENTIDAD
###### ingresos de sociedades
upper60_MutilidadTot <- svyratio(~UTILIDAD,denominator=~Nhog,mydesign) # Total promedio
upper60_MutilidadENTIDAD <- svyby(~UTILIDAD,denominator=~Nhog,by=~ENTIDAD ,mydesign,svyratio) # por ENTIDAD
###### arrendamiento
upper60_MarrendaTot <- svyratio(~ARRENDA,denominator=~Nhog,mydesign) # Total promedio
upper60_MarrendaENTIDAD <- svyby(~ARRENDA,denominator=~Nhog,by=~ENTIDAD ,mydesign,svyratio) # Por ENTIDAD


###################################        Transferencias   

#las transferencias totales se definen como la suma de jubilacion, becas, donativos, remesas, bene_gob, transf_hog y trans_inst.

upper60_MtransferTot <- svyratio(~TRANSFER,denominator=~Nhog,mydesign) # Total promedio
upper60_MtransferENTIDAD <- svyby(~TRANSFER,denominator=~Nhog,by=~ENTIDAD ,mydesign,svyratio) # ENTIDAD

###### jubilacion se define como Jubilaciones, pensiones e indemnizaciones por accidente de trabajo despido y retiro voluntario.
#En el cuestionario solo se les pregunta si recibi? jubilaciones. As? que puede ser p?blicas o privadas.

upper60_MjubilacionTot <- svyratio(~JUBILA,denominator=~Nhog,mydesign) # Total promedio
upper60_MjubilacionENTIDAD <- svyby(~JUBILA,denominator=~Nhog,by=~ENTIDAD ,mydesign,svyratio) # ENTIDAD

###### becas que pueden ser, de nuevo, p?blicas privadas. 
upper60_MbecasTot <- svyratio(~BECA,denominator=~Nhog,mydesign) # Total promedio
upper60_MbecasENTIDAD <- svyby(~BECA,denominator=~Nhog,by=~ENTIDAD ,mydesign,svyratio) # ENTIDAD

###### donativos que tambi?n pueden ser p?blicos o privados.
upper60_MdonativosTot <- svyratio(~DONATIVO,denominator=~Nhog,mydesign) # Total promedio
upper60_MdonativosENTIDAD <- svyby(~DONATIVO,denominator=~Nhog,by=~ENTIDAD ,mydesign,svyratio) # ENTIDAD

###### remesas se definen como ingresos provenientes d eotros paises. As? de manera gen?rica.
upper60_MremesasTot <- svyratio(~REMESA,denominator=~Nhog,mydesign) # Total promedio
upper60_MremesasENTIDAD <- svyby(~REMESA,denominator=~Nhog,by=~ENTIDAD ,mydesign,svyratio) # ENTIDAD

###### bene_gob:  aqu? estna los programas p?blicos. Prospera, procampo, 65 y m?s, adultos mayores, sin hambre, empleo tempora y Otros.
upper60_Mbene_gobTot <- svyratio(~BENE_GOB,denominator=~Nhog,mydesign) # Total promedio
upper60_Mbene_gobENTIDAD <- svyby(~BENE_GOB,denominator=~Nhog,by=~ENTIDAD ,mydesign,svyratio) # ENTIDAD

###### transf_hog:  Esto es lo que transfiere otro hogar.
upper60_Mtransf_hogTot <- svyratio(~ESP_HOG,denominator=~Nhog,mydesign) # Total promedio
upper60_Mtransf_hogENTIDAD <- svyby(~ESP_HOG,denominator=~Nhog,by=~ENTIDAD ,mydesign,svyratio) #ENTIDAD

###### trans_inst: puede venir de institucione sp?blicas o privadas.
upper60_Mtrans_instTot <- svyratio(~ESP_INST,denominator=~Nhog,mydesign) # Total promedio
upper60_Mtrans_instENTIDAD <- svyby(~ESP_INST,denominator=~Nhog,by=~ENTIDAD ,mydesign,svyratio) # ENTIDAD


### estim_alqu ### Aparentemente se le pregunta al entrevistado cu?nto constar?a la renta del lugar donde vive.
upper60_Mestim_alquTot <- svyratio(~ESTI,denominator=~Nhog,mydesign) # Total promedio
upper60_Mestim_alquENTIDAD <- svyby(~ESTI,denominator=~Nhog,by=~ENTIDAD ,mydesign,svyratio) # ENTIDAD


### otros_ing ### es literalmente ?algo m?s?
upper60_Motros_ingTot <- svyratio(~OTROS,denominator=~Nhog,mydesign) # Total promedio
upper60_Motros_ingENTIDAD <- svyby(~OTROS,denominator=~Nhog,by=~ENTIDAD ,mydesign,svyratio) # ENTIDAD


######################################### Estimaciones 

ES_Ming_corTot <- upper60_Ming_corTot[[1]] #lo que estoy haciendo aqu? es extraer el valor de la primera columa que corresponde al c?lculo.
ES_Ming_corENTIDAD <- upper60_Ming_corENTIDAD[[2]] #En el caso de las entidades, los c?lculos quedaron en la segunda columna

ES_MingtrabTot <- upper60_MingtrabTot[[1]]
ES_MingtrabENTIDAD <- upper60_MingtrabENTIDAD[[2]]

ES_MtrabajoTot <- upper60_MtrabajoTot[[1]]
ES_MtrabajoENTIDAD <- upper60_MtrabajoENTIDAD[[2]]

ES_MnegocioTot <- upper60_MnegocioTot[[1]]
ES_MnegocioENTIDAD <- upper60_MnegocioENTIDAD[[2]]

ES_Motros_trabTot <- upper60_Motros_trabTot [[1]]
ES_Motros_trabENTIDAD <- upper60_Motros_trabENTIDAD [[2]]

ES_MrentasTot <- upper60_MrentasTot [[1]]
ES_MrentasENTIDAD <- upper60_MrentasENTIDAD [[2]]

ES_MutilidadTot <- upper60_MutilidadTot [[1]]
ES_MutilidadENTIDAD <- upper60_MutilidadENTIDAD [[2]]

ES_MarrendaTot <- upper60_MarrendaTot [[1]]
ES_MarrendaENTIDAD <- upper60_MarrendaENTIDAD [[2]]

ES_MtransferTot <- upper60_MtransferTot[[1]]
ES_MtransferENTIDAD <- upper60_MtransferENTIDAD[[2]]

ES_MjubilacionTot <- upper60_MjubilacionTot [[1]]
ES_MjubilacionENTIDAD <- upper60_MjubilacionENTIDAD [[2]]

ES_MbecasTot <- upper60_MbecasTot [[1]]
ES_MbecasENTIDAD <- upper60_MbecasENTIDAD [[2]]

ES_MdonativosTot <- upper60_MdonativosTot[[1]]
ES_MdonativosENTIDAD <- upper60_MdonativosENTIDAD[[2]]

ES_MremesasTot <- upper60_MremesasTot[[1]]
ES_MremesasENTIDAD <- upper60_MremesasENTIDAD[[2]]

ES_Mbene_gobTot <- upper60_Mbene_gobTot [[1]]
ES_Mbene_gobENTIDAD <- upper60_Mbene_gobENTIDAD [[2]]

ES_Mtransf_hogTot <- upper60_Mtransf_hogTot [[1]]
ES_Mtransf_hogENTIDAD <- upper60_Mtransf_hogENTIDAD [[2]]

ES_Mtrans_instTot <- upper60_Mtrans_instTot[[1]]
ES_Mtrans_instENTIDAD <- upper60_Mtrans_instENTIDAD[[2]]

ES_Mestim_alquTot <- upper60_Mestim_alquTot [[1]]
ES_Mestim_alquENTIDAD <- upper60_Mestim_alquENTIDAD [[2]]

ES_Motros_ingTot <- upper60_Motros_ingTot [[1]]
ES_Motros_ingENTIDAD <- upper60_Motros_ingENTIDAD [[2]]

########## Error Est?ndar 
SE_Ming_corTot <- SE (upper60_Ming_corTot)
SE_Ming_corENTIDAD <- SE (upper60_Ming_corENTIDAD)

SE_MingtrabTot <- SE (upper60_MingtrabTot)
SE_MingtrabENTIDAD <- SE (upper60_MingtrabENTIDAD)

SE_MtrabajoTot <- SE (upper60_MtrabajoTot)
SE_MtrabajoENTIDAD <- SE (upper60_MtrabajoENTIDAD)

SE_MnegocioTot <- SE (upper60_MnegocioTot)
SE_MnegocioENTIDAD <- SE (upper60_MnegocioENTIDAD)

SE_Motros_trabTot <- SE (upper60_Motros_trabTot)
SE_Motros_trabENTIDAD <- SE (upper60_Motros_trabENTIDAD)

SE_MrentasTot <- SE (upper60_MrentasTot)
SE_MrentasENTIDAD <- SE (upper60_MrentasENTIDAD)

SE_MutilidadTot <- SE (upper60_MutilidadTot)
SE_MutilidadENTIDAD <- SE (upper60_MutilidadENTIDAD)

SE_MarrendaTot <- SE (upper60_MarrendaTot)
SE_MarrendaENTIDAD <- SE (upper60_MarrendaENTIDAD)

SE_MtransferTot <- SE (upper60_MtransferTot)
SE_MtransferENTIDAD <- SE (upper60_MtransferENTIDAD)

SE_MjubilacionTot <- SE (upper60_MjubilacionTot)
SE_MjubilacionENTIDAD <- SE (upper60_MjubilacionENTIDAD)

SE_MbecasTot <- SE (upper60_MbecasTot)
SE_MbecasENTIDAD <- SE (upper60_MbecasENTIDAD)

SE_MdonativosTot <- SE (upper60_MdonativosTot)
SE_MdonativosENTIDAD <- SE (upper60_MdonativosENTIDAD)

SE_MremesasTot <- SE (upper60_MremesasTot)
SE_MremesasENTIDAD <- SE (upper60_MremesasENTIDAD)

SE_Mbene_gobTot <- SE (upper60_Mbene_gobTot)
SE_Mbene_gobENTIDAD <- SE (upper60_Mbene_gobENTIDAD)

SE_Mtransf_hogTot <- SE (upper60_Mtransf_hogTot)
SE_Mtransf_hogENTIDAD <- SE (upper60_Mtransf_hogENTIDAD)

SE_Mtrans_instTot <- SE (upper60_Mtrans_instTot)
SE_Mtrans_instENTIDAD <- SE (upper60_Mtrans_instENTIDAD)

SE_Mestim_alquTot <- SE (upper60_Mestim_alquTot)
SE_Mestim_alquENTIDAD <- SE (upper60_Mestim_alquENTIDAD)

SE_Motros_ingTot <- SE (upper60_Motros_ingTot)
SE_Motros_ingENTIDAD <- SE (upper60_Motros_ingENTIDAD)


#############################      Cuadros   
#este cuadro, lo ?nico que tiene son todas la estimaciones.
#son 10 filas y 18 columnas.
c_ENTIDAD_ES <-
  data.frame(c(ES_Ming_corTot,ES_Ming_corENTIDAD),c(ES_MingtrabTot,ES_MingtrabENTIDAD),c(ES_MtrabajoTot,ES_MtrabajoENTIDAD),c(ES_MnegocioTot,ES_MnegocioENTIDAD)
             ,c(ES_Motros_trabTot,ES_Motros_trabENTIDAD),c(ES_MrentasTot,ES_MrentasENTIDAD),c(ES_MutilidadTot,ES_MutilidadENTIDAD)
             ,c(ES_MarrendaTot,ES_MarrendaENTIDAD),c(ES_MtransferTot,ES_MtransferENTIDAD),c(ES_MjubilacionTot,ES_MjubilacionENTIDAD),c(ES_MbecasTot,ES_MbecasENTIDAD),
             c(ES_MdonativosTot,ES_MdonativosENTIDAD),c(ES_MremesasTot,ES_MremesasENTIDAD),c(ES_Mbene_gobTot,ES_Mbene_gobENTIDAD),c(ES_Mtransf_hogTot,ES_Mtransf_hogENTIDAD)
             ,c(ES_Mtrans_instTot,ES_Mtrans_instENTIDAD),c(ES_Mestim_alquTot,ES_Mestim_alquENTIDAD),c(ES_Motros_ingTot,ES_Motros_ingENTIDAD))
##### ERROR ESTANDAR
c_ENTIDAD_SE <-
  data.frame(c(SE_Ming_corTot,SE_Ming_corENTIDAD),c(SE_MingtrabTot,SE_MingtrabENTIDAD),c(SE_MtrabajoTot,SE_MtrabajoENTIDAD),c(SE_MnegocioTot,SE_MnegocioENTIDAD)
             ,c(SE_Motros_trabTot,SE_Motros_trabENTIDAD),c(SE_MrentasTot,SE_MrentasENTIDAD),c(SE_MutilidadTot,SE_MutilidadENTIDAD)
             ,c(SE_MarrendaTot,SE_MarrendaENTIDAD),c(SE_MtransferTot,SE_MtransferENTIDAD),c(SE_MjubilacionTot,SE_MjubilacionENTIDAD),c(SE_MbecasTot,SE_MbecasENTIDAD),
             c(SE_MdonativosTot,SE_MdonativosENTIDAD),c(SE_MremesasTot,SE_MremesasENTIDAD),c(SE_Mbene_gobTot,SE_Mbene_gobENTIDAD),c(SE_Mtransf_hogTot,SE_Mtransf_hogENTIDAD),c(SE_Mtrans_instTot,SE_Mtrans_instENTIDAD)
             ,c(SE_Mestim_alquTot,SE_Mestim_alquENTIDAD),c(SE_Motros_ingTot,SE_Motros_ingENTIDAD))



# se agregan los nombres de las entidades a las filas
#esta cadena est? bien loca, no?
ENTIDADES<-c(entidades<-c("MExico","Aguascalientes","Baja California","Baja California Sur","Campeche","Coahuila de Zaragoza",
                          "Colima","Chiapas","Chihuahua","Ciudad de Mexico","Durango","Guanajuato","Guerrero","Hidalgo",
                          "Jalisco","Mexico","Michoaca¡n de Ocampo","Morelos","Nayarit","Nuevo Leon","Oaxaca","Puebla",
                          "Queretaro","Quintana Roo","San Luis Potosi","Sinaloa","Sonora","Tabasco","Tamaulipas","Tlaxcala","Veracruz de Ignacio de la Llave","Yucatan","Zacatecas"))

row.names(c_ENTIDAD_ES)<-row.names(c_ENTIDAD_SE)<-ENTIDADES

#ahora vamos a ponerle nombre a las columnas
names(c_ENTIDAD_ES)=c("ING COR2012", "TRABAJO2012", "SUBORDINADO2012", "NEGOCIOS2012","OTROS TRAB2012", "RENTAS2012","UTILIDAD2012", "ARRENDA2012", "TRANSFER2012","JUBILACION2012", "BECAS2012", "DONATIVOS2012", "REMESAS2012", "BENEGOBIERNO2012", "TRANS HOG2012", "TRANS INST2012", "ESTIM ALQU2012", "OTROS INGRESOS2012")

names(c_ENTIDAD_SE)=c("ING COR2012", "TRABAJO2012", "SUBORDINADO2012", "NEGOCIOS2012","OTROS TRAB2012", "RENTAS2012","UTILIDAD2012", "ARRENDA2012", "TRANSFER2012","JUBILACION2012", "BECAS2012", "DONATIVOS2012", "REMESAS2012", "BENEGOBIERNO2012", "TRANS HOG2012", "TRANS INST2012", "ESTIM ALQU2012", "OTROS INGRESOS2012")


#ahora, lo que podemos hacer es mostrar los cuadros en la consola redondeados
# el comando round, redondea las cifra para mostrar, en el caso del coeficiente de variaci?n redondea a 4 decimales y luego multiplica por cien.
# Mostramos el resultado en pantalla
round(c_ENTIDAD_ES)
round(c_ENTIDAD_SE)


prueba<-c_ENTIDAD_ES%>%
  mutate(`ING COR2012`=`ING COR2012`, prueba=TRABAJO2012+RENTAS2012+JUBILACION2012+BECAS2012+DONATIVOS2012+REMESAS2012+BENEGOBIERNO2012+`TRANS HOG2012`+`TRANS INST2012`+`ESTIM ALQU2012`+`OTROS INGRESOS2012`)

all.equal(prueba$`ING COR2012`,prueba$prueba)

########## consumo por ENTIDAD 



Consumo_por_ENTIDAD <- svyby(~GASTO,denominator=~Nhog,by=~ENTIDAD,mydesign,svyratio)

Consumo_promedio <- svyratio(~GASTO,denominator=~Nhog,mydesign) 

SE_consumo_Tot <- SE (Consumo_promedio)
SE_consumo_ENTIDAD <- SE (Consumo_por_ENTIDAD)

Consumo_por_ENTIDAD <- Consumo_por_ENTIDAD[[2]] 
Consumo_promedio <- Consumo_promedio[[1]]

Consumo<-data.frame(c(Consumo_promedio,Consumo_por_ENTIDAD))

ENTIDADES<-c(entidades<-c("MExico","Aguascalientes","Baja California","Baja California Sur","Campeche","Coahuila de Zaragoza",
                          "Colima","Chiapas","Chihuahua","Ciudad de Mexico","Durango","Guanajuato","Guerrero","Hidalgo",
                          "Jalisco","Mexico","Michoaca¡n de Ocampo","Morelos","Nayarit","Nuevo Leon","Oaxaca","Puebla",
                          "Queretaro","Quintana Roo","San Luis Potosi","Sinaloa","Sonora","Tabasco","Tamaulipas","Tlaxcala","Veracruz de Ignacio de la Llave","Yucatan","Zacatecas"))

row.names(Consumo)<-ENTIDADES

Consumo_SE<-data.frame(c(SE_consumo_Tot,SE_consumo_ENTIDAD))
row.names(Consumo_SE)<-ENTIDADES





write.dbf(Consumo,file = "ESTADOS Consumo upper 60 2012.dbf")
write.dbf(Consumo_SE,file="ESTADOS Consumo upper 60 2012 SE.dbf")
write.dbf(c_ENTIDAD_ES,file = "ESTADOS upper 60 por fuente por ENTIDAD estimaciones 2012.dbf")
write.dbf(c_ENTIDAD_SE,file = "ESTADOS upper 60 fuente por ENTIDAD errores standard 2012.dbf")

rm(list = ls())


