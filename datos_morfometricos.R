install.packages("gridExtra")
install.packages("effectsize")
install.packages("nicheROVER")
install.packages("vegan")
install.packages("maptools")
install.packages("plyr")
library(nicheROVER)
library(vegan)
library(ggplot2)
library(gridExtra)
library(ggplot2)
library(dplyr)
library(effectsize)
library(maptools)
library( plyr)

#  alt+shif+k      Muestra kista de atajos
#  alt+shif+j      ir lista de seciones
#  ctrl+shif+R     Se inserta una label de sección

###########################  DATOS MORFOMETRICOS   ##############################

# LEEMOS LA BASE DE DATOS
BD_morfo<-read.csv("BD datos.csv",header = TRUE, dec=",",sep = ";")
str(BD_morfo)
summary(BD_morfo)
BD_morfo$Fecha <- as.Date(BD_morfo$Fecha, format = "%d-%m-%y")    # Cambiamo formato date
class(BD_morfo$Fecha)
summary(BD_morfo$Fecha)
?date

fechas_distintas <- BD_morfo %>% distinct(Fecha) # Cuantas fechas distintas hay

Fechas
# 1 2020-11-03       #  Encontramos que tenemos 4 fechas  distintas en la base de datos
# 2 2020-01-07
# 3 2020-05-13
#v4 2020-05-14BD_morfo <- BD_morfo %>%

# Generamos con las 4 fechas, 3 Estaciones, Primavera, Verano y Otoño

BD_morfo<- BD_morfo %>%
  mutate(Estacion = case_when(
    Fecha == "03-11-2022" ~ "Primavera",
    Fecha == "07-01-2023" ~ "Verano",
    Fecha == "13-05-2023" ~ "Otoño",
    Fecha == "14-05-2023" ~ "Otoño",
    TRUE ~ ""
  ))


names(BD_morfo)   # los nombres de las variables base de datos

# Dividimos la variable Long_cef en una nueva variable grupo, juvenil y adulto, usando dplyr
BD_morfo <- BD_morfo %>%
  mutate(Categoria = ifelse(Long_cef <= 30.00, "Juvenil", "Adulto"))

# Aqui le damos la clase correspondiente a las variables de tipo factor
BD_morfo$Localidad<-as.factor(BD_morfo$Localidad)
BD_morfo$Sexo<-as.factor(BD_morfo$Sexo)
BD_morfo$Art_Pesca<-as.factor(BD_morfo$Art_Pesca)
BD_morfo$Categoria<-as.factor(BD_morfo$Categoria)

#################    CALCULO DEL INDICE SMII  #########################

 
longitud_2 <- BD_morfo$Long_cef  # longitud corporal
length(longitud_2)
masa_2 <- BD_morfo$Peso..grs.     # masa corporal
length(masa_2)

SMA_2 <- (longitud_2 - mean(longitud_2)) / sd(longitud_2)  # Calcular el SMA_2
bSMA_2 <- exp(SMA_2)  #Calcular bSMA_2
BD_morfo$SMII_2 <- masa_2 / bSMA_2 # Agregar la columna SMII_2 a BD_morfo


# Definir el orden deseado de las Estaciones

BD_morfo$Estacion<-as.factor(BD_morfo$Estacion)
levels(BD_morfo$Estacion)
orden_estacion <- c("Primavera","Verano", "Otoño")
BD_morfo$Estacion <- factor(BD_morfo$Estacion, levels = orden_estacion)

#####  --------------------------------------------------
levels(BD_morfo$Localidad)
# [1] "Conanoxa"      "Cuya"          "Desembocadura" "Huancarane"    "Taltape Bajo" 

# Definir el orden deseado de las localidades
orden_localidades <- c("Desembocadura","Cuya", "Conanoxa", "Taltape Bajo","Huancarane")

# Convertir la variable localidad a factor con el orden deseado

BD_morfo$Localidad <- factor(BD_morfo$Localidad, levels = orden_localidades)

# Revisamos nuevamente el orden
levels(BD_morfo$Localidad)

# Revisamoas los niveles de Sexo
levels(BD_morfo$Sexo)

dev.off()  ### Limpia la ventana de los graficos


#### Aqui revisamos algunos histogramas    ......................

names(BD_morfo)
summary(BD_morfo)


histogram(BD_morfo$Peso..grs.)
hist(BD_morfo$Long_cef)
hist(BD_morfo$Long_Total)
hist(BD_morfo$Long_Total)
hist(BD_morfo$SMII_2)

###############   GRAFICOS DE BARRAS    ####################

# Filtramos por Long_cef > 21.58 y sin categoría SEXO = indeterminados
BD_morfo_menor_inde <- subset(BD_morfo, Long_cef > "21,58" & !BD_morfo$Sexo=="Indet")
summary(BD_morfo_menor_inde)


# Cantidad de individuos por localidad y sexo
ggplot(data = BD_morfo_menor_inde, aes(x = Localidad, fill = Sexo)) +
  geom_bar(position = "dodge")+labs(y= "Frecuencia")

# Cantidad de individuos por categoria y sexo
ggplot(data = BD_morfo_menor_inde, aes(x = Categoria, fill = Sexo)) +
  geom_bar(position = "dodge")+labs(y= "Frecuencia")

# Cantidad de individuos por localidad y categoria
ggplot(data = BD_morfo, aes(x = Localidad, fill = Categoria)) +
  geom_bar(position = "dodge")+labs(y= "Frecuencia")

# Cantidad de individuos por localidad y Estacion
ggplot(data = BD_morfo, aes(x = Localidad, fill = Estacion)) +
  geom_bar(position = "dodge")+labs(y= "Frecuencia")

# Cantidad de individuos por Estacion y sexo
ggplot(data = BD_morfo_menor_inde, aes(x = Estacion, fill = Sexo)) +
  geom_bar(position = "dodge")+labs(y= "Frecuencia")

# Cantidad de individuos por Art_Pesca y sexo
ggplot(data = BD_morfo_menor_inde, aes(x = Art_Pesca, fill = Sexo)) +
  geom_bar(position = "dodge")+labs(y= "Frecuencia")

# Cantidad de individuos por Art_Pesca y Categoria
ggplot(data = BD_morfo, aes(x = Art_Pesca, fill = Categoria)) +
  geom_bar(position = "dodge")+labs(y= "Frecuencia")


# Cantidad de individuos por estacion y Art_Pesca 
ggplot(data = BD_morfo, aes(x = Estacion, fill = Art_Pesca)) +
  geom_bar(position = "dodge")+labs(y= "Frecuencia")




names(BD_morfo)
################   TABLA RESUMN DE INFORMACION
################

data.resumen<-ddply(BD_morfo, c("Localidad","Estacion"), summarise,
             count=length(Localidad),
             p=mean(Peso..grs.), sp=sd(Peso..grs.),
             l=mean(Long_cef), sl=sd(Long_cef))
data.resumen<-as.data.frame(data.resumen)
dev.off()
ggsave("data.resumen1.png")
write.csv(data.resumen, "resumen_loc_est.csv")
write.table(data.resumen, "mi_tb.txt")
write.table(data.resumen, "vector.txt", fileEncoding = "UTF-16LE", quote = TRUE)

?write.csv

################################################################
##############  CLASE DE TALLA   ###############################
################################################################

histogram(~Temp | as.character(Month), data=airquality, 
          main="Estratificación de la temperatura por mes",
          xlab="Temperatura", ylab="Porcentaje del total", col="red")


histogram(~Long_cef | as.character(Estacion), data=BD_morfo, 
          main="Estratificación del Long_cef por Estacion",
          xlab="Estacion", ylab="Porcentaje del total", col="red")


# Realizando un diagrama de puntos
dotplot(~Long_cef | as.character(Estacion), data=BD_morfo, 
        main="Estratificación de la temperatura por mes",
        xlab="Temperatura", ylab="Porcentaje del total", col=1)



















x <- data.frame(a = I("a \" quote"), b = pi)
tf <- tempfile(fileext = ".csv")

# Crear una tabla con formato xtable
tabla_xtable <- xtable(data.resumen)

# Guardar la tabla en un archivo PDF
pdf("tabla_xtable.pdf")
print(tabla_xtable)
dev.off()
Cuando ejecutes este código, se generará un archivo PDF llamado "data_resumen.pdf" en el directorio de trabajo actual. Este archivo PDF contendrá una tabla con los datos del dataframe "data.resumen", formateada de manera adecuada y lista para su visualización o impresión. Cada columna de la tabla corresponderá a las variables del dataframe, y cada fila representará una observación con la media y desviación estándar correspondiente.







??ddply

#########  GRAFICOS BIPLOT Y TRIPLOT   ##############################
#====================================================================



camaron.triplots<-ggplot(BD_morfo, aes(x = Peso..grs., y = Long_cef, colour = Localidad)) +
  geom_point(alpha = 0.7, size=2) +
  facet_grid(. ~ Sexo) +
  theme_bw() +
  ylab("Long_cef") +
  xlab("Peso..grs.")
camaron.triplots

camaron2.triplots<-ggplot(BD_morfo, aes(x = Peso..grs., y = SMII_2, colour = Localidad)) +
  geom_point(alpha = 0.7, size=2) +
  facet_grid(. ~ Estacion) +
  theme_bw() +
  ylab("SMII_2") +
  xlab("Peso..grs.")
camaron2.triplots


+++++++++++++++##########
##########
##########  multiples variables 
ggplot(datos, aes(x = x)) +
  geom_line(aes(y = variable1, color = "Variable 1")) +
  geom_line(aes(y = variable2, color = "Variable 2")) +
  geom_line(aes(y = variable3, color = "Variable 3")) +
  # Agrega más líneas si tienes más variables para comparar
  labs(y = "Valores", color = "Variables") +
  theme_minimal()
###########
###########
###########



BD_morfo %>% filter(BD_morfo$Long_Total=="Femenino") %>%    #12
  
  ggplot(., aes(Fuma,  fill=Estatura)) + 
  geom_bar(position="dodge",colour="black") +
  
  labs(x= "Fuma", y="Frecuencias", fill="BD_morfo$Long_Total") +
  ylim(c(0,100)) +
  #xlim(c(0,300)) + 
  
  ggtitle("Diagrama de barras en el grupo de las mujeres") +   
  #theme_bw() + 
  theme_bw(base_size = 14) +
  #coord_flip() +
  
  #guides(fill=FALSE)+
  #scale_fill_manual(values = c("red","blue", "orange")) +
  
  geom_text(aes(label=..count..),stat='count',
            position=position_dodge(0.9),
            vjust=-0.5, 
            size=5.0) +
  
  facet_wrap(~"Distribución de estatura por fumadores y no fumadores")













hist(BD_morfo$, prob = TRUE,breaks = 200,
     main = "Histograma con curva normal", ylab = "Densidad")
x <- seq(min(BD_morfo$Peso..grs.), max(BD_morfo$Peso..grs.), length = 40)
f <- dnorm(x, mean = mean(BD_morfo$Peso..grs.), sd = sd(BD_morfo$Peso..grs.))
lines(x, f, col = "red", lwd = 2)

hist(BD_morfo$Peso..grs., prob = TRUE,breaks = 200,
     main = "Histograma con curva normal", ylab = "Densidad")
x <- seq(min(BD_morfo$Peso..grs.), max(BD_morfo$Peso..grs.), length = 40)
f <- dnorm(x, mean = mean(BD_morfo$Peso..grs.), sd = sd(BD_morfo$Peso..grs.))
lines(x, f, col = "red", lwd = 2)

hist(BD_morfo$SMII_2, prob = TRUE,
     main = "Histograma con curva normal", ylab = "Densidad")
x <- seq(min(BD_morfo$SMII_2), max(BD_morfo$SMII_2), length = 40)
f <- dnorm(x, mean = mean(BD_morfo$SMII_2), sd = sd(BD_morfo$SMII_2))
lines(x, f, col = "red", lwd = 2)





lines(density(distancia), lwd = 2, col = 'red')

hist(distancia, freq = FALSE, main = "Curva densidad", ylab = "Densidad")
lines(density(distancia), lwd = 2, col = 'red')

summary(BD_morfo$SMII_2)

lm1<-lm(BD_morfo$Long_cef~BD_morfo$Long_Total, data=BD_morfo)
summary(lm1)
dev.off()
plot(lm1)






################################## Estadisticos descriptivos   #####################################################

round(tapply(BD_morfo$Peso..grs., BD_morfo$Estacion, mean),1)
round(tapply(BD_morfo$SMII_2, list(BD_morfo$Sexo, BD_morfo$Estacion,BD_morfo$categoria), mean),1)
round(tapply(BD_morfo$SMII_2, list(BD_morfo$Localidad,BD_morfo$Sexo, BD_morfo$Estacion,BD_morfo$categoria), mean),1)
round(tapply(BD_morfo$SMII_2, list(BD_morfo$Sexo, BD_morfo$Estacion,BD_morfo$categoria), sd),1)
round(tapply(BD_morfo$SMII_2, list(BD_morfo$Sexo, BD_morfo$Estacion,BD_morfo$categoria), length),1)
round(tapply(BD_morfo$SMII_2, list(BD_morfo$Sexo, BD_morfo$Estacion,BD_morfo$categoria), min),1)
round(tapply(BD_morfo$SMII_2, list(BD_morfo$Sexo, BD_morfo$Estacion,BD_morfo$categoria), max),1)
round(tapply(BD_morfo$SMII_2, list(BD_morfo$Sexo, BD_morfo$Estacion,BD_morfo$categoria), max),1)

####################################################################################################################






 # par(mar = c(5, 4, 4, 2) + 0.1)    Ajusta los margenes del grafico

dev.off()  ### Limpia la ventana de los graficos

plot(BD_morfo$Long_Total,BD_morfo$Peso..grs.)   # Plot Peso..grs. vs  Long_Total 

plot(BD_morfo$Long_cef,BD_morfo$Peso..grs.)     # Plot Peso..grs. vs  Long_cef




### REVISAR GRAFICOS  #########
plot(BD_morfo$X15N,BD_morfo$Peso_grs)
plot(BD_morfo$X13C,BD_morfo$Peso_grs)
plot(BD_morfo$X34S,BD_morfo$Peso_grs)
plot(BD_morfo$X.N,BD_morfo$Peso_grs)
plot(BD_morfo$X.C,BD_morfo$Peso_grs)
plot(BD_morfo$C.N,BD_morfo$Peso_grs)
plot(BD_morfo$X15N,BD_morfo$SMII)
plot(BD_morfo$SMII,BD_morfo$BD_morfo$X13C)
plot(BD_morfo$SMII,BD_morfo$BD_morfo$X34S)
plot(BD_morfo$X34S,BD_morfo$SMII)

plot(datos.cuya$X15N,datos.cuya$Peso_grs)
plot(datos.Huancarane$X15N,datos.Huancarane$Peso_grs)
plot(datos.Conanoxa$X15N,datos.Conanoxa$Peso_grs)

dev.off()  ### Limpia la ventana de los graficos

## Aqui filtramos por Estacion
datos.cuya=BD_morfo[BD_morfo$Estacion=="Cuya",]
datos.Huancarane=BD_morfo[BD_morfo$Estacion=="Huancarane",]
datos.Conanoxa=BD_morfo[BD_morfo$Estacion=="Conanoxa",]
datos.Conanoxa=BD_morfo[BD_morfo$Estacion=="Conanoxa",(8:18)]

plot(datos.cuya$Peso_grs,datos.cuya$Long_Total)
plot(datos.Huancarane$Peso_grs,datos.Huancarane$Long_Total)
plot(datos.Conanoxa$Peso_grs,datos.Conanoxa$Long_Total)
names(datos.cuya)
mean(datos.cuya$Peso_grs)
mean(datos.Huancarane$Peso_gr)
mean(datos.Conanoxa$Peso_gr)



datos.juvenil=BD_morfo[BD_morfo$grupo=="Juvenil",]  ## Filtramos por grupo
datos.adulto=BD_morfo[BD_morfo$grupo=="Adulto",]

###################   Boxplot y anovas   _________________________________

boxplot(BD_morfo$SMII ~ BD_morfo$Estacion, data = BD_morfo) # SMII por estacion
boxplot(BD_morfo$Peso_grs ~ BD_morfo$SEXO, data = BD_morfo) # Hacemos un boxplot por sexo
anova_sexo<-aov(BD_morfo$Peso_grs~BD_morfo$SEXO,data=BD_morfo) # Realizamos un ANOVA
summary(anova_sexo)  #  Resumen del ANOVA
# Como el p-valor es menos a 0.05 se recha hipotesis nula H0 y se acepta la alternativa H1
# Lo que implica que hay diferencias significativas en la medias.


boxplot(BD_morfo$Peso_grs ~ BD_morfo$grupo, data = BD_morfo) # Hacemos un boxplot
# "Peso_grs" numerica con los niveles de la variable categorica "grupo"
anova_grupo<-aov(BD_morfo$Peso_grs~BD_morfo$grupo,data=BD_morfo) # Comparamos sus medias

# H0: Las medias son iguales entre juveniles y adultos 
# H1: Las media son distintas distinta

summary(anova_grupo)   # Como el p-valor es <  0.05 rechazamos la 
# hipotesis nula Ho y aceptamos la alternativa H1, o sea, las medias de juveniles
# y adultos son diferentes.

##### Test de tukey
resultado_tukey <- TukeyHSD(anova_grupo)
summary(resultado_tukey)

##### Test de Bonferroni
resultado_bonferroni <- pairwise.t.test(BD_morfo$Peso_grs, BD_morfo$grupo, p.adjust.method = "bonferroni")
print(resultado_bonferroni)

##### Test de comparaciones múltiples de Tukey-Kramer
resultado_tukey_kramer <- TukeyHSD(anova_grupo, alpha = 0.05)
print(resultado_tukey_kramer)

#______________________________________________________________________________________
# Comparacion general Estaciones vs pesos

boxplot(BD_morfo$Peso_grs ~ BD_morfo$Estacion, data = BD_morfo)

stripchart(BD_morfo$Peso_grs ~ BD_morfo$Estacion, vertical = TRUE, method = "jitter",
           pch = 20, add = TRUE, col = 1:length(levels(BD_morfo$Estacion)))

anova_estación<-aov(BD_morfo$Peso_grs~BD_morfo$Estacion,data=BD_morfo) # Comparamos sus medias

# H0: Las medias son iguales entre las estaciones 
# H1: Las media son al menos una distinta.

summary(anova_estación)   # Como el p-valor es <  0.05 rechazamos la 
# hipotesis nula Ho y aceptamos la alternativa H1, o sea, las medias al menos una es diferente.


##### Test de tukey
resultado_tukey <- TukeyHSD(anova_estación)
print(resultado_tukey)

##### Test de Bonferroni
resultado_bonferroni <- pairwise.t.test(BD_morfo$Peso_grs, BD_morfo$Estacion, p.adjust.method = "bonferroni")
print(resultado_bonferroni)

##### Test de comparaciones múltiples de Tukey-Kramer
resultado_tukey_kramer <- TukeyHSD(anova_estación, alpha = 0.05)
print(resultado_tukey_kramer)


#___________________________________________________________________________________________________
# pesos de adultos y juveniles de Cuya

boxplot(datos.cuya$Peso_grs ~ datos.cuya$grupo, data = datos.cuya)


boxplot(datos.Conanoxa$Peso_grs ~ datos.Conanoxa$grupo, data = datos.Conanoxa)
boxplot(datos.Huancarane$Peso_grs ~ datos.Huancarane$grupo, data = datos.Huancarane)



##ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
## X15N
boxplot(BD_morfo$X15N ~ BD_morfo$Estacion, data = BD_morfo)

stripchart(BD_morfo$X15N ~ BD_morfo$Estacion, vertical = TRUE, method = "jitter",
           pch = 20, add = TRUE, col = 1:length(levels(BD_morfo$Estacion)))

anova_X15N_estación<-aov(BD_morfo$X15N~BD_morfo$Estacion,data=BD_morfo)
summary(anova_X15N_estación)
##### Test de comparaciones múltiples de Tukey-Kramer
resultado_tukey_kramer <- TukeyHSD(anova_X15N_estación, alpha = 0.05)
print(resultado_tukey_kramer)
summary(BD_morfo$X15N)
length(BD_morfo$X15N)

# X13C
boxplot(BD_morfo$X13C ~ BD_morfo$Estacion, data = BD_morfo)

stripchart(BD_morfo$X13C ~ BD_morfo$Estacion, vertical = TRUE, method = "jitter",
           pch = 20, add = TRUE, col = 1:length(levels(BD_morfo$Estacion)))

anova_X13C_estación<-aov(BD_morfo$X13C~BD_morfo$Estacion,data=BD_morfo)
summary(anova_X13C_estación)
resultado_tukey_kramer <- TukeyHSD(anova_X13C_estación, alpha = 0.05)
print(resultado_tukey_kramer)


# X34S
boxplot(BD_morfo$X34S ~ BD_morfo$Estacion, data = BD_morfo)

stripchart(BD_morfo$X34S ~ BD_morfo$Estacion, vertical = TRUE, method = "jitter",
           pch = 20, add = TRUE, col = 1:length(levels(BD_morfo$Estacion)))

anova_X34S_estación<-aov(BD_morfo$X34S~BD_morfo$Estacion,data=BD_morfo)
summary(anova_X34S_estación)
resultado_tukey_kramer <- TukeyHSD(anova_X34S_estación, alpha = 0.05)
print(resultado_tukey_kramer)

summary(BD_morfo$X34S)
length(BD_morfo$X34S)

#  C.N
boxplot(BD_morfo$C.N ~ BD_morfo$Estacion, data = BD_morfo)

stripchart(BD_morfo$C.N ~ BD_morfo$Estacion, vertical = TRUE, method = "jitter",
           pch = 20, add = TRUE, col = 1:length(levels(BD_morfo$Estacion)))

anova_C.N_estación<-aov(BD_morfo$C.N~BD_morfo$Estacion,data=BD_morfo)
summary(anova_C.N_estación)
resultado_tukey_kramer <- TukeyHSD(anova_C.N_estación, alpha = 0.05)
print(resultado_tukey_kramer)




# ## dev.off() ------------------------------------------------------------


##ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
dev.off()
datos_filtrados <- subset(BD_morfo, SEXO == "Hembra")

# Crear el gráfico de caja para los datos filtrados
boxplot(Peso_grs ~ SEXO, data = datos_filtrados)


SEXO_hembra<-BD_morfo[BD_morfo$SEXO=="Hembra",]                             
summary(por_SEXO_Hembra)
SEXO_macho<-BD_morfo[BD_morfo$SEXO=="Macho",]
SEXO_indeter<-BD_morfo[BD_morfo$SEXO=="Indeter",]

##########  macho por estacion y X15











.








# macho-X15N
a<-boxplot(SEXO_macho$X15N ~ SEXO_macho$Estacion,data=SEXO_macho)
stripchart(SEXO_macho$X15N ~ SEXO_macho$Estacion, vertical = TRUE, method = "jitter",
           pch = 20, add = TRUE, col = 1:length(levels(SEXO_macho$Estacion)))

anova_X15N_estación_machos<-aov(SEXO_macho$X15N~SEXO_macho$Estacion,data=SEXO_macho)
summary(anova_X15N_estación_machos)
resultado_tukey_kramer <- TukeyHSD(anova_X15N_estación_machos, alpha = 0.05)
print(resultado_tukey_kramer)

# hembra-X15N
boxplot(SEXO_hembra$X15N ~ SEXO_hembra$Estacion,data=SEXO_hembra)
stripchart(SEXO_hembra$X15N ~ SEXO_hembra$Estacion, vertical = TRUE, method = "jitter",
           pch = 20, add = TRUE, col = 1:length(levels(SEXO_hembra$Estacion)))

anova_X15N_estación_hembras<-aov(SEXO_hembra$X15N~SEXO_hembra$Estacion,data=SEXO_hembra)
summary(anova_X15N_estación_hembras)
resultado_tukey_kramer <- TukeyHSD(anova_X15N_estación_hembras, alpha = 0.05)
print(resultado_tukey_kramer)

# indeterminado-X15N
c<-boxplot(SEXO_indeter$X15N ~ SEXO_indeter$Estacion,data=SEXO_indeter)
stripchart(SEXO_indeter$X15N ~ SEXO_indeter$Estacion, vertical = TRUE, method = "jitter",
           pch = 19, add = TRUE, col = 1:length(levels(SEXO_indeter$Estacion)))

anova_X15N_estación_indeter<-aov(SEXO_indeter$X15N~SEXO_indeter$Estacion,data=SEXO_indeter)
summary(anova_X15N_estación_indeter)

##########  macho por estacion y X13C
# macho-X13C

boxplot(SEXO_macho$X13C ~ SEXO_macho$Estacion,data=SEXO_macho)
stripchart(SEXO_macho$X13C ~ SEXO_macho$Estacion, vertical = TRUE, method = "jitter",
           pch = 20, add = TRUE, col = 1:length(levels(SEXO_macho$Estacion)))

anova_X13C_estación_machos<-aov(SEXO_macho$X13C~SEXO_macho$Estacion,data=SEXO_macho)
summary(anova_X13C_estación_machos)
resultado_tukey_kramer <- TukeyHSD(anova_X13C_estación_machos, alpha = 0.05)
print(resultado_tukey_kramer)

# hembra-X13C
boxplot(SEXO_hembra$X13C ~ SEXO_hembra$Estacion,data=SEXO_hembra)
stripchart(SEXO_hembra$X13C ~ SEXO_hembra$Estacion, vertical = TRUE, method = "jitter",
           pch = 20, add = TRUE, col = 1:length(levels(SEXO_hembra$Estacion)))

anova_X13C_estación_hembras<-aov(SEXO_hembra$X13C~SEXO_hembra$Estacion,data=SEXO_hembra)
summary(anova_X13C_estación_hembras)
resultado_tukey_kramer <- TukeyHSD(anova_X13C_estación_hembras, alpha = 0.05)
print(resultado_tukey_kramer)

# indeterminado-X13C
boxplot(SEXO_indeter$X13C ~ SEXO_indeter$Estacion,data=SEXO_indeter)
stripchart(SEXO_indeter$X13C ~ SEXO_indeter$Estacion, vertical = TRUE, method = "jitter",
           pch = 19, add = TRUE, col = 1:length(levels(SEXO_indeter$Estacion)))

anova_X13C_estación_indeter<-aov(SEXO_indeter$X13C~SEXO_indeter$Estacion,data=SEXO_indeter)
summary(anova_X13C_estación_indeter)

##########  macho por estacion y X34S
# macho-X34S

boxplot(SEXO_macho$X34S ~ SEXO_macho$Estacion,data=SEXO_macho)
stripchart(SEXO_macho$X34S ~ SEXO_macho$Estacion, vertical = TRUE, method = "jitter",
           pch = 20, add = TRUE, col = 1:length(levels(SEXO_macho$Estacion)))

anova_X34S_estación_machos<-aov(SEXO_macho$X34S~SEXO_macho$Estacion,data=SEXO_macho)
summary(anova_X34S_estación_machos)
resultado_tukey_kramer <- TukeyHSD(anova_X34S_estación_machos, alpha = 0.05)
print(resultado_tukey_kramer)

# hembra-X34S
boxplot(SEXO_hembra$X34S ~ SEXO_hembra$Estacion,data=SEXO_hembra)
stripchart(SEXO_hembra$X34S ~ SEXO_hembra$Estacion, vertical = TRUE, method = "jitter",
           pch = 20, add = TRUE, col = 1:length(levels(SEXO_hembra$Estacion)))

anova_X34S_estación_hembras<-aov(SEXO_hembra$X34S~SEXO_hembra$Estacion,data=SEXO_hembra)
summary(anova_X34S_estación_hembras)
resultado_tukey_kramer <- TukeyHSD(anova_X34S_estación_hembras, alpha = 0.05)
print(resultado_tukey_kramer)

# indeterminado-X34S
boxplot(SEXO_indeter$X34S ~ SEXO_indeter$Estacion,data=SEXO_indeter,boxwex = 0.5,staplewex = 0.2)
stripchart(SEXO_indeter$X34S ~ SEXO_indeter$Estacion, vertical = TRUE, method = "jitter",
           pch = 19, ,boxwex = 0.8,add = TRUE, col = 1:length(levels(SEXO_indeter$Estacion)))

anova_X34S_estación_indeter<-aov(SEXO_indeter$X34S~SEXO_indeter$Estacion,data=SEXO_indeter)
summary(anova_X34S_estación_indeter)
?boxplot

###############
###############
###############
###############






b<-boxplot(SEXO_hembra$X15N ~ SEXO_hembra$Estacion,data=SEXO_hembra)
c<-boxplot(SEXO_no-idnentf$X15N ~ SEXO_no-idnentf$Estacion,data=SEXO_no-idnentf)

d<- boxplot(BD_morfo$X15N ~ BD_morfo$Estacion, BD_morfo)
e<- boxplot(BD_morfo$X15N ~ BD_morfo$Estacion, datos.cuya)

f <- boxplot(BD_morfo$X13C ~ BD_morfo$Estacion, BD_morfo)   
gg <- boxplot(BD_morfo$SMII ~ BD_morfo$Estacion, BD_morfo)
estadisticos_g<-boxplot.stats(gg)
###############################################################################
############### Crear el gráfico de barras horizontales #####################║
###############################################################################

###  https://rbiologos.com/blog/a011/   LINK GRAFICOS GGPLOT2
###  https://rbiologos.com/blog/a011/   ###  https://rbiologos.com/blog/a011/ 

##### isótopo X.S  por estación
graf1<-ggplot()+
  geom_bar(data=BD_morfo,
           aes(x=BD_morfo$Estacion,y=BD_morfo$X.S,fill = BD_morfo$Estacion),
           stat = "identity",
           position = "dodge")+
  labs(x="zona", y="Isótopos de XS",fill="Colores de zonas")+
  coord_flip()+   # pone las barras horizontales
  theme_minimal()
print(graf1)

##### isótopo X.C  por estación
graf2<-ggplot()+
  geom_bar(data=BD_morfo,
           aes(x=BD_morfo$Estacion,y=BD_morfo$X.C,fill = BD_morfo$Estacion),
           stat = "identity",
           position = "dodge")+
  labs(x="zona", y="Isótopos de X.C",fill="Colores de zonas")+
  coord_flip()+   # pone las barras horizontales
  theme_minimal()
View(graf2)

##### isótopo C.N  por estación
graf3<-ggplot()+
  geom_bar(data=BD_morfo,
           aes(x=BD_morfo$Estacion,y=BD_morfo$C.N,fill = BD_morfo$Estacion),
           stat = "identity",
           position = "dodge")+
  labs(x="zona", y="Isótopos de C.N",fill="Colores de zonas")+
  coord_flip()+   # pone las barras horizontales
  theme_minimal()

##### isótopo X15N  por estación
ggplot()+
  geom_bar(data=BD_morfo,
           aes(x=BD_morfo$Estacion,y=BD_morfo$X15N,fill = BD_morfo$Estacion),
           stat = "identity",
           position = "dodge")+
  labs(x="zona", y="Isótopos de X15N",fill="Colores de zonas")+
  coord_flip()+   # pone las barras horizontales
  theme_minimal()

##### isótopo X13C  por estación

ggplot()+
  geom_bar(data=BD_morfo,
           aes(x=BD_morfo$Estacion,y=BD_morfo$X13C,fill = BD_morfo$Estacion),
           stat = "identity",
           position = "dodge")+
  labs(x="zona", y="Isótopos de X13C",fill="Colores de zonas")+
  coord_flip()+   # pone las barras horizontales
  theme_minimal()

##### isótopo X34S  por estación
pairs(1.1)
ggplot()+
  geom_bar(data=BD_morfo,
           aes(x=BD_morfo$Estacion,y=BD_morfo$X34S,fill = BD_morfo$Estacion),
           stat = "identity",
           position = "dodge")+
  labs(x="zona", y="Isótopos de X34S",fill="Colores de zonas")+
  coord_flip()+   # pone las barras horizontales
  theme_minimal()
plot(AAA,BBB)
ggplot() +
  geom_bar(data = BD_morfo,
           aes(x = Estacion, y = X.S, fill = combined),
           stat = "identity",
           position = position_dodge(width = 0.9)) +  # Ajusta el ancho de la agrupación
  coord_flip() +
  theme_minimal() +
  labs(fill = "Nombres de colores")

# Crear el primer gráfico
plot1 <- barplot(height = datos1$variable2, names.arg = datos1$variable1, main = "Gráfico 1")

# Crear el segundo gráfico
plot2 <- plot(datos2$variable3, datos2$variable4, type = "l", main = "Gráfico 2")

# Definir la estructura de la combinación de gráficos
      par(mfrow = c(1, 1))
      #layout(matrix(c(2, 2), nrow = 1))
grid.arrange(AAA, BBB, nrow = 1)
graf1
graf2

# Mostrar los gráficos en una única ventana gráfica
plot1
plot2


# ANOVAS ------------------------------------------------------------------


###############################################################################
############### ANOVAS #####################║
###############################################################################

#  ANOVA por Estación
attach(BD_morfo)
datos.fil.peso<-na.omit(BD_morfo[c("Peso_grs","Estacion","SEXO","grupo")])

# Estadisticos descriptivos
round(tapply(Peso_grs,Estacion,mean),1)
round(tapply(Peso_grs,SEXO,mean),1)
round(tapply(Peso_grs,list(Estacion,SEXO),mean),1)
round(tapply(Peso_grs,list(Estacion,SEXO),sd),1)
round(tapply(Peso_grs,list(Estacion,SEXO),length),1)

# Analisis de graficos
interaction.plot(Estacion,SEXO,Peso_grs, col = 1:12, lwd=2)
interaction.plot(fecha,SEXO,Peso_grs, col = 1:12, lwd=2)
interaction.plot(grupo,SEXO,Peso_grs, col = 1:12, lwd=2)


# Ajuste de Anova
mod1<-aov(Peso_grs~Estacion*SEXO,datos.fil.peso[SEXO=="Macho",])
mod2<-aov(Peso_grs~Estacion*SEXO,datos.fil.peso[SEXO=="Hembra",])
mod3<-aov(Peso_grs~Estacion*grupo,datos.fil.peso[grupo=="Juvenil",])
mod4<-aov(Peso_grs~Estacion*grupo,datos.fil.peso[grupo=="Adulto",])
mod5<-aov(Peso_grs~Estacion*grupo,data=datos.fil.peso)
anova(mod1)
anova(mod2)
anova(mod3)
anova(mod4)
anova(mod5)







boxplot(BD_morfo$Peso_grs ~ BD_morfo$Estacion, data = BD_morfo)

stripchart(BD_morfo$Peso_grs ~ BD_morfo$Estacion, vertical = TRUE, method = "jitter",
           pch = 19, add = TRUE, col = 1:length(levels(BD_morfo$Estacion)))

anova1<-aov(BD_morfo$Peso_grs~BD_morfo$Estacion,data=BD_morfo)

# H0: Las medias son iguales en las 3 estaciones
# H1: Al menos una estación tiene una media distinta

summary(anova1)

#  ANOVA por SEXO

boxplot(BD_morfo$Peso_grs ~ BD_morfo$SEXO, data = BD_morfo)

stripchart(BD_morfo$Peso_grs ~ BD_morfo$SEXO, vertical = TRUE, method = "jitter",
           pch = 19, add = TRUE, col = 1:length(levels(BD_morfo$SEXO)))


anova2<-aov(BD_morfo$Peso_grs~BD_morfo$SEXO,data=BD_morfo)

# H0: Las medias son iguales en las 3 estaciones
# H1: Al menos una estación tiene una media distinta

summary(anova2)



#  ANOVA por SEXO=1

boxplot(BD_morfo$Peso_grs ~ BD_morfo$Estacion, data = BD_morfo)

stripchart(BD_morfo$Peso_grs ~ BD_morfo$Estacion, vertical = TRUE, method = "jitter",
           pch = 19, add = TRUE, col = 1:length(levels(BD_morfo$Estacion)))


anova3<-aov(BD_morfo$Peso_grs~BD_morfo$Estacion,data=BD_morfo)


# H0: Las medias son iguales en las 3 estaciones
# H1: Al menos una estación tiene una media distinta

summary(anova3)

#  ANOVA por SEXO=2

boxplot(por_SEXO_2$Peso_grs ~ por_SEXO_2$Estacion, data = por_SEXO_2)

stripchart(por_SEXO_2$Peso_grs ~ por_SEXO_2$Estacion, vertical = TRUE, method = "jitter",
           pch = 19, add = TRUE, col = 1:length(levels(por_SEXO_2$Estacion)))


anova4<-aov(por_SEXO_2$Peso_grs~por_SEXO_2$Estacion,data=por_SEXO_2)


# H0: Las medias son iguales en las 3 estaciones
# H1: Al menos una estación tiene una media distinta

summary(anova4)


#  ANOVA sólo Estación  Conanoxa y Huancarane

boxplot(BD_morfo$Peso_grs ~ BD_morfo$Estacion, data = BD_morfo, 
        subset = BD_morfo$Estacion %in% c("Conanoxa", "Huancarane"))

stripchart(BD_morfo$Peso_grs ~ BD_morfo$Estacion, vertical = TRUE, method = "jitter",
           pch = 19, add = TRUE, col = 1:length(levels(BD_morfo$Estacion)))

anova5<-aov(BD_morfo$Peso_grs~BD_morfo$Estacion,data=BD_morfo)

# H0: Las medias son iguales en las 3 estaciones
# H1: Al menos una estación tiene una media distinta

summary(anova5)

mean(datos.cuya$Peso_grs,na.rm=TRUE)
mean(datos.Conanoxa$Peso_grs,na.rm=TRUE)
mean(datos.Huancarane$Peso_grs,na.rm=TRUE)
?mean
class(datos.cuya$Peso_grs)




#_------------------------------------------------------------------------------------

BD_morfo$combined <- interaction(BD_morfo$X15N, BD_morfo$X13C, BD_morfo$X34S)

ggplot() +
  geom_bar(data = BD_morfo,
           aes(x = Estacion, y = X.S, fill = combined),
           stat = "identity",
           position = "dodge") +
  coord_flip() +
  theme_minimal() +
  labs(fill = "Nombres de colores")

##========================================================================
ggplot(BD_morfo, aes(x = Estacion, y = X13C)) +
  geom_boxplot(fill = "#0099f8") +
  labs(
    title = "Concentraci?n isotopos X13C por Estaci?n",
    subtitle = "Is?topos musculos camar?n",
    caption = "IFOP: Hern?n Padilla",
    x = "Estaci?n de muestreo",
    y = "Is?topos X13C"
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(color = "#0099f8", size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(face = "bold.italic", hjust = 0.5),
    plot.caption = element_text(face = "italic")
  )
##====================================================================================
ggplot(BD_morfo, aes(x = BD_morfo$Estacion, y = BD_morfo$X.C)) +
  geom_boxplot(fill = "#0099f8") +
  labs(
    title = "Concentraci?n isotopos X.C por Estaci?n",
    subtitle = "Is?topos musculos camar?n",
    caption = "IFOP: Hern?n Padilla",
    x = "Estaci?n de muestreo",
    y = "Is?topos X.C"
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(color = "#0099f8", size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(face = "bold.italic", hjust = 0.5),
    plot.caption = element_text(face = "italic")
  )

##====================================================================================
ggplot(BD_morfo, aes(x = BD_morfo$Estacion, y = BD_morfo$C.N)) +
  geom_boxplot(fill = "#0099f8") +
  labs(
    title = "Concentraci?n isotopos C.N por Estaci?n",
    subtitle = "Is?topos musculos camar?n",
    caption = "IFOP: Hern?n Padilla",
    x = "Estaci?n de muestreo",
    y = "Is?topos X.C"
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(color = "#0099f8", size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(face = "bold.italic", hjust = 0.5),
    plot.caption = element_text(face = "italic")
  )

##====================================================================================
ggplot(BD_morfo, aes(x = Estacion, y = Long._Cefal_mm)) +
  geom_boxplot(fill = Estacion) +
  labs(
    title = "Concentraci?n isotopos Long._Cefal_mm por Estaci?n",
    subtitle = "Is?topos musculos camar?n",
    caption = "IFOP: Hern?n Padilla",
    #col=(c("red", "blue","green")),
    x = "Estaci?n de muestreo",
    y = "Is?topos X.C"
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(color = "#0099f8", size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(face = "bold.italic", hjust = 0.5),
    plot.caption = element_text(face = "italic")
  )

##====================================================================================

ggplot(BD_morfo, aes(x = BD_morfo$Estacion, y = BD_morfo$X.S)) +
  geom_boxplot(fill = "#0099f8") +
  labs(
    title = "Concentraci?n isotopos X.S por Estaci?n",
    subtitle = "Is?topos musculos camar?n",
    caption = "IFOP: Hern?n Padilla",
    x = "Estaci?n de muestreo",
    y = "Is?topos X.C"
  ) +
  theme_classic(
    
  )  +
  theme(
    plot.title = element_text(color = "#0099f8", size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(face = "bold.italic", hjust = 0.5),
    plot.caption = element_text(face = "italic")
  )

+
  axis(1, at = c(0,1,2,3,4,5,6), labels = c("Cuya - Machos", "Cuya - Hembras", "Conanoxa - Machos",
                                            "Conanoxa - Hembras", "Huancarane - Machos", "Huancarane - Hembras")
  )




##====================================================================================
summary(m2)
m3 <- boxplot(BD_morfo$X34S ~ BD_morfo$Estacion, BD_morfo)
m4 <- boxplot(BD_morfo$X.N ~ BD_morfo$Estacion, BD_morfo)
m5 <- boxplot(BD_morfo$X.C ~ BD_morfo$Estacion, BD_morfo)
m6 <- boxplot(BD_morfo$X.S ~ BD_morfo$Estacion, BD_morfo)
m7 <- boxplot(BD_morfo$C.N ~ BD_morfo$Estacion, BD_morfo)



# Crear grupos combinando las variables categ?ricas
grupos <- interaction(BD_morfo$localidad, BD_morfo$SEXO)
interact_var <- interaction(BD_morfo$Estacion, BD_morfo$SEXO)
interact_var <- interaction(BD_morfo$SEXO,BD_morfo$Estacion)

# Crear el diagrama de caja con la variable combinada
boxplot(BD_morfo$X15N ~ interact_var, data = BD_morfo)

axis(1, at = c(1,2,3 ), labels = c("Cuya - Machos", "Cuya - Hembras", "Conanoxa - Machos",
                                   "Conanoxa - Hembras", "Huancarane - Machos", "Huancarane - Hembras"))


##################################################
datos.especie48=datos.ejemplo[datos.ejemplo$COD_ESPECIE=="48",]
names(datos.especie48)
#################################################################################
##################################################################################
ggplot(df, aes(x = cyl, y = mpg)) +
  geom_boxplot(fill = "#0099f8") +
  labs(
    title = "Miles per gallon among different cylinder options",
    subtitle = "Made by Appsilon",
    caption = "Source: MTCars dataset",
    x = "Number of cylinders",
    y = "Miles per gallon"
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(color = "#0099f8", size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(face = "bold.italic", hjust = 0.5),
    plot.caption = element_text(face = "italic")
  )
#################################################################################
#################################################################################
datos_tallas_cuya<-BD_morfo[BD_morfo$Estacion=="Cuya",9]
datos_tallas_conanoxa<-BD_morfo[BD_morfo$Estacion=="Conanoxa",9]
datos_tallas_Huancarane<-BD_morfo[BD_morfo$Estacion=="Huancarane",9]
hist(datos_tallas_cuyas)
hist(datos_tallas_conanoxa)
hist(datos_tallas_Huancarane)
class(BD_morfo$Estacion)
levels(BD_morfo$Estacion)
####################################################################################

attach(datos.especie48[datos.especie48$PESO>0,])
##################################################


x<-plot(var_num$Long_Total,var_num$Long._Cefal_mm)
?abline
abliine(x)
boxplot(Estacion)
cor(var_num)[1,]
?par
par(var_num)
class(BD_morfo$Peso_grs)
class(Mass_mg)
cor(var_num)[8,]



cor(data.trabajo)[1,]
cor(BD_morfo)[10,11]

####################################################################
################ chi cuadrado  #####################################

# Crear una tabla de contingencia con los datos de las dos variables
tabla_contingencia1<- table(BD_morfo$Estacion, BD_morfo$SEXO)  # Estacion y SEXO
tabla_contingencia2<- table(BD_morfo$Estacion, BD_morfo$grupo) # Estación y grupo




tabla_contingencia3<- table(BD_morfo$Estacion, BD_morfo$grupo) # Estación (Cuya y Conanoxa)
tabla_filtrada1 <- tabla_contingencia3[c("Cuya", "Conanoxa"), ]
tabla_filtrada2 <- tabla_contingencia3[c("Cuya", "Huancarane"), ]
tabla_contingencia4<- table(BD_morfo$Estacion, BD_morfo$grupo)
tabla_contingencia5<- table(BD_morfo$Estacion, BD_morfo$grupo)

prop_3<-prop.table(tabla_filtrada2) # da la proporcion de c/u en la tabla 
barplot(prop_3,main = "Comparación",legend=c("Cuya","Conanoxa"),xlab = "Grupo", beside = TRUE)

prop_1<-prop.table(tabla_contingencia1) # da la proporcion de c/u en la tabla 
barplot(prop_1,main = "Comparación",legend=c("Cuya","Conanoxa","Huancarane"),xlab = "SEXO", beside = TRUE)

# grados de libertad es df=(nf-1)*(nc-1)    tabla contingencia1
#                       df=(3-1)*(3-1)
#                       dF=4

# Realizar la prueba de chi-cuadrado de independencia
resultado3 <- chisq.test(tabla_filtrada)
resultado4 <- chisq.test(tabla_filtrada2)






resultado2 <- chisq.test(tabla_contingencia2)


### NicheRover
# analysis for fish data

# NicheRover --------------------------------------------------------------

names(BD_morfo)
BD_morfo_nicho<-na.omit(BD_morfo[,c(4,(12:14))])
names(BD_morfo_nicho)
aggregate(BD_morfo_nicho[2:4], BD_morfo_nicho[1], mean) # isotope means per BD_morfo
result <- aggregate(BD_morfo_nicho[2:4], BD_morfo_nicho[1], mean)   ##########################?
length(BD_morfo_nicho[2:4])
length(BD_morfo_nicho[4])
str(BD_morfo_nicho)

# random draws from posterior distribution with default prior
nsamples <- 500
BD_morfo_nicho.par <- tapply(1:nrow(BD_morfo_nicho), BD_morfo_nicho$Estacion,
                   function(ii) niw.post(nsamples = nsamples, X = BD_morfo_nicho[ii,2:4]))

# display p(mu | X) and p(Sigma | X) for each fish
clrs <- c("red", "blue", "orange") # colors for each species
par(mar = c(4.4, 4.4, 3, 3)+.1)
niche.par.plot(BD_morfo_nicho.par, col = clrs)
legend(x = "topright", legend = names(BD_morfo_nicho.par), fill = clrs)

# 2-d projections of 10 niche regions
nsamples <- 100
BD_morfo_nicho.par <- tapply(1:nrow(BD_morfo_nicho), BD_morfo_nicho$Estacion,
                   function(ii) niw.post(nsamples = nsamples, X = BD_morfo_nicho[ii,2:4]))

# format data for plotting function
BD_morfo_nicho.data <- tapply(1:nrow(BD_morfo_nicho), BD_morfo_nicho$Estacion, function(ii) X = BD_morfo_nicho[ii,2:4])

niche.plot(niche.par = BD_morfo_nicho.par, niche.data = BD_morfo_nicho.data, pfrac = .05,
           iso.names = expression(delta^{15}*N, delta^{13}*C, delta^{34}*S),
           col = clrs, xlab = expression("Isotope Ratio (\u2030)"))

# niche overlap plots for 95% niche region sizes

# overlap calculation.  use nsamples = nprob = 1e4 for higher accuracy.
nsamples <- 500
over.stat <- overlap(BD_morfo_nicho.par, nreps = nsamples, nprob = nsamples, alpha = .95)

# overlap plot
overlap.plot(over.stat, col = clrs, mean.cred.col = "turquoise",
             equal.axis = TRUE,
             xlab = "Overlap Probability (%) -- Niche Region Size: 95%")


data(wrld_simpl)

## analisis utilizando por estacion y prporcion isitopica

BD_morfo_nicho<-na.omit(BD_morfo[,c(4,(15:17))])

names(BD_morfo_nicho)
aggregate(BD_morfo_nicho[2:4], BD_morfo_nicho[1], mean) # isotope means per BD_morfo
result <- aggregate(BD_morfo_nicho[2:4], BD_morfo_nicho[1], mean)   ##########################?
length(BD_morfo_nicho[2:4])
length(BD_morfo_nicho[4])
str(BD_morfo_nicho)

# random draws from posterior distribution with default prior
nsamples <- 500
BD_morfo_nicho.par <- tapply(1:nrow(BD_morfo_nicho), BD_morfo_nicho$Estacion,
                           function(ii) niw.post(nsamples = nsamples, X = BD_morfo_nicho[ii,2:4]))

# display p(mu | X) and p(Sigma | X) for each fish
clrs <- c("red", "blue", "orange") # colors for each species
par(mar = c(4.4, 4.4, 3, 3)+.1)
niche.par.plot(BD_morfo_nicho.par, col = clrs)
legend(x = "topright", legend = names(BD_morfo_nicho.par), fill = clrs)

# 2-d projections of 10 niche regions
dev.off()
nsamples <- 100
BD_morfo_nicho.par <- tapply(1:nrow(BD_morfo_nicho), BD_morfo_nicho$Estacion,
                           function(ii) niw.post(nsamples = nsamples, X = BD_morfo_nicho[ii,2:4]))

# format data for plotting function
BD_morfo_nicho.data <- tapply(1:nrow(BD_morfo_nicho), BD_morfo_nicho$Estacion, function(ii) X = BD_morfo_nicho[ii,2:4])

niche.plot(niche.par = BD_morfo_nicho.par, niche.data = BD_morfo_nicho.data, pfrac = .05,
           iso.names = expression(delta^{15}*N, delta^{13}*C, delta^{34}*S),
           col = clrs, xlab = expression("Isotope Ratio (\u2030)"))

# niche overlap plots for 95% niche region sizes

# overlap calculation.  use nsamples = nprob = 1e4 for higher accuracy.
nsamples <- 500
over.stat <- overlap(BD_morfo_nicho.par, nreps = nsamples, nprob = nsamples, alpha = .95)

# overlap plot
overlap.plot(over.stat, col = clrs, mean.cred.col = "turquoise",
             equal.axis = TRUE,
             xlab = "Overlap Probability (%) -- Niche Region Size: 95%")


data(wrld_simpl)



##### Proporcio isitopica por Sexo

names(BD_morfo)
levels(BD_morfo$SEXO)
nicho_juvenil_estacion<-na.omit(BD_morfo[(BD_morfo$SEXO=="Macho"),c(4,(15:17))])
BD_morfo_nicho_hembra<-na.omit(BD_morfo[(BD_morfo$SEXO=="Hembra"),c(4,(15:17))])

BD_morfo_nicho<-na.omit(BD_morfo[,c(4,(15:17))])

names(BD_morfo_nicho)
aggregate(nicho_juvenil_estacion[2:4], nicho_juvenil_estacion[1], mean) # isotope means per BD_morfo
result <- aggregate(nicho_juvenil_estacion[2:4], nicho_juvenil_estacion[1], mean)   ##########################?
length(nicho_juvenil_estacion[2:4])
length(nicho_juvenil_estacion[4])
str(nicho_juvenil_estacion)

# random draws from posterior distribution with default prior
nsamples <- 500
nicho_juvenil_estacion.par <- tapply(1:nrow(nicho_juvenil_estacion), nicho_juvenil_estacion$Estacion,
                           function(ii) niw.post(nsamples = nsamples, X = nicho_juvenil_estacion[ii,2:4]))
names(nicho_juvenil_estacion)

# display p(mu | X) and p(Sigma | X) for each fish
clrs <- c("red", "blue", "orange") # colors for each species
par(mar = c(4.4, 4.4, 3, 3)+.1)
niche.par.plot(nicho_juvenil_estacion.par, col = clrs)
legend(x = "topright", legend = names(nicho_juvenil_estacion.par), fill = clrs)

# 2-d projections of 10 niche regions
nsamples <- 100
nicho_juvenil_estacion.par <- tapply(1:nrow(nicho_juvenil_estacion), nicho_juvenil_estacion$Estacion,
                           function(ii) niw.post(nsamples = nsamples, X = nicho_juvenil_estacion[ii,2:4]))

# format data for plotting function
nicho_juvenil_estacion.data <- tapply(1:nrow(nicho_juvenil_estacion), nicho_juvenil_estacion$Estacion, function(ii) X = BD_morfo_nicho[ii,2:4])

niche.plot(niche.par = nicho_juvenil_estacion.par, niche.data = nicho_juvenil_estacion.data, pfrac = .05,
           iso.names = expression(delta^{15}*N, delta^{13}*C, delta^{34}*S),
           col = clrs, xlab = expression("Isotope Ratio (\u2030)"))

# niche overlap plots for 95% niche region sizes

# overlap calculation.  use nsamples = nprob = 1e4 for higher accuracy.
nsamples <- 500
over.stat <- overlap(nicho_juvenil_estacion.par, nreps = nsamples, nprob = nsamples, alpha = .95)

# overlap plot
overlap.plot(over.stat, col = clrs, mean.cred.col = "turquoise",
             equal.axis = TRUE,
             xlab = "Overlap Probability (%) -- Niche Region Size: 95%")


data(wrld_simpl)



# SEGUNDA ETAPA -----------------------------------------------------------

nicho_juvenil_estacion<-na.omit(BD_morfo[(BD_morfo$SEXO=="Macho"),c(4,(15:17))])
## analisis utilizando por grupo= adulto, estacion y prporcion isitopica

BD_morfo_nicho2<-na.omit(BD_morfo[BD_morfo$grupo == "Adulto" & BD_morfo$Estacion %in% c("Cuya","Conanoxa"), c(4,(15:17))])

names(BD_morfo_nicho)
aggregate(BD_morfo_nicho[2:4], BD_morfo_nicho[1], mean) # isotope means per BD_morfo
result <- aggregate(BD_morfo_nicho[2:4], BD_morfo_nicho[1], mean)   ##########################?
length(BD_morfo_nicho[2:4])
length(BD_morfo_nicho[4])
str(BD_morfo_nicho)

# random draws from posterior distribution with default prior
nsamples <- 500
BD_morfo_nicho.par <- tapply(1:nrow(BD_morfo_nicho), BD_morfo_nicho$Estacion,
                           function(ii) niw.post(nsamples = nsamples, X = BD_morfo_nicho[ii,2:4]))

# display p(mu | X) and p(Sigma | X) for each fish
clrs <- c("red", "blue", "orange") # colors for each species
par(mar = c(1.2, 1.2, 1, 1)+.1)
niche.par.plot(BD_morfo_nicho.par, col = clrs)
legend(x = "topright", legend = names(BD_morfo_nicho.par), fill = clrs)

# 2-d projections of 10 niche regions
nsamples <- 100
BD_morfo_nicho.par <- tapply(1:nrow(BD_morfo_nicho), BD_morfo_nicho$Estacion,
                           function(ii) niw.post(nsamples = nsamples, X = BD_morfo_nicho[ii,2:4]))

# format data for plotting function
BD_morfo_nicho.data <- tapply(1:nrow(BD_morfo_nicho), BD_morfo_nicho$Estacion, function(ii) X = BD_morfo_nicho[ii,2:4])

niche.plot(niche.par = BD_morfo_nicho.par, niche.data = BD_morfo_nicho.data, pfrac = .05,
           iso.names = expression(delta^{15}*N, delta^{13}*C, delta^{34}*S),
           col = clrs, xlab = expression("Isotope Ratio (\u2030)"))

# niche overlap plots for 95% niche region sizes

# overlap calculation.  use nsamples = nprob = 1e4 for higher accuracy.
nsamples <- 500
over.stat <- overlap(BD_morfo_nicho.par, nreps = nsamples, nprob = nsamples, alpha = .95)

# overlap plot
overlap.plot(over.stat, col = clrs, mean.cred.col = "turquoise",
             equal.axis = TRUE,
             xlab = "Overlap Probability (%) -- Niche Region Size: 95%")

###    nicho ADULTO POR ESTACION  _____________________________________________________________________________________

#####  nicho_juvenil_estacion<-na.omit(BD_morfo[BD_morfo$grupo == "Adulto" & BD_morfo$Estacion %in% c("Cuya"), c(4,(15:17))])
nicho_adulto_estacion<-na.omit(BD_morfo[BD_morfo$grupo == "Adulto" , c(4,(15:17))])

nsamples <- 500
nicho_adulto_estacion.par <- tapply(1:nrow(nicho_adulto_estacion), nicho_adulto_estacion$Estacion,
                                    function(ii) niw.post(nsamples = nsamples, X = nicho_adulto_estacion[ii,2:4]))

# display p(mu | X) and p(Sigma | X) for each fish
clrs <- c("red", "blue", "orange") # colors for each species
par(mar = c(1.5, 4,1, 1)+.3)
niche.par.plot(nicho_adulto_estacion.par, col = clrs)
legend(x = "topright", legend = names(nicho_adulto_estacion.par), fill = clrs)

# 2-d projections of 10 niche regions
nsamples <- 100
nicho_adulto_estacion.par <- tapply(1:nrow(nicho_adulto_estacion), nicho_adulto_estacion$Estacion,
                                    function(ii) niw.post(nsamples = nsamples, X = nicho_adulto_estacion[ii,2:4]))

# format data for plotting function
nicho_adulto_estacion.data <- tapply(1:nrow(nicho_adulto_estacion), nicho_adulto_estacion$Estacion, function(ii) X = nicho_adulto_estacion[ii,2:4])

niche.plot(niche.par = nicho_adulto_estacion.par, niche.data = nicho_adulto_estacion.data, pfrac = .05,
           iso.names = expression(delta^{15}*N, delta^{13}*C, delta^{34}*S),
           col = clrs, xlab = expression("Isotope Ratio (\u2030)"))

# niche overlap plots for 95% niche region sizes

# overlap calculation.  use nsamples = nprob = 1e4 for higher accuracy.
nsamples <- 500
over.stat <- overlap(nicho_adulto_estacion.par, nreps = nsamples, nprob = nsamples, alpha = .95)

# overlap plot
overlap.plot(over.stat, col = clrs, mean.cred.col = "turquoise",
             equal.axis = TRUE,
             xlab = "Overlap Probability (%) -- Niche Region Size: 95%")


data(wrld_simpl)

##  nicho JUVENIL POR ESTACION  __________________________________________


nicho_juvenil_estacion<-na.omit(BD_morfo[BD_morfo$grupo == "Adulto" , c(4,(15:17))])

nsamples <- 500
nicho_juvenil_estacion.par <- tapply(1:nrow(nicho_juvenil_estacion), nicho_juvenil_estacion$Estacion,
                                    function(ii) niw.post(nsamples = nsamples, X = nicho_juvenil_estacion[ii,2:4]))

# display p(mu | X) and p(Sigma | X) for each fish
clrs <- c("red", "blue", "orange") # colors for each species
par(mar = c(1.5, 4,1, 1)+.3)
niche.par.plot(nicho_juvenil_estacion.par, col = clrs)
legend(x = "topright", legend = names(nicho_juvenil_estacion.par), fill = clrs)

# 2-d projections of 10 niche regions
nsamples <- 100
nicho_juvenil_estacion.par <- tapply(1:nrow(nicho_juvenil_estacion), nicho_juvenil_estacion$Estacion,
                                    function(ii) niw.post(nsamples = nsamples, X = nicho_juvenil_estacion[ii,2:4]))

# format data for plotting function
nicho_juvenil_estacion.data <- tapply(1:nrow(nicho_juvenil_estacion), nicho_juvenil_estacion$Estacion, function(ii) X = nicho_juvenil_estacion[ii,2:4])

niche.plot(niche.par = nicho_juvenil_estacion.par, niche.data = nicho_juvenil_estacion.data, pfrac = .05,
           iso.names = expression(delta^{15}*N, delta^{13}*C, delta^{34}*S),
           col = clrs, xlab = expression("Isotope Ratio (\u2030)"))

# niche overlap plots for 95% niche region sizes

# overlap calculation.  use nsamples = nprob = 1e4 for higher accuracy.
nsamples <- 500
over.stat <- overlap(nicho_juvenil_estacion.par, nreps = nsamples, nprob = nsamples, alpha = .95)

# overlap plot
overlap.plot(over.stat, col = clrs, mean.cred.col = "turquoise",
             equal.axis = TRUE,
             xlab = "Overlap Probability (%) -- Niche Region Size: 95%")


data(wrld_simpl)

# filtrado de base de datos solo estacion y deltas isotopos  -----------------------------------------------------
  
nicho_estacion<-na.omit(BD_morfo[ ,c(4,(12:14))])

nicho_adulto_macho<-na.omit(BD_morfo[BD_morfo$grupo == "Adulto" & BD_morfo$SEXO %in% c("Macho"), c(4,(12:14))])  #  delta isotopos

names(BD_morfo_BASE)
nicho_adulto_hembra<-na.omit(BD_morfo_BASE[BD_morfo_BASE$grupo == "Adulto" & BD_morfo_BASE$SEXO %in% c("Hembra"), c(4,(12:14))])   #  delta isotopos
nicho_adulto_hembra[nicho_adulto_hembra$Estacion=="Conanoxa" & nicho_adulto_hembra$X15N=="4.147461" & nicho_adulto_hembra$X13C=="-27.06187" & nicho_adulto_hembra$X34S=="5.309763",]
nicho_adulto_hembra[nicho_adulto_hembra$Estacion=="Conanoxa" ,]

nicho_adulto_hembra<-na.omit(BD_morfo[BD_morfo$grupo == "Adulto" & BD_morfo$SEXO %in% c("Hembra"), c(4,(12:14))])
nicho_hembra<-na.omit(BD_morfo[BD_morfo$SEXO=="Hembra", c(4,(12:14))])   #  delta isotopos

##  Aquí con 3 condiciones
nicho_adulto_hembra2<-na.omit(BD_morfo[BD_morfo$grupo == "Adulto" & BD_morfo$SEXO %in% c("Hembra") & BD_morfo$Estacion %in% c("Huancarane"), c(4,(12:14))])

names(BD_morfo)

######################################################################################################################
######################################################################################################################
# TRATAMIENTOS DE NICHOS --------------------------------------------------

###    nicho de todos por estación 

nsamples <- 500
nicho_estacion.par <- tapply(1:nrow(nicho_estacion), nicho_estacion$Estacion,
                                 function(ii) niw.post(nsamples = nsamples, X = nicho_estacion[ii,2:4]))

# display p(mu | X) and p(Sigma | X) for each fish
clrs <- c("red", "blue", "orange") # colors for each species
par(mar = c(4.4, 4.4, 3, 3)+.1)
niche.par.plot(nicho_estacion.par, col = clrs)
legend(x = "topright", legend = names(nicho_estacion.par), fill = clrs)

# 2-d projections of 10 niche regions
nsamples <- 100
nicho_estacion.par <- tapply(1:nrow(nicho_estacion), nicho_estacion$Estacion,
                                 function(ii) niw.post(nsamples = nsamples, X = nicho_estacion[ii,2:4]))

# format data for plotting function
nicho_estacion.data <- tapply(1:nrow(nicho_estacion), nicho_estacion$Estacion, function(ii) X = nicho_estacion[ii,2:4])

niche.plot(niche.par = nicho_estacion.par, niche.data = nicho_estacion.data, pfrac = .05,
           iso.names = expression(delta^{15}*N, delta^{13}*C, delta^{34}*S),
           col = clrs, xlab = expression("Isotope Ratio (\u2030)"))

# niche overlap plots for 95% niche region sizes

# overlap calculation.  use nsamples = nprob = 1e4 for higher accuracy.
nsamples <- 500
over.stat <- overlap(nicho_estacion.par, nreps = nsamples, nprob = nsamples, alpha = .95)

# overlap plot
overlap.plot(over.stat, col = clrs, mean.cred.col = "turquoise",
             equal.axis = TRUE,
             xlab = "Overlap Probability (%) -- Niche Region Size: 95%")


data(wrld_simpl)

##-------------------------------------------------------------------------------------------------------------


###    nicho ADULTO MACHO POR ESTACION

nsamples <- 500
nicho_adulto_macho.par <- tapply(1:nrow(nicho_adulto_macho), nicho_adulto_macho$Estacion,
                                     function(ii) niw.post(nsamples = nsamples, X = nicho_adulto_macho[ii,2:4]))

# display p(mu | X) and p(Sigma | X) for each fish
clrs <- c("red", "blue", "orange") # colors for each species
par(mar = c(4.4, 4.4, 3, 3)+.1)
niche.par.plot(nicho_adulto_macho.par, col = clrs)
legend(x = "topright", legend = names(nicho_adulto_macho.par), fill = clrs)

# 2-d projections of 10 niche regions
nsamples <- 100
nicho_adulto_macho.par <- tapply(1:nrow(nicho_adulto_macho), nicho_adulto_macho$Estacion,
                                     function(ii) niw.post(nsamples = nsamples, X = nicho_adulto_macho[ii,2:4]))

# format data for plotting function
nicho_adulto_macho.data <- tapply(1:nrow(nicho_adulto_macho), nicho_adulto_macho$Estacion, function(ii) X = nicho_adulto_macho[ii,2:4])

niche.plot(niche.par = nicho_adulto_macho.par, niche.data = nicho_adulto_macho.data, pfrac = .05,
           iso.names = expression(delta^{15}*N, delta^{13}*C, delta^{34}*S),
           col = clrs, xlab = expression("Isotope Ratio (\u2030)"))

# niche overlap plots for 95% niche region sizes

# overlap calculation.  use nsamples = nprob = 1e4 for higher accuracy.
nsamples <- 500
over.stat <- overlap(nicho_adulto_macho.par, nreps = nsamples, nprob = nsamples, alpha = .95)

# overlap plot
overlap.plot(over.stat, col = clrs, mean.cred.col = "turquoise",
             equal.axis = TRUE,
             xlab = "Overlap Probability (%) -- Niche Region Size: 95%")


data(wrld_simpl)
#---------------------------------------------------------------------------------------
###    nicho ADULTO HEMBRA POR ESTACION
ncol(nicho_adulto_hembra)
nrow(nicho_adulto_hembra)
?niw.post

nsamples <- 2
nicho_adulto_hembra.par <- tapply(1:nrow(nicho_adulto_hembra), nicho_adulto_hembra$Estacion,
                                 function(ii) niw.post(nsamples = nsamples, X = nicho_adulto_hembra[ii,2:4]))

# display p(mu | X) and p(Sigma | X) for each fish
clrs <- c("red", "blue", "orange") # colors for each species
par(mar = c(4.4, 4.4, 3, 3)+.1)
niche.par.plot(nicho_adulto_hembra.par, col = clrs)
legend(x = "topright", legend = names(nicho_adulto_hembra.par), fill = clrs)

# 2-d projections of 10 niche regions
nsamples <- 2
nicho_adulto_hembra.par <- tapply(1:nrow(nicho_adulto_hembra), nicho_adulto_hembra$Estacion,
                                 function(ii) niw.post(nsamples = nsamples, X = nicho_adulto_hembra[ii,2:4]))

# format data for plotting function
nicho_adulto_hembra.data <- tapply(1:nrow(nicho_adulto_hembra), nicho_adulto_hembra$Estacion, function(ii) X = nicho_adulto_hembra[ii,2:4])




niche.plot(niche.par = nicho_adulto_hembra.par, niche.data = nicho_adulto_hembra.data, pfrac = .05,
           iso.names = expression(delta^{15}*N, delta^{13}*C, delta^{34}*S),
           col = clrs, xlab = expression("Isotope Ratio (\u2030)"))

# niche overlap plots for 95% niche region sizes

# overlap calculation.  use nsamples = nprob = 1e4 for higher accuracy.
nsamples <- 2
over.stat <- overlap(nicho_adulto_hembra.par, nreps = nsamples, nprob = nsamples, alpha = .95)

# overlap plot
overlap.plot(over.stat, col = clrs, mean.cred.col = "turquoise",
             equal.axis = TRUE,
             xlab = "Overlap Probability (%) -- Niche Region Size: 95%")


data(wrld_simpl)

#_-----------------------------------------------------------------------------------------
###    nicho HEMBRA POR ESTACION
dev.off()

nsamples <- 1000
nicho_hembra.par <- tapply(1:nrow(nicho_hembra), nicho_hembra$Estacion,
                                 function(ii) niw.post(nsamples = nsamples, X = nicho_hembra[ii,2:4]))

# display p(mu | X) and p(Sigma | X) for each fish
clrs <- c("red", "blue", "orange") # colors for each species
par(mar = c(4.4, 4.4, 3, 3)+.1)
niche.par.plot(nicho_hembra.par, col = clrs)
legend(x = "topright", legend = names(nicho_hembra.par), fill = clrs)

# 2-d projections of 10 niche regions
nsamples <- 100
nicho_hembra.par <- tapply(1:nrow(nicho_hembra), nicho_hembra$Estacion,
                                 function(ii) niw.post(nsamples = nsamples, X = nicho_hembra[ii,2:4]))

# format data for plotting function
nicho_hembra.data <- tapply(1:nrow(nicho_hembra), nicho_hembra$Estacion, function(ii) X = nicho_hembra[ii,2:4])

niche.plot(niche.par = nicho_hembra.par, niche.data = nicho_hembra.data, pfrac = .05,
           iso.names = expression(delta^{15}*N, delta^{13}*C, delta^{34}*S),
           col = clrs, xlab = expression("Isotope Ratio (\u2030)"))

# niche overlap plots for 95% niche region sizes

# overlap calculation.  use nsamples = nprob = 1e4 for higher accuracy.
nsamples <- 1000
over.stat <- overlap(nicho_hembra.par, nreps = nsamples, nprob = nsamples, alpha = .95)

# overlap plot
overlap.plot(over.stat, col = clrs, mean.cred.col = "turquoise",
             equal.axis = TRUE,
             xlab = "Overlap Probability (%) -- Niche Region Size: 95%")


data(wrld_simpl)

#---------------## -------------------------------------------------------------------------------------------
###      nicho JUVENILES POR ESTACION
nicho_juveniles2<-na.omit(BD_morfo[BD_morfo$grupo == "Juvenil", c(4,(12:14))])
nicho_juveniles<-na.omit(BD_morfo[BD_morfo$grupo == "Juvenil" & !BD_morfo$Estacion %in% c("Huancarane"), c(4,(12:14))])#  delta isotopos
nicho_juveniles$Estacion <- factor(nicho_juveniles$Estacion, levels = c("Cuya", "Conanoxa"))

str(nicho_juveniles)
levels(nicho_juveniles$Estacion)
summary(nicho_juveniles$Estacion)
dev.off()

nsamples <- 2
nicho_juveniles.par <- tapply(1:nrow(nicho_juveniles), nicho_juveniles$Estacion,
                           function(ii) niw.post(nsamples = nsamples, X = nicho_juveniles[ii,2:4]))

# display p(mu | X) and p(Sigma | X) for each fish
clrs <- c("red", "blue", "orange") # colors for each species
par(mar = c(4.4, 4.4, 3, 3)+.1)
niche.par.plot(nicho_juveniles.par, col = clrs)
legend(x = "topright", legend = names(nicho_juveniles.par), fill = clrs)

# 2-d projections of 10 niche regions
nsamples <- 2
nicho_juveniles.par <- tapply(1:nrow(nicho_juveniles), nicho_juveniles$Estacion,
                           function(ii) niw.post(nsamples = nsamples, X = nicho_juveniles[ii,2:4]))

# format data for plotting function
nicho_juveniles.data <- tapply(1:nrow(nicho_juveniles), nicho_juveniles$Estacion, function(ii) X = nicho_juveniles[ii,2:4])

niche.plot(niche.par = nicho_juveniles.par, niche.data = nicho_juveniles.data, pfrac = .05,
           iso.names = expression(delta^{15}*N, delta^{13}*C, delta^{34}*S),
           col = clrs, xlab = expression("Isotope Ratio (\u2030)"))

# niche overlap plots for 95% niche region sizes

# overlap calculation.  use nsamples = nprob = 1e4 for higher accuracy.
nsamples <- 2
over.stat <- overlap(nicho_juveniles.par, nreps = nsamples, nprob = nsamples, alpha = .95)

# overlap plot
overlap.plot(over.stat, col = clrs, mean.cred.col = "turquoise",
             equal.axis = TRUE,
             xlab = "Overlap Probability (%) -- Niche Region Size: 95%")


data(wrld_simpl)


save(file = "huella.RData")
?save



