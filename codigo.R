#----------------------------------------------------------------------------------------#

# MLG-NP
# Tarea 2
# Cesar A. Saavedra Vanegas
# Angie Rodriguez Duque 

#----------------------------------------------------------------------------------------#

# Librerias 
suppressMessages(library(dplyr))
suppressMessages(library(readxl))
suppressMessages(library(tidyverse))
suppressMessages(library(FactoMineR))
suppressMessages(library(factoextra))
suppressMessages(library(foreign))
suppressMessages(library(corrplot))
suppressMessages(library(polycor))
suppressMessages(library(psych))
suppressMessages(library(gplots))
suppressMessages(library(gridExtra))
suppressMessages(library(viridis))
suppressMessages(library(lsr))
suppressMessages(library(DescTools))
suppressMessages(library(magrittr))
suppressMessages(library(nlme))
suppressMessages(library(MASS))
suppressMessages(library(multilevel))
suppressMessages(library(plspm))
suppressMessages(library(reshape))
suppressMessages(library(homals))
suppressMessages(library(GGally))
suppressMessages(library(CCA))
suppressMessages(library(plotly))
suppressMessages(library(broom))
suppressMessages(library(readr))


#----------------------------------------------------------------------------------------#
# Fijar directorio
setwd("C:/Users/Angie Rodr?guez/Documents/GitHub/MLG-NP_Tarea2")
setwd("/Users/cesar.saavedra/Documents/GitHub/MLG-NP_Tarea2")
#----------------------------------------------------------------------------------------#
# Actividad 1
#----------------------------------------------------------------------------------------#
# Cargar los datos
Tiempos <- read_excel("TiemposFalla.xlsx", col_types = c("numeric"))

# Cambio en la unidad de tiempo (Horas -> Minutos)
Tiempo_dias <- (Tiempos/24)
Tiempo_dias

#Tamaño de la muestra
n <- 36
# Lambda
lambda <- 2

#Selección de la muestra
set.seed(123456)
muestra <- sample(Tiempos$TdeFalla, size= n)
vector <- (muestra/24)
vector

# Algoritmo de Newton-Rapshon
cuadrados <- c()
for(i in 1:length(vector)){
  cuadrados<-c(cuadrados,vector[i]^lambda)      
  
}
sum(cuadrados)
sumatoria <- sum(cuadrados) #Sumatoria de los yi^lambda
expresion <- expression ((-lambda*n/x)+lambda*sumatoria/x^(lambda+1)) # escribimos el polinomio
derivada <- D(expresion, "x") # Derivada del polinomio

x <- 0 # Cualquier valor diferente de aprox
aprox <- min(vector) # valor punto inicial

while ( x != aprox) {
  x <- aprox # Se le asigna el valor aproximado a x.
  reemplazoexpresion <- eval(expresion) #Reemplaza el valor de x en "expresión"
  reemplazoderiv <- eval(derivada) #Reemplaza el valor de x en "derivada"
  
  #newton
  aprox <- x - (reemplazoexpresion/reemplazoderiv) #Ecuación método de Newton
  print(x)
}

#----------------------------------------------------------------------------------------#
# Actividad 2
#----------------------------------------------------------------------------------------#
# Cargar los datos
Datos <- read.table("Datos.txt",header=T,sep = ",")
Datos 
  
View(Datos)
dim(Datos)
names(Datos)
str(Datos)
describe(Datos)
# Estadisticas descriptivas
summary(Datos)

# Analisis univariado
p1 <- ggplot(Datos) + geom_histogram(aes(alcohol), color="black", fill="#ce2d4f")
p2 <- ggplot(Datos) + geom_histogram(aes(chlorides), color="black", fill="#ce6d8b")
p3 <- ggplot(Datos) + geom_histogram(aes(citric.acid), color="black", fill="#cebbc9")
p4 <- ggplot(Datos) + geom_histogram(aes(density), color="black", fill="#4056f4")
p5 <- ggplot(Datos) + geom_histogram(aes(fixed.acidity), color="black", fill="#470ff4")
p6 <- ggplot(Datos) + geom_histogram(aes(free.sulfur.dioxide), color="black", fill="#e54b4b")
p7 <- ggplot(Datos) + geom_histogram(aes(pH), color="black", fill="#ffa987")
p8 <- ggplot(Datos) + geom_histogram(aes(quality), color="black", fill="#c8d5b9")
p9 <- ggplot(Datos) + geom_histogram(aes(residual.sugar), color="black", fill="#4a7c59")
p10 <- ggplot(Datos) + geom_histogram(aes(sulphates), color="black", fill="#c4b7cb")
p11 <- ggplot(Datos) + geom_histogram(aes(total.sulfur.dioxide), color="black", fill="#98e2c6")
p12 <- ggplot(Datos) + geom_histogram(aes(volatile.acidity), color="black", fill="#06bee1")

grid.arrange(p1, p2, p3, p4, p5, p6,p7, p8, p9, p10, p11, p12, ncol= 3)

ggplot(Datos, aes(x=pH, y=fixed.acidity, color=pHi)) + geom_point(show.legend = T) + 
  labs(color = "pHi",fill = " ") + xlab("pH") + ylab("Acidez fija")

# Correlacion
corrplot(cor(Datos), method="number")
corrplot(cor(Datos), method="square", type="upper", order="hclust", tl.col="black")

# Variable indicadora: pHi
summary(Datos[,"pH"])
rango = (max(Datos$pH)-min(Datos$pH))/3
a = min(Datos$pH); a
b = a + rango; b
c = b + rango; c
d = c + rango; d
pHi <- vector() 
pHi[Datos$pH < b] <- "Bajo"
pHi[Datos$pH >= b & Datos$pH < c] <- "Medio"
pHi[Datos$pH > c] <- "Alto"
pHi <- as.factor(pHi)
table(pHi)

G1 <- ggplot(data = Datos, aes(x=pHi, fill=pHi)) +
  geom_bar(position="dodge") + ylab("") + xlab(" ") +
  scale_fill_discrete(name = "Nivel pH:", labels = c("Bajo", "Medio", "Alto"))
G1

# Analisis bivariado: Variable de respuesta calidad y explicativas pHi y Acidez fija
G2 <- ggplot(Datos, aes(group = cut_width(quality, 1)))+ 
  geom_boxplot(aes(quality, fixed.acidity), colour = "#417b5a")+
  xlab("Calidad")+ylab("Acidez fija")
G2
grid.arrange(G1, G2, ncol= 2)

ggplot(Datos, aes(x=pHi, y=fixed.acidity, fill=as.factor(quality))) + geom_boxplot(show.legend = T) + 
  labs(color = "quality",fill = " ") + scale_fill_discrete(name = "Calidad:") + 
  xlab("pHi") + ylab("Acidez fija")

# Modelo sin variable indicadora
Modelo0 <- glm(Datos$quality ~ Datos$fixed.acidity, data=Datos)
summary(Modelo0)

# Modelo con variable indicadora
Modelo <- glm(Datos$quality ~ Datos$fixed.acidity + pHi, data=Datos)
summary(Modelo)


library(MASS)
library(VGAM)
fit = vglm(quality ~ fixed.acidity + pHi, data = Datos, family = cumulative(parallel = TRUE))
summary(fit)

modelo.logit <- glm(calidadAB ~ fixed.acidity + alcoholAB, 
                    data = muestra, family = "binomial")
summary(modelo.logit)

ggplot(muestra, aes(x=alcoholAB, y=fixed.acidity, fill=calidadAB)) + geom_boxplot(show.legend = T) + 
  labs(color = "quality",fill = " ") + scale_fill_discrete(name = "CalidadAB:") + 
  xlab("AlcoholAB") + ylab("Acidez fija")

ggplot(muestra, aes(x=alcohol, y=fixed.acidity, fill=calidadAB)) + geom_point(show.legend = T) + 
  labs(color = "",fill = " ") + xlab("") + ylab("Acidez fija")

ggplot(muestra, aes(x=alcohol, y=fixed.acidity, color=calidadAB)) + geom_point(show.legend = T) + 
  labs(color = "alcoholAB",fill = " ") + xlab("alcohol") + ylab("Acidez fija")


ggplot(muestra, aes(x=alcoholAB, y=fixed.acidity, fill=alcoholAB)) + geom_boxplot(show.legend = T) +
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(),axis.ticks.x=element_blank()) +
  theme(legend.position="bottom") + scale_fill_brewer(palette="Dark2") + ylab("Acidez fija")
  
ggplot(muestra, aes(x=calidadAB, y=fixed.acidity, fill=calidadAB)) + geom_boxplot(show.legend = T) +
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(),axis.ticks.x=element_blank()) +
  theme(legend.position="bottom") + scale_fill_brewer(palette="Dark2") + ylab("Acidez fija")

