

# Comentarios

# Ctrl + L  Limpia la consola

3 + 4 - 1
5-2
4*1
# suma +
# resta -
# multiplicación *
# división /
# potencia ^


# Funciones

# nombre()


univalle <- function(A,B){
  X =3*A
  area= B*X
  print(area)
}

univalle(5,3)

X=3

x = c(1,5,8,3,7)  
x
x <- c(1,5,8,3,7,7,7.7,7,7,7)

X <- 5
X = "Univalle"

G = c("Casa", "Armario", "Agua")

y <- 1:5
y

# Indexación 

# [ ]

y <- 23:67
y[16]
y[1:3]



Edad <- c(18,20,35,40)
Sexo <- c("M","F","F","F")
Id <- c(156,342,567,123)

Base<- data.frame(Id,Edad,Sexo)
Base

# data.frame


iris
View(iris)

iris[1,2]


iris["Species"]

iris[ ,5]

names(iris)


iris$Species


iris$Species

iris$Petal.Length



# Los decimales se especifican con puntos!!! .

iris[1,]

Z<-iris[,1]


mean(Z)


str(iris)

# Listas


Lista<-list(A = c(1,2,3), 
            B = iris, 
            C = 3)

Lista

Lista[1]

Lista[[1]]


Lista[[1]][3]


Lista$C

Lista$C


Lista$A


Lista[[1]]


###################################################################################



#install.packages("readxl")





library(readxl)
Salaries <- read_excel("Datos/Salaries.xlsx")
View(Salaries)

Salaries


names(Salaries)



names(Salaries)[3]



################################################################


Salaries


# Esto es importante
X <- Salaries$salary

X

# promedio

f <- c(3, 4, 5, 6 , NA, 8)
f

mean(f)

mean(f[-5])

mean(f, na.rm=TRUE)

mean(na.omit(f))



mean(X,na.rm = TRUE)

# Mediana
median(X)
# El 50% de las personas tiene salarios menores o iguales a $107300

# Moda
library(modeest)
mlv(X, method="mfv")[1]
# La moda de los salarios es $92000


# Varianza y desviación estándar
var(X)
sd(X)
# Los datos se alejan en promedio $30289.04 de la media


# Cuantiles: Percentiles, Deciles, Cuartiles
quantile(X, 0.25)
# El 25% de los encuestados ganan $91000 o menos

# Coef de variación
( sd(X) /   mean(X) ) * 100
#El grado de homogeneidad de los salarios es de 26.6%


# Resumen
summary(X)

min(X)

max(X)



############################################

library(readxl)
Salaries <- read_excel("Datos/Salaries.xlsx")
Salaries
View(Salaries)



library(tidyverse)

View(Salaries)




starwars

newdata <- select(starwars, name, height, gender)

newdata


Salaries

# Select
BaseSal_1 <- select(Salaries, rank, sex, salary)

View(BaseSal_1)


# pipeline    %>% %>% %>% %>% %>% %>% %>% %>% %>% %>% %>% %>% %>% %>% %>% %>% %>% %>% %>% %>% %>% %>% 




BaseSal_1 <- Salaries %>% 
  select(rank, sex, salary) 



# Filter
BaseSal_2 <- filter(BaseSal_1, sex == "Male" ,
                    rank=="Prof")

View(BaseSal_2)



Base <- Salaries %>% 
  select(rank, sex, salary) %>% 
  filter(sex == "Male",rank=="Prof")

Base




# Mutate
BaseSal_3 <- Salaries %>% 
  select(rank, sex, yrs.service, salary) %>% 
  filter(sex == "Female" &  salary > 100000) %>% 
  mutate(SalCOP = salary * 4556.04,
         salary2 = (salary / yrs.service) * 123 - log(salary),
         salarioUnivalle = salary^2 - log(salary),
         indice = salary2/salarioUnivalle * 100) 


View(BaseSal_3)

Salaries



# Group_by - Summarize
BaseSal_3 <- Salaries %>% 
  select(rank, sex, yrs.service, salary) %>% 
  #filter(sex == "Female") %>% 
  mutate(SalCOP = salary * 4556.04) %>% 
  group_by(sex,rank) %>%   
  summarize( PromUSD = mean(salary),
             PromCOP = mean(SalCOP),
             sdUSD = sd(salary),
             sdCOP = sd(SalCOP),
             Mediana = median(salary))

mean(BaseSal_3$PromUSD)

BaseFinal <- Salaries %>% 
  select(-discipline) %>% 
  filter(yrs.service >= 10) %>% 
  mutate(SalCOP = salary * 4556.04) %>% 
  #group_by(rank, sex) %>% 
  summarize( PromUSD = mean(salary),
             PromCOP = mean(SalCOP),
             MedUSD = median(salary),
             MedCOP = median(SalCOP))


#################################################

#Actividad

#Covid Muestra

library(readxl)
CovidMuestra <- read_excel("Datos/CovidMuestra.xlsx")
View(CovidMuestra)

names(CovidMuestra)


Act <- CovidMuestra %>% 
  select(Departamento, Edad) %>% 
  group_by(Departamento) %>% 
  summarize(PromEdad = mean(Edad)) %>% 
  arrange(-PromEdad)


Act



#################################################################################


# ggplot

library(tidyverse)

# library(ggplot2)




grafico1<- ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width) )  +
  geom_point()

grafico1

library(plotly)

ggplotly(grafico1)



library(ggplot2)
data(Marriage, package = "mosaicData")
# plot the age distribution using a histogram
ggplot(Marriage, aes(x = age)) +
  geom_histogram(fill = "#57E609", color="#61665E") + 
  labs(title = "Participants by age",
       x = "Age")+
  theme()






library(plotly)

ggplotly(grafico1)





names(Salaries)

modelo1<-lm(salary~yrs.since.phd,Salaries)

summary(modelo1)


























