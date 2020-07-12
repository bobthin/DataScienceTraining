library(tidyverse)
library(dslabs)
library(ggplot2)

## Carga de archivo .csv y transformacion de fecha - archivo filtrado con total por SKU

setwd("~/Documents/Work/Barraza/Analisis")
mydata = read.csv("Master Data - Category Analysis - SKU.csv", header = TRUE, 
                  na.strings = 0, colClasses = c("Date" = "character"), as.is = "SKU")

mydata$Date <- gsub("-","/",mydata$Date)
mydata$Date <- as.Date(mydata$Date)
str(mydata)

## Carga de archivo .csv y transformacion de fecha - complete dataset

fulldata <- read.csv("Complete DN Dataset.csv", header = TRUE, 
                     na.strings = 0, colClasses = c("Date" = "character"), as.is = "SKU")

fulldata$Date <- gsub("-","/",fulldata$Date)
fulldata$Date <- as.Date(fulldata$Date)


myfilter <- fulldata %>% filter(Cadena == "Rey" & 
                                  Categoría == "Limpiadores, desinfectantes y desengrasantes" & 
                                  Date >= "2020-01-01" & Fabricante == "Barraza")
myfilter  

##### Generacion de subgrupo y filtro para ploteado - TOTALES POR CADENA - TRELLIS POR CATEGORIA

mydata1 <-  mydata %>% select(Date,Category,Cadena,Fabricante,USD) 
mydata1 <- mydata1 %>% filter(Cadena != "Todas las cadenas" & 
                                Category != "Jabón en barra para ropa" &
                                Date >= "2019-01-01") 
mydata1 <- group_by(mydata1,Date,Category,Cadena) %>% summarize(USD=sum(USD),n())
mydata1 %>%
  ggplot(aes(Date,USD,group = Cadena)) + 
  geom_line(aes(color=Cadena)) + 
  labs(title = "Evolucion de Ventas") + 
  facet_wrap( ~ Category, ncol=2, scales = "free")



##### Generacion de subgrupo y filtro para ploteado - TOTALES POR FABRICANTE / TRELLIS POR CADENA (LAUNDRY)

mydata1 <-  mydata %>% select(Date,Category,Cadena,Fabricante,USD) 
mydata1 <- mydata1 %>% filter(Cadena != "Todas las cadenas" & 
                                Category == "Detergentes para ropa" &
                                Date >= "2019-01-01" & 
                                Fabricante %in% c("Barraza","Industrias Panama Boston","Henkel"))
mydata1 <- group_by(mydata1,Date,Cadena, Fabricante) %>% summarize(USD=sum(USD),n())
mydata1 %>%
  ggplot(aes(Date,USD,group = Fabricante)) + 
  geom_line(aes(color=Fabricante)) +
  labs(title = "Evolucion de Ventas") + 
  facet_wrap( ~ Cadena, ncol=2, scales = "free")



##### Generacion de subgrupo y filtro para ploteado - TOTALES POR CADENA / TRELLIS POR CATEGORIA

mydata1 <-  mydata %>% select(Date,Category,Cadena,Fabricante,USD) 
mydata1 <- mydata1 %>% filter(Cadena != "Todas las cadenas" & 
                                Category != "Jabón en barra para ropa" &
                                Date >= "2019-01-01") 
mydata1 <- group_by(mydata1,Category,Cadena) %>% summarize(USD=sum(USD),n())
mydata1$Cadena <- factor(mydata1$Cadena) %>% fct_reorder(mydata1$USD,sum,.desc=FALSE)
mydata1 %>%
  ggplot(aes(Cadena,USD/10^3)) + geom_col(aes(fill=Cadena)) + 
  labs(title = "Participacion de Ventas") + 
  facet_wrap( ~ Category, ncol=2, scales = "free") + coord_flip() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1)) +
  ylab("Ventas ('000)")
  



##### Generacion de subgrupo y filtro para ploteado - TOTALES POR CADENA / TRELLIS POR CATEGORIA / STACKED POR MARCA

mydata1 <-  mydata %>% select(Date,Category,Cadena,Fabricante,Marca,USD) 
mydata1 <- mydata1 %>% filter(Cadena != "Todas las cadenas" & 
                                Category != "Jabón en barra para ropa" &
                                Fabricante == "Barraza" & 
                                Date >= "2019-01-01") 
mydata1 <- group_by(mydata1,Category,Cadena,Fabricante,Marca) %>% summarize(USD=sum(USD),n())
mydata1$Cadena <- factor(mydata1$Cadena) %>% fct_reorder(mydata1$USD,sum,.desc=FALSE)
mydata1 %>%
  ggplot(aes(Cadena,USD)) + 
  geom_bar(aes(fill=Marca),position="stack", stat="identity") + 
  labs(title = "Participacion de Ventas") + 
  facet_wrap( ~ Category, ncol=2, scales = "free") + 
  coord_flip() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 


##### Generacion de subgrupo y filtro para ploteado - STACKED POR SKU  XXX NO SIRVE XXX

mydata1 <-  mydata %>% select(Date,Category,Cadena,Fabricante,Marca,SKU,USD) 
mydata1 <- mydata1 %>% filter(Cadena != "Todas las cadenas" & 
                                Category != "Jabón en barra para ropa" &
                                Fabricante == "Barraza" & 
                                Date >= "2019-01-01") 
mydata1 <- group_by(mydata1,Category,Cadena,Fabricante,Marca,SKU) %>% summarize(USD=sum(USD),n())
mydata1$Cadena <- factor(mydata1$Cadena) %>% fct_reorder(mydata1$USD,sum,.desc=FALSE)
#str(mydata1)
mydata1 %>%
  ggplot(aes(Cadena,USD)) + geom_bar(aes(fill=SKU),position="stack", stat="identity") + 
  labs(title = "Participacion de Ventas") + 
  facet_wrap( ~ Category, ncol=2, scales = "free") + coord_flip() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 



################
d <- data.frame(
  x   = runif(90),
  grp = gl(3, 30))
ordered <- d[order(d$x, decreasing = TRUE), ]    
splits <- split(ordered, ordered$grp)
heads <- lapply(splits, head)
do.call(rbind, heads)
###############


# load the library
library(forcats)
# Dataset 1: one value per group
data <- data.frame(
  name=c("north","south","south-east","north-west","south-west","north-east","west","east"),
  val=sample(seq(1,10), 8 )
)
# Reorder following the value of another column:
data %>%
  mutate(name = fct_reorder(name, val)) %>%
  ggplot( aes(x=name, y=val)) +
  geom_bar(stat="identity", fill="#f68060", alpha=.6, width=.4) +
  coord_flip() +
  xlab("") +
  theme_bw()

# Reverse side
data %>%
  mutate(name = fct_reorder(name, desc(val))) %>%
  ggplot( aes(x=name, y=val)) +
  geom_bar(stat="identity", fill="#f68060", alpha=.6, width=.4) +
  coord_flip() +
  xlab("") +
  theme_bw()