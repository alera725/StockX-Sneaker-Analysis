# StockX 

# Cargamos las paqueterías necesarias
suppressMessages(library(ggplot2))
suppressMessages(library(plotly))
suppressMessages(library(Quandl))
suppressMessages(library(TTR))
suppressMessages(library(PortfolioAnalytics))
suppressMessages(library(quantmod))
suppressMessages(library(PerformanceAnalytics))
suppressMessages(library(readxl))
suppressMessages(library(MASS))
suppressMessages(library(DiscreteLaplace))
suppressMessages(library(extraDistr))
suppressMessages(library(smoothmest))

require(reshape2)

# Elegimos el directorio en donde vamos a trabajar 
setwd("~/Documents/StockX") # Donde estan nuestros archivos ubicados en el PC


#-------------------------------------- IMPORTAR DATOS, ACOMODO DE DATOS Y CALCULO DE TASAS ------------------------------------------

# -- Leemos la base de datos de las ventas de las Empresas
Datos <- readxl::read_excel("Stockx.xlsx", sheet = 1)
Datos <- as.data.frame(Datos)

# -- Modelos de tenis 
Sneaker_models <- unique(Datos$Sneaker_Name)
Sneaker_models_abb <- c('350 V2 Beluga','350 V2 Black Cooper','350 V2 Black Green','350 V2 Black Red','350 V2 Black Red 2017','350 V2 Black White',
                        '350 V2 Cream White','350 V2 Zebra','350 Low Moonrock','Air max 90 Off white','Air presto Off white','Air vapormax Off white',
                        'Jordan 1 retro high Off white','Blazer mid Off white','350 Low Pirate Black','350 Low Oxford Tan','350 Low Turtledove',
                        '350 Low Pirate Black 2015', '350 V2 SemiFrozen','Air force 1 Off white','Air max 97 Off white','Air force 1 low virgil AF100',
                        'React hyperdrunk 2017 Off white', 'Zoom fly Off white','350 V2 Beluga 2.0','350 V2 Blue Tint','Vapor max Off white 2018','Jordan 1 retro high Off White',
                        'Air vapormax Off white black','Jordan 1 retro high Off white Uni blue', 'Air presto Off white black 2018','Air presto Off white white 2018',
                        'ZoomFly mercurial Off white black','ZoomFly mercurial Off white T Orange','350 V2 Butter','Air max 97 Off white Elemental RQueen',
                        'Blazer mid Off white all H eves', 'Blazer mid Off white grim reaper', '350 V2 Sesame','Blazer mid Off white w grey','Air max 97 Off white menta',
                        'Air max 97 Off white black','ZoomFly Off white black silver','ZoomFly Off white pink','Air force 1 low white volt','Air force 1 low white black white',
                        '350 V2 Static','350 V2 Static-Reflective','Air max 90 Off white black','Air max 90 Off white desert ore')

# -- Marca 
Sneaker_brand <- unique(Datos$Brand_u)

# -- Region
Buyer_region <- unique(Datos$Buyer_Region)

# -- Talla
Sneaker_size <- unique(Datos$Shoe_Size)

# -- Generamos un DataFrame por modelo de sneaker 
my_keys <- Sneaker_models
DF_sneaker_model <- vector(mode="list", length=length(my_keys))
names(DF_sneaker_model) <- my_keys

for (i in 1:length(Sneaker_models)){
  
  DF_sneaker_model[[i]] <- Datos[Datos$Sneaker_Name %in% Sneaker_models[i],]
  
}

# -- Calculamos rendimientos diarios y vemos cual es el día de la semana con mayor rendimiento
my_keys <- Sneaker_models
Rendimientos <- vector(mode="list", length=length(my_keys))
names(Rendimientos) <- my_keys
d_rends <- c() # Rendimientos diarios maximos
sd_drends <- c()
t_rends <- c() # Rendimiento total del periodo 
sd_trends <- c() 
max_rend <- c()
min_rend <- c()
day_max <- c()
day_min <- c()

for (i in 1:length(Sneaker_models)){

  DF_new <- DF_sneaker_model[[i]]$Sale_Price
  rends <- diff(DF_new)/DF_new[-length(DF_new)]
  temp_dataframe <- data.frame(rends, DF_sneaker_model[[i]]$Order_Date[1:length(DF_sneaker_model[[i]]$Order_Date)-1], DF_sneaker_model[[i]]$Wee_day_order[1:length(DF_sneaker_model[[i]]$Wee_day_order)-1])
  colnames(temp_dataframe) <- c('Rends', 'Date', 'Week_day')
  Rendimientos[[i]] <- temp_dataframe
  d_rends[i] <- max(rends)
  t_rends[i] <- (last(DF_new)-first(DF_new))/first(DF_new)
  week_days <- unique(Rendimientos[[i]]$Week_day) 
  prom_week_days <- c()
  data_week <- data.frame(matrix(nrow = 2, ncol = length(week_days)))
  colnames(data_week) <- week_days
  rownames(data_week) <- c('Mean', 'Sd')
  
  for (j in 1:length(week_days)){
    
    index <- which(Rendimientos[[i]]$Week_day==week_days[j])
    prom <- mean(Rendimientos[[i]]$Rends[index])
    sd <- sd(Rendimientos[[i]]$Rends[index])
    data_week[1,j] <- prom
    data_week[2,j] <- sd

  }
  
  max_rend[i] <- max(data_week[1,])
  min_rend[i] <- min(data_week[1,])
  id <- which(data_week[1,]==max(data_week[1,]))
  id_min <- which(data_week[1,]==min(data_week[1,]))
  day_max[i] <- colnames(data_week)[id]
  day_min[i] <- colnames(data_week)[id_min]

}


# Graficamos dia de la semana con mayor rendimiento y falta el sneaker  

# Top 3 días con mayor rendimiento 
tt <- sort(table(day_max))

Day <- c(names(tt[length(tt)]), names(tt[length(tt)-1]), names(tt[length(tt)-2]))
Rep <- c(tt[length(tt)], tt[length(tt)-1], tt[length(tt)-2])
data <- data.frame(Day, Rep)

p <- plot_ly(data, x = ~Day, y = ~Rep, type = 'bar', name = 'Return') %>%
  layout(title = 'Top 3 days (High Daily Return)',yaxis = list(title = 'Repeat'), barmode = 'group')


p


# Top 3 días con menor rendimiento 

tt <- sort(table(day_min))

Day <- c(names(tt[length(tt)]), names(tt[length(tt)-1]), names(tt[length(tt)-2]))
Rep <- c(tt[length(tt)], tt[length(tt)-1], tt[length(tt)-2])
data <- data.frame(Day, Rep)

p <- plot_ly(data, x = ~Day, y = ~Rep, type = 'bar', name = 'Return') %>%
  layout(title = 'Top 3 days (Low Daily Return)',yaxis = list(title = 'Repeat'), barmode = 'group')


p

# Top 3 sneakers con mayor rendimiento de todo el periodo

tt <- sort(t_rends)
id1 <- which(t_rends == tt[length(tt)])
id2 <- which(t_rends == tt[length(tt)-1])
id3 <- which(t_rends == tt[length(tt)-2])

Sneaker <- c(Sneaker_models_abb[id1], Sneaker_models_abb[id2], Sneaker_models_abb[id3])
Rend <- c(tt[length(tt)], tt[length(tt)-1], tt[length(tt)-2])*100
data <- data.frame(Sneaker, Rend)

p <- plot_ly(data, x = ~Sneaker, y = ~Rend, type = 'bar', name = 'Return') %>%
  layout(title = 'Top 3 Sneaker (Return of the period)',yaxis = list(title = 'Return %'), barmode = 'group')


p


# -- Generamos un DataFrame por marca 
my_keys_brand <- Sneaker_brand
DF_brand <- vector(mode="list", length=length(my_keys_brand))
names(DF_brand) <- my_keys_brand

for (i in 1:length(Sneaker_brand)){
  
  DF_brand[[i]] <- Datos[Datos$Brand_u %in% Sneaker_brand[i],]
  
}

# -- Generamos un DataFrame por region del comprador 
my_keys <- Buyer_region
DF_region <- vector(mode="list", length=length(my_keys))
names(DF_region) <- my_keys

for (i in 1:length(Buyer_region)){
  
  DF_region[[i]] <- Datos[Datos$Buyer_Region %in% Buyer_region[i],]
  
}

# -- Generamos un DataFrame por talla  
my_keys <- Sneaker_size
DF_size <- vector(mode="list", length=length(my_keys))
names(DF_size) <- my_keys

for (i in 1:length(Sneaker_size)){
  
  DF_size[[i]] <- Datos[Datos$Shoe_Size %in% Sneaker_size[i],]
  
}

# ----------------------------- GRAFICAMOS SERIES DE TIEMPO -----------------------------

# Graficamos cada modelo de sneaker con todas sus tallas 

P <- list()
for (j in 1:length(Sneaker_models_abb)){
  
  model <- Sneaker_models[j]
  model_abb <- Sneaker_models_abb[j]
  sizes <- unique(DF_sneaker_model[[model]]$Shoe_Size)
  size  <- sizes[1]
  
  index_size <- which(DF_size[[paste(size,sep=" ")]]$Sneaker_Name==model)
  
  my_x <- DF_size[[paste(size,sep=" ")]]$Order_Date[index_size]
  my_y <- DF_size[[paste(size,sep=" ")]]$Sale_Price[index_size]
  
  # Let's do a first plot
  p<-plot_ly(y=my_y, x=my_x , type="scatter", mode="lines",  name = size, evaluate = TRUE) %>%
    layout(
      title = paste("Sale Price by Size", model_abb,sep=" "),
      font = list(color = toRGB("white")),
      xaxis = list(title = "Date"),
      yaxis = list(title = "Sale price USD"))%>% 
    
    layout(plot_bgcolor='black') %>% 
    layout(paper_bgcolor='black')
  
  # Add 9 trace to this graphic with a loop!
  for(i in 2:length(sizes)){
    
    size <- sizes[i]
    index_size <- which(DF_size[[paste(size,sep=" ")]]$Sneaker_Name==Sneaker_models[i])
    
    my_x = DF_size[[paste(size,sep=" ")]]$Order_Date[index_size]
    my_y = DF_size[[paste(size,sep=" ")]]$Sale_Price[index_size]
    p <- add_trace(p, x=my_x, y=my_y, type="scatter", mode="lines", name = sizes[i], evaluate = TRUE)
    
  }
  
  P[[j]] <- p

}

P

# Graficamos con grafica de barras

P2 <- list()

for (j in 1:length(Sneaker_models_abb)){
  
  items <- c()
  sizes <- unique(DF_sneaker_model[[j]]$Shoe_Size)
    
  for (i in 1:length(sizes)){
    
    size <- sizes[i]
    items[i] <- length(which(DF_size[[paste(size,sep=" ")]]$Sneaker_Name==Sneaker_models[j]))
    
  }
  
  p <- plot_ly(x = sizes, y = items, type = "bar")%>%
    layout(
      title = paste("Sales by Size:", Sneaker_models_abb[j],sep=" "),
      xaxis = list(title = "Size"),
      yaxis = list(title = "Sales"))
  
  P2[[j]] <- p

}

P2


# -- Graficamos de barra por marca y ver que talla es la mas vendida 

P3 <- list()

for (j in 1:length(DF_brand)){
  
  items <- c()
  brandss <- my_keys_brand[j]
  sizes <- unique(DF_brand[[j]]$Shoe_Size)
  
  for (i in 1:length(sizes)){
    
    size <- sizes[i]
    items[i] <- length(which(DF_brand[[j]]$Shoe_Size==size))
    
  }
  
  p <- plot_ly(x = sizes, y = items, type = "bar")%>%
    layout(
      title = paste("Sales by Size: ", brandss, sep=" "),
      xaxis = list(title = "Size"),
      yaxis = list(title = "Sales"))
  
  P3[[j]] <- p
  
}

P3


# -- Graficamos los Sneakers más vendidos 

items <- c()

for (i in 1:length(Sneaker_models_abb)){
  
  items[i] <- nrow(DF_sneaker_model[[i]])
  
}

p <- plot_ly(x = Sneaker_models_abb, y = items, type = "bar", evaluate = TRUE)%>%
  layout(
    title = paste("Sales by Sneaker"),
    xaxis = list(title = "Sneaker"),
    yaxis = list(title = "Sales"))

p

# -- Grafica de pastel 

#USPersonalExpenditure <- data.frame("Categorie"=rownames(USPersonalExpenditure), USPersonalExpenditure)
Nike <- length(DF_brand$Nike$Order_Date)
Adidas <- length(DF_brand$Adidas$Order_Date)
sales <- c(Adidas,Nike)
brands <- c('Adidas', 'Nike')
data <- data.frame('Brand' = brands, 'Sales'= sales)

p <- plot_ly(data, labels = ~Brand, values = ~Sales, type = 'pie') %>%
  layout(title = 'Sales by Brand',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

p


# -- Promedios de precio de venta contra retail de cada marca 

Nike_retail_prom <- mean(DF_brand$Nike$Retail_Price) 
Nike_sale_prom <- mean(DF_brand$Nike$Sale_Price)
Adidas_retail_prom <- mean(DF_brand$Adidas$Retail_Price)
Adidas_sale_prom <- mean(DF_brand$Adidas$Sale_Price)

Brand <- c("Adidas", "Nike")
Retail <- c(Adidas_retail_prom,Nike_retail_prom)
Sale <- c(Adidas_sale_prom,Nike_sale_prom)
data <- data.frame(Brand, Retail, Sale)

p <- plot_ly(data, x = ~Brand, y = ~Retail, type = 'bar', name = 'Retail Price') %>%
  add_trace(y = ~Sale, name = 'Sale Price') %>%
  layout(title = 'Average Retail & Sale Price by Brand',yaxis = list(title = 'Sale Price'), barmode = 'group')


p

# Marcas más vendida por Region 

items_n <- c()
items_a <- c()

for (i in 1:length(Buyer_region)){
  
  items_n[i] <- length(which(DF_region[[paste(Buyer_region[i],sep=" ")]]$Brand_u=='Nike'))
  items_a[i] <- length(which(DF_region[[paste(Buyer_region[i],sep=" ")]]$Brand_u=='Adidas'))
  
}

Region <- Buyer_region
Nike <- items_n
Adidas <- items_a
data <- data.frame(Region, Nike, Adidas)

p <- plot_ly(data, x = ~Region, y = ~Nike, type = 'bar', name = 'Nike') %>%
  add_trace(y = ~Adidas, name = 'Adidas') %>%
  layout(title = 'Sales by Region Buyer',yaxis = list(title = 'Sale Price'), barmode = 'group')


p

# Graficas de los top 3 regiones y top 3 tallas y top 3 sneakers

# Top 3 region
tt <- sort(table(Datos$Buyer_Region))

Region <- c(names(tt[length(tt)]), names(tt[length(tt)-1]), names(tt[length(tt)-2]))
Sales <- c(tt[length(tt)], tt[length(tt)-1], tt[length(tt)-2])
data <- data.frame(Region, Sales)

p <- plot_ly(data, x = ~Region, y = ~Sales, type = 'bar', name = 'Sales') %>%
  layout(title = 'Top 3 # of Sales by Region',yaxis = list(title = 'Sales'), barmode = 'group')


p

#Top 3 tallas
tt <- sort(table(Datos$Shoe_Size))

Sizing <- c(names(tt[length(tt)]), names(tt[length(tt)-1]), names(tt[length(tt)-2]))
Sales <- c(tt[length(tt)], tt[length(tt)-1], tt[length(tt)-2])
data <- data.frame(Sizing, Sales)

p <- plot_ly(data, x = ~Sizing, y = ~Sales, type = 'bar', name = 'Sales') %>%
  layout(title = 'Top 3 # of Sales by Size',yaxis = list(title = 'Sales'), barmode = 'group')


p

#Top 3 Sneakers
tt <- sort(table(Datos$Sneaker_Name))

Sneakerb <- c(names(tt[length(tt)]), names(tt[length(tt)-1]), names(tt[length(tt)-2]))
Sales <- c(tt[length(tt)], tt[length(tt)-1], tt[length(tt)-2])
data <- data.frame(Sneakerb, Sales)
Sneaker <- c(Sneaker_models_abb[25], Sneaker_models_abb[35], Sneaker_models_abb[8])

p <- plot_ly(data, x = ~Sneaker, y = ~Sales, type = 'bar', name = 'Sales') %>%
  layout(title = 'Top 3 # of Sales by Sneaker',yaxis = list(title = 'Sales'), barmode = 'group')

p


# Dia de la semana mas caro y barato por marca 



## -- Gaficamos con precio inicial en 100

my_keys <- Sneaker_models
Sale_price_rends <- vector(mode="list", length=length(my_keys))
names(Sale_price_rends) <- my_keys
precio_inicial <- 100
size <- 10.5#sort(Sneaker_size)[6]

for (i in 1:length(Sneaker_models)){
  
  Sale_price_rends[[i]] <- 100*(1+Rendimientos[[i]]) 
  Sale_price_rends[[i]] <- c(precio_inicial,Sale_price_rends[[i]])
  
}


my_y=Sale_price_rends[[1]]
my_x=DF_sneaker_model[[1]]$Order_Date #[1:length(DF_sneaker_model[[1]]$Order_Date)-1]#as.numeric(DF_sneaker_model[[1]]$Order_Date) #rownames(Ventas_Normalizadas))

# Let's do a first plot
p<-plot_ly(y=my_y, x=my_x , type="scatter", mode="lines",  name = paste(Sneaker_models_abb[1],sep=" "), evaluate = TRUE) %>%
  layout(
    title = "Sneakers sale price (sep 2017- feb 2019)",
    xaxis = list(title = "Date"),
    yaxis = list(title = "Sale price"))

# Add 9 trace to this graphic with a loop!
for(i in 2:6){ #length(Sneaker_models)){
  
  my_x=DF_sneaker_model[[i]]$Order_Date
  my_y=Sale_price_rends[[i]] 
  p<-add_trace(p, x=my_x, y=my_y, type="scatter", mode="lines", name = paste(Sneaker_models_abb[i],sep=" "), evaluate = TRUE)
  
}

p

