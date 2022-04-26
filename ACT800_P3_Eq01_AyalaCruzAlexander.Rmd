---
title: "Práctica No. 3"
author: "Equipo 1"
date: "`r Sys.Date()`"
output: 
    rmdformats::downcute:
      highlight: "tango"
      lightbox: TRUE
      self_contained: TRUE
      code_folding: hide
      gallery: TRUE
      fig_width: 10
      fig_height: 4
      df_print: kable
      toc_float:
        collapsed: TRUE
        smooth_scroll: TRUE
---

```{r knitr_init, echo=FALSE, cache=FALSE}
library(knitr)
library(rmdformats)
## Global options
options(max.print = "75")
opts_chunk$set(echo = TRUE,
	             cache = FALSE,
               prompt = FALSE,
               tidy = FALSE,
               comment = NA,
               message = FALSE,
               warning = FALSE)
opts_knit$set(width = 75)
```

```{r, echo=FALSE,message=F,warning=F}
library(fpp2)
library(readxl)
library(tidyverse)
library(magrittr)
library(nortest)
library(dygraphs)
library(lmtest)
library(knitr)
source("ggIndexPlot.R")
```

# Ejercicio 3.1 Descomposición Clasica {.tabset .tabset-fade .tabset-pills}

El archivo `cavsales2.csv` representa las ventas mensuales de Cavanaugh Company desde enero de 2001 hasta diciembre de 2006.

## 1

Realice un análisis exploratorio de la serie y redacte un escrito de sus hallazgos. ( 1.5 PUNTOS)

```{r}
sales_data <- read_csv("cavsales.csv") # Se importan los datos
Ventas <- ts(sales_data$y,frequency=12,start=2001) # Se crea objeto de tipo ts()
```

### Análisis Exploratorio

#### Componente de tendencia

```{r}
autoplot(Ventas) + geom_smooth(method = "loess", span=0.2) + ggtitle("Ventas mensuales Cavanaugh: 2001-2006.") + xlab("Tiempo (meses)") + ylab("Número de ventas")
```

> Se tiene una serie mensual (periodo estacional $s=12$); Se observa tendencia; no se observan valores atípicos; no se observa componente horizontal; se observa un patrón estacional,  además la amplitud de la variación estacional amenta conforme el tiempo amenta.

#### Componente estacional

```{r}
ggAcf(Ventas, lag.max = 48)
```

> El Correlograma de la serie de tiempo nos muestra la relación lineal que tiene la serie con su pasado remoto, de esta manera podemos decir que la serie de tiempo presenta correlación serial. Además se puede ver que la correlación aumenta en los multiplos del periodo estacional, donde esa correlación va disminuyendo gradualmente conforme el número de rezagos aumenta.

```{r}
ggseasonplot(Ventas)
ggsubseriesplot(Ventas)
```

> En esta gráfica se puede observar un poco más claro el comportamiento estacional de la serie, en donde la linea azul es el valor promedio de las ventas mesuales para cada periodo estacional, en las cuales se pude notar que su valor aumenta para los ultimos meses del año. Las ventas mensuales alcanza su mayor nivel en mes de noviembre para todos los años, mientras que para el mes de mayo se tiene su mayor caida. 

## 2

Genere pronósticos para los primeros 5 meses de 2007 mediante una descomposición multiplicativa e interprete los índices estacionales. Graficar la serie de tiempo, los datos ajustados estacionalmene y el componente de tendencia. (2.0 PUNTOS)


> La variación estacional es dependiente de la tendencia, por lo que se opta por una descomposición multiplicativa.

```{r}
dcm <- decompose(Ventas, type="mult")
autoplot(dcm)
```

Los indices de variación estacional se grafican con la función `ggIndexPlot()`

```{r}
index_plot(dcm)
```

> Se observa que para el mes de noviembre las ventas mensuales presentan su mayor incremento, un 82.5% respecto del promedio anual, mientras que para el mes de mayo presenta su mayor caida en cerca de 56.6% respecto del promedio anual.


### Desestacionalización 

```{r}
yaj <- dcm$x/dcm$seasonal # Desestacionalización: modelo multiplicativo
trend <- dcm$trend  #Tendencia
```

#### Generar pronosticos desestacionalizados

```{r,fig.width=8}
fc_adj <- holt(yaj, h=5) #se usa metodo de holt justo porque se tiene una tendencia lineal
all <- cbind('Observados'= yaj, "lower"=fc_adj$lower[,2],'fitted'= fc_adj$mean, "upper"=fc_adj$upper[,2])
dygraph(all,main = "Ventas Mensuales Cavanaugh Company", ylab = "Ventas", xlab = "Tiempo") %>% 
  dyAxis("x", drawGrid = FALSE) %>% dyAxis("y", drawGrid = FALSE) %>%
  dySeries("Observados", color = "black") %>%
  dySeries(c("lower", "fitted","upper"), label = "Holt MP", color="red")  %>%
  dyHighlight(highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 0.2) %>% 
  dyLegend(width = 400) %>% 
  dyRangeSelector(height = 20)

```

#### Pronosticos reestacionalizados

```{r,fig.width=8}
#Aquí se multiplica por la variacion estacional para que los pronosticos se ajusten al componente estacional
fc <- (fc_adj$mean)*(dcm$figure[1:5])
all2 <- cbind('Observados'= Ventas,'Pronosticos'= fc)
dygraph(all2,main = "Ventas Mensuales Cavanaugh Company", ylab = "Ventas", xlab = "Tiempo") %>% 
  dyAxis("x", drawGrid = FALSE) %>% dyAxis("y", drawGrid = FALSE) %>%
  dySeries("Observados", color = "black") %>%
  dySeries("Pronosticos", color = "red") %>%
  dyHighlight(highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 0.2) %>% 
  dyLegend(width = 400) %>% 
  dyRangeSelector(height = 20)
```


```{r,fig.width=8}

all3 <- cbind('Data'=Ventas,'Tendencia'= trend,'Seas_Adj'= yaj,'fitted'= fc_adj$mean, 'Pronosticos'= fc)
  dygraph(all3, main = "Ventas Mensuales Cavanaugh Company", ylab = "Ventas", xlab = "Tiempo") %>% 
  dyAxis("x", drawGrid = FALSE) %>% dyAxis("y", drawGrid = FALSE) %>%
  dySeries("Data", color = "black") %>%
  dySeries("Tendencia", color = "green") %>%
  dySeries("Seas_Adj", color="blue")  %>%
  dySeries("fitted", label = "Holt MP", color="#28E2E5")  %>%
  dySeries("Pronosticos", color = "red") %>%
  dyHighlight(highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 0.2) %>% 
  dyLegend(width = 400) %>% 
  dyRangeSelector(height = 20)
```


## 3

El componente irregular (resto) de la descomposición multiplicativo es ruido blanco? Use el siguiente comando `Box.test(dcm$random, lag=24, type="Ljung-Box")` (1.5 PUNTOS)

### Análisis residual

#### Prueba de no correlación (*Ljung-Box test*)

```{r}
res <- dcm$random
checkresiduals(res, test=F)
```


```{r}
(ljb_test <- Box.test(dcm$random, lag=24, type="Ljung-Box"))
```
> Ya que **Valor p=** `r ljb_test[["p.value"]]`< 0.05,  se rechaza $H_{0}$. Se encontro evidencia suficiente que justifica el rechazo de que los residuos son ruido blanco. Esto quiere decir el pronóstico del remanente no es cero y que hay información relevante que debe considerarse para mejorar los pronosticos.

## 4

Los pronósticos para los primeros 5 meses de 2007 fueron 628, 308, 324, 248 y 272, respectivamente. Compare sus pronósticos con los valores reales. (1.0 PUNTOS)



```{r,fig.width=8}
reales <- c(628, 308, 324, 248, 272)
reales2 <- c(rep(NA,72),reales)

all4 <- cbind('Observados'=Ventas,'Descomposición clásica'= fc,'Datos Reales'=reales2)
  dygraph(all4, main = "Ventas Mensuales Cavanaugh Company", ylab = "Ventas", xlab = "Tiempo") %>% 
  dyAxis("x", drawGrid = FALSE) %>% dyAxis("y", drawGrid = FALSE) %>%
  dySeries("Observados", color = "black") %>%
  dySeries("Descomposición clásica", color = "red") %>%
  dySeries("Datos Reales", color="blue")  %>%
  dyHighlight(highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 0.2) %>% 
  dyLegend(width = 400) %>% 
  dyRangeSelector(height = 20)
```


```{r}
tabla <- matrix(c(reales, fc), ncol = 5, byrow = T)
colnames(tabla) <- c("Jan","Feb","Mar","Apr","May")
rownames(tabla) <- c("Datos reales" ,"Descomposición Clasica M")
tabla %>% knitr::kable(digits=2)

```
> Los pronosticos se ajustan adecuadamente al comportamiento real de los datos pero se podrian hallar mejores pronosticos

```{r}
accu2 <- accuracy(fc,reales) %>% round(2)
knitr::kable(accuracy(fc,reales), caption = "Datos reales vs Descomposición Clásica",digits=2)
```



> El `MAE` nos indica que los valores ajustados con este método tiene un promedio de error de `r round(accu2[3], 2)`. Podría parecer que este método es infructuoso, pero si observamos la gráfica de valores ajustados y observados, trabaja bien dentro de la muestra. Ese valor puede ser debido a la escala de la variable Ingresos.

> El `MAPE` indica el error como  un porcentaje promedio del valor real que se va  ajustar.  El error promedio relativo al valor real observado de ventas es de `r round(accu2[5],2)`%. En promedio los pronósticos generados con este método van a tener una desviación del `r round(accu2[5],2)`% respecto del valor verdadero.

> El `MPE` mide el sesgo de los valores ajustados, $\{\hat{y}_{t}\}$. Con un  valor de `r round(accu2[4],2)`% ,es decir,el método de pronóstico no esta subestumando ni sobreestimando consistentemente los datos.


## 5

Use alguno de los siguientes métodos de descomposición de series de tiempo,

  • Descomposición STL: `stl()` para la estimación de componentes latentes y `forecast()` para generar pronósticos.

  • Descomposición con variables dummy: `tslm()` del paquete forecast

para generar pronósticos de los primeros 5 meses de 2007. Verifique si el componente irregular (resto) es ruido blanco.(2.5 PUNTOS)

### Descomposición STL (Descomposición por regresión local)

```{r}
l <- BoxCox.lambda(Ventas)
fit_stl <- BoxCox(Ventas, lambda = l)
```

> Dado que la descomposición por regresión local únicamente funciona para series de tiempo con estacionalidad aditiva, aplicamos la tranformación de Box Cox.

```{r}
fit <- stl(fit_stl, s.window=7, robust=FALSE) 
autoplot(fit)
```

> En esta grafica se muestra la descomposicion de la serie de tiempo para cada uno de sus componentes latentes, tomando en concideracion que tanto el componente de tendencia como el de estacionalidad cambia un poco con forme pasa el tiempo.

<!--

t.window = numero impar: significa que el componente de tendecia cambia un poco en el tiempo

s.window = numero impar: significa que el componente estacional cambia un poco en el tiempo

--->

#### Generar pronosticos desestacionalizados

```{r,fig.width=8}
fc_adj <- fit %>% seasadj() %>% rwf(h=5, drift = T) 

all <- cbind('Observados'= seasadj(fit), "lower"=fc_adj$lower[,2],'fitted'= fc_adj$mean, "upper"=fc_adj$upper[,2])
dygraph(all,main = "Ventas Mensuales Cavanaugh Company", ylab = "Ventas", xlab = "Tiempo") %>% 
  dyAxis("x", drawGrid = FALSE) %>% dyAxis("y", drawGrid = FALSE) %>%
  dySeries("Observados", color = "black") %>%
  dySeries(c("lower", "fitted","upper"), label = "Rwdrift", color="red")  %>%
  dyHighlight(highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 0.2) %>% 
  dyLegend(width = 400) %>% 
  dyRangeSelector(height = 20)

```
> En esta grafica se muestran los pronosticos(metodo informal con drift) desestacionalizados a traves del metodo Stl 

#### Pronosticos reestacionalizados

```{r,fig.width=8}
fc_fit <- fit %>% forecast(method="rwdrift", h= 5)

all <- cbind('Observados'= fit_stl, "lower"=fc_fit$lower[,2],'fitted'= fc_fit$mean, "upper"=fc_fit$upper[,2])
dygraph(all,main = "Ventas Mensuales Cavanaugh Company", ylab = "Ventas", xlab = "Tiempo") %>% 
  dyAxis("x", drawGrid = FALSE) %>% dyAxis("y", drawGrid = FALSE) %>%
  dySeries("Observados", color = "black") %>%
  dySeries(c("lower", "fitted","upper"), label = "Stl-rwdrift", color="red")  %>%
  dyHighlight(highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 0.2) %>% 
  dyLegend(width = 400) %>% 
  dyRangeSelector(height = 20)

```

```{r,fig.width=8}
all4 <- cbind('Data'=fit_stl,'Tendencia'= trendcycle(fit),'Desestacionalizados'= seasadj(fit),'Pronosticos Desestacionalizados'= fc_adj$mean, 'Pronosticos estacionalizados'= fc_fit$mean)
  dygraph(all4, main = "Ventas Mensuales Cavanaugh Company", ylab = "Ventas", xlab = "Tiempo") %>% 
  dyAxis("x", drawGrid = FALSE) %>% dyAxis("y", drawGrid = FALSE) %>%
  dySeries("Data", color = "black") %>%
  dySeries("Tendencia", color = "green") %>%
  dySeries("Desestacionalizados", color="blue")  %>%
  dySeries("Pronosticos Desestacionalizados", color="#28E2E5")  %>%
  dySeries("Pronosticos estacionalizados", color = "red") %>%
  dyHighlight(highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 0.2) %>% 
  dyLegend(width = 400) %>% 
  dyRangeSelector(height = 20)
```


> Se utilizo el método de rwdrift (método informal con drift) para realizar los pronósticos de los datos reestacionalizados a traves del metodo Stl

#### Análisis residual

#### Prueba de no correlación (*Ljung-Box test*)

```{r}
ljb_test2 <- checkresiduals(fc_fit)
```


> Ya que **Valor p=** `r ljb_test2[["p.value"]]`< 0.05,  se rechaza $H_{0}$. Se encontro evidencia suficiente que justifica el rechazo de que los residuos son ruido blanco. Esto quiere decir el pronóstico del remanente no es cero y que hay información relevante que debe considerarse para mejorar los pronosticos.



## 6

Redacte brevemente un escrito en el que compare los resultados con descomposición clásica y el método seleccionado en el inciso anterior. (1.5 PUNTOS)

```{r,fig.width=8}
reales <- c(628, 308, 324, 248, 272)
reales2 <- c(rep(NA,72),reales)
fcs <- InvBoxCox(fc_fit$mean, lambda = l)
all5 <- cbind('Observados'=Ventas,'Descomposición clásica'= fc,'Datos Reales'=reales2,'Descomposición Stl'=fcs )
  dygraph(all5, main = "Ventas Mensuales Cavanaugh Company", ylab = "Ventas", xlab = "Tiempo") %>% 
  dyAxis("x", drawGrid = FALSE) %>% dyAxis("y", drawGrid = FALSE) %>%
  dySeries("Observados", color = "black") %>%
  dySeries("Descomposición clásica", color = "red") %>%
  dySeries("Datos Reales", color="blue")  %>%
  dySeries("Descomposición Stl", color="green")  %>%   
  dyHighlight(highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 0.2) %>% 
  dyLegend(width = 400) %>% 
  dyRangeSelector(height = 20)
```


```{r,}
fcs <- InvBoxCox(fc_fit$mean, lambda = l)
reales <- c(628, 308, 324, 248, 272)
tabla <- matrix(c(reales, fc,fcs), ncol = 5, byrow = T)
colnames(tabla) <- c("Jan","Feb","Mar","Apr","May")
rownames(tabla) <- c("Datos reales" ,"Descomposición Clasica M", "Descomposición STL")
tabla %>% knitr::kable(digits=2)

```

> A pesar de que los residuos en ninguno de los dos métodos se comporta como ruido blanco, basados en las medidas de presición, podemos decir que los pronósticos realizados por descomposición multiplicativa son más adecuados. Esto se puede deber a distintas razónes, por ejemplo, la descomposición por regresión local realmente es para series con una amplitud de variacion estacional que no cambia en el tiempo, o también a que esta descomposición depende de la rápidez con la que evoluciona la tendencia y la estacionalidad.
