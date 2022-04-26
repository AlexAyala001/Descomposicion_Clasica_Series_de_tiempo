index_plot <- function(obj){
  if(!class(obj)=="decomposed.ts") 
    stop("Se requiere un objeto de clase 'decomposed.ts ")
  type=obj$type
  if( !type %in% c("multiplicative","additive") )
    stop("El tipo de modelo debe ser mutiplicativo o aditivo")
  f <- frequency(obj$x)
  index <- round(obj$figure,3)
  if(f==12)  periodo=format(ISOdate(2000, 1:12, 1), "%b")
  else periodo=c("Q1","Q2","Q3","Q4")
  
  df <- data.frame(periodo=periodo,season=factor(index))
  labs_val <- paste(round(index-1,3)*100,"%")
  
  g1 <- ggplot(data=df,aes(x=periodo,y=season,fill=periodo))+
    geom_bar(stat = "identity",position=position_dodge())+
    scale_x_discrete(limits=periodo)+
    theme_minimal()
  
  if(type=="multiplicative") 
    g1+
    geom_text(aes(label=season), vjust=1.6, color="white", size=3.5)+
    geom_text(aes(label=labs_val), vjust=-0.3, size=3.5)+
    ggtitle("Indices de variacion estacional corregido",
            subtitle=paste(type,"model"))
  else 
    g1+
    geom_text(aes(label=season), vjust=-0.3, size=3.5)+
    ggtitle("Coeficientes de variacion estacional corregido",
            subtitle=paste(type,"model"))
  
}