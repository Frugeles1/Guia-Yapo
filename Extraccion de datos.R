#setwd "es el espacio de trabajo"

#Importando Librerias

library(xml2)
library(rvest)

#leer una pagina web


Yapo_laflorida <- read_html("https://www.yapo.cl/region_metropolitana/todos_los_avisos?ca=15_s&l=0&w=1&cmn=&cmn=310")
print(html_text(Yapo_laflorida))

#obteniendo listing_thumbs.

listing_thumbs <- html_nodes(Yapo_laflorida, css=".listing_thumbs")
print(html_text(listing_thumbs))

# obteniendo la fecha de los productos
fechaProductosYapo <- html_nodes(listing_thumbs, css=".date")
print(html_text(fechaProductosYapo))

# obteniendo la hora de los productos
horaProductosYapo <- html_nodes(listing_thumbs, css=".hour")
print(html_text(horaProductosYapo))

# Obteniendo los titulos de los productos
nombresProductosYapo <- html_nodes(listing_thumbs, css=".title")
print(html_text(nombresProductosYapo))

# Obteniendo los precios
preciosProductosYapo <- html_nodes(listing_thumbs, css=".price")
print(html_text(preciosProductosYapo))

# Obteniendo las categoria de los productos
categoriasProductosYapo <- html_nodes(listing_thumbs, css=".category")
print(html_text(categoriasProductosYapo))

# Obteniendo las regiones de los productos
regionesProductosYapo <- html_nodes(listing_thumbs, css=".region")
print(html_text(regionesProductosYapo))

# Obteniendo las comunas de los productos
comunasProductosYapo <- html_nodes(listing_thumbs, css=".commune")
print(html_text(comunasProductosYapo))


#====================================#
for (elemento in listing_thumbs) {
  precio <- html_nodes(elemento,css=".price")
  print(precio)
}     

#====================================#
for (elemento in listing_thumbs) {
  precio <- html_nodes(elemento,css=".price")
  if(length(precio)>0){
    print(html_text(precio))
  }else{
    print("no hay datos")
  }
}

#====================================#
for (elemento in listing_thumbs) {
    precio <- html_nodes(elemento,css=".price")
    if(length(precio)>0){
      precio <- html_text(precio)
      precio <- gsub("\t","",precio)
      precio <- gsub("\n","",precio)
      precio <- gsub("[$]","",precio)
      precio <- gsub("[.]","",precio)
      precio <- gsub(",",".",precio)
      precio <- gsub(" ","",precio)
    }else{
      precio <- NA
      print("no hay datos")
    }
    print(precio)
  }

#====================================#
for (elemento in listing_thumbs) {
  precio <- html_nodes(elemento,css=".price")
  if(length(precio)>0){
    precio <- html_text(precio)
    precio <- gsub("\t","",precio)
    precio <- gsub("\n","",precio)
    precio <- gsub("[$]","",precio)
    precio <- gsub("[.]","",precio)
    precio <- gsub(",",".",precio)
    precio <- gsub("UF","",precio)
    precio <- gsub(" ","",precio)
    precio <- as.numeric(precio)
  }else{
    precio <- NA
    print("no hay datos")
  }
  print(precio)
}  

#====================================#   
Valor_UF <- 30887.21
for (elemento in listing_thumbs) {
  precio <- html_nodes(elemento,css=".price")
  if(length(precio)>0){
    precio <- html_text(precio)
    precio <- gsub("\t","",precio)
    precio <- gsub("\n","",precio)
    precio <- gsub("[$]","",precio)
    precio <- gsub("[.]","",precio)
    precio <- gsub(",",".",precio)
    precio <- gsub(" ","",precio)
    
    if(substr(precio,1,2) == 'UF'){
      precio <- gsub("UF","",precio)
      precio <- as.numeric(precio)
      precio <- precio*Valor_UF
      print("esto es una uf")
    }else{
      precio <- as.numeric(precio)
    }
      
     }else{
    precio <- NA
    print("no hay datos")
  }
  print(precio)
}  
 
#====================================#   
Valor_UF <- 30887.21
precio_normal <- c()
precio_calculado <- c()
vectorvalor_uf <- c()
tipo_moneda <- c()
for (elemento in listing_thumbs) {
  precio <- html_nodes(elemento,css=".price")
  if(length(precio)>0){
    precio <- html_text(precio)
    precio <- gsub("\t","",precio)
    precio <- gsub("\n","",precio)
    precio <- gsub("[$]","",precio)
    precio <- gsub("[.]","",precio)
    precio <- gsub(",",".",precio)
    precio <- gsub(" ","",precio)
    
    if(substr(precio,1,2) == 'UF'){
      precio <- gsub("UF","",precio)
      precio <- as.numeric(precio)
      #guardando precio normal
      precio_normal <- c(precio_normal,precio)
      
      precio <- precio*Valor_UF
    }else{
      precio <- as.numeric(precio)
      precio_normal <- c(precio_normal,precio)
    }
    
  }else{
    precio <- NA
  }
  print(precio)
}
 
#====================================#  
Valor_UF <- 30887.21
precio_normal <- c()
precio_calculado <- c()
vectorvalor_uf <- c()
tipo_moneda <- c()
for (elemento in 1:length(listing_thumbs)) {
  precio <- html_nodes(listing_thumbs[elemento],css=".price")
  if(length(precio)>0){
    precio <- html_text(precio)
    precio <- gsub("\t","",precio)
    precio <- gsub("\n","",precio)
    precio <- gsub("[$]","",precio)
    precio <- gsub("[.]","",precio)
    precio <- gsub(",",".",precio)
    precio <- gsub(" ","",precio)
    
    if(substr(precio,1,2) == 'UF'){
      precio <- gsub("UF","",precio)
      precio <- as.numeric(precio)
      #guardando precio normal
      precio_normal <- c(precio_normal,precio)
      
      precio <- precio*Valor_UF
    }else{
      precio <- as.numeric(precio)
      precio_normal <- c(precio_normal,precio)
    }
    
  }else{
    precio <- NA
    precio_normal <- c(precio_normal,precio)
  }
  print("====================================" )
  print(precio)
}

#====================================#  
Valor_UF <- 30887.21
precio_normal <- c()
precio_calculado <- c()
vectorvalor_uf <- c()
tipo_moneda <- c()
for (elemento in 2:length(listing_thumbs)) {
  precio <- html_nodes(listing_thumbs[elemento],css=".price")
  if(length(precio)>0){
    precio <- html_text(precio)
    precio <- gsub("\t","",precio)
    precio <- gsub("\n","",precio)
    precio <- gsub("[$]","",precio)
    precio <- gsub("[.]","",precio)
    precio <- gsub(",",".",precio)
    precio <- gsub(" ","",precio)
    
    if(substr(precio,1,2) == 'UF'){
      precio <- gsub("UF","",precio)
      precio <- as.numeric(precio)
      #guardando precio normal
      precio_normal <- c(precio_normal,precio)
      
      precio <- precio*Valor_UF
    }else{
      precio <- as.numeric(precio)
      precio_normal <- c(precio_normal,precio)
    }
    
  }else{
    precio <- NA
    precio_normal <- c(precio_normal,precio)
  }
  print("====================================" )
  print(precio)
}

print(precio_normal)

#====================================#

Valor_UF <- 30887.21
precio_normal <- c()
precio_calculado <- c()
vectorvalor_uf <- c()
tipo_moneda <- c()
for (elemento in 2:length(listing_thumbs)) {
  precio <- html_nodes(listing_thumbs[elemento],css=".price")
  if(length(precio)>0){
    precio <- html_text(precio)
    precio <- gsub("\t","",precio)
    precio <- gsub("\n","",precio)
    precio <- gsub("[$]","",precio)
    precio <- gsub("[.]","",precio)
    precio <- gsub(",",".",precio)
    precio <- gsub(" ","",precio)
    
    if(substr(precio,1,2) == 'UF'){
      precio <- gsub("UF","",precio)
      precio <- as.numeric(precio)
      #guardando precio normal
      precio_normal <- c(precio_normal,precio)
      
      precio <- precio*Valor_UF
      precio_calculado <- c(precio_calculado,precio)
      vectorvalor_uf <- c(vectorvalor_uf,Valor_UF)
      tipo_moneda <- c(tipo_moneda,"UF")
    }else{
      precio <- as.numeric(precio)
      precio_normal <- c(precio_normal,precio)
      precio_calculado <- c(precio_calculado,NA)
      vectorvalor_uf <- c(vectorvalor_uf,NA)
      tipo_moneda <- c(tipo_moneda,"peso")
    }
    
  }else{
    precio <- NA
    precio_normal <- c(precio_normal,precio)
    precio_calculado <- c(precio_calculado,NA)
    vectorvalor_uf <- c(vectorvalor_uf,NA)
    tipo_moneda <- c(tipo_moneda,NA)
  }
  print("====================================" )
  print(precio)
}

#====================================#

datosYapo_1 <- data.frame(Fecha=html_text(fechaProductosYapo),Hora=html_text(horaProductosYapo),Nombre=html_text(nombresProductosYapo),
                               Categoria=html_text(categoriasProductosYapo),Region=html_text(regionesProductosYapo),
                               Comuna=html_text(comunasProductosYapo),PrecioNormal=precio_normal,
                               PrecioCalculado=precio_calculado,TipoMoneda=tipo_moneda,ValorUf=vectorvalor_uf)

write.csv2(araucania_1,"datosYapo_1.csv")

#====================================#

#rm(elemento,precio,precio_calculado,precio_normal,tipo_moneda,vectorvalor_uf)

#Warning messages:
  #1: In if (substr(precio, 1, 2) == "UF") { :
      #la condición tiene longitud > 1 y sólo el primer elemento será usado
   # 2: In as.numeric(precio) : NAs introducidos por coerción
