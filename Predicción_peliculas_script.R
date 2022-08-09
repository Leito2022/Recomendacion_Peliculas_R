## PROYECTO DE RECOMENDACIÓN DE PELICULAS ##
##4/5/2022##
##CREACIÓN Y BASE DEL PROYECTO##
## RODRÍGUEZ JORGE LEONARDO##
##LA IDEA DEL PROYECTO ES GENERAR UN SCRIPT QUE PERMITA LA RECOMENDACIÓN DE PELICULAS
## EN BASE A LA SELECCIÓN DE UNA PELICULA DE UNA BASE DE DATOS##
install.packages('recommenderlab')
install.packages("reshape2")
install.packages("tidyverse")
install.packages("usethis")

usethis::use_git()
library (usethis)
library (recommenderlab)
library (ggplot2)
library (data.table)
library (reshape2)

getwd()

movie_data <- read.csv("movies.csv", stringsAsFactors = F)
rating_data <- read.csv("ratings.csv")

str(movie_data)
#Tenemos 3 variables, el ID de las películas, el título de las películas y el
#Género de las películas, una película puede incluirse en más de un género.

#Exploramos los datos, la cantidad de registros, el tipo de dato, los primeros y
#los últimos registros.
summary(movie_data)
head(movie_data)
tail(movie_data)

#Exploramos los datos del DF rating de la misma manera
summary (rating_data)
head(rating_data)
tail(rating_data)

#Vamos a comprimir los datos de la DF movie, la sección género para poder modificarla
#y ser más fácil de usar.

movie_genre <- as.data.frame(movie_data$genres, stringsAsFactors = F)
movie_genre2 <- as.data.frame(tstrsplit(movie_genre[,1], "[|]",
                                       type.convert = T),
                             stringAsFactors = F)
colnames(movie_genre2) <- c(1:10)

list_genre <- c("Action","Adventure","Animation","Children","Comedy","Crime",
                "Documentary","Drama","Fantasy","Film-Noir","Horror","Musical",
                "Mistery","Romance","Sci-Fi","Thriller","War","Western")
genre_mat1 <- matrix(0,10330,18)
genre_mat1[1,] <- list_genre
colnames(genre_mat1) <- list_genre

for (index in 1:nrow(movie_genre2)) {
  for(col in 1:ncol(movie_genre2))  {
    gen_col = which (genre_mat1[1,] == movie_genre2[index,col])
    genre_mat1[index+1, gen_col] <- 1
  }
}
genre_mat2 <- as.data.frame(genre_mat1[-1,], stringsAsFactors = F)

for (col in 1:ncol (genre_mat2)){
  genre_mat2[,col] <- as.integer (genre_mat2[,col])
}
str(genre_mat2)

#una vez que hemos despejado la columna de genero y pasado los generos a 0 y 1 
#podemos vincular mejor e individualmente cada genero a cada película. Volvemos 
#a anexar con los títulos de película en otra variable. Llamaremos a la variable
#SearchMatrix como una matriz para búsqueda.

SearchMatrix <- cbind(movie_data[,1:2], genre_mat2[])
head(SearchMatrix)

#Para que recomendarlabs reconozca el sistema de calificaciones, debemos convertir
#la matriz de calificaciones a una matriz dispersa de la siguiente manera

ratingMatrix <- reshape2::dcast(rating_data, userId~movieId, value.var = "rating",na.rm=FALSE)
ratingMatrix <- as.matrix(ratingMatrix[,-1]) #remove userIds
#Convert rating matrix into a recommenderlab sparse matrix
ratingMatrix <- as(ratingMatrix, "realRatingMatrix")
ratingMatrix

# Ahora vemos algunas funciones de recommendation

modelo_recomendacion <- recommenderRegistry$get_entries(dataType = "realRatingMatrix")
names(modelo_recomendacion)

lapply(modelo_recomendacion, "[[", "description")
#Usaremos la IBCF recomendar basando en item-base filtro colaborativo

modelo_recomendacion$IBCF_realRatingMatrix$parameters

#El Sistema de filtro colaborativo permite recoger las preferencias de los usuarios
#y brindar recomendaciones a usuarios con las mismas preferencias. De esa manera
#el usuario A que tenga x preferencia recibirá recomendaciones de peliculas vistas
#por el usuario B que posea la misma preferencia, y viceversa.
#Recomendar peliculas se basa en la similitud de las preferencias de los usuarios
#Con recommenderlabs se puede calcular la similitud con operadores como pearsone,
#cosine o jaccard

similitud <- similarity(ratingMatrix[1:4, ],
                             method = "cosine",
                             which = "users")
as.matrix(similitud)

image(as.matrix(similitud), main = "Similitud del usuario")

#En la matrix obtenida, tenemos 4 usuarios, cada columna y fila representa a un
#usuario y la similitud entre ellos

#ahora delimitamos la similitud que se comparte entre peliculas

similitud_peliculas <- similarity(ratingMatrix[ ,1:4],
                                  method = "cosine",
                                  which = "items")
as.matrix(similitud_peliculas)

image(as.matrix(similitud_peliculas), main = "Similitud entre películas")

#Ahora sacamos los valores unicos de raitings
valor_rating <- as.vector(ratingMatrix@data)
unique(valor_rating)
#Vemos que tenemos valores desde 0.0 a 5.0 subiendo cada 0.5 puntos
#Haremos una tabla con los ratings que muestren los valores unicos

tabla_de_ratings <- table(valor_rating)
tabla_de_ratings

#Ahora visualizaremos los datos de las peliculas más vistas, se cuenta la cant. 
#de vistas y luego organizaremos decrecientemente.

vistas_peliculas <- colCounts(ratingMatrix) #cuenta las vistas para cada pelicula
tabla_vistas <- data.frame(pelicula = names(vistas_peliculas),
                           vistas = vistas_peliculas)#crea un df de vistas
tabla_vistas <- tabla_vistas[order(tabla_vistas$vistas,
                                   decreasing = T),]#ordena decrecientemente
tabla_vistas$title <- NA
for (index in 1:10325){
  tabla_vistas[index,3] <- as.character(subset(movie_data, movie_data$movieId
                                               == tabla_vistas[index,1])$title)
}
tabla_vistas[1:6,]

#Ahora tenemos una tabla con el id de pelicula, la cantidad de vistas y el título
#lo visualizamos en ggplot
ggplot(tabla_vistas[1:6, ], aes(x = title, y = vistas)) + 
  geom_bar(stat="identity", fill = "steelblue")+
  geom_text(aes(label = vistas), vjust = -0.3, size = 3.5) +
  theme(axis.text.x = element_text (angle = 45, hjust = 1)) +
  ggtitle("Total de vistas para el Top de Peliculas")
#Visualizaremos las primeras 25 filas y 25 columnas

image(ratingMatrix[1:20, 1:25], axes = F, main = "Mapa de color de las primeras
      columnas y filas")
#Limpiaremos y restringiremos un poco más nuestros datos, determinaremos un umbral
#de que haya almenos 50 usuarios visto y calificado una pelicula.

rating_peliculas <- ratingMatrix[rowCounts(ratingMatrix)>50,
                                 colCounts(ratingMatrix)>50]
rating_peliculas
#Ahora vemos que tenemos una selección de datos más utiles y reducida.
#Ahora podemos delinear una matriz con usuarios más relevantes.

minimo_peliculas <- quantile(rowCounts(rating_peliculas), 0.98)
minimo_usuarios <- quantile(colCounts(rating_peliculas), 0.98)
image(rating_peliculas[rowCounts(rating_peliculas)>minimo_peliculas,
                       colCounts(rating_peliculas)>minimo_usuarios],
      main = "Mapa de color - TOP usuarios y películas")
#Ahora vamos a visualizar la distribución promedio de rankings por usuarios

promedio_rating <- rowMeans(rating_peliculas)
qplot(promedio_rating, fill = I("steelblue"), col = I("red"))+
  ggtitle("Distribución del promedio de rating por usuario")
#Normalizaremos los datos para evitar sesgos con los usuarios que puntuan muy 
#seguido calificaciones muy bajas o muy altas

rating_normalizado <- normalize(rating_peliculas)
sum(rowMeans(rating_normalizado) > 0.00001)

image(rating_normalizado[rowCounts(rating_normalizado)> minimo_peliculas,
                         colCounts(rating_normalizado)> minimo_usuarios],
      main = "Rating normalizado del promedio de los TOP usuarios")

#Procedemos a binarizar los datos, de tal manera que los datos >3 serán 1 y sino
#0.

binario_minimo_pelicula <- quantile(rowCounts(rating_peliculas), 0.95)
binario_minimo_usuario <- quantile(colCounts(rating_peliculas), 0.95)

buen_rating_pelicula <- binarize(rating_peliculas, minRating = 3)
image(buen_rating_pelicula[rowCounts(rating_peliculas) > binario_minimo_pelicula,
                           colCounts(rating_peliculas) > binario_minimo_usuario],
      main = "Mapa de color de los top user y peliculas binarizado")
##Creación del sistema de fitro por colaboración##
#
#

dato_muestra <- sample(x = c(T,F),
                       size = nrow(rating_peliculas),
                       replace = T,
                       prob = c(0.8 , 0.2))
dato_entrenamiento <- rating_peliculas[dato_muestra, ]
dato_prueba <- rating_peliculas[!dato_muestra, ]

##Construyendo el Sistema de Recomendación##

sistema_recomendacion <- recommenderRegistry$get_entries(dataType ="realRatingMatrix")
sistema_recomendacion$IBCF_realRatingMatrix$parameters

modelo_recomendacion <- Recommender(data = dato_entrenamiento,
                                    method = "IBCF",
                                    parameter = list(k = 30))
modelo_recomendacion
class(modelo_recomendacion)
##

modelo_info <- getModel(modelo_recomendacion)
class(modelo_info$sim)
dim(modelo_info$sim)
top_items <- 20
image(modelo_info$sim[1:top_items, 1:top_items],)

##Sumamos filas y columnas cuya similitud sea mayor a 0

suma_filas <- rowSums(modelo_info$sim > 0)
table (suma_filas)

suma_columnas <- colSums(modelo_info$sim > 0)

qplot(suma_columnas, fill = I("steelblue"), col = I("red")) + ggtitle (
  "Distribución de la suma de columnas")

#Haremos una variable que inicialice con 10,será el n° de peliculas por usuario
#

top_recomendaciones <- 10 # es la cant. de recomendaciones que recibirá cada usuario

recomendacion_prediccion <- predict(object = modelo_recomendacion,
                                    newdata = dato_prueba,
                                    n = top_recomendaciones)
recomendacion_prediccion

usuario1 <- recomendacion_prediccion@items[[1]] #recomendación para el 1er usuario

peliculas_usuario1 <- recomendacion_prediccion@itemLabels[usuario1]
peliculas_usuario2 <- peliculas_usuario1
for (index in 1:10) (
  peliculas_usuario2[index] <- as.character(subset(movie_data,
                                                   movie_data$movieId == peliculas_usuario1[index])
                                            $title)
)
peliculas_usuario2

recomendacion_matriz <- sapply(recomendacion_prediccion@items,
                               function(x){as.integer(colnames(rating_peliculas)
                                                      [x])})# matriz con las recomendaciones
#para cada usuario
recomendacion_matriz[,1:4]
