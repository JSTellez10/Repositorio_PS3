
#------------------------------------------------------------------------------#
#
#                                4 - CLUSTERS
#
#------------------------------------------------------------------------------#
 
#Regresiones----
 
  reg1 <- lm(price~distancia_hospital + area_hospital, data = train)

  stargazer(reg1,type = "text", dep.var.labels = "Precio del Inmueble", digits = 0)
 
 
#Clusters----
  
  traind_sf1 <- sf::st_as_sf(train, coords = c("Longitude", "Latitude"), remove=FALSE, crs = 4326)
  
  class(traind_sf1)
  
  ggplot() + geom_sf(data=traind_sf1) + theme_bw()
  
  map1 <- leaflet() %>% addTiles() %>% addCircleMarkers(data = traind_sf1, opacity = 0.5, radius = 0.5)
  map1
  
  map2 <- leaflet() %>% addProviderTiles(providers$Stamen.Toner) %>% addCircles(data = traind_sf1, opacity = 0.5, radius = 0.5)
  map2
  
  set.seed(101011)
  
  traind_sf2 <- traind_sf1  %>%  select(geometry) #me quedo sólo con la geometría
  head(traind_sf2)
  
  traind_sf2 <- st_distance(traind_sf2) #matriz de distancias
  head(traind_sf2)
  
  
  traind_sf2 <- units::drop_units(traind_sf2) #elimina las unidades de la matriz
  head(traind_sf2)
  
  k3 <- kmeans(traind_sf2, centers = 3, nstart = 25)
  str(k3)
  
  traind_sf1 <- traind_sf1 %>% mutate(clusters=factor(k3$cluster))
  
  ggplot() + geom_sf(data=traind_sf1,aes(col=clusters)) + theme_bw() #graficamos las predicciones
  
  
#Método del codo----
  
  #Función que calcula la SSR within-cluster 
  wss <- function(k) {
                      kmeans(db, k, nstart = 25 )$tot.withinss
                      }
  
  #Calculamos y graficamos para k = 1 hasta k = 12
  wss_values <- sapply(1:12,wss)
  
  plot(1:12, wss_values,
       type="b", pch = 19, frame = FALSE, 
       xlab="Número de clusters (K)",
       ylab="SSR within-clusters total")
  
  #Coeficiente de Silhouette
  
  p_load("cluster")
  # función para extraer el coeficiente de silhouette
  
  avg_sil <- function(k) {
    km.res <- kmeans(db, centers = k, nstart = 25)
    ss <- cluster::silhouette(km.res$cluster, dist(db))
    mean(ss[, 3])
  }
  
  # Calcular el coeficiente de silhouette para  k = 2 hasta k = 12
  valores_sil <-  sapply(2:12,avg_sil)
  
  plot(2:12, valores_sil,
       type = "b", pch = 19, frame = FALSE, 
       xlab="Número de clusters (K)",
       ylab = "Coeficiente de Silhouette")
  
  k4 <- kmeans(db, centers = 4, nstart = 25)
  
  ames_sample<- ames_sample %>% mutate(clusters=factor(k4$cluster))
  
  
  ggplot() +
    geom_sf(data=ames_sample,aes(col=clusters)) + #graficamos las predicciones
    theme_bw()
  
  #Super learner
  
  ames<- ames  %>% mutate(logprice=log(Sale_Price))
  
  p_load("caret")
  set.seed(1011)
  inTrain <- createDataPartition(
    y = ames$logprice,## La variable dependiente u objetivo 
    p = .7, ## Usamos 70%  de los datos en el conjunto de entrenamiento 
    list = FALSE)
  
  train <- ames[ inTrain,]
  test  <- ames[-inTrain,]
  colnames(train)
  
  ySL<-train$logprice
  XSL<- train  %>% select(Year_Built, Bldg_Type, Gr_Liv_Area)
  
  sl.lib <- c("SL.randomForest", "SL.lm") #lista de los algoritmos a correr
  
  # Fit using the SuperLearner package,
  
  fitY <- SuperLearner(Y = ySL,  X= data.frame(XSL),
                       method = "method.NNLS", # combinación convexa
                       SL.library = sl.lib)
  
  fitY
  
  test <- test  %>%  mutate(yhat_Sup=predict(fitY, newdata = data.frame(test), onlySL = T)$pred)
  head(test$yhat_Sup)
  
  with(test,mean(abs(logprice-yhat_Sup))) #MAE
  
  #Customize the defaults for random forest.
  custon_ranger = create.Learner("SL.ranger", params = list(num.trees = 1000))
  
  #Look at the object.
  custon_ranger$names
  
  custom_rf = create.Learner("SL.randomForest",
                             tune = list(mtry = round(c(1, sqrt(4), 3))))
  custom_rf$names
  
  #Customize the defaults for random forest.
  custon_glmnet = create.Learner("SL.glmnet", tune = list(alpha = seq(0, 1, length.out=5)))
  
  #Look at the object.
  custon_glmnet$names
  
  sl.lib2 <- c("SL.randomForest", "SL.lm",custon_ranger$names, custon_ranger$lmnet$names,custom_rf$names)
  sl.lib2
  
  #Fit (takes forever)
  
  fitY_long <- SuperLearner(Y = ySL, X = data.frame(XSL),
                            method = "method.NNLS", SL.library = sl.lib2)
  
  fitY_long
  
  #Spatial Cross Validation
  
  ames_sf <- sf::st_as_sf(
    ames,
    # "coords" is in x/y order -- so longitude goes first!
    coords = c("Longitude", "Latitude"),
    # Set our coordinate reference system to EPSG:4326,
    # the standard WGS84 geodetic coordinate reference system
    crs = 4326
  )
  
  set.seed(123)
  block_folds <- spatial_block_cv(ames_sf, v = 15)
  
  autoplot(block_folds) + theme_bw()
  
  set.seed(123)
  cluster_folds <- spatial_clustering_cv(ames_sf, v = 15)
  autoplot(cluster_folds) + theme_bw()
  
  set.seed(123)
  location_folds <- 
    spatial_leave_location_out_cv(
      ames_sf,
      group = Neighborhood,
      v = 15
    )
  
  autoplot(location_folds)+ theme_bw()
  
  table(ames_sf$Neighborhood)
  
  ames_sf <- ames_sf   %>% mutate(Neighborhood=droplevels(Neighborhood))
  
  table(ames_sf$Neighborhood)
  
  length(unique(ames_sf$Neighborhood))
  
  test_neigh<- ames_sf  %>% filter(Neighborhood=="North_Ames")
  test_neigh <- test_neigh   %>% mutate(Neighborhood=droplevels(Neighborhood))
  train_neigh<- ames_sf  %>% filter(Neighborhood!="North_Ames")
  train_neigh <- train_neigh   %>% mutate(Neighborhood=droplevels(Neighborhood))
  
  y_neigh<-train_neigh$logprice
  X_neigh<- train_neigh  %>% select(Year_Built, Bldg_Type, Gr_Liv_Area)  %>% st_drop_geometry()
  
  index <- split(1:nrow(train_neigh),train_neigh$Neighborhood)
  
  index
  
  folds<-length(index)
  folds
  
  fitY_neigh <- SuperLearner(Y = y_neigh, X = data.frame(X_neigh),
                             method = "method.NNLS", SL.library = sl.lib,
                             cvControl = list(V = folds, validRows = index))
  
  fitY_neigh
  
  yhat_SL_neigh=predict(fitY_neigh, newdata = data.frame(test_neigh), onlySL = T)$pred
  
  with(test_neigh,mean(abs(logprice-yhat_SL_neigh)))
  
  with(test ,mean(abs(logprice-yhat_Sup)))
  