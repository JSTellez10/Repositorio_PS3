
#------------------------------------------------------------------------------#
#
#                                4 - CLUSTERS
#
#------------------------------------------------------------------------------#
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
  