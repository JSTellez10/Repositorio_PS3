
#------------------------------------------------------------------------------#
#
#                                4 - SUPER LEARNER
#
#------------------------------------------------------------------------------#

#Particiones Train-Test

  p_load(SuperLearner, caret)
  set.seed(1011)
  
  inTrain <- createDataPartition(y = train$price, p = .7, list = FALSE)
  
  train_7 <- train[ inTrain,]
  test_3  <- train[-inTrain,]
  
  colnames(train)
  
  YSL<-train_7$price
  
  XSL<- train_7 %>% select(surface_total,surface_covered,rooms,bedrooms,bathrooms,property_type,area_maxima,
                             distancia_parque,distancia_museo,distancia_ips,distancia_ese,distancia_colegios,distancia_cai, 
                             distancia_best,distancia_centrof,distancia_cuadrantes,distancia_buses,distancia_tm,
                             total_eventos_2022)
  
#Regresiones Simples----

  glimpse(train)
  
  reg1 <- lm(price~distancia_parque+distancia_museo+distancia_ips+distancia_ese +distancia_colegios+distancia_cai+ 
               distancia_best+distancia_centrof+distancia_cuadrantes+distancia_buses+distancia_tm+
               total_eventos_2022, data = train_7)
  
  stargazer(reg1, type = "text", dep.var.labels = "Precio de venta", digits = 4)
  summary(reg1)
  
  test_3$y_hat <- predict(reg1, newdata = test_3)
  MSE_model1 <- with(test_3, mean((price - y_hat)^2)) #Calculating the MSE
  MSE_model1

#Elastic Net ----

  set.seed(123)
  fitControl <- trainControl(method = "cv", number = 5)

  EN <-  train(y=YSL,x=XSL, 
               method = 'glmnet', 
               trControl = fitControl,
               tuneGrid = expand.grid(alpha = seq(0,1,by = 0.1),lambda = seq(0.001,0.02,by = 0.001)),
               preProcess = c("center", "scale")) 
  
  coef_EN <- coef(EN$finalModel, EN$bestTune$lambda)
  coef_EN

#Super Learner----


  
  sl.lib <- c("SL.randomForest", "SL.lm") #lista de los algoritmos a correr
  
  #Fit using the SuperLearner package
  
  fitY <- SuperLearner(Y = YSL,  X= data.frame(XSL),
                       method = "method.NNLS", # combinación convexa
                       SL.library = sl.lib)
  
  fitY
  
  test_3 <- test_3  %>%  mutate(yhat_Sup = predict(fitY, newdata = data.frame(test_3), onlySL = T)$pred)
  head(test_3$yhat_Sup)
  
  with(test,mean(abs(logprice-yhat_Sup))) #MAE
  
  #Customize the defaults for random forest
  custon_ranger = create.Learner("SL.ranger", params = list(num.trees = 1000))
  
  #Look at the object
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
  
  fitY_long <- SuperLearner(Y = YSL, X = data.frame(XSL),
                            method = "method.NNLS", SL.library = sl.lib2)
  
  fitY_long
  
  #Spatial Cross Validation
  
  train_sf_4326 <- sf::st_as_sf(train, coords = c("lon", "lat"), crs = 4326)
  
  set.seed(123)
  
  block_folds <- spatial_block_cv(train_sf_4326, v = 15)
  
  autoplot(block_folds) + theme_bw()
  
  set.seed(123)
  
  cluster_folds <- spatial_clustering_cv(train_sf_4326, v = 15)
  autoplot(cluster_folds) + theme_bw()
  
  set.seed(123)
  
  location_folds <- spatial_leave_location_out_cv(train_sf_4326, group = Neighborhood, v = 15)
  
  autoplot(location_folds)+ theme_bw()
  
  table(train_sf_4326$Neighborhood)
  
  train_sf_4326 <- train_sf_4326   %>% mutate(Neighborhood=droplevels(Neighborhood))
  
  table(train_sf_4326$Neighborhood)
  
  length(unique(train_sf_4326$Neighborhood))
  
  test_neigh<- train_sf_4326  %>% filter(Neighborhood=="North_Ames")
  test_neigh <- test_neigh   %>% mutate(Neighborhood=droplevels(Neighborhood))
  train_neigh<- train_sf_4326  %>% filter(Neighborhood!="North_Ames")
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
  