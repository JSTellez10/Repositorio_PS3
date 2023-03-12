
#------------------------------------------------------------------------------#
#
#                                4 - MODELS
#
#------------------------------------------------------------------------------#

#Particiones Train-Test----

p_load(SuperLearner, caret)
set.seed(1011)

inTrain <- createDataPartition(y = train$price, p = .7, list = FALSE)

train_7 <- train[ inTrain,]
test_3  <- train[-inTrain,]


#Regresiones----

glimpse(train)

##Regresion 1----
reg1 <- lm(price~distancia_parque+distancia_museo+distancia_ips+distancia_ese +distancia_colegios+distancia_cai+ 
             distancia_best+distancia_centrof+distancia_cuadrantes+distancia_buses+distancia_tm+
             total_eventos_2022, data = train_7)

stargazer(reg1, type = "text", dep.var.labels = "Precio de venta", digits = 4)
summary(reg1)

test_3$y_hat1 <- predict(reg1, newdata = test_3)
MAE_model1 <- with(test_3, mean(abs(price - y_hat1))) #Calculating the MAE
MAE_model1


##Regresion 2----
reg2 <- lm(price~surface_total+surface_covered+rooms+bedrooms+bathrooms+property_type+area_maxima+
             distancia_parque+distancia_museo+distancia_ips+distancia_ese+distancia_colegios+distancia_cai+
             distancia_best+distancia_centrof+distancia_cuadrantes+distancia_buses+distancia_tm+
             total_eventos_2022, data = train_7)

stargazer(reg2, type = "text", dep.var.labels = "Precio de venta", digits = 4)
summary(reg2)

test_3$y_hat2 <- predict(reg2, newdata = test_3)
MAE_model2 <- with(test_3, mean(abs(price - y_hat2))) #Calculating the MSE
MAE_model2

reg21 <- lm(price~surface_total+surface_covered+rooms+bedrooms+bathrooms+property_type+area_maxima+
              distancia_parque+distancia_museo+distancia_ips+distancia_ese+distancia_colegios+distancia_cai+
              distancia_best+distancia_centrof+distancia_cuadrantes+distancia_buses+distancia_tm+
              total_eventos_2022+I(total_eventos_2022^2)+I(distancia_cai^2)+I(distancia_colegios^2), data = train_7)

stargazer(reg21, type = "text", dep.var.labels = "Precio de venta", digits = 4)
summary(reg21)

test_3$y_hat21 <- predict(reg21, newdata = test_3)
MAE_model21 <- with(test_3, mean(abs(price - y_hat21))) #Calculating the MSE
MAE_model21


##Regresion 3----
reg22 <- lm(price~rooms+bedrooms+bathrooms+property_type+area_maxima+
              distancia_parque+distancia_museo+distancia_ips+distancia_ese+distancia_colegios+distancia_cai+
              distancia_best+distancia_centrof+distancia_cuadrantes+distancia_buses+distancia_tm+
              total_eventos_2022+I(total_eventos_2022^2)+I(distancia_cai^2)+I(distancia_colegios^2), data = train_7)

stargazer(reg22, type = "text", dep.var.labels = "Precio de venta", digits = 4)
summary(reg22)

test_3$y_hat22 <- predict(reg22, newdata = test_3)
MAE_model22 <- with(test_3, mean(abs(price - y_hat22))) #Calculating the MSE
MAE_model22


#Elastic Net ----

set.seed(10101)
fitControl <- trainControl(method = "cv", number = 10)

##EN1----
EN <-  train(price~rooms+bedrooms+bathrooms+property_type+area_maxima+distancia_parque+distancia_museo+
               distancia_ips+distancia_ese+distancia_colegios+
               distancia_best+distancia_centrof+distancia_cuadrantes+distancia_buses+distancia_tm+
               total_eventos_2022+I(total_eventos_2022^2)+
               (distancia_colegios^2), data = train_7, 
             method = 'glmnet', 
             trControl = fitControl,
             tuneGrid = expand.grid(alpha = 0.5,lambda = seq(0.001,0.02,by = 0.001)),
             preProcess = c("center", "scale")) 

coef_EN <- coef(EN$finalModel, EN$bestTune$lambda)
coef_EN

test_3$y_hat3 <- predict(EN, newdata = test_3)
MAE_model3 <- with(test_3, mean(abs(price - y_hat3))) #Calculating the MSE
MAE_model3


##EN2----

set.seed(10101)
EN2 <-  train(price~rooms+bedrooms+bathrooms+property_type+area_maxima+
                distancia_parque+distancia_museo+distancia_ips+distancia_ese+distancia_colegios+distancia_cai+
                distancia_best+distancia_centrof+distancia_cuadrantes+distancia_buses+distancia_tm+
                total_eventos_2022+I(total_eventos_2022^2), data = train_7, 
              method = 'glmnet', 
              trControl = fitControl,
              tuneGrid = expand.grid(alpha = seq(0,1,by = 0.1),lambda = seq(0.001,0.02,by = 0.001)),
              preProcess = c("center", "scale")) 

coef_EN2 <- coef(EN2$finalModel, EN2$bestTune$lambda)
coef_EN2

test_3$y_hat4 <- predict(EN2, newdata = test_3)
MAE_model4 <- with(test_3, mean(abs(price - y_hat4))) #Calculating the MSE
MAE_model4

##EN3----
set.seed(10101)
EN3 <-  train(price~rooms+bedrooms+bathrooms+property_type+area_maxima+
                distancia_parque+distancia_museo+distancia_ips+distancia_ese+distancia_colegios+distancia_cai+
                distancia_best+distancia_centrof+distancia_cuadrantes+distancia_buses+distancia_tm+
                total_eventos_2022+I(total_eventos_2022^2)+ I(distancia_cai^2)+(distancia_colegios^2), data = train_7, 
              method = 'glmnet', 
              trControl = fitControl,
              tuneGrid = expand.grid(alpha = seq(0,1,by = 0.1),lambda = seq(0.001,0.02,by = 0.001)),
              preProcess = c("center", "scale")) 

coef_EN3 <- coef(EN3$finalModel, EN3$bestTune$lambda)
coef_EN3

test_3$y_hat5 <- predict(EN3, newdata = test_3)
MAE_model5 <- with(test_3, mean(abs(price - y_hat5))) #Calculating the MSE
MAE_model5


##EN4----
set.seed(10101)
EN4 <-  train(price~rooms+bedrooms+bathrooms+property_type+area_maxima+
                distancia_parque+distancia_museo+distancia_ips+distancia_ese+distancia_colegios+distancia_cai+
                distancia_best+distancia_centrof+distancia_cuadrantes+distancia_buses+distancia_tm+
                total_eventos_2022+I(total_eventos_2022^2)+ I(distancia_cai^2)+I(distancia_colegios^2), data = train_7, 
              method = 'glmnet', 
              trControl = fitControl,
              tuneGrid = expand.grid(alpha = seq(0,1,by = 0.1),lambda = seq(0.001,0.02,by = 0.001)),
              preProcess = c("center", "scale")) 

coef_EN4 <- coef(EN4$finalModel, EN4$bestTune$lambda)
coef_EN4

test_3$y_hat6 <- predict(EN4, newdata = test_3)
MAE_model6 <- with(test_3, mean(abs(price - y_hat6))) #Calculating the MSE
MAE_model6


set.seed(10101)
fitControl <- trainControl(method = "cv", number = 10)

##EN5----
set.seed(10101)
EN5 <-  train(price~rooms+bedrooms+bathrooms+property_type+area_maxima+
                distancia_parque+distancia_museo+distancia_ips+distancia_ese+distancia_colegios+distancia_cai+
                distancia_best+distancia_centrof+distancia_cuadrantes+distancia_buses+distancia_tm+
                total_eventos_2022+I(total_eventos_2022^2)+I(total_eventos_2022^3) + I(distancia_cai^2)+I(distancia_colegios^2)+
                I(distancia_parque*distancia_buses) + I(total_eventos_2022*distancia_cai) + I(distancia_tm*distancia_buses)+
                I(distancia_ips*distancia_ese) + I(distancia_parque^2),
              data = train_7, 
              method = 'glmnet', 
              trControl = fitControl,
              tuneGrid = expand.grid(alpha = seq(0,1,by = 0.1),lambda = seq(0.001,0.02,by = 0.001)),
              preProcess = c("center", "scale")) 

coef_EN5 <- coef(EN5$finalModel , EN5$bestTune$lambda)
coef_EN5

test_3$y_hat7 <- predict(EN5, newdata = test_3)
MAE_model7 <- with(test_3, mean(abs(price - y_hat7))) #Calculating the MSE
MAE_model7

##EN6----
set.seed(10101)
EN6 <-  train(price~property_type+area_maxima+
                distancia_parque+distancia_museo+distancia_ips+distancia_ese+distancia_colegios+distancia_cai+
                distancia_best+distancia_centrof+distancia_cuadrantes+distancia_buses+distancia_tm+
                total_eventos_2022+I(total_eventos_2022^2)+I(total_eventos_2022^3) + I(distancia_cai^2)+I(distancia_colegios^2)+
                I(distancia_parque*distancia_buses) + I(total_eventos_2022*distancia_cai) + I(distancia_tm*distancia_buses)+
                I(distancia_ips*distancia_ese) + I(distancia_parque^2)+mts2+ I(mts2^2)+surface_total_imp+
                surface_covered_imp+bedrooms_imp+bathrooms_imp+rooms_imp,
              data = train_7, 
              method = 'glmnet', 
              trControl = fitControl,
              tuneGrid = expand.grid(alpha = seq(0,1,by = 0.1),lambda = seq(0.001,0.02,by = 0.001)),
              preProcess = c("center", "scale")) 

coef_EN6 <- coef(EN6$finalModel , EN6$bestTune$lambda)
coef_EN6

test_3$y_hat8 <- predict(EN6, newdata = test_3)
MAE_model8 <- with(test_3, mean(abs(price - y_hat8))) #Calculating the MSE
MAE_model8

#Random forest----------------------------------------------------------

tunegrid_rf <- expand.grid(mtry = c(3, 5, 10), #Mejor modelo
                           min.node.size = c(10, 30, 50, 70, 100),
                           splitrule = "variance")

control_rf <- trainControl(method = "cv", number = 5)

modelo_rf <- train(price~rooms+bedrooms+bathrooms+property_type+area_maxima+
                     distancia_parque+distancia_museo+distancia_ips+distancia_ese+distancia_colegios+distancia_cai+
                     distancia_best+distancia_centrof+distancia_cuadrantes+distancia_buses+distancia_tm+
                     total_eventos_2022+I(total_eventos_2022^2)+I(total_eventos_2022^3) + I(distancia_cai^2)+I(distancia_colegios^2)+
                     I(distancia_parque*distancia_buses) + I(total_eventos_2022*distancia_cai) + I(distancia_tm*distancia_buses)+
                     I(distancia_ips*distancia_ese) + I(distancia_parque^2),
                   data = train_7, 
                   method = "ranger", 
                   trControl = control_rf,
                   metric = 'RMSE', 
                   tuneGrid = tunegrid_rf)

Grilla_1 <- ggplot(modelo_rf$results, 
                   aes(x = min.node.size, y = RMSE, 
                       color = as.factor(mtry))) +
  geom_line() +
  geom_point() +
  labs(title = "Resultados del grid search",
       x = "Mínima cantidad de observaciones por hoja",
       y = "RMSE (Cross-Validation)") +
  scale_color_discrete("Número de predictores seleccionados al azar") +
  theme_bw() +
  theme(legend.position = "bottom")

Grilla_1

test_3$y_hat9 <- predict(modelo_rf, newdata = test_3)
MAE_model9 <- with(test_3, mean(abs(price - y_hat9))) #Calculating the MSE
MAE_model9


#Random forest2----------------------------------------------------------

tunegrid_rf <- expand.grid(mtry = c(3, 5, 10), 
                           min.node.size = c(10, 30, 50, 70, 100),
                           splitrule = "variance")

control_rf2 <- trainControl(method = "cv", number = 10)

modelo_rf2 <- train(price~rooms+bedrooms+bathrooms+property_type+area_maxima+
                      distancia_parque+distancia_museo+distancia_ips+distancia_ese+distancia_colegios+distancia_cai+
                      distancia_best+distancia_centrof+distancia_cuadrantes+distancia_buses+distancia_tm+
                      total_eventos_2022+I(total_eventos_2022^2)+I(total_eventos_2022^3) + I(distancia_cai^2)+I(distancia_colegios^2)+
                      I(distancia_parque*distancia_buses) + I(total_eventos_2022*distancia_cai) + I(distancia_tm*distancia_buses)+
                      I(distancia_ips*distancia_ese) + I(distancia_parque^2)+mts2+ I(mts2^2)+surface_total_imp+
                      surface_covered_imp+bedrooms_imp+bathrooms_imp+rooms_imp,
                    data = train_7, 
                    method = "ranger", 
                    trControl = control_rf2,
                    metric = 'RMSE', 
                    tuneGrid = tunegrid_rf)

Grilla_12 <- ggplot(modelo_rf2$results, 
                    aes(x = min.node.size, y = RMSE, 
                        color = as.factor(mtry))) +
  geom_line() +
  geom_point() +
  labs(title = "Resultados del grid search",
       x = "Mínima cantidad de observaciones por hoja",
       y = "RMSE (Cross-Validation)") +
  scale_color_discrete("Número de predictores seleccionados al azar") +
  theme_bw() +
  theme(legend.position = "bottom")

Grilla_12

test_3$y_hat10 <- predict(modelo_rf2, newdata = test_3)
MAE_model10 <- with(test_3, mean(abs(price - y_hat10))) #Calculating the MSE
MAE_model10
