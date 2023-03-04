
 
 #Regresión----
 
 reg1 <- lm(price~distancia_hospital + area_hospital, data = train)
 stargazer(reg1,type = "text", dep.var.labels = "Precio del Inmueble", digits = 0)
 