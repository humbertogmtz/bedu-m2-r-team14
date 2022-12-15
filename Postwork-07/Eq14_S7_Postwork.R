#Utilizando el siguiente vector numérico, realiza lo que se indica:
  
  
  url = "https://raw.githubusercontent.com/beduExpert/Programacion-R-Santander-2022/main/Sesion-07/Data/global.txt"
  Global <- scan(url, sep="")
  Global
  str(Global)
  class(Global)
  
  
  #1.Crea un objeto de serie de tiempo con los datos de Global. La serie debe ser mensual 
  #comenzando en Enero de 1856
  
  st1 <- ts(Global, start=c(1856,1), freq=12)
  class(st1)
  
  #2. Realiza una gráfica de la serie de tiempo anterior
  
  
  plot((st1), 
       main = "Serie de tiempo de Global", 
       xlab = "Tiempo",
       sub = "Enero de 1856 - Diciembre de 2005")
  
  
  #3. Ahora realiza una gráfica de la serie de tiempo anterior, transformando a la primera diferencia
  
  plot(diff(st1), 
       main = "Serie de tiempo de Global", 
       xlab = "Tiempo",
       sub = "Gráfica primera diferencia")
  
  
  #4. ¿Consideras que la serie es estacionaria en niveles o en primera diferencia?
  #En primera diferencia
  
  
  #5. Con base en tu respuesta anterior, obtén las funciones de autocorrelación y autocorrelación parcial?
  acf(diff(st1))
  pacf(diff(st1))
  
  
  

  
  
  
  

  