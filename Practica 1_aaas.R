#Linea Recta 1
m <- -1 
b <- 0 # 


f <- function (m , b , x){
  return ( m * x + b )
 }

x <- seq ( -5 , 5, 1)
y <- f(m , b , x) 
plot(x, y, type = "l", xlab = "Eje X", ylab = "Eje Y") 
abline(h = 0, v = 0)

#Linea Recta 2
m <- 1 
b <- 8 


f <- function (m , b , x){
  return ( m * x + b )
}

x <- seq ( -5 , 5, 1)
y <- f(m , b , x) 
plot(x, y, type = "l", xlab = "Eje X", ylab = "Eje Y") 
abline(h = 0, v = 0)

#Parabola 1
g <- function ( x)
  {
   return (3*x ^2 + 6*x - 3)
  }

x <- seq ( -10 , 10 , 0.01) 
y <- g(x)

plot (x , y , type = "l", xlab = "Eje X", ylab = " Eje Y") 
abline ( h = 0, v = 0) 

#Parabola 2
g <- function ( x)
{
  return (x ^2 + 2*x + 0)
}

x <- seq ( -10 , 10 , 0.01) 
y <- g(x)

plot (x , y , type = "l", xlab = "Eje X", ylab = " Eje Y") 
abline ( h = 0, v = 0) 

#Circunferencia 1
circunferencia  <- function(h, k, r){
  if (r  >= 0){ 
    if (r == 0){ 
       plot(x = h, y = k, xlab = "Eje X", ylab = "Eje Y")  
      } else{
        x <- seq(h - r, h + r, 0.01) 
        ypositiva  <- k + sqrt(r^2 - ((x - h)^2)) 
        ynegativa  <- k - sqrt(r^2 - ((x - h)^2)) 
       
        plot(x, ypositiva , type = "l", xlim = c(h - (r + 1), h + (r + 1)), ylim = c(k - (r + 1), k + (r + 1)), xlab = "Eje X", ylab = "Eje Y")
        lines(x, ynegativa , type = "l") 
        abline(h = 0, v = 0) 
        points(x = h, y = k, col = "red") 
        }
    } else{
      return(print("El radio  no es  positivo."))
      }
  }

  circunferencia (1, 1, 1)

#Circunferencia 2
circunferencia  <- function(h, k, r){
  if (r  >= 0){ 
    if (r == 0){ 
       plot(x = h, y = k, xlab = "Eje X", ylab = "Eje Y") 
      } else{
        x <- seq(h - r, h + r, 0.01) 
        ypositiva  <- k + sqrt(r^2 - ((x - h)^2)) 
        ynegativa  <- k - sqrt(r^2 - ((x - h)^2)) 
        
        plot(x, ypositiva , type = "l", xlim = c(h - (r + 1), h + (r + 1)), ylim = c(k - (r + 1),k + (r + 1)),xlab = "Eje X", ylab = "Eje Y")
        lines(x, ynegativa , type = "l") 
        abline(h = 0, v = 0) 
        points(x = h, y = k, col = "red") 
      }
    } else{
      return(print("El radio  no es  positivo."))
    }
  }
# ejecutamos  la  funcion
circunferencia (2, 2, 2)

#Elipse 1
elipse <- function(h, k, a, b, horizontal){
  if(a > b){
    c <- sqrt(a^2 - b^2)
    if(horizontal){
        x <- seq(h-a, h+a, 0.01)
        ypositiva <- k + sqrt ((b^2 - (b^2/a^2) * ((x - h) ^2) ))
        ynegativa <- k - sqrt ((b^2 - (b^2/a^2) * ((x - h) ^2) ))
        plot(x, ypositiva, type = "l", xlim = c(h - (a + 1 ), h + (a + 1)), ylim = c(k - (b + 1), k + (b + 1)))
        lines(x, ynegativa, type = "l")
        abline(h = 0, v = 0)
        points(x = c(h - c, h + c), y = c(k, k), col = "red")
     }  else{
        x <- seq(h - b, h + b, 0.01)
        ypositiva <- k + sqrt ((a^2 - (a^2/b^2) * ((x - h) ^2)))
        ynegativa <- k - sqrt ((a^2 - (a^2/b^2) * ((x - h) ^2)))
        plot (x, ypositiva , type = "l", xlim = c(h - (b + 1) , h + (b + 1)), ylim = c(k - (a + 1) , k + (a + 1)))
        lines (x, ynegativa , type = "l")
        abline (h = 0, v = 0)
        points (x = c(h, h), y = c(k - c, k + c), col = " red ")
    }
  } else{
    return(print("No cumple con las condiciones para ser una elipse. (a no es mayor que b)"))
  }
}


elipse(2, 2, 25, 9, T) 
  
#Elipse 2
elipse <- function(h, k, a, b, horizontal){
  if(a > b){
    c <- sqrt(a^2 - b^2)
    if(horizontal){
      x <- seq(h-a, h+a, 0.01)
      ypositiva <- k + sqrt ((b^2 - (b^2/a^2) * ((x - h) ^2) ))
      ynegativa <- k - sqrt ((b^2 - (b^2/a^2) * ((x - h) ^2) ))
      plot(x, ypositiva, type = "l", xlim = c(h - (a + 1 ), h + (a + 1)), ylim = c(k - (b + 1), k + (b + 1)))
      lines(x, ynegativa, type = "l")
      abline(h = 0, v = 0)
      points(x = c(h - c, h + c), y = c(k, k), col = "red")
    }  else{
      x <- seq(h - b, h + b, 0.01)
      ypositiva <- k + sqrt ((a^2 - (a^2/b^2) * ((x - h) ^2)))
      ynegativa <- k - sqrt ((a^2 - (a^2/b^2) * ((x - h) ^2)))
      plot (x, ypositiva , type = "l", xlim = c(h - (b + 1) , h + (b + 1)), ylim = c(k - (a + 1) , k + (a + 1)))
      lines (x, ynegativa , type = "l")
      abline (h = 0, v = 0)
      points (x = c(h, h), y = c(k - c, k + c), col = " red ")
    }
  } else{
    return(print("No cumple con las condiciones para ser una elipse. (a no es mayor que b)"))
  }
}


elipse(2, 3, 9, 4, T)   

#Hiperbola
hiperbola <- function(h, k, a, b, horizontal){
  c <- sqrt(a^2 + b^2) 
  if (horizontal){ 
    xizq <- seq(h - (a + 3), h - a, 0.01) 
    xder <- seq(h + a, h + (a + 3), 0.01) 
    yizqpositiva <- k + sqrt((b^2/a^2)*((xizq - h)^2) - b^2) 
    yderpositiva <- k + sqrt((b^2/a^2)*((xder - h)^2) - b^2)
    yizqnegativa <- k - sqrt((b^2/a^2)*((xizq - h)^2) - b^2) 
    ydernegativa <- k - sqrt((b^2/a^2)*((xder - h)^2) - b^2)
    
    plot(xizq, yizqpositiva, type = "l", xlim = c(h - (a + 4), h + (a + 4)), ylim = c(k - (b + 4), k + (b + 4)),
         xlab = "Eje X", ylab = "Eje Y")
    
    lines(xizq, yizqnegativa, type = "l") 
    lines(xder, ydernegativa, type = "l")
    lines(xder, yderpositiva, type = "l")
    lines(xizq, yizqpositiva, type = "l")
    
    abline(h = 0, v = 0) 
    points(x = c(h - (a + c)), y = c(k), col = "red") 
  } else{ 
    yizq <- seq(k - (a + 3), k - a, 0.01) 
    yder <- seq(k + a, k + (a + 3), 0.01) 
    xizqpositiva <- h + sqrt((b^2/a^2)*((yizq - k)^2) - b^2) 
    xizqnegativa <- h - sqrt((b^2/a^2)*((yizq - k)^2) - b^2) 
    
    plot(xizqpositiva, yizq, type = "l", xlim = c(h - (b + 4), h + (b + 4)), ylim = c(k - (a + 4), k + (a + 4)),
         xlab = "Eje X", ylab = "Eje Y")
    lines(xizqnegativa, yizq, type = "l")
    abline(h = 0, v = 0)
    points(x = c(h), y = c(k - (a + c)), col = "red") # focos
  }
}

hiperbola(1, 1, 9, 4, TRUE)
hiperbola(2, 2, 1, 2, TRUE)
  
  
  
  
    