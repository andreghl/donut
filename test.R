donut <- function(){
  
  A <- 1
  B <- 1
  
  b <- rep(NA, 1760)
  z <- rep(NA, 1760)
  
  cat("\33[2J")
  cat("\033[?25l")
  
  while(TRUE){
    
    A <- A + 0.07
    B <- B + 0.03
    cA <- cos(A)
    sA <- sin(A)
    cB <- cos(B)
    sB <- sin(B)
    
    for(k in 1:length(b)){
      
      if(k %% 80 == 79){
        b[k] <- "\n"
        z[k] <- 0
      } else {
        b[k] <- " "
        z[k] <- 0
      }
    }
    
    for(j in seq(from = 0, to = 6.28, by = 0.07)){
      
      ct <- cos(j)
      st <- sin(j)
      
      for(i in seq(from = 0, to = 6.28, by = 0.02)){
        
        sp <- sin(i)
        cp <- cos(i)
        h <- ct + 2
        D <- 1 / (sp * h * sA + st * cA + 5)
        t <- sp * h * cA - st * sA
        
        x <- trunc(40+30*D*(cp*h*cB-t*sB))
        y <- trunc(12+15*D*(cp*h*sB+t*cB))
        o <- x + 80 * y
        N <- trunc(8*((st*sA-sp*ct*cA)*cB-sp*ct*sA-st*cA-cp*ct*sB))
        
        if(y < 22 & y >= 0 & x >= 0 & x < 79 & D > z[o]){
          
          z[o] <- D
          
          if(N > 1){
            b[o] <- "*" 
          } else {
            b[o] <- "." 
          }
        }
        
        
      }
    }
    cat("\033[1;1H", paste(b, collapse = ""))
  }
  
}

donut()


