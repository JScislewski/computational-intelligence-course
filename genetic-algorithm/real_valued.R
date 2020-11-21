library(GA)

#funkcja oceniajaca 
fp <- function(x1, x2)
{
  #funkcja Rastrigina
  20 + x1^2 + x2^2 - 10*(cos(2*pi*x1) + cos(2*pi*x2))
}

#zmienne x1 i x2
x1 <- x2 <- seq(-5.12, 5.12, by = 0.1)
z <- outer(x1, x2, fp)
GA <- ga(type = "real-valued",
         fitness =  function(x) -fp(x[1], x[2]),
         lower = c(-5.12, -5.12), upper = c(5.12, 5.12),
         popSize = 50, maxiter = 1000, run = 100)
summary(GA)
plot(GA)



