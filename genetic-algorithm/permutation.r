
library(GA)

# Wczytanie macierzy odleglosci z pliku
cities = read.table('genetic-algorithm\\distances.txt')
distance_matrix <- as.matrix(cities)
    
#funkcja oceniajaca
f.fitness <- function(city_list) {
  city_list <- c(city_list, city_list[1])
  section <- embed(city_list, 2)               
  return (1/sum(distance_matrix[section]))    
                                              
}

#algorytm ewolucyjny
GA <- ga(type = "permutation", fitness = f.fitness,
         lower = 1, upper = 5, popSize = 10, maxiter = 10)

summary(GA)
plot(GA)

decode=function(city_list)
{
  city_list <- c(city_list, city_list[1])
  section <- embed(city_list, 2)
  distance <- sum(distance_matrix[section])
  cat( paste("Cities amount: ", 5,"\n") )
  cat( paste("Shortest path: ", distance, "\n") )
  cat(paste("Cities on path: ","\n"))
  label<-colnames(distance_matrix)
  for(index in city_list) {
    cat(paste("->", label[index]),"\n")
  }
}

decode(GA@solution[1,])

