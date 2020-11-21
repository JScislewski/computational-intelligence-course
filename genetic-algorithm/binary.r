library(GA)

#Zbiór losowo generowanych pracowników
# id - id pracownika
# performance - ocena wydajnosci pracownika od 1 do 100
# expected_salary - oczekiwane miesieczne wynagrodzenie danego pracownika
employeesDb = data.frame(
	id = c(1:100),
	performance = c(sample(1:100, 100)),
	expected_salary = c(sample(2700:20000, 100))
)

# miesieczny budzet firmy na wynagrodzenia dla pracownikow
company_budget = 50000

fitnessFunc = function(chr) {
	performance_chr = chr %*% employeesDb$performance
	salary_chr = chr %*% employeesDb$expected_salary
	if (salary_chr > company_budget) return(-performance_chr) 
	else return(performance_chr)
}

results=ga(type="binary",nBits=100,fitness=fitnessFunc,popSize=100,pmutation=0.05,elitism=5,maxiter=1000,seed=10)

   
summary(results)
plot(results)

decode=function(chr){
	print("Results: ")
	print( employeesDb[chr == 1, ] )
	print( paste("Company performance: =",chr %*% employeesDb$performance) )
	print( paste("Monthly salaries: =",chr %*% employeesDb$expected_salary) )
}
decode(results@solution[1,])
