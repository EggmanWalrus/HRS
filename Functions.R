# Functions

##Generate a vector of variables
crosswave <- function(variable, n=14, m=1){
  variable_vector <- sprintf(variable, seq(m, n))
  return(variable_vector)
}

##Identify columns before and after WidowWave
