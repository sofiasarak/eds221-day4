# function that adds up the number of birds and dogs

#defined function
birddog_sum <- function(bird, dog) { #bird and dog are placeholder variable, can be anything!
  pets <- bird + dog
  return(pets)
} #running stores the function in the environment!

#use it!
total_pets <- birddog_sum(bird=2, dog=5) #returns 7! storing it by assigning it to total_pets
#don't need to specify our arguments 
total_pets <- birddog_sum(2, 3) #but have to be in the right order

#create a function to double values

x <- 1 #helpful to make fake data sometimes to make sure your function works!
print(2*x) #will print 2 times any value I pass

#when assigning function name, don't need parentheses
double_it <- function(x){
  print(2*x)
}

double_it(5) #works, yay!

#write a function with conditionals
# example is converting animals' ages

animal_age <- function(animal, age){
  if (animal == "dog"){ #animal and age are just arbitrary variables that will be arguments within our function
    print(age*7)
  } else if (animal == "goat") {
    print(age*4.7)
  }
}

#try using for an 8 year old dog, etc
animal_age(animal = "dog", age = 8) #returns 56!
animal_age("goat", 3)
animal_age("cow", age=8) #nothing happened; nothing inside function tells R what to do with cow

#write an updated version of the animal age function with error messages

animal_age_stop <- function(animal, age){
  if (!animal %in% c("dog", "goat")){ #if animal is not "in" the vector we made; if the animal name inputted is NOT
    #in the vector c(dog, goat), the error message will run; ! has to go first with %in% operator
    stop("Oops! Animal must be a dog or goat.") #have to think about how someone might misuse/break this function
  }
  if (is.numeric(age)==FALSE){ #if age is not numeric; better error message than what R would've provided
    stop("The age must be a number.")
  }
  if(age <= 0 | age > 50){
    warning("Are you sure about your animal's age?")
  }
  if (animal == "dog"){ #animal and age are just arbitrary variables that will be arguments within our function
    print(age*7)
  } else if (animal == "goat") {
    print(age*4.7)
  }
}

animal_age_stop("dog", 100) #returned the warning message!
animal_age_stop("elephant", 10) #didn't actually figure out how old the elephant is -- error message
animal_age_stop("dog", "two") #returned error

#Functions meet for loops!

# all the dataframes in the function are called df --> argument df

df_means <- function(df) {
  for(i in 1:ncol(df)) { #for loop that steps through each column, pulls out the name, and prints this statement
    if (is.numeric(df[[i]])==TRUE){
  column_name <- colnames(df[i])
  col_mean <- mean(df[[i]], na.rm=TRUE)
  print(paste("The mean value of", column_name, "is", round(col_mean,2)))
    }
  }
  }

#trying the function!
df_means(df=mtcars)

df_means(palmerpenguins::penguins)


#logistic growth example

#logistic growth equation
logistic_growth <- function(N0, K, r, time) {
  Nt <- K / (1 + ((K-N0)/N0) * exp(-r * time))
  print(Nt)
}

# check for one set of values (whether or not that basic function worked)
logistic_growth(N0 =100, K=6000, r=0.27, time=40)


# working on example just dealing with time
time_vec <- seq(0, 35, by=0.1)

#apply the logistic growth function to that vector
pop_35 <-  logistic_growth(N0=100, K=6000, r=0.27, time=time_vec)

#turn into a data frame; combining time setps and population size
pop_time_35 <- data.frame(time_vec, pop_35)

#plot it!
library(ggplot2)
ggplot(data=pop_time_35, aes(x=time_vec, y=pop_35)) + geom_line(linewidth=0.5) #looks like logistic growth function!

#alternatively, with an internal for loop
pop_35_vector <- vector(mode="numeric", length=length(time_vec))

for(i in seq_along(time_vec)) {
population <-  logistic_growth(N0=100, K=6000, r=0.27, time=time_vec[i])
pop_35_vector[i] <- population
}

# now building to estimating across growth rates
#creating series of growth rates 
r_seq <- seq(from = 0.2, to=0.4, by=0.01)

#creating a MATRIX to store output values
out_matrix <- matrix(nrow = length(time_vec), ncol = length(r_seq))

for(i in seq_along(time_vec)) {
  for (j in seq_along(r_seq)){ #doesn't seem to make a difference output-wise which order you index in 
  population <-  logistic_growth(N0=100, K=6000, 
                                 r=r_seq[j], time=time_vec[i])
  out_matrix[i,j] <- population #need two dimensions for where we're going to store because it's a matrix
  }
}

#data wrangling to get a plot
#adding time as a variable
out_df <- data.frame(out_matrix, time=time_vec) #make it a data frame and add time

#updating column names for growth rates
colnames(out_df) <- c(paste0("gr_", r_seq),"time") #using paste to make it something like "gr_0.21"!

#pivot longer to make it tidy; we like longer data instead of wider data!
library(tidyverse)
out_df_long <- out_df |> 
  pivot_longer(cols = -time, 
               names_to = "growth_rate", 
               values_to = "population")

#time to plot!
ggplot(data=out_df_long, aes(x=time, y=population)) + geom_line(aes(color = growth_rate)) + theme_minimal()
