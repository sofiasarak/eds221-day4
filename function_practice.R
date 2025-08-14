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


