
## Guess The Random Number Function
# author: Darren Kagey

library(tidyverse) # load tidyeverse package for necessary functions

GuessTheNumber <- function(lower=0, upper=10,seed=NULL){ 
  # default bounds are 0:10, default  seed is NULL
  if(!is.numeric(lower) | lower-as.integer(lower)!=0 | !is.numeric(upper) | upper-as.integer(upper) !=0){
    stop("upper and lower must be integers") # stops function if user set bounds
    # are not integers
  }else if(lower >= upper){ 
    stop("upper must be greater than lower") 
    #stops function if lower bound value is greater than higher bound value
  }
  set.seed(seed) # sets random number generated seed to equal the 
  # user specified seed value
  randomNum <- sample(c(lower:upper), size=1) # generates a random number
  # within the user specified bounds
  guess <- FALSE # creates an object representing whether
  # or not user guess equals the random number
  NumGuesses <- 0 #creates an object for the initial number of guesses
  # by the user, 0
  Guesses <- c() # creates blank vector to be used later to report 
  # the value of each guess by the user
  while(guess==FALSE){ # creates a loop that stops when the user's
    # guess is TRUE (equals the random number)
    value <- readline("Enter a number: ") #prompts user to guess a number
    value = as.numeric(value) # turns user guess into numeric data type
    NumGuesses <- NumGuesses+1 #increases reported number of guesses
    # by one for each guess by the user until correct
    Guesses <- c(Guesses, value) # vector that holds a vector that has
    # values of guesses the user took before and including 
    # the correct number
    if(value == randomNum){
      cat("Correct!\n")
      guess=TRUE #if user guess equals random number, guess= TRUE, so
      # stop loop and print 'correct'
    }else if(value < randomNum){ # if user guess is less than random
      # number, print 'too low' and continue loop
      cat("Too Low")
    }else if(value > randomNum){ # if user guess is greater than
      # random number, print 'too high' and continue loop
      cat("Too High")
    }
  }
  list(RandomNumber=randomNum, NumGuesses=NumGuesses, Guesses=Guesses)
  #list that generates when user correctly guesses random number
  # that reports 1) the actual random number generated, 2) the number
  # of guesses it took the user to correctly guess the random number,
  # and 3) each individual guess made by the user
}

GuessTheNumber()

