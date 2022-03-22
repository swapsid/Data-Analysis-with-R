#Loading the required libraries for assignment
library(purrr)
library(dplyr)
library(ggplot2)

#Creating tibble d1 from mpg that is grouped by class
d1 <- mpg %>% group_by(class)
  
#Creating function my_mpg_lms() which creates an output which is copied to the variable mods1, and this contains a list of 7 elements
#Each element contains the results of the linear model for each class of car.
  
my_mpg_lms <- function(x){
  distinct_class <- group_keys(x)
  class_names <-distinct_class$class 
  class_data <- x %>% group_split() %>% map(function(x){lm(formula = x$cty ~ x$displ)})
  names(class_data) <- class_names
  class(class_data) <- "my_mpg_lms"
  class_data
}

mods1 <- my_mpg_lms(d1)
length(mods1)
class(mods1)
mods1

#Creating summary function that generates required output as mentioned in assignment

summary.my_mpg_lms <- function(dt)
{
  print("The following are the model groups")
  print(names(dt))
  print("Here are the result...")
  
  k <- 1
  
  walk(dt,function(z)
    {
    
    print(paste("Model # ", k, " Group ", names(dt[k]), " Obs = ", nrow(z$model) ))
    print(summary(z))
    k <<- k + 1
    print("=======================================================")
     })
  
}
summary(mods1)