#Assignment 8 - Function Factories and Closures
library(tidyverse)
library(dplyr)
#Simple timer
simple_timer <- function(){
    name <- ""
    start_time <- 0
    end_time <- 0
    time_difference <-0
    data_tibble <-tibble("Name" = character(), "StartTime" = character(), "FinishTime" = character(), "Duration" = character())
    start_environment <- ""
    finish_environment <- ""
    get_time_environment <- ""
    get_all_times_environment <- ""
    list(
      start = function(names){
        start_environment <<- parent.env(environment())
        name <<- names
        start_time <<- Sys.time()
      },
      finish = function(){
        finish_environment <<- parent.env(environment())
        end_time <- Sys.time()
        time_difference <<- end_time-start_time
        name <- name
        archive <- function(){
            data_tibble <<- data_tibble %>% add_row("Name" = as.character(name), "StartTime" =as.character(start_time), "FinishTime" = as.character(end_time), "Duration" = as.character(time_difference))
            data_tibble
        }
        data_tibble <<- archive()
      },
      get_time = function(){
        get_time_environment <<-parent.env(environment())
        time_difference
      },
      get_all_times = function(){
        get_all_times_environment <<- parent.env(environment())
        data_tibble
      }
    )
}

a <- simple_timer()
print(str(a))
# Check  to  see  that  an  empty  tibble  has  been  created  in  the  closure.
print(environment(a$start)$data_tibble)

a$start("Luke Skywalker")
Sys.sleep(7)
a$finish()
print(a$get_time())
print(a$get_all_times())

a$start("Anakin Skywalker")
Sys.sleep(8)
a$finish()
print(a$get_time())
print(a$get_all_times())

#Environments of all the functions in the list
print(environment(a$start)$start_environment)
print(environment(a$start)$finish_environment)
print(environment(a$start)$get_time_environment)
print(environment(a$start)$get_all_times_environment) 