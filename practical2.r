
################################################################################
##################### 2: The prisoner problem simulation #######################
################################################################################

## Contributors: 
##
##  Yannis Stylianou (s2448736), Kartik (s2407270), Wang Chuhan(s??????) 

#--------------------------    Work Distribution   ----------------------------#

## Yannis: General comments, Pone function
## Kartik: 
## Wang:   

#---------------------------  Problem Description  ----------------------------#
 
## 2n prisoners each have a unique prisoner number from 1 to 2n. The prison 
## contains a room in which there are 2n boxes, each with a unique number from 1 
## to 2n painted on its lid. 2n cards, each printed with a unique number from 1 
## to 2n, are randomly placed one in each box. The prisoners have the task of 
## finding the card with their number on it by opening a maximum on n boxes. 
## After each prisoner’s go, the room is returned exactly to its original state 
## and the prisoner is not allowed to communicate with prisoners yet to have 
## their go. If all prisoners succeed in finding their number, then they all go 
## free.

#--------------------------------   Task   ------------------------------------#

## We will estimate the probabilities of successfully finding the correct card, 
## both for individual prisoners and for the whole group. We will follow and  
## assess 3 strategies:
##
## 1) Each prisoner starts at the box with their number on it, opens it and 
##    reads the number on the card: k, say. If k is not their prisoner number, 
##    they go to box number k, open it and repeat the process until they have 
##    either found the card with their number on it, or opened n boxes without  
##    finding it.
##
## 2) Same as strategy 1, but starting from a randomly selected box.
##
## 3) They open n boxes at random, checking each card for their number.
##

#------------------------  Theoretical expectations  --------------------------#

##
##
##
##
##

n <- 100

pone <- function(n, k, strategy, nreps = 10000){ 
## This function estimates the probability of a single prisoner succeeding in  
## finding their number. The function takes arguments n, k (prisoner’s number)
## strategy and nreps. nreps is the number of replicate simulations to run in 
## order to estimate the probability.
  
  num_success <- 0  ## Initializing number of successful tries. 
                    ## I.e. how many times the prisoner-k 
                    ## managed to find the k-card after opening 
                    ## less or equal to n boxes.
  
  for (i in c(1:nreps)){  ## Iterating over nreps
                                      
    p_numbers <- c(1:(2*n))           ## Creating an array of box indices. 
                                      ## The order of the boxes plays no role in
                                      ## the problem.
    
    box_content <- sample(p_numbers)  ## Shuffling box indices. This way we 
                                      ## randomly arrange the cards in the   
                                      ## boxes. Box k contains box_content[k].

    if (strategy == 1){  ## We will examine strategy 1 first.
      
      path <- c(box_content[k])       ## The path of boxes the prisoner will 
                                      ## follow is fully dependent on the box
                                      ## content. The first box choice is always
                                      ## k (which contains card box_content[k])

      j <- 1                          ## Initializing the counter of attempts.
      
      while (j < (n+1)){  ## While the number of tries is smaller or equal to n:
        
        if (path[j] == k){  ## if the j-th card number in the path is the 
                            ## prisoners number:
                                                                                
          num_success <- num_success + 1  ## We found the correct card on time, 
                                          ## thus increase the number of 
                                          ## successes by 1.
          
          j <- n + 1                      ## This is done in order to break the   
                                          ## while loop if we find the correct 
        }                                 ## card on time.
        
        else{ ## if the j-th card number in the path is not the prisoners number
          
          path[j+1] <- box_content[path[j]]   ## Add the next card in the path.
                                              ## box_content[path[j]] = 
                                              ## box_content[box_content[j-1]]
                                              ## etc.
          
          j <- j + 1                          ## Increase counter to check the 
        }                                     ## check the next box.
      }
    }
    
    else if (strategy == 2){ ## Strategy number 2
      
      first <- sample((p_numbers), 1) ## This time the prisoner chooses the 
      path <- c(box_content[first])   ## first box randomly. The rest is the 
                                      ## same as strategy number 1.
      
      j <- 1                          ## Initializing the counter of attempts
      
      while (j < (n+1)){  ## While the number of tries is smaller or equal to n:
        
        if (path[j] == k){  ## if the j-th card number in the path is the 
                            ## prisoners number:
          
          num_success <- num_success + 1  ## We found the correct card on time, 
                                          ## thus increase the number of 
                                          ## successes by 1.
          
          j <- n + 1                      ## This is done in order to break the   
                                          ## while loop if we find the correct 
        }                                 ## card on time.
        
        else{ ## if the j-th card number in the path is not the prisoners number
          
          path[j+1] <- box_content[path[j]]   ## Add the next card in the path.
                                              ## box_content[path[j]] = 
                                              ## box_content[box_content[j-1]]
                                              ## etc.
          
          j <- j + 1
        }
      }
    }
    else{
      
      n_choices <- sample((p_numbers), n)
      
      if (k %in% n_choices){num_success <- num_success + 1}
      
    }
  }
  prob <- num_success/nreps
  return(prob)
}

prob <- pone(n, 52, strategy = 3)
prob



