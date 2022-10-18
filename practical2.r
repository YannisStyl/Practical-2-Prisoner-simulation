
################################################################################
##################### 2: The prisoner problem simulation #######################
################################################################################

## Contributors: 
##
##  Yannis Stylianou (s2448736), Kartik (s2407270), Wang Chuhan(s2324597) 

#--------------------------    Work Distribution   ----------------------------#

## Yannis: General comments, Pone function
## Kartik: Dloop function
## Wang: Pall function

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

Pone <- function(n, k, strategy, nreps = 10000){ 
  
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
    
    if (strategy == 1 || strategy == 2){ 
      
      if (strategy == 1){  ## We will examine strategy 1 first.
      
        path <- c(box_content[k])       ## The path of boxes the prisoner will 
                                      ## follow is fully dependent on the box
                                      ## content. The first box choice is always
                                      ## k (which contains card box_content[k])
    }
    
    else if (strategy == 2){ ## Strategy number 2
      
      first <- sample((p_numbers), 1)    ## This time the prisoner chooses the 
      path <- c(box_content[first])      ## first box randomly. The rest is the 
                                         ## same as strategy number 1.
    } 
      j <- 1                             ## Initializing the counter of attempts
      
      while (j < (n+1)){  ## While the number of tries is smaller or equal to n:
        
        if (path[j] == k){  ## if the j-th card number in the path is the 
                            ## prisoners number:
          
          num_success <- num_success + 1  ## We found the correct card on time, 
                                          ## thus increase the number of 
                                          ## successes by 1.
          
          break                           ## This is done in order to break the   
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
    else{                                     ## Strategy number 3
      
      n_choices <- sample((p_numbers), n)     ## Randomly choose n boxes
      
      if (k %in% n_choices){num_success <- num_success + 1}
                                              ## Identify whether k is selected
    }
  }
  prob <- num_success/nreps                   ## Probability of escape of 
                                              ## prisoners
  return(prob)
}

prob <- Pone(n, 52, strategy = 3)
prob


Pall = function(n, strategy, nreps = 10000){
## This function estimate the probability of all prisoners finding their number,
## so that all are released.The function takes arguments n, strategy and nreps.
## The function returns the probability estimate.
  num_success = 0              
  
  for (i in c(1:nreps)){                   ## iterating over nreps
    
    count = 0                              ## Count the prisoner who 
                                           ## successfully found his number.
    p_numbers = c(1:(2 * n))               ## Creating an array of box indices.
                                   
    box_content = sample(p_numbers)        ## randomly arrange the cards in the 
                                           ## boxes.
    for (num in c(1:(2 * n))){             ## Iterating all prisoners
      
      if (strategy == 1 || strategy == 2){ 
        
        if(strategy == 1){                 ## We will examine strategy 1 first.
  
        path = c(box_content[num])         ## The path of boxes the prisoner 
                                           ## will follow is fully dependent
                                           ## on the box content. The first box 
                                           ## choice is always No. num
        
        j = 1                              ## Initializing the counter of 
                                           ## attempts.
      }  
        else { ## strategy 2
        
          first = sample((p_numbers), 1)      ## This time the prisoner chooses 
                                              ## the first box randomly.
          path = c(box_content[first])
        
          j = 1                               ## Initializing the counter of 
                                              ## attempts
        }
        while(j < (n+1)){                     ## While the number of tries is 
                                              ## smaller or equal to n:
          
          if (path[j] == num){                ## if the prisoners' number is in 
                                              ## the j-th path          
            count = count + 1                 ## Add one to the number of people
                                              ## who successfully found the card
            break                             ## and leave the while loop
          }
          
          else{
            
            path[j+1] = box_content[path[j]]  ## Set the number in this box 
                                              ## to the next path index
            
            j = j + 1                         ## Counter plus 1 
          }                                   
        }
      }
      else{                                   ## Strategy number 3
        
        n_choices = sample((p_numbers),n)     ## Randomly choose n boxes
        
        ## Identify whether num is selected
        if (num %in% n_choices){count = count + 1}
      }
    }
    ## Identify whether all the prisoners find their numbers
    if(count == 2*n){num_success = num_success + 1}
  }
  prob = num_success / nreps                 ## Probability of escape of 
                                             ## prisoners
  return(prob)
}

prob_5_1_pall = Pall(5, strategy = 1)        ## The probability of 10 prisoners 
                                             ## escaping, using strategy 1

prob_5_2_pall = Pall(5, strategy = 2)        ## The probability of 10 prisoners 
                                             ## escaping, using strategy 2

prob_5_3_pall = Pall(5, strategy = 3)        ## The probability of 10 prisoners
                                             ## 5escaping, using strategy 3

prob_50_1_pall = Pall(50, strategy = 1)      ## The probability of 100 prisoners
                                             ## escaping, using strategy 1

prob_50_2_pall = Pall(50, strategy = 2)      ## The probability of 100 prisoners
                                             ## escaping, using strategy 2

prob_50_3_pall = Pall(50, strategy = 3)      ## The probability of 100 prisoners
                                             ## escaping, using strategy 3

dloop <- function(n, nreps = 10000){
## A loop occurs when some card in the sequence of opened boxes 
## (under strategy 1 or 2) has the number of the first box opened. 
## This function estimates the probability of each loop length, from 1 to 2n, 
## occurring at least once in a random shuffling of cards to boxes. The 
## functions takes as arguments n (the number of boxes/2) and nreps 
## (the number of simulations to run in order to estimate the probability).
## Returns a 2n-vector of probabilities.

      
  freq <- rep(0,length.out = 2*n)
  
  for(i in 1:nreps){
    
    cycle_lengths <- rep(0, length.out = 2*n)
    
    permutation <- sample(c(1:(2*n)))
    
    path <- c(permutation[1])
    
    while (sum(permutation) > 0){
      
      i <- 2
      
      while (path[1] != permutation[path[i-1]]){
        
        path[i] <- permutation[path[i-1]]
        
        i <- i + 1
        
        }
      
      cycle_lengths[length(path)] <- 1
      
      permutation[path] <- rep(0, length.out = length(path)) ## Every element of
                                                             ## the permutation 
                                                             ## that is in the
                                                             ## cycle is 
                                                             ## replaced by 0.
      
      first_non_zero = which(permutation != 0)[1]            ## Find the first
                                                             ## non-zero element
                                                             ## of the 
                                                             ## permutation, to 
                                                             ## begin the new
                                                             ## cycle.
      
      path <- c(permutation[first_non_zero])                 ## Restart the path
                                                             ## and find new
    }                                                        ## cycle. 
    
    freq <- freq + cycle_lengths
    
  }
 
 probabilities <- freq/nreps 
 
 return(probabilities) 
}


n=50                                                         ## Using dloop to 
                                                             ## estimate the 
Prob<-dloop(n)                                               ## probabilities 
                                                             ## for n = 50

xx<-c(1:(2*n))                                               ## Creating for x 
                                                             ## axis for the
                                                             ## no. of Prisoners

plot(xx,Prob,pch=19,cex=.5,xlab="Prisoner no.",ylab=expression(P(X)))   
                                                             ## Plotting a graph 
                                                             ## to show our 
                                                             ##Probability Dist.  

     
