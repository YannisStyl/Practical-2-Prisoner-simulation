
################################################################################
##################### 2: The prisoner problem simulation #######################
################################################################################

## Contributors: 
##
##  Yannis Stylianou (s2448736), Kartik (s2407270), Wang Chuhan(s2324597) 

## GitHub Repo: 
##
##  https://github.com/YannisStyl/Practical-2-Prisoner-simulation.git


#--------------------------    Work Distribution   ----------------------------#

## Yannis: General comment format, Pone function, dloop function (40.02 %)
## Kartik: dloop function, graph (29.88 %)
## Wang: Pall function (30.10 %)

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
## both for individual prisoners (Pone) and for the whole group (Pall).   
## We will follow and assess 3 strategies:
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
## Additionally, we will estimate and plot the probabilities of each loop  
## length, from 1 to 2n, occurring at least once in a random shuffling of cards  
## to boxes. (A loop occurs when some card in the sequence of opened boxes 
## has the number of the first box opened).


#------------------------  Theoretical expectations  --------------------------#

## First we will examine the individual probabilities. 
##
##  For strategy 1, we can view the boxes as nodes of a graph, connected with  
##  vertexes iff the card number of the first box is the number of the second 
##  box.
##  e.g. 4[6] -> 6[1] -> 1[...], 4,6,1 are nodes, (4,6), (6,1) are vertexes.
##
##  This graph is always partitioned into disjoint cycles. 
##
##  Cycles because when a prisoner opens a box, the only possible outcomes are 
##  either 1) open a new box or 2) return to the initial box, since the cards 
##  that lead to the other boxes in the cycle have already been found. Disjoint 
##  because two different cards can't lead to the same box, nor can one card 
##  lead to more (or less) than 1 boxes. 
##
##  This way, the prisoner finds his card after opening less or equal to n 
##  boxes, iff the cycle that contains his initial box choice has length smaller 
##  than n.
##
##  The possible cycle outcomes for prisoner k are {k}U{A} where A is an element 
##  of power_set({1,...,k-1,k+1,..2n}). Thus the sample space size is 2^(2n-1),
##  and exactly half of these samples result in the prisoner finding his card
##  (symmetry of the power set). Thus the probability of success should be 1/2.
##
##  For strategy number 2 the format is the same as strategy number 1, but this
##  time, it is possible for a prisoner to choose a cycle that doesn't contain
##  his number. Thus, we expect the probability to be lower compared to
##  strategy number 1. 
##
##  With strategy number 3, using simple combination theory, we can derive that
##  the probability of success is 1/2.
##
## If we consider 2n random variables as the result of each prisoners attempt
## (success or failure), then with strategies number 2 and 3 these random 
## variables would be i.i.d. This means that the probability of everyone
## finding his card would be the product of the individual probabilities. As n
## increases, this probability decreases exponentially. 
##
## With strategy 1 however, if prisoner k succeeds, every prisoner that begins
## with a box of the same cycle as prisoner k will succeed. This means that the
## variables are not independent. The result of the arithmetical approximation
## of the probability of success in this case, is very impressive!


#----------------------------------  Code  ------------------------------------#

Pone <- function(n, k, strategy, nreps = 10000){ 
  
## This function estimates and returns the probability of a single prisoner   
## succeeding in finding their number. The function takes arguments n, k 
## (prisoner’s number), strategy and nreps. nreps is the number of replicate  
## simulations to run in order to estimate the probability.
  
  num_success <- 0  ## Initializing number of successful tries. 
                    ## I.e. how many times the prisoner-k 
                    ## managed to find the k-card after opening 
                    ## less or equal to n boxes.
  
  for (i in c(1:nreps)){  ## Iterating over nreps
                                      
    p_numbers <- c(1:(2*n))           ## Creating an array of box indices. 

    box_content <- sample(p_numbers)  ## Shuffling box indices. This way we 
                                      ## randomly arrange the cards in the   
                                      ## boxes. Box k contains box_content[k].
    
    if (strategy == 1 || strategy == 2){ 
      
      if (strategy == 1){  ## We will examine strategy 1 first.
      
        path <- c(box_content[k])     ## The path of boxes the prisoner will 
                                      ## follow is fully dependent on the box
                                      ## content. The first box choice is always
                                      ## k (which contains card box_content[k]).
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
          
          j <- j + 1                          ## Increase the counter of boxes.
        }
      }  
    }
    else{ ## Strategy number 3
      
      n_choices <- sample((p_numbers), n)     ## Randomly choose n boxes.
      
      if (k %in% n_choices){                  ## Identify whether k is selected.
        
        num_success <- num_success + 1        ## If so, increase the number of 
                                              ## successes.
      }
    }
  }
  prob <- num_success/nreps                   ## Probability of success of 
                                              ## prisoner for given strategy
  return(prob)
}

#Computing individual probabilities
prob_5_1_pone <- pone(5, 5, 1)

prob_5_2_pone <- pone(5, 5, 2)

prob_5_3_pone <- pone(5, 5, 3)

prob_50_1_pone <- pone(50, 25, 1)

prob_50_2_pone <- pone(50, 25, 2)

prob_50_3_pone <- pone(50, 25, 3)

Pall = function(n, strategy, nreps = 10000){
  
## This function estimates the probability of all prisoners finding their number
## under different strategies. The function takes arguments n, strategy and 
## nreps (same as Pone). The function returns the probability estimate.
  
  num_success = 0              
  
  for (i in c(1:nreps)){                   ## iterating over nreps.
    
    count = 0                              ## Counter for the prisoners who 
                                           ## successfully found his number.
    
    p_numbers = c(1:(2 * n))               ## Creating an array of box indices.
                                   
    box_content = sample(p_numbers)        ## randomly arrange the cards in the 
                                           ## boxes.
    
    for (num in c(1:(2 * n))){             ## Iterating over all prisoners.
      
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
                                              ## attempts.
        }
        while(j < (n+1)){                     ## While the number of tries is 
                                              ## smaller or equal to n:
          
          if (path[j] == num){                ## if the prisoners' number is in 
                                              ## the j-th box:
            
            count = count + 1                 ## Add one to the number of people
                                              ## who successfully found the card
            break                             ## and leave the while loop.
          }
          
          else{
            
            path[j+1] = box_content[path[j]]  ## Set the number in this box 
                                              ## to the next path index.
            
            j = j + 1                         ## Counter plus 1. 
          }                                   
        }
      }
      else{ ## Strategy number 3
        
        n_choices = sample((p_numbers),n)     ## Randomly choose n boxes.
        
        ## Identify whether num is selected.
        if (num %in% n_choices){count = count + 1}
      }
    }
    
    ## Identify whether all the prisoners find their numbers.
    if(count == 2*n){num_success = num_success + 1}
  }
  
  prob = num_success / nreps                 ## The approximation of the  
                                             ## probability is the number of 
                                             ## successful attempts, divided by
                                             ## the total number of attempts.
  return(prob)
}

#Computing joint probabilities
prob_5_1_pall = Pall(5, strategy = 1)        ## The probability of 10 prisoners 
                                             ## escaping, using strategy 1.

prob_5_2_pall = Pall(5, strategy = 2)        ## The probability of 10 prisoners 
                                             ## escaping, using strategy 2.

prob_5_3_pall = Pall(5, strategy = 3)        ## The probability of 10 prisoners
                                             ## escaping, using strategy 3.

prob_50_1_pall = Pall(50, strategy = 1)      ## The probability of 100 prisoners
                                             ## escaping, using strategy 1.

prob_50_2_pall = Pall(50, strategy = 2)      ## The probability of 100 prisoners
                                             ## escaping, using strategy 2.

prob_50_3_pall = Pall(50, strategy = 3)      ## The probability of 100 prisoners
                                             ## escaping, using strategy 3.

dloop <- function(n, nreps = 10000){
  
## A loop occurs when some card in the sequence of opened boxes 
## (under strategy 1 or 2) has the number of the first box opened. 
## This function estimates the probability of each loop length, from 1 to 2n, 
## occurring at least once in a random shuffling of cards to boxes. The 
## functions takes as arguments n (the number of boxes/2) and nreps 
## (the number of simulations to run in order to estimate the probability).
## Returns a 2n-vector of cycle length probabilities.
      
  freq <- rep(0,length.out = 2*n)             ## Initializing the length 
                                              ## frequencies.
  
  for(i in 1:nreps){ ## Iterating over nreps to estimate the probabilities
    
    cycle_lengths <- rep(0, length.out = 2*n)     ## Initializing a list of 
                                                  ## zeros to represent the 
                                                  ## cycle lengths. 1 at index 
                                                  ## k indicates existence of 
                                                  ## cycle of length k.
    
    permutation <- sample(c(1:(2*n)))             ## Shuffling the cards inside
                                                  ## the boxes.
    
    path <- c(permutation[1])                     ## Initializing the cycle 
                                                  ## search from the first box.
    
    
    while (sum(permutation) > 0){ ## This will make more sense later. Basically
                                  ## if all the values of permutation are zero
                                  ## we have examined all boxes and thus cycles.
      
      i <- 2                      ## Initializing a counter for the cycle
      
      while (path[1] != permutation[path[i-1]]){  ## While the last card doesn't
                                                  ## lead to the first box:
        
        path[i] <- permutation[path[i-1]]         ## Add a new box to the cycle
        
        i <- i + 1                                ## and continue cycling.
        
        }
      
      cycle_lengths[length(path)] <- 1           ## After the cycle length is  
                                                 ## found, say k, then 
                                                 ## cycle_lengths[k] = 1.
                                                 ## After we examine all the 
                                                 ## boxes, this will be a length
                                                 ## indicator list for the
                                                 ## specific permutation.
      
      permutation[path] <- rep(0, length.out = length(path)) ## Every element of
                                                             ## the permutation 
                                                             ## that was in the
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
                                                             ## cycle, until
                                                             ## sum(permutation)
                                                             ## is zero.
    }                                                        
    
    freq <- freq + cycle_lengths  ## Add the nreps indicator lists element-wise,
                                  ## to create a list of length frequencies.
  }
 
 probabilities <- freq/nreps      ## Once we have the frequencies, we divide by
                                  ## the number of repetitions to estimate the
                                  ## probabilities. 
 
 return(probabilities) 
}


n <- 50

## Using dloop to estimate the probabilities for n = 50
Loops <- dloop(n)      

## The probability of having loops of length n and bellow is:
## 1 - Prob(lengths > n) = 1 - sum(Prob(length = i)) for i = n+1,..,2n
Prob_small_loop <- 1 - sum(Loops[(n+1):length(Loops)])

## Creating the x axis as the no. of possible cycle lengths
xx <- c(1:(2*n))

## Plotting the probabilities of cycle lengths
plot(xx, Prob, pch=19, cex=.5, xlab = "Cycle length", ylab = expression(P(X)))   

cat('Individual probabilities for each strategy (n=5):')
cat('Strategy 1:',prob_5_1_pone )
cat('Strategy 2:',prob_5_2_pone )
cat('Strategy 3:',prob_5_3_pone )
cat('')                                                              
cat('Individual probabilities for each strategy (n=50):')
cat('Strategy 1:',prob_50_1_pone )
cat('Strategy 2:',prob_50_2_pone )
cat('Strategy 3:',prob_50_3_pone )
cat('')
cat('Group probabilities for each strategy (n=5):')
cat('Strategy 1:',prob_5_1_pall )
cat('Strategy 2:',prob_5_2_pall )
cat('Strategy 3:',prob_5_3_pall )
cat('')
cat('Group probabilities for each strategy (n=50):')
cat('Strategy 1:',prob_50_1_pall )
cat('Strategy 2:',prob_50_2_pall )
cat('Strategy 3:',prob_50_3_pall )
cat('')

## It is clear that even for the case of n=5, with strategies 2 and 3 the
## prisoners are doomed to stay imprisoned. Strategy 1 however, even for the 
## n=50 case, gives them quite surprisingly high odds. 

cat('The probability that there is no loop longer than 50 in a random 
reshuffling of cards to boxes is:')
cat(Prob_small_loop)

## The probability of all loops being smaller than 50, is similar to the value 
## of Pall for n = 50. This is no coincidence, since the statements:
## "All prisoners found their card" and "There was no loop bigger than 50" are
## equivalent. This result confirms that the probability of all prisoners
## finding their card, under strategy 1, is indeed high. 
