# *------------------------------------------------------------------
# Unsupervised Machine Learning - Homework 4
# *----------------------------------------------------------------

# Clear cache
rm(list = ls())

#library('dummies')
#library('ramify')
#library('readxl')

####################################### Read and prepare data ###################################################

# even though name is day1, I am using data of day 7
day1 = read.table("/Users/gabrielaszini/Documents/Unsupervised ML/Assignment 4/ydata-fp-td-clicks-v1_0.20090507", 
                   header = FALSE, fill = TRUE,col.names = paste0("V",seq_len(157))) 
unique(day1[,2]) #we see that for some observations we have number of articles that do not make much sense
# we delete variables with such values
subset_day1 = day1[which(day1[,2] != "2:0.295484" & day1[,2] != "2:0.348813" & day1[,2] != "2:0.277121" & day1[,2] != "2:0.310657" & day1[,2] != "2:0.332416" & day1[,2] != "2:0.211406"), ]
# taking only important rows
subset_day1 = subset_day1[,1:3]
# change variables from factors to numeric
subset_day1_numeric = subset_day1
subset_day1_numeric[,1] = as.numeric(as.character(subset_day1_numeric[,1]))
subset_day1_numeric[,2] = as.numeric(as.character(subset_day1_numeric[,2]))
subset_day1_numeric[,3] = as.numeric(as.character(subset_day1_numeric[,3]))
unique(subset_day1_numeric[,2]) 
# reordering rows
rownames(subset_day1_numeric) = seq(length=nrow(subset_day1_numeric))

####################################### epsilon-greedy ###################################################
# Note : if we choose epsilon = 0, then we have greedy

### Function that returns matrix to be updated with articles, number of times pulled and average of successes
# Initial value can be optimistic or not
articles_updates_egreedy = function(subset_day1_numeric, initial_values){
articles = unique(subset_day1_numeric[,2]) 
N = rep(0,length=length(articles))
Q = rep(initial_values,length=length(articles))
articles_updates_egreedy = data.frame(articles, N, Q)
return(articles_updates_egreedy)}

### Function that returns the matrix of iterations to be updated, with the arm pulled, the reward and whether we do exploration or exploitation
outcome_egreedy = function(iterations, epsilon){
exploration = rbinom(n=iterations, size=1, prob=epsilon)
iterations_matrix = rep(1:iterations)
arm = rep(NA, iterations)
reward = rep(NA, iterations)  
outcome_egreedy = data.frame(iterations_matrix, exploration, arm, reward)
return(outcome_egreedy)
}

### Epsilon-greedy algorithm 
epsilon_greedy = function(subset_day1_numeric, initial_values, iterations, epsilon){
#initialize matrices 
  articles_updates_egreedy = articles_updates_egreedy(subset_day1_numeric, initial_values)
  outcome_egreedy = outcome_egreedy(iterations, epsilon)
#change dataset
  subset_day1_numeric_egreedy = subset_day1_numeric
for(i in 1:iterations){
  # in 1-epsilon times look for the one with the highest reward - if several, pick it randomly
  if(outcome_egreedy$exploration[i] == 0){
    row_max_Q = which(articles_updates_egreedy$Q == max(articles_updates_egreedy$Q))
    outcome_egreedy$arm[i] = articles_updates_egreedy$articles[sample(row_max_Q,1)]
    # now we find the first observation in the dataset with this article being proposed
    outcome_egreedy$reward[i] = subset_day1_numeric_egreedy$V3[which(subset_day1_numeric_egreedy$V2 == outcome_egreedy$arm[i])[1]]
    # update N and Q of this arm in articles_updates
    articles_updates_egreedy$N[which(articles_updates_egreedy$articles == outcome_egreedy$arm[i])] = articles_updates_egreedy$N[which(articles_updates_egreedy$articles == outcome_egreedy$arm[i])] + 1
    articles_updates_egreedy$Q[which(articles_updates_egreedy$articles == outcome_egreedy$arm[i])] = (articles_updates_egreedy$Q[which(articles_updates_egreedy$articles == outcome_egreedy$arm[i])]*(articles_updates_egreedy$N[which(articles_updates_egreedy$articles == outcome_egreedy$arm[i])]-1) + outcome_egreedy$reward[i])/articles_updates_egreedy$N[which(articles_updates_egreedy$articles == outcome_egreedy$arm[i])] 
    #delete this row in dataset
    subset_day1_numeric_egreedy <- subset_day1_numeric_egreedy[-c(which(subset_day1_numeric_egreedy$V2 == outcome_egreedy$arm[i])[1]), ]
    #rearrange indices
    rownames(subset_day1_numeric_egreedy) = seq(length=nrow(subset_day1_numeric_egreedy))
  }
  # in epsilon times take randomly one of the arms and update this arm and the average N
  if(outcome_egreedy$exploration[i] == 1){
    #taking randomly one of arms
    outcome_egreedy$arm[i] = articles_updates_egreedy$articles[sample(dim(articles_updates_egreedy)[1],1)]
    # now we find the first observation in the dataset with this article being proposed
    outcome_egreedy$reward[i] = subset_day1_numeric_egreedy$V3[which(subset_day1_numeric_egreedy$V2 == outcome_egreedy$arm[i])[1]]
    # update N and Q of this arm in articles_updates
    articles_updates_egreedy$N[which(articles_updates_egreedy$articles == outcome_egreedy$arm[i])] = articles_updates_egreedy$N[which(articles_updates_egreedy$articles == outcome_egreedy$arm[i])] + 1
    articles_updates_egreedy$Q[which(articles_updates_egreedy$articles == outcome_egreedy$arm[i])] = (articles_updates_egreedy$Q[which(articles_updates_egreedy$articles == outcome_egreedy$arm[i])]*(articles_updates_egreedy$N[which(articles_updates_egreedy$articles == outcome_egreedy$arm[i])]-1) + outcome_egreedy$reward[i])/articles_updates_egreedy$N[which(articles_updates_egreedy$articles == outcome_egreedy$arm[i])] 
    #delete this row in dataset
    subset_day1_numeric_egreedy <- subset_day1_numeric_egreedy[-c(which(subset_day1_numeric_egreedy$V2 == outcome_egreedy$arm[i])[1]), ]
    #rearrange indices
    rownames(subset_day1_numeric_egreedy) = seq(length=nrow(subset_day1_numeric_egreedy))
  }
}
  return(list(articles_updates_egreedy, outcome_egreedy))
}

#compute average reward to see if works
initial_values = 0.5
iterations = 1000
epsilon = 0.1
epsilon_greedy = epsilon_greedy(subset_day1_numeric, initial_values, iterations, epsilon)
articles_updates_egreedy = epsilon_greedy[[1]]
outcome_egreedy = epsilon_greedy[[2]]

#check average rewards
average_reward_egreedy = c()
for(i in 1:1000){
  average_reward_egreedy[i] = sum(outcome_egreedy$reward[1:i])/i
}
plot(average_reward_egreedy)

##################### UCB1 with non optimistic and optimistic start ###################################

### Function that returns matrix to be updated with articles, number of times pulled, average of successes and the objective function
# Initial value can be optimistic or not
# Assume in the beginning N = 1 for all, due to the form of the objective function
articles_updates_ucb1 = function(subset_day1_numeric, initial_values){
articles = unique(subset_day1_numeric[,2]) 
N = rep(1,length=length(articles))
Q = rep(initial_values,length=length(articles))
objective = rep(0,length=length(articles))
articles_updates_ucb1 = data.frame(articles, N, Q, objective)
return(articles_updates_ucb1)
}

### Function that returns the matrix of iterations to be updated, with the arm pulled and the reward 
outcome_ubc1 = function(iterations){
iterations_matrix = rep(1:iterations)
arm = rep(NA, iterations)
reward = rep(NA, iterations)  
outcome_ubc1 = data.frame(iterations_matrix, arm, reward)
return(outcome_ubc1)
}

### UCB1 algorithm
ucb1 = function(subset_day1_numeric, initial_values, iterations){
#initialize matrices
articles_updates_ucb1 = articles_updates_ucb1(subset_day1_numeric, initial_values)
outcome_ubc1 = outcome_ubc1(iterations)
#change dataset
subset_day1_numeric_ucb1 = subset_day1_numeric
### UCB1 algorithm
for(i in 1:iterations){
  # before making the optimal decision compute objective function
  articles_updates_ucb1$objective = articles_updates_ucb1$Q + sqrt((2*log(i))/articles_updates_ucb1$N)
  # choose the maximum one - if ties, take a random
  row_max_objective = which(articles_updates_ucb1$objective == max(articles_updates_ucb1$objective))
  outcome_ubc1$arm[i] = articles_updates_ucb1$articles[sample(row_max_objective,1)]
  outcome_ubc1$reward[i] = subset_day1_numeric_ucb1$V3[which(subset_day1_numeric_ucb1$V2 == outcome_ubc1$arm[i])[1]]
  # update N and Q 
  articles_updates_ucb1$N[which(articles_updates_ucb1$articles == outcome_ubc1$arm[i])] = articles_updates_ucb1$N[which(articles_updates_ucb1$articles == outcome_ubc1$arm[i])] + 1
  articles_updates_ucb1$Q[which(articles_updates_ucb1$articles == outcome_ubc1$arm[i])] = (articles_updates_ucb1$Q[which(articles_updates_ucb1$articles == outcome_ubc1$arm[i])]*(articles_updates_ucb1$N[which(articles_updates_ucb1$articles == outcome_ubc1$arm[i])]-1) + outcome_ubc1$reward[i])/articles_updates_ucb1$N[which(articles_updates_ucb1$articles == outcome_ubc1$arm[i])] 
  #delete this row in dataset
  subset_day1_numeric_ucb1 <- subset_day1_numeric_ucb1[-c(which(subset_day1_numeric_ucb1$V2 == outcome_ubc1$arm[i])[1]), ]
  #rearrange indices
  rownames(subset_day1_numeric_ucb1) = seq(length=nrow(subset_day1_numeric_ucb1))
}
return(list(articles_updates_ucb1, outcome_ubc1))
}

initial_values = 0.5
iterations = 1000
ucb1 = ucb1(subset_day1_numeric, initial_values, iterations)
articles_updates_ucb1 = ucb1[[1]]
outcome_ubc1 = ucb1[[2]]

average_reward_ucb1 = c()
for(i in 1:1000){
  average_reward_ucb1[i] = sum(outcome_ubc1$reward[1:i])/i
}
plot(average_reward_ucb1)


################################# Running several combinations #########################################

### Epsilon-greedy
# non-optimistic version, epsilon greedy 0.1
initial_values = 0.5
iterations = 1000
epsilon = 0.1

epsilon_greedy_nopt_01 = epsilon_greedy(subset_day1_numeric, initial_values, iterations, epsilon)
articles_updates_egreedy_nopt_01 = epsilon_greedy_nopt_01[[1]]
outcome_egreedy_nopt_01 = epsilon_greedy_nopt_01[[2]]

# non-optimistic version, epsilon greedy 0.1
initial_values = 0.5
iterations = 1000
epsilon = 0.01

epsilon_greedy_nopt_001 = epsilon_greedy(subset_day1_numeric, initial_values, iterations, epsilon)
articles_updates_egreedy_nopt_001 = epsilon_greedy_nopt_001[[1]]
outcome_egreedy_nopt_001 = epsilon_greedy_nopt_001[[2]]

# non-optimistic version, epsilon greedy 0.15
initial_values = 0.5
iterations = 1000
epsilon = 0.15

epsilon_greedy_nopt_015 = epsilon_greedy(subset_day1_numeric, initial_values, iterations, epsilon)
articles_updates_egreedy_nopt_015 = epsilon_greedy_nopt_015[[1]]
outcome_egreedy_nopt_015 = epsilon_greedy_nopt_015[[2]]

# optimistic version, epsilon greedy 0.1
initial_values = 1
iterations = 1000
epsilon = 0.1

epsilon_greedy_opt_01 = epsilon_greedy(subset_day1_numeric, initial_values, iterations, epsilon)
articles_updates_egreedy_opt_01 = epsilon_greedy_opt_01[[1]]
outcome_egreedy_opt_01 = epsilon_greedy_opt_01[[2]]

# optimistic version, epsilon greedy 0.1
initial_values = 1
iterations = 1000
epsilon = 0.01

epsilon_greedy_opt_001 = epsilon_greedy(subset_day1_numeric, initial_values, iterations, epsilon)
articles_updates_egreedy_opt_001 = epsilon_greedy_opt_001[[1]]
outcome_egreedy_opt_001 = epsilon_greedy_opt_001[[2]]

# non-optimistic version, epsilon greedy 0.15
initial_values = 1
iterations = 1000
epsilon = 0.15

epsilon_greedy_opt_015 = epsilon_greedy(subset_day1_numeric, initial_values, iterations, epsilon)
articles_updates_egreedy_opt_015 = epsilon_greedy_opt_015[[1]]
outcome_egreedy_opt_015 = epsilon_greedy_opt_015[[2]]

###UCB1
#non-optimistic version
initial_values = 0.5
iterations = 1000
ucb1_nopt = ucb1(subset_day1_numeric, initial_values, iterations)
articles_updates_ucb1_nopt = ucb1_nopt[[1]]
outcome_ubc1_nopt = ucb1_nopt[[2]]

#optimistic version
initial_values = 1
iterations = 1000
ucb1_opt = ucb1(subset_day1_numeric, initial_values, iterations)
articles_updates_ucb1_opt = ucb1_opt[[1]]
outcome_ubc1_opt = ucb1_opt[[2]]

### TO DO:
#repeat procedure for different clusters
