#load packages
if ( !require(ggplot2) ) install.packages("ggplot2"); library(ggplot2)
if ( !require(reshape) ) install.packages("reshape"); library(reshape)

################## Picking the best candidate out of N ########################
## run algorithm n times and count average number of Wins for different N's.
n           <- 5000
N_values    <- seq(1,20, 1)
rank_chosen <- matrix( nrow=n, ncol=length(N_values) )

for ( j in 1:length(N_values) ) {
    
    # total number of candidates
    N <- N_values[j]
    
    # calculate m*
    m_crit <- N/exp(1)
    
    for (i in 1:n) {
        # randomise the order in which candidates are interviewed
        # value = rank of candidate, where 1 is the best. 
        candidates <- sample(N, N)
        
        # candidates interviewed after m*-1th candidate
        considered_candidates   <- candidates[ceiling(m_crit):N]
        
        # score of best candidate up until and including m*-1th candidate
        if ( m_crit < 1) {
            best_so_far <- NULL
        } else {
            best_so_far <- min( candidates[1:floor(m_crit)] )
        }
        
        # if m_crit is less than 1 (N=1 or N=2) then take the first candidate 
        if ( is.null(best_so_far) ) {
            rank_chosen[i,j] <- considered_candidates[1]
            
        # if a viable option exists in remaining candidates
        } else if ( min(considered_candidates) < best_so_far) {
            cand_no <- min( which(considered_candidates < best_so_far) )
            rank_chosen[i,j] <- considered_candidates[cand_no]  
            
        # if no viable option exists, must employ final candidate
        } else {
            rank_chosen[i,j] <- candidates[N]
        }
        
    }  
    
}

# average times the best candidate was selected
#prob_best <- apply(rank_chosen, 2, function(x) sum(x==1)/n)
#plot(prob_best)

# plot showing how many times we need to carry out the hiring process to really 
# reach the expected value
prob_best_evo <- matrix(nrow=n, ncol=N)
prob_best_evo[1,] <-  ifelse( rank_chosen[1,]==1, 1, 0)
for (t in 2:n) {
    prob_best_evo[t,] <- apply(rank_chosen[1:t,], 2, function(x) sum(x==1)/t)
}

### plots
# 1. plot proportion of Wins after 1000 iters for different N's
df <- data.frame(N = N_values, prob = prob_best_evo[n,])
ggplot( data=df, aes(x=N, y=prob) ) + 
    geom_line( colour="coral", size=1.25 ) + 
    geom_point( colour="coral", size=2.5 ) +
    geom_hline( yintercept = 1/exp(1), linetype="dashed", size=0.8) +
    scale_x_continuous( breaks=seq(0, 20, 5)) +
    scale_y_continuous( breaks=seq(0, 1, 0.2), 
                        name="proportion of times best candidate was picked" ) +
    coord_cartesian( ylim=c(0, 1) ) +
    ggtitle( "Best of N\n(5000 simulations)" ) +
    theme( plot.title = element_text(size=26, face="bold", margin=margin(t=12, b=16)),
           axis.title.x = element_text(size=18, margin=margin(b=5, t=16)),
           axis.text.x  = element_text(size=14),
           axis.title.y = element_text(size=18, margin=margin(l=10, r=20)),
           axis.text.y  = element_text(size=14) )


# 2. plot evolution of probabilities with increasing no. of iters
# get data ready for plotting
prob_best_evo <- as.data.frame(prob_best_evo)
names(prob_best_evo) <- seq(1, N)
prob_best_evo <- cbind(simulations = 1:n, prob_best_evo )
df <- melt(prob_best_evo[1:1000,1:11], id.vars = "simulations")

# plot evolution
ggplot( df, aes(simulations, value) ) +
    geom_line( aes(colour = variable), size=0.8 ) +
    geom_hline( yintercept = 1/exp(1), linetype="dashed" ) +
    scale_colour_discrete( name ="No. candidates" ) +
    ylab( "proportion of times best candidate picked" ) +
    ggtitle( "Best of N: probability of a Win evolution" ) +
    theme( plot.title = element_text(size=26, face="bold", margin=margin(t=12, b=16)),
           axis.title.x = element_text(size=18, margin=margin(b=5, t=16)),
           axis.text.x  = element_text(size=14),
           axis.title.y = element_text(size=18, margin=margin(l=10, r=20)),
           axis.text.y  = element_text(size=14) )

######################### One of the best k of N ###############################
n           <- 10000
N           <- 100
k_values    <- seq(1,10,1)
prob_1_of_k <- rep(NA, length(k_values))

for ( i in 1:length(k_values) ) {
    
    # set k 
    k <- k_values[i]
    
    # set thresholds for given k
    thresh <- N/exp(1)
    if (k>1) {
        for (j in 2:k) {
            thresh <- c( thresh, ((j/(2*j-1))^(1/(j-1)))*N )
        }
    }
    
    rank_chosen <- rep(NA, n)
    for (iter in 1:n) {
        # randomise the order in which candidates are interviewed 
        candidates <- sample(N, N)
        
        # find minimumn in observation set
        best_so_far <- min( candidates[ 1:(floor( thresh[1] )) ] )
        
        # recursively run algorithm
        if ( k > 1 ) {
            for (t in 2:k) { #check we are subsetting candidates and not a simple 1:something vector
                
                # find best candidate in current interval
                considered_candidates <- candidates[ (ceiling( thresh[t-1] )):(floor( thresh[t] )) ]
                best_cand <- min( considered_candidates )
                
                # if candidate is better than (t-1)th best so far --> accept
                if ( best_cand < max(best_so_far) ) {
                    cand_no <- min( which(considered_candidates < max(best_so_far)) )
                    rank_chosen[iter] <- considered_candidates[cand_no]
                    break
                    # otherwise update the best so far to contain the best t candidates so far    
                } else {
                    best_so_far <- sort( candidates[ 1:(floor( thresh[t] )) ] )[1:t]
                }
            }
        }
        
        # check final interval s_k:N if no candidate has been chosen yet
        if ( is.na(rank_chosen[iter]) ) {
            
            # find best candidate
            considered_candidates <- candidates[ (ceiling( thresh[k] )):N ]
            best_cand <- min( considered_candidates )
            
            # if better than kth best so far --> accept
            if ( best_cand < max(best_so_far) ) {
                cand_no <- min( which(considered_candidates < max(best_so_far)) )
                rank_chosen[iter] <- considered_candidates[cand_no] 
            } else {
                rank_chosen[iter] <- candidates[N]
            }
        }  
    }
    
    # count how many times the candidate selected was actually in the top k
    prob_1_of_k[i] <- mean( rank_chosen %in% 1:k )
    
}    


### plots
# 3. plot probability of a Win 
df <- data.frame(k = k_values, prob = prob_1_of_k)
ggplot( data=df, aes(x=k, y=prob) ) + 
    geom_line( colour="coral", size=1.25 ) + 
    geom_point( colour="coral", size=2.5 ) +
    scale_x_continuous( breaks=seq(0, 10, 1), minor_breaks = seq(0, 10, 1) ) +
    scale_y_continuous( breaks=seq(0, 1, 0.2), 
                        name="proportion of times 1 of best k candidates picked" ) +
    expand_limits(y=0) +
    ggtitle( "One of the best k of N\n(N=100, 10 000 simulations)" ) +
    theme( plot.title = element_text(size=26, face="bold", margin=margin(t=12, b=16)),
           axis.title.x = element_text(size=18, margin=margin(b=5, t=16)),
           axis.text.x  = element_text(size=14),
           axis.title.y = element_text(size=18, margin=margin(l=10, r=20)),
           axis.text.y  = element_text(size=14) )

############################# Best k out of N ##################################
n           <- 10000
N_values    <- 100
k_values    <- seq(1,20, 1)
prob_best_k <- rep(NA, length(k_values))

for ( j in 1:length(k_values) ) {
    
    # set k
    k <- k_values[j]
    
    # total number of candidates
    #N <- N_values[j]
    N <- N_values
    
    # calculate m*
    m_crit <- N/(k*exp(1)^(1/k))
    
    #initialise matrix for saving the ranks of the chosen candidates
    rank_chosen <- matrix( nrow=n, ncol=k )
    
    for (i in 1:n) {
        # randomise the order in which candidates are interviewed 
        candidates <- sample(N, N)
        
        # candidates interviewed after m*-1th candidate
        considered_candidates   <- candidates[ceiling(m_crit):N]
        
        # score of best candidate up until and including m*-1th candidate
        if ( m_crit < 1) {
            threshold <- NULL
        } else {
            threshold <- min( candidates[1:floor(m_crit)] )
        }
        
        # choose candidates:
        # if m_crit is less than 1 (N=1 or N=2), take the first k candidates 
        if ( is.null(threshold) ) {
            rank_chosen[i,1:k] <- considered_candidates[1:k]
            
        # if k or more viable candidates exist (ie threshold >= k+1), employ first k of them
        } else if ( sum(considered_candidates < threshold) >= k) {
            for (t in 1:k) {
                
                #find first viable candidate in remaining candidates
                cand_no <- min( which(considered_candidates < threshold) )
                rank_chosen[i,t] <- considered_candidates[cand_no] 
                
                # update candidates interviewd afterwards
                considered_candidates <- considered_candidates[-(1:cand_no)]
            }
            
        # if less than k viable options exist (ie the threshold is <= k) it is impossible to hire the best k candidates 
        } else {
            # insert dummy values (that aren't 1:k)
            rank_chosen[i,1:k] <- rep(100,k)
        }
        
    }  
    
    # proportion of times the top k candidates were picked
    prob_best_k[j] <- mean( apply(rank_chosen, 1, function(x) all(x %in% (1:k))) ) 
}

### plots
# 4. plot probability of a Win 
theoret_prob <- data.frame( k=k_values, probs=exp(-1)/k_values )
df <- data.frame(k = k_values, prob = prob_best_k)
ggplot( data=df, aes(x=k, y=prob) ) + 
    geom_line( colour="coral", size=1.25 ) + 
    geom_point( colour="coral", size=2.5 ) +
    geom_line( data=theoret_prob, aes(x=k_values, y=probs), linetype="dashed" ) +
    scale_x_continuous( breaks=seq(0, 20, 5), minor_breaks = seq(0, 20, 1) ) +
    scale_y_continuous( breaks=seq(0,0.4, 0.05), 
                        name="proportion of times best k candidates picked" ) +
    ggtitle( "Best k of N\n(N=100, 10 000 simulations)" ) +
    theme( plot.title = element_text(size=26, face="bold", margin=margin(t=12, b=16)),
           axis.title.x = element_text(size=18, margin=margin(b=5, t=16)),
           axis.text.x  = element_text(size=14),
           axis.title.y = element_text(size=18, margin=margin(l=10, r=20)),
           axis.text.y  = element_text(size=14) )
 


    