source('unifrac.R')
source('find_alpha.R')
library(ggplot2)
library(gridExtra)

## Functions
### Get n replications from simulate_unifrac
rep_unifrac  <- function(n_reps,
						 tree,
						 prop_size,
						 non_overlap,
						 n_loc = 2,
						 n_treat = 1,
						 dist = 'uniform',
						 gate = 0){
		S  <- length(tree$tip.label)
		replicate(n_reps,
				  simulate_unifrac(tree = tree,
								   n_sam = S*prop_size,
								   non_overlap = non_overlap,
								   n_loc = n_loc,
								   n_treat = n_treat,
								   dist = dist,
								   gate = gate
				  )
		)
}

### Make length(size) simulations with n_reps and structure them
### into a data frame.
unifrac_simulation  <- function(n_reps,
								tree,
								prop_size,
								non_overlap,
								n_loc = 2,
								n_treat = 1,
								dist = 'uniform',
								gate = 0){
		sims  <- c(
				   sapply(prop_size,
						  function(X){
								  rep_unifrac(n_reps = n_reps,
											  tree = tree,
											  prop_size = X,
											  non_overlap = non_overlap,
											  n_loc = n_loc,
											  n_treat = n_treat,
											  dist = dist,
											  gate = gate
								  )
						  }
				   )
		)

		sizes  <- rep(prop_size, each=n_reps)

		simulations  <- data.frame(
								   'Unifrac' = sims,
								   'size' = sizes
		)

		return(simulations)
}

unifrac_simulation(n_reps = 1,
				   tree = tree,
				   prop_size = 1,
				   non_overlap = 0,
				   dist = bin_dist,
				   gate=1/S)
