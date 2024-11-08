source('unifrac.R')
source('find_alpha.R')
library(ggplot2)
library(gridExtra)

## Functions
### Get n replications from simulate_unifrac
rep_unifrac  <- function(n,
						 tree,
						 S,
						 size,
						 overlap,
						 dist = 'uniform',
						 gate = 0){
		replicate(n,
				  simulate_unifrac(tree,
								   S*size,
								   overlap,
								   dist,
								   gate
				  )
		)
}

### Make length(size) simulations with n_reps and structure them
### into a data frame.
unifrac_simulation  <- function(n_reps,
								tree,
								S, size,
								overlap,
								dist = 'uniform',
								gate = 0){
		sims  <- c(
				   sapply(size,
						  function(X){
								  rep_unifrac(n_reps,
											  tree, S,
											  X, overlap,
											  dist, gate
								  )
						  }
				   )
		)
		sizes  <- rep(size, each=n_reps)

		data.frame(
				   'Unifrac' = sims,
				   'size' = sizes
		)
}

unifrac_simulation(1, tree, 20*S, 1, 0, bin_dist, gate=1/S)
