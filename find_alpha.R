require(MCMCpack)

rm_obs  <- function(mat,
					params = NULL,
					gate = 0.05){
		all_obs <- rowSums(mat)

		s  <-  length(all_obs)
		N  <- sum(all_obs)
		post  <- list(alpha=numeric(s),
				      beta =numeric(s))

		if(is.null(params)){
				params$alpha  <- rep(1, s)
				params$beta  <- rep(1, s)
		}

		post$alpha  <- all_obs + params$alpha
		post$beta  <- N - all_obs + params$alpha

		prob = post$alpha/(post$alpha+post$beta)
		keep  <- prob > gate

		return(mat*keep)
}


