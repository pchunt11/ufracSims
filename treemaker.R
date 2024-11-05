library(ape)
library(phyloseq)

binary_tree <- function(labels,#,
						#b_lengths#,
						root=TRUE
						){
		s <- length(labels)

		node  <- '( :1, :1):1'
		i <- 1
		while(i < log(s, 2)){
				node  <- paste(c('(',
								 node, ':1, ',
								 node, '):1'),
							   collapse='')
				i <- i+1
		}
		if(root==TRUE){
				node <- paste(
							  c('(',
								node,
								':1)'
							  ),
							  collapse=''
				)
		}
		tree <- paste(c(node,';'),
						collapse='')
		phylo_tree  <- read.tree(text=tree)
		phylo_tree$tip.label <- labels
		#phylo_tree  <- stree(s,
		#					 type='balanced',
		#					 tip.label=labels
		#)
		#phylo_tree$edge.length  <- b_lengths

		return(phylo_tree)
}


#test_tree  <- binary_tree(1:2^3)#, rep(1,8*7))
#plot(test_tree)
#plot(phylo_tree)

