source('unifrac.R')
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
						 gate = FALSE){
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

#S  <- 2^10
#tree <- binary_tree(1:S, 1)
#
### Simulations
#simulation  <- unifrac_simulation(1000,
#								  tree,
#								  S,
#								  size,
#								  0)
#simulation2  <- unifrac_simulation(100,
#								  tree,
#								  S,
#								  size,
#								  0)
#simulation3  <- unifrac_simulation(100,
#								  tree,
#								  S,
#								  size,
#								  0.1)
#simulation4  <- unifrac_simulation(100,
#								  tree,
#								  S,
#								  size,
#								  0.25)
#simulation5  <- unifrac_simulation(100,
#								  tree,
#								  S,
#								  size,
#								  0.5)
#simulation6  <- unifrac_simulation(100,
#								  tree,
#								  S,
#								  size,
#								  0.75)
#simulation7  <- unifrac_simulation(100,
#								  tree,
#								  S,
#								  size,
#								  0.9)
#simulation8  <- unifrac_simulation(100,
#								  tree,
#								  S,
#								  size,
#								  1)
#gate_sim1  <- unifrac_simulation(100,
#								 tree,
#								 S,
#								 size,
#								 0.99,
#								 dist = dbinom(1:S,
#											   prob=0.5,
#											   size =S
#								 ),
#								 gate=TRUE
#								 )
#nogate_sim1  <- unifrac_simulation(100,
#								 tree,
#								 S,
#								 size,
#								 0.99,
#								 dist = dbinom(1:S,
#											   prob=0.5,
#											   size =S
#								 ),
#								 gate=FALSE
#								 )
#
#gate_sim2  <- unifrac_simulation(100,
#								 tree,
#								 S,
#								 size,
#								 0.9,
#								 dist = dbinom(1:S,
#											   prob=0.5,
#											   size =S
#								 ),
#								 gate=TRUE
#								 )
#nogate_sim2  <- unifrac_simulation(100,
#								 tree,
#								 S,
#								 size,
#								 0.9,
#								 dist = dbinom(1:S,
#											   prob=0.5,
#											   size =S
#								 ),
#								 gate=FALSE
#								 )
#gate_sim3  <- unifrac_simulation(100,
#								 tree,
#								 S,
#								 size,
#								 0.99,
#								 dist = 'uniform',
#								 gate=TRUE
#								 )
#nogate_sim3  <- unifrac_simulation(100,
#								 tree,
#								 S,
#								 size,
#								 0.99,
#								 dist = 'uniform',
#								 gate=FALSE
#								 )
#gate_sim4  <- unifrac_simulation(100,
#								 tree,
#								 S,
#								 size,
#								 0.9,
#								 dist = 'uniform',
#								 gate=TRUE
#								 )
#nogate_sim4  <- unifrac_simulation(100,
#								 tree,
#								 S,
#								 size,
#								 0.9,
#								 dist = 'uniform',
#								 gate=FALSE
#								 )
#
### Plots
#png('sample_size_plots.png')
#ggplot(simulation) +
#		geom_point(aes(x=size,
#					   y=Unifrac),
#				   alpha=0.25,
#				   position='jitter'
#		) +
#		theme_bw() +
#		labs(
#			 title='Simulated Unifrac (100% overlap)',
#			 x='size (times 1024)'
#		)
#dev.off()
#png('sample_size_plots2.png')
#ggplot(simulation2) +
#		geom_point(aes(x=size,
#					   y=Unifrac),
#				   alpha=0.25,
#				   position='jitter'
#		) +
#		theme_bw() +
#		labs(
#			 title='Simulated Unifrac (100% overlap)',
#			 x='size (times 1024)'
#		)
#
#dev.off()
#png('sample_size_plots3.png')
#ggplot(simulation3) +
#		geom_point(aes(x=size,
#					   y=Unifrac),
#				   alpha=0.25,
#				   position='jitter'
#		) +
#		theme_bw() +
#		labs(
#			 title='Simulated Unifrac (90% overlap)',
#			 x='size (times 1024)'
#		)
#
#dev.off()
#png('sample_size_plots4.png')
#ggplot(simulation4) +
#		geom_point(aes(x=size,
#					   y=Unifrac),
#				   alpha=0.25,
#				   position='jitter'
#		) +
#		theme_bw()+
#		labs(
#			 title='Simulated Unifrac (75% overlap)',
#			 x='size (times 1024)'
#		)
#
#dev.off()
#png('sample_size_plots5.png')
#ggplot(simulation5) +
#		geom_point(aes(x=size,
#					   y=Unifrac),
#				   alpha=0.25,
#				   position='jitter'
#		) +
#		theme_bw()+
#		labs(
#			 title='Simulated Unifrac (50% overlap)',
#			 x='size (times 1024)'
#		)
#
#dev.off()
#png('sample_size_plots6.png')
#ggplot(simulation6) +
#		geom_point(aes(x=size,
#					   y=Unifrac),
#				   alpha=0.25,
#				   position='jitter'
#		) +
#		theme_bw()+
#		labs(
#			 title='Simulated Unifrac (25% overlap)',
#			 x='size (times 1024)'
#		)
#
#dev.off()
#png('sample_size_plots7.png')
#ggplot(simulation7) +
#		geom_point(aes(x=size,
#					   y=Unifrac),
#				   alpha=0.25,
#				   position='jitter'
#		) +
#		theme_bw()+
#		labs(
#			 title='Simulated Unifrac (10% overlap)',
#			 x='size (times 1024)'
#		)
#
#dev.off()
#png('sample_size_plots8.png')
#ggplot(simulation8) +
#		geom_point(aes(x=size,
#					   y=Unifrac),
#				   alpha=0.25,
#				   position='jitter'
#		) +
#		theme_bw()+
#		labs(
#			 title='Simulated Unifrac (no overlap)',
#			 x='size (times 1024)'
#		)
#
#dev.off()
#gateP1  <- ggplot(gate_sim1) +
#		geom_point(aes(x=size,
#					   y=Unifrac),
#				   alpha=0.25,
#				   position='jitter'
#		) +
#		theme_bw()+
#		labs(
#			 title='Simulated Unifrac (no overlap)',
#			 x='size (times 1024)'
#		)
#nogateP1  <- ggplot(nogate_sim1) +
#		geom_point(aes(x=size,
#					   y=Unifrac),
#				   alpha=0.25,
#				   position='jitter'
#		) +
#		theme_bw()+
#		labs(
#			 title='Simulated Unifrac (no overlap)',
#			 x='size (times 1024)'
#		)
#png('gate_sim1.png')
#grid.arrange(gateP1,
#			 nogateP1,
#			 ncol=2
#)
#dev.off()
#gateP2  <- ggplot(gate_sim2) +
#		geom_point(aes(x=size,
#					   y=Unifrac),
#				   alpha=0.25,
#				   position='jitter'
#		) +
#		theme_bw()+
#		labs(
#			 title='Simulated Unifrac (no overlap)',
#			 x='size (times 1024)'
#		)
#nogateP2  <- ggplot(nogate_sim2) +
#		geom_point(aes(x=size,
#					   y=Unifrac),
#				   alpha=0.25,
#				   position='jitter'
#		) +
#		theme_bw()+
#		labs(
#			 title='Simulated Unifrac (no overlap)',
#			 x='size (times 1024)'
#		)
#png('gate_sim1.png')
#grid.arrange(gateP2,
#			 nogateP2,
#			 ncol=2
#)
#dev.off()
#gateP3  <- ggplot(gate_sim3) +
#		geom_point(aes(x=size,
#					   y=Unifrac),
#				   alpha=0.25,
#				   position='jitter'
#		) +
#		theme_bw()+
#		labs(
#			 title='Simulated Unifrac (no overlap)',
#			 x='size (times 1024)'
#		)
#nogateP3  <- ggplot(nogate_sim3) +
#		geom_point(aes(x=size,
#					   y=Unifrac),
#				   alpha=0.25,
#				   position='jitter'
#		) +
#		theme_bw()+
#		labs(
#			 title='Simulated Unifrac (no overlap)',
#			 x='size (times 1024)'
#		)
#png('gate_sim3.png')
#grid.arrange(gateP3,
#			 nogateP3,
#			 ncol=2
#)
#dev.off()
#gateP4  <- ggplot(gate_sim4) +
#		geom_point(aes(x=size,
#					   y=Unifrac),
#				   alpha=0.25,
#				   position='jitter'
#		) +
#		theme_bw()+
#		labs(
#			 title='Simulated Unifrac (no overlap)',
#			 x='size (times 1024)'
#		)
#nogateP4  <- ggplot(nogate_sim4) +
#		geom_point(aes(x=size,
#					   y=Unifrac),
#				   alpha=0.25,
#				   position='jitter'
#		) +
#		theme_bw()+
#		labs(
#			 title='Simulated Unifrac (no overlap)',
#			 x='size (times 1024)'
#		)
#png('gate_sim4.png')
#grid.arrange(gateP4,
#			 nogateP4,
#			 ncol=2
#)
#dev.off()
