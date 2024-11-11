source('unifrac_simulation.R')
library(dplyr)
library(ggplot2)
library(doParallel)
registerDoParallel(cores=8)

S  <- 2^10
tree <- binary_tree(1:S, 1)
n_sim  <- 100
size  <- seq(0.25, 5, 0.25)

bin_dist <- dbinom(1:S, size=S, prob=0.5)

## Simulations
#simulation  <- unifrac_simulation(1000,
#								  tree,
#								  #								  size,
#								  0)
#
unif_ng.100 <- unifrac_simulation(100,
								  tree,
								  size,
								  0)
unif_g.100  <- unifrac_simulation(100,
								  tree,
								  size,
								  0,
								  gate=1/S)
bin_g.100  <- unifrac_simulation(100,
								 tree,
								 size,
								 0,
								 dist = bin_dist,
								 gate=1/S
								 )
bin_ng.100  <- unifrac_simulation(100,
								 tree,
								 size,
								 0,
								 dist = bin_dist,
								 gate=0
								 )
sim.100  <- rbind(unif_ng.100,
				  unif_g.100,
				  bin_ng.100,
				  bin_g.100) %>%
		    cbind(Gate = rep(c('No Gate', 'Gate', 'No Gate', 'Gate'),
							 each=n_sim*length(size)),
				  Distribution = rep(c('Uniform', 'Binomial'),
							 each=2*n_sim*length(size))
			)

plot.100  <- ggplot(sim.100)+
		geom_point(aes(x=size,
					   y=Unifrac),
				   alpha=0.25,
				   position='jitter'
		) +
		facet_wrap(vars(Distribution, Gate)) +
		theme_bw()+
		labs(
			 title='Simulated Unifrac (100% Overlap)',
			 x='size (times 1024)'
		)
svg('Plots/sim_100.svg')
		plot.100
dev.off()

## 90% Overlap ##
unif_ng.90  <- unifrac_simulation(100,
								  tree,
								  size,
								  0.1)
unif_g.90  <- unifrac_simulation(100,
								  tree,
								  size,
								  0.1,
								  gate=1/S)
bin_g.90  <- unifrac_simulation(100,
								 tree,
								 size,
								 0.1,
								 dist = bin_dist,
								 gate=1/S
								 )
bin_ng.90  <- unifrac_simulation(100,
								 tree,
								 size,
								 0.1,
								 dist = bin_dist,
								 gate=0
								 )
sim.90  <- rbind(unif_ng.90,
				  unif_g.90,
				  bin_ng.90,
				  bin_g.90) %>%
		    cbind(Gate = rep(c('No Gate', 'Gate', 'No Gate', 'Gate'),
							 each=n_sim*length(size)),
				  Distribution = rep(c('Uniform', 'Binomial'),
							 each=2*n_sim*length(size))
			)

plot.90  <- ggplot(sim.90)+
		geom_point(aes(x=size,
					   y=Unifrac),
				   alpha=0.25,
				   position='jitter'
		) +
		facet_wrap(vars(Distribution, Gate)) +
		theme_bw()+
		labs(
			 title='Simulated Unifrac 90% Overlap)',
			 x='size (times 1024)'
		)
svg('Plots/sim_90.svg')
		plot.90
dev.off()

## 75% Overlap ##
unif_ng.75  <- unifrac_simulation(100,
								  tree,
								  size,
								  0.25)
unif_g.75  <- unifrac_simulation(100,
								  tree,
								  size,
								  0.25,
								  gate=1/S)
bin_g.75  <- unifrac_simulation(100,
								 tree,
								 size,
								 0.25,
								 dist = bin_dist,
								 gate=1/S
								 )
bin_ng.75  <- unifrac_simulation(100,
								 tree,
								 size,
								 0.25,
								 dist = bin_dist,
								 gate=0
								 )

sim.75  <- rbind(unif_ng.75,
				  unif_g.75,
				  bin_ng.75,
				  bin_g.75) %>%
		    cbind(Gate = rep(c('No Gate', 'Gate', 'No Gate', 'Gate'),
							 each=n_sim*length(size)),
				  Distribution = rep(c('Uniform', 'Binomial'),
							 each=2*n_sim*length(size))
			)

plot.75  <- ggplot(sim.75)+
		geom_point(aes(x=size,
					   y=Unifrac),
				   alpha=0.25,
				   position='jitter'
		) +
		facet_wrap(vars(Distribution, Gate)) +
		theme_bw()+
		labs(
			 title='Simulated Unifrac (75% Overlap)',
			 x='size (times 1024)'
		)
svg('Plots/sim_75.svg')
		plot.75
dev.off()

## 50% Overlap ##
unif_ng.50  <- unifrac_simulation(100,
								  tree,
								  size,
								  0.5)
unif_g.50  <- unifrac_simulation(100,
								  tree,
								  size,
								  0.5,
								  gate=1/S)
bin_g.50  <- unifrac_simulation(100,
								 tree,
								 size,
								 0.5,
								 dist = bin_dist,
								 gate=1/S
								 )
bin_ng.50  <- unifrac_simulation(100,
								 tree,
								 size,
								 0.5,
								 dist = bin_dist,
								 gate=0
								 )

sim.50  <- rbind(unif_ng.50,
				  unif_g.50,
				  bin_ng.50,
				  bin_g.50) %>%
		    cbind(Gate = rep(c('No Gate', 'Gate', 'No Gate', 'Gate'),
							 each=n_sim*length(size)),
				  Distribution = rep(c('Uniform', 'Binomial'),
							 each=2*n_sim*length(size))
			)

plot.50  <- ggplot(sim.50)+
		geom_point(aes(x=size,
					   y=Unifrac),
				   alpha=0.25,
				   position='jitter'
		) +
		facet_wrap(vars(Distribution, Gate)) +
		theme_bw()+
		labs(
			 title='Simulated Unifrac (50% Overlap)',
			 x='size (times 1024)'
		)
svg('Plots/sim_50.svg')
		plot.50
dev.off()

## 25% Overlap ##
unif_ng.25  <- unifrac_simulation(100,
								  tree,
								  size,
								  0.75)
unif_g.25  <- unifrac_simulation(100,
								  tree,
								  size,
								  0.75,
								  gate=1/S)
bin_g.25  <- unifrac_simulation(100,
								 tree,
								 size,
								 0.75,
								 dist = bin_dist,
								 gate=1/S
								 )
bin_ng.25  <- unifrac_simulation(100,
								 tree,
								 size,
								 0.75,
								 dist = bin_dist,
								 gate=0
								 )

sim.25  <- rbind(unif_ng.25,
				  unif_g.25,
				  bin_ng.25,
				  bin_g.25) %>%
		    cbind(Gate = rep(c('No Gate', 'Gate', 'No Gate', 'Gate'),
							 each=n_sim*length(size)),
				  Distribution = rep(c('Uniform', 'Binomial'),
							 each=2*n_sim*length(size))
			)

plot.25  <- ggplot(sim.25)+
		geom_point(aes(x=size,
					   y=Unifrac),
				   alpha=0.25,
				   position='jitter'
		) +
		facet_wrap(vars(Distribution, Gate)) +
		theme_bw()+
		labs(
			 title='Simulated Unifrac (25% Overlap)',
			 x='size (times 1024)'
		)
svg('Plots/sim_25.svg')
		plot.25
dev.off()

## 10% Overlap ##
unif_ng.10  <- unifrac_simulation(100,
								  tree,
								  size,
								  0.9)
unif_g.10  <- unifrac_simulation(100,
								  tree,
								  size,
								  0.9,
								  gate=1/S)
bin_g.10  <- unifrac_simulation(100,
								 tree,
								 size,
								 0.9,
								 dist = bin_dist,
								 gate=1/S
								 )
bin_ng.10  <- unifrac_simulation(100,
								 tree,
								 size,
								 0.9,
								 dist = bin_dist,
								 gate=0
								 )

sim.10  <- rbind(unif_ng.10,
				  unif_g.10,
				  bin_ng.10,
				  bin_g.10) %>%
		    cbind(Gate = rep(c('No Gate', 'Gate', 'No Gate', 'Gate'),
							 each=n_sim*length(size)),
				  Distribution = rep(c('Uniform', 'Binomial'),
							 each=2*n_sim*length(size))
			)

plot.10  <- ggplot(sim.10)+
		geom_point(aes(x=size,
					   y=Unifrac),
				   alpha=0.25,
				   position='jitter'
		) +
		facet_wrap(vars(Distribution, Gate)) +
		theme_bw()+
		labs(
			 title='Simulated Unifrac (10% Overlap)',
			 x='size (times 1024)'
		)
svg('Plots/sim_10.svg')
		plot.10
dev.off()

## 1% Overlap ##
unif_ng.01  <- unifrac_simulation(100,
								  tree,
								  size,
								  0.99)
unif_g.01  <- unifrac_simulation(100,
								  tree,
								  size,
								  0.99,
								  gate=1/S)
bin_g.01  <- unifrac_simulation(100,
								 tree,
								 size,
								 0.99,
								 dist = bin_dist,
								 gate=1/S
								 )
bin_ng.01  <- unifrac_simulation(100,
								 tree,
								 size,
								 0.99,
								 dist = bin_dist,
								 gate=0
								 )

sim.01  <- rbind(unif_ng.01,
				  unif_g.01,
				  bin_ng.01,
				  bin_g.01) %>%
		    cbind(Gate = rep(c('No Gate', 'Gate', 'No Gate', 'Gate'),
							 each=n_sim*length(size)),
				  Distribution = rep(c('Uniform', 'Binomial'),
							 each=2*n_sim*length(size))
			)

plot.01  <- ggplot(sim.01)+
		geom_point(aes(x=size,
					   y=Unifrac),
				   alpha=0.25,
				   position='jitter'
		) +
		facet_wrap(vars(Distribution, Gate)) +
		theme_bw()+
		labs(
			 title='Simulated Unifrac (01% Overlap)',
			 x='size (times 1024)'
		)
svg('Plots/sim_01.svg')
		plot.01
dev.off()

## 0.5% Overlap ##
unif_ng.005  <- unifrac_simulation(100,
								  tree,
								  size,
								  0.995)
unif_g.005  <- unifrac_simulation(100,
								  tree,
								  size,
								  0.995,
								  gate=1/S)
bin_g.005  <- unifrac_simulation(100,
								 tree,
								 size,
								 0.995,
								 dist = bin_dist,
								 gate=1/S
								 )
bin_ng.005  <- unifrac_simulation(100,
								 tree,
								 size,
								 0.995,
								 dist = bin_dist,
								 gate=0
								 )

sim.005  <- rbind(unif_ng.005,
				  unif_g.005,
				  bin_ng.005,
				  bin_g.005) %>%
		    cbind(Gate = rep(c('No Gate', 'Gate', 'No Gate', 'Gate'),
							 each=n_sim*length(size)),
				  Distribution = rep(c('Uniform', 'Binomial'),
							 each=2*n_sim*length(size))
			)

plot.005  <- ggplot(sim.005)+
		geom_point(aes(x=size,
					   y=Unifrac),
				   alpha=0.25,
				   position='jitter'
		) +
		facet_wrap(vars(Distribution, Gate)) +
		theme_bw()+
		labs(
			 title='Simulated Unifrac (0.5% Overlap)',
			 x='size (times 1024)'
		)
svg('Plots/sim_005.svg')
		plot.005
dev.off()

## 0% Overlap ##
unif_ng.0  <- unifrac_simulation(100,
								  tree,
        						  size,
								  1)
unif_g.0  <- unifrac_simulation(100,
								  tree,
								  size,
								  1,
								  gate=1/S)
bin_g.0  <- unifrac_simulation(100,
								 tree,
							     size,
								 1,
								 dist = bin_dist,
								 gate=1/S
								 )
bin_ng.0  <- unifrac_simulation(100,
								 tree,
								 size,
								 1,
								 dist = bin_dist,
								 gate=0
								 )
sim.0  <- rbind(unif_ng.0,
				  unif_g.0,
				  bin_ng.0,
				  bin_g.0) %>%
		    cbind(Gate = rep(c('No Gate', 'Gate', 'No Gate', 'Gate'),
							 each=n_sim*length(size)),
				  Distribution = rep(c('Uniform', 'Binomial'),
							 each=2*n_sim*length(size))
			)

plot.0  <- ggplot(sim.0)+
		geom_point(aes(x=size,
					   y=Unifrac),
				   alpha=0.25,
				   position='jitter'
		) +
		facet_wrap(vars(Distribution, Gate)) +
		theme_bw()+
		labs(
			 title='Simulated Unifrac (0% Overlap)',
			 x='size (times 1024)'
		)
svg('Plots/sim_0.svg')
		plot.0
dev.off()

