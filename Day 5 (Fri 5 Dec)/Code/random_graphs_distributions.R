# Random networks

library(igraph)

# Create a random network
g <- erdos.renyi.game(n = 1000,p.or.m = 0.1,type = "gnp")
#g <- watts.strogatz.game(1,size = 100,nei = 5,p = 0.05)
#g <- barabasi.game(500, power = 1.5, m = NULL, out.dist = NULL, out.seq = NULL,out.pref = FALSE, zero.appeal = 1, directed = FALSE,algorithm ="psumtree", start.graph = NULL)
#g <- static.power.law.game(500, 500, exponent.out= 2.2, exponent.in = -1, loops = FALSE, multiple = FALSE, finite.size.correction = TRUE) 

# Determine degree (and max degree)
degree <- degree(g)
maxdeg <- max(degree)

# Visualize network (possibly adjust 20 factor)
plot(g, vertex.size=20*(degree/maxdeg)+3,cex=0.5,edge.width=2,edge.color="gray",vertex.label=NA)

# Determine degree distribution (converting to factors so I also include zero-frequency values)
degdist <- table(factor(degree, levels = 0:max(degree)+1))

# Plot as frequency distribution (proportions, not absolte numbers)
barplot(degdist/vcount(g), las=2,xlab = "Degree",ylab = "Frequency")
