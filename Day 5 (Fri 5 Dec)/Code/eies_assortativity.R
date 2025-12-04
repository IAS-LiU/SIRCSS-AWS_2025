# EIES and assortativity coefficient

# init igraph library
library(igraph)

# Set working directory
setwd("C:/Users/pekpi/Dropbox/work/dropboxR/WinterCSS")

# Load example network file and store as 'matrix' object
# Here we start with loading EIES friendship nominations
# Conference with 32 scholars on network analysis
# Friendship nominations at beginning of conference: Likert 1-4, directional
# Here: dichotomized: only including friendship ties 2 and above
# Dichotomized: no reciprocity needed: one-directional assumed symmetric
# Already prepared this datafile

matrix <- as.matrix(
  read.csv("data/symmmin_dichGE3_eies_t1.txt",
           sep="\t",
           header=TRUE,
           row.names=1,
           check.names = FALSE,
           na.strings = "")
)

# We convert this into igraph object
# This data is undirected and without weights
g <- graph.adjacency(matrix, mode="undirected", weighted=NULL)

# In what follows, we will do various visualizations of this dataset
# To keep the same layout, we first define a particular layout
# Here: Fruchterman-Reingold - force-directed layout algorithm
# We can then reuse this layout in subsequent plots
coords=layout_with_fr(g)

# We visualize our graph using these coordinates
plot(g,layout=coords,edge.width=2)

# Ok - so all actors have a discipline as well
# Not only given by an index number but also a letter
# S: Sociology
# A: Anthropology
# M: Mathematics
# O: Other
# Check the nodes (do note that I have integrated the attributes into the labels here)
V(g)

# However, would be nice to also have them stored as proper attributes, and for
# simplicity, having them coded as integer values
# Given the order above, I recode these in the following way:
# S=1, A=2, M=3, O=4

# We define a vector with these numbers (probably possible to do this automatically)
disccode <- c(1,2,4,1,4,4,4,2,2,1,1,1,2,1,1,3,4,1,1,1,3,1,1,1,1,1,3,1,1,2,4,2)
# What is the distribution among different scholars?
table(disccode)
# Clearly mostly Sociologists attending this conference, but also 6 Anthropologists,
# 6 mathematicians, and 6 others

# We use this vector as a new nodal attributes called 'disccode'.
g <- set_vertex_attr(g, "disccode",index=V(g), disccode)


# We can then plot the network again (on same layout) and now use this new 'disc' attribute for setting color
plot(g, vertex.color=V(g)$disccode, layout=coords)

# Make sure that each letter indeed corresponds to a particular color:
# I got:
# Sociologists: orange
# Anthropologists: blue
# Mathematicians: green
# Other: yellow

# Are nodes that share same attributes more likely to connect? Given this nominal
# attribute, we can use assortativity coefficient to measure this
# See Newman (2003), Mixing patterns in networks, Phys Rev E

assortativity_nominal(g, V(g)$disccode)
# 0.106

# Ok, slight assortative mixing but really not much

# In visualization there might indeed look like a lot of ties among sociologists,
# but bear in mind that they are quite many of them, so likely to be connnected
# by chance

#####
# Community detection
#####
# Let's focus on the relations now for a while, trying to cluster these scholars
# on the basis of their communities
# We use 4 heuristics for finding suitable partitions into communities
# As we plot all four, we prepare a panel for holding all four plots
par(mfrow=c(2,2))
# Also clear the Rstudio plot window!

# EDGE BETWEENNESS
# First, the classical one based on edge betweenness:
c_eb <- cluster_edge_betweenness(g)
# let's check some statistics on this partition
length(communities(c_eb))
modularity(c_eb)
# So, we find 12 communities, but with a really low modularity: 0.032

# let's plot it nevertheless
plot(c_eb, g, layout=coords, main="Edge betweenness (mod=0.032)")
# So evidently one large community and a lot of singleton communities

# LOUVAIN
# let's check Louvain: this works more on finding an optimal modularity
c_l <- cluster_louvain(g)
# let's check some statistics on this partition
length(communities(c_l))
modularity(c_l)
# We find 4 communities, and a higher modularity: 0.167
# We plot it:
plot(c_l, g, layout=coords, main="Louvain (mod=0.167)")
# Evidently the communities are of more equal size here

# FAST GREEDY
# Then we check the fast_greedy algorithm:
c_fg <- cluster_fast_greedy(g)
# let's check some statistics on this partition
length(communities(c_fg))
modularity(c_fg)
# This time I find 3 communities, with modularity of 0.161
plot(c_fg, g, layout=coords, main="Fast greedy (mod=0.161)")
# What is interesting here is that one of the communities that is discovered
# is almost exclusively sociologists
# Also interesting that what was identified as two distinct communities in
# Louvain above now are merged
# Note: nice to have same layout when comparing

# The final community detection goes through ALL possible partitions, selecting
# the most optimal (i.e. maximizing the modularity)
# This takes some time to run - and even more for larger networks
c_opt <- cluster_optimal(g)
# let's check some statistics on this partition
length(communities(c_opt))
modularity(c_opt)
# 4 communities, modularity of 0.167
# We plot it:
plot(c_opt, g, layout=coords, main="Optimal (mod=0.167)")
# Hm - this indeed looks similar to the one we got using Louvain (and same
# modularity). Is this the same partition?

# We check if the membership vectors of respective solutions are identical
all(c_opt$membership == c_l$membership)
# Yes they are! This means that the Louvain algorithm found the partition that
# maximizes the modularity (but then again: modularity might not be all)

# How do this 4-community structure relate to the disciplines? Whereas node
# colors now indicate community membership, we can add vertex shapes to indicate
# disciplines (in addition to the characters in their labels)
# we define these shapes first:
shapetypes=c("circle","square","sphere","sphere")
# Sociologists will be circles, anthropologists will be squares, mathematicians
# will be 3d spheres, and other scholars will have no shape at all
plot(c_l, g, layout=coords, vertex.shape=shapetypes[V(g)$disccode])

# Anthropologists indeed mostly together, which also contains a sociologists, a
# mathematician and another scholar
# Another group only consists of sociologists, plus one mathematician
# Third group good mixing: sociologists, mathematicians, others, anthropologist

# What if we do this with other level of dichotomization?
# Try repeating with this file:
# symmmin_dichGE3_eies_t1.txt

# Also: how many cliques are there?
max_cliques(g)
