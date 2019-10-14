# SNA Class
# Lab 1

######################################################################################
#
# Intro (DON'T INCLUDE IN REPORT)
#
######################################################################################

# Lines that start with a hashtag/pound symbol, like this one, are comment lines.
# Comment lines are ignored by R when it is running the code.

# To run a non-commented line in RStudio, click the "Run" button above
# or press Ctrl+Enter on Windows and Cmd+Enter on macOS.
# Any output will be printed in the Console pane (placed below) and all plots
# will be displayed in the Plots pane (placed bottom right).
# Try running the lines below and observing what happens:

print("Hello, R!")
plot(1:10)

# Complete the lab work by following the instructions and running the provided code.
# You may be required to edit some lines before running them to achieve the desired result.

######################################################################################
#
# Import the necessary libraries (DON'T INCLUDE IN REPORT)
#
######################################################################################

# First time you run this file, you will need to install several packages.
# To do that, run code lines 36-40. It may take up a copule of minutes.
# You only need to install packages once, next time you should skip those lines.

if (!"vosonSML" %in% installed.packages()) install.packages('vosonSML') ## this package is a social media data collection tool
if (!"magrittr" %in% installed.packages()) install.packages("magrittr") ## this package allows you to use so-called pipe (%>%)
if (!"igraph" %in% installed.packages()) install.packages("igraph") ## this package is a network analysis tool
if (!"sna" %in% install.packages()) install.packages("sna") ## this package is another popular network analysis tool
if (!"network" %in% install.packages()) install.packages("network") ## this package supplies additional methods for 'sna'

# Now run the lines below to load the packages you have installed.
# You need to load packages every time you run the script or restart R.
library(magrittr)
library(igraph)
library(vosonSML)

# To check whether your R loads these packages, run te following code
sessionInfo() ## check other attached packages. If magrittr, vosonSML, & igraph are listed there, you're ready!


######################################################################################
#
# Set current directory (DON'T INCLUDE IN REPORT)
#
######################################################################################

# In this step you tell R where to look for your files.
# From the menu, select "Session > Set Working Directory... > To Source File Location".

# Alternatively, if you know the filename, you can uncomment the line below and run it.
# setwd("replace this with path to your directory")

######################################################################################
#
# Part I: Network Data Collection from Social Media
#
######################################################################################

# Pick ONE of the three platforms to collect your own data
# Criteria of your network data:
# 1. Include 100-1000 actors in the giant component of your network
# (Don't overkill to include 1 million actors for this lab)
# 2. Pick a (seemingly) controvertial topic
# (e.g., topic = MeToo Movement, hashtags = c(#metoomovement))


# The process of authentication, data collection and creating social network in 
# vosonSML is expressed with the three verb functions: Authenticate, Collect and Create.

#########################
# Twitter data collection
#########################

# apiKey - Replace with your API key
# apiSecret - Replace with your API secret
# accesToken - Replace with your access token
# accessTokenSecret - Replace with your access token secret
myKeys <- list(appName = "My App", apiKey = "Kkixx41by5hzJBwYovzntv5VZ", apiSecret = "AoSUuRNsl3RtFv8ogwNLD1QPudzELEihDq8K1KqImdHwj0CoEv", 
               accessToken = "883941173018099712-Up0W8pFVL3o6H87ray8K3fbmwYZaSHD", accessTokenSecret = "1A2dSWTcajk9NsrrAAy1e9KMeeN8CM7Tyy4uXALuBfvMt")

twitterAuth <- Authenticate("twitter", appName = myKeys$appName, apiKey = myKeys$apiKey, 
                            apiSecret = myKeys$apiSecret, accessToken = myKeys$accessToken,
                            accessTokenSecret = myKeys$accessTokenSecret)

# twitter authentication creates an access token as part of the auth object
# this can and should be re-used by saving it and then loading it for future bsessions
# save the auth object after authenticate 
saveRDS(twitterAuth, file = "~/.twitter_auth")

# load a previously saved auth object for use in collect
twitterAuth <- readRDS("~/.twitter_auth")

# searchTerm - Replace with your search terms such as c('#prolife','#prochoice')
# collect 500 recent tweets
twittersearchTerm <- c('#metoomovement')
twitterData <- twitterAuth %>%
  Collect(searchTerm = twittersearchTerm, searchType = "recent", numTweets = 100, 
          includeRetweets = TRUE , retryOnRateLimit = TRUE, writeToFile = FALSE, 
          verbose = TRUE)
View(twitterData)

## actor network - nodes are users who have tweeted
## edges are based on either retweets or @mention (so-called reply)
actorNetwork <- twitterData %>% Create("actor")
actorGraph <- actorNetwork$graph  # igraph network graph


# optional step to add additional twitter user profile info to actor network graph as
# node attributes
# actorGraphWithUserAttr <- actorNetwork %>% 
#   AddUserData(twitterData, 
#               lookupUsers = TRUE,
#               twitterAuth = twitterAuth)

#########################
# Youtube data collection
#########################

# YoutubeAPIKey - Replace with your API key
myYoutubeAPIKey <- "AIzaSyBKSk5aAPM2aXc4yKzn4zH4cjE-tunrxzc"

# helper to create a list of youtube video ids from urls
# Replace with your list of youtube video urls
myYoutubeVideoIds <- GetYoutubeVideoIDs(c("https://www.youtube.com/watch?v=ATYK2svJ6eM"))

# authenticate and collect 100 top-level comments per youtube video in list
# also collects reply-comments for each top-level comment
youtubeData <- Authenticate("youtube", apiKey = myYoutubeAPIKey) %>%
  Collect(videoIDs = myYoutubeVideoIds, maxComments = 100,verbose = TRUE)
View(youtubeData)


## actor network - nodes are users who have posted comments
actorGraph <- youtubeData %>% Create("actor") %>% .$graph

########################
# Reddit data collection
########################

# collect reddit comment threads
# Replace with your list of reddit comment thread urls
myThreadUrls <- c("https://www.reddit.com/r/AskReddit/comments/7sk2ox/what_are_your_views_on_the_metoo_movement/")

# authentication does not require credentials
redditData <- Authenticate("reddit") %>%
  Collect(threadUrls = myThreadUrls, waitTime = 5)
View(redditData)


## actor network - nodes are users who have posted comments
# create an actor network with comment text as edge attribute
actorGraph <- redditData %>% Create("actor") %>% .$graph


########################
# Clean up the data
########################

## clean up the graph data removing self-loop 
edge_cleanup <- function(graph = actorGraph){
  library(igraph)
  df <- get.data.frame(actorGraph)
  names_list <- data.frame('name' = as.character(V(actorGraph)$name),
                           'label' = as.character(V(actorGraph)$label))
  df$from <- sapply(df$from, function(x) names_list$label[match(x,names_list$name)] %>% as.character())
  df$to <- sapply(df$to, function(x) names_list$label[match(x,names_list$name)] %>% as.character())
  nodes <- data.frame(sort(unique(c(df$from,df$to))))
  links <- df[,c('from','to')]
  net <- graph.data.frame(links, nodes, directed=T)
  E(net)$weight <- 1
  net <- igraph::simplify(net,edge.attr.comb="sum")
  return(net)
}

actorGraph <- edge_cleanup()

# check if the network is directed or undirected
is.directed(actorGraph)

########################
# Check the criterion
########################
# check the size of the network
vcount(actorGraph) ## the number of nodes/actors/users
ecount(actorGraph) ## the number of edges

# calculate the density of the network
graph.density(actorGraph)

########################
# Save your data    #qu it doesnt work
########################

# The following command saves your R environment as RData
save.image('Lab1_SocialMedia.RData')

# Next time, you'll work on the same data
# Run the following command
# Make sure that you put the RData in your working directory
load('Lab1_SocialMedia.RData')

######################################################################################
#
# Part II: Network Visualization
#
######################################################################################

# Plot a graph
actorGraph %>% 
  plot(.,
       # layout = layout_with_fr(.),
       edge.arrow.size = .3,
       vertex.size = 7,
       vertex.color = 'red',
       # vertex.label = NA, ## uncomment here if you don't want to display labels
       vertex.label.cex = .5,
       vertex.label.color = 'black')

# Calculate the number of components in the graph
comp <- components(actorGraph)
comp

# Take out a giant component from the graph
giantGraph <- actorGraph %>% 
  induced.subgraph(., which(comp$membership == which.max(comp$csize)))
vcount(giantGraph) ## the number of nodes/actors/users
ecount(giantGraph) ## the number of edges

# Plot a graph of giant component. (.) in layout gets the last data
giantGraph %>% 
  plot(.,
       #layout = layout_with_fr(.),
       edge.arrow.size = .3,
       vertex.size = 4,
       vertex.color = 'red',
       vertex.label.cex = .5,
       vertex.label.color = 'black')


# Experiment with other layouts by replacing with your choice of layout, then running a plot function above.
# layout = layout_with_...(.)
# For that, you can pick one of the options below.
# layout_with_dh(.) # Davidson and Harel
# layout_with_drl(.) # Force-directed
# layout_with_kk(.) # Spring

# You can play with the vertex sizes and/or edge sizes to get a better vizualization for the graphs above

######################################################################################
#
# Part III: Individual Network Properties
#
######################################################################################

# For this part, you switch 'igraph' to 'sna' package because we are going to use 
# some functions that only are available in sna package
# As a first step, create a 'sna' graph object from an 'igraph' object
sna_g <- igraph::get.adjacency(giantGraph, sparse=FALSE) %>% network::as.network.matrix()

# this detaching is a necessary step since the two packages have some same function names
# R is often confuesed
detach('package:igraph')
library(sna)
library(network)

# Compute centralities based on 'network' package
# Calculate in-degree centrality
degree(sna_g, cmode = 'indegree')
# Store the information
centralities <- data.frame('node_name' = as.character(network.vertex.names(sna_g)),
                           'in_degree' = degree(sna_g, cmode = 'indegree'))

# Calculate out-degree centrality and store it in the data.frame called 'centralities'
centralities$out_degree <- degree(sna_g, cmode = 'outdegree')

# Calculate betweenness centrality and store it in the data.frame called 'centralities'
centralities$betweenness <- betweenness(sna_g)

# Calculate closeness centrality and store it in the data.frame called 'centralities'
centralities$incloseness <- igraph::closeness(giantGraph, mode = 'in')
centralities$outcloseness <- igraph::closeness(giantGraph, mode = 'out')

# Calculate eigenvector centrality and store it in the data.frame called 'centralities'
centralities$eigen <- evcent(sna_g)

# Calculate Burt's network constraint and store it in the data.frame called 'centralities'
# using 'igraph' because 'sna' doesn't have the function
centralities$netconstraint <- igraph::constraint(giantGraph)

# Calculate authority and store it in the data.frame called 'centralities'
# using 'igraph' because 'sna' doesn't have the function
# 'igraph::' allows calling for any igraph function without loading the package
centralities$authority <- igraph::authority_score(giantGraph, scale = TRUE)$`vector`

# Calculate hub and store it in the data.frame called 'centralities'
# using 'igraph' because 'sna' doesn't have the function
centralities$hub <- igraph::hub_score(giantGraph, scale = TRUE)$`vector`

View(centralities)

# For creating the table in part III you can use the following, replace eigen with other centrality measures that you want
sort(centralities$eigen,decreasing=TRUE)[1:5]
sort(centralities$authority,decreasing=TRUE)[1:5]

# Produce a target diagram, centering by one of your choice of centralities (e.g., betweenness)
# WARNING - below might take some time to produce it 
# if you're running other tasks on your computer, make sure that your CPU has some space to run this plot
sna::gplot.target(sna_g, centralities$betweenness,
                  displaylabels = TRUE, ## if you don't want to see labels, replace TRUE with FALSE
                  label.cex=.5,
                  main="Betweenness",
                  circ.lab = TRUE, 
                  circ.col="skyblue",
                  usearrows = TRUE,
                  # vertex.col=c("blue", rep("red", 32), "yellow"),
                  edge.col="darkgray")

######################################################################################
#
# Part IV: Global Network Proporties
#
######################################################################################
# If you want to go back to igraph analysis, don't forget detaching 'sna' and 'network' first
# before recalling 'igraph'
#  go back to 'igraph'
detach('package:sna', unload = TRUE)
detach('package:network', unload = TRUE)
library(igraph)

kcore <- giantGraph %>% graph.coreness(.)
kcore

giantGraph %>% 
  plot(.,
       layout = layout_with_gem(.),
       # layout = layout_with_sugiyama(.),
       edge.arrow.size = .3,
       vertex.size = 4,
       vertex.color = adjustcolor(graph.coreness(.), alpha.f = .3),
       vertex.label.cex = .5,
       vertex.label.color = 'black',
       mark.groups = by(seq_along(graph.coreness(.)), graph.coreness(.), invisible),
       mark.shape = 1/4,
       mark.col = rainbow(length(unique(graph.coreness(.))),alpha = .1),
       mark.border = NA
  )

# Community detection: Plot the number of clusters in the graph and their size
# there are also other algorithms for this you may want to explore
# below is using Newman-Girvan Algorithm (2003)
# if communities do not make sense to you, replace with your choice
# e.g., cluster_infomap, cluster_walktrap etc.
cluster <- giantGraph %>% cluster_edge_betweenness()
cluster

# modularity measure
modularity(cluster)

# Find the number of clusters
membership(cluster)   # affiliation list
length(cluster) # number of clusters

# Find the size the each cluster 
# Note that communities with one node are isolates, or have only a single tie
sizes(cluster) 

# Visualize clusters - that puts colored blobs around the nodes in the same community.
# You may want to remove vertex.label=NA to figure out what terms are clustered.
cluster %>% plot(.,giantGraph,
                 # layout = layout_with_gem(.),
                 layout = layout_with_fr(giantGraph),
                 edge.arrow.size = .3,
                 vertex.size = 4,
                 vertex.color = adjustcolor(membership(.), alpha.f = .3),
                 vertex.label.cex = .5,
                 vertex.label.color = 'black',
                 mark.groups = by(seq_along(membership(.)), membership(.), invisible),
                 mark.shape = 1/4,
                 mark.col = rainbow(length(.),alpha = .1),
                 mark.border = NA
)


# Examine the in-degree distribution
giantGraph %>% degree.distribution(.,mode="in") %>% 
  plot(., col = 'black', pch = 19, cex = 1.5,
       main = 'In-degree Distribution',
       ylab = 'Density',
       xlab = 'In-degree')
# CCDF - Complementary Cumulative Distribution Function
# Plot a log-log plot of in-degree distribution
giantGraph %>% 
  degree.distribution(.,cumulative = TRUE,mode ='in') %>% 
  plot(1:(max(degree(giantGraph,mode='in'))+1),., ## since log doesn't take 0, add 1 to every degree
       log='xy', type = 'l',
       main = 'Log-Log Plot of In-degree',
       ylab = 'CCDF',
       xlab = 'In-degree')
# Fit a power law to the degree distribution
# The output of the power.law.fit() function tells us what the exponent of the power law is ($alpha)
# and the log-likelihood of the parameters used to fit the power law distribution ($logLik)
# Also, it performs a Kolmogov-Smirnov test to test whether the given degree distribution could have
# been drawn from the fitted power law distribution.
# The function thus gives us the test statistic ($KS.stat) and p-vaule ($KS.p) for that test
in_power <- giantGraph %>% 
  degree.distribution(., mode='in') %>%
  power.law.fit(.)
in_power

# Examine the out-degree distribution
giantGraph %>% degree.distribution(.,mode="out") %>% 
  plot(., col = 'black', pch = 19, cex = 1.5,
       main = 'Out-degree Distribution',
       ylab = 'Density',
       xlab = 'Out-degree')
# Plot a log-log plot
giantGraph %>% 
  degree.distribution(.,cumulative = TRUE,mode ='out') %>% 
  plot(1:(max(degree(giantGraph,mode='out'))+1), ## since log doesn't take 0, add 1 to every degree
       ., log='xy', type = 'l',
       main = 'Log-Log Plot of Out-degree',
       ylab = 'CCDF',
       xlab = 'Out-degree')
# Fit a power law to the degree distribution
out_power <- giantGraph %>% 
  degree.distribution(., mode='out') %>%
  power.law.fit(.)


# Small-world Characteristics
ntrials <- 1000 ## set a value for the repetition
cl.rg <- numeric(ntrials) ## create an estimated value holder for clustering coefficient
apl.rg <- numeric(ntrials) ## create an estimated value holder for average path length
for (i in (1:ntrials)) {
  g.rg <- rewire(giantGraph, keeping_degseq(niter = 100))
  cl.rg[i] <- transitivity(g.rg, type = 'average')
  apl.rg[i] <- average.path.length(g.rg)
}

# plot a histogram of simulated values for clustering coefficient + the observed value
hist(cl.rg,
     main = 'Histogram of Clustering Coefficient',
     xlab = 'Clustering Coefficient')
par(xpd = FALSE)
# the line indicates the mean value of clustering coefficient for your network
abline(v = giantGraph %>% transitivity(., type = 'average'), col = 'red', lty = 2)
# this tests whether the observed value is statistically different from the simulated distribution
t.test(cl.rg, mu=giantGraph %>% transitivity(., type = 'average'),
       alternative = 'less') ##pick either 'less' or 'greater' based on your results

# plot a histogram of simulated values for average path length + the observed value
hist(apl.rg,
     main = 'Histogram of Average Path Length',
     xlab = 'Average Path Length')
# the line indicates the mean value of average path length for your network
abline(v = giantGraph %>% average.path.length(), col = 'red', lty = 2)
# this tests whether the observed value is statistically different from the simulated distribution
t.test(apl.rg, mu=giantGraph %>% average.path.length(.),
       alternative = 'greater') ##pick either 'less' or 'greater' based on your results
