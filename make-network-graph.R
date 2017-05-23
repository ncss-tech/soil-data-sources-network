library(igraph)
library(plyr)
library(networkD3)
library(RColorBrewer)
library(scales)

ds <- read.csv('data-sources.csv', stringsAsFactors = FALSE)
dl <- read.csv('links.csv', stringsAsFactors = FALSE)

# check for missing entries
setdiff(unique(dl$from, dl$to), sort(unique(ds$name)))
setdiff(sort(unique(ds$name)), unique(dl$from, dl$to))

# make basic graph
g <- graph_from_data_frame(dl, directed = FALSE, vertices = ds)

g.com <- cluster_walktrap(g)
g.com.membership <- membership(g.com)

# set colors
g.com.length <- length(g.com)

if (g.com.length <= 9 & g.com.length > 2) 
  cols <- brewer.pal(n = g.com.length, name = "Set1")
if (g.com.length < 3) 
  cols <- brewer.pal(n = 3, name = "Set1")
if (g.com.length > 9) 
  cols <- colorRampPalette(brewer.pal(n = 9, name = "Set1"))(g.com.length)

cols.alpha <- alpha(cols, 0.65)

# assign styling to igrpah object
V(g)$color <- cols.alpha[membership(g.com)]
V(g)$label.family <- 'sans'
V(g)$label.font <- 2
V(g)$label.cex <- 0.8
V(g)$label.color='black'
V(g)$frame.color <- NA

# quick check
par(mar=c(1,1,1,1))
set.seed(1010)


# static version
png(file='example.png', width=1000, height=1000, type = 'cairo', antialias = 'subpixel')
plot(g)
dev.off()

## TODO: interactive version
# https://github.com/timelyportfolio/d3r/issues/3

# save to d3



