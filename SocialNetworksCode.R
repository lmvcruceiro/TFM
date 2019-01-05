setwd("C:/TFM")
library(data.table)
library(igraph)

biciMadData <- fread("Data/BiciMad.csv")
biciMadData$unplug_hourTime <-as.Date(biciMadData$unplug_hourTime)  
biciMad <- biciMadData %>% filter(unplug_hourTime >= "2018-07-01")

biciMad <- biciMad[, -c(1, 2, 4, 6, 7, 8)]


biciMad <- biciMad %>% filter(idunplug_station <= 10, idplug_station <= 10)
biciMad <- biciMad[c(2,1)]

groups <- biciMad %>% group_by(idunplug_station, idplug_station) %>% summarise(Total = n())
joinData <- left_join(biciMad, groups, by = c("idunplug_station", "idplug_station"))
head(biciMad)

g <- graph_from_data_frame(biciMad, directed = TRUE)
E(g)$weight = joinData$Total

g <- simplify(g, remove.multiple = F, remove.loops = T, edge.attr.comb = list(weight = "sum", type = "ignore"))


deg <- degree(g, mode = "out")
hs <- hub_score(g, weights = NA)$vector
as <- authority_score(g, weights = NA)$vector

gSimple <- simplify(g, remove.multiple = T, remove.loops = T)

plot(gSimple, vertex.size = deg/(max(deg)/20) , edge.width=E(g)$weight/40, edge.arrow.mode = 5 )
plot(g, vertex.size = hs*20, edge.arrow.size = .05)
plot(g, vertex.size = as*20, edge.arrow.size = .05 )

summary(g)

Nn <- vcount(g) #número de nodos do grafo.
Ne <- ecount(g) #número de enlaces do grafo.


#collemos 10 nodos aleatoriamente:
inodes <- sample(1:Nn, size = 30)
g2 <- induced_subgraph(g, vids = inodes)
summary(g2)
plot(g2)
plot(g2, vertex.label = "")
plot(g2, vertex.label = "", vertex.size = 1)
ll <- layout.circle(g2)
plot(g2, layout = ll, vertex.label = "", vertex.size = 5, vertex.color = "blue")

cols <- sample(c("green", "blue", "red"))
plot(g2, layout = ll, vertex.label = "", vertex.size = 5, vertex.color = cols)


iedges <- sample(1:Ne, size = 100)
g3 <- subgraph.edges(g, eids = iedges, delete.vertices = T)

plot(g3, vertex.label = "", vertex.size = 5)


#calculamos as compoñentes conexas
ccs <- clusters(g)
ccs

imax <- which.max(ccs$csize)
imax

#que nodos pertencen á compoñente máis grande
inodes <- which(ccs$membership == imax)
inodes

#subgrafo da maior compoñente conexa
gmax <- induced_subgraph(g, vids=inodes)
summary(gmax)

is.connected(gmax)


deg <- degree(gmax, mode="out")
hs <- hub_score(gmax, weights = NA)$vector
as <- authority_score(gmax, weights = NA)$vector

png("degree2.png", width = 800, height = 800)
plot(gmax, vertex.size=deg/100)
dev.off()
