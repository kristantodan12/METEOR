library(networkD3)
library(dplyr)
library(igraph)
library(visNetwork)
library(rgl)
library(ggplot2)

rm(list = ls())

#######################################
###Visualization of aggregated steps###
#######################################
###Read data
dat <- read.csv(file = 'coding_steps.csv')
dat_prep1 <- select(dat, contains("Step"))
dat_prep1[dat_prep1 == ""] <- NA 
steps <- read.csv(file = 'all_steps.csv')

###Defining nodes
nodes <- steps
#nodes <- subset(nodes,Groups == "Structural_preprocessing" | Groups == "Functional_preprocessing" | Groups == "Noise_removal")
nodes$Groups.type <- as.factor(nodes[ ,c('Groups')])
nodes[sapply(nodes, is.factor)] <- data.matrix(nodes[sapply(nodes, is.factor)])
clrs <- c("coral3", "gold", "darkolivegreen2", "aquamarine3", "forestgreen")
nodes$size <- 0
for (nd in 1:nrow(nodes)){
  nd_p <- nodes$Names[nd]
  row_nd <- which(apply(dat, 1, function(x2) all(nd_p %in% x2)))
  nodes$size[[nd]] <- length(row_nd)
}


###Defining links
links <- expand.grid(source = nodes$Names, target = nodes$Names)
links$value <- 0
  
for (pp in 1:nrow(links)){
    st_l <- as.character(c(links$source[pp], links$target[pp]))
    row_l <- which(apply(dat, 1, function(x1) {
    idx_l <- match(st_l, x1)
    all(!is.na(idx_l)) && all(diff(idx_l) == 1)
  }))
  links$value[[pp]] <- length(row_l)
}


###Graph visualization
nodes2 <- subset(nodes,size > 0 )
nodes2$size <- nodes2$size/(max(nodes2$size)/28)
links2 <- subset(links,value > 0 )
links2$color <- "gray"
links2$color[links2$value>5] <- "blue"
links2$color[links2$value>20] <- "red"
links2$value <- links2$value/(max(links2$value)/10)
g <- graph_from_data_frame(d=links2, vertices=nodes2, directed=T)
V(g)$color <- clrs[V(g)$Groups.type]
V(g)$width <- nodes2$size
E(g)$width <- links2$value
E(g)$color <- links2$color
#tkid <- tkplot(g)
#cd <- tkplot.getcoords(tkid)
#saveRDS(cd, file = "coordinates_vis_steps2.RDS")
cdi <- readRDS("coordinates_vis_steps2.RDS")
#tk_set_coords(tkid, cdi)
dev.new(width=1000, height=700, unit="px")
saveWidget(visIgraph(g), file = "test.html")

###Using Graph Viz###
nodes2 <- nodes
links2 <- links
nodes2$size2 <- nodes2$size/(max(nodes2$size)/28)
links2$color <- "gray"
links2$color[links2$value>5] <- "blue"
links2$color[links2$value>20] <- "red"
links2$value2 <- links2$value/(max(links2$value)/10)

links3 <- data.frame(source = match(links2$source, nodes2$Names) - 1,
                     target = match(links2$target, nodes2$Names) - 1,
                     value = links2$value,
                     value2 = links2$value2)

g <- graph_from_data_frame(d=links2, vertices=nodes2, directed=T)
rEG <- as_graphnel(g)
plot(rEG)

##########################
###Not reported options###
##########################
dat_op_or <- read.csv(file = 'coding_options_or.csv')
dat_op_or[dat_op_or == ""] <- NA 
colnames(dat_op_or)[3] <- "F-Removal_Init_Vol" ### - becomes .
colnames(dat_op_or)[4] <- "F-Alignment/Head_motion_est"
colnames(dat_op_or)[5] <- "F-Spatial_normalization"
colnames(dat_op_or)[6] <- "F-Spatial_smooth"
colnames(dat_op_or)[7] <- "F-Temporal_detrending"
colnames(dat_op_or)[8] <- "F-motion_regression"
colnames(dat_op_or)[9] <- "F-Temporal_filt"
for (opc in 2:ncol(dat_op_or)){
  for (opr in 1:nrow(dat_op_or)){
    opt <- dat_op_or[opr,opc]
    if (is.na(opt)){
      pap <- dat[opr,]
      if (any(pap==colnames(dat_op_or[opc]))){
        dat_op_or[opr,opc] <- "Not reported" ## Step used but option not reproted
      }
      else{
        dat_op_or[opr,opc] <- "Not used"  ##step not used
      }
    }
  }
}
dat_op_or$Software[dat_op_or$Software == "Not used"] <- "Not reported"
st_w_op <- colnames(dat_op_or)
nr_mat <- data.frame(Not_Reported = numeric(),
                     Not_Used = numeric(),
                     Used = numeric())
for (st_op in 2:length(st_w_op)){
  cl <- dat_op_or[st_op]
  nr_mat[st_op,1] <- length(which(cl == "Not reported"))
  nr_mat[st_op,2] <- length(which(cl == "Not used"))
  nr_mat[st_op,3] <- 206 - nr_mat[st_op,1] - nr_mat[st_op,2]
}
nr_mat <- nr_mat[-1,]
steps_op <- c(rep(st_w_op[-1],3))
report <- c(rep("Step used option reported",17), rep("Step not used",17), rep("Step used option not reported",17))
values_rep <- c(nr_mat[,3], nr_mat[,2], nr_mat[,1])
nr_mat2 <- data.frame(steps_op,report,values_rep)
ggplot(nr_mat2, aes(fill=report, y=steps_op, x=values_rep)) + 
  geom_bar(position="stack", stat="identity") +
  scale_fill_manual(values=c('gray', 'dark red', 'dark green')) +
  ggtitle("Variability on reporting options")


####################
###Most debatable###
####################
st_deb <- nodes$Names[which(nodes$size>60 & nodes$size<120)]

###############################
###Graph measure; Centrality###
###############################
nodes3 <- nodes
links3 <- links
links3$value[links3$value>0] <- 1
links3 <- subset(links3, value>0)
g_m <- graph_from_data_frame(d=links3, vertices=nodes3, directed=T)
A <- as_adjacency_matrix(g_m, attr="value")
deg <- degree(g_m)
nodes3$degree <- deg
ggplot(nodes3, aes(y=Names, x=degree)) +
  geom_bar(stat = "identity") +
  ggtitle("Degree of each step")


nodes4 <- nodes
links4 <- links
links4 <- subset(links4, value>0)
g_m2 <- graph_from_data_frame(d=links4, vertices=nodes4, directed=T)
bc_n <- betweenness(g_m2, directed = TRUE)
nodes4$bc_n <- bc_n
bc_l <- edge_betweenness(g_m2, directed = TRUE)
links4$bc_l <- bc_l
ggplot(nodes4, aes(y=Names, x=bc_n)) +
  geom_bar(stat = "identity") +
  ggtitle("Degree of each step")
links4_2 <- links4[order(links4$bc_l),]
