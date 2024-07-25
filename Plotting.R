library(networkD3)
library(dplyr)
library(igraph)
library(visNetwork)
library(geomnet)
library(rgl)
library(ggplot2)
library(plotly)
library(extrafont)
library(qgraph)
library(forcats)
library(readxl)
font_import()
loadfonts(device="win")

rm(list = ls())


#############################
#########ReadData############
#############################
dat <- read_excel("Database.xlsx", sheet = "Coding_steps")
#Put Software as the first step of all papers
# Find rows where Step1 is not "Software"
rows_to_change <- dat$Step1 != "Software"

# For each row to change, shift values to the right and set Step1 to "Software"
for (i in which(rows_to_change)) {
  n <- ncol(dat)
  dat[i, 2:n] <- c("Software", dat[i, 2:(n-1)])
}
dat_prep1 <- select(dat, contains("Step"))
dat_prep1[dat_prep1 == ""] <- NA 
steps <- read_excel("Database.xlsx", sheet = "All_steps")
dat_op_or <- read_excel("Database.xlsx", sheet = "Coding_options_or")
dat_op_or[dat_op_or == ""] <- NA
dat_op <- read_excel("Database.xlsx", sheet = "Coding_options")
dat_prep_op1 <- select(dat_op, contains("Choice"))
dat_prep_op1[dat_prep_op1 == ""] <- NA 
steps_op <- read_excel("Database.xlsx", sheet = "All_options")
p_inf <- read_excel("Database.xlsx", sheet = "Paper_info")
cn_p_inf <- colnames(p_inf)


############################
###Visualization of steps###
############################
###Single paper visualization
# list_df <- list()
# for (p in 1:length(dat$Key)){
#   ppr <- dat_prep1[p, ]
#   dat_prep <- ppr
#   source <- c(dat_prep$Step1, dat_prep$Step2, dat_prep$Step3, dat_prep$Step4, dat_prep$Step5,
#               dat_prep$Step6, dat_prep$Step7, dat_prep$Step8, dat_prep$Step9, dat_prep$Step10,
#               dat_prep$Step11, dat_prep$Step12, dat_prep$Step13, dat_prep$Step14, dat_prep$Step15,
#               dat_prep$Step16, dat_prep$Step17, dat_prep$Step18, dat_prep$Step19, dat_prep$Step20,
#               dat_prep$Step21, dat_prep$Step22, dat_prep$Step23, dat_prep$Step24, dat_prep$Step25,
#               dat_prep$Step26, dat_prep$Step27, dat_prep$Step28, dat_prep$Step29)
#   target <- c(dat_prep$Step2, dat_prep$Step3, dat_prep$Step4, dat_prep$Step5,
#               dat_prep$Step6, dat_prep$Step7, dat_prep$Step8, dat_prep$Step9, dat_prep$Step10,
#               dat_prep$Step11, dat_prep$Step12, dat_prep$Step13, dat_prep$Step14, dat_prep$Step15,
#               dat_prep$Step16, dat_prep$Step17, dat_prep$Step18, dat_prep$Step19, dat_prep$Step20,
#               dat_prep$Step21, dat_prep$Step22, dat_prep$Step23, dat_prep$Step24, dat_prep$Step25,
#               dat_prep$Step26, dat_prep$Step27, dat_prep$Step28, dat_prep$Step29, dat_prep$Step30)
#   dat2 <- data.frame(table(source, target))
#   value <- c()
#   for (i in 1:length(source)){
#     s <- source[i]
#     if (is.na(s)){
#       value[i] <- 0
#     }
#     else {
#       t <- target[i]
#       if (is.na(t)){
#         value[i] <- 0
#       }
#       else {
#         ind <- which(dat2$source %in% s & dat2$target %in% t)
#         value[i] <- dat2$Freq[ind]
#       }
#     }
#   }
  
#   links <- data.frame(source = source,
#                       target = target,
#                       value  = value)
#   links <- na.omit(links)
#   links <- unique(links)
  
#   nodes <- steps
#   nodes$Groups.type <- as.factor(nodes$Groups)
#   nodes[sapply(nodes, is.factor)] <- data.matrix(nodes[sapply(nodes, is.factor)])
  
#   links$IDsource <- match(links$source, nodes$Names) - 1
#   links$IDtarget <- match(links$target, nodes$Names) - 1
#   links <- na.omit(links)
#   list_df[[p]] <- links
# }

# nodes <- steps
# nodes$ID <- c(1:nrow(steps))
# nodes$Groups.type <- as.factor(nodes$Groups)
# nodes[sapply(nodes, is.factor)] <- data.matrix(nodes[sapply(nodes, is.factor)])
# clrs <- c("mediumpurple3", "darkolivegreen3", "firebrick", "darkorange", "dodgerblue")
# nodes$col <- clrs[nodes$Groups.type]
# nodes$ID <- c(1:nrow(steps))

# sel_p <- 1
# id_p <- which(dat$Key == sel_p)
# links_ind <- list_df[[id_p]]
# g <- graph_from_data_frame(d=links_ind, vertices=nodes, directed=T)
# first_nd <- links_ind$source[1]
# id_fn <- which(nodes$Names == first_nd)
# V(g)$color <- nodes$col
# V(g)$color[id_fn] <- "black"
# tkid <- tkplot(g)
# cd <- tkplot.getcoords(tkid)
# saveRDS(cd, file = "coordinates_steps_aggregated.RDS")
# cdi <- readRDS("coordinates_steps_shiny3.RDS")
# tk_set_coords(tkid, cdi)

# plot(g, 
#      layout = cdi,
#      vertex.frame.color = "black",                 # Node border color
#      vertex.shape="circle",                        # One of “none”, “circle”, “square”, “csquare”, “rectangle” “crectangle”, “vrectangle”, “pie”, “raster”, or “sphere”
#      vertex.size=10,                               # Size of the node (default is 15)
     
#      # === vertex label
#      vertex.label.color="black",
#      vertex.label.font=2,                          # Font: 1 plain, 2 bold, 3, italic, 4 bold italic, 5 symbol
#      vertex.label.cex=0.7,                           # Font size (multiplication factor, device-dependent)
#      vertex.label.dist=0,                          # Distance between the label and the vertex
#      vertex.label.degree=0 ,                       # The position of the label in relation to the vertex (use pi)
     
#      # === Edge
#      edge.color="blue",                            # Edge color
#      edge.width=1,                                 # Edge width, defaults to 1
#      edge.arrow.size=0.6,                            # Arrow size, defaults to 1
#      edge.arrow.width=3,                           # Arrow width, defaults to 1
#      edge.lty="solid",                             # Line type, could be 0 or “blank”, 1 or “solid”, 2 or “dashed”, 3 or “dotted”, 4 or “dotdash”, 5 or “longdash”, 6 or “twodash”
#      edge.curved=0.3    ,                          # Edge curvature, range 0-1 (FALSE sets it to 0, TRUE to 0.5)
#      asp = 0.7
# )

# legend(x=-1, y=-1.1, c("FC definition","Functional_preprocessing", "Graph_analysis", "Noise_removal", "Structural_preprocessing"), pch=21,
#        col="#777777", pt.bg=clrs, pt.cex=2, cex=1, bty="n", ncol=1)


###Aggregated Graph visualization
###Defining nodes
nodes <- steps
# Get the indices of the rows in p_inf where Year is between 2010 and 2017 #uncomment for subset of graph
indices <- which(p_inf$Year %in% 2018:2023)
dat <- dat[indices, ]
#nodes <- subset(nodes,Groups == "Structural_preprocessing" | Groups == "Functional_preprocessing" | Groups == "Noise_removal")
nodes$Groups.type <- as.factor(nodes$Groups)
nodes[sapply(nodes, is.factor)] <- data.matrix(nodes[sapply(nodes, is.factor)])
clrs <-  c("mediumpurple3", "darkolivegreen3", "firebrick", "darkorange", "gray","dodgerblue")
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

nodes2 <- nodes
nodes2$size <- nodes2$size/(max(nodes2$size)/15)
links2 <- subset(links,value > 15) #16 for 2010 to 2017
links2$color <- "gray"
links2$color[links2$value>5] <- "blue"
links2$color[links2$value>20] <- "red"
links2$value <- links2$value/(max(links2$value)/5)
g <- graph_from_data_frame(d=links2, vertices=nodes2, directed=T)
V(g)$color <- clrs[V(g)$Groups.type]
V(g)$width <- (nodes2$size)
V(g)$label <- nodes2$Names_vis
E(g)$width <- (links2$value)
E(g)$color <- links2$color
# Get the indices of the isolated nodes
isolated_nodes <- which(degree(g) == 0)
# Remove isolated nodes from the graph
g <- delete.vertices(g, isolated_nodes) #uncomment for subset of graph
#tkid <- tkplot(g)
#cd <- tkplot.getcoords(tkid)
#saveRDS(cd, file = "coordinates_steps_aggregated3.RDS")
cdi <- readRDS("coordinates_steps_aggregated3.RDS")
# Remove corresponding rows from the layout
cdi <- cdi[-isolated_nodes, ] #uncomment for subset of graph
#cdi <- cdi[1:nrow(cdi)-1, ]
#tk_set_coords(tkid, cdi)
# Create the plot
pdf("network_plot.pdf")
plot(g, 
     layout = cdi,
     vertex.label.color="black",
     vertex.label.font=1,
     vertex.label.cex=0.7, 
     vertex.label.dist=0,
     vertex.label.degree=0,
     edge.arrow.size=0.7,
     edge.arrow.width=1,
     edge.lty="solid",
     edge.curved=0.3,
     asp = 0.7
)
# Close the PDF file
dev.off()
# plot(g, 
#      layout = cdi,
#      #vertex.frame.color = "black",                 # Node border color
#      #vertex.shape="circle",                        # One of “none”, “circle”, “square”, “csquare”, “rectangle” “crectangle”, “vrectangle”, “pie”, “raster”, or “sphere”
#      #vertex.size=10,                               # Size of the node (default is 15)
     
#      #vertex label
#      vertex.label.color="black",
#      vertex.label.font=2,                          # Font: 1 plain, 2 bold, 3, italic, 4 bold italic, 5 symbol
#      vertex.label.cex=0.95,                           # Font size (multiplication factor, device-dependent)
#      vertex.label.dist=0,                          # Distance between the label and the vertex
#      vertex.label.degree=0 ,                       # The position of the label in relation to the vertex (use pi)
     
#      #Edge
#      #edge.color="blue",                            # Edge color
#      #edge.width=1,                                 # Edge width, defaults to 1
#      edge.arrow.size=1,                            # Arrow size, defaults to 1
#      edge.arrow.width=1,                           # Arrow width, defaults to 1
#      edge.lty="solid",                             # Line type, could be 0 or “blank”, 1 or “solid”, 2 or “dashed”, 3 or “dotted”, 4 or “dotdash”, 5 or “longdash”, 6 or “twodash”
#      edge.curved=0.3    ,                          # Edge curvature, range 0-1 (FALSE sets it to 0, TRUE to 0.5)
#      asp = 0.7
# )




###Checking duplicated rows
dat_without_key <- dat[, !(names(dat) %in% "Key")]
duplicated_rows <- dat[duplicated(dat_without_key) | duplicated(dat_without_key, fromLast = TRUE), ]
print(duplicated_rows)

#saveWidget(visIgraph(g, layout = "layout_nicely"), file = "test.html")




nodes2 <- nodes
links2 <- links
nodes2$size2 <- nodes2$size/(max(nodes2$size)/28)
nodes2$ID <- c(1:62)
links2$color <- "gray"
links2$color[links2$value>5] <- "blue"
links2$color[links2$value>20] <- "red"
links2$value2 <- links2$value/(max(links2$value)/10)

links3 <- data.frame(source = match(links2$source, nodes2$Names) - 1,
                     target = match(links2$target, nodes2$Names) - 1,
                     value = links2$value,
                     value2 = links2$value2)

###Using networkD3###
fn <- forceNetwork(Links = links3, Nodes = nodes2,
                  Source = "source", Target = "target",
                  Value = "value", NodeID = "ID", linkWidth = JS("function(d) { return Math.sqrt(d.value)/3; }"),
                  Nodesize = "size2", Group = "Groups", radiusCalculation = JS("Math.sqrt(d.nodesize)+6"), linkDistance = 130,
                  opacity = 0.9, zoom = TRUE, opacityNoHover = 1, fontSize = 12, arrows = TRUE, legend = TRUE, bounded = TRUE, 
                  colourScale = JS('d3.scaleOrdinal().domain(["Structural_preprocessing", "Functional_preprocessing", "Noise_removal", "FC_def", "Graph_analysis"]).
                                  range(["#1f77b4", "#2ca02c", "#ff7f0e", "#9467bd", "#e31a1c"])'))


saveWidget(fn, "forcenet.html")

###Using visnetwork###
node_vis <- nodes2
link_vis <- links3

node_vis$id <- match(node_vis$Names, nodes2$Names) - 1
colnames(node_vis)[1] <- "label"
colnames(node_vis)[2] <- "group"
colnames(node_vis)[6] <- "value"
clrs <- c("coral3", "gold", "darkolivegreen2", "aquamarine3", "forestgreen")
node_vis$color <- clrs[nodes2$Groups.type]

colnames(link_vis)[1] <- "from"
colnames(link_vis)[2] <- "to"
colnames(link_vis)[3] <- "label"
link_vis$arrows <- c("to")

visNetwork(node_vis, link_vis, width = "100%")


###Using Graph Viz###
g <- graph_from_data_frame(d=links2, vertices=nodes2, directed=T)
rEG <- as_graphnel(g)
plot(rEG)

###Using qgraph###
nodes_q <- nodes
links_q <- links
gr <- list(c(1:10), c(11:30), c(31:50), c(51:56), c(57:62))
names(gr) <- c('Structural preprocessing', 'Functional preprocessing', 'Noise removal', 'FC definition', 'Graph analysis')
colors <- c("mediumpurple3", "darkolivegreen3", "firebrick", "darkorange", "dodgerblue")
#colors_or <- c("dodgerblue", "darkolivegreen3", "darkorange", "mediumpurple3", "firebrick")
#colors <- c("dodgerblue", "darkorange", "firebrick", "darkolivegreen3", "mediumpurple3")
nodes_q$col <- colors[nodes_q$Groups.type]
graph <- graph.data.frame(links_q, directed = TRUE)
adj_matrix <- as_adjacency_matrix(graph, attr = "value", sparse = FALSE)
network_plot <- qgraph(
  adj_matrix,
  labels = nodes_q$Names,
  #layout = "spring",
  groups = gr,
  legend = TRUE,
  borders = FALSE,
  cut = 0.4,
  vsize = 4
  #edge.color = "black",
  #edge.width = links_q$value
  #color = nodes_q$col
)

##############################
###Visualization of optioms###
##############################
###Single paper visualization
for (opc in 2:ncol(dat_op_or)){
  for (opr in 1:nrow(dat_op_or)){
    opt <- dat_op_or[opr,opc]
    if (is.na(opt)){
      pap <- dat[opr,]
      if (colnames(dat_op_or)[opc] %in% pap){
        dat_op_or[opr,opc] <- "Not_reported"
      }
      else{
        dat_op_or[opr,opc] <- "Not_used"
      }
    }
  }
} 

cn_dat_op <- colnames(dat_op_or)


###Listing paper
list_df_op <- list()
for (p in 1:length(dat_op$Key)){
  ppr <- dat_prep_op1[p, ]
  dat_prep_op <- ppr
  source_op <- c(dat_prep_op$Choice1, dat_prep_op$Choice2, dat_prep_op$Choice3, dat_prep_op$Choice4, dat_prep_op$Choice5,
                 dat_prep_op$Choice6, dat_prep_op$Choice7, dat_prep_op$Choice8, dat_prep_op$Choice9, dat_prep_op$Choice10,
                 dat_prep_op$Choice11, dat_prep_op$Choice12)
  target_op <- c(dat_prep_op$Choice2, dat_prep_op$Choice3, dat_prep_op$Choice4, dat_prep_op$Choice5,
                 dat_prep_op$Choice6, dat_prep_op$Choice7, dat_prep_op$Choice8, dat_prep_op$Choice9, dat_prep_op$Choice10, dat_prep_op$Choice11,
                 dat_prep_op$Choice12, dat_prep_op$Choice13)
  dat2_op <- data.frame(table(source_op, target_op))
  value_op <- c()
  for (i in 1:length(source_op)){
    s <- source_op[i]
    if (is.na(s)){
      value_op[i] <- 0
    }
    else {
      t <- target_op[i]
      if (is.na(t)){
        value_op[i] <- 0
      }
      else {
        ind <- which(dat2_op$source_op %in% s & dat2_op$target_op %in% t)
        value_op[i] <- dat2_op$Freq[ind]
      }
    }
  }
  
  links_op <- data.frame(source = source_op,
                         target = target_op,
                         value  = value_op)
  links_op <- na.omit(links_op)
  links_op <- unique(links_op)
  
  nodes_op <- steps_op
  nodes_op$Groups.type <- as.factor(nodes_op$Groups)
  nodes_op[sapply(nodes_op, is.factor)] <- data.matrix(nodes_op[sapply(nodes_op, is.factor)])
  
  links_op$IDsource <- match(links_op$source, nodes_op$Names) - 1
  links_op$IDtarget <- match(links_op$target, nodes_op$Names) - 1
  links_op <- na.omit(links_op)
  list_df_op[[p]] <- links_op
}

nodes_op <- steps_op
nodes_op$Groups.type <- as.factor(nodes_op$Groups)
nodes_op[sapply(nodes_op, is.factor)] <- data.matrix(nodes_op[sapply(nodes_op, is.factor)])
clrs_op <- c("antiquewhite4", "aquamarine3", "blue3", "blueviolet", "brown3",
             "chartreuse3", "chocolate1", "coral4", "cornflowerblue", "darkgoldenrod2",
             "lightpink2", "seagreen4", "lemonchiffon3", "gray75", "cadetblue1", "darkorchid1", "lawngreen")

sel_p_op <- 1
id_p_op <- which(dat_op$Key == sel_p_op)
links_op <- list_df_op[[id_p_op]]
g_op <- graph_from_data_frame(d=links_op, vertices=nodes_op, directed=T)
first_nd_op <- links_op$source[1]
id_fn_op <- which(nodes_op$Names == first_nd_op)
V(g_op)$color <- clrs_op[V(g_op)$Groups.type]
V(g_op)$color[id_fn_op] <- "black"
tkid <- tkplot(g_op)
cd <- tkplot.getcoords(tkid)
saveRDS(cd, file = "coordinates_options_shiny3.RDS")
cdi <- readRDS("coordinates_op.RDS")
tk_set_coords(tkid, cdi)
plot(g_op, 
     layout = cdi_op,
     vertex.frame.color = "black",                 # Node border color
     vertex.shape="circle",                        # One of “none”, “circle”, “square”, “csquare”, “rectangle” “crectangle”, “vrectangle”, “pie”, “raster”, or “sphere”
     vertex.size=10,                               # Size of the node (default is 15)
     
     # === vertex label
     vertex.label.color="black",
     vertex.label.font=2,                          # Font: 1 plain, 2 bold, 3, italic, 4 bold italic, 5 symbol
     vertex.label.cex=0.7,                           # Font size (multiplication factor, device-dependent)
     vertex.label.dist=0,                          # Distance between the label and the vertex
     vertex.label.degree=0 ,                       # The position of the label in relation to the vertex (use pi)
     
     # === Edge
     edge.color="blue",                            # Edge color
     edge.width=1,                                 # Edge width, defaults to 1
     edge.arrow.size=0.6,                            # Arrow size, defaults to 1
     edge.arrow.width=3,                           # Arrow width, defaults to 1
     edge.lty="solid",                             # Line type, could be 0 or “blank”, 1 or “solid”, 2 or “dashed”, 3 or “dotted”, 4 or “dotdash”, 5 or “longdash”, 6 or “twodash”
     edge.curved=0.3    ,                          # Edge curvature, range 0-1 (FALSE sets it to 0, TRUE to 0.5)
     asp = 0.7
)

legend(x=-0.5, y=-1.1, c("Atlas definition","Combine block", "Compute Connectivity", "Head motion correction", "Motion_regression",
                         "Removal initial volume","Spatial normalization", "Spatial smoothing", "Temporal detrending", "Temporal filtering",
                         "Graph characteristic","Negative correlation", "Network definition", "Result aggregation", "Software", "Sparsity control", "Time series ROI"), pch=21,
       col="#777777", pt.bg=clrs_op, pt.cex=2, cex=.8, bty="n", ncol=3)

##########################
###Not reported options###
##########################
for (opc in 2:ncol(dat_op_or)){
  for (opr in 1:nrow(dat_op_or)){
    opt <- dat_op_or[opr,opc]
    if (is.na(opt)){
      pap <- dat[opr,]
      if (colnames(dat_op_or)[opc] %in% pap){
        dat_op_or[opr,opc] <- "Not_reported"
      }
      else{
        dat_op_or[opr,opc] <- "Not_used"
      }
    }
  }
} 

st_w_op <- colnames(dat_op_or)
nr_mat <- data.frame(Not_Reported = numeric(),
                     Not_Used = numeric(),
                     Used = numeric())
for (st_op in 2:length(st_w_op)){
  cl <- dat_op_or[st_op]
  nr_mat[st_op,1] <- length(which(cl == "Not_reported"))
  nr_mat[st_op,2] <- length(which(cl == "Not_used"))
  nr_mat[st_op,3] <- 206 - nr_mat[st_op,1] - nr_mat[st_op,2]
}
nr_mat <- nr_mat[-1,]
steps_op2 <- c(rep(st_w_op[-1],3))
report <- c(rep("Step used option reported",17), rep("Step not used",17), rep("Step used option not reported",17))
values_rep <- c(nr_mat[,3], nr_mat[,2], nr_mat[,1])
nr_mat2 <- data.frame(steps_op2,report,values_rep)
nr_mat2$steps_op_vis <- c(rep(unique(steps_op$Groups_vis),3))
ggplot(nr_mat2, aes(fill=report, y=steps_op_vis, x=values_rep)) + 
     geom_bar(position="stack", stat="identity") +
     scale_fill_manual(values=c('gray', '#793d3d', '#446644')) +
     theme(text = element_text(size = 30), 
        axis.title.x = element_text(size = 30),  # Increase the size of the x axis label
        axis.title.y = element_text(size = 30)) 
ggsave("reporting_options.pdf")


####################
###Most debatable###
####################
st_deb <- nodes$Names[which(nodes$size>60 & nodes$size<120)]

###########################
###Graph measure; Degree###
###########################
nodes3 <- nodes
nodes3$ID <- c(1:61)
links3 <- links
links3$value[links3$value>0] <- 1
links3 <- subset(links3, value>0)
#nodes3$Names <- factor(nodes3$Names, levels=unique(nodes3$Names))
g_m <- graph_from_data_frame(d=links3, vertices=nodes3, directed=T)
#A <- as_adjacency_matrix(g_m, attr="value")
deg <- degree(g_m)
nodes3$degree <- deg
colors <- c("mediumpurple3", "darkolivegreen3", "firebrick", "darkorange", "gray","dodgerblue")
nodes3$col <- colors[nodes3$Groups.type]
#ggplot(nodes3, aes(y=Names, x=degree)) +
#  geom_bar(stat = "identity") +
#  ggtitle("Degree of each step")

###With lolipop plot
nodes3 <- nodes3 %>%
  mutate(Names_or = fct_reorder(Names_vis, desc(ID)))
ggplot(nodes3, aes(x=Names_or, y=degree)) +
  geom_segment( aes(x=Names_or, xend=Names_or, y=0, yend=degree), color=nodes3$col) +
  geom_point( color=nodes3$col, size=4, alpha=0.6) +
  geom_text(aes(label = degree), vjust = 0.5, hjust = -0.5, size = 4, color = "black") +
  theme_light() +
  coord_flip() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()
  ) +
  theme(text=element_text(size=18,  family="Times New Roman")) +
  labs(x = "Steps", y = "Degree")
#ggsave("degree.png", width = 15, height = 23, units = "cm", dpi = 300)
ggsave("degree.pdf",width = 7, height = 13)

###With polar plot
nodes3_SP <- subset(nodes3, Groups == "Structural_preprocessing")
nodes3_FP <- subset(nodes3, Groups == "Functional_preprocessing")
nodes3_NR <- subset(nodes3, Groups == "Noise_removal")
nodes3_FC <- subset(nodes3, Groups == "FC_def")
nodes3_GA <- subset(nodes3, Groups == "Graph_analysis")


fig <- plot_ly(
  type = 'scatterpolar',
  mode = 'text+lines'
) 

fig <- fig %>%
  add_trace(
    r = c(0, nodes3_SP$degree, 0),
    theta <- seq(0, 360, by = 360/(length(nodes3_SP$degree)+1)),
    theta[length(theta)] <- 0,
    fill = 'toself',
    fillcolor = '#1f77b4',
    line = list(
      color = 'black'
    ),
    name = 'Structural preprocessing',
    text = c(' ', nodes3_SP$ID, ' '),
    textfont = list(color = '#000000', size = 12),
    textposition='top center',
    opacity = 0.4
    
)

fig <- fig %>%
  add_trace(
    r = c(0, nodes3_FP$degree, 0),
    theta <- seq(0, 360, by = 360/(length(nodes3_FP$degree)+1)),
    theta[length(theta)] <- 0,
    fill = 'toself',
    fillcolor = '#2ca02c',
    line = list(
      color = 'black'
    ),
    name = 'Functional preprocessing',
    text = c(' ', nodes3_FP$ID, ' '),
    textfont = list(color = '#000000', size = 12),
    textposition='top center',
    opacity = 0.4
    #subplot = FP
)

fig <- fig %>%
  add_trace(
    r = c(0, nodes3_NR$degree, 0),
    theta <- seq(0, 360, by = 360/(length(nodes3_NR$degree)+1)),
    theta[length(theta)] <- 0,
    fill = 'toself',
    fillcolor = '#ff7f0e',
    line = list(
      color = 'black'
    ),
    name = 'Noise removal',
    text = c(' ', nodes3_NR$ID, ' '),
    textfont = list(color = '#000000', size = 12),
    textposition='top center',
    opacity = 0.4
    #subplot = NR
)

fig <- fig %>%
  add_trace(
    r = c(0, nodes3_FC$degree, 0),
    theta <- seq(0, 360, by = 360/(length(nodes3_FC$degree)+1)),
    theta[length(theta)] <- 0,
    fill = 'toself',
    fillcolor = '#9467bd',
    line = list(
      color = 'black'
    ),
    name = 'Functional connectivity definition',
    text = c(' ', nodes3_FC$ID, ' '),
    textfont = list(color = '#000000', size = 12),
    textposition='top center',
    opacity = 0.4
    #subplot = FC
)

fig <- fig %>%
  add_trace(
    r = c(0, nodes3_GA$degree, 0),
    theta <- seq(0, 360, by = 360/(length(nodes3_GA$degree)+1)),
    theta[length(theta)] <- 0,
    fill = 'toself',
    fillcolor = '#e31a1c',
    line = list(
      color = 'black'
    ),
    name = 'Graph analysis',
    text = c(' ', nodes3_GA$ID, ' '),
    textfont = list(color = '#000000', size = 12),
    textposition='top center',
    opacity = 0.4
    #subplot = GA
)

fig <- fig %>%
  layout(
    polar = list(
      radialaxis = list(
        visible = T,
        range = c(0,50)
      )
    ),
    showlegend = F
  )


#p <- ggplotly(fig)
if (!require("processx")) install.packages("processx")
orca(fig, "degree.png")


###########################
###Combination and order###
###########################
##Note that this only involves group of functional preprocessing and noise removal
mat_yn <- readRDS("mat_yn.RDS")
mat_or <- readRDS("mat_or.RDS")

##Combination
st_sel <- "GlobalSigRegress"
st_dat <- mat_yn[st_sel, ]
st_dat <- data.frame(st_dat)
st_dat$name <- row.names(st_dat)
colnames(st_dat) <- c("value", "name")
st_dat$Groups <- nodes$Groups
st_dat$Groups <- factor(st_dat$Groups, levels = unique(st_dat$Groups))
st_dat$col <- nodes$col
st_dat <- st_dat %>%
  mutate(Names_or = fct_reorder(name, desc(nodes$ID)))
label_colors <- ifelse(st_dat$Names_or == st_sel, "red", "black")
names(label_colors) <- st_dat$Names_or
par(mar=c(10,4,4,1)+.1)

# Create the ggplot
p <-  ggplot(st_dat, aes(x = Names_or, y = value, color = Groups)) +
      geom_segment(aes(xend = Names_or, yend = 0), size = 1) +
      geom_point(size = 4, alpha = 0.6) +
      geom_text(aes(label = value), vjust = 0.5, hjust = -0.5, size = 4, color = "black", family = "Times New Roman") +
      scale_color_manual(values = unique(st_dat$col)) +
      theme_light() +
      theme(
        panel.grid.major.y = element_blank(),
        panel.border = element_blank(),
        axis.ticks.y = element_blank(),
        text = element_text(size = 12, family = "Times New Roman"),
        axis.text.y = element_text(color = label_colors[st_dat$Names_or]),
      ) +
      coord_flip() +
      labs(
        x = "Steps",
        y = paste("Number of papers (out of 220 papers) used it together with", st_sel),
        color = "Groups"
      )

# Save the plot as a PDF
ggsave("GSR_comb.pdf", plot = p, width = 4, height = 7)

##Order
st_sel_OR <- "MotionRegress"
st_dat_OR <- mat_or[st_sel_OR, ]
st_dat_OR <- data.frame(st_dat_OR)
st_dat_OR$name <- row.names(st_dat_OR)
colnames(st_dat_OR) <- c("value", "name")
st_dat_OR$Groups <- nodes$Groups
st_dat_OR$Groups <- factor(st_dat_OR$Groups, levels = unique(st_dat_OR$Groups))
st_dat_OR$col <- nodes$col
st_dat_OR <- st_dat_OR %>%
  mutate(Names_or = fct_reorder(name, desc(nodes$ID)))
label_colors <- ifelse(st_dat_OR$Names_or == st_sel_OR, "red", "black")
names(label_colors) <- st_dat_OR$Names_or
par(mar=c(10,4,4,1)+.1)
p <- ggplot(st_dat_OR, aes(x = Names_or, y = value, color = Groups)) +
  geom_segment(aes(xend = Names_or, yend = 0), size = 1) +
  geom_point(size = 4, alpha = 0.6) +
  geom_text(aes(label = value), vjust = 0.5, hjust = -0.5, size = 4, color = "black", family = "Times New Roman") +
  scale_color_manual(values = unique(st_dat_OR$col)) +
  theme_light() +
  coord_flip() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank(),
    text = element_text(size = 12, family = "Times New Roman"),
    axis.text.y = element_text(color = label_colors[st_dat_OR$Names_or]),
  ) +
  labs(
    x = "Steps",
    y = paste("Number of papers (out of 220 papers) used it after", st_sel_OR),
    color = "Groups"
  )

# Save the plot as a PDF
ggsave("motion_order.pdf", plot = p, width = 4, height = 7)

#######################
###Temporal Analysis###
#######################

createCountDf <- function(table_DIY, p_inf) {
  step_DIY1 <- c(table_DIY$Names)
  step_DIY <- nodes$Names[match(step_DIY1, nodes$Names_vis, nomatch = 0)] 
  option_DIY <- c(table_DIY$Options)
  option_DIY <- option_DIY[option_DIY!=""]

  # Find only step first
  order_not <- TRUE
  if (order_not == T){
    row_stepDIY <- which(apply(dat, 1, function(x1) {
      if (length(x1) < length(step_DIY)) {
        return(FALSE)
      }
      idx <- match(step_DIY, x1)
      all(!is.na(idx)) && all(diff(idx) == 1)
    }))
  }
  else {
    row_stepDIY <- which(apply(dat, 1, function(x2) all(step_DIY %in% x2)))
  }

  paper_opt <- dat_op[row_stepDIY, ]

  # Find also with option
  id_D_all <- list()
  for (na in 1:length(option_DIY)){
    opt_D <- option_DIY[na]
    if (opt_D == "Any"){
      id_D_all[[na]] <- 1:nrow(dat_op_or)
    }
    else {
      st_D1 <- table_DIY$Names[table_DIY$Options == opt_D]
      st_D <- nodes$Names[nodes$Names_vis %in% st_D1]
      id_D <- which(dat_op_or[,st_D] == opt_D)
      id_D_all[[na]] <- id_D
    }
  }

  vals <- unlist(id_D_all)
  row_optDIY <- which(tabulate(vals) >= length(id_D_all))
  row_finDIY <- intersect(row_stepDIY, row_optDIY)
  count <- length(row_finDIY)

  table_DIYfin <- p_inf[row_finDIY, ]

  # Count the number of papers for each year for the selected pipeline
  count_data <- table(table_DIYfin$Year)

  # Create a data frame from the count data
  count_df <- data.frame(
    Year = as.numeric(names(count_data)),
    Count = as.vector(count_data)
  )

  # Filter the data for the years 2010 to 2023
  count_df <- count_df[count_df$Year >= 2010 & count_df$Year <= 2023, ]

  return(count_df)
}

table_DIY1 <- data.frame(Names = "MotionRegress", Options = "6p", check.names = FALSE)
table_DIY2 <- data.frame(Names = "MotionRegress", Options = "12p", check.names = FALSE)
table_DIY3 <- data.frame(Names = "MotionRegress", Options = "24p", check.names = FALSE)
table_DIY4 <- data.frame(Names = "MotionRegress", Options = "36p", check.names = FALSE)
table_DIY5 <- data.frame(Names = "AtlasDefine", Options = "Voxel_wise", check.names = FALSE)
table_DIY6 <- data.frame(Names = "AtlasDefine", Options = "Power", check.names = FALSE)
table_DIY7 <- data.frame(Names = "AtlasDefine", Options = "AAL1", check.names = FALSE)
table_DIY8 <- data.frame(Names = "AtlasDefine", Options = "ICA", check.names = FALSE)
table_ref <- data.frame(Names = "Software", Options = "Any", check.names = FALSE)

# Assume you have four data frames table_DIY1, table_DIY2, table_DIY3, table_DIY4
# You need to create count data frames for each of them
count_df1 <- createCountDf(table_DIY1, p_inf)
count_df2 <- createCountDf(table_DIY2, p_inf)
count_df3 <- createCountDf(table_DIY3, p_inf)
count_df4 <- createCountDf(table_DIY4, p_inf)
count_df5 <- createCountDf(table_DIY5, p_inf)
count_df6 <- createCountDf(table_DIY6, p_inf)
count_df7 <- createCountDf(table_DIY7, p_inf)
count_df8 <- createCountDf(table_DIY8, p_inf)
count_ref <- createCountDf(table_ref, p_inf)

# Combine all count data frames into one
count_df_all <- rbind(
  transform(count_df1, Option = "6p"),
  transform(count_df2, Option = "12p"),
  transform(count_df3, Option = "24p"),
  transform(count_df4, Option = "36p"),
  transform(count_df5, Option = "Voxel wise"),
  transform(count_df6, Option = "Power"),
  transform(count_df7, Option = "AAL1"),
  transform(count_df8, Option = "ICA")
)

library(RColorBrewer)

# Get 8 colors from the YlOrRd color map
colors <- rev(brewer.pal(4, "YlOrRd"))
colors2 <- rev(brewer.pal(4, "BuPu"))

# Create the plot
p <- ggplot() +
  geom_bar(data = count_ref, aes(x = Year, y = Count), stat = "identity", fill = "gray", alpha = 0.3, width = 0.5) +
  geom_line(data = count_df_all[count_df_all$Option %in% c("6p", "12p", "24p", "36p"), ], aes(x = Year, y = Count, color = Option), size = 1) +
  geom_line(data = count_df_all[count_df_all$Option %in% c("Voxel wise", "Power", "AAL1", "ICA"), ], aes(x = Year, y = Count, color = Option), size = 1) +
  scale_color_manual(values = c(setNames(colors, c("6p", "12p", "24p", "36p")), setNames(colors2, c("Voxel wise", "Power", "AAL1", "ICA")))) +
  scale_x_continuous(breaks = 2010:2023) +  # Set the x-axis breaks
  expand_limits(x = 2010:2023) +  # Expand the x-axis limits
  labs(x = "Year", y = "Number of Papers", color = "Option") +
  theme_minimal() +
  theme(panel.grid = element_blank(),  # Remove the grid
        text = element_text(size = 20),  # Increase the size of the text
        legend.position = "bottom")  # Position the legend at the bottom

# Save the plot as a PDF
ggsave("temporal motion.pdf", plot = p, width = 10, height = 7)



############################
###Variability of options###
############################
###Read data
gr_dec <- "AtlasDefine"
gr_dec <- nodes$Names[nodes$Names_vis == gr_dec]
gr_dat <- dat_op_or[ ,c(gr_dec)]
gr_ds <- colSums(mtabulate(gr_dat))
gr_ds <- data.frame(gr_ds)
gr_ds$name <- row.names(gr_ds)
colnames(gr_ds) <- c("value", "name")
gr_ds$name <- fct_relevel(gr_ds$name, "Not_reported", "Not_used")
custom_colors <- c("Not_reported" = "red", "Not_used" = "blue")

p <-  ggplot(gr_ds, aes(x = name, y = value)) +
      geom_segment(aes(xend = name, yend = 0, color = name), size = 1) +
      geom_point(aes(color = name), size = 4, alpha = 0.6) +
      geom_text(aes(label = value), vjust = 0.5, hjust = -0.5, size = 4, color = "black", family = "Times New Roman") +
      scale_color_manual(values = custom_colors, guide = "none") +  # Remove the legend
      theme_light() +
      coord_flip() +
      theme(
        panel.grid.major.y = element_blank(),
        panel.border = element_blank(),
        axis.ticks.y = element_blank(),
        text = element_text(size = 12, family = "Times New Roman"),
      ) +
      labs(x = "Options", y = "Number of papers (out of 220 papers)")
ggsave("software option.pdf", plot = p, width = 4, height = 7)
