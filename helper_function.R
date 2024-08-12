library(shiny)
library(networkD3)
library(dplyr)
library(igraph)
library(visNetwork)
library(stringr)
library(png)
library(shinyjs)
library(DT)
library(rintrojs)
library(ggplot2)
library(qdapTools)
library(RColorBrewer)
library(forcats)
library(readxl)
rm(list = ls())
Sys.setlocale('LC_CTYPE','C')

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

#as.integer(dat$Key)
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
p_inf$DOI <- sprintf('<a href="https://doi.org/%s" target="_blank">%s</a>', p_inf$DOI, p_inf$DOI)
as.integer(p_inf$Year)
p_inf1 <- read_excel("Database.xlsx", sheet = "Paper_info1")
p_inf1$DOI <- sprintf('<a href="https://doi.org/%s" target="_blank">%s</a>', p_inf1$DOI, p_inf1$DOI)
as.integer(p_inf1$Year)
cn_p_inf <- colnames(p_inf)

#dat with name visual
replace_names <- function(x) {
  mapping <- setNames(steps$Names_vis, steps$Names)
  return(mapping[x])
}
dat_vis <- data.frame(lapply(dat, replace_names))
dat_vis$Key <- dat$Key

replace_column_names <- function(x) {
  mapping <- setNames(steps$Names_vis, steps$Names)
  new_names <- sapply(names(x), function(col_name) {
    if (col_name %in% names(mapping)) {
      return(mapping[col_name])
    } else {
      return(col_name)
    }
  })
  names(x) <- new_names
  return(x)
}

dat_op_or_vis <- replace_column_names(dat_op_or)

#############################
#########WholePipe###########
#############################
###Defining nodes
nodes <- steps
nodes$Groups.type <- as.factor(nodes$Groups)
nodes[sapply(nodes, is.factor)] <- data.matrix(nodes[sapply(nodes, is.factor)])
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
nodes2 <- nodes
links2 <- links

#########################
#########STEPS###########
#########################
###Listing paper
list_df <- list()
for (p in 1:length(dat$Key)){
  ppr <- dat_prep1[p, ]
  dat_prep <- ppr
  source <- c(dat_prep$Step1, dat_prep$Step2, dat_prep$Step3, dat_prep$Step4, dat_prep$Step5,
              dat_prep$Step6, dat_prep$Step7, dat_prep$Step8, dat_prep$Step9, dat_prep$Step10,
              dat_prep$Step11, dat_prep$Step12, dat_prep$Step13, dat_prep$Step14, dat_prep$Step15,
              dat_prep$Step16, dat_prep$Step17, dat_prep$Step18, dat_prep$Step19, dat_prep$Step20,
              dat_prep$Step21, dat_prep$Step22, dat_prep$Step23, dat_prep$Step24, dat_prep$Step25,
              dat_prep$Step26, dat_prep$Step27, dat_prep$Step28, dat_prep$Step29)
  target <- c(dat_prep$Step2, dat_prep$Step3, dat_prep$Step4, dat_prep$Step5,
              dat_prep$Step6, dat_prep$Step7, dat_prep$Step8, dat_prep$Step9, dat_prep$Step10,
              dat_prep$Step11, dat_prep$Step12, dat_prep$Step13, dat_prep$Step14, dat_prep$Step15,
              dat_prep$Step16, dat_prep$Step17, dat_prep$Step18, dat_prep$Step19, dat_prep$Step20,
              dat_prep$Step21, dat_prep$Step22, dat_prep$Step23, dat_prep$Step24, dat_prep$Step25,
              dat_prep$Step26, dat_prep$Step27, dat_prep$Step28, dat_prep$Step29, dat_prep$Step30)
  dat2 <- data.frame(table(source, target))
  value <- c()
  for (i in 1:length(source)){
    s <- source[i]
    if (is.na(s)){
      value[i] <- 0
    }
    else {
      t <- target[i]
      if (is.na(t)){
        value[i] <- 0
      }
      else {
        ind <- which(dat2$source %in% s & dat2$target %in% t)
        value[i] <- dat2$Freq[ind]
      }
    }
  }
  
  links <- data.frame(source = source,
                      target = target,
                      value  = value)
  links <- na.omit(links)
  links <- unique(links)
  
  nodes <- steps
  nodes$Groups.type <- as.factor(nodes$Groups)
  nodes[sapply(nodes, is.factor)] <- data.matrix(nodes[sapply(nodes, is.factor)])
  
  links$IDsource <- match(links$source, nodes$Names) - 1
  links$IDtarget <- match(links$target, nodes$Names) - 1
  links <- na.omit(links)
  list_df[[p]] <- links
}



nodes <- steps
nodes$ID <- c(1:nrow(steps))
nodes$Groups.type <- as.factor(nodes$Groups)
nodes[sapply(nodes, is.factor)] <- data.matrix(nodes[sapply(nodes, is.factor)])
clrs <- c("mediumpurple3", "darkolivegreen3", "firebrick", "darkorange", "gray","dodgerblue")
nodes$col <- clrs[nodes$Groups.type]
nodes$ID <- c(1:nrow(steps))

###########################
#########CHOICES###########
###########################
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
clrs_op <- c(
  "#FF0000",   # Red
  "#0000FF",   # Blue
  "#00FF00",   # Green
  "#FFFF00",   # Yellow
  "#800080",   # Purple
  "#FFA500",   # Orange
  "#FFC0CB",   # Pink
  "#A52A2A",   # Brown
  "#00FFFF",   # Cyan
  "#FF00FF",   # Magenta
  "#40E0D0",   # Turquoise
  "#E6E6FA",   # Lavender
  "#008080",   # Teal
  "#800000",   # Maroon
  "#FFD700",   # Gold
  "#4B0082",   # Indigo
  "#808000"    # Olive
)

####################################
#########Further Analyses###########
####################################
###Yes or no
# st <- nodes[ ,"Names"]
# st <- data.frame(st)
# colnames(st) <- "Names"
# mat_yn <- matrix(0, nrow = length(st$Names), ncol = length(st$Names))
# for (i in 1:length(st$Names)){
#   sti <- st$Names[i]
#   id_i <- which(apply(dat, 1, function(x2) all(sti %in% x2))) #which(dat == sti, arr.ind = T)
#   for (j in 1:length(st$Names)){
#     stj <- st$Names[j]
#     id_j <- which(apply(dat, 1, function(x2) all(stj %in% x2))) #which(dat == stj, arr.ind = T)
#     use_both <- intersect(id_i, id_j)
#     n_p <- length(use_both)
#     mat_yn[i,j] <- n_p
#   }
# }
# diag(mat_yn) <- 0
# colnames(mat_yn) <- nodes$Names_vis
# rownames(mat_yn) <- nodes$Names_vis
# saveRDS(mat_yn, "mat_yn.RDS")
mat_yn <- readRDS("mat_yn.RDS")

###Steps
# mat_or <- matrix(0, nrow = length(st$Names), ncol = length(st$Names))
# for (i in 1:length(st$Names)){
#   sti <- st$Names[i]
#   id_i <- which(apply(dat, 1, function(x2) all(sti %in% x2)))
#   for (j in 1:length(st$Names)){
#     stj <- st$Names[j]
#     id_j <- which(apply(dat, 1, function(x2) all(stj %in% x2)))
#     use_both <- intersect(id_i, id_j)
#     ord <- sapply(use_both, function(x) min(which(dat[x,] == sti)) - min(which(dat[x,] == stj)))
#     i_first <- sum(ord < 0)
#     mat_or[i,j] <- i_first
#   }
# }

# diag(mat_or) <- 0
# colnames(mat_or) <- nodes$Names_vis
# rownames(mat_or) <- nodes$Names_vis
# saveRDS(mat_or, "mat_or.RDS")
mat_or <- readRDS("mat_or.RDS")

#######################
#########DIY###########
#######################
nodes_DIY <- data.frame(Names = "") 
