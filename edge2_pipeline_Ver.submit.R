######### Modified EDGE2 calculation code ######### 

# Original code by Rikki Gumbs
# Modified by Ruth Brown
# Adapted to run with predictions by Rikki Gumbs and Felix Forest

# Provide phylogenetic tree and dataframe with Red List categories and predictions for 200 draws: 
# function returns three objects: 
# 1. dataframe with terminal branch length, GE2, ED2 and EDGE2 scores for each species
# 2. expected PD loss tree
# 3. PD and expected PD loss in Myr for the clade

library(ape)
pext.vals <- data.frame(rl.cat = rev(c("CR","EN","VU","NT","LC")) , pext = rev(c(0.97, 0.97/2, 0.97/4,0.97/8,0.97/16)))
GE.2.calc <- function(pext){
  iucn <- sample(1:5, size=1000000, replace=TRUE)
  data <- data.frame(species=c(1:1000000), pext=pext$pext[iucn])
  data <- data[order(data$pext),]
  data$rank <- seq_len(nrow(data))
  rank <- c(0, with(data, tapply(rank, pext, median)))
  pext.tap <- c(0, pext$pext)
  rank.sq <- rank^2; rank.cub <- rank^3; rank.qu <- rank^4; rank.quu <- rank^5
  model <- lm(pext.tap ~ rank + rank.sq + rank.cub + rank.qu)
  data$rank.sq <- data$rank^2; data$rank.cub <- data$rank^3; data$rank.qu <- data$rank^4; data$rank.quu <- data$rank^5
  data$rank.pext <- predict(model, data)
  data$rank.pext[data$rank.pext <= 0] <- 0.0001
  data$rank.pext[data$rank.pext >= 1] <- 0.9999
  pext.LC <- data.frame(RL.cat = "LC", pext =data$rank.pext[data$pext == pext.tap[2]])
  pext.NT <- data.frame(RL.cat = "NT", pext =data$rank.pext[data$pext == pext.tap[3]])
  pext.VU <- data.frame(RL.cat = "VU", pext =data$rank.pext[data$pext == pext.tap[4]])
  pext.EN <- data.frame(RL.cat = "EN", pext =data$rank.pext[data$pext == pext.tap[5]])
  pext.CR <- data.frame(RL.cat = "CR", pext =data$rank.pext[data$pext == pext.tap[6]])
  pext.obj <- rbind(pext.CR,pext.EN, pext.VU, pext.NT, pext.LC)
  pext.obj.sample <- NULL
  for(i in unique(pext.obj$RL.cat)){
    a <- pext.obj$pext[pext.obj$RL.cat == i]
    if(median(a) < pext$pext[pext$rl.cat == i]){
      while(median(a) < pext$pext[pext$rl.cat == i]){
        a <- a[-sample(c(1:length(a)),50,prob = rev(a))]
      }
    }else{
      while(median(a) > pext$pext[pext$rl.cat == i]){
        a <- a[-sample(c(1:length(a)),50,prob = a)]
      }
    }
    pext.obj.sample <- rbind(pext.obj.sample,data.frame(RL.cat = i, pext = a))
  }
  return(pext.obj.sample)
}

pext.dist <- GE.2.calc(pext.vals)


start_time <- Sys.time()
tree_files <- list.files(
  "/Volumes/PortableSSD/equal_splits_both",
  pattern = "\\.tre$",
  full.names = TRUE
)


iucn <- read.csv(file="/Users/alexgjl/dated-complete-tree/config/latest_iucn_2025.csv")
leaves_table <-  read.csv("/Users/alexgjl/Desktop/third_submission_to_nc/updated_fields/updated_ordered_leaves_3.0.csv",
                          stringsAsFactors=F, header=T)
ott_name <- select(leaves_table, ott,name)
iucn<- merge(iucn,ott_name,by = "ott",all.x = TRUE)

iucn<-select(iucn,ott,name,status_code)
iucn$name <- gsub("_", "", iucn$name)


ED_calculation <- function(tree){
  
  # ===== reorder tree =====
  tree_post <- reorder(tree, "postorder")
  edges_post <- tree_post$edge
  
  n_tips  <- length(tree$tip.label)
  n_nodes <- n_tips + tree$Nnode
  
  # ===== branch length =====
  branch.length <- numeric(n_nodes)
  branch.length[edges_post[,2]] <- tree_post$edge.length
  
  # ===== postorder: descendant count =====
  desc_count <- numeric(n_nodes)
  desc_count[1:n_tips] <- 1  
  
  for(i in seq_len(nrow(edges_post))){
    parent <- edges_post[i,1]
    child  <- edges_post[i,2]
    
    desc_count[parent] <- desc_count[parent] + desc_count[child]
  }
  
  # ===== preorder: ED =====
  edges_pre <- edges_post[nrow(edges_post):1, ]
  
  ED <- numeric(n_nodes)
  
  root <- n_tips + 1
  ED[root] <- 0
  
  for(i in seq_len(nrow(edges_pre))){
    p <- edges_pre[i,1]
    c <- edges_pre[i,2]
    
    ED[c] <- ED[p] + branch.length[c] / desc_count[c]
  }
  
  
  return(list(
    ED = ED[1:n_tips],
    desc_count = desc_count,
    branch.length = branch.length
  ))
}  #Faith ED

EDGE_PD_combined <- function(tree, sp.pext){
  
  tree_post <- reorder(tree, "postorder")
  edges_post <- tree_post$edge
  
  n_tips  <- length(tree$tip.label)
  n_nodes <- n_tips + tree$Nnode
  
  # ===== branch length =====
  branch.length <- numeric(n_nodes)
  branch.length[edges_post[,2]] <- tree_post$edge.length
  
  # ===== pext =====
  idx <- match(tree$tip.label, sp.pext$label)
  
  pext_tips <- sp.pext$pext[idx]
  
  # ===== postorder: pext_product + PD + ePD =====
  pext_product <- rep(1, n_nodes)
  pext_product[1:n_tips] <- pext_tips
  
  PD_all  <- numeric(n_nodes)
  ePD_all <- numeric(n_nodes)
  
  for(i in seq_len(nrow(edges_post))){
    parent <- edges_post[i,1]
    child  <- edges_post[i,2]
    bl     <- tree_post$edge.length[i]
    
    # pext
    pext_product[parent] <- pext_product[parent] * pext_product[child]
    
    # PD
    PD_all[parent] <- PD_all[parent] + PD_all[child] + bl
    
    # ePD
    ePD_all[parent] <- ePD_all[parent] + ePD_all[child] + bl * pext_product[child]
  }
  
  # ===== preorder: ED / EDGE =====
  edges_pre <- edges_post[nrow(edges_post):1, ]
  
  ed2_inter <- numeric(n_nodes)
  ED  <- numeric(n_nodes)
  EDGE <- numeric(n_nodes)
  
  root <- n_tips + 1
  ed2_inter[root] <- branch.length[root] * pext_product[root]
  
  for(i in seq_len(nrow(edges_pre))){
    p <- edges_pre[i,1]
    c <- edges_pre[i,2]
    
    if(c <= n_tips){
      ED[c]   <- branch.length[c] + ed2_inter[p] / pext_tips[c]
      EDGE[c] <- pext_tips[c] * ED[c]
    } else {
      ed2_inter[c] <- ed2_inter[p] + branch.length[c] * pext_product[c]
    }
  }
  
  # ===== node mapping（PD用）=====
  node_labels <- tree$node.label
  ott <- sub(".*ott([0-9]+).*", "\\1", node_labels)
  ott_map <- setNames(seq_along(node_labels) + n_tips, ott)
  
  list(
    ED = ED[1:n_tips],
    EDGE = EDGE[1:n_tips],
    PD_all = PD_all,
    ePD_all = ePD_all,
    ott_map = ott_map,
    branch.length = branch.length
  )
}


#some node might be removed when removing ex/ew species, therefore pd related results may not have same length for each tree
df_ed_final       <- NULL
df_edge_final     <- NULL
df_faith_ed_final <- NULL

pd_store  <- list()
epd_store <- list()
ott_store <- list()



for (t in 1:1000) {
  
  cat("Tree:", t, "\n")
  
  tree_file <- tree_files[t]
  
  if (is.na(tree_file) || tree_file == "" || !file.exists(tree_file)) {
    cat("Skip invalid tree file:", tree_file, "\n")
    next
  }
  
  tree <- read.tree(tree_file)
  
  if (!inherits(tree, "phylo")) {
    cat("Skip non-phylo tree:", tree_file, "\n")
    next
  }
  
  
  tree$tip.ott <- as.numeric(sub(".*_ott([0-9]+)$", "\\1", tree$tip.label))
  
  #### ========= drop EX/EW =========
  extinct_ott <- iucn$ott[iucn$status_code %in% c("EX","EW")]
  drop_labels <- tree$tip.label[tree$tip.ott %in% extinct_ott]
  tree <- drop.tip(tree, drop_labels, collapse.singles = TRUE)
  
  tree$tip.ott <- as.numeric(sub(".*_ott([0-9]+)$", "\\1", tree$tip.label))
  tree$tip.name <- tree$tip.label %>%
    sub("_ott[0-9]+$", "", .) %>%     
    gsub("_", " ", .) %>%             
    gsub("\\bsp\\.\\b", "", .) %>%   
    trimws()
  #### ===== IUCN mapping =====
  df <- data.frame(
    Species = tree$tip.label,
    name = tree$tip.name
  )
iucn <- iucn %>%
  dplyr::distinct(name, .keep_all = TRUE)
  
  idx <- match(df$name, iucn$name)
  
  df$status_code <- iucn$status_code[idx]
  
  draw <- df$status_code
  draw[draw %in% c("LR/nt","LR/cd")] <- "NT"
  draw[draw %in% c("LR/lc")] <- "LC"
  draw[draw %in% c("RE")] <- "CR"
  draw[draw == "DD"] <- NA
  
  set.seed(t)
  
  df$pext <- NA
  
  pool_CR <- pext.dist$pext[pext.dist$RL.cat == "CR"]
  pool_EN <- pext.dist$pext[pext.dist$RL.cat == "EN"]
  pool_VU <- pext.dist$pext[pext.dist$RL.cat == "VU"]
  pool_NT <- pext.dist$pext[pext.dist$RL.cat == "NT"]
  pool_LC <- pext.dist$pext[pext.dist$RL.cat == "LC"]
  pool_ALL <- pext.dist$pext
  
  py_sample <- function(x, n) {
    x[sample.int(length(x), n, replace = TRUE)]
  }
  
  fill_pext <- function(cond, pool) {
    idx <- which(cond)
    if (length(idx) > 0) {
      df$pext[idx] <<- py_sample(pool, length(idx))
    }
  }
  
  fill_pext(draw == "CR", pool_CR)
  fill_pext(draw == "EN", pool_EN)
  fill_pext(draw == "VU", pool_VU)
  fill_pext(draw == "NT", pool_NT)
  fill_pext(draw == "LC", pool_LC)
  fill_pext(is.na(draw), pool_ALL)
  idx_na <- which(is.na(df$pext))
  
  
  
  sp.pext <- data.frame(label = tree$tip.label, pext = df$pext)
  
  #### ===== main computation =====
  res <- EDGE_PD_combined(tree, sp.pext)
  ed_res <- ED_calculation(tree)
  
  #### ===== node mapping =====
  labels <- tree$node.label
  ott_list <- as.numeric(sub(".*_ott([0-9]+)$", "\\1",
                             labels[grepl("_ott[0-9]+$", labels)]))
  
  nodes <- res$ott_map[as.character(ott_list)]
  
  
  pd_vec  <- rep(NA_real_, length(nodes))
  epd_vec <- rep(NA_real_, length(nodes))
  
  valid_pd <- !is.na(nodes) &
    nodes > 0 &
    nodes <= length(res$PD_all)
  
  pd_vec[valid_pd] <-
    res$PD_all[nodes[valid_pd]] +
    res$branch.length[nodes[valid_pd]]
  
  valid_epd <- !is.na(nodes) &
    nodes > 0 &
    nodes <= length(res$ePD_all)
  
  epd_vec[valid_epd] <- res$ePD_all[nodes[valid_epd]] + 
    (res$branch.length[nodes[valid_epd]] * res$pext_product[nodes[valid_epd]])
  
  if (is.null(df_ed_final)) {
    
    df_ed_final       <- data.frame(Species = df$Species)
    df_edge_final     <- data.frame(Species = df$Species)
    df_faith_ed_final <- data.frame(Species = df$Species)
    
  }
  
  # ===== ED =====
  ed_map <- setNames(res$ED, tree$tip.label)
  df_ed_final[[paste0("tree_", t)]] <-
    ed_map[df_ed_final$Species]
  
  # ===== EDGE =====
  edge_map <- setNames(res$EDGE, tree$tip.label)
  df_edge_final[[paste0("tree_", t)]] <-
    edge_map[df_edge_final$Species]
  
  # ===== Faith ED =====
  faith_map <- setNames(ed_res$ED, tree$tip.label)
  df_faith_ed_final[[paste0("tree_", t)]] <-
    faith_map[df_faith_ed_final$Species]
  
  pd_store[[t]]  <- pd_vec
  epd_store[[t]] <- epd_vec
  
  ott_store[[t]] <- ott_list
  
  rm(tree, res, ed_res)
  gc()
}


#combine pd data
lens <- sapply(ott_store, length)
ref_id <- which.max(lens)

master_ott <- ott_store[[ref_id]]
df_pd_final <- data.frame(ott = master_ott)
df_epd_final <- data.frame(ott = master_ott)

for (t in seq_along(pd_store)) {
  
  pd_vec  <- pd_store[[t]]
  epd_vec <- epd_store[[t]]
  ott_vec <- ott_store[[t]]
  
  # mapping ott → value
  pd_map  <- setNames(pd_vec, ott_vec)
  epd_map <- setNames(epd_vec, ott_vec)
  
  # align to master
  df_pd_final[[paste0("tree_", t)]] <-
    pd_map[as.character(master_ott)]
  
  df_epd_final[[paste0("tree_", t)]] <-
    epd_map[as.character(master_ott)]
}


write.csv(x = df_epd_final, file = "/Volumes/PortableSSD/1_1000_tree_epd.csv")
write.csv(x = df_pd_final, file = "/Volumes/PortableSSD/1_1000_tree_pd.csv")
write.csv(x = df_ed_final, file = "/Volumes/PortableSSD/1_1000_tree_ed2.csv")
write.csv(x = df_edge_final, file = "/Volumes/PortableSSD/1_1000_tree_edge2.csv")
write.csv(x = df_faith_ed_final, file = "/Volumes/PortableSSD/1_1000_tree_ed.csv")

end_time <- Sys.time()
cat("Total running time:", end_time - start_time, "hour")
rm(list = ls())



################################################################################################################################################












##########  check whether giving sister species LC instead of VU will influence the EDGE2 score of Latimeria_chalumna ##############################

library(ape)
library(dplyr)

pext.vals <- data.frame(rl.cat = rev(c("CR","EN","VU","NT","LC")) , pext = rev(c(0.97, 0.97/2, 0.97/4,0.97/8,0.97/16)))

GE.2.calc <- function(pext){
  iucn <- sample(1:5, size=1000000, replace=TRUE)
  data <- data.frame(species=c(1:1000000), pext=pext$pext[iucn])
  data <- data[order(data$pext),]
  data$rank <- seq_len(nrow(data))
  rank <- c(0, with(data, tapply(rank, pext, median)))
  pext.tap <- c(0, pext$pext)
  rank.sq <- rank^2; rank.cub <- rank^3; rank.qu <- rank^4; rank.quu <- rank^5
  model <- lm(pext.tap ~ rank + rank.sq + rank.cub + rank.qu)
  data$rank.sq <- data$rank^2; data$rank.cub <- data$rank^3; data$rank.qu <- data$rank^4; data$rank.quu <- data$rank^5
  data$rank.pext <- predict(model, data)
  data$rank.pext[data$rank.pext <= 0] <- 0.0001
  data$rank.pext[data$rank.pext >= 1] <- 0.9999
  pext.LC <- data.frame(RL.cat = "LC", pext =data$rank.pext[data$pext == pext.tap[2]])
  pext.NT <- data.frame(RL.cat = "NT", pext =data$rank.pext[data$pext == pext.tap[3]])
  pext.VU <- data.frame(RL.cat = "VU", pext =data$rank.pext[data$pext == pext.tap[4]])
  pext.EN <- data.frame(RL.cat = "EN", pext =data$rank.pext[data$pext == pext.tap[5]])
  pext.CR <- data.frame(RL.cat = "CR", pext =data$rank.pext[data$pext == pext.tap[6]])
  pext.obj <- rbind(pext.CR,pext.EN, pext.VU, pext.NT, pext.LC)
  pext.obj.sample <- NULL
  for(i in unique(pext.obj$RL.cat)){
    a <- pext.obj$pext[pext.obj$RL.cat == i]
    if(median(a) < pext$pext[pext$rl.cat == i]){
      while(median(a) < pext$pext[pext$rl.cat == i]){
        a <- a[-sample(c(1:length(a)),50,prob = rev(a))]
      }
    }else{
      while(median(a) > pext$pext[pext$rl.cat == i]){
        a <- a[-sample(c(1:length(a)),50,prob = a)]
      }
    }
    pext.obj.sample <- rbind(pext.obj.sample,data.frame(RL.cat = i, pext = a))
  }
  return(pext.obj.sample)
}

pext.dist <- GE.2.calc(pext.vals)

iucn <- read.csv(file="/Users/alexgjl/dated-complete-tree/config/latest_iucn_2025.csv")
iucn$status_code[iucn$ott == 898631] <- "LC"




leaves_table <- read.csv("/Users/alexgjl/Desktop/third_submission_to_nc/updated_fields/updated_ordered_leaves_3.0.csv",
                         stringsAsFactors=F, header=T)
ott_name <- select(leaves_table, ott, name)
iucn <- merge(iucn, ott_name, by = "ott", all.x = TRUE)
iucn <- select(iucn, ott, name, status_code)
iucn$name <- gsub("_", "", iucn$name)


EDGE_only <- function(tree, sp.pext){
  tree_post <- reorder(tree, "postorder")
  edges_post <- tree_post$edge
  
  n_tips  <- length(tree$tip.label)
  n_nodes <- n_tips + tree$Nnode
  
  # get branch length
  branch.length <- numeric(n_nodes)
  branch.length[edges_post[,2]] <- tree_post$edge.length
  
  # match pext
  idx <- match(tree$tip.label, sp.pext$label)
  pext_tips <- sp.pext$pext[idx]
  
 
  pext_product <- rep(1, n_nodes)
  pext_product[1:n_tips] <- pext_tips
  
  for(i in seq_len(nrow(edges_post))){
    parent <- edges_post[i,1]
    child  <- edges_post[i,2]
    pext_product[parent] <- pext_product[parent] * pext_product[child]
  }
  
  
  edges_pre <- edges_post[nrow(edges_post):1, ]
  
  ed2_inter <- numeric(n_nodes)
  ED  <- numeric(n_nodes)
  EDGE <- numeric(n_nodes)
  
  root <- n_tips + 1
  ed2_inter[root] <- branch.length[root] * pext_product[root]
  
  for(i in seq_len(nrow(edges_pre))){
    p <- edges_pre[i,1]
    c <- edges_pre[i,2]
    
    if(c <= n_tips){
      
      ED[c]   <- branch.length[c] + ed2_inter[p] / pext_tips[c]
      EDGE[c] <- pext_tips[c] * ED[c]
    } else {
      ed2_inter[c] <- ed2_inter[p] + branch.length[c] * pext_product[c]
    }
  }
  
  return(list(ED2 = ED[1:n_tips], EDGE = EDGE[1:n_tips]))
}

tree_files <- list.files(
  "/Volumes/PortableSSD/equal_splits_both",
  pattern = "\\.tre$",
  full.names = TRUE
)

#get target species Latimeria chalumnae
target_species_base <- "Latimeria_chalumnae"
results_list <- list()

for (t in 1:1000) {
  
  cat("Processing Tree:", t, "\n")
  
  tree_file <- tree_files[t]
  if (is.na(tree_file) || tree_file == "" || !file.exists(tree_file)) next
  
  tree <- read.tree(tree_file)
  if (!inherits(tree, "phylo")) next
  
 
  target_tip <- grep(target_species_base, tree$tip.label, value = TRUE)
  if (length(target_tip) == 0) {
    cat("  -> Skip: target species not found\n")
    next
  }
  
  tree$tip.ott <- as.numeric(sub(".*_ott([0-9]+)$", "\\1", tree$tip.label))
  
  extinct_ott <- iucn$ott[iucn$status_code %in% c("EX","EW")]
  drop_labels <- tree$tip.label[tree$tip.ott %in% extinct_ott]
  
  
  tree <- drop.tip(tree, drop_labels, collapse.singles = TRUE)
  tree$tip.ott <- as.numeric(sub(".*_ott([0-9]+)$", "\\1", tree$tip.label))
  
  # match IUCN
  tree$tip.name <- tree$tip.label %>%
    sub("_ott[0-9]+$", "", .) %>%     
    gsub("_", " ", .) %>%             
    gsub("\\bsp\\.\\b", "", .) %>%   
    trimws()
  
  df <- data.frame(Species = tree$tip.label, name = tree$tip.name)
  
  # --- GET Pext ---
  iucn_unique <- iucn %>% dplyr::distinct(name, .keep_all = TRUE)
  idx <- match(df$name, iucn_unique$name)
  df$status_code <- iucn_unique$status_code[idx]
  
  draw <- df$status_code
  draw[draw %in% c("LR/nt","LR/cd")] <- "NT"
  draw[draw %in% c("LR/lc")] <- "LC"
  draw[draw %in% c("RE")] <- "CR"
  draw[draw == "DD"] <- NA
  
  set.seed(t) 
  df$pext <- NA
  
  py_sample <- function(x, n) x[sample.int(length(x), n, replace = TRUE)]
  
  fill_pext <- function(cond, pool) {
    idx_cond <- which(cond)
    if (length(idx_cond) > 0) df$pext[idx_cond] <<- py_sample(pool, length(idx_cond))
  }
  
  fill_pext(draw == "CR", pext.dist$pext[pext.dist$RL.cat == "CR"])
  fill_pext(draw == "EN", pext.dist$pext[pext.dist$RL.cat == "EN"])
  fill_pext(draw == "VU", pext.dist$pext[pext.dist$RL.cat == "VU"])
  fill_pext(draw == "NT", pext.dist$pext[pext.dist$RL.cat == "NT"])
  fill_pext(draw == "LC", pext.dist$pext[pext.dist$RL.cat == "LC"])
  fill_pext(is.na(draw), pext.dist$pext)
  
  sp.pext <- data.frame(label = tree$tip.label, pext = df$pext)
  
  # calculate edge
  res <- EDGE_only(tree, sp.pext)
  
  target_idx <- match(target_tip[1], tree$tip.label)
  
  # save list
  results_list[[length(results_list) + 1]] <- data.frame(
    Tree = t,
    Species_Label = target_tip[1],
    ED2 = res$ED2[target_idx],
    EDGE = res$EDGE[target_idx]
  )
  
  rm(tree, res, df, sp.pext)
  gc()
}


final_target_results <- do.call(rbind, results_list)
print(final_target_results)

write.csv(final_target_results, "Latimeria_chalumnae_EDGE_underestimated.csv", row.names = FALSE)





