rm(list = ls())
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggpubr)
library(ggpmisc)
library(RColorBrewer)
library(ggnewscale)
library(tidyverse)


leaves_table <-  read.csv("/Users/alexgjl/Desktop/third_submission_to_nc/updated_fields/updated_ordered_leaves_3.0.csv",
                          stringsAsFactors=F, header=T)
nodes_table <- read.csv("/Users/alexgjl/Desktop/third_submission_to_nc/updated_fields/updated_ordered_nodes_3.0.csv",
                        stringsAsFactors=F, header=T)

iucn <- read.csv(file="/Users/alexgjl/dated-complete-tree/config/latest_iucn_2025.csv")


#resolved_medianED <-  read.csv("/Users/alexgjl/Desktop/third_submission_to_nc/updated_fields/ed_median.csv",
#                              stringsAsFactors=F, header=T)

pd_all_life <-  read.csv("/Users/alexgjl/Desktop/Dated_tree_paper/PD_all_Life.csv",
                         stringsAsFactors=F, header=T)
median_PD <- median(as.numeric(pd_all_life[1, -1]), na.rm = TRUE)
tree_with_median <- colnames(pd_all_life)[-1][which.min(abs(as.numeric(pd_all_life[1, -1]) - median(as.numeric(pd_all_life[1, -1]), na.rm = TRUE)))]
data.frame(
  tree = tree_with_median,
  value = pd_all_life[1, -1][which.min(abs(pd_all_life[1, -1] - median_PD))],
  median = median_PD
)


#Tree_377 provides the closest estimation to median PD
#Therefore, while seeing the distribution of ED/ED2, I will use the estimation from this tree.

#####################################################################   Figure 1   ################################################################################
#####################################################################              ################################################################################

#onezoom_medianED <-  read.csv("/Users/alexgjl/Desktop/third_submission_to_nc/updated_fields/ed_median.csv",
#                                                   stringsAsFactors=F, header=T)
EQS_medianED<- read.csv("/Users/alexgjl/Desktop/Dated_tree_paper/ED_median.csv",
                        stringsAsFactors=F, header=T)

Dis_ED <- read.csv("/Users/alexgjl/Desktop/Dated_tree_paper/median_tree_ed.csv",
                   stringsAsFactors=F, header=T)
Dis_ED2 <- read.csv("/Users/alexgjl/Desktop/Dated_tree_paper/median_tree_ed2.csv",
                    stringsAsFactors=F, header=T)

#make distribution figrue based on previously selected clades
library(ape)
tree_files <- list.files(
  "/Volumes/PortableSSD/equal_splits_both",
  pattern = "\\.tre$",
  full.names = TRUE
)
tree_file <- tree_files[1]
tree <- read.tree(tree_file)

get_nodename<- select(nodes_table, ott,name)
previous_clades <- c(
  "Metazoa","Fungi","Chloroplastida","Spermatophyta",
  "Diaphoretickes","Vertebrata","Lepidoptera",
  "Diptera","Coleoptera","Hymenoptera","Chelicerata",
  "Mollusca","Aves","Mammalia","Squamata","Testudines",
  "Crocodilia","Amphibia","Actinopterygii",
  "CYCLOSTOMATA","Eukaryota"
)
matched_rows <- get_nodename[
  tolower(get_nodename$name) %in% tolower(previous_clades),
]
new_rows <- data.frame(
  name = c("Chondrichthyes", "SAR"),
  ott  = c(278108, 5246039),
  stringsAsFactors = FALSE
)
matched_rows <- rbind(matched_rows, new_rows)

#Diaphoretickes no longer exist
matched_rows<- filter(matched_rows,matched_rows$name != "Diaphoretickes")
matched_rows<- filter(matched_rows,matched_rows$ott != 359037)
node_ott <- as.numeric(str_extract(tree$node.label, "(?<=ott)\\d+"))
selected_ott <- matched_rows$ott
selected_node_idx <- match(selected_ott, node_ott)
n_tips <- length(tree$tip.label)
selected_nodes <- selected_node_idx + n_tips

get_tips_from_node <- function(tree, node){
  tips <- extract.clade(tree, node)$tip.label
  # extract ott from tip.label
  return(tips)
}

result_list <- lapply(seq_along(selected_nodes), function(i){
  node <- selected_nodes[i]
  node_ott_val <- selected_ott[i]
  tip_ott <- get_tips_from_node(tree, node)
  data.frame(
    Group = node_ott_val,
    Species = tip_ott
  )
})

nodes_with_tips <- bind_rows(result_list)

matched_rows$node_ott<- matched_rows$ott
matched_rows$Group<- matched_rows$name
matched_rows1<- select(matched_rows,node_ott,Group)
nodes_with_tips$node_ott <- nodes_with_tips$Group
nodes_with_tips<- select(nodes_with_tips,Species, node_ott)
species_in_clade <- merge(nodes_with_tips,matched_rows1,how = "left",on = "node_ott")

species_in_clade$name <- sub("_ott.*", "", species_in_clade$Species)      
species_in_clade$name <- gsub("_", " ", species_in_clade$name)            
species_in_clade$tip_ott <- as.numeric(sub(".*_ott", "", species_in_clade$Species))

Dis_ED<- select(Dis_ED,Species,tree_377)

####overlapped species:
#Loligopsis_sp._ott0000  
#Periboea_sp._ott0000    
#Pasithoe_sp._ott0000     
Dis_ED1 <- Dis_ED %>%
  group_by(Species) %>%
  summarise(median_ed = median(tree_377, na.rm = TRUE))

species_in_clade1 <- select(species_in_clade,Species,Group)


species_in_clade_final<- merge(species_in_clade1,Dis_ED1,by = "Species", all.x = TRUE)
missing_rows <- species_in_clade_final[is.na(species_in_clade_final$median_ed), ]##all extinct species
missing_rows<- select(missing_rows,Species)
missing_rows<-unique(missing_rows)
missing_rows1 <- missing_rows %>%
  extract(Species,
          into = c("name", "ott"),
          regex = "^(.*)_ott(\\d+)$") %>%
  mutate(name = gsub("_", " ", name))

missing_rows1<- merge(missing_rows1,iucn,by = "ott", all.x = TRUE)
##all lacked species are either extinct in wild or extinct
species_in_clade_final <- species_in_clade_final[!is.na(species_in_clade_final$median_ed), ]
species_in_clade_final<- filter(species_in_clade_final,species_in_clade_final$Group != "CHONDRICHTHYES")
#diaphoretickes removed, holomycota replaced by fungi, 


#add crocodyle
add_vert <- data.frame(name = "Crocodylidae",
                       ott = 335582)
###get descendants from fully-dated tree
node_ott <- as.numeric(str_extract(tree$node.label, "(?<=ott)\\d+"))
selected_ott <- add_vert$ott
selected_node_idx <- match(selected_ott, node_ott)
n_tips <- length(tree$tip.label)
selected_nodes <- selected_node_idx + n_tips
get_tips_from_node <- function(tree, node){
  tips <- extract.clade(tree, node)$tip.label
  return(tips)
}
result_list <- lapply(seq_along(selected_nodes), function(i){
  node <- selected_nodes[i]
  node_ott_val <- selected_ott[i]
  tip_ott <- get_tips_from_node(tree, node)
  data.frame(
    Group = node_ott_val,
    Species = tip_ott
  )
})
nodes_with_tips <- bind_rows(result_list)
nodes_with_tips$ott <- nodes_with_tips$Group
nodes_with_tips<- merge(nodes_with_tips,add_vert,by = "ott", all.x = TRUE)
nodes_with_tips<- select(nodes_with_tips,ott,Species,name)
nodes_with_tips$Group <- nodes_with_tips$name
nodes_with_tips<- select(nodes_with_tips,ott,Species,Group)
add_croc<- merge(nodes_with_tips,Dis_ED1,by = "Species", all.x = TRUE)
add_croc<- select(add_croc,Species,Group,median_ed)
species_in_clade_final1<- rbind(species_in_clade_final,add_croc)



test_normality_by_group <- function(df) {
  df %>%
    group_by(Group) %>%
    group_modify(~{
      
      x <- .x$median_ed
      
      if (.y$Group == "Crocodylidae") {
        p <- shapiro.test(x)$p.value
        method <- "Shapiro-Wilk normality test"
      } else {
        p <- nortest::ad.test(x)$p.value
        method <- "Anderson-Darling normality test"
      }
      
      data.frame(
        p_value = p,
        method = method
      )
    }) %>%
    ungroup()
}



#left:Testudines,Crocodylia,Actinopterygii,CYCLOSTOMATA
pd_vals <- read.csv("/Users/alexgjl/Desktop/Dated_tree_paper/PD_median.csv",
                    stringsAsFactors=F, header=T)
selected_clades_all <- unique(species_in_clade_final1$Group)
matched_rows1$ott <- matched_rows1$node_ott
matched_rows2<- select(matched_rows1,ott,Group)
add_vert$Group<- add_vert$name
add_vert1<- select(add_vert,ott,Group)
matched_rows_all <- rbind(matched_rows2,add_vert1)
Selected_PD <-merge(matched_rows_all,pd_vals,by = "ott",all.x = TRUE)
Selected_PD$logPD<- log(Selected_PD$median_pd,10)
Selected_PD<- select(Selected_PD,Group,median_pd,logPD)
species_in_clade_final1<-merge(species_in_clade_final1,Selected_PD,by = "Group",all.x = TRUE) 


Clade_Biota<- Dis_ED1
Clade_Biota$median_pd <- rep(median_PD)
Clade_Biota$logPD<- log10(Clade_Biota$median_pd)
Clade_Biota$Group <- rep("Biota")
species_in_clade_final2 <- rbind(Clade_Biota,species_in_clade_final1)

build_clade_groups <- function(df) {
  pd_table <- df %>%
    group_by(Group) %>%
    summarise(logPD = unique(logPD), .groups = "drop") %>%
    arrange(desc(logPD))
  n <- nrow(pd_table)
  base_size <- floor(n / 4)
  remainder <- n %% 4
  
  group_sizes <- rep(base_size, 4)
  group_sizes[1:remainder] <- group_sizes[1:remainder] + 1
  
  pd_table$AtoG <- rep(LETTERS[1:4], times = group_sizes)
  
  col_fun <- colorRampPalette(c("#660066", "#FFCC00"))
  pd_table$color <- col_fun(nrow(pd_table))
  
  list(pd_table = pd_table)
}
plot_clade_density_by_group <- function(df) {
  
  info <- build_clade_groups(df)
  pd_table <- info$pd_table
  
  global_min <- min(df$logPD, na.rm = TRUE)
  global_max <- max(df$logPD, na.rm = TRUE)
  
  df2 <- df %>%
    select(-logPD) %>%
    left_join(pd_table, by = "Group")
  
  pd_table <- pd_table %>% arrange(desc(logPD))
  df2$Group <- factor(df2$Group, levels = pd_table$Group)
  
  result_list <- list()
  for (g in LETTERS[1:4]) {
    sub_df <- df2 %>% filter(AtoG == g)
    p <- ggplot(sub_df, aes(x = log10(median_ed), fill = logPD)) +
      
      geom_density(alpha = 0.6) +
      
      facet_grid(Group ~ .) +
      
      theme_classic() +
      
      scale_fill_gradient(
        low = "#FFCC00",
        high = "#660066",
        name = "logPD",
        limits = c(global_min, global_max)
      ) +
      
      labs(
        x = "Log10 (ED) Myr",
        y = "",
        title = paste("Group", g)
      ) +
      
      geom_vline(
        xintercept = quantile(log10(df$median_ed), probs = 0.95,na.rm = TRUE),
        color = "black",
        linetype = "solid",
        size = 0.4
      ) +
      
      geom_vline(
        xintercept = quantile(log10(df$median_ed), probs = 0.99,na.rm = TRUE),
        color = "black",
        linetype = "dashed",
        size = 0.4
      )+ 
      theme(
        axis.text.y = element_text(size = 9, margin = margin(r = 5)),
        strip.text.y = element_text(size = 8),
        plot.margin = margin(5, 25, 5, 5)
      )
    
    result_list[[g]] <- p
  }
  
  return(result_list)
}
library(ggplot2)

plot_pd_colorbar <- function(df) {
  
  min_val <- min(df$logPD, na.rm = TRUE)
  max_val <- log10(median_PD)
  
  ticks <- seq(min_val, max_val, length.out = 5)
  
  grad_df <- data.frame(
    x = 1,
    y = seq(min_val, max_val, length.out = 500)
  )
  
  ggplot(grad_df, aes(x = x, y = y, fill = y)) +
    
    geom_tile(width = 0.3) +   
    
    scale_fill_gradient(
      low = "#FFCC00",
      high = "#660066",
      guide = "none"
    ) +
    
   
    scale_x_continuous(
      limits = c(0.5, 1.5),
      expand = c(0, 0)
    ) +
    
    scale_y_continuous(
      limits = c(min_val, max_val),
      expand = expansion(mult = c(0.05, 0.05))  
    )+
    
    theme_void() +
    theme(
      plot.margin = margin(10, 20, 10, 20)
    ) +
    geom_text(
      data = data.frame(
        x = rep(1.4, 5),
        y = ticks,
        label = round(ticks, 2)
      ),
      aes(x = x, y = y, label = label),
      inherit.aes = FALSE,
      size = 4
    )
}

species_in_clade_final2$Group[which(species_in_clade_final2$Group == "CYCLOSTOMATA")] <- "Cyclostomata"

species_in_clade_final2<- unique(species_in_clade_final2)
#duplicate_summary2 <- species_in_clade_final2 %>%
# dplyr::count(Group, Species) %>%
#dplyr::filter(n > 1)


plots <- plot_clade_density_by_group(species_in_clade_final2)


plots$A
plots$B
plots$C
plots$D


#do the same analysis for ED2

#######################################        supplementary figure 2     ##################################################################################################
#############################################        figure S2     ##################################################################################################

Dis_ED2$median_ed2<-Dis_ED2$tree_377
Dis_ED2<- select(Dis_ED2,-tree_377)
species_in_clade_final2_2<-merge(species_in_clade_final2,Dis_ED2,all.x = TRUE,by = "Species")
species_in_clade_final2_2$median_ed <- species_in_clade_final2_2$median_ed2
plots_ed2 <- plot_clade_density_by_group(species_in_clade_final2_2)
plots_ed2$A
plots_ed2$B
plots_ed2$C
plots_ed2$D


nor_ed1<- test_normality_by_group(species_in_clade_final2)
nor_ed2<- test_normality_by_group(species_in_clade_final2_2)

write.csv(x = nor_ed1, file = "/Users/alexgjl/Desktop/Dated_tree_paper/normality_test_ED.csv")
write.csv(x = nor_ed2, file = "/Users/alexgjl/Desktop/Dated_tree_paper/normality_test_ED2.csv")
write.csv(x = species_in_clade_final2, file = "/Users/alexgjl/Desktop/Dated_tree_paper/ED_Dis_figure1.csv")
write.csv(x = species_in_clade_final2_2, file = "/Users/alexgjl/Desktop/Dated_tree_paper/ED2_Dis_figure1.csv")
##previous code has been checked.





##################################################################     Figure 2   ####################################################################################

#in this part, we use median ED value instead of a median tree (Tree_377)

get_nodename<- select(nodes_table, ott,name)
clades <- c( "Eukaryota","Metazoa","Chordata","Actinopteri","Squamata","Aves", "Amphibia","Mammalia",
             "Chondrichthyes","Testudines","Arthropoda","Pancrustacea", "Insecta","Coleoptera","Lepidoptera",
             "Diptera","Hymenoptera","Hemiptera", "Odonata","Malacostraca","Copepoda","Branchiopoda","Chelicerata",
             "Myriapoda", "Mollusca","Platyhelminthes","Annelida","Nematoda","Cnidaria","Porifera", "Echinodermata",
             "Bryozoa","Tardigrada","Chloroplastida","Tracheophyta", "Magnoliopsida","Polypodiopsida","Bryophyta","Marchantiophyta",
             "Chlorophyta", "Rhodophyta","Fungi","Ascomycota","Basidiomycota","Microsporidia","SAR",
             "Stramenopiles","Alveolata","Rhizaria","Archaea","Bacteria" )
matched_rows <- get_nodename[
  tolower(get_nodename$name) %in% tolower(clades),
]

new_rows <- data.frame(
  name = c("Chondrichthyes", "Archaea", "SAR", "Rhodophyta", "Bacteria"),
  ott  = c(278108, 996421, 5246039, 878953, 844192),
  stringsAsFactors = FALSE
)
matched_rows <- rbind(matched_rows, new_rows)


###get descendants from fully-dated tree
node_ott <- as.numeric(str_extract(tree$node.label, "(?<=ott)\\d+"))
selected_ott <- matched_rows$ott

selected_node_idx <- match(selected_ott, node_ott)
n_tips <- length(tree$tip.label)
selected_nodes <- selected_node_idx + n_tips

get_tips_from_node <- function(tree, node){
  tips <- extract.clade(tree, node)$tip.label
  return(tips)
}
result_list <- lapply(seq_along(selected_nodes), function(i){
  node <- selected_nodes[i]
  node_ott_val <- selected_ott[i]
  tip_ott <- get_tips_from_node(tree, node)
  data.frame(
    Group = node_ott_val,
    Species = tip_ott
  )
})
nodes_with_tips <- bind_rows(result_list)

matched_rows$node_ott<- matched_rows$ott
matched_rows$Group<- matched_rows$name
matched_rows1<- select(matched_rows,node_ott,Group)
nodes_with_tips$node_ott <- nodes_with_tips$Group
nodes_with_tips<- select(nodes_with_tips,Species, node_ott)
species_in_clade <- merge(nodes_with_tips,matched_rows1,how = "left",on = "node_ott")

species_in_clade$name <- sub("_ott.*", "", species_in_clade$Species)      
species_in_clade$name <- gsub("_", " ", species_in_clade$name)            
species_in_clade$tip_ott <- as.numeric(sub(".*_ott", "", species_in_clade$Species))


EQS_medianED1<- select(EQS_medianED,Species,median_ed)


#repeat name：
#Loligopsis_sp._ott0000  
#Periboea_sp._ott0000    
#Pasithoe_sp._ott0000     

EQS_medianED1 <- EQS_medianED1 %>%
  group_by(Species) %>%
  summarise(median_ed = median(median_ed, na.rm = TRUE))

species_in_clade1 <- select(species_in_clade,Species,Group)
species_in_clade_final<- merge(species_in_clade1,EQS_medianED1,by = "Species", all.x = TRUE)

missing_rows <- species_in_clade_final[is.na(species_in_clade_final$median_ed), ]##all extinct species
missing_rows<- select(missing_rows,Species)
missing_rows<-unique(missing_rows)
missing_rows1 <- missing_rows %>%
  extract(Species,
          into = c("name", "ott"),
          regex = "^(.*)_ott(\\d+)$") %>%
  mutate(name = gsub("_", " ", name))

missing_rows1<- merge(missing_rows1,iucn,by = "ott", all.x = TRUE)
##all lacked species are either extinct in wild or extinct
species_in_clade_final <- species_in_clade_final[!is.na(species_in_clade_final$median_ed), ]
species_in_clade_final<- filter(species_in_clade_final,species_in_clade_final$Group != "CHONDRICHTHYES")
#write.csv(x = species_in_clade_final, file = "/Users/alexgjl/Desktop/Dated_tree_paper/Selected_clades_with_descendants.csv")
add_vert<-get_nodename[get_nodename$name %in% c(
  "Testudines", "Actinopterygii", "CYCLOSTOMATA"
), ]
add_vert <- rbind(add_vert,
                  data.frame(name = "Crocodylidae",
                             ott = 335582))
###get descendants from fully-dated tree
node_ott <- as.numeric(str_extract(tree$node.label, "(?<=ott)\\d+"))
selected_ott <- add_vert$ott
selected_node_idx <- match(selected_ott, node_ott)
n_tips <- length(tree$tip.label)
selected_nodes <- selected_node_idx + n_tips

get_tips_from_node <- function(tree, node){
  tips <- extract.clade(tree, node)$tip.label
  # edxtract ott ftom tip.label
  return(tips)
}
result_list <- lapply(seq_along(selected_nodes), function(i){
  node <- selected_nodes[i]
  node_ott_val <- selected_ott[i]
  tip_ott <- get_tips_from_node(tree, node)
  data.frame(
    Group = node_ott_val,
    Species = tip_ott
  )
})
nodes_with_tips <- bind_rows(result_list)


nodes_with_tips$ott <- nodes_with_tips$Group
nodes_with_tips<- merge(nodes_with_tips,add_vert,by = "ott", all.x = TRUE)
nodes_with_tips<- select(nodes_with_tips,ott,Species,name)
nodes_with_tips$Group <- nodes_with_tips$name
nodes_with_tips<- select(nodes_with_tips,ott,Species,Group)
vert4ED<- merge(nodes_with_tips,EQS_medianED1,by = "Species", all.x = TRUE)
vert4ED<- select(vert4ED,Species,Group,median_ed)
species_in_clade_final1<- rbind(species_in_clade_final,vert4ED)

pd_vals <- read.csv("/Users/alexgjl/Desktop/Dated_tree_paper/PD_median.csv",
                    stringsAsFactors=F, header=T)
selected_clades_all <- unique(species_in_clade_final1$Group)

matched_rows1$ott <- matched_rows1$node_ott
matched_rows2<- select(matched_rows1,ott,Group)
add_vert$Group<- add_vert$name
add_vert1<- select(add_vert,ott,Group)
matched_rows_all <- rbind(matched_rows2,add_vert1)
Selected_PD <-merge(matched_rows_all,pd_vals,by = "ott",all.x = TRUE)
Selected_PD$logPD<- log(Selected_PD$median_pd,10)
Selected_PD<- select(Selected_PD,Group,median_pd,logPD)
species_in_clade_final1<-merge(species_in_clade_final1,Selected_PD,by = "Group",all.x = TRUE) 

Clade_Biota<- EQS_medianED
Clade_Biota$median_pd <- rep(median_PD)
Clade_Biota$logPD<- log10(Clade_Biota$median_pd)
Clade_Biota$Group <- rep("Biota")
species_in_clade_final2 <- rbind(Clade_Biota,species_in_clade_final1)
species_in_clade_final2<- unique(species_in_clade_final2)
duplicate_summary2 <- species_in_clade_final2 %>%
  dplyr::count(Group, Species) %>%
  dplyr::filter(n > 1)





EQS_medianED2<- read.csv("/Users/alexgjl/Desktop/Dated_tree_paper/ED2_median.csv",
                         stringsAsFactors=F, header=T)
EQS_medianED2$median_ed2 <- EQS_medianED2$median_ed
EQS_medianED2<- select(EQS_medianED2,Species,median_ed2)
species_in_clade_final3<- merge(species_in_clade_final2,EQS_medianED2,on = "Species", all.x = TRUE)

colnames(species_in_clade_final3)

duplicate_summary3 <- species_in_clade_final3 %>%
  dplyr::count(Group, Species) %>%
  dplyr::filter(n > 1)


compare_ED <- function(df) {
  df <- df[!is.na(df$median_ed), ]
  Q50 <- quantile(EQS_medianED1$median_ed, probs = 0.5, na.rm = TRUE)
  Q95 <- quantile(EQS_medianED1$median_ed, probs = 0.95, na.rm = TRUE)
  Q99 <- quantile(EQS_medianED1$median_ed, probs = 0.99, na.rm = TRUE)
  ls_quantile <- vector("list", length = nrow(df))
  for (i in 1:nrow(df)) {
    ed_value <- df$median_ed[i]
    if (ed_value > Q99) {
      ls_quantile[i] <- "Q(0.99)"
    } else if (ed_value > Q95) {
      ls_quantile[i] <- "Q(0.95)"
    } else if (ed_value > Q50) {
      ls_quantile[i] <- "Q(0.5)"
    } else {
      ls_quantile[i] <- " "
    }
  }
  df$Quantile <- unlist(ls_quantile)
  return(df)
}

# generate Null Distribution 
analyze_quantile_enrichment2 <- function(df, EQS_medianED1) {
  EQS_medianED1 <- EQS_medianED1[!is.na(EQS_medianED1$median_ed), ]
  threshold_99 <- quantile(EQS_medianED1$median_ed, probs = 0.99, na.rm = TRUE)
  
  summary_df <- df %>%
    group_by(Group) %>%
    summarise(
      num_top_99 = sum(Quantile == "Q(0.99)"),
      num_species = n(),
      .groups = "drop"
    )
  
  result_df <- summary_df %>%
    rowwise() %>%
    mutate(
      random_counts = list(replicate(1000, {
        sampled <- sample(EQS_medianED1$median_ed, size = num_species, replace = FALSE)
        sum(sampled >= threshold_99)
      })),
      p_greater_equal = mean(unlist(random_counts) >= num_top_99),
      p_less_equal = mean(unlist(random_counts) <= num_top_99)
    ) %>%
    ungroup()
  return(result_df)
}


build_clade_groups_fig2 <- function(df) {
  pd_table <- df %>%
    group_by(Group) %>%
    summarise(logPD = mean(logPD, na.rm = TRUE), .groups = "drop") %>%
    arrange(desc(logPD))
  
  col_fun <- colorRampPalette(c("#660066", "#FFCC00"))
  pd_table$color <- col_fun(nrow(pd_table))
  
  list(pd_table = pd_table)
}

# 2.1 Enrichment Analysis and Annotation
EQS_medianED1<- select(EQS_medianED,Species,median_ed)
species_in_clade_final4 <- compare_ED(species_in_clade_final3)
analyzed_species_in_clade_final <- analyze_quantile_enrichment2(species_in_clade_final4, EQS_medianED1)
species_in_clade_final4_1 <- merge(species_in_clade_final4, analyzed_species_in_clade_final, by = "Group", all.x = TRUE)

duplicate_summary4 <- species_in_clade_final4 %>%
  dplyr::count(Group, Species) %>%
  dplyr::filter(n > 1)




# 2.2 Extract sorting and color palette information
info <- build_clade_groups_fig2(species_in_clade_final4_1)
pd_table <- info$pd_table
global_min <- min(species_in_clade_final4_1$logPD, na.rm = TRUE)
global_max <- max(species_in_clade_final4_1$logPD, na.rm = TRUE)

# 2.3 Map the sort order back to the dataset and lock the factor levels.
grouped_species_in_clade_final4 <- species_in_clade_final4_1 %>%
  select(-logPD) %>%
  left_join(pd_table, by = "Group")
pd_table <- pd_table %>% arrange(desc(logPD))
grouped_species_in_clade_final4$Group <- factor(grouped_species_in_clade_final4$Group, levels = pd_table$Group)

# 2.4 Separate plotting subsets.
df_fig2_vioin1 <- select(grouped_species_in_clade_final4, Group, Species, median_ed, logPD)
df_fig2_vioin2 <- select(grouped_species_in_clade_final4, Group, random_counts, num_top_99, logPD, num_species) %>% unique()

df_fig2_vioin1$Group <- as.character(df_fig2_vioin1$Group)
df_fig2_vioin1$Group[df_fig2_vioin1$Group == "CYCLOSTOMATA"] <- "Cyclostomata"
df_fig2_vioin1$Group <- factor(df_fig2_vioin1$Group)

df_fig2_vioin2$Group <- as.character(df_fig2_vioin2$Group)
df_fig2_vioin2$Group[df_fig2_vioin2$Group == "CYCLOSTOMATA"] <- "Cyclostomata"
df_fig2_vioin2$Group <- factor(df_fig2_vioin2$Group)

# ==============================================================================
# 3. construct tree structure
# ==============================================================================
df_tree <- data.frame(
  name = c(
    "Biota", "Eukaryota", "Metazoa", "Chordata",
    "Actinopteri","Squamata","Aves","Amphibia","Mammalia","Chondrichthyes","Testudines",
    "Arthropoda", "Pancrustacea", "Insecta",
    "Coleoptera","Lepidoptera","Diptera","Hymenoptera","Hemiptera","Odonata",
    "Malacostraca","Copepoda","Branchiopoda",
    "Chelicerata","Myriapoda",
    "Mollusca","Platyhelminthes","Annelida","Nematoda","Cnidaria","Porifera","Echinodermata","Bryozoa","Tardigrada",
    "Chloroplastida", "Tracheophyta", "Magnoliopsida","Polypodiopsida",
    "Bryophyta","Marchantiophyta","Chlorophyta","Rhodophyta",
    "Fungi", "Ascomycota","Basidiomycota","Microsporidia",
    "SAR", "Stramenopiles","Alveolata","Rhizaria",
    "Archaea","Bacteria"
  ),
  level = c(
    0, 1, 2, 3, 4,4,4,4,4,4,4,
    3, 4, 5, 6,6,6,6,6,6, 5,5,5, 4,4,
    3,3,3,3,3,3,3,3,3,
    2, 3, 4,4, 3,3,3,3,
    2, 3,3,3,
    2, 3,3,3,
    1,1
  )
)

df_tree <- df_tree %>%
  mutate(y = seq(n(), 1)) %>%      
  mutate(x = level * 0.15)         

df_tree$parent <- NA
for (i in 2:nrow(df_tree)) {
  for (j in (i-1):1) {
    if (df_tree$level[j] == df_tree$level[i] - 1) {
      df_tree$parent[i] <- j
      break
    }
  }
}

segments <- df_tree %>%
  filter(!is.na(parent)) %>%
  mutate(
    x_start = df_tree$x[parent],
    y_start = df_tree$y[parent],
    x_end = x,
    y_end = y
  )

pd_table$Group <- as.character(pd_table$Group)
pd_table$Group[pd_table$Group == "CYCLOSTOMATA"] <- "Cyclostomata"
pd_table$Group <- factor(pd_table$Group)

df_violin1_aligned <- df_fig2_vioin1 %>%
  select(-logPD) %>%
  left_join(pd_table, by = "Group") %>%
  left_join(df_tree %>% select(name, tree_y = y), by = c("Group" = "name")) %>%
  filter(!is.na(tree_y)) %>%
  mutate(
    log_ed_scaled = log10(median_ed)
  )



# 4.1 second column of violin plot
df_prep <- df_fig2_vioin2 %>%
  left_join(df_tree %>% select(name, tree_y = y), by = c("Group" = "name")) %>%
  filter(!is.na(tree_y))

# 1. p upper/p lower, label "+" or "-"
df_labels <- df_prep %>%
  rowwise() %>%
  mutate(
    k_obs = num_top_99, 
    p_upper = mean(unlist(random_counts) >= k_obs),
    p_lower = mean(unlist(random_counts) <= k_obs),
    sig_label = case_when(
      p_lower < 0.05 ~ "*-",
      p_upper < 0.05 ~ "*+",
      TRUE ~ ""
    )
  ) %>%
  ungroup()

# 2. Expand the list and convert the count into a percentage relative to the total number of species in each group.
expanded_list <- lapply(1:nrow(df_prep), function(i) {
  row_data <- df_prep[i, ]
  data.frame(
    Group = row_data$Group,
    tree_y = row_data$tree_y,
    random_pct = (unlist(row_data$random_counts) / row_data$num_species) * 100
  )
})
df_violin2_flat <- do.call(rbind, expanded_list)


max_pct <- max(df_violin2_flat$random_pct, na.rm = TRUE)

# layout
violin1_offset <- 2.2  
violin1_width  <- 1.5  
violin2_offset <- 4.2  
violin2_width  <- 2.5   
shape_compress <- 0.5  
df_violin1_aligned$violin_x <- violin1_offset + 
  ((df_violin1_aligned$log_ed_scaled - min(df_violin1_aligned$log_ed_scaled, na.rm=TRUE)) / 
     (max(df_violin1_aligned$log_ed_scaled, na.rm=TRUE) - min(df_violin1_aligned$log_ed_scaled, na.rm=TRUE))) * violin1_width

df_violin2_aligned <- df_violin2_flat %>%
  mutate(
    violin_x = violin2_offset + (random_pct / max_pct) * violin2_width * shape_compress
  )

# Dynamically calculate the coordinates for the first column (Log10 ED) precisely
min_ed_log <- round(min(df_violin1_aligned$log_ed_scaled, na.rm = TRUE), 2)
med_ed_log <- round(median(df_violin1_aligned$log_ed_scaled, na.rm = TRUE), 2)
max_ed_log <- round(max(df_violin1_aligned$log_ed_scaled, na.rm = TRUE), 2)

label_min <- paste0("Log(10)ED Min\n(", min_ed_log, ")")
label_med <- paste0("Log(10)ED Med\n(", med_ed_log, ")")
label_max <- paste0("Log(10)ED Max\n(", max_ed_log, ")")

x_pos_min <- violin1_offset
x_pos_max <- violin1_offset + violin1_width
x_pos_med <- violin1_offset + ((med_ed_log - min_ed_log) / (max_ed_log - min_ed_log)) * violin1_width

# 4.2 Generate the final composite image without a gray background.
p_merged <- ggplot() +
  
  
  geom_segment(data = segments,
               aes(x = x_start, xend = x_start, y = y_start, yend = y_end),
               linewidth = 0.4, color = "grey30") +
  geom_segment(data = segments,
               aes(x = x_start, xend = x_end, y = y_end, yend = y_end),
               linewidth = 0.4, color = "grey30") +
  geom_text(data = df_tree,
            aes(x = x + 0.05, y = y, label = name),
            hjust = 0, size = 2.4, fontface = "italic") +
  
  # violin column 1
  geom_violin(
    data = df_violin1_aligned,
    aes(x = violin_x, y = tree_y, group = tree_y, fill = logPD),
    alpha = 0.8,
    scale = "width",
    color = "grey20",
    size = 0.3
  ) +
  
  # violin2
  geom_violin(
    data = df_violin2_aligned,
    aes(x = violin_x, y = tree_y, group = tree_y),
    fill = "transparent", 
    color = "grey40",     
    scale = "width",
    size = 0.3,
    adjust = 1.2  
  ) +
  
  # add *+  or  *-)
  geom_text(
    data = df_labels,
    aes(
      x = violin2_offset + (violin2_width * shape_compress) + 0.08, 
      y = tree_y, 
      label = sig_label
    ),
    size = 3.5, 
    fontface = "bold",
    color = "black",
    hjust = 0
  ) +
  
  # fill color
  scale_fill_gradient(
    low = "#FFCC00",
    high = "#660066",
    name = "logPD",
    limits = c(global_min, global_max)
  ) +
  
  # label axis
  annotate(
    "text", 
    x = c(x_pos_min, x_pos_med, x_pos_max), 
    y = 0, 
    label = c(label_min, label_med, label_max), 
    size = 2.2, color = "black", vjust = 1
  ) +
  annotate(
    "text", 
    x = c(violin2_offset, violin2_offset + (violin2_width * shape_compress)/2, violin2_offset + (violin2_width * shape_compress)), 
    y = 0, 
    label = c("0%", paste0(round(max_pct / 2, 1), "%"), paste0(round(max_pct, 1), "%")), 
    size = 2.2, color = "black", vjust = 1
  ) +
  
  
  annotate("text", x = violin1_offset + violin1_width/2, y = max(df_tree$y) + 1.2, 
           label = "Log10(ED)", size = 2.5, fontface = "bold", color = "grey20") +
  annotate("text", x = violin2_offset + (violin2_width * shape_compress)/2, y = max(df_tree$y) + 1.2, 
           label = "Random Null Distribution (%)", size = 2.5, fontface = "bold", color = "grey20") +
  
  
  theme_void() +
  theme(
    legend.position = "right",
    legend.key.height = unit(1, "cm"),
    legend.title = element_text(size = 9, face = "bold"),
    legend.text = element_text(size = 8)
  ) +
  xlim(-0.5, violin2_offset + violin2_width + 0.5) 


print(p_merged)



#################################vertebrate in another panel###################################
###############################################################################################
df_vert <- data.frame(
  name = c("Vertebrata","Aves","Mammalia","Squamata","Testudines",
           "Crocodylidae","Amphibia","Actinopterygii",
           "Chondrichthyes","Cyclostomata"),
  level = c(0,1,1,1,1,1,1,1,1,1)
)


df_vert <- df_vert %>%
  mutate(y = seq(n(), 1)) %>%      
  mutate(x = level * 0.15)         


df_vert$parent <- NA
for (i in 2:nrow(df_vert)) {
  for (j in (i-1):1) {
    if (df_vert$level[j] == df_vert$level[i] - 1) {
      df_vert$parent[i] <- j
      break
    }
  }
}

segments <- df_vert %>%
  filter(!is.na(parent)) %>%
  mutate(
    x_start = df_vert$x[parent],
    y_start = df_vert$y[parent],
    x_end = x,
    y_end = y
  )


violin1_offset <- 1.2   
violin1_width  <- 1.5   


violin2_offset <- 3.2   
violin2_width  <- 2.5   
shape_compress <- 0.5   


df_violin1_aligned_vert <- df_fig2_vioin1 %>%
  select(-logPD) %>%
  left_join(pd_table, by = "Group") %>%
  left_join(df_vert %>% select(name, tree_y = y), by = c("Group" = "name")) %>%
  filter(!is.na(tree_y)) %>%
  mutate(
    log_ed_scaled = log10(median_ed),
    violin_x = violin1_offset + ((log_ed_scaled - min(log_ed_scaled, na.rm=TRUE)) / 
                                   (max(log_ed_scaled, na.rm=TRUE) - min(log_ed_scaled, na.rm=TRUE))) * violin1_width
  )


df_prep_vert <- df_fig2_vioin2 %>%
  left_join(df_vert %>% select(name, tree_y = y), by = c("Group" = "name")) %>%
  filter(!is.na(tree_y))

df_labels_vert <- df_prep_vert %>%
  rowwise() %>%
  mutate(
    k_obs = num_top_99, 
    p_upper = mean(unlist(random_counts) >= k_obs),
    p_lower = mean(unlist(random_counts) <= k_obs),
    sig_label = case_when(
      p_lower < 0.05 ~ "*-",
      p_upper < 0.05 ~ "*+",
      TRUE ~ ""
    )
  ) %>%
  ungroup()


expanded_list_vert <- lapply(1:nrow(df_prep_vert), function(i) {
  row_data <- df_prep_vert[i, ]
  data.frame(
    Group = row_data$Group,
    tree_y = row_data$tree_y,
    random_pct = (unlist(row_data$random_counts) / row_data$num_species) * 100
  )
})
df_violin2_flat_vert <- do.call(rbind, expanded_list_vert)


max_pct_vert <- max(df_violin2_flat_vert$random_pct, na.rm = TRUE)
df_violin2_aligned_vert <- df_violin2_flat_vert %>%
  mutate(
    violin_x = violin2_offset + (random_pct / max_pct_vert) * violin2_width * shape_compress
  )


min_ed_log_vert <- round(min(df_violin1_aligned_vert$log_ed_scaled, na.rm = TRUE), 2)
med_ed_log_vert <- round(median(df_violin1_aligned_vert$log_ed_scaled, na.rm = TRUE), 2)
max_ed_log_vert <- round(max(df_violin1_aligned_vert$log_ed_scaled, na.rm = TRUE), 2)

label_min_vert <- paste0("Log(10)ED Min\n(", min_ed_log_vert, ")")
label_med_vert <- paste0("Log(10)ED Med\n(", med_ed_log_vert, ")")
label_max_vert <- paste0("Log(10)ED Max\n(", max_ed_log_vert, ")")

x_pos_min_vert <- violin1_offset
x_pos_max_vert <- violin1_offset + violin1_width
x_pos_med_vert <- violin1_offset + ((med_ed_log_vert - min_ed_log_vert) / (max_ed_log_vert - min_ed_log_vert)) * violin1_width


p_vert <- ggplot() +
  
  
  
  geom_segment(data = segments,
               aes(x = x_start, xend = x_start, y = y_start, yend = y_end),
               linewidth = 0.4, color = "grey30") +
  geom_segment(data = segments,
               aes(x = x_start, xend = x_end, y = y_end, yend = y_end),
               linewidth = 0.4, color = "grey30") +
  geom_text(data = df_vert,
            aes(x = x + 0.05, y = y, label = name),
            hjust = 0, size = 2.4, fontface = "italic") +
  
  
  geom_violin(
    data = df_violin1_aligned_vert,
    aes(x = violin_x, y = tree_y, group = tree_y, fill = logPD),
    alpha = 0.8,
    scale = "width",
    color = "grey20",
    size = 0.3
  ) +
  
  
  geom_violin(
    data = df_violin2_aligned_vert,
    aes(x = violin_x, y = tree_y, group = tree_y),
    fill = "transparent", 
    color = "grey40",     
    scale = "width",
    size = 0.3,
    adjust = 1.2  
  ) +
  
  
  geom_text(
    data = df_labels_vert,
    aes(
      x = violin2_offset + (violin2_width * shape_compress) + 0.08, 
      y = tree_y, 
      label = sig_label
    ),
    size = 3.5, 
    fontface = "bold",
    color = "black",
    hjust = 0
  ) +
  
  
  scale_fill_gradient(
    low = "#FFCC00",
    high = "#660066",
    name = "logPD",
    limits = c(global_min, global_max)
  ) +
  
  
  annotate(
    "text", 
    x = c(x_pos_min_vert, x_pos_med_vert, x_pos_max_vert), 
    y = 0, 
    label = c(label_min_vert, label_med_vert, label_max_vert), 
    size = 2.2, color = "black", vjust = 1
  ) +
  annotate(
    "text", 
    x = c(violin2_offset, violin2_offset + (violin2_width * shape_compress)/2, violin2_offset + (violin2_width * shape_compress)), 
    y = 0, 
    
    label = c("0%", paste0(round(max_pct_vert / 2, 1), "%"), paste0(round(max_pct_vert, 1), "%")), 
    size = 2.2, color = "black", vjust = 1
  ) +
  
  
  annotate("text", x = violin1_offset + violin1_width/2, y = max(df_vert$y) + 0.4, 
           label = "Log10(ED)", size = 2.5, fontface = "bold", color = "grey20") +
  annotate("text", x = violin2_offset + (violin2_width * shape_compress)/2, y = max(df_vert$y) + 0.4, 
           label = "Random Null Distribution (%)", size = 2.5, fontface = "bold", color = "grey20") +
  
  
  theme_void() +
  theme(
    legend.position = "right",
    legend.key.height = unit(1, "cm"),
    legend.title = element_text(size = 9, face = "bold"),
    legend.text = element_text(size = 8)
  ) +
  xlim(-0.5, violin2_offset + violin2_width + 0.5) 


print(p_vert)






####Next: make EDGE & EDGE2 plot
#select Species from IUCN, select specific value from 1000 Jackknife analysis, get a list of name
#1. Estimate EDGE values
#2. get top20/top100 EDGE species


#step1: get top100 EDGE species
ott_name <- select(leaves_table, ott,name)
iucn<- merge(iucn,ott_name,by = "ott",all.x = TRUE)
iucn<-select(iucn,name,status_code)
iucn$name <- gsub("_", "", iucn$name)
EQS_medianED<- read.csv("/Users/alexgjl/Desktop/Dated_tree_paper/ED_median.csv",
                        stringsAsFactors=F, header=T)

EQS_medianED <- EQS_medianED %>%
  separate(Species, into = c("name", "ott"), sep = "_ott") %>%
  mutate(
    name = gsub("_", " ", name),
    name = gsub("\\bsp\\.\\b", "", name),
    name = trimws(name),
    ott = as.numeric(ott)
  )

Evaluated_Species<- merge(iucn,EQS_medianED,by = "name", all.x = TRUE)

Evaluated_Species$status_code[which(Evaluated_Species$status_code == "CR")] <- 4
Evaluated_Species$status_code[which(Evaluated_Species$status_code == "EN")] <- 3
Evaluated_Species$status_code[which(Evaluated_Species$status_code == "VU")] <- 2
Evaluated_Species$status_code[which(Evaluated_Species$status_code == "NT")] <- 1
Evaluated_Species$status_code[which(Evaluated_Species$status_code == "LC")] <- 0
Evaluated_Species <- filter(Evaluated_Species,Evaluated_Species$status_code != "EX")
Evaluated_Species <- filter(Evaluated_Species,Evaluated_Species$status_code != "EW")
Evaluated_Species <- filter(Evaluated_Species,Evaluated_Species$status_code != "DD")

Evaluated_Species$EDGE <- log(1+as.numeric(Evaluated_Species$median_ed))+log(2)*as.numeric(Evaluated_Species$status_code)
Evaluated_Species <- na.omit(Evaluated_Species)
Evaluated_Species_top100 <- Evaluated_Species %>%
  arrange(desc(EDGE)) %>%
  slice_head(n = 100)

EQS_medianED<- read.csv("/Users/alexgjl/Desktop/Dated_tree_paper/ED_median.csv",
                        stringsAsFactors=F, header=T)

EQS_medianED_getname <- EQS_medianED %>%
  mutate(
    name = Species %>%
      str_remove("_ott\\d+$") %>%        
      str_replace("_sp\\.", "") %>%      
      str_replace_all("_", " ")          
  )
EQS_medianED_getname<-select(EQS_medianED_getname,name,Species)
Evaluated_Species_top100<- merge(Evaluated_Species_top100,EQS_medianED_getname,by = "name",all.x = TRUE)
Evaluated_Species_top100<- select(Evaluated_Species_top100,Species,EDGE,median_ed,status_code)

Evaluated_Species_top100 <- Evaluated_Species_top100 %>%
  arrange(desc(EDGE)) %>%
  slice_head(n = 100)
write.csv(x = Evaluated_Species_top100, file = "/Users/alexgjl/Desktop/Dated_tree_paper/top100_EDGE_Species.csv")





IUCN_evaluated_ED_vals<- read.csv("/Users/alexgjl/Desktop/Dated_tree_paper/IUCN_evaluated_species_ED.csv",
                                  stringsAsFactors=F, header=T)

top20_edge_species <- Evaluated_Species_top100[1:20, ]

top20_edge_vals <- IUCN_evaluated_ED_vals[
  match(top20_edge_species$Species, IUCN_evaluated_ED_vals$Species),
]

#rm(IUCN_evaluated_ED_vals)

top20_edge_vals <- top20_edge_vals %>%
  separate(Species, into = c("name", "ott"), sep = "_ott") %>%
  mutate(
    name = gsub("_", " ", name),
    ott = as.numeric(ott)
  )
intersect(top20_edge2_vals$name, top20_edge_vals$name)



#get top100 edge2
median_edge2<- read.csv("/Users/alexgjl/Desktop/Dated_tree_paper/EDGE2_median.csv",
                        stringsAsFactors=F, header=T)
median_edge2 <- median_edge2 %>%
  mutate(
    name = Species %>%
      str_remove("_ott\\d+$") %>%      
      str_replace("_sp\\.", "") %>%      
      str_replace_all("_", " ")          
  )


getname_iucn <-select(leaves_table,name,ott)
iucn_named<- merge(iucn,getname_iucn,by = "ott",all.x = TRUE)
iucn_named <- select(iucn_named,name,status_code)

iucn_ranked_edge2 <- median_edge2[
  median_edge2$name %in% iucn_named$name,
]


iucn_ranked_edge2_top100 <- iucn_ranked_edge2 %>%
  arrange(desc(median_edge2)) %>%
  slice_head(n = 100)

iucn_ranked_edge2_top100_1<- select(iucn_ranked_edge2_top100,Species, median_edge2)

iucn_ranked_edge2_top100_1 <- iucn_ranked_edge2_top100_1 %>%
  arrange(desc(median_edge2)) %>%
  slice_head(n = 100)
#this csv will be used to extract values in python
write.csv(x = iucn_ranked_edge2_top100_1, file = "/Users/alexgjl/Desktop/Dated_tree_paper/top100_EDGE2_Species.csv")










###get top20 values
top100_ed_vals<- read.csv("/Users/alexgjl/Desktop/Dated_tree_paper/top100_species_ED.csv",
                          stringsAsFactors=F, header=T)
top100_ed2_vals<- read.csv("/Users/alexgjl/Desktop/Dated_tree_paper/top100_species_ED2.csv",
                           stringsAsFactors=F, header=T)
#Evaluated_Species_top100
#iucn_ranked_edge2_top100
top20_edge_species <- Evaluated_Species_top100[1:20, ]
top20_edge2_species <- iucn_ranked_edge2_top100[1:20, ]


top20_edge_vals <- top100_ed_vals[
  match(top20_edge_species$Species, top100_ed_vals$Species),
]

top20_edge2_vals <- top100_ed2_vals[
  match(top20_edge2_species$Species, top100_ed2_vals$Species),
]

top20_edge_vals <- top20_edge_vals %>%
  separate(Species, into = c("name", "ott"), sep = "_ott") %>%
  mutate(
    name = gsub("_", " ", name),
    ott = as.numeric(ott)
  )
top20_edge2_vals <- top20_edge2_vals %>%
  separate(Species, into = c("name", "ott"), sep = "_ott") %>%
  mutate(
    name = gsub("_", " ", name),
    ott = as.numeric(ott)
  )

#top20_edge_vals have 1003 columns, 3 extras are status_code, name and ott
write.csv(x = top20_edge2_vals, file = "/Users/alexgjl/Desktop/Dated_tree_paper/top20_EDGE2_values.csv")
write.csv(x = top20_edge_vals, file = "/Users/alexgjl/Desktop/Dated_tree_paper/top20_EDGE_values.csv")

#Get top20 EDGE values

###################################################EDGE FIG################################################


top20_edge_vals <-  read.csv("/Users/alexgjl/Desktop/Dated_tree_paper/top20_EDGE_values.csv",
                             stringsAsFactors=F, header=T)
top20_edge2_vals <-  read.csv("/Users/alexgjl/Desktop/Dated_tree_paper/top20_EDGE2_values.csv",
                              stringsAsFactors=F, header=T)




top20_edge_vals$Group <- c("Vertebrata", "Invertebrata","Invertebrata","Plantae","Invertebrata",
                           "Invertebrata",
                           "Plantae","Plantae","Plantae","Vertebrata","Invertebrata",
                           "Invertebrata","Invertebrata","Invertebrata","Plantae",
                           "Invertebrata",
                           "Vertebrata","Invertebrata","Plantae","Plantae")


top20_edge2_vals$Group <-  c("Vertebrata", "Invertebrata","Invertebrata","Plantae","Invertebrata",
                             "Invertebrata","Plantae","Plantae","Plantae","Vertebrata",
                             "Invertebrata","Invertebrata","Invertebrata","Invertebrata","Plantae",
                             "Plantae","Invertebrata","Plantae","Invertebrata","Vertebrata")



top20_edge_vals <- select(top20_edge_vals,-X,-ott)

top20_edge2_vals <- select(top20_edge2_vals,-X,-ott)
top20_edge_vals <- top20_edge_vals[, c(
  1,                              
  (ncol(top20_edge_vals)-1):ncol(top20_edge_vals),          
  2:(ncol(top20_edge_vals)-2)                  
)]

top20_edge2_vals <- top20_edge2_vals[, - (ncol(top20_edge2_vals) - 2)]


edge_top20_l <- pivot_longer(
  top20_edge_vals,
  cols = 4:1003,
  names_to = "tree",
  values_to = "ED"
)

edge2_top20_l <- pivot_longer(
  top20_edge2_vals,
  cols = 2:1001,
  names_to = "tree",
  values_to = "ED2"
)

edge_top20_l$Group <- factor(edge_top20_l$Group,levels = c("Plantae","Invertebrata","Vertebrata"))
edge2_top20_l$Group <- factor(edge2_top20_l$Group,levels = c("Plantae","Invertebrata","Vertebrata"))

edge_top20_l<- select(edge_top20_l,name,Group,ED)
edge2_top20_l<- select(edge2_top20_l,name,Group,ED2)

edge_top20_l$name <- factor(edge_top20_l$name,levels = c( "Vandiemenia ratkowskiana" ,"Podadenia sapida","Mictocaris halope" ,"Latonia nigriventer",
                                                          "Speleoperipatus spelaeus","Dubautia microcephala","Lancaris kumariae","Syncaris pacifica","Acostaea rivoli",
                                                          "Engaeus spinicaudatus" ,"Anoxypristis cuspidata","Dubautia imbricata" ,"Cycas chamaoensis","Geothallus tuberosus",
                                                          "Eostemmiulus caecus",
                                                          "Nanocopia minuta","Mankyua chejuensis" ,"Antrisocopia prehensilis",
                                                          "Helioseris cucullata","Latimeria chalumnae sp."))

edge2_top20_l$name <- factor(edge2_top20_l$name,levels = c(  "Nematogenys inermis","Aneuretus simoni","Medusagyne oppositifolia","Mictocaris halope","Vandiemenia ratkowskiana",
                                                             "Latonia nigriventer",
                                                             "Latimeria menadoensis sp.",
                                                             "Dubautia microcephala" ,"Dubautia imbricata","Engaeus spinicaudatus",
                                                             "Syncaris pacifica",
                                                             "Anoxypristis cuspidata","Cycas chamaoensis",
                                                             "Geothallus tuberosus","Eostemmiulus caecus",
                                                             "Nanocopia minuta", "Mankyua chejuensis","Antrisocopia prehensilis","Helioseris cucullata",
                                                             "Latimeria chalumnae sp."))




Box_EDGE <- ggplot(edge_top20_l, aes(y = name, x = ED,fill = Group)) + 
  geom_boxplot(alpha = 0.6)+theme_classic()+xlim(0,500)+ theme(text = element_text(size = 15))+labs(x="ED (Myr)",y="")+
  scale_fill_manual(values = c(Plantae= "grey",Vertebrata = "#ea9c9d",
                               Invertebrata = "#4d97cd"))



Box_EDGE2 <- ggplot(edge2_top20_l, aes(y = name, x = ED2,fill = Group)) + 
  geom_boxplot(alpha = 0.6)+theme_classic()+xlim(0,500)+ theme(text = element_text(size = 15))+labs(x="ED2",y="")+
  scale_fill_manual(values = c(Plantae= "grey",Vertebrata = "#ea9c9d",
                               Invertebrata = "#4d97cd"))

Box_EDGE
Box_EDGE2
#Anoxypristis cuspidata: Henri Farrugio, Jean Marin, CC BY 4.0
#Scott Loarie: Syncaris pacifica, public domain
#Acostaea rivolii: , Deshayes, CC0
#Helioseris cucullata :robin_dive_diva, CC-BY

#Medusagyne oppositifolia: James Hardcastle (CC BY)
#Nematogenys inermis: J. H. Richard, public domain
#Aneuretus simoni: Jonghyun Park, CC BY 4.0



####################################  remake figure s2-7  ################################################

#figure s4: distribution of ed score

grouped_species<- select(grouped_species_in_clade_final4,Group,Species)
groups <- unique(grouped_species$Group)
out_dir <- "/Users/alexgjl/Desktop/Dated_tree_paper/Clades_included_in_this_study"

for (g in groups) {
  sub_df <- grouped_species[grouped_species$Group == g, ]
  file_name <- paste0(gsub("[^A-Za-z0-9_]", "_", g), ".csv")
  write.csv(sub_df,
            file = file.path(out_dir, file_name),
            row.names = FALSE)
}



###figure s4

pd_median <- read.csv("/Users/alexgjl/Desktop/Dated_tree_paper/PD_median.csv",
                      stringsAsFactors=F, header=T)

library(ape)
tree_files <- list.files(
  "/Volumes/PortableSSD/equal_splits_both",
  pattern = "\\.tre$",
  full.names = TRUE
)
tree_file <- tree_files[1]
tree <- read.tree(tree_file)


node_ott <- as.numeric(str_extract(tree$node.label, "(?<=ott)\\d+"))
selected_ott <- pd_median$ott
selected_node_idx <- match(selected_ott, node_ott)
n_tips <- length(tree$tip.label)
selected_nodes <- selected_node_idx + n_tips


tip_count <- rep(1, length(tree$tip.label))
Ntip <- length(tree$tip.label)
Nnode <- tree$Nnode
total_nodes <- Ntip + Nnode
node_size <- rep(0, total_nodes)
node_size[1:Ntip] <- 1
edge <- tree$edge
for (i in nrow(edge):1) {
  parent <- edge[i, 1]
  child <- edge[i, 2]
  
  node_size[parent] <- node_size[parent] + node_size[child]
}
pd_median$species_richness <- node_size[selected_nodes]
pd_median$logpd <- log10(pd_median$median_pd)
pd_median$logrich <- log10(pd_median$species_richness)


scatter_PD <- ggplot(data = pd_median, aes(x = logrich, y = logpd)) + 
  geom_point(alpha = .5) +
  theme_classic() +
  theme(
    panel.grid = element_blank(),
    text = element_text(size = 17),  
    legend.title = element_text(size = 30),   
    axis.text.x = element_text(size = 17),  
    axis.text.y = element_text(size = 17)   
  ) +
  xlab("Log10 (Richness)") +
  ylab("Log10 (PD) Myr")
scatter_PD


cor_result <- cor.test(pd_median$logrich, pd_median$logpd, method = "spearman")
round(cor_result$estimate, 3)
signif(cor_result$p.value)



##############Figure S6-7
#Supplementary Figure 6
library(gridExtra)

EQS_medianED<- read.csv("/Users/alexgjl/Desktop/Dated_tree_paper/ED_median.csv",
                        stringsAsFactors=F, header=T)

EQS_medianED$logED <- log10(EQS_medianED$median_ed)
install.packages("readxl")
library(readxl)
setwd("/Users/alexgjl/Desktop/master/项目2/论文修改_回答reviewer问题/")
excel_file <- "EDGE-Lists-2020_website.xlsx"
sheets <- excel_sheets(excel_file)
sheet_data <- list()
for (sheet in sheets) {
  data <- read_excel(excel_file, sheet = sheet)
  data$sheet_name <- sheet  
  sheet_data[[sheet]] <- data
}

for (i in seq_along(sheet_data)) {
  assign(paste0("df", i), sheet_data[[sheets[i]]])
}



get_ED <- function(df) {
  if ("species" %in% colnames(df)) {
    df$pre_median_ED <- log(df$ED,10)
    df$name <- df$species
    df1 <- select(df,name,ED,pre_median_ED)
    return(df1)
  } 
  else if ("Species" %in% colnames(df)) {
    df$species <- df$Species
    df$pre_median_ED <- log(df$ED,10)
    df$name <- df$species
    df1 <- select(df,name,ED,pre_median_ED)
    return(df1)
  }
}




arrange_precalcukated_ED <- function(df) {
  df$pre_median_ED <- log(df$ED,10)
  df$name <- df$Species
  df1 <- select(df,name,pre_median_ED)
  return(df1)
}


ED_amp <- get_ED(df2)
ED_aves <- get_ED(df4)
ED_mam <- get_ED(df6)
df_test <- filter(df8,df8$Order == "Testudines")
df_squa <-filter(df8,df8$Order == "Squamata")
df_croc <- filter(df8,df8$Order == "Crocodylia")
ED_test <- get_ED(df_test)
ED_squa<- get_ED(df_squa)
ED_croc<- get_ED(df_croc)
ED_coral <- get_ED(df10)
ED_chon <- get_ED(df12)
ED_gymn<-get_ED(df14)
pre_ed_all<- rbind(ED_amp,ED_aves,ED_mam,ED_test,
                   ED_squa,ED_croc,ED_coral,ED_chon,ED_gymn)





#introduce percentage error here
#get name for resolved ED:

EQS_medianED$name <- sub("_ott.*", "", EQS_medianED$Species)      
EQS_medianED$name <- gsub("_", " ", EQS_medianED$name)  
EQS_medianED$name <- gsub("\\bsp\\.\\s*", "", EQS_medianED$name)
ED_all <- select(EQS_medianED,name,logED)


merge_ED <- function(df) {
  df1 <- merge(df,ED_all,how = "left", on = name)
  df1$pre_ED <- 10**(df1$pre_median_ED)
  df1$per_error <- abs(10**df1$logED-df1$pre_ED)/df1$pre_ED
  return(df1)
}

ED_amp1 <- merge_ED(ED_amp)
ED_aves1 <- merge_ED(ED_aves)
ED_mam1 <- merge_ED(ED_mam)
ED_test1 <- merge_ED(ED_test)
ED_squa1<- merge_ED(ED_squa)
ED_croc1<- merge_ED(ED_croc)
ED_coral1 <- merge_ED(ED_coral)
ED_chon1 <- merge_ED(ED_chon)
ED_gymn1<-merge_ED(ED_gymn)




scatter_comparison <- function(df, input_clade) {
  scatter_ED <- ggplot(data = df, aes(x = pre_median_ED, y = logED, color = per_error)) +
    geom_point(alpha = 0.6) +
    theme_classic() +
    scale_color_gradient(low = "#A50F15", high = "#FCBBA1") +  
    theme(panel.grid = element_blank()) +  
    xlim(0, 2.5) + 
    ylim(0, 2.5) + 
    labs(
      x = "ED estimated in previous research",
      y = "ED calculated in this study",
      title = input_clade
    ) +
    geom_abline(intercept = 0, slope = 1, color = "#08306B", linewidth = 1) +  
    stat_poly_eq(
      aes(label = paste(..eq.label.., ..adj.rr.label.., ..p.value.label.., sep = "~~~~")),
      formula = y ~ x,
      parse = TRUE,
      size = 3.5
    )
  
  return(scatter_ED)
}


scatter_comparison(ED_gymn1,"Gymnosperm")
scatter_comparison(ED_coral1,"Cnidaria")
scatter_comparison(ED_chon1,"Chondrichthyes")
scatter_comparison(ED_amp1,"Amphibian")
scatter_comparison(ED_croc1,"Crocodylia")
scatter_comparison(ED_test1,"Testudines")
scatter_comparison(ED_squa1,"Squamata")
scatter_comparison(ED_mam1,"Mammalia")
scatter_comparison(ED_aves1,"Aves")

#figure S6
grid.arrange( scatter_comparison(ED_gymn1,"Gymnosperm"),scatter_comparison(ED_coral1,"Cnidaria"),
              scatter_comparison(ED_chon1,"Chondrichthyes"),scatter_comparison(ED_amp1,"Amphibian"),
              scatter_comparison(ED_croc1,"Crocodylia"),scatter_comparison(ED_test1,"Testudines"),
              scatter_comparison(ED_squa1,"Squamata"),scatter_comparison(ED_mam1,"Mammalia"),
              scatter_comparison(ED_aves1,"Aves"),
              ncol = 3)




#compare edge
get_EDGE <- function(df) {
  if ("species" %in% colnames(df)) {
    df$pre_median_ED <- log(df$ED,10)
    df$name <- df$species
    df1 <- select(df,name,GE,EDGE)
    return(df1)
  } 
  else if ("Species" %in% colnames(df)) {
    df$species <- df$Species
    df$pre_median_ED <- log(df$ED,10)
    df$name <- df$species
    df1 <- select(df,name,GE,EDGE)
    return(df1)
  }
}

EDGE_amp <- get_EDGE(df3)
EDGE_aves <- get_EDGE(df5)
EDGE_mam <- get_EDGE(df7)

df_test <- filter(df9,df9$Order == "Testudines")
df_squa <-filter(df9,df9$Order == "Squamata")
df_croc <- filter(df9,df9$Order == "Crocodylia")
EDGE_test <- get_EDGE(df_test)
EDGE_squa<- get_EDGE(df_squa)
EDGE_croc<- get_EDGE(df_croc)
EDGE_coral <- get_EDGE(df11)
EDGE_chon <- get_EDGE(df13)
EDGE_gymn<-get_EDGE(df15)


calculate_new_EDGE <- function(df) {
  df1 <-merge(df,ED_all,how = "left",on = name)
  df1$new_ED <- 10**(df1$logED)
  df1$new_EDGE <- log(1+as.numeric(df1$new_ED))+log(2)*as.numeric(df1$GE)
  df1$per_error <- abs(df1$new_EDGE-df1$EDGE)/df1$EDGE
  return(df1)
}


EDGE_gymn <- calculate_new_EDGE(EDGE_gymn)
EDGE_coral<- calculate_new_EDGE(EDGE_coral)
EDGE_chon <- calculate_new_EDGE(EDGE_chon)
EDGE_amp<- calculate_new_EDGE(EDGE_amp)
EDGE_croc<- calculate_new_EDGE(EDGE_croc)
EDGE_test<- calculate_new_EDGE(EDGE_test)
EDGE_squa<- calculate_new_EDGE(EDGE_squa)
EDGE_mam<- calculate_new_EDGE(EDGE_mam)
EDGE_aves<- calculate_new_EDGE(EDGE_aves)

#recalculate EDGE to use the same IUCN_category as previous estimation.
#make figure
scatter_comparison_EDGE <- function(df, input_clade) {
  scatter_EDGE <- ggplot(data = df, aes(x=as.numeric(EDGE), y = as.numeric(new_EDGE), color = per_error)) +
    geom_point(alpha = 0.6) +
    theme_classic() +
    scale_color_gradient(low = "#A50F15", high = "#FCBBA1") + 
    theme(panel.grid = element_blank()) +  
    
    labs(
      x = "EDGE estimated in previous research",
      y = "EDGE calculated in this study",
      title = input_clade
    ) +
    geom_abline(intercept = 0, slope = 1, color = "#08306B", linewidth = 1) + 
    stat_poly_eq(
      aes(label = paste(..eq.label.., ..adj.rr.label.., ..p.value.label.., sep = "~~~~")),
      formula = y ~ x,
      parse = TRUE,
      size = 3.5
    )+xlim(0,8)+ylim(0,8)
  
  return(scatter_EDGE)
}

scatter_comparison_EDGE(EDGE_gymn,"Gymnosperm")
scatter_comparison_EDGE(EDGE_coral,"Cnidaria")
scatter_comparison_EDGE(EDGE_chon,"Chondrichthyes")
scatter_comparison_EDGE(EDGE_amp,"Amphibian")
scatter_comparison_EDGE(EDGE_croc,"Crocodylia")
scatter_comparison_EDGE(EDGE_test,"Testudines")
scatter_comparison_EDGE(EDGE_squa,"Squamata")
scatter_comparison_EDGE(EDGE_mam,"Mammalia")
scatter_comparison_EDGE(EDGE_aves,"Aves")


grid.arrange(scatter_comparison_EDGE(EDGE_gymn,"Gymnosperm"), scatter_comparison_EDGE(EDGE_coral,"Cnidaria"),
             scatter_comparison_EDGE(EDGE_chon,"Chondrichthyes"), scatter_comparison_EDGE(EDGE_chon,"Chondrichthyes"),
             scatter_comparison_EDGE(EDGE_croc,"Crocodylia"), scatter_comparison_EDGE(EDGE_test,"Testudines"),
             scatter_comparison_EDGE(EDGE_squa,"Squamata"), scatter_comparison_EDGE(EDGE_mam,"Mammalia"),
             scatter_comparison_EDGE(EDGE_aves,"Aves"), ncol = 3)


#######make species in clade final 4
multimodels <- filter(species_in_clade_final4,species_in_clade_final4$Group %in% c("Basidiomycota","Alveolata","Rhizaria" ,"Stramenopiles" ,"Porifera",
                                                                                   "Echinodermata","Bryozoa","Marchantiophyta","Tardigrada" ))

multimodels<- select(multimodels,Group,Species)

#write.csv(x = multimodels, file = "/Users/alexgjl/Desktop/Dated_tree_paper/multimodels.csv")




multimodel_edvals <-read.csv("/Users/alexgjl/Desktop/Dated_tree_paper/sampled_100_trees.csv",
                             stringsAsFactors=F, header=T)
multimodel_edvals_clean <- multimodel_edvals %>%
  dplyr::select(
    Group,
    Species,
    dplyr::starts_with("X")
  )  

ED_long <- multimodel_edvals_clean %>%
  pivot_longer(
    cols = starts_with("X"),
    names_to = "tree",
    values_to = "ED"
  )
ggplot(ED_long, aes(x = log10(ED), group = tree)) +
  geom_density(alpha = 0.2) +
  facet_wrap(~Group, scales = "free") +
  
  theme_classic() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )



#########make tabel 1###############


pd_vals <- read.csv("/Users/alexgjl/Desktop/Dated_tree_paper/PD_median.csv",
                    stringsAsFactors=F, header=T)
epd_vals <- read.csv("/Users/alexgjl/Desktop/Dated_tree_paper/ePD_median.csv",
                     stringsAsFactors=F, header=T)
epd_vals$threatened_pd <- epd_vals$median_pd
epd_vals<- select(epd_vals,ott,threatened_pd)
PD_and_epd<- merge(pd_vals,epd_vals,by = "ott",all.x = TRUE)
get_nodename<- select(nodes_table, ott,name)
named_PD_and_epd<- merge(PD_and_epd,get_nodename,by = "ott",all.x = TRUE)

named_PD_and_epd1 <- named_PD_and_epd %>%
  filter(
    name %in% unique(species_in_clade_final4$Group)|
      ott %in% c(278108, 996421, 5246039, 878953, 844192,278108,335582)
  )

named_PD_and_epd1 <- named_PD_and_epd1 %>%  mutate(
  name = case_when(ott == 996421  ~ "Archaea",ott == 5246039 ~ "SAR",ott == 335582 ~ "Crocodylidae",ott == 878953 ~"Rhodophyta",TRUE ~ name))

named_PD_and_epd1$name[which(named_PD_and_epd1$name == "CHONDRICHTHYES")] <- "Chondrichthyes"
named_PD_and_epd1$name[which(named_PD_and_epd1$name == "Eubacteria")] <- "Bacteria"
named_PD_and_epd1$name[which(named_PD_and_epd1$name == "CYCLOSTOMATA")] <- "Cyclostomata"

ott_name <- select(leaves_table, ott,name)
iucn<- merge(iucn,ott_name,by = "ott",all.x = TRUE)
iucn<-select(iucn,name,status_code)
iucn$name <- gsub("_", "", iucn$name)
species_within_clade <- select(species_in_clade_final4,Group,Species)
species_within_clade <- species_within_clade %>%
  mutate(
    name = Species %>%
      str_remove("_ott\\d+$") %>%     
      str_replace_all("_", " ") %>%   
      str_replace("\\bsp\\.\\b\\s*", "") %>%  
      str_squish()                    
  )

iucn_ranked<- filter(iucn,iucn$status_code != "DD")

species_within_clade<- select(species_within_clade,name,Group)



Evaluated_proportion <- species_within_clade %>%
  mutate(
    is_iucn_ranked = name %in% iucn_ranked$name,
    threatened = name %in% iucn_ranked$name[!iucn_ranked$status_code %in% c("LC", "NT")]
  )

iucn_ranked<- unique(iucn_ranked)


df <- species_within_clade %>%
  left_join(iucn_ranked, by = "name")

Evaluated_proportion <- df %>%
  distinct(Group, name, .keep_all = TRUE) %>% 
  group_by(Group) %>%
  summarise(
    total_species = n(),  
    
    iucn_ranked_n = sum(!is.na(status_code)),
    prop_iucn_ranked = iucn_ranked_n / total_species,
    
    threatened_n = sum(status_code %in% c("VU", "EN", "CR"), na.rm = TRUE),
    prop_threatened = threatened_n / total_species
  )

Evaluated_proportion$Group[which(Evaluated_proportion$Group == "CYCLOSTOMATA")] <- "Cyclostomata"
named_PD_and_epd1$Group <- named_PD_and_epd1$name
named_PD_and_epd3 <- merge(named_PD_and_epd1,Evaluated_proportion,by = "Group",all.x = TRUE)
named_PD_and_epd3$Ratio_PD_to_Richness <- named_PD_and_epd3$median_pd/named_PD_and_epd3$total_species

named_PD_and_epd3<- select(named_PD_and_epd3,Group,median_pd,threatened_pd,total_species,Ratio_PD_to_Richness,prop_iucn_ranked,prop_threatened)

named_PD_and_epd3$median_pd <- named_PD_and_epd3$median_pd/1000
named_PD_and_epd3$threatened_pd<- named_PD_and_epd3$threatened_pd/1000
named_PD_and_epd3$prop_iucn_ranked <- 100*named_PD_and_epd3$prop_iucn_ranked
named_PD_and_epd3$prop_threatened <- 100*named_PD_and_epd3$prop_threatened



named_PD_and_epd3$median_pd <- round(
  named_PD_and_epd3$median_pd,
  2
)
named_PD_and_epd3$threatened_pd <- round(
  named_PD_and_epd3$threatened_pd,
  2
)


named_PD_and_epd3$Ratio_PD_to_Richness <- round(
  named_PD_and_epd3$Ratio_PD_to_Richness,
  2
)
named_PD_and_epd3$prop_iucn_ranked <- round(
  named_PD_and_epd3$prop_iucn_ranked,
  2
)
named_PD_and_epd3$prop_threatened <- round(
  named_PD_and_epd3$prop_threatened,
  2
)
named_PD_and_epd3


greater_cont<- filter(named_PD_and_epd3,named_PD_and_epd3$Ratio_PD_to_Richness>20)
greater_cont$Group
smaller_cont<- filter(named_PD_and_epd3,named_PD_and_epd3$Ratio_PD_to_Richness<7.5)
smaller_cont$Group


named_PD_and_epd3$prop_threatened_PD <- named_PD_and_epd3$threatened_pd/named_PD_and_epd3$median_pd
named_PD_and_epd3$prop_threatened_PD <- 100*named_PD_and_epd3$prop_threatened_PD
named_PD_and_epd3$prop_threatened_PD <- round(
  named_PD_and_epd3$prop_threatened_PD,
  2
)

write.csv(x = named_PD_and_epd3, file = "/Users/alexgjl/Desktop/Dated_tree_paper/Table1.csv")



#QQ plot
#figure s3 Supplementary figure 3


ED_for_qq_plot <- select(species_in_clade_final4,Species,Group,median_ed)
ED_for_qq_plot$logED <- log10(ED_for_qq_plot$median_ed)


ED_for_qq_plot$Group[which(ED_for_qq_plot$Group == "CYCLOSTOMATA")] <- "Cyclostomata"
#average ED plot

library(ggplot2)

# ==========================================
# select 1000 dots from each clade
# ==========================================
ED_qq_clean <- ED_for_qq_plot %>%
  filter(!is.na(logED)) %>%                      
  group_by(Group) %>%
  slice_sample(n = 1000, replace = FALSE) %>%    
  ungroup()

# ==========================================
# make qq plots
# ==========================================
p_all <- ggplot(ED_qq_clean, aes(sample = logED)) +
  stat_qq(size = 0.3, color = "grey30", alpha = 0.4) + 
  stat_qq_line(color = "#D95F02", linewidth = 0.4) +   
  facet_wrap(~Group, scales = "free", ncol = 8) +     
  labs(
    title = "Q-Q Plots for Log10(ED) — All 55 Clades Baseline Distribution",
    x = "Theoretical Quantiles",
    y = "Sample Quantiles"
  ) +
  theme_classic(base_size = 9) + 
  theme(                          
    strip.text = element_text(face = "italic", size = 7.5, color = "white"), 
    strip.background = element_rect(fill = "grey40"),
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    panel.grid.major = element_line(color = "grey95"), 
    panel.grid.minor = element_blank()
  )
p_all

###############################################mean_ED box################################################
################################################figure s4 Supplementary figure 4##########################


mean_ED <- read.csv("/Users/alexgjl/Desktop/Dated_tree_paper/group_tree_mean_ed_FINAL.csv",
                    stringsAsFactors=F, header=T)


df_color2clade <- data.frame(info)
df_color2clade$pd_table.Group[which(df_color2clade$pd_table.Group == "CYCLOSTOMATA")] <- "Cyclostomata"
df_color2clade$Group<- df_color2clade$pd_table.Group
df_color2clade$color<- df_color2clade$pd_table.color
df_color2clade<- select(df_color2clade,Group,color)
mean_ED<- merge(mean_ED,df_color2clade,by = "Group",all.x = TRUE)
mean_ED$logED <- log(mean_ED$mean_ed)

colnames(mean_ED)


library(dplyr)
library(ggplot2)

df_tree <- df_tree %>%
  mutate(y = seq(n(), 1)) %>%      
  mutate(x = level * 0.15)         

df_tree$parent <- NA
for (i in 2:nrow(df_tree)) {
  for (j in (i-1):1) {
    if (df_tree$level[j] == df_tree$level[i] - 1) {
      df_tree$parent[i] <- j
      break
    }
  }
}

segments <- df_tree %>%
  filter(!is.na(parent)) %>%
  mutate(
    x_start = df_tree$x[parent],
    y_start = df_tree$y[parent],
    x_end = x,
    y_end = y
  )

box_offset <- 1.5   
box_width  <- 4.0  

df_boxplot_aligned <- mean_ED %>%
  
  left_join(df_tree %>% select(name, tree_y = y), by = c("Group" = "name")) %>%
  filter(!is.na(tree_y)) %>%
  mutate(
    box_x = box_offset + ((logED - min(logED, na.rm=TRUE)) / 
                            (max(logED, na.rm=TRUE) - min(logED, na.rm=TRUE))) * box_width
  )


min_ed_log <- round(min(df_boxplot_aligned$logED, na.rm = TRUE), 2)
med_ed_log <- round(median(df_boxplot_aligned$logED, na.rm = TRUE), 2)
max_ed_log <- round(max(df_boxplot_aligned$logED, na.rm = TRUE), 2)

label_min <- paste0("Log(10)ED Min\n(", min_ed_log, ")")
label_med <- paste0("Log(10)ED Med\n(", med_ed_log, ")")
label_max <- paste0("Log(10)ED Max\n(", max_ed_log, ")")

x_pos_min <- box_offset
x_pos_max <- box_offset + box_width
x_pos_med <- box_offset + ((med_ed_log - min_ed_log) / (max_ed_log - min_ed_log)) * box_width

p_box_tree <- ggplot() +
  geom_segment(data = segments,
               aes(x = x_start, xend = x_start, y = y_start, yend = y_end),
               linewidth = 0.4, color = "grey30") +
  geom_segment(data = segments,
               aes(x = x_start, xend = x_end, y = y_end, yend = y_end),
               linewidth = 0.4, color = "grey30") +
  geom_text(data = df_tree,
            aes(x = x + 0.05, y = y, label = name),
            hjust = 0, size = 2.4, fontface = "italic") +
  geom_boxplot(
    data = df_boxplot_aligned,
    aes(x = box_x, y = tree_y, group = tree_y, fill = color), # 👈 映射自带的 color 列
    outlier.size = 0.5,       
    outlier.alpha = 0.5,      
    width = 0.5,              
    color = "grey20",         
    linewidth = 0.35,
    orientation = "y"         
  ) +
  scale_fill_identity() +
  annotate(
    "text", 
    x = c(x_pos_min, x_pos_med, x_pos_max), 
    y = 0, 
    label = c(label_min, label_med, label_max), 
    size = 2.2, color = "black", vjust = 1
  ) +
  annotate(
    "text", 
    x = box_offset + box_width/2, 
    y = max(df_tree$y) + 1.2, 
    label = "Log10(ED)", 
    size = 2.5, 
    fontface = "bold", 
    color = "grey20"
  ) +
  theme_void() +
  xlim(-0.5, box_offset + box_width + 0.5)
print(p_box_tree)





#get median ED for different clades
getED <- select(species_in_clade_final4,Species,  median_ed,Group )

quantile(log10(EQS_medianED$median_ed), probs = c(0.95, 0.99))



df_group_median <- getED %>%
  group_by(Group) %>%
  summarise(
    #get median value of median ED
    group_median_ed = median(median_ed, na.rm = TRUE) 
  )




################supplementary figure 1

#####discover bimodal distribution
ed_test <- filter(species_in_clade_final4,species_in_clade_final4$Group == "Testudines")
ed_test<- unique(ed_test)
ed_hyme<- filter(species_in_clade_final4,species_in_clade_final4$Group == "Hymenoptera")
ed_lepi<- filter(species_in_clade_final4,species_in_clade_final4$Group == "Lepidoptera")
ed_croc<- filter(species_in_clade_final4,species_in_clade_final4$Group == "Crocodylidae")
ed_cyclo<- filter(species_in_clade_final4,species_in_clade_final4$Group == "CYCLOSTOMATA")
ed_chon<- filter(species_in_clade_final4,species_in_clade_final4$Group == "Chondrichthyes")
ed_test<- select(ed_test,Species,median_ed)
ed_test$logED<- log10(ed_test$median_ed) 
ed_hyme<- select(ed_hyme,Species,median_ed)
ed_hyme$logED<- log10(ed_hyme$median_ed) 
ed_lepi<- select(ed_lepi,Species,median_ed)
ed_lepi$logED<- log10(ed_lepi$median_ed) 
ed_croc<- select(ed_croc,Species,median_ed)
ed_croc$logED<- log10(ed_croc$median_ed) 
ed_cyclo<- select(ed_cyclo,Species,median_ed)
ed_cyclo$logED<- log10(ed_cyclo$median_ed) 
ed_chon<- select(ed_chon,Species,median_ed)
ed_chon$logED<- log10(ed_chon$median_ed) 
install.packages("mclust")
library(mclust)
library(ggplot2)
library(dplyr)
library(cowplot)


clade_list <- list(
  Hymenoptera    = ed_hyme,
  Lepidoptera    = ed_lepi,
  Crocodylidae   = ed_croc,
  Cyclostomata   = ed_cyclo,
  Chondrichthyes = ed_chon,
  Testudines     = ed_test
)

plot_list <- list()
species_peaks_list <- list()



#find peaks for bimodal distribution clades
for (clade_name in names(clade_list)) {
  
  cat("Provessing Clade:", clade_name, "...\n")
  df_sub <- clade_list[[clade_name]]
  
  df_sub <- df_sub %>% filter(!is.na(logED))
  
  set.seed(123)
  gmm <- Mclust(df_sub$logED, G = 2:3, modelNames = "E", verbose = FALSE)
  
  df_sub$Peak_Raw <- paste0("Peak_", gmm$classification)
  peak_means <- tapply(df_sub$logED, df_sub$Peak_Raw, mean)
  ordered_peaks <- names(sort(peak_means))
  
  peak_labels <- if(length(ordered_peaks) == 2) c("Low_ED_Peak", "High_ED_Peak") else c("Low_ED_Peak", "Mid_ED_Peak", "High_ED_Peak")
  names(peak_labels) <- ordered_peaks
  
  df_sub$Peak_Assignment <- peak_labels[df_sub$Peak_Raw]
  df_sub$Clade <- clade_name
  species_peaks_list[[clade_name]] <- df_sub %>% select(Species, Clade, median_ed, logED, Peak_Assignment)
  p <- ggplot(df_sub, aes(x = logED)) +
    geom_density(fill = "grey90", color = NA, alpha = 0.5) +
    geom_density(aes(fill = Peak_Assignment), alpha = 0.7, color = "white", size = 0.3) +
    scale_fill_viridis_d(option = "plasma", name = "Sub-peaks") +
    labs(
      title = paste0(clade_name, " (N = ", nrow(df_sub), ")"),
      x = "log10(Evolutionary Distinctiveness)",
      y = "Density"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
      panel.grid.minor = element_blank(),
      legend.position = "bottom"
    )+theme_classic()
  
  plot_list[[clade_name]] <- p
}



bi_test<- species_peaks_list$Testudines
bi_cycl<- species_peaks_list$Cyclostomata
bi_hym<- species_peaks_list$Hymenoptera
bi_lep<- species_peaks_list$Lepidoptera
bi_cro<- species_peaks_list$Crocodylidae
bi_cho<- species_peaks_list$Chondrichthyes

bi_test_high <- filter(bi_test,bi_test$Peak_Assignment =="High_ED_Peak")
bi_test_low <- filter(bi_test,bi_test$Peak_Assignment =="Low_ED_Peak")
plot_list$Testudines

#not contributed by certain sub-clade
bi_cycl_high<- filter(bi_cycl,bi_cycl$Peak_Assignment =="High_ED_Peak")
bi_cycl_low<- filter(bi_cycl,bi_cycl$Peak_Assignment =="Low_ED_Peak")

bi_hym_low<- filter(bi_hym,bi_hym$Peak_Assignment =="Low_ED_Peak")

bi_lep_low<- filter(bi_lep,bi_lep$Peak_Assignment =="Low_ED_Peak")


bi_cro_low<- filter(bi_cro,bi_cro$Peak_Assignment =="Low_ED_Peak")
bi_cro_high<- filter(bi_cro,bi_cro$Peak_Assignment =="High_ED_Peak")

bi_cho_low<- filter(bi_cho,bi_cho$Peak_Assignment =="Low_ED_Peak")
bi_cho_high<- filter(bi_cho,bi_cho$Peak_Assignment =="High_ED_Peak")



n_tips <- length(tree$tip.label)
chalc_node_idx <- grep("Chalcidoidea", tree$node.label)
if (length(chalc_node_idx) == 0) stop("Chalcidoidea not found")

chalc_node_id <- chalc_node_idx[1] + n_tips 
chalc_species <- extract.clade(tree, node = chalc_node_id)$tip.label


bombyc_node_idx <- grep("Bombycoidea", tree$node.label)
if (length(bombyc_node_idx) == 0) stop("Bombycoidea not found")


bombyc_node_id <- bombyc_node_idx[1] + n_tips
bombyc_species <- extract.clade(tree, node = bombyc_node_id)$tip.label



ed_hyme_plot <- ed_hyme %>%
  mutate(Group = ifelse(Species %in% chalc_species, "Chalcidoidea", "Other Hymenoptera"))

ed_lep_plot <- ed_lepi %>%
  mutate(Group = ifelse(Species %in% bombyc_species, "Bombycoidea", "Other Lepidoptera"))



ggplot(ed_hyme_plot, aes(x = logED, fill = Group)) +
  geom_density(alpha = 0.5, color = "white") +
  theme_minimal() +
  scale_fill_manual(values = c("Chalcidoidea" = "#00A087", "Other Hymenoptera" = "#3C5488")) +
  labs(title = "Hymenoptera", x = "log10(ED)", y = "Density")+theme_classic()

ggplot(ed_lep_plot, aes(x = logED, fill = Group)) +
  geom_density(alpha = 0.5, color = "white") +
  theme_minimal() +
  scale_fill_manual(values = c("Bombycoidea" = "#00A087", "Other Lepidoptera" = "#3C5488")) +
  labs(title = "Lepidoptera", x = "log10(ED)", y = "Density")+theme_classic()

 
archaea<- filter(species_in_clade_final4,species_in_clade_final4$Group =="Archaea")
mean_arch <- filter(mean_ED,mean_ED$Group == "Archaea")

median_meanED <- mean_ED %>%
  group_by(Group) %>%
  summarise(median_ed = median(mean_ed, na.rm = TRUE))
