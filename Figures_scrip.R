rm(list = ls())
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggpubr)
library(ggpmisc)
library(RColorBrewer)
library(ggnewscale)
library(tidyverse)

#arrange ed table
resolved_medianED <-  read.csv("/Users/alexgjl/Desktop/third_submission_to_nc/updated_fields/ed_median.csv",
                               stringsAsFactors=F, header=T)
getname <- select(leaves_table,id,name,ott)
get_ci <- read.csv("/Users/alexgjl/Desktop/third_submission_to_nc/updated_fields/95ci_ed.csv",
                   stringsAsFactors=F, header=T)

ed_table <- merge(resolved_medianED,get_ci,how = "left",on ="id")
ed_table<- merge(ed_table,getname,how = "left",on = "id")
ed_table<- select(ed_table,-X)
write.csv(x = ed_table, file = "/Users/alexgjl/Desktop/third_submission_to_nc/updated_fields/Supplementary_Data_S1.csv")


median_pd <-  read.csv("pd_median.csv",
                       stringsAsFactors=F, header=T)
get_ci_pd <- read.csv("/Users/alexgjl/Desktop/third_submission_to_nc/updated_fields/95ci_pd.csv",
                   stringsAsFactors=F, header=T)
pd_table <- merge(median_pd,get_ci_pd,how = "left",on = "id")
pd_table<- select(pd_table,-X)
write.csv(x = pd_table, file = "/Users/alexgjl/Desktop/third_submission_to_nc/updated_fields/Supplementary_Data_S2.csv")



colnames(leaves_table)
leaves_to_upload <- select(leaves_table,-X,-iucn,-raw_popularity,-popularity,-popularity_rank,-price)
colnames(leaves_to_upload)
write.csv(x = leaves_to_upload, file = "/Users/alexgjl/Desktop/third_submission_to_nc/updated_fields/Onezoomdata_Leaves.csv")

colnames(nodes_table)
nodes_to_upload <- select(nodes_table,-raw_popularity,-popularity,       
                           -iucnNE,              -iucnDD,              -iucnLC,              -iucnNT,             
                         -iucnVU,              -iucnEN,              -iucnCR,              -iucnEW,              
                         -iucnEX,             
                          -popleaf,             -popleaf_ott)
colnames(nodes_to_upload)
write.csv(x = nodes_to_upload, file = "/Users/alexgjl/Desktop/third_submission_to_nc/updated_fields/Onezoomdata_Nodes.csv")
rm(list = ls())







setwd("/Users/alexgjl/Desktop/third_submission_to_nc/updated_fields/")
leaves_table <-  read.csv("updated_ordered_leaves_3.0.csv",
                        stringsAsFactors=F, header=T)
nodes_table <- read.csv("updated_ordered_nodes_3.0.csv",
                        stringsAsFactors=F, header=T)

#nodes_with_pd： id	name	ott	richness
nodes_table1 <- nodes_table
nodes_table1$richness <- nodes_table1$leaf_rgt-nodes_table1$leaf_lft+1
nodes_with_pd<- select(nodes_table1, id,name,ott,richness)

library(readxl)
excel_file <- "/Users/alexgjl/Desktop/master/项目2/论文修改_回答reviewer问题/EDGE-Lists-2020_website.xlsx"
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
df_names <- select(leaves_table,id,name)
pre_ed_all_with_id <- merge(pre_ed_all,df_names,how = "left", on = name)
#write.csv(x = pre_ed_all_with_id, file = "/Users/alexgjl/Desktop/third_submission_to_nc/updated_fields/Rikki_estimated_ED_with_ID.csv")



###Figure 1: The distribution of ED scores across selected groups
resolved_medianED <-  read.csv("/Users/alexgjl/Desktop/third_submission_to_nc/updated_fields/ed_median.csv",
                               stringsAsFactors=F, header=T)

rikki_ed <-  read.csv("/Users/alexgjl/Desktop/third_submission_to_nc/updated_fields/Rikki_estimated_ED_with_ID.csv",
                               stringsAsFactors=F, header=T)
our_ed <- resolved_medianED
getname <- select(leaves_table,id,name)
our_ed <- merge(our_ed,getname, how = "left",on = id)


missing_name <- rikki_ed$name[!(rikki_ed$name %in% our_ed$name)]
#no missing name at all



###########################################replace our estimation with rikki's###################################################
###########################################replace our estimation with rikki's###################################################
ED_all <- our_ed
rikki_ed$median <- rikki_ed$ED
idx <- match(rikki_ed$name, ED_all$name)  
matched_rikki <- which(!is.na(idx))
ED_all$median[idx[matched_rikki]] <- rikki_ed$median[matched_rikki]
ED_all$logED <- log(ED_all$median,10)
ED_all$Group <- rep("Biota",times = )
quantile(ED_all$median, probs = c(0.025,0.5, 0.975), na.rm = TRUE)

ED_all <- select(ED_all, id,Group,logED)



#"Eubacteria","All"
ED_Euk <- filter(ED_all,ED_all$id>59642)
ED_Euk$Group = rep("Eukaryota",times = )
dfED_dis1 <- rbind(ED_all,ED_Euk)

#"Metazoa","Holomycota","Chloroplastida","Spermatophyta","Diaphoretickes","TSAR"))
ED_met <- filter(ED_all,ED_all$id>805307)
ED_met$Group = rep("Metazoa",times = )
ED_hol <- filter(ED_all,543113< ED_all$id & ED_all$id<804914)
ED_hol$Group = rep("Holomycota",times = )
ED_chl<- filter(ED_all,122044 < ED_all$id & ED_all$id<541348)
ED_chl$Group = rep("Chloroplastida",times = )
ED_spe <- filter(ED_all,172685< ED_all$id & ED_all$id<541348)
ED_spe$Group = rep("Spermatophyta",times =  )
ED_dia <- filter(ED_all,63334< ED_all$id & ED_all$id<541348)
ED_dia$Group = rep("Diaphoretickes",times = )
ED_tsa <- filter(ED_all,63777< ED_all$id & ED_all$id<112742)
ED_tsa$Group = rep("TSAR",times = )
dfED_dis2 <- rbind(ED_met,ED_hol,ED_chl,ED_spe,ED_dia,ED_tsa)

#"Vertebrata","Lepidoptera","Diptera","Coleoptera","Hymenoptera","Chelicerata","Mollusca"))
ED_vert <- filter(ED_all,  840048<ED_all$id & ED_all$id<910759)
ED_vert$Group = rep("Vertebrata",times = )
ED_lep<- filter(ED_all,2056498< ED_all$id)
ED_lep$Group = rep("Lepidoptera",times = )
ED_dip <- filter(ED_all,1882223< ED_all$id & ED_all$id<2043133)
ED_dip$Group = rep("Diptera",times = )
ED_col <- filter(ED_all,1595857< ED_all$id & ED_all$id<1879264)
ED_col$Group = rep("Coleoptera",times = )
ED_hym <- filter(ED_all,1452619< ED_all$id & ED_all$id<1588532)
ED_hym$Group = rep("Hymenoptera",times = )
ED_che <- filter(ED_all,1082354< ED_all$id & ED_all$id<1175804)
ED_che$Group = rep("Chelicerata",times = )
ED_mol <- filter(ED_all, 972344 < ED_all$id & ED_all$id<1061704)
ED_mol$Group = rep("Mollusca",times = )
dfED_dis3 <- rbind(ED_vert,ED_lep,ED_dip,ED_col,ED_hym,ED_che,ED_mol)

#"Aves","Mammalia","Squamata","Testudines","Crocodilia","Amphibia",
#"Actinopterygii","Chondrichthyes",
#"cyclostomata(agnatha)"))
ED_ave <- filter(ED_all, 889846< ED_all$id & ED_all$id<899849)
ED_ave$Group= rep("Aves",times = )
ED_mam <- filter(ED_all,884546< ED_all$id & ED_all$id<889593)
ED_mam$Group = rep("Mammalia",times = )
ED_squa<- filter(ED_all, 899849< ED_all$id & ED_all$id<910759)
ED_squa$Group = rep("Squamata",times = )
ED_test <- filter(ED_all,889592< ED_all$id & ED_all$id<889824)
ED_test$Group = rep("Testudines",times = )
ED_croc <- filter(ED_all,889823< ED_all$id & ED_all$id<889847)
ED_croc$Group = rep("Crocodylia",times = )
ED_amph<- filter(ED_all,875858< ED_all$id & ED_all$id<884547)
ED_amph$Group = rep("Amphibia",times = )
ED_oste <- filter(ED_all,841418< ED_all$id & ED_all$id<875851)
ED_oste$Group = rep("Actinopterygii",times = )
ED_chon <- filter(ED_all,840162< ED_all$id & ED_all$id<841419)
ED_chon$Group = rep("Chondrichthyes",times = )
ED_agna <- filter(ED_all,840048< ED_all$id & ED_all$id<840163)
ED_agna$Group = rep("Cyclostomata",times = )
dfED_dis4 <- rbind(ED_ave,ED_mam,ED_squa,ED_test,ED_croc,ED_amph,ED_oste,ED_chon,ED_agna)

##density_plot
dfED_dis_new1 <- rbind(dfED_dis1,dfED_dis2)
dfED_dis_new2 <- dfED_dis3
dfED_dis_new3 <- dfED_dis4
dfED_dis_new1$Group <- factor(dfED_dis_new1$Group,levels = c("Biota","Eukaryota","TSAR","Diaphoretickes",
                                                             "Spermatophyta",
                                                             "Chloroplastida","Holomycota","Metazoa"))


install.packages("nortest")  
library(nortest)
#check median value

ED_tsa1 <- ED_tsa
ED_tsa1$ED <- 10**ED_tsa$logED
median(ED_tsa1$ED)
ED_mol1 <- ED_mol
ED_mol1$ED <- 10**ED_mol1$logED
median(ED_mol1$ED)
ED_chon1 <- ED_chon
ED_chon1$ED <- 10**ED_chon1$logED
median(ED_chon1$ED)

ED_amph1 <- ED_amph
ED_amph1$ED <- 10**ED_amph1$logED
median(ED_amph1$ED)

ED_croc1 <- ED_croc
ED_croc1$ED <- 10**ED_croc1$logED
median(ED_croc1$ED)

ED_test1 <- ED_test
ED_test1$ED <- 10**ED_test1$logED
median(ED_test1$ED)

ad.test(ED_all$logED)
ad.test(ED_agna$logED)
ad.test(ED_amph$logED)
ad.test(ED_ave$logED)
ad.test(ED_che$logED)
ad.test(ED_chl$logED)
ad.test(ED_chon$logED)
ad.test(ED_col$logED)
ad.test(ED_dia$logED)
ad.test(ED_dip$logED)
ad.test(ED_Euk$logED)
ad.test(ED_hol$logED)
ad.test(ED_hym$logED)
ad.test(ED_lep$logED)
ad.test(ED_mam$logED)
ad.test(ED_met$logED)
ad.test(ED_mol$logED)
ad.test(ED_oste$logED)
ad.test(ED_spe$logED)
ad.test(ED_squa$logED)
ad.test(ED_test$logED)
ad.test(ED_tsa$logED)
ad.test(ED_vert$logED)
shapiro.test(ED_croc$logED)

dis_plot1 <- ggplot(dfED_dis_new1, aes(x = logED,fill = Group)) + geom_density(alpha = 0.6) +
  facet_grid(Group ~ .)+theme(panel.grid.major=element_blank(),
                              panel.grid.minor=element_blank())+theme_classic()+
  scale_fill_manual(values=c( Biota = "grey", Eukaryota =  "grey",TSAR =  "grey",
                              Diaphoretickes= "grey",Spermatophyta=  "grey",
                              Chloroplastida = "grey",Holomycota=  "grey",
                              Metazoa = "#4d97cd"))  + labs(x="Log10 (ED) Myr",y="")+
  geom_vline(xintercept = quantile(log(resolved_medianED$median,10), probs = 0.95), color = "black", linetype = "solid", size = 0.4) +  
  geom_vline(xintercept = quantile(log(resolved_medianED$median,10), probs = 0.99), color = "black", linetype = "dashed", size = 0.4)   

dfED_dis_new2$Group <- factor(dfED_dis_new2$Group,levels = c("Mollusca","Chelicerata","Hymenoptera",
                                                             "Coleoptera","Diptera","Lepidoptera","Vertebrata"))
#dis_plot1

dis_plot2 <- ggplot(mapping = aes(x)) + geom_density(data = dfED_dis_new2, aes(x = logED,fill = Group),alpha = 0.6) +
  facet_grid(Group ~ .)+theme(panel.grid.major=element_blank(),
                              panel.grid.minor=element_blank())+theme_classic()+
  scale_fill_manual(values=c(Mollusca = "#4d97cd",
                             Chelicerata = "#4d97cd",Hymenoptera= "#4d97cd",
                             Coleoptera = "#4d97cd",Diptera= "#4d97cd", 
                             Lepidoptera = "#4d97cd",Vertebrata = "#ea9c9d" )) + labs(x="Log10 (ED) Myr",y="")+
  geom_vline(xintercept = quantile(log(resolved_medianED$median,10), probs = 0.95), color = "black", linetype = "solid", size = 0.4) +  
  geom_vline(xintercept = quantile(log(resolved_medianED$median,10), probs = 0.99), color = "black", linetype = "dashed", size = 0.4) 

dfED_dis_new3$Group <- factor(dfED_dis_new3$Group,levels = c("Cyclostomata","Chondrichthyes","Actinopterygii","Amphibia",
                                                             "Crocodylia","Testudines","Squamata","Mammalia",
                                                             "Aves"))

dis_plot3 <- ggplot(mapping = aes(x)) + geom_density(data = dfED_dis_new3, aes(x = logED,fill = Group),alpha = 0.6) +
  facet_grid(Group ~ .)+theme(panel.grid.major=element_blank(),
                              panel.grid.minor=element_blank())+theme_classic()+
  scale_fill_manual(values=c( Cyclostomata="#ea9c9d" ,Chondrichthyes ="#ea9c9d" ,Actinopterygii = "#ea9c9d",
                              Amphibia = "#ea9c9d" ,Crocodylia = "#ea9c9d",Testudines = "#ea9c9d",
                              Squamata= "#ea9c9d",Mammalia = "#ea9c9d",
                              Aves= "#ea9c9d")) + labs(x="Log10 (ED) Myr",y="")+
  geom_vline(xintercept = quantile(log(resolved_medianED$median,10), probs = 0.95), color = "black", linetype = "solid", size = 0.4) +  
  geom_vline(xintercept = quantile(log(resolved_medianED$median,10), probs = 0.99), color = "black", linetype = "dashed", size = 0.4) 

library(ggpubr)

Fig1 <- ggarrange(dis_plot1,dis_plot2,dis_plot3,labels = c("","",""),ncol = 3, nrow = 1)
dis_plot2
Fig1 <- ggarrange(dis_plot1, dis_plot2, dis_plot3,
                  labels = c("", "", ""), 
                  ncol = 3, nrow = 1,
                  align = "v")  
Fig1 <- annotate_figure(Fig1,
                        left = text_grob("Probability Density", 
                                         rot = 90, vjust = 1, size = 12))

Fig1
write.csv(x = dfED_dis_new1, file = "/Users/alexgjl/Desktop/final_data/files_for_plotting/Figure_1_Eukaryota.csv")
write.csv(x = dfED_dis_new2, file = "/Users/alexgjl/Desktop/final_data/files_for_plotting/Figure_1_Metazoa.csv")
write.csv(x = dfED_dis_new3, file = "/Users/alexgjl/Desktop/final_data/files_for_plotting/Figure_1_Vertebrata.csv")



####################################################################################################################
##Figure 3: Estimate PD for selected clades
library(data.table)
setwd("/Users/alexgjl/Desktop/third_submission_to_nc/updated_fields")
median_pd <-  read.csv("pd_median.csv",
                       stringsAsFactors=F, header=T)
df_pd <-  read.csv("selected_24pd.csv",
                       stringsAsFactors=F, header=T)



pd_table24 <- df_pd

pd_table24 <- select(pd_table24,-"Unnamed..0" ,-"id",-"median")
df_pd<-pd_table24

#change pd to long-table
all_pd_data_l <- df_pd %>%
  pivot_longer(
    cols = starts_with("pd"),   
    names_to = "Group",         
    values_to = "PD"           
  )

all_pd_data_l <- select(all_pd_data_l ,name,PD)


df_pd$Group <- c("Biota","Eukaryota","Eukaryota","Eukaryota","Eukaryota","Eukaryota","Eukaryota",
                 "Metazoa", "Vertebrata","Vertebrata","Vertebrata",
                 "Vertebrata","Vertebrata","Vertebrata","Vertebrata",
                 "Vertebrata","Vertebrata","Vertebrata",
                 "Metazoa","Metazoa","Metazoa","Metazoa","Metazoa","Metazoa")

get_Group <- select(df_pd,name,Group)
pd_table24_l_Grouped <- merge(all_pd_data_l,get_Group,how = "left",on = name)

pd_table24_l_Grouped$name[which(pd_table24_l_Grouped$name == "biota")] <- "Biota"
pd_table24_l_Grouped$name[which(pd_table24_l_Grouped$name == "CYCLOSTOMATA")] <- "Cyclostomata"
pd_table24_l_Grouped$name[which(pd_table24_l_Grouped$name == "CHONDRICHTHYES")] <- "Chondrichthyes"

#pd_table24_biotal<- filter(pd_table24_l,pd_table24_l$name =="Biota")
#median(pd_table24_biotal$PD)

pd_table24_l_Grouped$name <- factor(pd_table24_l_Grouped$name,levels =  c("Aves","Mammalia","Squamata","Testudines","Crocodylia","Amphibia",
                                                                          "Actinopterygii","Chondrichthyes",
                                                                          "Cyclostomata","Vertebrata","Lepidoptera","Diptera","Coleoptera",
                                                                          "Hymenoptera","Chelicerata","Mollusca","Metazoa","Holomycota","Chloroplastida",
                                                                          "Spermatophyta","Diaphoretickes",
                                                                          "TSAR","Eukaryota","Biota"))


#see 95 confidence interval width

library(dplyr)
library(purrr)

grouped_dfs <- split(pd_table24_l_Grouped, pd_table24_l_Grouped$name)
ci_widths <- map_df(grouped_dfs, function(df) {
  mean_pd <- mean(df$PD, na.rm = TRUE)
  sd_pd <- sd(df$PD, na.rm = TRUE)
  n <- nrow(df)
  
  error_margin <- 1.96 * sd_pd / sqrt(n)  
  width_percent <- (2 * error_margin / mean_pd) * 100  
  
  data.frame(
    name = unique(df$name),
    ci_width_percent = width_percent
  )
})
ci_widths
pd_table24_l_Grouped$logPD <-  log(pd_table24_l_Grouped$PD,10)

#scatter_PD
pd_table24_l_nonvert <- filter(pd_table24_l_Grouped,pd_table24_l_Grouped$Group!="Vertebrata")
pd_table24_l_vert <- filter(pd_table24_l_Grouped,pd_table24_l_Grouped$Group=="Vertebrata")


box_vert <- ggplot(pd_table24_l_vert, aes(fill = name,x = name, y = log(PD,10))) + 
  geom_boxplot(alpha = 0.8, width = 0.7)+theme_classic()+
  scale_fill_manual(values=c(Vertebrata = "#ea9c9d",
                             Cyclostomata="#ea9c9d" ,Chondrichthyes ="#ea9c9d" ,Cyclostomata = "#ea9c9d",
                             Amphibia = "#ea9c9d" ,Crocodylia = "#ea9c9d",Testudines = "#ea9c9d",
                             Squamata= "#ea9c9d",Mammalia = "#ea9c9d",
                             Aves= "#ea9c9d"))+ theme(legend.title = element_text(size = 30))+
  labs(y="Log10 (PD) Myr",x="")+ theme(text = element_text(size = 17))+theme(axis.text.x = element_text(angle = 45, hjust = 1))
box_vert
box_nonvert <- ggplot(pd_table24_l_nonvert, aes(fill = name,x = name, y = log(PD,10))) + 
  geom_boxplot(alpha = 0.8, width = 0.7)+theme_classic()+
  scale_fill_manual(values=c(  Biota = "grey", Eukaryota = "grey",TSAR = "grey",
                               Diaphoretickes= "grey",Spermatophyta="grey",
                               Chloroplastida = "grey",Holomycota= "grey",
                               Metazoa = "#4d97cd", Mollusca ="#4d97cd",
                               Chelicerata = "#4d97cd",Hymenoptera= "#4d97cd",
                               Coleoptera = "#4d97cd",Diptera= "#4d97cd", 
                               Lepidoptera ="#4d97cd"))+ theme(legend.title = element_text(size = 30))+
  labs(y="Log10 (PD) Myr",x="")+ theme(text = element_text(size = 17))+theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

box_all <- ggplot(pd_table24_l_Grouped, aes(fill = name,y = name, x = log(PD,10))) + 
  geom_boxplot(alpha = 0.8, width = 0.7)+theme_classic()+
  scale_fill_manual(values=c(  Biota = "grey", Eukaryota = "grey",TSAR = "grey",
                               Diaphoretickes= "grey",Spermatophyta="grey",
                               Chloroplastida = "grey",Holomycota= "grey",
                               Metazoa = "#4d97cd", Mollusca ="#4d97cd",
                               Chelicerata = "#4d97cd",Hymenoptera= "#4d97cd",
                               Coleoptera = "#4d97cd",Diptera= "#4d97cd", 
                               Lepidoptera ="#4d97cd",Vertebrata = "#ea9c9d",
                               Cyclostomata="#ea9c9d" ,Chondrichthyes ="#ea9c9d" ,Actinopterygii = "#ea9c9d",
                               Amphibia = "#ea9c9d" ,Crocodylia = "#ea9c9d",Testudines = "#ea9c9d",
                               Squamata= "#ea9c9d",Mammalia = "#ea9c9d",
                               Aves= "#ea9c9d"))+ theme(legend.title = element_text(size = 30))+
  labs(x="Log10 (PD) Myr",y="")+ theme(text = element_text(size = 17))
points_df <- data.frame(
  name = c("Testudines", "Amphibia", "Crocodylia", 
           "Actinopterygii", "Mammalia", "Aves"),
  x = c(log10(6520),
        log10(142670),
        log10(613),
        log10(329935),
        log10(37068),
        log10(89747))
)


box_all +
  geom_text(
    data = points_df,
    aes(x = x, y = name, label = "X"),
    color = "black",
    size = 4,
    inherit.aes = FALSE
  )

#Testudines	log(6520,10)
#Amphibia	 log(142670,10)
#Crocodylia	 log(613,10)
#Actinopterygii	 log(329935,10)
#Mammalia	 log(37068,10)
#Mammalia	 log(89747,10)

Figure3<-box_all
Figure3


write.csv(x = pd_table24_l_Grouped, file = "/Users/alexgjl/Desktop/final_data/files_for_plotting/Figure_3_PDvalues.csv")



########################################################################################################################
########################################Supplementary Figure 5 #########################################################
########################################################################################################################
nodes_table$richness <- nodes_table$leaf_rgt-nodes_table$leaf_lft+1
get_rich <- select(nodes_table,id,richness)
median_pd1 <- merge(median_pd,get_rich,how = "left",on = "id")
median_pd1$logrich<-log(median_pd1$richness,10)
median_pd1$logpd<-log(median_pd1$median,10)
#supplementary figure 5
scatter_PD <- ggplot(data = median_pd1, aes(x = logrich, y = logpd)) + 
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


cor_result <- cor.test(median_pd1$logrich, median_pd1$logpd, method = "spearman")
round(cor_result$estimate, 3)
signif(cor_result$p.value, 3)

write.csv(x = median_pd, file = "/Users/alexgjl/Desktop/final_data/files_for_plotting/FigureS5_PDvalues.csv")




#Figure 4: EDGE estimation

resolved_medianED <-  read.csv("/Users/alexgjl/Desktop/third_submission_to_nc/updated_fields/ed_median.csv",
                               stringsAsFactors=F, header=T)
iucn <-  read.csv("/Users/alexgjl/Desktop/third_submission_to_nc/updated_fields/iucn_for_all_ranked_species.csv")
resolved_medianED$ott <- leaves_table$ott

df_iucn<- iucn
df_iucn$ott<-as.numeric(df_iucn$ott)
resolved_medianED$ott<-as.numeric(resolved_medianED$ott)

df_iucn <- select(df_iucn, ott, status_code)
ed_table_for_edge <- merge(df_iucn,resolved_medianED,how = "left",on = ott)

ed_table_for_edge$status_code[which(ed_table_for_edge$status_code == "CR")] <- 4
ed_table_for_edge$status_code[which(ed_table_for_edge$status_code == "EN")] <- 3
ed_table_for_edge$status_code[which(ed_table_for_edge$status_code == "VU")] <- 2
ed_table_for_edge$status_code[which(ed_table_for_edge$status_code == "NT")] <- 1
ed_table_for_edge$status_code[which(ed_table_for_edge$status_code == "LC")] <- 0
ed_table_for_edge <- filter(ed_table_for_edge,ed_table_for_edge$status_code != "EX")
ed_table_for_edge <- filter(ed_table_for_edge,ed_table_for_edge$status_code != "EW")
ed_table_for_edge <- filter(ed_table_for_edge,ed_table_for_edge$status_code != "DD")

#EDGEi = ln (1 + EDi) + GEi  ln (2)
ed_table_for_edge$EDGE <- log(1+as.numeric(ed_table_for_edge$median))+log(2)*as.numeric(ed_table_for_edge$status_code)
ed_table_for_edge1 <- na.omit(ed_table_for_edge)

df_name <- select(leaves_table,ott,name)


df_edge <- merge(ed_table_for_edge1,df_name,how = "left",on = ott)

df_edge <- filter(df_edge, df_edge$name != "Psephurus gladius")
df_edge<-df_edge[order(-df_edge$EDGE),]
rownames(df_edge) <- 1:nrow(df_edge )
ed_top100 <- df_edge[0:100, ]
ed_top100$status_code[which(ed_top100$status_code == "4")] <- "CR"
ed_top100$status_code[which(ed_top100$status_code == "3")] <- "EN"
ed_top100$ed <- ed_top100$median
ed_top100<-select(ed_top100,id,name,ed,EDGE,status_code)

ed_top100$ED <- round(ed_top100$ed,2)
ed_top100$EDGE <- round(ed_top100$EDGE,2)
write.csv(x=ed_top100,file = "/Users/alexgjl/Desktop/third_submission_to_nc/updated_fields/top_100_edge_species.csv")


ed_top100 <-  read.csv("/Users/alexgjl/Desktop/third_submission_to_nc/updated_fields/top_100_edge_species.csv")

library(data.table)
ed_100<- read.csv("/Users/alexgjl/Desktop/third_submission_to_nc/updated_fields/top100_ed_values.csv",stringsAsFactors=F, header=T)
ed_values2 <- ed_100[7:1006]
colnames(ed_values2) <- c(paste0('X', 0:999))

ed_100_1 <- ed_100[2:5]
ed_top100 <- cbind(ed_100_1,ed_values2)
ed_top100_l <- pivot_longer(ed_top100,  cols  = X0:X999, names_to = 'Species',values_to = 'ED')
ed_top100_l<- select(ed_top100_l,id,name,ED)

mean_func <- function(data, indices) {
  return(mean(data[indices]))  
}
set.seed(1)
library(boot)
df_summary_bootstrap <- ed_top100_l %>%
  group_by(name) %>%
  summarise(
    bootstrap_results = list(boot(data = ED, statistic = mean_func, R = 1000)),  
    .groups = "drop"
  ) %>%
  mutate(
    ci_lower = sapply(bootstrap_results, function(x) quantile(x$t, 0.025)),  
    ci_upper = sapply(bootstrap_results, function(x) quantile(x$t, 0.975))   
  ) %>%
  select(name, ci_lower, ci_upper)



ed_100_ranked <- select(ed_100,id,name,EDGE, status_code,)
ed_100_ranked<- merge(ed_100_ranked,df_summary_bootstrap,how = "left",on = "name")
resolved_medianED$ED<-resolved_medianED$median
geted <- select(resolved_medianED,id,ED)
ed_100_ranked1<- merge(ed_100_ranked,geted,how = "left",on = "id")
ed_100_ranked1<-ed_100_ranked1[order(-ed_100_ranked1$EDGE),]
ed_100_ranked1$rank <- c(1:100)

ed_100_ranked1$`95ci` <- sprintf("%.2f-%.2f", ed_100_ranked1$ci_lower, ed_100_ranked1$ci_upper)
write.csv(x=ed_100_ranked1,file = "/Users/alexgjl/Desktop/third_submission_to_nc/updated_fields/top_100_edge_species_with_CI.csv")


ed_top20<- ed_100[0:20, ]
ed_values1 <- ed_top20[7:1006]
colnames(ed_values1) <- c(paste0('X', 0:999))
ed_top20_1 <- ed_top20[1:4]
ed_top20 <- cbind(ed_top20_1,ed_values1)

#check group
ed_top20$Group <- c("Vertebrata","Plantae","Plantae","Vertebrata","Vertebrata",
                    "Vertebrata","Invertebrata","Invertebrata","Invertebrata","Vertebrata",
                    "Invertebrata","Invertebrata","Invertebrata","Invertebrata","Invertebrata",
                    "Plantae","Invertebrata","Vertebrata","Invertebrata","Invertebrata")

ed_top20_l <- pivot_longer(ed_top20,  cols  = X0:X999, names_to = 'Species',values_to = 'ED')
ed_top20_l <- select(ed_top20_l,name,ED,Group)

ed_top20_l$name <- factor(ed_top20_l$name,levels = c("Symmetromphalus hageni","Acharax alinae","Scaphirhynchus albus" ,"Aneuretus simoni","Geothallus tuberosus" ,
                                                     "Eostemmiulus caecus" ,"Nanocopia minuta" ,"Euastacus girurmulayn","Margaritifera auricularia","Antrisocopia prehensilis",
                                                     "Erymnochelys madagascariensis","Mictocaris halope" ,"Margaritifera marocana",
                                                     "Margaritifera hembeli","Acipenser oxyrinchus","Acipenser sturio",
                                                     "Neoceratodus forsteri","Mankyua chejuensis","Galaxaura barbata","Latimeria chalumnae"))


ed_top20_l$Group <- factor(ed_top20_l$Group,levels = c("Plantae","Invertebrata","Vertebrata"))


Fig4 <- ggplot(ed_top20_l, aes(y = name, x = ED,fill = Group)) + 
  geom_boxplot(alpha = 0.6)+theme_classic()+xlim(0,500)+ theme(text = element_text(size = 15))+labs(x="ED (Myr)",y="")+
  scale_fill_manual(values = c(Plantae= "grey",Vertebrata = "#ea9c9d",
                               Invertebrata = "#4d97cd"))
Fig4
rikki_ed <-  read.csv("/Users/alexgjl/Desktop/third_submission_to_nc/updated_fields/Rikki_estimated_ED_with_ID.csv",
                      stringsAsFactors=F, header=T)
rikki_ed

points_df <- merge(
  ed_top20_l, 
  rikki_ed[, c("name", "ED")], 
  by = "name", 
  all.x = TRUE, 
  suffixes = c("", "_rikki")
)

Fig4 <- ggplot(ed_top20_l, aes(y = name, x = ED, fill = Group)) + 
  geom_boxplot(alpha = 0.6) +
  theme_classic() +
  xlim(0,500) +
  theme(text = element_text(size = 15)) +
  labs(x="ED (Myr)", y="") +
  scale_fill_manual(values = c(
    Plantae = "grey",
    Vertebrata = "#ea9c9d",
    Invertebrata = "#4d97cd"
  )) +
  # 在这里加红点
  geom_text(
    data = points_df[!is.na(points_df$ED_rikki), ],
    aes(x = ED_rikki, y = name, label = "X"),
    color = "black",
    size = 4,
    inherit.aes = FALSE
  )
  
Fig4

write.csv(x = ed_top20, file = "/Users/alexgjl/Desktop/third_submission_to_nc/updated_fields/Top20_EDGE_Species.csv")






#Supplementary Figure 2
qq1<- ggplot(ED_all, aes(sample = logED)) +
  stat_qq() +
  stat_qq_line()+
  ggtitle(ED_all$Group)

qq2<- ggplot(ED_Euk, aes(sample = logED)) +
  stat_qq() +
  stat_qq_line()+
  ggtitle(ED_Euk$Group)

qq3<- ggplot(ED_tsa, aes(sample = logED)) +
  stat_qq() +
  stat_qq_line()+
  ggtitle(ED_tsa$Group)
qq4<- ggplot(ED_dia, aes(sample = logED)) +
  stat_qq() +
  stat_qq_line()+
  ggtitle(ED_dia$Group)
qq5<- ggplot(ED_spe, aes(sample = logED)) +
  stat_qq() +
  stat_qq_line()+
  ggtitle(ED_spe$Group)
qq6<- ggplot(ED_chl, aes(sample = logED)) +
  stat_qq() +
  stat_qq_line()+
  ggtitle(ED_chl$Group)
qq7<- ggplot(ED_hol, aes(sample = logED)) +
  stat_qq() +
  stat_qq_line()+
  ggtitle(ED_hol$Group)
qq8<- ggplot(ED_met, aes(sample = logED)) +
  stat_qq() +
  stat_qq_line()+
  ggtitle(ED_met$Group)
qq9<- ggplot(ED_mol, aes(sample = logED)) +
  stat_qq() +
  stat_qq_line()+
  ggtitle(ED_mol$Group)
qq10<- ggplot(ED_che, aes(sample = logED)) +
  stat_qq() +
  stat_qq_line()+
  ggtitle(ED_che$Group)
qq11<- ggplot(ED_hym, aes(sample = logED)) +
  stat_qq() +
  stat_qq_line()+
  ggtitle(ED_hym$Group)
qq12<- ggplot(ED_col, aes(sample = logED)) +
  stat_qq() +
  stat_qq_line()+
  ggtitle(ED_col$Group)
qq13<- ggplot(ED_dip, aes(sample = logED)) +
  stat_qq() +
  stat_qq_line()+
  ggtitle(ED_dip$Group)
qq14<- ggplot(ED_lep, aes(sample = logED)) +
  stat_qq() +
  stat_qq_line()+
  ggtitle(ED_lep$Group)
qq15<- ggplot(ED_vert, aes(sample = logED)) +
  stat_qq() +
  stat_qq_line()+
  ggtitle(ED_vert$Group)
qq16<- ggplot(ED_agna, aes(sample = logED)) +
  stat_qq() +
  stat_qq_line()+
  ggtitle(ED_agna$Group)
qq17<- ggplot(ED_chon, aes(sample = logED)) +
  stat_qq() +
  stat_qq_line()+
  ggtitle(ED_chon$Group)
qq18<- ggplot(ED_oste, aes(sample = logED)) +
  stat_qq() +
  stat_qq_line()+
  ggtitle(ED_oste$Group)
qq19<- ggplot(ED_amph, aes(sample = logED)) +
  stat_qq() +
  stat_qq_line()+
  ggtitle(ED_amph$Group)
qq20<- ggplot(ED_croc, aes(sample = logED)) +
  stat_qq() +
  stat_qq_line()+
  ggtitle(ED_croc$Group)
qq21<- ggplot(ED_test, aes(sample = logED)) +
  stat_qq() +
  stat_qq_line()+
  ggtitle(ED_test$Group)
qq22<- ggplot(ED_squa, aes(sample = logED)) +
  stat_qq() +
  stat_qq_line()+
  ggtitle(ED_squa$Group)
qq23<- ggplot(ED_mam, aes(sample = logED)) +
  stat_qq() +
  stat_qq_line()+
  ggtitle(ED_mam$Group)
qq24<- ggplot(ED_ave, aes(sample = logED)) +
  stat_qq() +
  stat_qq_line()+
  ggtitle(ED_ave$Group)
FigS2<- ggarrange(qq1,qq2,qq3,qq4,qq5,qq6,qq7,qq8,qq9,qq10,qq11,qq12,
                  qq13,qq14,qq15,qq16,qq17,qq18,qq19,qq20,qq21,qq22,qq23,qq24,
                  ncol = 4, nrow = 6)


FigS2
write.csv(x = ED_all, file = "/Users/alexgjl/Desktop/final_data/files_for_plotting/FigureS2_EDvalues.csv")



##Supplementary Figure 3
#s1a tsar
ED_tsa <- filter(ED_all,63777< ED_all$id & ED_all$id<112742)
ED_tsa$Group<- rep("TSAR",times = 48964)

ED_rhizaria <- filter(ED_all,63777< ED_all$id & ED_all$id<79849)
ED_rhizaria$Group<- rep("Rhizaria",times = 16071)

ED_Alveolata <- filter(ED_all,79848< ED_all$id & ED_all$id<93782)
ED_Alveolata$Group<- rep("Alveolata",times = 13933)

ED_stram <- filter(ED_all,93781< ED_all$id & ED_all$id<112742)
ED_stram$Group<- rep("Stramenopiles",times = 18960)

df_dis_tsar <- rbind(ED_tsa, ED_rhizaria,ED_Alveolata, ED_stram)
df_dis_tsar$Group <- factor(df_dis_tsar$Group,levels = c("Alveolata","Rhizaria","Stramenopiles","TSAR"))

mycolors<-brewer.pal(9, "YlOrRd")

FigS3_a <- ggplot(df_dis_tsar, aes(x = logED,fill = Group)) + geom_density(alpha = 0.5) +
  facet_grid(Group ~ .)+theme(panel.grid.major=element_blank(),
                              panel.grid.minor=element_blank())+theme_classic()+
  scale_fill_manual(values = c(Rhizaria = "#FFEDA0",Alveolata = "#FFFFCC",
                               Stramenopiles = "#FED976",TSAR = "#FEB24C"))+
  #scale_fill_brewer("mycolors")+
  labs(x="Log10 (ED) Myr",y="")

ED_dia <- filter(ED_all,63334< ED_all$id & ED_all$id<541348)
ED_dia$Group = rep("Diaphoretickes",times = 478013)
ED_hap<- filter(ED_dia,id<63778)
ED_hap$Group <- rep("Haptista",times = )

ED_arch<- filter(ED_dia,113027< id)
ED_arch$Group <- rep("Archaeplastida",times = )


ED_cryp <- filter(ED_dia,112741< id&id <113028)
ED_cryp$Group <- rep("Cryptista",times = )

df_dis_dia <- rbind(ED_cryp,ED_dia,ED_hap,ED_tsa,ED_arch)


df_dis_dia$Group <- factor(df_dis_dia$Group, levels = c("Haptista","TSAR","Cryptista","Archaeplastida",
                                                        "Diaphoretickes"))


FigS3_b <- ggplot(df_dis_dia, aes(x = logED,fill = Group)) + geom_density(alpha = 0.5) +
  facet_grid(Group ~ .)+theme(panel.grid.major=element_blank(),
                              panel.grid.minor=element_blank())+theme_classic()+
  scale_fill_manual(values = c(Haptista = "#FFFFCC",TSAR = "#FFEDA0",
                               Cryptista = "#FED976",Archaeplastida = "#FEB24C",Diaphoretickes = "#FD8D3C")) +labs(x="Log10 (ED) Myr",y="")

#s2c hymenoptera
ED_bees <- filter(ED_all,1493995< ED_all$id & ED_all$id<1516043)
ED_ants <- filter(ED_all,1471246< ED_all$id & ED_all$id<1485398)
##
ED_bees$ED <- 10**(ED_bees$logED)#unnamed_nodes
ED_bees$Group <- rep("Bee",times = 22047) 

ED_ants$ED <- 10**(ED_ants$logED)#Formicidae
ED_ants$Group <- rep("Ant",times = 14151) 
ED_hym$ED <- 10**(ED_hym$logED)
ED_hym1<-select(ED_hym,id,Group,ED,logED)
within_hymenoptera <- rbind(ED_hym1,ED_bees,ED_ants)
hyme_other1 <- filter(ED_all,1452619< ED_all$id & ED_all$id<1471246)
hyme_other2 <- filter(ED_all,1485397< ED_all$id & ED_all$id<1493995)
hyme_other3 <- filter(ED_all,1516042< ED_all$id & ED_all$id<1588532)

hyme_other <- rbind(hyme_other1,hyme_other2,hyme_other3)
hyme_other$ED <- 10**(hyme_other$logED)
hyme_other$Group <- rep("Sawflies and Wasps",times = 99712)
within_hymenoptera1 <- rbind(within_hymenoptera,hyme_other)

within_hymenoptera1$Group <- factor(within_hymenoptera1$Group,levels = c("Ant","Bee","Sawflies and Wasps","Hymenoptera"))

within_hymenoptera2 <- within_hymenoptera1

within_hymenoptera2$Group <- factor(within_hymenoptera1$Group,levels = c("Hymenoptera","Sawflies and Wasps","Bee","Ant"))

FigS3_c <- ggplot(within_hymenoptera1, aes(x = logED,fill = Group)) + geom_density(alpha = 0.7) +
  facet_grid(Group ~ .)+theme(panel.grid.major=element_blank(),
                              panel.grid.minor=element_blank())+theme_classic()+
  scale_fill_brewer(palette="Blues")+
  labs(x="Log10 (ED) Myr",y="")

FigS3 <- ggarrange(FigS3_a,FigS3_b,FigS3_c,labels = c("     TSAR","Diaphoretickes","Hymenoptera"),ncol = 3, nrow = 1)
FigS3

write.csv(x = df_dis_tsar, file = "/Users/alexgjl/Desktop/final_data/files_for_plotting/FigureS3_ED_TSAR.csv")
write.csv(x = df_dis_dia, file = "/Users/alexgjl/Desktop/final_data/files_for_plotting/FigureS3_ED_Dia.csv")
write.csv(x = within_hymenoptera1, file = "/Users/alexgjl/Desktop/final_data/files_for_plotting/FigureS3_ED_Hym.csv")












#Supplementary Figure 6
library(gridExtra)
setwd("/Users/alexgjl/Desktop/third_submission_to_nc/updated_fields/")
resolved_medianED <-  read.csv("ed_median.csv",
                               stringsAsFactors=F, header=T)

resolved_medianED$logED <- log(resolved_medianED$median,10)
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
df_names <- select(leaves_table,id,name)
pre_ed_all_with_id <- merge(pre_ed_all,df_names,how = "left", on = name)

#introduce percentage error here
#get name for resolved ED:

resolved_medianED <- merge(df_names,resolved_medianED,how = "left", on = id)
ED_all <- select(resolved_medianED,name,logED)

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


ED_gymn1$Group<- rep("Gymnosperm",times = )
ED_coral1$Group<- rep("Cnidaria",times = )
ED_chon1$Group<- rep("Chondrichthyes",times = )
ED_amp1$Group<- rep("Amphibian",times = )
ED_croc1$Group<- rep("Crocodylia",times = )
ED_test1$Group<- rep("Testudines",times = )
ED_squa1$Group<- rep("Squamata",times = )
ED_mam1$Group<- rep("Mammalia",times = )
ED_aves1$Group<- rep("Aves",times = )



df_s6 <- rbind(ED_gymn1,ED_coral1,ED_chon1,ED_amp1,
               ED_croc1,ED_test1,ED_squa1,ED_mam1,ED_aves1)
write.csv(x = df_s6, file = "/Users/alexgjl/Desktop/final_data/files_for_plotting/FigureS6_ED_comparison.csv")
write.csv(x = pre_ed_all_with_id, file = "/Users/alexgjl/Desktop/third_submission_to_nc/updated_fields/Rikki_estimated_ED_with_ID.csv")


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

EDGE_gymn$Group <- rep("Gymnosperm")
EDGE_coral$Group <- rep("Cnidaria")
EDGE_chon$Group <- rep("Chondrichthyes")
EDGE_amp$Group <- rep("Amphibian")
EDGE_croc$Group <- rep("Crocodylia")
EDGE_test$Group <- rep("Testudines")
EDGE_squa$Group <- rep("Squamata")
EDGE_mam$Group <- rep("Mammalia")
EDGE_aves$Group <- rep("Aves")

dfs7<- rbind(EDGE_gymn,EDGE_coral,
             EDGE_chon,EDGE_amp,EDGE_croc,EDGE_test,EDGE_squa,
             EDGE_mam,EDGE_aves)

write.csv(x = dfs7, file = "/Users/alexgjl/Desktop/final_data/files_for_plotting/FigureS7_EDGE_comparison.csv")





#Figure 2
#A stacked bar with different colors

#rebuild these tables
#0.99，0.95，0.5

dfED_dis_new1$ed <- 10**dfED_dis_new1$logED
dfED_dis_new1<- filter(dfED_dis_new1,dfED_dis_new1$Group != "Biota")
dfED_dis_new2$ed <- 10**dfED_dis_new2$logED
dfED_dis_new3$ed <- 10**dfED_dis_new3$logED


#get popularity from leaves table
pop <- select(leaves_table,id,popularity)
dfED_dis_new1<- merge(dfED_dis_new1,pop,how = "left",on = "id")
dfED_dis_new2<- merge(dfED_dis_new2,pop,how = "left",on = "id")
dfED_dis_new3<- merge(dfED_dis_new3,pop,how = "left",on = "id")

seeedtsa <- filter(ED_tsa,ED_tsa$logED > 1.696429)

ED_all$ED <- 10**ED_all$logED
compare_ED <- function(df) {
  Q50<- quantile(ED_all$ED, probs = 0.5)
  Q95<- quantile(ED_all$ED, probs = 0.95)
  Q99<- quantile(ED_all$ED, probs = 0.99)
  ls_quantile <- vector("list", length = nrow(df))
  for (i in 1:nrow(df)) {
    ed_value <- df$ed[i]
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

dfED_dis2 <- compare_ED(dfED_dis_new1)
dfED_dis3 <- compare_ED(dfED_dis_new2)
dfED_dis4 <- compare_ED(dfED_dis_new3)


analyze_quantile_enrichment <- function(df, ED_all) {
  library(dplyr)
  library(purrr)
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
        sampled <- sample(ED_all$ED, size = num_species, replace = FALSE)
        sum(sampled >= quantile(ED_all$ED, probs = 0.99))
      })),
      # prop_high 和 prop_low
      prop_high = mean(unlist(random_counts) >= num_top_99),
      prop_low = mean(unlist(random_counts) < num_top_99)
    ) %>%
    select(Group, num_top_99, num_species, prop_high, prop_low) %>%
    ungroup()
  
  return(result_df)
}




dfED_dis2_2 <- analyze_quantile_enrichment(dfED_dis2,ED_all)
dfED_dis3_2 <- analyze_quantile_enrichment(dfED_dis3,ED_all)
dfED_dis4_2 <- analyze_quantile_enrichment(dfED_dis4,ED_all)


dfED_dis2_final <- merge(dfED_dis2,dfED_dis2_2,how = "Group",on = "left")
dfED_dis3_final <- merge(dfED_dis3,dfED_dis3_2,how = "Group",on = "left")
dfED_dis4_final <- merge(dfED_dis4,dfED_dis4_2,how = "Group",on = "left")



label_df2 <- dfED_dis2_final %>%
  select(Group, prop_low, prop_high) %>%
  distinct() %>%
  mutate(
    label = case_when(
      prop_low > 0.95 ~ "*+" ,
      prop_high > 0.95  ~ "*-",
      TRUE ~ ""
    )
  )
label_df3 <- dfED_dis3_final %>%
  select(Group, prop_low, prop_high) %>%
  distinct() %>%
  mutate(
    label = case_when(
      prop_low > 0.95 ~ "*+" ,
      prop_high > 0.95  ~ "*-",
      TRUE ~ ""
    )
  )
label_df4 <- dfED_dis4_final %>%
  select(Group, prop_low, prop_high) %>%
  distinct() %>%
  mutate(
    label = case_when(
      prop_low > 0.95 ~ "*+" ,
      prop_high > 0.95  ~ "*-",
      TRUE ~ ""
    )
  )


p2 <- ggplot(dfED_dis2, aes(x = Group, y = log(ed,10)))  + 
  geom_point(aes(color = Quantile), 
             position = position_jitter(width = 0.2),  
             alpha = 0.6, size = 2) +  
  geom_violin(trim = FALSE, fill = NA, color = "black")+
  theme_classic() +  
  scale_color_manual(values = c(
    "Q(0.99)" = "grey27" , 
    "Q(0.95)" = "grey45", 
    "Q(0.5)"  = "grey60", 
    " "       = "grey70"
  )) +  
  labs(title = "", x = "Clade", y = "Log10 (ED) Myr") +
  theme(legend.title = element_blank()) +
  geom_text(
    data = label_df2,
    aes(x = Group, y = 0.5+max(log(dfED_dis2$ed, 10)), label = label),
    inherit.aes = FALSE,
    size = 7,
    fontface = "bold"
  )


p3 <- ggplot(dfED_dis3, aes(x = Group, y = log(ed,10)))  + 
  geom_point(aes(color = Quantile), 
             position = position_jitter(width = 0.2),  
             alpha = 0.6, size = 2) +  
  geom_violin(trim = FALSE, fill = NA, color = "black")+
  theme_classic() +  
  scale_color_manual(values = c("Q(0.99)" = "#08519C" , "Q(0.95)" =  "#2171B5", "Q(0.5)" = "#9ECAE1", " " = "#DEEBF7" )) +  
  labs(title = "", x = "Clade", y = "Log10 (ED) Myr") +
  theme(legend.title = element_blank()) +
  geom_text(
    data = label_df3,
    aes(x = Group, y = 0.5+max(log(dfED_dis3$ed, 10)), label = label),
    inherit.aes = FALSE,
    size = 7,
    fontface = "bold"
  )


dfED_dis4$Group <- factor(dfED_dis4$Group,levels = c("Cyclostomata","Chondrichthyes","Actinopterygii","Amphibia",
                                                              "Crocodylia","Testudines","Squamata","Mammalia",
                                                             "Aves"))

p4 <- ggplot(dfED_dis4, aes(x = Group, y = log(ed,10)))  + 
  geom_point(aes(color = Quantile), 
             position = position_jitter(width = 0.2),  
             alpha = 0.6, size = 2) +  
  geom_violin(trim = FALSE, fill = NA, color = "black")+
  theme_classic() +  
  scale_color_manual(values = c("Q(0.99)" = "#67000D" , "Q(0.95)" =  "#CB181D", "Q(0.5)" = "#FCBBA1", " " = "#FFF5F0" )) +  
  labs(title = "", x = "Clade", y = "Log10 (ED) Myr") +
  theme(legend.title = element_blank()) +
  geom_text(
    data = label_df4,
    aes(x = Group, y = 0.5+max(log(dfED_dis4$ed, 10)), label = label),
    inherit.aes = FALSE,
    size = 7,
    fontface = "bold"
  )

quantile(ED_all$ED, probs = 0.5)
quantile(ED_all$ED, probs = 0.95)
quantile(ED_all$ED, probs = 0.99)
quantile(ED_all$logED, probs = 0.95)
quantile(ED_all$logED, probs = 0.99)

library(ggpubr)
Fig2 <- ggarrange(p2,p3,p4,labels = c("","",""),ncol = 1, nrow = 3)
Fig2



write.csv(x = rbind(dfED_dis2_final,dfED_dis3_final,dfED_dis4_final), file = "/Users/alexgjl/Desktop/final_data/files_for_plotting/Figure2_ED_lables.csv")
write.csv(x = dfED_dis2, file = "/Users/alexgjl/Desktop/final_data/files_for_plotting/Figure2_ED_Eukaryota.csv")
write.csv(x = dfED_dis3, file = "/Users/alexgjl/Desktop/final_data/files_for_plotting/Figure2_ED_Metazoa.csv")
write.csv(x = dfED_dis4, file = "/Users/alexgjl/Desktop/final_data/files_for_plotting/Figure2_ED_Vertebrata.csv")
#file of ed scores









##Supplemenrary Figure 4
mean_tsa<- filter(ave_ed_table24_l,name =="TSAR")
median(mean_tsa$ED)
mean_mam<- filter(ave_ed_table24_l,name =="Mammalia")
median(mean_mam$ED)
mean_ave<- filter(ave_ed_table24_l,name =="Aves")
median(mean_ave$ED)

ave_ed_table24 <-  read.csv("/Users/alexgjl/Desktop/third_submission_to_nc/updated_fields/average_ed_table_24(real_parent).csv",
                            stringsAsFactors=F, header=T)

getname <- select(nodes_table,id,name)
ave_ed_table24 <- merge(ave_ed_table24,getname, how = "left",on = id)

ave_ed_table24_l <- pivot_longer(ave_ed_table24, cols = X0:X999, 
                                 names_to = 'Group',values_to = 'ED')

ave_ed_table24_l$name[which(ave_ed_table24_l$name == "CHONDRICHTHYES")] <- "Chondrichthyes"
ave_ed_table24_l$name[which(ave_ed_table24_l$name == "CYCLOSTOMATA")] <- "Cyclostomata"
ave_ed_table24_l$name[which(ave_ed_table24_l$name == "biota")] <- "Biota"
ave_ed_table24_l$name<- factor(ave_ed_table24_l$name,levels =  c("Aves","Mammalia","Squamata","Testudines","Crocodylia","Amphibia",
                                                                 "Actinopterygii","Chondrichthyes",
                                                                 "Cyclostomata","Vertebrata","Lepidoptera","Diptera","Coleoptera",
                                                                 "Hymenoptera","Chelicerata","Mollusca","Metazoa","Holomycota","Chloroplastida",
                                                                 "Spermatophyta","Diaphoretickes",
                                                                 "TSAR","Eukaryota","Biota"))
FigS4<- ggplot(ave_ed_table24_l , aes(fill = name,y = name, x = log(ED,10))) + 
  geom_boxplot(alpha = 0.6, width = 0.7)+theme_classic()+
  scale_fill_manual(values=c(Biota = "grey", Eukaryota = "grey",TSAR = "grey",
                             Diaphoretickes= "grey",Spermatophyta="grey",
                             Chloroplastida = "grey",Holomycota= "grey",
                             Metazoa = "#4d97cd", Mollusca ="#4d97cd",
                             Chelicerata = "#4d97cd",Hymenoptera= "#4d97cd",
                             Coleoptera = "#4d97cd",Diptera= "#4d97cd", 
                             Lepidoptera ="#4d97cd",Vertebrata = "#ea9c9d",
                             Cyclostomata="#ea9c9d" ,Chondrichthyes ="#ea9c9d" ,Cyclostomata = "#ea9c9d",
                             Amphibia = "#ea9c9d" ,Crocodylia = "#ea9c9d",Testudines = "#ea9c9d",
                             Squamata= "#ea9c9d",Mammalia = "#ea9c9d",
                             Aves= "#ea9c9d"))+ theme(legend.title = element_text(size = 30))+
  labs(x="Log10 (Average ED) Myr",y="")+ theme(text = element_text(size = 17))
FigS4

ave_ed_ave <- filter(ave_ed_table24_l,ave_ed_table24_l$name == "Aves")
median(ave_ed_ave$ED)
ave_ed_mam <- filter(ave_ed_table24_l,ave_ed_table24_l$name == "Mammalia")
median(ave_ed_mam$ED)
ave_ed_cro <- filter(ave_ed_table24_l,ave_ed_table24_l$name == "Crocodylia")
median(ave_ed_cro$ED)
ave_ed_squ <- filter(ave_ed_table24_l,ave_ed_table24_l$name == "Squamata")
median(ave_ed_squ$ED)
ave_ed_tes <- filter(ave_ed_table24_l,ave_ed_table24_l$name == "Testudines")
median(ave_ed_tes$ED)
ave_ed_amp <- filter(ave_ed_table24_l,ave_ed_table24_l$name == "Amphibia")
median(ave_ed_amp$ED)
ave_ed_fish <- filter(ave_ed_table24_l,ave_ed_table24_l$name == "Actinopterygii")
median(ave_ed_fish$ED)
ave_ed_shark <- filter(ave_ed_table24_l,ave_ed_table24_l$name == "Chondrichthyes")
median(ave_ed_shark$ED)
ave_ed_cyc <- filter(ave_ed_table24_l,ave_ed_table24_l$name == "Cyclostomata")
median(ave_ed_cyc$ED)

write.csv(x = ave_ed_table24_l, file = "/Users/alexgjl/Desktop/final_data/files_for_plotting/FigureS4_averageED.csv")


#——————————————————————————————————————————————————————above updated—————————————————————————————————————————————————————

#Supplementary Figure 8 
#need to be update since top20 edge species has been updated!!

df_phylo <-  read.csv("/Users/alexgjl/Desktop/third_submission_to_nc/updated_fields/phyloinfo_for_edge20_with_ed.csv",
                      stringsAsFactors=F, header=T)
getname <- select(leaves_table,id,name)
df_phylo <- merge(df_phylo,getname, how = "left",on = id)
df_phylo$id <- as.factor(df_phylo$id)

getmane<- select(leaves_table,id,name)

ed_top20$rank =  c('No.1','No.2','No.3',"No.4","No.5","No.6","No.7","No.8","No.9",'No.10',
                  'No.11','No.12','No.13','No.14','No.15','No.16','No.17','No.18','No.19','No.20')
ed_top20$rank2 = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20)

ed_top20<- merge(ed_top20,getname, how = "left", on = "id")
getrank <- select(ed_top20,name,rank,rank2)

df_phylo <- merge(df_phylo,getrank, how = "left",on = name)
df_phylo$rank  <- factor(df_phylo$rank ,levels = c('No.1','No.2','No.3',"No.4","No.5","No.6","No.7","No.8","No.9",'No.10',
                                                   'No.11','No.12','No.13','No.14','No.15','No.16','No.17','No.18','No.19','No.20'))

df_phylo$Group[which(df_phylo$Group == "A")] <- "Dated Node"
df_phylo$Group[which(df_phylo$Group == "B")] <- "Bifurcating Node without Date"
df_phylo$Group[which(df_phylo$Group == "C")] <- "Resolved Node"
df_phylo$Group[which(df_phylo$Group == "D")] <- "Leaf"
df_phylo$Group  <- factor(df_phylo$Group,levels = c("Dated Node","Bifurcating Node without Date","Resolved Node", "Leaf"))

df_phylo_top10 <- filter(df_phylo,0<df_phylo$rank2 & df_phylo$rank2<11)
df_phylo_top10$log_age<- log(df_phylo_top10$age+1,10)

Fig_S8 <- ggplot(data=df_phylo_top10,aes(x=(0-log_age),y=log(node_ED,10),color = rank,alpha = 0.9))+
  geom_point(aes(shape = Group, color=rank,size = Group))+theme_classic()+
  scale_shape_manual(values = c("Bifurcating Node without Date"=1,"Dated Node" = 19,"Resolved Node" = 13,"Leaf" = 1))+
  scale_size_manual(values= c("Resolved Node"=2, "Bifurcating Node without Date"=4,"Dated Node"=6,"Leaf" = 6))+
  geom_line(aes(color=rank))+
  scale_color_manual(values=c(No.1= "#c74546",No.2= "grey",No.3= "#c74546",No.4= "grey",
                              No.5= "#c74546",No.6= "#4d97cd",No.7="#c74546",No.8= "#c74546",
                              No.9="#4d97cd",No.10= "grey"))+xlab("Log10 (Node Date Estimate) Myr")+
  ylab("Log10 (ED) Myr") +facet_wrap(~ rank, scales = "free")+theme_classic()
Fig_S8
write.csv(x = df_phylo_top10, file = "/Users/alexgjl/Desktop/final_data/files_for_plotting/FigureS8_phyloinfo.csv")


#Figure S9

setwd("/Users/alexgjl/Desktop/master/项目2/文件")

df_nodeinfo <- read.csv("df_nodes_with_proportion_of_dated&realparent.csv",
                        stringsAsFactors=F, header=T)

df_nodeinfo$species_richness <- df_nodeinfo$leaf_rgt-df_nodeinfo$leaf_lft+1
df_nodeinfo$log_richness <- log(df_nodeinfo$species_richness,10)
dfnode_forsample <- filter(df_nodeinfo,df_nodeinfo$dated_nodes>1)
dfnode_forsample <- filter(dfnode_forsample,dfnode_forsample$species_richness>10)
dfnode_forsample1 <- merge(dfnode_forsample,getname,how = "left",on = "id")
dfnode_forsample1[is.na(dfnode_forsample1)] <- 0
df_filtered <- dfnode_forsample1 %>%
  filter(name != "" & !grepl("[\\d_]", name))

#divide into 6 groups: 1-2，2-3，3-4，4-5，5-6，6-7
df_filtered <- df_filtered %>%
  mutate(speciesrichness_group = case_when(
    log_richness >= 1 & log_richness < 2 ~ "1-2",
    log_richness >= 2 & log_richness < 3 ~ "2-3",
    log_richness >= 3 & log_richness < 4 ~ "3-4",
    log_richness >= 4 & log_richness < 5 ~ "4-5",
    log_richness >= 5 & log_richness < 6 ~ "5-6",
    log_richness >= 6 & log_richness <= 7 ~ "6-7"
  ))

set.seed(1)  
samples <- df_filtered %>%
  group_by(speciesrichness_group) %>%
  sample_n(4)  # sample 4 for each group


result_list <- list()
for (i in 1:nrow(samples)) {
  leaf_lft <- samples$leaf_lft[i]
  leaf_rgt <- samples$leaf_rgt[i]
  name <- samples$name[i]
  subset_ed <- ED_all %>%
    filter(id > leaf_lft - 1 & id < leaf_rgt + 1)
  result_list[[name]] <- subset_ed
}
sample_sees <- select(samples,leaf_lft,leaf_rgt,name)
write.csv(x=samples,file = "sampled_clades.csv")

setwd("/Users/alexgjl/Desktop/final_data")
install.packages("ape")



#visulization of the newick data
library("ape")
library(ggtree)
library(ape)


selected_clade <- read.csv("/Users/alexgjl/Desktop/final_data/sampled_clades.csv",
                           stringsAsFactors = FALSE, header = TRUE)
tree <- read.tree("/Users/alexgjl/Desktop/final_data/tree_output.nwk")


selected_clade_id <- as.character(selected_clade$id)
selected_clade_names <- as.character(selected_clade$name)
leaf_ids <- tree$tip.label
interior_node_ids <- setdiff(selected_clade_id, leaf_ids)

tree$node.label <- ifelse(tree$node.label %in% interior_node_ids, tree$node.label, NA)
tree$node.label <- selected_clade$name[match(tree$node.label, selected_clade$id)]
tree$tip.label <- selected_clade$name[match(tree$tip.label, selected_clade$id)]

tip_colors <- c(
  Mesalina = "#2c4ca0",
  Arthroleptis = "#2c4ca0",
  Leersia = "#2c4ca0",
  Lichmera = "#2c4ca0",
  
  Copelatinae = "#478ecc",
  Tettigoniinae = "#478ecc",
  Metastelmatinae = "#478ecc",
  Euryops = "#478ecc",
  
  Orthotylinae = "#75b5dc",
  Pleurostigmophora = "#75b5dc",
  Brassicaceae = "#75b5dc",
  Trechini = "#75b5dc",
  
  Pentatomomorpha = "#FEB24C",
  Cichorieae = "#FEB24C",
  Aves = "#FEB24C",
  Polyneoptera = "#FEB24C",
  
  Mesangiospermae = "#FD8D3C",
  Holomycota = "#FD8D3C",
  Trochozoa = "#FD8D3C",
  Holometabola = "#FD8D3C"
)

all_labels <- unique(c(tree$tip.label, tree$node.label))
all_color_map <- tip_colors
unspecified <- setdiff(all_labels, names(tip_colors))
all_color_map[unspecified] <- "#c44438"

p <- ggtree(tree, layout = "fan")
p_data <- p$data

tip_labels <- subset(p_data, isTip & label != "Aves")
node_labels <- subset(p_data, !isTip & !(label %in% c("Holozoa", "Filozoa", "Pentatomomorpha")))


holozoa_row <- subset(p_data, label == "Holozoa")
aves_row <- subset(p_data, label == "Aves")
filozoa_row <- subset(p_data, label == "Filozoa")
pentatom_row <- subset(p_data, label == "Pentatomomorpha")

special_labels <- c("Holozoa", "Aves", "Filozoa", "Pentatomomorpha")


tip_labels <- subset(p_data, isTip & !(label %in% special_labels))
node_labels <- subset(p_data, !isTip & !(label %in% special_labels))

p <- ggtree(tree, layout = "fan") +
  geom_text(data = tip_labels, 
            aes(x = x * 1.08, y = y, label = label, color = label), 
            size = 5, angle = 0) +
  geom_text(data = node_labels, 
            aes(x = x, y = y, label = label, color = label), 
            size = 5, angle = 0) +
  
  geom_text(data = holozoa_row, 
            aes(x = x, y = y - 0.8, label = label), 
            color = all_color_map["Holozoa"], size = 5, angle = 0) +
  
  geom_text(data = aves_row, 
            aes(x = x, y = y + 0.2, label = label), 
            color = all_color_map["Aves"], size = 5, angle = 0) +
  
  geom_text(data = filozoa_row, 
            aes(x = x - 1.2, y = y, label = label), 
            color = all_color_map["Filozoa"], size = 5, angle = 0) +
  
  geom_text(data = pentatom_row, 
            aes(x = x +12, y = y, label = label), 
            color = all_color_map["Pentatomomorpha"], size = 5, angle = 0) +
  

  scale_color_manual(values = all_color_map) +
  theme(legend.position = "none")
p


#ED distribution of random sampled ED clades

####An alternative idea would be to seek clades with best coverage of dates and with a target species richness. 
#Way to do that is to percolate the number of dated notes up the tree in Jialiang’s dataset and then output a
#dataset of named nodes with species richness within a given range and date coverage also above a given minimum.
#This selection of clades would then be used to generate a new figure 2 with a different selection of clades for 
#comparison.  Existing figure 2 could be moved to supplementary


setwd("/Users/alexgjl/Desktop/final_data/")
selected_clades <-  read.csv("sampled_clades.csv",
                             stringsAsFactors=F, header=T)

median_pd <-  read.csv("/Users/alexgjl/Desktop/third_submission_to_nc/updated_fields/pd_median.csv",
                       stringsAsFactors=F, header=T)

#use ED-all
#get ed values based on the given name of clades
get_ED <- function(name) {
  leaf_lft <- selected_clades[selected_clades$name == name, "leaf_lft"]
  leaf_rgt <- selected_clades[selected_clades$name == name, "leaf_rgt"]
  df1 <- filter(ED_all, leaf_lft - 1 < id & id < leaf_rgt + 1)
  df1$Group <- rep(name, times = nrow(df1))
  return(df1)
}

ls_names <- list(selected_clades$name)

#creat dataframe that cover species in selected clades
lapply(ls_names[[1]], function(name) {
  df <- get_ED(name)
  assign(paste0("ed_", name), df, envir = .GlobalEnv)
})




make_ed_distributions <- function(df, ED_all) {

  dis_figures <- list()
  
  #
  color_map <- c("1-2" = "#2c4ca0", "2-3" = "#478ecc", "3-4" = "#75b5dc", 
                 "4-5" = "#f6e09d", "5-6" = "#f5a65b", "6-7" = "#c44438",">7" = "#9f0000")
  
  # 
  for (i in 1:nrow(df)) {
    # 
    name <- df$name[i]
    richness <- df$log_richness[i]
    speciesrichness_group <- df$speciesrichness_group[i]
    
    # 
    leaf_lft <- df$leaf_lft[i]
    leaf_rgt <- df$leaf_rgt[i]
    
    #
    df1 <- filter(ED_all, leaf_lft - 1 < id & id < leaf_rgt + 1)
    df1$Group <- rep(name, times = nrow(df1))
    df1$richgroup <- rep(speciesrichness_group, times = nrow(df1))  
    if (1 < richness & richness < 2) {
      dis_figure <- ggplot(mapping = aes(x)) + 
        geom_density(data = df1, aes(x = logED, fill = richgroup), alpha = 0.9) +
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank()) + 
        theme_classic() + xlim(-2, 3) +
        labs(x = "Log10 (ED) Myr", y = "") + 
        ggtitle(df1$Group[1]) +
        scale_fill_manual(values = color_map)  # 
    } else if (2 < richness & richness < 3) {
      dis_figure <- ggplot(mapping = aes(x)) + 
        geom_density(data = df1, aes(x = logED, fill = richgroup), alpha = 0.9) +
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank()) + 
        theme_classic() + xlim(-2, 3) +
        labs(x = "Log10 (ED) Myr", y = "") + 
        ggtitle(df1$Group[1]) +
        scale_fill_manual(values = color_map)
    } else if (3 < richness & richness < 4) {
      dis_figure <- ggplot(mapping = aes(x)) + 
        geom_density(data = df1, aes(x = logED, fill = richgroup), alpha = 0.9) +
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank()) + 
        theme_classic() + xlim(-2, 3) +
        labs(x = "Log10 (ED) Myr", y = "") + 
        ggtitle(df1$Group[1]) +
        scale_fill_manual(values = color_map)
    } else if (4 < richness & richness < 5) {
      dis_figure <- ggplot(mapping = aes(x)) + 
        geom_density(data = df1, aes(x = logED, fill = richgroup), alpha = 0.9) +
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank()) + 
        theme_classic() + xlim(-2, 3) +
        labs(x = "Log10 (ED) Myr", y = "") + 
        ggtitle(df1$Group[1]) +
        scale_fill_manual(values = color_map)
    } else if (5 < richness & richness < 6) {
      dis_figure <- ggplot(mapping = aes(x)) + 
        geom_density(data = df1, aes(x = logED, fill = richgroup), alpha = 0.9) +
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank()) + 
        theme_classic() + xlim(-2, 3) +
        labs(x = "Log10 (ED) Myr", y = "") + 
        ggtitle(df1$Group[1]) +
        scale_fill_manual(values = color_map)
    } else if (richness > 6) {
      dis_figure <- ggplot(mapping = aes(x)) + 
        geom_density(data = df1, aes(x = logED, fill = richgroup), alpha = 0.9) +
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank()) + 
        theme_classic() + xlim(-2, 3) +
        labs(x = "Log10 (ED) Myr", y = "") + 
        ggtitle(df1$Group[1]) +
        scale_fill_manual(values = color_map)
    }
    
    # 
    dis_figures[[i]] <- dis_figure
  }
  
  ED_all$richgroup <- rep(">7",times = )
  ED_all$Group <- rep("Biota",times = )
  dis_figure <- ggplot(mapping = aes(x)) + 
    geom_density(data = ED_all, aes(x = logED, fill = richgroup), alpha = 0.9) +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) + 
    theme_classic() +
    labs(x = "Log10 (ED) Myr", y = "") + 
    ggtitle("Biota") +
    scale_fill_manual(values = color_map)
  
  # 
  dis_figures[[length(dis_figures) + 1]] <- dis_figure
  
  # 
  return(dis_figures)
}
ED_dis_figures <-make_ed_distributions(selected_clades,ED_all)
ED_dis_figures[[1]]



write.csv(x = selected_clades, file = "/Users/alexgjl/Desktop/final_data/files_for_plotting/FigureS9_selected_clades.csv")



library(gridExtra)
do.call(grid.arrange, c(ED_dis_figures, ncol = 5))



#———————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————
#threatened_pd_analysis

setwd("/Users/alexgjl/Desktop/third_submission_to_nc/updated_fields")


th_euk <-  read.csv("threatenedpd_euk.csv",
                    stringsAsFactors=F, header=T)
th_euk <- select(th_euk,-X)
list_euk <- as.list(th_euk)
list_euk<- as.numeric(list_euk)

th_meta <-  read.csv("threatenedpd_metazoa.csv",
                     stringsAsFactors=F, header=T)
th_meta <- select(th_meta,-X)
list_meta <- as.list(th_meta)
list_meta<- as.numeric(list_meta)

th_holow <-  read.csv("threatenedpd_holomycota_w.csv",
                      stringsAsFactors=F, header=T)
th_holow <- select(th_holow,-X)
list_holow <- as.list(th_holow)
list_holow<- as.numeric(list_holow)


th_tsaw <-  read.csv("threatenedpd_tsa_w.csv",
                     stringsAsFactors=F, header=T)
th_tsaw <- select(th_tsaw,-X)
list_tsaw<- as.list(th_tsaw)
list_tsaw<- as.numeric(list_tsaw)
median(list_tsaw)


th_chlw <-  read.csv("threatenedpd_chl_w.csv",
                     stringsAsFactors=F, header=T)
th_chlw <- select(th_chlw,-X)
list_chlw<- as.list(th_chlw)
list_chlw<- as.numeric(list_chlw)


th_spew <-  read.csv("threatenedpd_spe_w.csv",
                     stringsAsFactors=F, header=T)
th_spew <- select(th_spew,-X)
list_spew<- as.list(th_spew)
list_spew<- as.numeric(list_spew)


th_diaw <-  read.csv("threatenedpd_dia_w.csv",
                     stringsAsFactors=F, header=T)
th_diaw <- select(th_diaw,-X)
list_diaw<- as.list(th_diaw)
list_diaw<- as.numeric(list_diaw)
median(list_diaw)


th_chl <-  read.csv("threatenedpd_chl.csv",
                    stringsAsFactors=F, header=T)
th_chl <- select(th_chl,-X)
list_chl <- as.list(th_chl)
list_chl<- as.numeric(list_chl)

th_holo <-  read.csv("threatenedpd_holomycota.csv",
                     stringsAsFactors=F, header=T)
th_holo <- select(th_holo,-X)
list_holo <- as.list(th_holo)
list_holo<- as.numeric(list_holo)

th_spe <-  read.csv("threatenedpd_spe.csv",
                    stringsAsFactors=F, header=T)
th_spe <- select(th_spe,-X)
list_spe <- as.list(th_spe)
list_spe<- as.numeric(list_spe)

th_dia <-  read.csv("threatenedpd_dia.csv",
                    stringsAsFactors=F, header=T)
th_dia <- select(th_dia,-X)
list_dia <- as.list(th_dia)
list_dia<- as.numeric(list_dia)

th_tsa <-  read.csv("threatenedpd_tsa.csv",
                    stringsAsFactors=F, header=T)

th_mol <-  read.csv("threatenedpd_mol.csv",
                    stringsAsFactors=F, header=T)
th_mol <- select(th_mol,-X)
list_mol <- as.list(th_mol)
list_mol<- as.numeric(list_mol)

th_molw <-  read.csv("threatenedpd_mol_w.csv",
                     stringsAsFactors=F, header=T)
th_molw <- select(th_molw,-X)
list_molw <- as.list(th_molw)
list_molw<- as.numeric(list_molw)

th_che <-  read.csv("threatenedpd_che.csv",
                    stringsAsFactors=F, header=T)
th_che <- select(th_che,-X)
list_che <- as.list(th_che)
list_che<- as.numeric(list_che)

th_chew <-  read.csv("threatenedpd_che_w.csv",
                     stringsAsFactors=F, header=T)
th_chew <- select(th_chew,-X)
list_chew <- as.list(th_chew)
list_chew<- as.numeric(list_chew)


th_hym <-  read.csv("threatenedpd_hym.csv",
                    stringsAsFactors=F, header=T)
th_hym <- select(th_hym,-X)
list_hym <- as.list(th_hym)
list_hym<- as.numeric(list_hym)
median(list_hym)
th_hymw<-  read.csv("threatenedpd_hym_w.csv",
                    stringsAsFactors=F, header=T)
th_hymw <- select(th_hymw,-X)
list_hymw <- as.list(th_hymw)
list_hymw<- as.numeric(list_hymw)

th_dip <-  read.csv("threatenedpd_dip.csv",
                    stringsAsFactors=F, header=T)
th_dip <- select(th_dip,-X)
list_dip <- as.list(th_dip)
list_dip<- as.numeric(list_dip)
median(list_dip)

th_lep <-  read.csv("threatenedpd_lep.csv",
                    stringsAsFactors=F, header=T)
th_lep <- select(th_lep,-X)
list_lep <- as.list(th_lep)
list_lep<- as.numeric(list_lep)

th_col <-  read.csv("threatenedpd_col.csv",
                    stringsAsFactors=F, header=T)
th_col <- select(th_col,-X)
list_col <- as.list(th_col)
list_col<- as.numeric(list_col)

th_colw <-  read.csv("threatenedpd_col_w.csv",
                     stringsAsFactors=F, header=T)
th_colw <- select(th_colw,-X)
list_colw <- as.list(th_colw)
list_colw<- as.numeric(list_colw)

th_dipw <-  read.csv("threatenedpd_dip_w.csv",
                     stringsAsFactors=F, header=T)
th_dipw <- select(th_dipw,-X)
list_dipw <- as.list(th_dipw)
list_dipw<- as.numeric(list_dipw)

th_lepw <-  read.csv("threatenedpd_lep_w.csv",
                     stringsAsFactors=F, header=T)
th_lepw <- select(th_lepw,-X)
list_lepw <- as.list(th_lepw)
list_lepw<- as.numeric(list_lepw)



th_vert <-  read.csv("threatenedpd_vert.csv",
                     stringsAsFactors=F, header=T)
th_vert <- select(th_vert,-X)
list_vert <- as.list(th_vert)
list_vert<- as.numeric(list_vert)

th_vertw <-  read.csv("threatenedpd_vert_w.csv",
                      stringsAsFactors=F, header=T)
th_vertw <- select(th_vertw,-X)
list_vertw <- as.list(th_vertw)
list_vertw<- as.numeric(list_vertw)


th_cycl <-  read.csv("threatenedpd_cyclostomata.csv",
                     stringsAsFactors=F, header=T)
th_cycl <- select(th_cycl,-X)
list_cycl <- as.list(th_cycl)
list_cycl<- as.numeric(list_cycl)

th_cyclw <-  read.csv("threatenedpd_cyclostomata_w.csv",
                      stringsAsFactors=F, header=T)
th_cyclw <- select(th_cyclw,-X)
list_cyclw <- as.list(th_cyclw)
list_cyclw<- as.numeric(list_cyclw)



th_chon <-  read.csv("threatenedpd_shark_ray.csv",
                     stringsAsFactors=F, header=T)
th_chon <- select(th_chon,-X)
list_chon <- as.list(th_chon)
list_chon<- as.numeric(list_chon)
median(list_chon)
th_chonw <-  read.csv("threatenedpd_shark_ray_w.csv",
                      stringsAsFactors=F, header=T)
th_chonw <- select(th_chonw,-X)
list_chonw <- as.list(th_chonw)
list_chonw<- as.numeric(list_chonw)
median(list_chonw)

th_oste <-  read.csv("threatenedpd_bonyfish.csv",
                     stringsAsFactors=F, header=T)
th_oste <- select(th_oste,-X)
list_oste <- as.list(th_oste)
list_oste<- as.numeric(list_oste)
th_ostew <-  read.csv("threatenedpd_bonyfish_w.csv",
                      stringsAsFactors=F, header=T)
th_ostew <- select(th_ostew,-X)
list_ostew <- as.list(th_ostew)
list_ostew<- as.numeric(list_ostew)
median(list_ostew)

th_amph <-  read.csv("threatenedpd_amph.csv",
                     stringsAsFactors=F, header=T)
th_amph <- select(th_amph,-X)
list_amph <- as.list(th_amph)
list_amph<- as.numeric(list_amph)
th_amphw <-  read.csv("threatenedpd_amph_w.csv",
                      stringsAsFactors=F, header=T)
th_amphw <- select(th_amphw,-X)
list_amphw <- as.list(th_amphw)
list_amphw<- as.numeric(list_amphw)
median(list_amphw)

th_croc <-  read.csv("threatenedpd_croc.csv",
                     stringsAsFactors=F, header=T)
th_croc <- select(th_croc,-X)
list_croc <- as.list(th_croc)
list_croc<- as.numeric(list_croc)
th_crocw <-  read.csv("threatenedpd_croc_w.csv",
                      stringsAsFactors=F, header=T)
th_crocw <- select(th_crocw,-X)
list_crocw <- as.list(th_crocw)
list_crocw<- as.numeric(list_crocw)


th_test <-  read.csv("threatenedpd_test.csv",
                     stringsAsFactors=F, header=T)
th_test <- select(th_test,-X)
list_test <- as.list(th_test)
list_test<- as.numeric(list_test)
th_testw <-  read.csv("threatenedpd_test_w.csv",
                      stringsAsFactors=F, header=T)
th_testw <- select(th_testw,-X)
list_testw <- as.list(th_testw)
list_testw<- as.numeric(list_testw)
median(list_testw)
median(list_test)

th_squa <-  read.csv("threatenedpd_squa.csv",
                     stringsAsFactors=F, header=T)
th_squa <- select(th_squa,-X)
list_squa <- as.list(th_squa)
list_squa<- as.numeric(list_squa)
th_squaw <-  read.csv("threatenedpd_squa_w.csv",
                      stringsAsFactors=F, header=T)
th_squaw <- select(th_squaw,-X)
list_squaw <- as.list(th_squaw)
list_squaw<- as.numeric(list_squaw)

th_mam <-  read.csv("threatenedpd_mam.csv",
                    stringsAsFactors=F, header=T)
th_mam <- select(th_mam,-X)
list_mam <- as.list(th_mam)
list_mam<- as.numeric(list_mam)
th_mamw <-  read.csv("threatenedpd_mam_w.csv",
                     stringsAsFactors=F, header=T)
th_mamw <- select(th_mamw,-X)
list_mamw <- as.list(th_mamw)
list_mamw<- as.numeric(list_mamw)


th_ave <-  read.csv("threatenedpd_ave.csv",
                    stringsAsFactors=F, header=T)
th_ave <- select(th_ave,-X)
list_ave <- as.list(th_ave)
list_ave<- as.numeric(list_ave)
th_avew <-  read.csv("threatenedpd_ave_w.csv",
                     stringsAsFactors=F, header=T)
th_avew <- select(th_avew,-X)
list_avew <- as.list(th_avew)
list_avew<- as.numeric(list_avew)





setwd("/Users/alexgjl/Desktop/third_submission_to_nc/updated_fields")
th_met0 <-  read.csv("threatenedpd_metazoa_w0.csv",
                     stringsAsFactors=F, header=T)
th_met1 <-  read.csv("threatenedpd_metazoa_w1.csv",
                     stringsAsFactors=F, header=T)
th_met2 <-  read.csv("threatenedpd_metazoa_w2.csv",
                     stringsAsFactors=F, header=T)
th_met3 <-  read.csv("threatenedpd_metazoa_w3.csv",
                     stringsAsFactors=F, header=T)
th_met4 <-  read.csv("threatenedpd_metazoa_w4.csv",
                     stringsAsFactors=F, header=T)
th_met5 <-  read.csv("threatenedpd_metazoa_w5.csv",
                     stringsAsFactors=F, header=T)
th_met6 <-  read.csv("threatenedpd_metazoa_w6.csv",
                     stringsAsFactors=F, header=T)
th_met7 <-  read.csv("threatenedpd_metazoa_w7.csv",
                     stringsAsFactors=F, header=T)
th_met8 <-  read.csv("threatenedpd_metazoa_w8.csv",
                     stringsAsFactors=F, header=T)
th_met9 <-  read.csv("threatenedpd_metazoa_w9.csv",
                     stringsAsFactors=F, header=T)
th_met10 <-  read.csv("threatenedpd_metazoa_w10.csv",
                      stringsAsFactors=F, header=T)
th_met11 <-  read.csv("threatenedpd_metazoa_w11.csv",
                      stringsAsFactors=F, header=T)
th_met <- rbind(th_met0,th_met1,th_met2,th_met3,th_met4,th_met5,
                th_met6,th_met7,th_met8,th_met9,th_met10,th_met11)
write.csv(x = th_met, file = "/Users/alexgjl/Desktop/third_submission_to_nc/updated_fields/threatenedpd_metazoa_w.csv")
th_met <- select(th_met,-X)
th_met <- as.data.frame(t(colSums(th_met)))
list_met <- as.list(th_met)
list_met<- as.numeric(list_met)
median(list_met)

setwd("/Users/alexgjl/Desktop/third_submission_to_nc/updated_fields")

th_euk00 <-  read.csv("threatenedpd_euk_w00.csv",
                     stringsAsFactors=F, header=T)
th_euk01 <-  read.csv("threatenedpd_euk_w01.csv",
                     stringsAsFactors=F, header=T)
th_euk02 <-  read.csv("threatenedpd_euk_w02.csv",
                     stringsAsFactors=F, header=T)
th_euk03 <-  read.csv("threatenedpd_euk_w03.csv",
                     stringsAsFactors=F, header=T)
th_euk04 <-  read.csv("threatenedpd_euk_w04.csv",
                     stringsAsFactors=F, header=T)
th_euk05 <-  read.csv("threatenedpd_euk_w05.csv",
                      stringsAsFactors=F, header=T)


th_euk0<- rbind(th_euk00,th_euk01,
                th_euk02,th_euk03,th_euk04,
                th_euk05)


th_euk1 <-  read.csv("threatenedpd_euk_w1.csv",
                     stringsAsFactors=F, header=T)
th_euk2 <-  read.csv("threatenedpd_euk_w2.csv",
                     stringsAsFactors=F, header=T)
th_euk3 <-  read.csv("threatenedpd_euk_w3.csv",
                     stringsAsFactors=F, header=T)
th_euk4 <-  read.csv("threatenedpd_euk_w4.csv",
                     stringsAsFactors=F, header=T)
th_euk5 <-  read.csv("threatenedpd_euk_w5.csv",
                     stringsAsFactors=F, header=T)
th_euk6 <-  read.csv("threatenedpd_euk_w6.csv",
                     stringsAsFactors=F, header=T)
th_euk7 <-  read.csv("threatenedpd_euk_w7.csv",
                     stringsAsFactors=F, header=T)
th_euk8 <-  read.csv("threatenedpd_euk_w8.csv",
                     stringsAsFactors=F, header=T)
th_euk9 <-  read.csv("threatenedpd_euk_w9.csv",
                     stringsAsFactors=F, header=T)
th_euk10 <-  read.csv("threatenedpd_euk_w10.csv",
                     stringsAsFactors=F, header=T)
th_euk11 <-  read.csv("threatenedpd_euk_w11.csv",
                     stringsAsFactors=F, header=T)


th_euk <- rbind(th_euk0,th_euk1,th_euk2,
                th_euk3,th_euk4,th_euk5,
                th_euk6,th_euk7,th_euk8,
                th_euk9,th_euk10,th_euk11)
th_euk <- select(th_euk,-X)
th_euk <- as.data.frame(t(colSums(th_euk)))
list_euk <- as.list(th_euk)
list_euk<- as.numeric(list_euk)
median(list_euk)














##table1 data
##get iucn ranked species
iucn <-  read.csv("/Users/alexgjl/Desktop/third_submission_to_nc/updated_fields/latest_iucn_data_all.csv")

getid <- select(leaves_table,id,iucn)
iucn$iucn <- iucn$speciesID
iucn_withid <- merge(iucn,getid,how = "left",on = "iucn")
iucn_threatened <- filter(iucn_withid, !category %in% c("LC", "NT", "DD"))

ratio_id_in_iucn <- function(df, iucn_withid) {
  nums <- sum(df$id %in% iucn_withid$id)
  return(nums / nrow(df))
}

ratio_id_threatened <- function(df, iucn_threatened) {
  nums <- sum(df$id %in% iucn_threatened$id)
  return(nums / nrow(df))
}



ratio_id_threatened(ED_ave, iucn_threatened)



##get proportion of dated nodes
dated_nodes <-  read.csv("/Users/alexgjl/Desktop/third_submission_to_nc/updated_fields/df_nodes_with_proportion_of_dated&realparent.csv")
nodes_table <- select(nodes_table ,id,name
                      )
dated_nodes <- merge(dated_nodes,nodes_table,how = "left",on ="id")

target_names <- c(
  "Aves","Mammalia","Squamata","Testudines","Crocodylia","Amphibia",
  "Actinopterygii","CHONDRICHTHYES","CYCLOSTOMATA","Vertebrata",
  "Lepidoptera","Diptera","Coleoptera","Hymenoptera","Chelicerata",
  "Mollusca","Metazoa","Holomycota","Chloroplastida","Spermatophyta",
  "Diaphoretickes","TSAR","Eukaryota"
)

filtered <- dated_nodes[dated_nodes$name %in% target_names, ]
filtered$proportion <- filtered$dated_nodes/filtered$all_descendants

nodedates <-  read.csv("/Users/alexgjl/Desktop/third_submission_to_nc/updated_fields/latest_node_dates(real_parent)_3.0.csv")
nodedates<- merge(nodedates,nodes_table,how = "left",on ="id")


pdhym <- filter(pd_table24_l_Grouped,name == "Hymenoptera")
pddip <- filter(pd_table24_l_Grouped,name == "Diptera")
pdcyc <- filter(pd_table24_l_Grouped,name == "Cyclostomata")
