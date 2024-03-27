library(ggplot2)
library(dplyr)
library(tidyr)
library(ggpubr)
library(ggpmisc)
library(RColorBrewer)
library(ggnewscale)
library(tidyverse)

leaves_table <-  read.csv("updated_ordered_leaves_2.0.csv",
                          stringsAsFactors=F, header=T)

nodes_table <- read.csv("updated_ordered_nodes_2.0.csv",
                        stringsAsFactors=F, header=T)

###Figure 2: The distribution of ED scores across selected groups
resolved_medianED <-  read.csv("median_ed_final.csv",
                         stringsAsFactors=F, header=T)
resolved_medianED$logED <- log(resolved_medianED$ed,10)
ED_all <- resolved_medianED


ED_all <- select(ED_all, id,Group,logED,data)
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
dfED_dis1$Group[dfED_dis1$Group == "All"] <- "Biota"
dfED_dis_new1 <- rbind(dfED_dis1,dfED_dis2)
dfED_dis_new2 <- dfED_dis3
dfED_dis_new3 <- dfED_dis4
dfED_dis_new1$Group <- factor(dfED_dis_new1$Group,levels = c("Biota","Eukaryota","TSAR","Diaphoretickes",
                                                             "Spermatophyta",
                                                             "Chloroplastida","Holomycota","Metazoa"))

dis_plot1 <- ggplot(dfED_dis_new1, aes(x = logED,fill = Group)) + geom_density(alpha = 0.6) +
  facet_grid(Group ~ .)+theme(panel.grid.major=element_blank(),
                              panel.grid.minor=element_blank())+theme_classic()+
  scale_fill_manual(values=c( Biota = "#459943", Eukaryota ="#a3d393",TSAR = "#a3d393",
                              Diaphoretickes= "#a3d393",Spermatophyta= "#a3d393",
                              Chloroplastida = "#a3d393",Holomycota="#a3d393",
                              Metazoa = "#4d97cd")) + labs(x="Log10(ED) Myr",y="")

dfED_dis_new2$Group <- factor(dfED_dis_new2$Group,levels = c("Mollusca","Chelicerata","Hymenoptera",
                                                             "Coleoptera","Diptera","Lepidoptera","Vertebrata"))
dfED_dis_new2_1<- filter(dfED_dis_new2,dfED_dis_new2$data == "OneZoom")
dfED_dis_new2_2<- filter(dfED_dis_new2,dfED_dis_new2$data == "OTL")

dis_plot2 <- ggplot(mapping = aes(x)) + geom_density(data = dfED_dis_new2_1, aes(x = logED,fill = Group),alpha = 0.6) +
  facet_grid(Group ~ .)+theme(panel.grid.major=element_blank(),
                              panel.grid.minor=element_blank())+theme_classic()+
  scale_fill_manual(values=c(Mollusca = "#4d97cd",
                             Chelicerata = "#4d97cd",Hymenoptera= "#4d97cd",
                             Coleoptera = "#4d97cd",Diptera= "#4d97cd", 
                             Lepidoptera = "#4d97cd",Vertebrata = "#ea9c9d" )) + labs(x="Log10(ED) Myr",y="")

dfED_dis_new3$Group <- factor(dfED_dis_new3$Group,levels = c("Cyclostomata","Chondrichthyes","Actinopterygii","Amphibia",
                                                             "Crocodylia","Testudines","Squamata","Mammalia",
                                                             "Aves"))

dis_plot3 <- ggplot(mapping = aes(x)) + geom_density(data = dfED_dis_new3_1, aes(x = logED,fill = Group),alpha = 0.6) +
  facet_grid(Group ~ .)+theme(panel.grid.major=element_blank(),
                              panel.grid.minor=element_blank())+theme_classic()+
  scale_fill_manual(values=c( Cyclostomata="#ea9c9d" ,Chondrichthyes ="#ea9c9d" ,Actinopterygii = "#ea9c9d",
                              Amphibia = "#ea9c9d" ,Crocodylia = "#ea9c9d",Testudines = "#ea9c9d",
                              Squamata= "#ea9c9d",Mammalia = "#ea9c9d",
                              Aves= "#ea9c9d")) + labs(x="Log10(ED) Myr",y="")

library(ggpubr)
Fig2 <- ggarrange(dis_plot1,dis_plot2,dis_plot3,labels = c("","",""),ncol = 3, nrow = 1)
Fig2



##Figure 3: Estimate PD for selected clades
pd_table24 <-  read.csv("pd_24(real_parent).csv",
                       stringsAsFactors=F, header=T)

pd_table24_l <- pivot_longer(pd_table24, cols = X0:X99, 
                           names_to = 'Group',values_to = 'PD')

pd_table24_l$name[which(pd_table24_l$name == "biota")] <- "Biota"
pd_table24_l$name[which(pd_table24_l$name == "CYCLOSTOMATA")] <- "Cyclostomata"
pd_table24_l$name[which(pd_table24_l$name == "CHONDRICHTHYES")] <- "Chondrichthyes"


pd_table24_biotal<- filter(pd_table24_l,pd_table24_l$name =="Biota")
median(pd_table24_biotal$PD)



pd_table24_l$name <- factor(pd_table24_l$name,levels =  c("Aves","Mammalia","Squamata","Testudines","Crocodylia","Amphibia",
                                  "Actinopterygii","Chondrichthyes",
                                  "Cyclostomata","Vertebrata","Lepidoptera","Diptera","Coleoptera",
                                  "Hymenoptera","Chelicerata","Mollusca","Metazoa","Holomycota","Chloroplastida",
                                  "Spermatophyta","Diaphoretickes",
                                  "TSAR","Eukaryota","Biota"))
pd_table24_l$Data <-  rep("OTL", times = )
pd_table24_l$Data <-  rep("OneZoom", times = )
pd_table24_l$logPD <-  log(pd_table24_l$PD,10)
Fig3 <- ggplot(pd_table24_l, aes(fill = name,y = name, x = log(PD,10))) + 
  geom_boxplot(alpha = 0.8, width = 0.7)+theme_classic()+
  scale_fill_manual(values=c(Biota = "#fdc58f", Eukaryota = "#f47720",TSAR = "#f47720",
                             Diaphoretickes= "#f47720",Spermatophyta="#f47720",
                             Chloroplastida = "#f47720",Holomycota= "#f47720",
                             Metazoa = "#4d97cd", Mollusca ="#4d97cd",
                             Chelicerata = "#4d97cd",Hymenoptera= "#4d97cd",
                             Coleoptera = "#4d97cd",Diptera= "#4d97cd", 
                             Lepidoptera ="#4d97cd",Vertebrata = "#ea9c9d",
                             Cyclostomata="#ea9c9d" ,Chondrichthyes ="#ea9c9d" ,Cyclostomata = "#ea9c9d",
                             Amphibia = "#ea9c9d" ,Crocodylia = "#ea9c9d",Testudines = "#ea9c9d",
                             Squamata= "#ea9c9d",Mammalia = "#ea9c9d",
                             Aves= "#ea9c9d"))+ theme(legend.title = element_text(size = 30))+
  labs(x="Log10 (PD) Myr",y="")+ theme(text = element_text(size = 17))

#Figure 4: Relationship between PD and species richness
sampled_unnamed_PD <- read.csv("sampled_unnamed_pd(real_parent_only).csv",
                               stringsAsFactors=F, header=T)

sampled_named_PD <-  read.csv("sampled_named_pd(real_parent).csv",
                                stringsAsFactors=F, header=T)
sampled_named_PD$Group <- rep("Named_Clades",times = )
sampled_unnamed_PD$Group <- rep("Unnamed_Clades",times = )
sampled_named_PD<- filter(sampled_named_PD,sampled_named_PD$median_PD>0)
sampled_unnamed_PD<- filter(sampled_unnamed_PD,sampled_unnamed_PD$median_PD>0)
scatter_PD <- ggplot(data=sampled_named_PD, aes(x=log(richness,10), y=log(median_PD,10))) + geom_point(alpha = .5) +
  theme_bw() + theme(panel.grid=element_blank())+theme_classic()+
  xlab("Log10(Richness)")+ylab("Log10(PD) Myr")
scatter_unnamed_PD <- ggplot(data=sampled_unnamed_PD, aes(x=log(richness,10), y=log(median_PD,10))) + geom_point(alpha = .5) +
  theme_bw() + theme(panel.grid=element_blank())+theme_classic()+
  xlab("Log10(Richness)")+ylab("Log10(PD) Myr")
cor.test (log(sampled_named_PD$richness,10),log(sampled_named_PD$median_PD,10),method = "spearman")
cor.test (log(sampled_unnamed_PD$richness,10),log(sampled_unnamed_PD$median_PD,10),method = "spearman")
Fig4 <- ggarrange(scatter_PD,scatter_unnamed_PD,labels = c("Named Clades","Unnamed Clades"),ncol = 2, nrow = 1)

#Figure 5
pd_table24$Group <- c("Biota","Eukaryota","Eukaryota","Eukaryota","Eukaryota","Eukaryota","Eukaryota",
           "Metazoa", "Vertebrata","Vertebrata","Vertebrata",
           "Vertebrata","Vertebrata","Vertebrata","Vertebrata",
           "Vertebrata","Vertebrata","Vertebrata",
           "Metazoa","Metazoa","Metazoa","Metazoa","Metazoa","Metazoa")
get_Group <- select(pd_table24,name,Group)
get_Group$name[which(get_Group$name == "biota")] <- "Biota"
get_Group$name[which(get_Group$name == "CYCLOSTOMATA")] <- "Cyclostomata"
get_Group$name[which(get_Group$name == "CHONDRICHTHYES")] <- "Chondrichthyes"
pd_table24_l2<- select(pd_table24_l, name,richness,PD)
pd_table24_l_Grouped <- merge(pd_table24_l2,get_Group,how = "left",on = name)

pd_table24_l_Grouped$log_Richness <-log(pd_table24_l_Grouped$richness,10)
pd_table24_l_Grouped$log_Richness <- round(pd_table24_l_Grouped$log_Richness,2)
Fig5 <- ggplot(pd_table24_l_Grouped,aes(x=as.factor(log_Richness),y=log(PD,10),fill=Group,alpha = 0.6))+
  geom_boxplot(size=0.5,fill="white",outlier.fill="white",outlier.color="white")+ 
  geom_jitter(aes(fill=Group),width =0.2,shape = 21,size=1.5)+ 
  scale_fill_manual(values=c(Biota = "#fdc58f",Eukaryota = "#f47720", Metazoa = "#4d97cd", Vertebrata ="#ea9c9d" ))+ 
  scale_color_manual(values=c("black","black"))+ ggtitle("")+
  theme_bw()+ 
  theme(legend.position=c(0.8,0.3),
        axis.text.x=element_text(colour="black" ,size=14),                  
        axis.text.y=element_text( size=14,face="plain"), #
        axis.title.y=element_text( size = 14,face="plain"), #
        axis.title.x=element_text( size = 14,face="plain"), #
        plot.title = element_text( size=15,face="bold",hjust = 0.5), 
        panel.grid.major = element_blank(), #
        panel.grid.minor = element_blank())+
  ylab("Log10 (PD) Myr")+xlab("Log10 (Richness)") #

#Figure 6: EDGE estimation

df_iucn <- read.csv("iucn.csv",
                    stringsAsFactors=F, header=T)

resolved_medianED$ott <- leaves_table$ott

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
ed_table_for_edge$EDGE <- log(1+as.numeric(ed_table_for_edge$ed))+log(2)*as.numeric(ed_table_for_edge$status_code)
df_name <- select(leaves_table,ott,name)
df_edge <- merge(df_edge,df_name,how = "left",on = ott)
df_edge <- filter(df_edge, df_edge$name != "Psephurus gladius")
df_edge<-df_edge[order(-df_edge$EDGE),]
rownames(df_edge) <- 1:nrow(df_edge )
ed_top100 <- df_edge[0:100, ]
ed_top100$status_code[which(ed_top100$status_code == "4")] <- "CR"
ed_top100$status_code[which(ed_top100$status_code == "3")] <- "EN"
ed_top100<-select(ed_top100,name,ed,EDGE,status_code)
ed_top100$ED <- round(ed_top100$ED,2)
ed_top100$EDGE <- round(ed_top100$EDGE,2)
ed_top20 <- ed_top100[0:20, ]


ed_values1 <- ed_top20[10:109]
ls_names <- colnames(pd_table24[5:104])
colnames(ed_values1) <- ls_names
ed_top20_1 <- ed_top20[1:6]
ed_top20 <- cbind(ed_top20_1,ed_values1)
ed_top20<- subset(ed_top20, select = -ed)
ed_top20_l <- pivot_longer(ed_top20,  cols  = X0:X99, names_to = 'Species',values_to = 'ED')

ed_top20$Group <- c("Vertebrata","Vertebrata","Plantae","Plantae","Invertebrata","Vertebrata",
"Invertebrata","Vertebrata","Invertebrata","Vertebrata","Vertebrata","Invertebrata",
"Invertebrata","Plantae","Invertebrata","Invertebrata","Invertebrata","Invertebrata","Invertebrata","Vertebrata")
ed_top20_l <- select(ed_top20_l,name,ED,Group)
ed_top20_l$name <- factor(ed_top20_l$name,levels = c("Crotaphatrema lamottei" ,"Symmetromphalus hageni" ,
                                   "Microsphaerotherium anjozorobe", "Aneuretus simoni" ,
                            "Speleoperipatus spelaeus","Nanocopia minuta","Geothallus tuberosus","Antrisocopia prehensilis",
                            "Eostemmiulus caecus","Leiopelma archeyi","Huso huso","Euastacus girurmulayn",
                            "Erymnochelys madagascariensis","Mictocaris halope","Acipenser sturio","Procaris chacei",
                            "Mankyua chejuensis","Galaxaura barbata","Neoceratodus forsteri","Latimeria chalumnae" ))
ed_top20_l$Group <- factor(ed_top20_l$Group,levels = c("Plantae","Invertebrata","Vertebrata"))


#box_ploted_top20$name <- factor(ed_top20$name,levels = c(ed_top20$name))
Fig6 <- ggplot(ed_top20_l, aes(y = name, x = ED,fill = Group)) + 
  geom_boxplot(alpha = 0.6)+theme_classic()+xlim(0,500)+ theme(text = element_text(size = 15))+labs(x="ED (Myr)",y="")+
  scale_fill_manual(values = c(Plantae= "#f47720",Vertebrata = "#ea9c9d",
                               Invertebrata = "#4d97cd"))


#Supplementary Figure 1
qq1<- ggplot(ED_all, aes(sample = logED)) +
  stat_qq() +
  stat_qq_line()
qq2<- ggplot(ED_Euk, aes(sample = logED)) +
  stat_qq() +
  stat_qq_line()
qq3<- ggplot(ED_tsa, aes(sample = logED)) +
  stat_qq() +
  stat_qq_line()
qq4<- ggplot(ED_dia, aes(sample = logED)) +
  stat_qq() +
  stat_qq_line()
qq5<- ggplot(ED_spe, aes(sample = logED)) +
  stat_qq() +
  stat_qq_line()
qq6<- ggplot(ED_chl, aes(sample = logED)) +
  stat_qq() +
  stat_qq_line()
qq7<- ggplot(ED_hol, aes(sample = logED)) +
  stat_qq() +
  stat_qq_line()
qq8<- ggplot(ED_met, aes(sample = logED)) +
  stat_qq() +
  stat_qq_line()
qq9<- ggplot(ED_mol, aes(sample = logED)) +
  stat_qq() +
  stat_qq_line()
qq10<- ggplot(ED_che, aes(sample = logED)) +
  stat_qq() +
  stat_qq_line()
qq11<- ggplot(ED_hym, aes(sample = logED)) +
  stat_qq() +
  stat_qq_line()
qq12<- ggplot(ED_col, aes(sample = logED)) +
  stat_qq() +
  stat_qq_line()
qq13<- ggplot(ED_dip, aes(sample = logED)) +
  stat_qq() +
  stat_qq_line()
qq14<- ggplot(ED_lep, aes(sample = logED)) +
  stat_qq() +
  stat_qq_line()
qq15<- ggplot(ED_vert, aes(sample = logED)) +
  stat_qq() +
  stat_qq_line()
qq16<- ggplot(ED_agna, aes(sample = logED)) +
  stat_qq() +
  stat_qq_line()
qq17<- ggplot(ED_chon, aes(sample = logED)) +
  stat_qq() +
  stat_qq_line()
qq18<- ggplot(ED_oste, aes(sample = logED)) +
  stat_qq() +
  stat_qq_line()
qq19<- ggplot(ED_amph, aes(sample = logED)) +
  stat_qq() +
  stat_qq_line()
qq20<- ggplot(ED_croc, aes(sample = logED)) +
  stat_qq() +
  stat_qq_line()
qq21<- ggplot(ED_test, aes(sample = logED)) +
  stat_qq() +
  stat_qq_line()
qq22<- ggplot(ED_squa, aes(sample = logED)) +
  stat_qq() +
  stat_qq_line()
qq23<- ggplot(ED_mam, aes(sample = logED)) +
  stat_qq() +
  stat_qq_line()
qq24<- ggplot(ED_ave, aes(sample = logED)) +
  stat_qq() +
  stat_qq_line()
qq_sum<- ggarrange(qq1,qq2,qq3,qq4,qq5,qq6,qq7,qq8,qq9,qq10,qq11,qq12,
                   qq13,qq14,qq15,qq16,qq17,qq18,qq19,qq20,qq21,qq22,qq23,qq24,
                   labels = c("All Life","Eukaryota","TSAR","Diaphoretickes",
                              "Spermatophyta","Chloroplastida",
                              "Holomycota","Metazoa","Mollusca","Chelicerata","Hymenoptera",
                              "Coleoptera","Diptera","Lepidoptera","Vertebrata","Cyclostomata",
                              "Chondrichthyes",
                              "Actinopterygii","Amphibia","Crocodylia","Testudines",
                              "Squamata","Mammalia","Aves"),ncol = 4, nrow = 6)

##Supplementary Figure 2
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

FigS2_a <- ggplot(df_dis_tsar, aes(x = logED,fill = Group)) + geom_density(alpha = 0.5) +
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


FigS2_b <- ggplot(df_dis_dia, aes(x = logED,fill = Group)) + geom_density(alpha = 0.5) +
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
ED_hym1<-select(ED_hym,id,Group,ED,logED,data)
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

FigS2_c <- ggplot(within_hymenoptera1, aes(x = logED,fill = Group)) + geom_density(alpha = 0.7) +
  facet_grid(Group ~ .)+theme(panel.grid.major=element_blank(),
                              panel.grid.minor=element_blank())+theme_classic()+
  scale_fill_brewer(palette="Blues")+
  labs(x="Log10 (ED) Myr",y="")

FigS2 <- ggarrange(FigS1_a,FigS1_b,FigS1_c,labels = c("     TSAR","Diaphoretickes","Hymenoptera"),ncol = 3, nrow = 1)



#Supplementary Figure 3
precalculated_ed_table <- read.csv("previously calculated ed table.csv",stringsAsFactors=F, header=T)
precalculated_ed_table$logED <- log(precalculated_ed_table$ED,10)
precalculated_ed_table <- select(precalculated_ed_table,Species,logED,EDGE)
edge_table <-  read.csv("EDGE_table(final).csv",
                          stringsAsFactors=F, header=T)

edge_table$Species <- edge_table$name 
edge_table$new_EDGE <- edge_table$EDGE
edge_table<- merge(edge_table,df_name, how = "left",on = ott) 
edge_table$Species <- edge_table$name
edge_table<- select(edge_table,Species,new_EDGE)
df_name0 <- select(leaves_table,id,name)
named_ED_all <- merge(resolved_medianED,df_name0,how = "left",on = id)

named_ED_all$Species  <- named_ED_all$name
named_ED_all$new_logED <- log(named_ED_all$ed,10)
named_ED_all<- select(named_ED_all,Species,new_logED)

df_estimates_this_study <- merge(named_ED_all,edge_table,how = "left", on = Species)

scatter_all <- merge(precalculated_ed_table,df_estimates_this_study,how = "left", on = Species)

scatter_ED <- ggplot(data=scatter_all, aes(x=logED, y=new_logED)) + geom_point(alpha = .5) +
  theme_bw() + theme(panel.grid=element_blank())+theme_classic()+xlim(0.5,2.5)+ylim(0,2.5) +
  xlab("ED estimated in previous research")+ylab("ED calculated in this study")+
  geom_abline(intercept = 0, slope=1,col = "Red",size = 1)+
  stat_poly_eq(aes(label=paste(..eq.label..,..adj.rr.label..,..p.value.label..,sep = "~~~~")),formula = y~x,parse=T,size=3.5)
scatter_ED


scatter_EDGE <-  ggplot(data=scatter_all, aes(x=EDGE, y=new_EDGE)) + geom_point(alpha = .5) +
  theme_bw() + theme(panel.grid=element_blank())+theme_classic() +
  xlab("EDGE estimated in previous research")+ylab("EDGE calculated in this study")+
  geom_abline(intercept = 0, slope=1,col = "Red",size = 1)+
  stat_poly_eq(aes(label=paste(..eq.label..,..adj.rr.label..,..p.value.label..,sep = "~~~~")),formula = y~x,parse=T,size=3.5)
scatter_EDGE 

FigS3 <- ggarrange(scatter_ED,scatter_EDGE,labels = c("", ""),ncol = 2, nrow = 1)


#Supplementary Figure 4
#A stacked bar with different colors
#23 selected groups
ED_Euk <- filter(resolved_medianED,resolved_medianED$id>59642)
ED_Euk$Group = rep("Eukaryota",times = )

#"Metazoa","Holomycota","Chloroplastida","Spermatophyta","Diaphoretickes","TSAR"))
ED_met <- filter(resolved_medianED,resolved_medianED$id>805307)
ED_met$Group = rep("Metazoa",times = )

#median(ED_met$logED)
ED_hol <- filter(resolved_medianED,543113< resolved_medianED$id & resolved_medianED$id<804914)
ED_hol$Group = rep("Holomycota",times = )

ED_chl<- filter(resolved_medianED,122044 < resolved_medianED$id & resolved_medianED$id<541348)
ED_chl$Group = rep("Chloroplastida",times = )
#10**(quantile(ED_chl$logED,0.5))

ED_spe <- filter(resolved_medianED,172685< resolved_medianED$id & resolved_medianED$id<541348)
ED_spe$Group = rep("Spermatophyta",times =  )

ED_dia <- filter(resolved_medianED,63334< resolved_medianED$id & resolved_medianED$id<541348)
ED_dia$Group = rep("Diaphoretickes",times = )

ED_tsa <- filter(resolved_medianED,63777< resolved_medianED$id & resolved_medianED$id<112742)
ED_tsa$Group = rep("TSAR",times = )

#"Vertebrata","Lepidoptera","Diptera","Coleoptera","Hymenoptera","Chelicerata","Mollusca"))
ED_vert <- filter(resolved_medianED,  840048<resolved_medianED$id & resolved_medianED$id<910759)
ED_vert$Group = rep("Vertebrata",times = )

ED_lep<- filter(resolved_medianED,2056498< resolved_medianED$id)
ED_lep$Group = rep("Lepidoptera",times = )

ED_dip <- filter(resolved_medianED,1882223<resolved_medianED$id & resolved_medianED$id<2043133)
ED_dip$Group = rep("Diptera",times = )

ED_col <- filter(resolved_medianED,1595857< resolved_medianED$id & resolved_medianED$id<1879264)
ED_col$Group = rep("Coleoptera",times = )

ED_hym <- filter(resolved_medianED,1452619< resolved_medianED$id & resolved_medianED$id<1588532)
ED_hym$Group = rep("Hymenoptera",times = )
ED_che <- filter(resolved_medianED,1082354< resolved_medianED$id & resolved_medianED$id<1175804)
ED_che$Group = rep("Chelicerata",times = )

ED_mol <- filter(resolved_medianED, 972344 < resolved_medianED$id & resolved_medianED$id<1061704)
ED_mol$Group = rep("Mollusca",times = )
ED_ave <- filter(resolved_medianED, 889846< resolved_medianED$id & resolved_medianED$id<899849)
ED_ave$Group = rep("Aves",times = )

ED_mam <- filter(resolved_medianED,884546< resolved_medianED$id & resolved_medianED$id<889593)
ED_mam$Group = rep("Mammalia",times = )

ED_squa<- filter(resolved_medianED, 899849< resolved_medianED$id & resolved_medianED$id<910759)
ED_squa$Group = rep("Squamata",times = )

ED_test <- filter(resolved_medianED,889592< resolved_medianED$id & resolved_medianED$id<889824)
ED_test$Group = rep("Testudines",times = )

ED_croc <- filter(resolved_medianED,889823< resolved_medianED$id & resolved_medianED$id<889847)
ED_croc$Group = rep("Crocodylia",times = )

ED_amph<- filter(resolved_medianED,875858< resolved_medianED$id & resolved_medianED$id<884547)
ED_amph$Group = rep("Amphibia",times = )

ED_oste <- filter(resolved_medianED,841418< resolved_medianED$id & resolved_medianED$id<875851)
ED_oste$Group = rep("Actinopterygii",times = )

ED_chon <- filter(resolved_medianED,840162<resolved_medianED$id & resolved_medianED$id<841419)
ED_chon$Group = rep("Chondrichthyes",times = )

ED_agna <- filter(resolved_medianED,840048< resolved_medianED$id & resolved_medianED$id<840163)
ED_agna$Group = rep("Cyclostomata",times = )

dfED_dis2 <- rbind(ED_met,ED_hol,ED_chl,ED_spe,ED_dia,ED_tsa)
dfED_dis3 <- rbind(ED_vert,ED_lep,ED_dip,ED_col,ED_hym,ED_che,ED_mol)
dfED_dis4 <- rbind(ED_ave,ED_mam,ED_squa,ED_test,ED_croc,ED_amph,ED_oste,ED_chon,ED_agna)
dfED_Selected <- rbind(ED_Euk,dfED_dis2,dfED_dis3,dfED_dis4)

dfED_dis2 <- rbind(ED_Euk,dfED_dis2)


##group by based on group and quant
dfED_dis2$Quantile[which(dfED_dis2$Quantile == 1)] <- "Q(0.99)"
dfED_dis2$Quantile[which(dfED_dis2$Quantile == 2)] <- "Q(0.95)"
dfED_dis2$Quantile[which(dfED_dis2$Quantile == 3)] <- "Q(0.95)"
dfED_dis2$Quantile[which(dfED_dis2$Quantile == 4)] <- "Q(0.5)"
dfED_dis2$Quantile[which(dfED_dis2$Quantile == 5)] <- "Q(0.5)"
dfED_dis2$Quantile[which(dfED_dis2$Quantile == 9)] <- " "
dfED_dis2$Quantile[which(dfED_dis2$Quantile == 0)] <- " "
dfED_dis3$Quantile[which(dfED_dis3$Quantile == 1)] <- "Q(0.99)"
dfED_dis3$Quantile[which(dfED_dis3$Quantile == 2)] <- "Q(0.95)"
dfED_dis3$Quantile[which(dfED_dis3$Quantile == 3)] <- "Q(0.95)"
dfED_dis3$Quantile[which(dfED_dis3$Quantile == 4)] <- "Q(0.5)"
dfED_dis3$Quantile[which(dfED_dis3$Quantile == 5)] <- "Q(0.5)"
dfED_dis3$Quantile[which(dfED_dis3$Quantile == 9)] <- " "
dfED_dis3$Quantile[which(dfED_dis3$Quantile == 0)] <- " "

dfED_dis4$Quantile[which(dfED_dis4$Quantile == 1)] <- "Q(0.99)"
dfED_dis4$Quantile[which(dfED_dis4$Quantile == 2)] <- "Q(0.95)"
dfED_dis4$Quantile[which(dfED_dis4$Quantile == 3)] <- "Q(0.95)"
dfED_dis4$Quantile[which(dfED_dis4$Quantile == 4)] <- "Q(0.5)"
dfED_dis4$Quantile[which(dfED_dis4$Quantile == 5)] <- "Q(0.5)"
dfED_dis4$Quantile[which(dfED_dis4$Quantile == 9)] <- " "
dfED_dis4$Quantile[which(dfED_dis4$Quantile == 0)] <- " "
dfED_dis2$loged <- log(dfED_dis2$ed,10)
#dfED_dis2$Quant<- dfED_dis2$quantile
dfED_dis2 <- select(dfED_dis2,id,Group,Quantile)
dfED_dis2$num <- rep(1,times = )

dfED_dis3$loged <- log(dfED_dis3$ed,10)
#dfED_dis3$Quant<- dfED_dis3$quantile
dfED_dis3 <- select(dfED_dis3,id,Group,Quantile)
dfED_dis3$num <- rep(1,times = )

dfED_dis4$loged <- log(dfED_dis4$ed,10)
#dfED_dis4$Quant<- dfED_dis4$quantile
dfED_dis4 <- select(dfED_dis4,id,Group,Quantile)
dfED_dis4$num <- rep(1,times = )
library(plyr)
library(tidyverse)
library(readxl)
dfED_dis2$Quantile <- factor(dfED_dis2$Quantile)
dfED_dis3$Quantile <- factor(dfED_dis3$Quantile)
dfED_dis4$Quantile <- factor(dfED_dis4$Quantile)

dfED_dis2_1 <- aggregate(x = dfED_dis2$num, by = list(dfED_dis2$Group,
                                          dfED_dis2$Quant),sum)
dfED_dis2_1 <- ddply(dfED_dis2_1,"Group.1",transform,percent_quant = x/sum(x)*100)

dfED_dis2_1$Group.1 <- factor(dfED_dis2_1 $Group.1,levels = c("Eukaryota","TSAR","Diaphoretickes",
                                                          "Spermatophyta",
                                                          "Chloroplastida","Holomycota","Metazoa"))
dfED_dis2_1$Group.2 <- factor(dfED_dis2_1$Group.2 ,levels = c("Q(0.99)","Q(0.95)","Q(0.5)"," "))

library(colorspace) 

dfED_dis2_1$Quantile <- dfED_dis2_1$Group.2
dfED_dis2_1$Clade <- dfED_dis2_1$Group.1
p2 <- ggplot(dfED_dis2_1,aes(x=Clade,y=percent_quant,fill=Quantile))+
  geom_bar(stat = 'identity',width = 0.5,colour = "black")+
 theme_classic()   + scale_fill_discrete_sequential(palette = "Peach", 
                                                  nmax = 4, 
                                                  rev = FALSE, 
                                                  order = 1:5)

dfED_dis3_1 <- aggregate(x = dfED_dis3$num, by = list(dfED_dis3$Group,
                                                      dfED_dis3$Quant),sum)
dfED_dis3_1 <- ddply(dfED_dis3_1,"Group.1",transform,percent_quant = x/sum(x)*100)
dfED_dis3_1$Group.1 <- factor(dfED_dis3_1 $Group.1,levels = c("Mollusca","Chelicerata","Hymenoptera",
                                                              "Coleoptera","Diptera","Lepidoptera","Vertebrata"))
dfED_dis3_1$Group.2 <- factor(dfED_dis3_1$Group.2 ,levels = c("Q(0.99)","Q(0.95)","Q(0.5)"," "))
dfED_dis3_1$Quantile <- dfED_dis3_1$Group.2
dfED_dis3_1$Clade <- dfED_dis3_1$Group.1
p3 <- ggplot(dfED_dis3_1,aes(x=Clade,y=percent_quant,fill=Quantile))+
  geom_bar(stat = 'identity',width = 0.5,colour = "black")+
  theme_classic()+ scale_fill_discrete_sequential(palette = "Blues 3", 
                                                    nmax = 5, 
                                                    rev = FALSE, 
                                                    order = 1:5)

dfED_dis4_1 <- aggregate(x = dfED_dis4$num, by = list(dfED_dis4$Group,
                                                      dfED_dis4$Quant),sum)
dfED_dis4_1 <- ddply(dfED_dis4_1,"Group.1",transform,percent_quant = x/sum(x)*100)
dfED_dis4_1$Group.1 <- factor(dfED_dis4_1 $Group.1,levels = c("Cyclostomata","Chondrichthyes","Actinopterygii","Amphibia",
                                                              "Crocodylia","Testudines","Squamata","Mammalia",
                                                              "Aves"))
dfED_dis4_1$Group.2 <- factor(dfED_dis4_1$Group.2 ,levels = c("Q(0.99)","Q(0.95)","Q(0.5)"," "))
dfED_dis4_1$Quantile <- dfED_dis4_1$Group.2
dfED_dis4_1$Clade <- dfED_dis4_1$Group.1

p4 <- ggplot(dfED_dis4_1,aes(x=Clade,y=percent_quant,fill=Quantile))+
  geom_bar(stat = 'identity',width = 0.5,colour = "black")+
  theme_classic()+  scale_fill_discrete_sequential(palette = "Reds 3", 
                                                    nmax = 5, 
                                                    rev = FALSE, 
                                                    order = 1:5)

FigS4 <- ggarrange(p2,p3,p4,labels = c("","",""),ncol = 1, nrow = 3)
FigS4



##Supplemenrary Figure 5
ave_ed_table24 <-  read.csv("average_ed_table_24(real_parent).csv",
                            stringsAsFactors=F, header=T)
getname <- select(pd_table24,id,name)
ave_ed_table24 <- merge(ave_ed_table24,getname, how = "left",on = id)
ave_ed_table24_l <- pivot_longer(ave_ed_table24, cols = X0:X99, 
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
FigS5<- ggplot(ave_ed_table24_l , aes(fill = name,y = name, x = log(ED,10))) + 
  geom_boxplot(alpha = 0.6, width = 0.7)+theme_classic()+
  scale_fill_manual(values=c(Biota = "#fdc58f", Eukaryota = "#f47720",TSAR = "#f47720",
                             Diaphoretickes= "#f47720",Spermatophyta="#f47720",
                             Chloroplastida = "#f47720",Holomycota= "#f47720",
                             Metazoa = "#4d97cd", Mollusca ="#4d97cd",
                             Chelicerata = "#4d97cd",Hymenoptera= "#4d97cd",
                             Coleoptera = "#4d97cd",Diptera= "#4d97cd", 
                             Lepidoptera ="#4d97cd",Vertebrata = "#ea9c9d",
                             Cyclostomata="#ea9c9d" ,Chondrichthyes ="#ea9c9d" ,Cyclostomata = "#ea9c9d",
                             Amphibia = "#ea9c9d" ,Crocodylia = "#ea9c9d",Testudines = "#ea9c9d",
                             Squamata= "#ea9c9d",Mammalia = "#ea9c9d",
                             Aves= "#ea9c9d"))+ theme(legend.title = element_text(size = 30))+
  labs(x="Log10 (Average ED) Myr",y="")+ theme(text = element_text(size = 17))

#Supplementary Figure 6 and 7
df_phylo <-  read.csv("phyloinfo_for_edge20_with_ed.csv",
                      stringsAsFactors=F, header=T)
getname <- select(leaves_table,id,name)
df_phylo <- merge(df_phylo,getname, how = "left",on = id)
df_phylo$id <- as.factor(df_phylo$id)
ed_top20$rank = c('No.1','No.2','No.3',"No.4","No.5","No.6","No.7","No.8","No.9",'No.10',
                  'No.11','No.12','No.13','No.14','No.15','No.16','No.17','No.18','No.19','No.20')
ed_top20$rank2 = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20)
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
Fig_S6 <- ggplot(data=df_phylo_top10,aes(x=(0-log_age),y=log(node_ED,10),color = rank,alpha = 0.9))+
  geom_point(aes(shape = Group, color=rank,size = Group))+theme_classic()+
  scale_shape_manual(values = c("Bifurcating Node without Date"=1,"Dated Node" = 19,"Resolved Node" = 13,"Leaf" = 1))+
  scale_size_manual(values= c("Resolved Node"=2, "Bifurcating Node without Date"=4,"Dated Node"=6,"Leaf" = 6))+
  geom_line(aes(color=rank))+
  scale_color_manual(values=c(No.1= "#c74546",No.2= "#c74546",No.3= "#fdc58f",No.4= "#fdc58f",
                              No.5= "#4d97cd",No.6= "#c74546",No.7= "#4d97cd",No.8= "#c74546",
                              No.9="#4d97cd",No.10= "#c74546"))+xlab("Log10(Node Date Estimate) Myr")+
  ylab("Log10(ED) Myr") +facet_wrap(~ rank, scales = "free")+theme_classic()


