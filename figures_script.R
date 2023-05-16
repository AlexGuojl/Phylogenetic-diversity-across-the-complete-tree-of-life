rm(list = ls())
setwd("/Users/alexgjl/Desktop/master/项目2/文件/")###can be change to

library(ggplot2)
library(dplyr)
library(tidyr)
library(ggpubr)
library(ggpmisc)
library(RColorBrewer)

#Fig2: The distribution of ED scores across selected groups
df_medianED <-  read.csv("median_ed.csv",
                         stringsAsFactors=F, header=T)
#
##animals——vertbrate
df_medianED$logED <- log(df_medianED$median_ed,10)
df_medianED$Group <- rep("All",times = 2235076)
#All
ED_all <- select(df_medianED, id,Group,logED)

#"Eubacteria","All"

ED_Euk <- filter(ED_all,ED_all$id>59642)
ED_Euk$Group = rep("Eukaryota",times = 2175434)

dfED_dis1 <- rbind(ED_all,ED_Euk)



#"Metazoa","Holomycota","Chloroplastida","Spermatophyta","Diaphoretickes","TSAR"))
ED_met <- filter(ED_all,ED_all$id>805307)
ED_met$Group = rep("Metazoa",times = 1429769)

ED_hol <- filter(ED_all,543113< ED_all$id & ED_all$id<804914)
ED_hol$Group = rep("Holomycota",times = 261800)

ED_chl<- filter(ED_all,122044 < ED_all$id & ED_all$id<541348)
ED_chl$Group = rep("Chloroplastida",times = 419303)
10**(quantile(ED_chl$logED,0.5))

ED_spe <- filter(ED_all,172685< ED_all$id & ED_all$id<541348)
ED_spe$Group = rep("Spermatophyta",times = 368662 )

ED_dia <- filter(ED_all,63334< ED_all$id & ED_all$id<541348)
ED_dia$Group = rep("Diaphoretickes",times = 478013)

ED_tsa <- filter(ED_all,63777< ED_all$id & ED_all$id<112742)
ED_tsa$Group = rep("TSAR",times = 48964)

dfED_dis2 <- rbind(ED_met,ED_hol,ED_chl,ED_spe,ED_dia,ED_tsa)

#"Vertebrata","Lepidoptera","Diptera","Coleoptera","Hymenoptera","Chelicerata","Mollusca"))
ED_vert <- filter(ED_all,  840048<ED_all$id & ED_all$id<910759)
ED_vert$Group = rep("Vertebrata",times = 70710)

ED_lep<- filter(ED_all,2056498< ED_all$id)
ED_lep$Group = rep("Lepidoptera",times = 178578)

ED_dip <- filter(ED_all,1882223< ED_all$id & ED_all$id<2043133)
ED_dip$Group = rep("Diptera",times = 160909)

ED_col <- filter(ED_all,1595857< ED_all$id & ED_all$id<1879264)
ED_col$Group = rep("Coleoptera",times = 283406)


ED_hym <- filter(ED_all,1452619< ED_all$id & ED_all$id<1588532)
ED_hym$Group = rep("Hymenoptera",times = )
##

ED_che <- filter(ED_all,1082354< ED_all$id & ED_all$id<1175804)
ED_che$Group = rep("Chelicerata",times = )

ED_mol <- filter(ED_all, 972344 < ED_all$id & ED_all$id<1061704)
ED_mol$Group = rep("Mollusca",times = )

dfED_dis3 <- rbind(ED_vert,ED_lep,ED_dip,ED_col,ED_hym,ED_che,ED_mol)


#"Aves","Mammalia","Squamata","Testudines","Crocodilia","Amphibia",
#"Actinopterygii","Chondrichthyes",
#"cyclostomata(agnatha)"))
ED_ave <- filter(ED_all, 889846< ED_all$id & ED_all$id<899849)
ED_ave$Group = rep("Aves",times = )

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
dfED_dis1$Group[dfED_dis1$Group == "All"] <- "All_Life"

dfED_dis_new1 <- rbind(dfED_dis1,dfED_dis2)

dfED_dis_new2 <- dfED_dis3
dfED_dis_new3 <- dfED_dis4

dfED_dis_new1$Group <- factor(dfED_dis_new1$Group,levels = c("All_Life","Eukaryota","TSAR","Diaphoretickes",
                                                             "Spermatophyta",
                                                             "Chloroplastida","Holomycota","Metazoa"))

dis_plot1 <- ggplot(dfED_dis_new1, aes(x = logED,fill = Group)) + geom_density(alpha = 0.3) +
  facet_grid(Group ~ .)+theme(panel.grid.major=element_blank(),
                              panel.grid.minor=element_blank())+theme_classic()+
  scale_fill_manual(values=c( All_Life = "#99FF66", Eukaryota = "#FF9933",TSAR = "#FF9933",
                              Diaphoretickes= "#FF9933",Spermatophyta= "#FF9933",
                              Chloroplastida = "#FF9933",Holomycota= "#FF9933",
                              Metazoa = "#3399FF"))+
  labs(x="Log10(ED) Myr",y="")

dfED_dis_new2$Group <- factor(dfED_dis_new2$Group,levels = c("Mollusca","Chelicerata","Hymenoptera",
                                                             "Coleoptera","Diptera","Lepidoptera","Vertebrata"))

dis_plot2 <- ggplot(dfED_dis_new2, aes(x = logED,fill = Group)) + geom_density(alpha = 0.3) +
  facet_grid(Group ~ .)+theme(panel.grid.major=element_blank(),
                              panel.grid.minor=element_blank())+theme_classic()+
  scale_fill_manual(values=c(Mollusca = "#3399FF"
                             ,Chelicerata = "#3399FF",Hymenoptera= "#3399FF",
                             Coleoptera = "#3399FF",Diptera= "#3399FF", 
                             Lepidoptera = "#3399FF",Vertebrata = "#FF3333" ))+
  labs(x="Log10(ED) Myr",y="")


dfED_dis_new3$Group <- factor(dfED_dis_new3$Group,levels = c("Cyclostomata","Chondrichthyes","Actinopterygii","Amphibia",
                                                             "Crocodylia","Testudines","Squamata","Mammalia",
                                                             "Aves"))

dis_plot3 <- ggplot(dfED_dis_new3, aes(x = logED,fill = Group)) + geom_density(alpha = 0.3) +
  facet_grid(Group ~ .)+theme(panel.grid.major=element_blank(),
                              panel.grid.minor=element_blank())+theme_classic()+
  scale_fill_manual(values=c( Cyclostomata="#FF3333" ,Chondrichthyes ="#FF3333" ,Actinopterygii = "#FF3333",
                              Amphibia = "#FF3333" ,Crocodylia = "#FF3333",Testudines = "#FF3333",
                              Squamata= "#FF3333",Mammalia = "#FF3333",
                              Aves= "#FF3333"))+ theme(legend.title = element_text(size = 30))+
  labs(x="Log10(ED) Myr",y="")
library(ggpubr)
Fig2 <- ggarrange(dis_plot1,dis_plot2,dis_plot3,labels = c("","",""),ncol = 3, nrow = 1)
Fig2



#Supplementary Fig 1
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

FigS1_a <- ggplot(df_dis_tsar, aes(x = logED,fill = Group)) + geom_density(alpha = 0.5) +
  facet_grid(Group ~ .)+theme(panel.grid.major=element_blank(),
                              panel.grid.minor=element_blank())+theme_classic()+
  scale_fill_manual(values = c(Rhizaria = "#FFEDA0",Alveolata = "#FFFFCC",
                               Stramenopiles = "#FED976",TSAR = "#FEB24C"))+
  #scale_fill_brewer("mycolors")+
  labs(x="Log10 (ED) Myr",y="")



#s1b dia
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


FigS1_b <- ggplot(df_dis_dia, aes(x = logED,fill = Group)) + geom_density(alpha = 0.5) +
  facet_grid(Group ~ .)+theme(panel.grid.major=element_blank(),
                              panel.grid.minor=element_blank())+theme_classic()+
  scale_fill_manual(values = c(Haptista = "#FFFFCC",TSAR = "#FFEDA0",
                               Cryptista = "#FED976",Archaeplastida = "#FEB24C",Diaphoretickes = "#FD8D3C"))+
  #scale_fill_brewer("mycolors")+
  labs(x="Log10 (ED) Myr",y="")



#s1c hymenoptera
ED_bees <- filter(ED_all,1493995< ED_all$id & ED_all$id<1516043)
ED_ants <- filter(ED_all,1471246< ED_all$id & ED_all$id<1485398)
##
ED_bees$ED <- 10**(ED_bees$logED)#unnamed_nodes
ED_bees$Group <- rep("Bee",times = 22047) 

ED_ants$ED <- 10**(ED_ants$logED)#Formicidae
ED_ants$Group <- rep("Ant",times = 14151) 
ED_hym$ED <- 10**(ED_hym$logED)
within_hymenoptera <- rbind(ED_hym,ED_bees,ED_ants)
hyme_other1 <- filter(ED_all,1452619< ED_all$id & ED_all$id<1471246)
hyme_other2 <- filter(ED_all,1485397< ED_all$id & ED_all$id<1493995)
hyme_other3 <- filter(ED_all,1516042< ED_all$id & ED_all$id<1588532)

hyme_other <- rbind(hyme_other1,hyme_other2,hyme_other3)
hyme_other$ED <- 10**(hyme_other$logED)
hyme_other$Group <- rep("Others",times = 99712)
within_hymenoptera1 <- rbind(within_hymenoptera,hyme_other)

within_hymenoptera1$Group <- factor(within_hymenoptera1$Group,levels = c("Ant","Bee","Others","Hymenoptera"))

within_hymenoptera2 <- within_hymenoptera1

within_hymenoptera2$Group <- factor(within_hymenoptera1$Group,levels = c("Hymenoptera","Others","Bee","Ant"))

FigS1_c <- ggplot(within_hymenoptera1, aes(x = logED,fill = Group)) + geom_density(alpha = 0.7) +
  facet_grid(Group ~ .)+theme(panel.grid.major=element_blank(),
                              panel.grid.minor=element_blank())+theme_classic()+
  scale_fill_brewer(palette="Blues")+
  labs(x="Log10 (ED) Myr",y="")


#s1d chondrichthyes
ED_chon <- filter(ED_all,840162< ED_all$id & ED_all$id<841419)
ED_chon$Group = rep("Chondrichthyes",times = )

ED_holoce <- filter(ED_chon,id<840218)
ED_holoce$Group = rep("Holocephali",times = )

ED_sela <- filter(ED_chon, 840217< id & id<840661)
ED_sela$Group <- rep("Selachii",times = )

ED_bato <- filter(ED_chon,id>840660)
ED_bato$Group <- rep("Batoidea",times = )

df_des_chon <- rbind(ED_chon,ED_holoce,ED_sela,ED_bato)

df_des_chon$Group <- factor(df_des_chon$Group, levels = c("Holocephali","Selachii","Batoidea","Chondrichthyes"))

FigS1_d <- ggplot(df_des_chon, aes(x = logED,fill = Group)) + geom_density(alpha = 0.5) +
  facet_grid(Group ~ .)+theme(panel.grid.major=element_blank(),
                              panel.grid.minor=element_blank())+theme_classic()+
  scale_fill_brewer(palette="Oranges")+
  labs(x="Log10 (ED) Myr",y="")







##Fig3
pd_table24 <-  read.csv("pd_table24.csv",
                        stringsAsFactors=F, header=T)
Fig3 <- ggplot(pd_table24, aes(fill = name,y = name, x = log(Total_PD,10))) + 
  geom_boxplot(alpha = 0.6, width = 0.7)+theme_classic()+
  scale_fill_manual(values=c(All_Life = "#99FF66", Eukaryota = "#FF9933",TSAR = "#FF9933",
                             Diaphoretickes= "#FF9933",Spermatophyta= "#FF9933",
                             Chloroplastida = "#FF9933",Holomycota= "#FF9933",
                             Metazoa = "#3399FF", Mollusca = "#3399FF",
                             Chelicerata = "#3399FF",Hymenoptera= "#3399FF",
                             Coleoptera = "#3399FF",Diptera= "#3399FF", 
                             Lepidoptera = "#3399FF",Vertebrata = "#FF3333",
                             Cyclostomata="#FF3333" ,Chondrichthyes ="#FF3333" ,Cyclostomata = "#FF3333",
                             Amphibia = "#FF3333" ,Crocodylia = "#FF3333",Testudines = "#FF3333",
                             Squamata= "#FF3333",Mammalia = "#FF3333",
                             Aves= "#FF3333"))+ theme(legend.title = element_text(size = 30))+
  labs(x="Log10 (PD) Myr",y="")+ theme(text = element_text(size = 17))




#Fig 4
sampled_named_PD <-  read.csv("sample_named_PD_3.0.csv",
                              stringsAsFactors=F, header=T)

sampled_unnamed_PD <-  read.csv("sample_unnamed_PD_3.0.csv",
                                stringsAsFactors=F, header=T)

sampled_named_PD <-  read.csv("sample_named_PD_3.0.csv",
                              stringsAsFactors=F, header=T)

sampled_unnamed_PD <-  read.csv("sample_unnamed_PD_3.0.csv",
                                stringsAsFactors=F, header=T)

sampled_named_PD$Group <- rep("Named_Clades",times = 2073)
sampled_unnamed_PD$Group <- rep("Unnamed_Clades",times = 3417)


scatter_PD <- ggplot(data=sampled_named_PD, aes(x=log(richness,10), y=log(pd,10))) + geom_point(alpha = .5) +
  theme_bw() + theme(panel.grid=element_blank())+theme_classic()+
  xlab("Log(10) Richness")+ylab("Log10 (PD) Myr")


scatter_unnamed_PD <- ggplot(data=sampled_unnamed_PD, aes(x=log(richness,10), y=log(pd,10))) + geom_point(alpha = .5) +
  theme_bw() + theme(panel.grid=element_blank())+theme_classic()+
  xlab("Log(10) Richness")+ylab("Log10 (PD) Myr")


Fig4 <- ggarrange(scatter_PD,scatter_unnamed_PD,labels = c("Named Groups","Unnamed Groups"),ncol = 2, nrow = 1)



#Fig 5
Fig5 <- ggplot(pd_table24,aes(x=as.factor(log_Richness),y=log(Total_PD,10),fill=Group,alpha = 0.6))+
  geom_boxplot(size=0.5,fill="white",outlier.fill="white",outlier.color="white")+ 
  geom_jitter(aes(fill=Group),width =0.2,shape = 21,size=1.5)+ 
  scale_fill_manual(values=c(All_Life = "#009933",Eukaryota = "#FF9933", Metazoa = "#3399FF", Vertebrata = "#FF6666"))+ 
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

#Fig6
ed_top20 <- read.csv("top20edge_ed.csv",encoding = "gbk",stringsAsFactors=F, header=T)
#remove the chinese paddle fish, which was already reported extinct

ed_top20 <- ed_top20[-3,]


ed_top20$name <- factor(ed_top20$name,levels = c("Crotaphatrema lamottei","Speleoperipatus spelaeus","Symmetromphalus hageni","Aneuretus simoni", "Nanocopia minuta",
                                                 "Geothallus tuberosus","Antrisocopia prehensilis","Eostemmiulus caecus","Lepidogalaxias salamandroides",
                                                 "Leiopelma archeyi","Euastacus girurmulayn","Erymnochelys madagascariensis","Mictocaris halope",
                                                 "Dermatemys mawii","Acharax alinae","Acipenser sturio","Procaris chacei",
                                                 "Galaxaura barbata","Latimeria chalumnae","Mankyua chejuensis"))


ed_top20$class <- c("Polypodiopsida","Osteichthyes","Florideophyceae","Malacostraca","Osteichthyes",
                    "Mollusca","Testudines","Malacostraca","Testudines",
                    "Malacostraca","Amphibia","Osteichthyes","Diplopoda",
                    "Copepoda",
                    
                    "Marchantiopsida",
                    "Copepoda","Insecta","Mollusca","Udeonychophora",
                    "Amphibia")

ed_top20$Group <- c("Plantae","Vertebrata","Plantae","Invertebrata","Vertebrata",
                    "Invertebrata","Vertebrata","Invertebrata","Vertebrata",
                    "Invertebrata","Vertebrata",
                    "Vertebrata", "Invertebrata","Invertebrata","Plantae",
                    "Invertebrata","Invertebrata","Invertebrata",
                    "Invertebrata","Vertebrata")



ed_top20_l <- pivot_longer(ed_top20, cols = new_id:new_id.9.4, names_to = 'Species',values_to = 'ED')
ed_top20_l <- select(ed_top20_l,Group,name,ED)
ed_top20_l$name <-  factor(ed_top20_l$name,levels = c("Crotaphatrema lamottei","Speleoperipatus spelaeus","Symmetromphalus hageni","Aneuretus simoni", "Nanocopia minuta",
                                                      "Geothallus tuberosus","Antrisocopia prehensilis","Eostemmiulus caecus","Lepidogalaxias salamandroides",
                                                      "Leiopelma archeyi","Euastacus girurmulayn","Erymnochelys madagascariensis","Mictocaris halope",
                                                      "Dermatemys mawii","Acharax alinae","Acipenser sturio","Procaris chacei",
                                                      "Galaxaura barbata","Latimeria chalumnae","Mankyua chejuensis"))

#box_plot

#Fig5
box_edge20 <- ggplot(ed_top20_l, aes(y = name, x = ED,fill = Group)) + 
  geom_boxplot(alpha = 0.6)+theme_classic()
box_edge20+xlim(0,400)+
  scale_fill_manual(values=c(Plantae= "#FF9933",Vertebrata = "#FF3333",
                             Invertebrata = "#3399FF"))+ 
  theme(text = element_text(size = 15))+labs(x="ED (Myr)",y="")


#supplementary Figure 2

precalculated_ed_table <- read.csv("previously calculated ed table.csv",stringsAsFactors=F, header=T)

precalculated_ed_table$logED <- log(precalculated_ed_table$ED,10)
precalculated_ed_table <- select(precalculated_ed_table,Species,logED,EDGE)

edge_table <-  read.csv("EDGE_table_All.csv",
                          stringsAsFactors=F, header=T)

edge_table$Species <- edge_table$name 
edge_table$new_EDGE <- edge_table$EDGE

edge_table<- select(edge_table,Species,new_EDGE)


df_name <- select(leaves_table,id,name)
named_ED_all <- merge(ED_all,df_name,how = "left",on = id)

named_ED_all$Species  <- named_ED_all$name
named_ED_all$new_logED <- named_ED_all$logED
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

FigS2 <- ggarrange(scatter_ED,scatter_EDGE,labels = c("", ""),ncol = 2, nrow = 1)
FigS2


##Fig S3

sumED_all <-  read.csv("total_pd_all.csv",
                    stringsAsFactors=F, header=T)##total_pd in this table is sum—ED！


boo_sum_all <- sumED_all[,-1]


library(tidyr)
sumED_all <- pivot_longer(boo_sum_all, cols = X1:X100, names_to = 'Group',values_to = 'Sum_ED')

sumED_all <- select(sumED_all,name,Sum_ED)

sumED_all$name[which(sumED_all$name == "Crocodilia")] <- "Crocodylia"
sumED_all$name[which(sumED_all$name == "Agnatha")] <- "Cyclostomata"

sumED_all$name <- factor(sumED_all$name,levels =  c("Aves","Mammalia","Squamata","Testudines","Crocodylia","Amphibia",
                                                      "Actinopterygii","Chondrichthyes",
                                                      "Cyclostomata","Vertebrata","Lepidoptera","Diptera","Coleoptera",
                                                      "Hymenoptera","Chelicerata","Mollusca","Metazoa","Holomycota","Chloroplastida",
                                                      "Spermatophyta","Diaphoretickes",
                                                      "TSAR","Eukaryota","All_Life"))


df_richness <- select(pd_table24,name,Richness)
sumED_all <- merge(sumED_all,df_richness,how = "left",on = name)
sumED_all$averageED <- sumED_all$Sum_ED/sumED_all$Richness


box_panel_total2 <- ggplot(sumED_all, aes(fill = name,y = name, x = log(averageED,10))) + 
  geom_boxplot(alpha = 0.6, width = 0.7)+theme_classic()+
  scale_fill_manual(values=c(All_Life = "#99FF66", Eukaryota = "#FF9933",TSAR = "#FF9933",
                             Diaphoretickes= "#FF9933",Spermatophyta= "#FF9933",
                             Chloroplastida = "#FF9933",Holomycota= "#FF9933",
                             Metazoa = "#3399FF", Mollusca = "#3399FF"
                             ,Chelicerata = "#3399FF",Hymenoptera= "#3399FF",
                             Coleoptera = "#3399FF",Diptera= "#3399FF", 
                             Lepidoptera = "#3399FF",Vertebrata = "#FF3333",
                             Cyclostomata="#FF3333" ,Chondrichthyes ="#FF3333" ,Actinopterygii = "#FF3333",
                             Amphibia = "#FF3333" ,Crocodylia = "#FF3333",Testudines = "#FF3333",
                             Squamata= "#FF3333",Mammalia = "#FF3333",
                             Aves= "#FF3333"))+ theme(legend.title = element_text(size = 30))+
  labs(x="Log10 (ED) Myr",y="")+ theme(text = element_text(size = 17))

box_panel_total2

