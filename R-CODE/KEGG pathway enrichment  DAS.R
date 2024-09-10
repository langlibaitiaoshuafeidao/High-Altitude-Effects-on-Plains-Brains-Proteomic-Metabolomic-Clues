rm(list = ls())
library(tidyverse)
dat <-  read.delim('DEM KEGG.txt')#reading data
names(dat)
               dat <- dat %>% 
                 mutate(diff.abundance = (Diff.upcounts - Diff.downcounts)/Background.counts,
                        x=rep(0,n=nrow(.))) %>% #diff.abundance=x.trend
                 slice_min(order_by =P.value,n=20) %>% 
                 mutate(index =seq(1,20)) %>% 
                 mutate(
                   group=case_when(
                     diff.abundance>0~"more up-reulated metabolites",
                     diff.abundance<0~"more down-regulated metabolites",
                     TRUE~"equal up- and down-regulated metabolites"
                   ),
                   label=paste0("p=",round(P.value,3)," count=",Diff.counts))
               plot.kegg2 <- ggplot() +
                 geom_vline(xintercept = c(0),size = 0.1,color = "#1f294e")+
                 ggforce::geom_link(data = dat,aes(x = x,
                                                   y = reorder(pathway, -index),
                                                   xend = diff.abundance,yend = pathway,
                                                   size = after_stat(index),
                                                   alpha = after_stat(index),
                                                   color=group
                 ),
                 show.legend = F)+
                 geom_point(data = dat,
                            aes(x = diff.abundance, y = pathway,color=group,
                            ),
                            size=5,
                            shape = 21,fill = "white")+
                 scale_colour_manual(values =c("#492952","#00B0F0","#EE0000"))+
                 guides(color=guide_legend('pathway condition')
                 )+ 
                 geom_text(dat,mapping=aes(x=diff.abundance,y=pathway,label=label),
                           size =2.5,color="grey20",
                           hjust = 0, nudge_x = 0.05)+
                 theme_test()+
                 xlab("Differential Abundance Score")+
                 ylab(NULL)
               ggsave("outputfile/KEGG_abundancescore2.png", width = 8, height = 4.5)
               print(plot.kegg2)
               ggsave("outputfile/KEGG_abundancescore2.pdf", width =8, height = 4.5,onefile=F)
               print(plot.kegg2)
               dev.off()   
               plot.kegg3 <- 
                 ggplot() +
                 geom_vline(xintercept = c(0),size = 0.1,color = "#1f294e")+
                 ggforce::geom_link(data = dat,aes(x = x,
                                                   y = reorder(pathway, -index),
                                                   xend = diff.abundance,yend = pathway,
                                                   size = after_stat(index),
                                                   alpha = after_stat(index),
                                                   color=group
                 ),show.legend = F)+
                 geom_point(data = dat,
                            aes(x = diff.abundance, y = pathway,color=group,
                                size=(Diff.counts)
                            ),
                            shape = 21,fill = "white")+
                 scale_colour_manual(values =c("#492952","#00B0F0","#EE0000"))+
                 guides(color=guide_legend('pathway condition'),
                        size=guide_legend('metabolite counts in each pathway'))+ 
                 geom_text(dat,mapping=aes(x=diff.abundance,y=pathway,label=label),
                           size =2.5,color="grey20",
                           hjust = 0, nudge_x = 0.05)+
                 theme_test()+  
                 xlab("Differential Abundance Score")+
                 ylab(NULL)
               #saving
               ggsave("outputfile/KEGG_abundancescore3.png", width = 8, height = 4.5)
               print(plot.kegg3)
               ggsave("outputfile/KEGG_abundancescore3.pdf", width =8, height = 4.5,onefile=F)
               print(plot.kegg3)
               dev.off()      