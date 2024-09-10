
library(reshape2)
library(ggalluvial)
ceRNA <- read.csv('ce.csv', stringsAsFactors = FALSE)
color <- c('#8DD3C7', '#FFFFB3', '#BEBADA', '#FB8072', '#80B1D3', '#FDB462', 
'#B3DE69', '#FCCDE5', '#BC80BD', '#CCEBC5', '#FFED6F', '#E41A1C', 
'#377EB8', '#4DAF4A', '#984EA3', '#FF7F00', '#FFFF33', '#A65628', 
'#F781BF', '#66C2A5', '#6181BD', '#F34800', '#64A10E', '#FF00FF', 
'#c7475b', '#049a0b', '#BEAED4', '#FDC086', '#FFFF99', '#386CB0', 
'#4253ff', '#ff4308', '#D8D155', '#F0027F', '#9FAED4', '#F7CDBD','#8DD3C7', '#FFFFB3', '#BEBADA', '#FB8072', '#80B1D3','#8DD3C7', '#FFFFB3', '#BEBADA', '#FB8072', '#80B1D3','#8DD3C7', '#FFFFB3', '#BEBADA', '#FB8072', '#80B1D3','#8DD3C7', '#FFFFB3', '#BEBADA', '#FB8072', '#80B1D3','#8DD3C7', '#FFFFB3', '#BEBADA', '#FB8072', '#80B1D3', '#80B1D3', '#80B1D3', '#80B1D3')

ceRNA$link <- 1
ceRNA <- reshape::melt(ceRNA, id = 'link')

variable <- summary(ceRNA$variable)
ceRNA$flow <- rep(1:variable[1], length(variable))

link <- 1 / table(ceRNA$value)
for (i in names(link)) ceRNA[which(ceRNA$value == i),'link'] <- link[i]
ggplot(ceRNA, aes(x = variable, y = link, stratum = value, alluvium = flow, fill = value)) +
geom_stratum() + 
geom_flow(aes.flow = 'forward') +  
scale_fill_manual(values = color) + 
geom_text(stat = 'stratum', infer.label = TRUE, size = 2.5) +  
scale_x_discrete(limits = c('ln', 'mi', 'm')) + 
theme(legend.position = 'none', panel.background = element_blank(), line = element_blank(), axis.text.y = element_blank()) +
labs(x = '', y = '')

