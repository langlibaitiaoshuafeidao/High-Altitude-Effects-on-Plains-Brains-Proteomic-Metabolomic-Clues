library(pathview)
gene_data <- read.delim('pro_data.4group.txt', row.names = 1) 
compound_data <- read.delim('compound_data.3group.txt', row.names = 1) 

p <- pathview(
	gene.data = gene_data,  
	gene.idtype = 'entrez',  
	cpd.data = compound_data, 
	cpd.idtype = "kegg", 
	species = 'hsa',  
	kegg.native = FALSE,  
	pathway.id = c('00480','04210','00020','00010','00620','00190','00030')  
)


compound_data <- read.delim('compound_data.3group.txt', row.names = 1)  


p <- pathview(
  cpd.data = compound_data, 
  cpd.idtype = "kegg", 
  species = 'hsa',  
  kegg.native = FALSE, 
  pathway.id = c('00480','04210','00020','00010','00620','00190','00030')  
)






