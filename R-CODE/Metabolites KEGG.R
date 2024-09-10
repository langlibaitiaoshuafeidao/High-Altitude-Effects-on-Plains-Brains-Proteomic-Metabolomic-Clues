rm(list = ls())
library(tidyverse)
read_and_rename <- function(file_path, col_name) {
  data <- openxlsx::read.xlsx(file_path) %>%
    rename("Metabolites_CID" = col_name)
  return(data)
}
metaboDiff <- read_and_rename("diff.xlsx", "KEGG")
metaboBackground <- read_and_rename("all_meta_intensity.anno.xlsx", "KEGG")
result_long <- read_csv("KeggHsacompounds-2023-11-25.csv")
enrichBackground <- right_join(result_long, metaboBackground, by = "Metabolites_CID") %>%
  select(Pathway_name, Metabolites_CID)
enrichResult <- clusterProfiler::enricher(
  gene = metaboDiff$Metabolites_CID,
  TERM2GENE = enrichBackground,
  minGSSize = 1,
  pvalueCutoff = 0.05,
  qvalueCutoff = 1
)
enrichData <- enrichResult@result %>%
  rename("Diff.counts" = "Count", "P.value" = "pvalue")
enrichData$Total.background <- str_extract(enrichData$BgRatio, "\\d+(?=/)")
diff_counts <- enrichData %>%
  select(ID, geneID) %>%
  group_by(ID) %>%
  separate_rows(geneID, sep = "/") %>%
  rename("Metabolites_CID" = "geneID") %>%
  merge(metaboDiff, by = "Metabolites_CID") %>%
  group_by(ID, sig) %>%
  summarise(Diff.counts = n()) %>%
  pivot_wider(names_from = sig, values_from = Diff.counts, values_fill = 0) %>% 
  rename("Diff.downcounts" = "down", "Diff.upcounts" = "up")

# Merge the Diff.upcounts and Diff.downcounts columns
dat <- left_join(enrichData, diff_counts, by = "ID") %>% 
  rename("pathway"="ID") %>% 
  write_delim("outputfile/enrichData.txt")