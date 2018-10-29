.libPaths(new=c('/home/balter/conda/envs/dada2/lib/R/library', '/home/balter/R'))

setwd('/home/balter/hyperphylo')

library(phyloseq)
source('PhyseqFilter.R')

load('test_phyo.RData')

asv_table = as.data.frame(ps@otu_table)
ps@otu_table = otu_table(t(asv_table), taxa_are_rows=T)

print("creating psfilteSr1")


filter1 = PhyseqFilter$new(
  phyloseq_object=ps
)$
  filter(
    taxa_query="Phylum in ('Bacteroidetes','Fermicutes') and Genus in ('Prevotella_9','Bacteroides','Sellimonas','Anaerostipes','Barnesiella')",
    variable_value_query="CaseString = 'AMD'"
  )
  
filter1_ps = filter1$getPS()

filter1_tab = filter1$getTaxaAbundanceTable()

print(dim(filter1_tab))

writeTaxGlomTable(filter1_ps, filename="glomtst.xlsx", sheetname="testsheet")
