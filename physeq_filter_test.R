.libPaths(new=c('/home/balter/conda/envs/dada2/lib/R/library', '/home/balter/R'))

library(phyloseq)
source('PhyseqFilter.R')

load('test_phyo.RData')

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

