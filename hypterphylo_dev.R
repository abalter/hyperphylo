
.libPaths(new=c('/home/balter/conda/envs/dada2/lib/R/library', '/home/balter/R'))
library(sqldf)
library(tidyverse)


asv_table = read.table('small_asv_table.tsv', header=T, sep='\t')
tax_table = read.table('small_tax_table.tsv', header=T, sep='\t')
sample_metadata = read.table('map_091917.tsv', header=T, sep='\t')

age_rounded_numberic = sapply(sample_metadata['Age_Rounded'], as.numeric)
sample_metadata['Age_Rounded'] = as.factor(age_rounded_numberic)

CaseString = ifelse(sample_metadata['Case']==0, "AMD", "Control")
sample_metadata['CaseString'] = CaseString


tax_abundance_table = sqldf(row.names=T, "
select 
  * 
from 
  asv_table inner join tax_table 
  on 
  asv_table.row_names == tax_table.row_names
")

self = list(
  asv_table=asv_table,
  tax_table=tax_table,
  sample_metadata=sample_metadata,
  tax_abundance_table=tax_abundance_table
)



filterData = function(
  
  taxa_query = "", 
  ### e.g. 
  ### Phylum in (Bacteroidetes,Fermicutes) 
  ### and 
  ### Genus in (Prevotella_9,Bacteroides,Sellimonas,Anaerostipes,Barnesiella)
  variable_value_query="" ### e.g.: CaseString = AMD and Age_Rounded > 70 and Gender = M
)
{
  SQL = paste0("select SampleID from sample_metadata where ", variable_value_query)
  cat(SQL, '\n')
  samples = sqldf(paste0("select SampleID from sample_metadata where ", variable_value_query))
  print("samples")
  print(samples)
  print(samples$SamplID)
  cat('length samples$SampleID', length(samples$SampleID), '\n')
  

  if (length(samples$SampleID) == 0)
  {
    sample_list = "*"
  } else
  {
    sample_list = paste0(paste(samples$SampleID, collapse=",\n  ") )
  }
  
  print("sample_list")
  print(sample_list)
  
  SQL = sprintf("
select
  %s
from
  tax_abundance_table
where
  %s
",
    sample_list,
    taxa_query
  )
  
  cat(SQL)
  
  result = sqldf(SQL)
  
  return(result)
  
}


filterData(
  taxa_query="Phylum in ('Bacteroidetes','Firmicutes') and Genus in ('Prevotella_9,Bacteroides','Blautia','Faecalibacterium','Subdoligranulum')
",
  variable_value_query="CaseString=='AMD' and Age_Rounded>70"
)



tax_tab.Phylum in (Bacteroidetes,Fermicutes)
#   and
#   tax_tab.Genus in (Prevotella_9,Bacteroides,Sellimonas,Anaerostipes,Barnesiella)






getTaxaDistributions = function(
  taxa_list = c(), # named list in format e.g. c(Phyla=c("P1", "P2"), Genera=c("G1", "G2"))
  sample_names=c(), # list of sample names
  variables=c(),
  num_bins=0,
  bins=c(),
)
{
  

  
  
}

getAlphaDiversities = function(
  ps,
  sample_names=c(),
  methods="",
  taxa_list = c(), # named list in format e.g. c(Phyla=c("P1", "P2"), Genera=c("G1", "G2"))
  sample_names=c(), # list of sample names
  abundance_table="", # counts by unique row such as sample or taxa
)
{
  
}

filter = function(
  ps,
  taxa_list = c(), # named list in format e.g. c(Phylum=c("P1", "P2"), Genus=c("G1", "G2"))
  methods=c(), # prevalence_percent, abundance, relative_abundance, sample_IDs
  sampleIDs=c(),
  variables=c(),
  min_prev=0,
  max_prev=Inf,
  avg_prev=0,
  min_rel_prev=0,
  max_rel_prev=Inf,
  avg_rel_prev=0,
  min_abund=0,
  max_abund=Inf,
  avg_abund=0,
  min_rel_abund=0,
  max_rel_abund=Inf,
  avg_rel_abund=0
)
{
  requested_fliters = c()
  
  If (length(taxa_list) > 0)
  {
    requested_filters = c(requested_filters, "taxa_list")
    #create function call...
  }
  
  if (length(methods) > 0)
  {
    requested_filters = c(requested_filters, "taxa_list")
    #create function call...
  }
  
  if (length(sampleIDs) > 0)
  {
    requested_filters = c(requested_filters, "taxa_list")
    #create function call...
  }
  
  if (length(variables) > 0)
  {
    requested_filters = c(requested_filters, "taxa_list")
    #create function call...
  }
  
  
}

#     
#     
#   ),
#   
#   Private = list(
#     
#   )
# )