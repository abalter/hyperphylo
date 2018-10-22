
.libPaths(new=c('/home/balter/conda/envs/dada2/lib/R/library', '/home/balter/R'))
library(sqldf)


asv_table = read.table('small_asv_table.tsv', header=T, sep='\t')
tax_table = read.table('small_tax_table.tsv', header=T, sep='\t')
study_metadata = read.table('map_091917.tsv', header=T, sep='\t')

age_rounded_numeric = sapply(study_metadata['Age_Rounded'], as.numeric)
study_metadata['Age_Rounded'] = as.factor(age_rounded_numeric)

CaseString = list(ifelse(study_metadata['Case']==0, "AMD", "Control"))
study_metadata['CaseString'] = as.list(CaseString)
sample_names = levels(study_metadata[,'SampleID'])


tax_abundance_table = sqldf(row.names=T, "
  select 
    * 
  from 
    asv_table inner join tax_table 
  on 
    asv_table.row_names == tax_table.row_names
  ")
tax_abundance_table[,sample_names] = lapply(tax_abundance_table[,sample_names], function(col){col/sum(col)})

self = list(
  asv_table=asv_table,
  tax_table=tax_table,
  study_metadata=study_metadata,
  tax_abundance_table=tax_abundance_table
)



filterPhyloseq = function(
  
  taxa_query = "", ### Phylum in (Bacteroidetes,Fermicutes) and Genus in (Prevotella_9,Bacteroides,Sellimonas,Anaerostipes,Barnesiella)
  variable_value_query="", ### e.g.: CaseString = AMD and Age_Rounded > 70 and Gender = M
  additional_columns = "" ### not implemented yet, but to include taxa columns in results
)
{
  ### --- Dynamically Generate Field Selection --- ###
  ### The study metadata table has samples as rows. The tables with
  ### reads and taxonomy have samples as columns. Thus it is not possible
  ### to join them. 
  ###
  ### SQL does not natively have a way to dynamically create a field list
  ### for a select statement. In order to choose which samples to use as 
  ### fields in the select statement (for instance samples only for control), 
  ### we need to generate that first and munge it into a string to use in the
  ### SQL query.
  
  ### Query the SampleIDs to use based on the variable requirements
  samples = sqldf(paste0("select SampleID from study_metadata where ", variable_value_query))
  
  if (length(samples$SampleID) == 0)
  {
    sample_list = "*"
  } else
  {
    sample_list = paste0(paste(samples$SampleID, collapse=",\n  ") )
  }
  
  SQL = sprintf("
select
  %s,
  Kingdom, Phylum, Class, 'Order', Family, Genus
from
  tax_abundance_table
where
  %s
  ",
    sample_list,
    taxa_query
  )
  
  # SQL = "select Kingdom from tax_abundance_table"

  cat(SQL)
  result = sqldf(SQL, row.names=T)

  return(result)
  
}


f = filterPhyloseq(
  taxa_query="Phylum in ('Bacteroidetes','Firmicutes') and Genus in ('Prevotella_9,Bacteroides','Blautia','Faecalibacterium','Subdoligranulum')
  ",
  variable_value_query="CaseString=='AMD' and Age_Rounded>70"
)

print(f)


# tax_tab.Phylum in (Bacteroidetes,Fermicutes)
# #   and
# #   tax_tab.Genus in (Prevotella_9,Bacteroides,Sellimonas,Anaerostipes,Barnesiella)
# 
# 
# 
# 
# 
# 
# getTaxaDistributions = function(
#   taxa_list = c(), # named list in format e.g. c(Phyla=c("P1", "P2"), Genera=c("G1", "G2"))
#   sample_names=c(), # list of sample names
#   variables=c(),
#   num_bins=0,
#   bins=c(),
# )
# {
#   
#   
#   
#   
# }
# 
# getAlphaDiversities = function(
#   ps,
#   sample_names=c(),
#   methods="",
#   taxa_list = c(), # named list in format e.g. c(Phyla=c("P1", "P2"), Genera=c("G1", "G2"))
#   sample_names=c(), # list of sample names
#   abundance_table="", # counts by unique row such as sample or taxa
# )
# {
#   
# }
# 
# filter = function(
#   ps,
#   taxa_list = c(), # named list in format e.g. c(Phylum=c("P1", "P2"), Genus=c("G1", "G2"))
#   methods=c(), # prevalence_percent, abundance, relative_abundance, sample_IDs
#   sampleIDs=c(),
#   variables=c(),
#   min_prev=0,
#   max_prev=Inf,
#   avg_prev=0,
#   min_rel_prev=0,
#   max_rel_prev=Inf,
#   avg_rel_prev=0,
#   min_abund=0,
#   max_abund=Inf,
#   avg_abund=0,
#   min_rel_abund=0,
#   max_rel_abund=Inf,
#   avg_rel_abund=0
# )
# {
#   requested_fliters = c()
#   
#   If (length(taxa_list) > 0)
#   {
#     requested_filters = c(requested_filters, "taxa_list")
#     #create function call...
#   }
#   
#   if (length(methods) > 0)
#   {
#     requested_filters = c(requested_filters, "taxa_list")
#     #create function call...
#   }
#   
#   if (length(sampleIDs) > 0)
#   {
#     requested_filters = c(requested_filters, "taxa_list")
#     #create function call...
#   }
#   
#   if (length(variables) > 0)
#   {
#     requested_filters = c(requested_filters, "taxa_list")
#     #create function call...
#   }
#   
#   
# }
# 
# #     
# #     
# #   ),
# #   
# #   Private = list(
# #     
# #   )
# # )
