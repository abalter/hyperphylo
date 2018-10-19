getTaxaDistributions = function(
  taxa_list = c(), # named list in format e.g. c(Phyla=c("P1", "P2"), Genera=c("G1", "G2"))
  sample_names=c(), # list of sample names
  variables=c(),
  num_bins=0,
  bins=c(),
)
{
  asv_tab = self$sequence_abundance_table
  tax_tab = self$tsequence_taxa_table
  result = data.frame()
  
  
  ### Generate a query of the type:
  # select 
  #   samp1,
  #   samp2,
  #   samp3
  # from asv_tab inner join asv_tab
  # on tax_tab.sequence = asv_tab.sequence
  # where
  #   tax_tab.Phylum in (Bacteroidetes,Fermicutes)
  #   and
  #   tax_tab.Genus in (Prevotella_9,Bacteroides,Sellimonas,Anaerostipes,Barnesiella)
  #   and
  #   ...
  
  if (length(taxa_list) > 0)
  {
    ### filter by taxa
    ranks = names(taxa_list)
    taxa_selections = lapply(ranks, function(rank){ 
      paste0(paste0('tax_tab.',rank), " in ", '(', paste(taxa_list[[rank]], collapse=","), ')') ### rank in (r1, r1, r3)
    })
    
    taxa_where_clause = paste0("where\n  ", paste(taxa_selections, collapse="\n  and\n  "))
  }else
  {
    taxa_where_clause = ""
  }
  
  if (length(sample_names) == 0)
  {
    sample_list = "*"
  } else
  {
    sample_list = paste0("\n  ", paste(sample_names, collapse=",\n  ") )
  }
  
  
  SQL = sprintf(
    "select %s
from asv_tab inner join tax_tab
on asv_tab.row_names == tax_tab.row_names
",
    sample_list
  )
  
  if (taxa_where_clause != "")
  {
    SQL = paste0(SQL, "\n", taxa_where_clause)
  }
  
  print("executiong query:")
  cat(SQL)
  
  taxa_abundances = sqldf(SQL)
  
  
}
