library(R6)
library(sqldf)

.libPaths(new=c('/home/balter/conda/lib/R/library', '/home/balter/R'))

HyperPhyseq = R6Class("HyperPhyseq",
                      
                      
  private = list(
    use_phyloseq=NULL,
    use_tables=NULL
  ),
                      
  Initialize = function(
    phyloseq_object=NULL,
    sequence_taxa_table=NULL, # Rows are sequences. Cols are taxa ranks. Values are taxa names.
    sample_metadata=NULL, # Rows are samples. Cols are variables. Values are data.
    sequence_abundance_table=NULL # Rows are sequences of interest (e.g. ASV). Cols are sample IDS. Values are counts.
    )
{
    self$phyloseq_object = phyloseq_object
    self$sequence_taxa_table = sequence_taxa_table
    self$sequence_abundance_table = sequence_abundance_table
    self$sample_metadata = sample_metadata
    
    all_tables = list(
      sequence_data_table=sequence_data_table,
      sample_metadata=sample_metadata,
      sequence_abundance_table=sequence_abundance_table
      )
    
    if (!is.null(phyloseq_object))
    {
      private$use_phyloseq = TRUE
      private$use_tables = FALSE
    }else
    {
      private$use_phyloseq = FALSE
      private$use_tables = TRUE
    }
    
    if (!is.null(phyloseq_obejct) && !all(is.null(all_tables)))
    {
      private$use_phyloseq = TRUE
      message("WARNING: Tables given in addition to Phyloseq object.")
      message("         Phyloseq object will override given tables.")
    }
  
    if (is.null(phyloseq_object) && any(is.null(all_tables)))
    {
      message("ERROR: You must provide ALL required tables.")
      stop()
    }
    
    if (use_phyloseq)
    {
      private$createTablesFromPhyloseq()
    }
    
  }
)
               
# 
#   public = list(
createTablesFromPhyloseq = function()
{
  self$sequence_abundance_table = ps@otu_table
  self$sequence_taxa_table = ps@taxonomy_table
  self$sample_metadata = ps@sam_data
  
  asv_tab = self$sequence_abundance_table
  tax_tab = self$tsequence_taxa_table
  
  self.tax_abundance_table = sqldf("
select *
from asv_tab inner join tax_tab
on asv_tab.row_names == tax_tab.row_names
  ")
  
  ### delete asv_tab, tax_tab
}






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