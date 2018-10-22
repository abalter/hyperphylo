library(R6)
library(sqldf)
library(phyloseq)

.libPaths(new=c('/home/balter/conda/lib/R/library', '/home/balter/R'))

HyperPhyseq = R6Class("HyperPhyseq",
                      
  private = list(
    use_phyloseq=NULL,
    use_tables=NULL,
    ps_internal=NULL,
    
    createTablesFromPhyloseq = function()
    {
      self$sequence_abundance_table = ps@otu_table
      self$sequence_taxa_table = ps@taxonomy_table
      self$sstudy_metadata = ps@sam_data
      
      asv_tab = self$sequence_abundance_table
      tax_tab = self$tsequence_taxa_table
      
      SQL = "
select*
from asv_tab inner join tax_tab
on asv_tab.row_names == tax_tab.row_names
"
      self.tax_abundance_table = sqldf(row.names=T, SQL)
      
      rm(c(asv_tab, tax_tab))
      
    },
    
    createPhyloseqFromTables = function()
    {
      private$ps_internal = ps(
        otu_table(self$sequence_abundance_table),
        tax_table(self$sequence_taxa_table),
        sample_data(self$study_metadata)
      )
    }
    
    
  ),
                      
  Initialize = function(
    phyloseq_object=NULL,
    sequence_taxa_table=NULL, # Rows are sequences. Cols are taxa ranks. Values are taxa names.
    sample_metadata=NULL, # Rows are samples. Cols are variables. Values are data.
    sequence_abundance_table=NULL # Rows are sequences of interest (e.g. ASV). Cols are sample IDS. Values are counts.
    )
  {
    #self$phyloseq_object = phyloseq_object
    private$ps_internal = phyloseq_object
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
  
  
  public = list(
    filter = function(
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
      
      SQL = "
select
  %s,
  Kingdom, Phylum, Class, 'Order', Family, Genus
from
  tax_abundance_table
where
  %s
"
      SQL = sprintf(SQL, sample_list, taxa_query)
      #cat(SQL)
      result = sqldf(SQL, row.names=T)
      
      return(result)
      
    }
    
  )
  
  
)
              

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
