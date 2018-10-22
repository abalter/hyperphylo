library(R6)
library(sqldf)
library(phyloseq)

#.libPaths(new=c('/home/balter/conda/lib/R/library', '/home/balter/R'))

PhyseqFilter = R6Class("PhyseqFilter",
  portable = FALSE,
  lock_objects = FALSE,
  private = list(
    use_phyloseq = NULL,
    use_tables = NULL,
    have_phylo = NULL,
    have_all_table = NULL,
    ps_internal = NULL,

    createTablesFromPhyloseq = function()
    {
      # print("create tables from phyloseq")
      
      asv_table = as.data.frame(ps@otu_table)
      # print("dim asv table")
      # print(dim(asv_table))
      taxa_table = as.data.frame(ps@tax_table)
      # print(dim(taxa_table))
      study_metadata = as.data.frame(ps@sam_data)
      # print(dim(study_metadata))
      sampleIDs = colnames(asv_table)
      # print(sampleIDs)
      study_metadata$SampleID = unlist(sampleIDs)
      # print(dim(study_metadata))
      
      self$ASV_abundance_table = asv_table
      self$ASV_taxa_table = taxa_table
      self$study_metadata = study_metadata

      self$tax_abundance_table = sqldf(row.names=T, "
select * 
from asv_table inner join taxa_table 
on asv_table.row_names == taxa_table.row_names
      ")
      
      # print("done creating tables")

    },

    createPhyloseqFromTables = function(
      asv_abundance_table=NULL,
      asv_taxa_table=NULL,
      study_metadata=NULL
    )
    {
      # print("in create phyloseq from tables")
      # print(colnames(asv_abundance_table))
      # print(colnames(asv_taxa_table))
      
      # print(dim(asv_abundance_table))
      # print(dim(asv_taxa_table))
      
      private$ps_internal = phyloseq(
        otu_table(as.matrix(asv_abundance_table), taxa_are_rows=T),
        tax_table(as.matrix(asv_taxa_table))
      )
      
      # private$ps_internal@sam_data = study_metadata
      
      if (!is.null(study_metadata))
      {
        # print("no metadata")
        private$ps_internal@sam_data = sample_data(study_metadata)
      }
      
      #print(private$ps_internal)
    }
    
  ),

  public = list(
    initialize = function(
      phyloseq_object=NULL,
      sequence_taxa_table=NULL, # Rows are sequences. Cols are taxa ranks. Values are taxa names.
      study_metadata=NULL, # Rows are samples. Cols are variables. Values are data.
      sequence_abundance_table=NULL # Rows are sequences of interest (e.g. ASV). Cols are sample IDS. Values are counts.
    )
    {
      self$phyloseq_object = phyloseq_object
      private$ps_internal = phyloseq_object
      self$ASV_taxa_table = sequence_taxa_table
      self$ASV_abundance_table = sequence_abundance_table
      self$sample_metadata = study_metadata
  
      all_tables = list(
        asv_table=self$ASV_abundance_table,
        study_metadata=self$study_metadata,
        tax_table=self$ASV_taxa_table
      )
  
      private$have_all_tables = !all(is.null(all_tables))
      private$have_phylo = !is.null(phyloseq_object)
  
      if (private$have_phylo)
      {
        # print("use phyloseq")
        private$use_phyloseq = TRUE
        private$use_tables = FALSE
        private$has_phylow = TRUE
      }else
      {
        # print("use tables")
        private$use_phyloseq = FALSE
        private$use_tables = TRUE
      }
  
      if ( private$have_phylo && !any(is.null(private$all_tables)) )
      {
        private$use_phyloseq = TRUE
        message("WARNING: Tables given in addition to Phyloseq object.")
        message("         Phyloseq object will override given tables.")
      }
  
      if (!private$have_phylo && !private$have_all_tables)
      {
        message("ERROR: You must provide ALL required tables.")
        stop()
      }
  
      if (private$use_phyloseq)
      {
        # print(ps)
        # print("creating tables from phyloseq")
        private$createTablesFromPhyloseq()
      }

    },

    filter = function(
      taxa_query = "", ### Phylum in (Bacteroidetes,Fermicutes) and Genus in (Prevotella_9,Bacteroides,Sellimonas,Anaerostipes,Barnesiella)
      variable_value_query="" ### e.g.: CaseString = AMD and Age_Rounded > 70 and Gender = M
      # additional_columns = "" ### not implemented yet, but to include taxa columns in results
    )
    {
      # print("filter function")
      
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
      
      study_metadata = data.frame(self$study_metadata)
      # print(str(study_metadata))
      
      if ( variable_value_query == "")
      {
        # print("no variable query")
        filtered_study_metadata_query = paste0("select * from study_metadata")
      } else
      {
        filtered_study_metadata_query = paste0("select * from study_metadata where ", variable_value_query)
      }

      # print(filtered_study_metadata_query)
      
      filtered_study_metadata = sqldf(filtered_study_metadata_query)
      # print(str(filtered_study_metadata))
      # print(filtered_study_metadata$SampleID)
      filtered_sampleIDs = filtered_study_metadata$SampleID
      filtered_sample_ID_string = paste0(paste(filtered_sampleIDs, collapse=",\n  ") )
      
      ASV_abundance_table = data.frame(self$ASV_abundance_table)
      ASV_tax_table = data.frame(self$ASV_tax_table)
      tax_abundance_table = data.frame(self$tax_abundance_table)
      
      filtered_tax_abundance_table_query = "
select
  %s,
  Kingdom, Phylum, Class, 'Order', Family, Genus
from 
  tax_abundance_table
where
  %s
"
      filtered_tax_abundance_table_query = sprintf(
        filtered_tax_abundance_table_query, 
        filtered_sample_ID_string,
        taxa_query
        )

      
      # cat(filtered_tax_abundance_table_query)
      
      # print("querying for filterd tax abundance table")
      filtered_tax_abundance_table=sqldf(filtered_tax_abundance_table_query, row.names=T)
      # print(str(filtered_tax_abundance_table))
      
      
      # print(filtered_sampleIDs)

      # print("extracting asv table")
      filtered_ASV_abundance_table = filtered_tax_abundance_table[, filtered_sampleIDs]
      # print("extracting taxa table")
      filtered_ASV_taxa_table = filtered_tax_abundance_table[, c('Kingdom', 'Phylum', 'Class', "'Order'", 'Family', 'Genus')]
      
      # str(filtered_ASV_abundance_table)
      # str(filtered_ASV_taxa_table)
      
      # # # print('creating ps from tables')
      # # print(dim(filtered_ASV_taxa_table))
      # print(dim(filtered_ASV_abundance_table))
      
      private$createPhyloseqFromTables(
        asv_abundance_table=filtered_ASV_abundance_table,
        asv_taxa_table=filtered_ASV_taxa_table,
        study_metadata=filtered_study_metadata
      )
      
      return(self)
    },
    
    getPS = function()
    {
      # print("in getPS")
      return(private$ps_internal)
    }

  )
                
)

