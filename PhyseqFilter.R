d_bug_level = 0

library(R6)
library(sqldf)
library(phyloseq)

#.libPaths(new=c('/home/balter/conda/lib/R/library', '/home/balter/R'))

# d_bug("in physeqfilter")

d_bug = function(text, level=1)
{
  if (level <= d_bug_level)
  {
    d_bug(d_bug_level)
    d_bug(level < d_bug_level)
    d_bug(text)
  }
}

PhyseqFilter = R6Class("PhyseqFilter",
  portable = FALSE,
  lock_objects = FALSE,
  private = list(
    use_phyloseq = NULL,
    use_tables = NULL,
    have_phylo = NULL,
    have_all_table = NULL,
    ps_internal = NULL,

    createTablesFromPhyloseq = function(ps)
    {
      d_bug("in create tables from phyloseq",1)
      d_bug(ps, 2)
      d_bug(ps@otu_table, 3)
      
      asv_table = as.data.frame(t(ps@otu_table))
      d_bug("dim asv table", 2)
      d_bug(dim(asv_table), 2)
      taxa_table = as.data.frame(ps@tax_table)
      d_bug("dim taxa_table")
      d_bug(dim(taxa_table), 2)
      study_metadata = as.data.frame(ps@sam_data)
      d_bug("dim study_metadata", 2)
      d_bug(dim(study_metadata), 2)
      sampleIDs = colnames(asv_table)
      d_bug("sampleIDs", 2)
      d_bug(sampleIDs, 2)
      study_metadata$SampleID = unlist(sampleIDs)
      d_bug(dim(study_metadata), 2)
      
      self$ASV_abundance_table = asv_table
      self$ASV_taxa_table = taxa_table
      self$study_metadata = study_metadata
      self$sampleIDs = sampleIDs
      d_bug(self$sampleIDs, 2)
      
      d_bug(rownames(asv_table), 3)
      d_bug(rownames(taxa_table), 3)

      self$taxa_abundance_table = sqldf(row.names=T, "
select * 
from asv_table inner join taxa_table 
using(row_names)
      ")
      
      d_bug("done creating tables", 1)

    },

    createPhyloseqFromTables = function(
      asv_abundance_table=NULL,
      asv_taxa_table=NULL,
      study_metadata=NULL,
      sample_name_column=""
    )
    {
      d_bug("in create phyloseq from tables", 1)
      d_bug(colnames(asv_abundance_table), 2)
      d_bug(colnames(asv_taxa_table), 2)

      d_bug(dim(asv_abundance_table), 2)
      d_bug(dim(asv_taxa_table), 2)
      
      if (sample_name_column != "")
      {
        row.names(study_metadata) = study_metadata[, sample_name_column]
      }else
      {
        row.names(study_metadata) = colnames(asv_abundance_table)
      }
      
      private$ps_internal = phyloseq(
        otu_table(t(as.matrix(asv_abundance_table)), taxa_are_rows=F),
        tax_table(as.matrix(asv_taxa_table)),
        sample_data(study_metadata)
      )
      
      d_bug(private$ps_internal, 2)
    }
    
  ),

  public = list(
    taxa_abundance_table=NULL,
    sampleIDs=NULL,
    
    initialize = function(
      phyloseq_object=NULL,
      sequence_taxa_table=NULL, # Rows are sequences. Cols are taxa ranks. Values are taxa names.
      study_metadata=NULL, # Rows are samples. Cols are variables. Values are data.
      sequence_abundance_table=NULL, # Rows are sequences of interest (e.g. ASV). Cols are sample IDS. Values are counts.
      sample_name_column=""
    )
    {
      d_bug("initialize", 1)
      self$phyloseq_object = phyloseq_object
      private$ps_internal = phyloseq_object
      self$ASV_taxa_table = sequence_taxa_table
      self$ASV_abundance_table = sequence_abundance_table
      self$sample_metadata = study_metadata
      self$sample_name_column = sample_name_column
  
      all_tables = list(
        asv_table=self$ASV_abundance_table,
        study_metadata=self$study_metadata,
        tax_table=self$ASV_taxa_table
      )
  
      private$have_all_tables = !all(is.null(all_tables))
      private$have_phylo = !is.null(phyloseq_object)
  
      if (private$have_phylo)
      {
        d_bug("use phyloseq", 1)
        private$use_phyloseq = TRUE
        private$use_tables = FALSE
        private$has_phylo = TRUE
      }else
      {
        d_bug("use tables", 1)
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
        # d_bug(ps)
        d_bug("creating tables from phyloseq", 1)
        private$createTablesFromPhyloseq(self$phyloseq_object)
        private$ps_internal = self$phyloseq_object
      }
      
      if (private$use_tables)
      {
        private$createPhyloseqFromTables(
          asv_abundance_table=self$ASV_abundance_table,
          asv_taxa_table=self$ASV_taxa_table,
          study_metadata=self$study_metadata
        )
      }
      
      # d_bug("done initialize")

    },

    filter = function(
      taxa_query = "", ### Phylum in (Bacteroidetes,Fermicutes) and Genus in (Prevotella_9,Bacteroides,Sellimonas,Anaerostipes,Barnesiella)
      variable_value_query="", ### e.g.: CaseString = AMD and Age_Rounded > 70 and Gender = M
      # additional_columns = "" ### not implemented yet, but to include taxa columns in results
      sample_query = "" 
    )
    {
      d_bug("in filter function", 1)
      d_bug(paste("taxa_query", taxa_query))
      d_bug(paste("variable_value_query", variable_value_query))
      d_bug(paste("sample_query", sample_query))
      
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
      d_bug(str(study_metadata), 3)
      d_bug(study_metadata[,'SampleID'])
      d_bug(rownames(study_metadata))
      
      
      if(sample_query != "")
      {
        d_bug("has sample query", 1)
        d_bug(sample_query)
        d_bug("sampleIDs = data.frame(SampleID=self$sampleIDs)",3)
        sampleIDs = data.frame(SampleID=self$sampleIDs)
        d_bug("sampleIDs")
        d_bug(sampleIDs)
        filtered_samples_query = sprintf("select * from sampleIDs where %s", sample_query)
        d_bug(sqldf("select * from sampleIDs where SampleID != 'hello'"))
        d_bug("filtered_sample_query")
        d_bug(filtered_samples_query)
        d_bug("running sample query")
        filtered_sampleIDs = sqldf(filtered_samples_query)
        d_bug(filtered_sampleIDs,2)
        d_bug("subsetting study_metadata")
        sampleID_list = filtered_sampleIDs[['SampleID']]
        d_bug(class(sampleID_list))
        filtered_study_metadata = study_metadata[sampleID_list,]
        d_bug("setting self$sampleIDs", 2)
        self$sampleIDs = sampleID_list
        d_bug("setting self$ASV_abundance_table")
        self$ASV_abundance_table = self$ASV_abundance_table[,sampleID_list]
        self$ASV_tax_table = self$ASV_tax_table[,c("Taxa", sampleID_list)]
      }
      
      
      if ( variable_value_query == "")
      {
        d_bug("no variable query", 1)
        filtered_study_metadata_query = paste0("select * from study_metadata")
      } else
      {
        d_bug("variable query", 1)
        filtered_study_metadata_query = paste0("select * from study_metadata where ", variable_value_query)
      }

      d_bug(filtered_study_metadata_query, 1)
      
      d_bug("running variable query")
      filtered_study_metadata = sqldf(filtered_study_metadata_query)
      # d_bug(str(filtered_study_metadata))
      
      # d_bug("dim filtered study metadata")
      # d_bug(dim(filtered_study_metadata))
      
      # d_bug(filtered_study_metadata$SampleID)
      filtered_sampleIDs = filtered_study_metadata$SampleID
      self$sampleIDs = filtered_sampleIDs
      
      filtered_sample_ID_string = paste0(paste(filtered_sampleIDs, collapse=",\n  ") )
      
      ASV_abundance_table = data.frame(self$ASV_abundance_table)
      ASV_tax_table = data.frame(self$ASV_tax_table)
      taxa_abundance_table = data.frame(self$taxa_abundance_table)
      
      if (taxa_query == "")
      {
        d_bug("taxa query")
        filtered_taxa_abundance_table_query = "
select
  row_names, %s,
  Kingdom, Phylum, Class, [Order], Family, Genus
from 
  taxa_abundance_table
"
        filtered_taxa_abundance_table_query = sprintf(
          filtered_taxa_abundance_table_query, 
          filtered_sample_ID_string
        )
      }else
      {
      
        filtered_taxa_abundance_table_query = "
select
  row_names, %s,
  Kingdom, Phylum, Class, [Order], Family, Genus
from 
  taxa_abundance_table
where
  %s
"
        filtered_taxa_abundance_table_query = sprintf(
          filtered_taxa_abundance_table_query, 
          filtered_sample_ID_string,
          taxa_query
        )
        
        self$ps_internal = self
      }


      # d_bug("filtered_taxa_abundance_table_query")
      # cat(filtered_taxa_abundance_table_query)
      
      # d_bug("querying for filterd tax abundance table")
      d_bug("running taxa query")
      filtered_taxa_abundance_table=sqldf(
        filtered_taxa_abundance_table_query, 
        row.names=T
        )
      self$taxa_abundance_table = filtered_taxa_abundance_table
      # d_bug(str(filtered_taxa_abundance_table))
      
      
      # d_bug(filtered_sampleIDs)

      # d_bug("extracting asv table")
      filtered_ASV_abundance_table = filtered_taxa_abundance_table[, filtered_sampleIDs]
      # d_bug("extracting taxa table")
      filtered_ASV_taxa_table = filtered_taxa_abundance_table[, c('Kingdom', 'Phylum', 'Class', "Order", 'Family', 'Genus')]
      
      #d_bug("rownames filtered asv abundance table")
      #d_bug(rownames(filtered_ASV_abundance_table)[1:3])
      
      self$ASV_abundance_table = filtered_ASV_abundance_table
      self$ASV_taxa_table = filtered_ASV_taxa_table
      self$study_metadata = filtered_study_metadata
      self$sampleIDs = filtered_sampleIDs

      
      # str(filtered_ASV_abundance_table)
      # str(filtered_ASV_taxa_table)
      
      # # # d_bug('creating ps from tables')
      # # d_bug(dim(filtered_ASV_taxa_table))
      # d_bug(dim(filtered_ASV_abundance_table))
      
      private$createPhyloseqFromTables(
        asv_abundance_table=filtered_ASV_abundance_table,
        asv_taxa_table=filtered_ASV_taxa_table,
        study_metadata=filtered_study_metadata
      )
      
      return(self)
      
      d_bug("done filter", 1)
    },
    
    getPS = function()
    {
      d_bug("in getPS", 1)
      return(private$ps_internal)
    },
    
    getTables = function()
    {
      d_bug("in getTables", 1)
      physeq_filter_tables = c(
        ASV_abundance_table=self$ASV_abundance_table,
        ASV_taxa_table=self$ASV_taxa_table,
        study_metadata=self$study_metadata,
        sampleIDs=self$sampleIDs
      )
      
      return(physeq_filter_tables)
    },
    
    getTaxaAbundanceTable = function(normalize=T)
    {
      d_bug("in getTaxaAbundanceTable", 1)
      temp = self$taxa_abundance_table

      if (normalize)
      {
        temp[self$sampleIDs] = apply(temp[self$sampleIDs], 2, function(col){col/sum(col)})
      }
      return(temp)
    },
    
    getGlommedTaxaNamesTable = function(glom_query="Phylum||'_'||Genus as Taxa")
    {
      
      d_bug("in getGlommedTaxaNamesTable", 1)
      taxa_abundance_table = self$getTaxaAbundanceTable(normalize=T)
      sampleIDs = self$sampleIDs
      
      ### Vectorized paste to get:
      ### s001 --> sum(s001), s002 --> sum(s002), etc.
      sample_id_string = paste0(paste0("sum(", sampleIDs, ") as ", sampleIDs, collapse=",\n  ") )
      
      SQL = "
select
  %s,
  %s
from
  taxa_abundance_table
group by Taxa
  "
      
      SQL = sprintf(SQL, glom_query, sample_id_string)
      
      # cat(SQL)
      
      temp = sqldf(row.names=T, SQL)
      # d_bug(str(temp))
      
      temp[sampleIDs] = apply(temp[sampleIDs], 2, function(col){col/sum(col)})
      # d_bug(head(temp))
      
      # d_bug(colSums(temp[sampleIDs]))
      
      taxa_abundance_table = temp
      
      # d_bug("done with filter")
    }

  )
                
)

# library(xlsx)
# 
# writeTaxGlomTable = function(
#   ps,
#   filename="",
#   sheetname=""
# )
# {
#   asv_table = as.data.frame(ps@otu_table)
#   d_bug("dim asv table")
#   d_bug(dim(asv_table))
#   taxa_table = as.data.frame(ps@tax_table)
#   d_bug(dim(taxa_table))
#   study_metadata = as.data.frame(ps@sam_data)
#   d_bug("dim study metadata")
#   d_bug(dim(study_metadata))
#   sampleIDs = colnames(asv_table)
#   d_bug(sampleIDs)
#   study_metadata$SampleID = unlist(sampleIDs)
# 
# 
#   sample_id_string = paste0(paste0("sum(", sampleIDs, ") as ", sampleIDs, collapse=",\n  ") )
#   # sample_id_string = paste0(paste0(sampleIDs,collapse=",\n  ") )
# 
#   SQL = "
# select
#   Phylum||'_'||Genus as Taxa,
#   %s
# from
#   asv_table inner join taxa_table
# on
#   taxa_table.row_names == asv_table.row_names
# group by Taxa
#   "
# 
#   SQL = sprintf(SQL, sample_id_string)
# 
#   cat(SQL)
# 
#   temp = sqldf(row.names=T, SQL)
#   d_bug(str(temp))
# 
#   temp[sampleIDs] = apply(temp[sampleIDs], 2, function(col){col/sum(col)})
#   d_bug(head(temp))
# 
#   d_bug(colSums(temp[sampleIDs]))
# 
#   taxa_abundance_table = temp
# 
  # write.xlsx2(
  #   taxa_abundance_table,
  #   file=filename,
  #   sheetName=sheetname,
  #   append=T,
  #   col.names=T,
  #   row.names=T
  # )
# }
