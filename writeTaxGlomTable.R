library(xlsx)

writeTaxGlomTable = function(
  ps,
  filename="",
  sheetname=""
)
{
  asv_table = as.data.frame(ps@otu_table)
  print("dim asv table")
  print(dim(asv_table))
  taxa_table = as.data.frame(ps@tax_table)
  print(dim(taxa_table))
  study_metadata = as.data.frame(ps@sam_data)
  print("dim study metadata")
  print(dim(study_metadata))
  sampleIDs = colnames(asv_table)
  print(sampleIDs)
  study_metadata$SampleID = unlist(sampleIDs)
  
  
  sample_id_string = paste0(paste0("sum(", sampleIDs, ") as ", sampleIDs, collapse=",\n  ") )
  # sample_id_string = paste0(paste0(sampleIDs,collapse=",\n  ") )
  
  SQL = "
select
  Phylum||'_'||Genus as Taxa,
  %s
from
  asv_table inner join taxa_table
on
  taxa_table.row_names == asv_table.row_names
group by Taxa
  "
  
  SQL = sprintf(SQL, sample_id_string)
  
  cat(SQL)
  
  temp = sqldf(row.names=T, SQL)
  print(str(temp))
  
  temp[sampleIDs] = apply(temp[sampleIDs], 2, function(col){col/sum(col)})
  print(head(temp))
  
  print(colSums(temp[sampleIDs]))
  
  taxa_abundance_table = temp
  
  write.xlsx2(
    taxa_abundance_table,
    file=filename,
    sheetName=sheetname,
    append=T,
    col.names=T,
    row.names=T
  )
}
