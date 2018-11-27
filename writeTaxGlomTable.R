library(openxlsx)

writeTaxGlomTable = function(
  physeq_filter,
  filename="",
  sheetname="Sheet1",
  save_file=T
)
{
  taxa_abundance_table = physeq_filter$taxa_abundance_table
  sampleIDs = physeq_filter$sampleIDs
  
  ### Vectorized paste to get:
  ### s001 --> sum(s001), s002 --> sum(s002), etc.
  sample_id_string = paste0(paste0("sum(", sampleIDs, ") as ", sampleIDs, collapse=",\n  ") )

  SQL = "
select
  Phylum||'_'||Genus as Taxa,
  %s
from
  taxa_abundance_table
group by Taxa
  "
  
  SQL = sprintf(SQL, sample_id_string)
  
  cat(SQL)
  
  temp = sqldf(row.names=T, SQL)

  print("normalizing")
  temp[sampleIDs] = apply(temp[sampleIDs], 2, function(col){col/sum(col)})
  print("fixing na")
  temp[,"Taxa"][!is.na(temp[,'Taxa'])] = 'NA'
  # print("assigining rownames")
  # rownames(temp) = temp[,"Taxa"]
  # print("removing taxa column")
  # temp["Taxa"] = NULL
  
  # print(head(temp))
  
  # print(colSums(temp[sampleIDs]))
  
  print("assigning taxa_abundance_table")
  taxa_abundance_table = temp
  print(rownames(taxa_abundance_table))
  print(taxa_abundance_table['Taxa'])
  
  gc()
  gc()
  if (save_file)
  {
    write.xlsx(
      taxa_abundance_table,
      file=filename,
      sheetName=sheetname,
      append=T,
      col.names=T,
      row.names=T
    )
  }
  
  return(taxa_abundance_table)
}
