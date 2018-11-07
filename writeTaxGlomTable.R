library(xlsx)

writeTaxGlomTable = function(
  physeq_filter,
  filename="",
  sheetname=""
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
  
  # cat(SQL)
  
  temp = sqldf(row.names=T, SQL)
  # print(str(temp))
  
  temp[sampleIDs] = apply(temp[sampleIDs], 2, function(col){col/sum(col)})
  # print(head(temp))
  
  # print(colSums(temp[sampleIDs]))
  
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
