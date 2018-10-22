library(phyloseq)
load('~/MiSeq239/results/miseq_239_phyloseq-10-14-2018.RData')

asv_table = as.data.frame(t(ps@otu_table))
taxa_table = as.data.frame(ps@tax_table)
study_metadata = as.data.frame(ps@sam_data)
study_metadata$SampleID = colnames(asv_table)

someints = sample.int(134,20)

small_study_metadata = study_metadata[someints,]
small_asv_table = asv_table[, someints]

asvs_with_nonzero_counts = ( rowSums(small_asv_table) != 0 )

small_asv_table = small_asv_table[asvs_with_nonzero_counts,]
small_taxa_table = taxa_table[asvs_with_nonzero_counts,]

save(small_asv_table, small_taxa_table, small_study_metadata, file="small_tables.RData")

age_rounded_numeric = sapply(small_study_metadata$Age_Rounded, as.numeric)
small_study_metadata['Age_Rounded'] = as.factor(age_rounded_numeric)

CaseString = ifelse(small_study_metadata$Case == 0, "AMD", "Control")
small_study_metadata$CaseString = CaseString
small_study_metadata$SampleID = colnames(small_asv_table)
sample_names = levels(small_study_metadata$SampleID)

small_study_metadata = small_study_metadata[,sort(colnames(small_study_metadata))]

ps = phyloseq(
  otu_table(as.matrix(small_asv_table), taxa_are_rows=T),
  tax_table(as.matrix(small_taxa_table))
)
ps@sam_data = sample_data(small_study_metadata)

ps

save(ps, file="test_phyo.RData")

tax_abundance_table = sqldf(row.names=T,
"
select
  *
from
  small_taxa_table inner join small_asv_table
on
  small_taxa_table.row_names == small_asv_table.row_names
"
)
