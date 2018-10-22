library(phyloseq)

N_ASVs = 12
N_samples = 7
N_variables = 2

sample_names = paste0("Sample", 1:N_samples)
sample_names

asv_table = matrix(sample(1:100, N_ASVs*N_samples, replace = TRUE), nrow = N_ASVs, ncol = N_samples)
rownames(asv_table) <- paste0("ASV", 1:nrow(asv_table))
colnames(asv_table) <- sample_names
asv_table


taxonomy_table = matrix(sample(letters, N_ASVs*7, replace = TRUE), nrow = N_ASVs, ncol = 7)
rownames(taxonomy_table) <- rownames(asv_table)
colnames(taxonomy_table) <- c("Domain", "Phylum", "Class", "Order", "Family", "Genus", "Species")
taxonomy_table

study_metadata = data.frame(
  SampleID=sample_names,
  var1=sample(sample(letters, N_samples, replace = TRUE)),
  var2=sample(1:100, N_samples, replace = TRUE)
  )
# study_metadata = as.matrix(study_metadata)
study_metadata

ps = phyloseq(
  otu_table(asv_table, taxa_are_rows=T),
  tax_table(taxonomy_table)
)
ps@sam_data = sample_data(study_metadata)
ps

# ps2 = phyloseq(
#   otu_table(asv_table, taxa_are_rows=T),
#   tax_table(taxonomy_table),
#   sample_data(study_metadata)
# )
# ps2

filter1 = PhyseqFilter$new(
  phyloseq_object=ps
)
