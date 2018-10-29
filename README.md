# hyperphylo -- WIP

Building a tool to handle phyloseq objects in a more declarative way. For instance:

```
    filter = function(
      taxa_query = "", ### Phylum in (Bacteroidetes,Fermicutes) and Genus in (Prevotella_9,Bacteroides,Sellimonas,Anaerostipes,Barnesiella)
      variable_value_query="" ### e.g.: CaseString = AMD and Age_Rounded > 70 and Gender = M
      # additional_columns = "" ### not implemented yet, but to include taxa columns in results
    )
```

Working for my purposes now. More later...
