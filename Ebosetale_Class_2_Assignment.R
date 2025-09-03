# Differential Gene Expression (DGE) Classification

# 1. Load datasets manually
DEGs_Data_1 <- read.csv(file.choose())   # Pick first dataset
DEGs_Data_2 <- read.csv(file.choose())   # Pick second dataset

# 2. Define classify_gene() function
classify_gene <- function(logFC, padj) {
  if (logFC > 1 & padj < 0.05) {
    return("Upregulated")
  } else if (logFC < -1 & padj < 0.05) {
    return("Downregulated")
  } else {
    return("Not_Significant")
  }
}

# 3. Create Results folder if it doesn’t exist
if (!dir.exists("Results")) {
  dir.create("Results")
}

# 4. Put datasets into a list for looping
datasets <- list(DEGs_Data_1, DEGs_Data_2)
dataset_names <- c("DEGs_Data_1", "DEGs_Data_2")

# 5. Process each dataset in a loop
for (i in seq_along(datasets)) {
  data <- datasets[[i]]
  
  # Apply classification to each row
  data$status <- mapply(classify_gene, data$logFC, data$padj)
  
  # Save results into Results folder
  write.csv(
    data,
    file = paste0("Results/", dataset_names[i], "_classified.csv"),
    row.names = FALSE
  )

  # Print summary counts
  cat("\nSummary for", dataset_names[i], ":\n")
  print(table(data$status))
  
}
