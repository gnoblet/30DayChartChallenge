# Convert R files to Quarto documents
# This script converts R scripts (.R) to Quarto documents (.qmd)
# for the 30DayChartChallenge project.
# This version is simplified to avoid dependencies

# Function to convert an R file to a Quarto document
convert_r_to_quarto <- function(r_file_path) {
  # Extract day number from filename
  day_num <- gsub(".*/", "", r_file_path)  # Remove directory part
  day_num <- gsub("\\.R$", "", day_num)    # Remove extension
  
  # Read the R script content
  r_content <- readLines(r_file_path)
  
  # Create YAML front matter for Quarto
  yaml_header <- c(
    "---",
    paste0("title: \"", day_num, " - 30DayChartChallenge\""),
    "format:",
    "  html:",
    "    code-fold: true",
    "    toc: true",
    "---",
    "",
    paste0("# ", gsub("_", " ", day_num)),
    "",
    "This document contains the R code for creating the plot for this day's challenge.",
    "",
    "```{r}",
    "#| label: setup",
    "#| message: false",
    "#| warning: false",
    ""
  )
  
  # Create Quarto content (wrap R code in code block)
  quarto_content <- c(
    yaml_header,
    r_content,
    "```",
    "",
    "## Final Plot",
    "",
    paste0("![", day_num, " plot](", day_num, ".png)"),
    ""
  )
  
  # Define output path
  output_path <- gsub("\\.R$", ".qmd", r_file_path)
  
  # Write the Quarto document
  writeLines(quarto_content, output_path)
  
  return(output_path)
}

# Find all R files in the 2025 directory
r_files <- list.files("./2025", pattern = "\\.R$", full.names = TRUE)

# Convert each R file to a Quarto document
results <- list()
for (file_path in r_files) {
  cat("Converting", file_path, "to Quarto format...\n")
  qmd_path <- convert_r_to_quarto(file_path)
  cat("Created", qmd_path, "\n")
  results <- c(results, qmd_path)
}

cat("Conversion complete! Created", length(results), "Quarto documents.\n")

# Next steps:
# 1. To create a Quarto website, create _quarto.yml in the root directory
# 2. Run quarto render to build the website
cat("Next steps:\n")
cat("1. Create a _quarto.yml file for website configuration\n")
cat("2. Run 'quarto render' to build the website\n")