#!/bin/bash

# Exit on error
set -e

# Show commands as they're executed
set -x

# Go to the project directory (adjust if needed)
cd "$(dirname "$0")"

# Install any missing R packages
if command -v Rscript &> /dev/null; then
    Rscript -e 'if (!requireNamespace("renv", quietly = TRUE)) install.packages("renv")'
    Rscript -e 'renv::restore()'
fi

# Install any missing Python packages
if [ -f "requirements.txt" ]; then
    pip install -r requirements.txt
fi

# Run Quarto preview
quarto preview