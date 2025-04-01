
# ---------------------------------------------
# Cardiovascular Disease Dataset - Visual EDA Report
# ---------------------------------------------

# Install and load required libraries
packages <- c("DataExplorer")
install_if_missing <- function(p) {
  if (!require(p, character.only = TRUE)) {
    install.packages(p, dependencies = TRUE)
    library(p, character.only = TRUE)
  }
}
invisible(sapply(packages, install_if_missing))

# Load the dataset (update the path if your dataset is in Downloads folder)
df <- read.csv("data/cardio_train.csv", sep = ";")

# Generate Automated EDA Report
create_report(df, output_file = "results/EDA_Cardiovascular_Report.html", output_dir = getwd())

cat("Visual EDA report has been generated and saved as EDA_Cardiovascular_Report.html in your working directory.")
