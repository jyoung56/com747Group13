
packages <- c("DataExplorer")
install_if_missing <- function(p) {
  if (!require(p, character.only = TRUE)) {
    install.packages(p, dependencies = TRUE)
    library(p, character.only = TRUE)
  }
}
invisible(sapply(packages, install_if_missing))
df <- read.csv("C:/Users/PC/Downloads/cardio_train.csv", sep = ";")
create_report(df, output_file = "EDA_Cardiovascular_Report.html", output_dir = getwd())
cat("📊 EDA report generated: EDA_Cardiovascular_Report.html\n")
