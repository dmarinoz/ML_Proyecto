# init.R
#
# Example R code to install packages if not already installed
#

my_packages = c("shiny", "class", "caTools", "caret", "randomForest", "tree", "e1071", "naivebayes", "C50", "VGAM", "neuralnet","DT")
urlPackage <- "https://cran.r-project.org/src/contrib/Archive/randomForest/randomForest_4.6-12.tar.gz"
install_if_missing = function(p) {
  if (p %in% rownames(installed.packages()) == FALSE) {
    install.packages(p)
  }
}
install.packages(urlPackage, repos=NULL, type="source") 
invisible(sapply(my_packages, install_if_missing))
