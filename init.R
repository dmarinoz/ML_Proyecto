# init.R
#
# Example R code to install packages if not already installed
#

my_packages = c("shiny", "class", "caTools", "caret", "randomForest", "tree", "e1071", "naivebayes", "C50", "VGAM", "neuralnet")

install_if_missing = function(p) {
  if (p %in% rownames(installed.packages()) == FALSE) {
    install.packages(p)
  }
}

invisible(sapply(my_packages, install_if_missing))
