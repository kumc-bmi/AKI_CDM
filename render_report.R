##################
# Render Reports #
##################

#require libraries
source("./R/util.R")
require_libraries(c("rmarkdown"))


#===============generate report for Part I=========================#
#render the rmarkdown report with the following expected outputs:
# 1. AKI_CDM_EXT_VALID_p1_QA.html
# 2. AKI_CDM_EXT_VALID_p1_QA_TBL.xlsx

rmarkdown::render(input="./report/AKI_CDM_EXT_VALID_p1_QA.Rmd",
                  output_dir="./output/",
                  knit_root_dir="../")


#TODO:
#===============generate report for Part II=========================#