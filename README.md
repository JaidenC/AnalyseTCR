Step 1. 
Install R and R Studio by following the prompts at the following websites
Windows - https://cran.r-project.org/bin/windows/base/
Mac - https://cran.r-project.org/bin/macosx/
R studio - https://www.rstudio.com/products/rstudio/download/

Step 2.
Install latest version of Python by following the prompts at the website below
https://www.python.org/downloads/ - Choose the prompts based on the operating system you are using. 

Step 3.
Open R and type in the command below 
install.packages("shiny", "shinyFiles", "DT", "tcR", "shinyjs", "dplyr", "ggplot2", "reticulate")

Step 4. Download Immunarch package by going to the website below
install.packages("devtools", dependencies = T)
devtools::install_local("path/to/your/folder/with/immunarch.tar.gz", dependencies=T)
replace "path/to/your/folder/with/immunarch.tar.gz" with a path to the folder that contains immunarch.tar.gz
