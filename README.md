## INSTALATION ##

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

## RUNNING APP ##
 
 Step 1. 
 Make sure that you have the following files (preferrably saved in the same folder)
 - Metadata (please call this metadata.txt)
 - Mixcr files to analyse
 - "shinyApp" folder containing the app.R file

 Step 2.
 Open app.R in R Studio and click Session > Set Working Directory and choose the directory which the "shinyApp" folder is contained

 Step 3. 
 Click "Run App" button and if all packages have installed properly, a new window should open up with the app

 Step 4.
 Follow the navbar options from left to right on the app 
 NOTE: Mixcr file names should be kept to 13 characters or less otherwise one of the graphs produces an error