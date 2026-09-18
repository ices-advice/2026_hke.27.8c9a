#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Report plots for SS shake model                                      #
# Moving figures in data and output folders to the report/plots folder #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Modified 25/03/2024 #
#~~~~~~~~~~~~~~~~~~~~~~~~~
# Marta Cousido          #
# Anxo Paz               #
# Santiago Cervino       #
#~~~~~~~~~~~~~~~~~~~~~~~~~

## Press Ctrl + Shift + O to see the document outline

# Surveys data plots ------------------------------------------------------------

path1<-paste0(getwd(), "/","data/indices", sep="") 
path2<-paste0(getwd(), "/","report/plots", sep="") 

source_file <- file.path(path1, "figure 10.3.png")

file.copy(source_file, path2)

# CPUEs -----------------------------------------------------------------------

source_file <- file.path(path1, "Figure 10.4a.jpeg")

file.copy(source_file, path2)

source_file <- file.path(path1, "Figure 10.4b.jpeg")

file.copy(source_file, path2)

# Selectivity plots ------------------------------------------------------------

path1<-paste0(getwd(), "/","output/r4ss plots", sep="") 
source_file <- file.path(path1, "sel01_multiple_fleets_length1.png")

file.copy(source_file, path2)

# Old file name
old_file_name <-file.path(path2, "sel01_multiple_fleets_length1.png")

# New file name
new_file_name <- file.path(path2, "Figure 10.8.png")

# Rename the file
file.rename(from = old_file_name, to = new_file_name)


# Data plot --------------------------------------------------------------------

path1<-paste0(getwd(), "/","output/r4ss plots", sep="") 
source_file <- file.path(path1, "data_plot2.png")

file.copy(source_file, path2)

# Old file name
old_file_name <-file.path(path2, "data_plot2.png")

# New file name
new_file_name <- file.path(path2, "Figure 10.5.png")

# Rename the file
file.rename(from = old_file_name, to = new_file_name)


# SR plot --------------------------------------------------------------------

path1<-paste0(getwd(), "/","output/r4ss plots", sep="") 
source_file <- file.path(path1, "SR_curve.png")

file.copy(source_file, path2)

# Old file name
old_file_name <-file.path(path2, "SR_curve.png")

# New file name
new_file_name <- file.path(path2, "Figure 10.13.png")

# Rename the file
file.rename(from = old_file_name, to = new_file_name)