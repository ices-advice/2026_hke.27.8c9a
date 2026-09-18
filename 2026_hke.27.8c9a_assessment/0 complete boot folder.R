library(icesTAF)

# We can check the information available at: https://github.com/ices-taf/doc/wiki/Example-datasets-1
# Create a empty project

#taf.skeleton() #create structure in the project folder

# Data -------------------------------------------------------------------------
# Move data files to the initial/data folder


#draft.data() # create a empty entry for data. 
#?draft.data  # The help pages are more completed in the TAF package.

assessment_year<-2025

# Create the associated data.bib
draft.data(
  data.files ="indices_CPUEs",
  originator = "WGBIE",
  year=assessment_year,
  title = "Catch per unit effort (CPUE) indices derived from the standardized 
  process defined in the benchmark process (ICES, 2023). Two standardized indices: 
  one for three different Spanish trawl métiers targeting medium size fish (SpTrawl)
  and another for large fish combining gillnetters and longliners (SpVolpal).",
  file=TRUE) # create the DATA.bib with this entry

draft.data(
  data.files ="intercatch",
  originator = "intercatch",
  year=assessment_year,
  title = "NumbersAtAgeLength file provides the length data and StockOverview the catch data",
  file=TRUE,
  append = TRUE)

draft.data(
  data.files ="LFDs nsample intercatch",
  originator = "intercatch",
  year = assessment_year,
  period=1982-2024,
  title = "LFD (Length Frequency Distribution) Number of samples (NSample) for 1982-2024",
  file=TRUE,
  append = TRUE)

draft.data(
  data.files ="LFDs nsample surveys",
  originator = "WGBIE sharepoint accessions",
  year = assessment_year,
  period=1983-2024,
  title = "LFD (Length Frequency Distribution) Number of samples (NSample) for the SpGFS-WIBTS-Q4 (G2784),
  termed SpSurv, for the period 1983-2024 and
  PtGFS-WIBTS-Q4 (G8899), termed PtSurv, for the period 1989-2024",
  file=TRUE,
  append = TRUE)

draft.data(
  data.files ="Historical catch",
  originator = "WGBIE",
  year = assessment_year,
  period=1948-2024,
  title = "Catch data (from intercatch) after formatting in the WKANGHAKE benchmark for the period 1948 to 2024",
  file=TRUE,
  append = TRUE)

draft.data(
  data.files ="Historical LFDs",
  originator = "WGBIE",
  year = assessment_year,
  period=1982-2024,
  title = "Length frequency distribution (LFD) data (from intercatch) after formatting in 
  the WKANGHAKE benchmark for the period 1982 to 2024",
  file=TRUE,
  append = TRUE)

draft.data(
  data.files ="Historical indices",
  originator = "WGBIE",
  year = assessment_year,
  period=1982-2024,
  title = "Abundance indices (CPUE's and survey's abundance indices) after formatting 
  in the WGBIE 2025 for the period 1982 to 2024.
  Surveys include SPGFScaut-WIBTS-Q4 (G4309), SpGFS-WIBTS-Q4 (G2784) and ptGFS-WIBTSQ4
(G8899).",
  file=TRUE,
  append = TRUE)




draft.data(
  data.files ="Surveys",
  originator = "WGBIE sharepoint accessions",
  year = assessment_year,
  title = "Abundance and length distribution information for SPGFScaut-WIBTS-Q4 (G4309),
  SpGFS-WIBTS-Q4 (G2784) and ptGFS-WIBTSQ4
(G8899) surveys.",
  file=TRUE,
  append = TRUE)

draft.data(
  data.files ="SS files template",
  originator = "WGBIE",
  year = assessment_year,
  title = "Starter, data, forecast and control SS files to be used as a template
  (model of WGBIE 2025). This folder also includes the ss.par required to run the SS model. 
  This ss.par has been obtained by performing jitter on the model files located 
  in model/final. This process is carried out on a supercomputer due 
  to the computational demand. After this analysis, the ss.par reported is obtained",
  file=TRUE,
  append = TRUE)

draft.data(
  data.files ="ss.par files retros",
  originator = "WGBIE",
  year = assessment_year,
  title = "The ss.par files for running each of the retrospective model analyses.   
  The ss.par is derived from a jittering process performed on a supercomputer for 
  each of the retrospective models due to the computational requirements.",
  file=TRUE,
  append = TRUE)

draft.data(
  data.files ="Jitter models",
  originator = "WGBIE",
  year = assessment_year,
  title = "To ensure that the SS model finds a global optimum. A jittering procedure is performed
in a supercomputer (external). More specifically, the model is run many times with 
slightly different initial values. This file contains the model results of those 
jittered runs that can invert the Hessian. With this information the global
minimum is found and the ss.par of one of the models in the minimum is used to run our final model.",
  file=TRUE,
  append = TRUE)

draft.data(
  data.files ="Report tables last year",
  originator = "WGBIE",
  year = assessment_year,
  title = "This folder contains some of the tables from the previous year's report and are included to automatically update them in this project with the new information.",
  file=TRUE,
  append = TRUE)

draft.data(
  data.files ="Forecast last year",
  originator = "WGBIE",
  year = assessment_year,
  title = "This folder contains the short term forecast information of the previous year to be used to compare with the forecast values in this year.",
  file=TRUE,
  append = TRUE)

draft.data(
  data.files ="TACs",
  originator = "WGBIE",
  year = assessment_year,
  title = "Time series of TACs for plotting purposes.",
  file=TRUE,
  append = TRUE)


taf.boot() # Create the data folder in boot with all the files


draft.software("boot/initial/software/ss.exe",
               author = " Methot, Richard Donald",
               title="SS executable", 
               file=TRUE,
               version="3.30",append = TRUE)

draft.software("boot/initial/software/retro_create_files_function.R",
               author = "Marta Cousido Rocha based on retro function of r4ss",
               title="Function to create the reprospective analyses files", 
               file=TRUE,
               append = TRUE)


taf.boot()
