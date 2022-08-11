#!/usr/bin/env Rscript

#Cancer Data Services - Indexing Manifest CreatoR.R


##################
#
# USAGE
#
##################

#This takes a CDS v1.3.1 submission template as input, sets up the manifest for indexing and assign GUIDs to all unqiue files.

#Run the following command in a terminal where R is installed for help.

#Rscript --vanilla CDS-Indexing_Manifest_CreatoR.R --help

##################
#
# Env. Setup
#
##################

#List of needed packages
list_of_packages=c("dplyr","readr","stringi","janitor","readxl","optparse")

#Based on the packages that are present, install ones that are required.
new.packages <- list_of_packages[!(list_of_packages %in% installed.packages()[,"Package"])]
suppressMessages(if(length(new.packages)) install.packages(new.packages))

#Load libraries.
suppressMessages(library(dplyr,verbose = F))
suppressMessages(library(readr,verbose = F))
suppressMessages(library(stringi,verbose = F))
suppressMessages(library(janitor,verbose = F))
suppressMessages(library(readxl,verbose = F))
suppressMessages(library(optparse,verbose = F))

#remove objects that are no longer used.
rm(list_of_packages)
rm(new.packages)


##################
#
# Arg parse
#
##################

#Option list for arg parse
option_list = list(
  make_option(c("-f", "--file"), type="character", default=NULL, 
              help="A validated dataset file (.xlsx, .tsv, .csv) based on the template CDS_submission_metadata_template-v1.3.1.xlsx", metavar="character")
)

#create list of options and values for file input
opt_parser = OptionParser(option_list=option_list, description = "\nCDS-Indexing_Manifest_CreatoR v.1.3.1")
opt = parse_args(opt_parser)

#If no options are presented, return --help, stop and print the following message.
if (is.null(opt$file)){
  print_help(opt_parser)
  cat("Please supply the input file (-f).\n\n")
  suppressMessages(stop(call.=FALSE))
}

#Data file pathway
file_path=opt$file

#A start message for the user that the manifest creation is underway.
cat("The manifest files are being made at this time.\n")

###############
#
# File name manipulation
#
###############

#Rework the file path to obtain a file name, this will be used for the output file.
file_name=stri_reverse(stri_split_fixed(str = stri_reverse(file_path), pattern="/",n = 2)[[1]][1])

ext=tolower(stri_reverse(stri_split_fixed(str = stri_reverse(file_name),pattern = ".",n=2)[[1]][1]))

path=paste(stri_reverse(stri_split_fixed(str = stri_reverse(file_path), pattern="/",n = 2)[[1]][2]),"/",sep = "")

#Output file name based on input file name and date/time stamped.
output_file=paste(file_name,
                  "_indexing_output_",
                  stri_replace_all_fixed(
                    str = stri_replace_all_fixed(
                      str = stri_replace_all_fixed(
                        str = Sys.time(),
                        pattern = ":",
                        replacement = "_"),
                      pattern = "-",
                      replacement = "_"),
                    pattern = " ",
                    replacement = "_"),
                  ".txt",
                  sep="")


#Read in metadata page/file to check against the expected/required properties. 
#Logic has been setup to accept the original XLSX as well as a TSV or CSV format.
if (ext == "tsv"){
  df=suppressMessages(read_tsv(file = file_path, guess_max = 1000000, col_types = cols(.default = col_character())))
}else if (ext == "csv"){
  df=suppressMessages(read_csv(file = file_path, guess_max = 1000000, col_types = cols(.default = col_character())))
}else if (ext == "xlsx"){
  df=suppressMessages(read_xlsx(path = file_path,sheet = "Metadata", guess_max = 1000000, col_types = "text"))
}else{
  stop("\n\nERROR: Please submit a data file that is in either xlsx, tsv or csv format.\n\n")
}

#Check for the expected columns for indexing at minimum for DCF. These should already be present after validation, but this double checks that.
DCF_required=c("acl",
               "file_size",
               "md5sum",
               'file_url_in_cds')

#If one of these columns is deleted and missing, it will throw an error.
if (any(!DCF_required%in%colnames(df))){
  stop("\n\nThe input file is missing one of the required columns for indexing: acl, file_size, md5sum, file_url_in_cds.\n\n")
}

#Will stop the code if there are required columns that have missing data, and not all values in that row are missing, a row that doesn't have a file. This will not count against the acl property, as it is likely that will have a value even if there is not a file for that entry.
for (property in DCF_required){
  for (position in 1:dim(df[property])[1])
    if(is.na(df[property][position,])){
      if(!all(is.na(df[DCF_required[-1]][position,]))){
        stop("\n\nThere are missing portions of required information (acl, file_size, md5sum, file_url_in_cds) that are needed for manifest creation.\nPlease make sure to validate the submission using the CDS-SubmissionValidationR.\n\n")
      }
    }
}

#Take the data frame, clean up the property name for url, and bring those columns to the front.
df=df%>%
  mutate(url=file_url_in_cds)%>%
  select(acl,file_size,md5sum,url,-file_url_in_cds,everything())

#Make a subset data frame for DCF, since they only need those 5 columns for indexing.
df_dcf=df%>%
  mutate(GUID="")%>%
  select(GUID,acl,file_size,md5sum,url)

#Remove rows that do not have file specific info located in the required columns.
DCF_required_file_info=c("file_size",
               "md5sum",
               'url')

for (row in 1:dim(df_dcf)[1]){
  if (all(is.na(df_dcf[DCF_required_file_info][row,]))){
    df_dcf=df_dcf[-row,]
  }
}

#Determine that files are unique, prevents multiple GUIDs for one file.
df_dcf=unique(df_dcf)

#Function to determine if operating system is OS is mac or linux, to run the UUID generation.
get_os <- function(){
  sysinf <- Sys.info()
  if (!is.null(sysinf)){
    os <- sysinf['sysname']
    if (os == 'Darwin')
      os <- "osx"
  } else { ## mystery machine
    os <- .Platform$OS.type
    if (grepl("^darwin", R.version$os))
      os <- "osx"
    if (grepl("linux-gnu", R.version$os))
      os <- "linux"
  }
  tolower(os)
}

#For each unique file, apply a uuid to the GUID column. There is logic to handle this in both OSx and Linux, as the UUID call is different.
for (x in 1:dim(df_dcf)[1]){
  if (get_os()=="osx"){
    uuid=tolower(system(command = "uuidgen", intern = T))
  }else{
    uuid=system(command = "uuid", intern = T)
  }
  df_dcf$GUID[x]=uuid
}

#Take the uuids in the GUID column and paste on the 'dg.4DCF/' prefix to create GUIDs for all the files.
df_dcf=mutate(df_dcf,GUID=paste("dg.4DCF/",GUID,sep = ""))

#Take the DCF indexing manifest and join it back to the full manifest for Seven Bridges. This will fill in any duplictate files with the same GUID value, ensuring that only one file has one GUID value.
df_sb=suppressMessages(left_join(df,df_dcf)%>%
  select(GUID,acl, file_size, md5sum, url, everything()))

#write out TSVs for DCF and SB
write_tsv(x = df_dcf, file = paste(path,"DCF_",output_file,sep = ""),na="")
write_tsv(x = df_sb, file = paste(path,"SB_",output_file,sep = ""),na="")

#An end message for the user that the manifest files have been created.
cat("The manifest files are located in the same directory as your input file.\n")
