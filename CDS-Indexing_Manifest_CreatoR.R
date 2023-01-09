#!/usr/bin/env Rscript

#Cancer Data Services - Indexing Manifest CreatoR.R


##################
#
# USAGE
#
##################

#This takes a CDS v1.3.1 submission template as input, sets up the manifest for indexing and assign GUIDs to all unique files.

#Run the following command in a terminal where R is installed for help.

#Rscript --vanilla CDS-Indexing_Manifest_CreatoR.R --help

##################
#
# Env. Setup
#
##################

#List of needed packages
list_of_packages=c("dplyr","readr","stringi","janitor","readxl","optparse","tools")

#Based on the packages that are present, install ones that are required.
new.packages <- list_of_packages[!(list_of_packages %in% installed.packages()[,"Package"])]
suppressMessages(if(length(new.packages)) install.packages(new.packages, repos = "http://cran.us.r-project.org"))

#Load libraries.
suppressMessages(library(dplyr,verbose = F))
suppressMessages(library(readr,verbose = F))
suppressMessages(library(stringi,verbose = F))
suppressMessages(library(janitor,verbose = F))
suppressMessages(library(readxl,verbose = F))
suppressMessages(library(optparse,verbose = F))
suppressMessages(library(tools,verbose = F))

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
opt_parser = OptionParser(option_list=option_list, description = "\nCDS-Indexing_Manifest_CreatoR v2.0.0")
opt = parse_args(opt_parser)

#If no options are presented, return --help, stop and print the following message.
if (is.null(opt$file)){
  print_help(opt_parser)
  cat("Please supply the input file (-f).\n\n")
  suppressMessages(stop(call.=FALSE))
}

#Data file pathway
file_path=file_path_as_absolute(opt$file)

#A start message for the user that the manifest creation is underway.
cat("The manifest file is being made at this time.\n")

###############
#
# File name manipulation
#
###############

#Rework the file path to obtain a file name, this will be used for the output file.
file_name=stri_reverse(stri_split_fixed(stri_reverse(basename(file_path)),pattern = ".", n=2)[[1]][2])

ext=tolower(stri_reverse(stri_split_fixed(stri_reverse(basename(file_path)),pattern = ".", n=2)[[1]][1]))

path=paste(dirname(file_path),"/",sep = "")

#Output file name based on input file name and date/time stamped.
output_file=paste(file_name,
                  "_index",
                  stri_replace_all_fixed(
                    str = Sys.Date(),
                    pattern = "-",
                    replacement = ""),
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

DCF_required_file_info=DCF_required[-1]

#If one of these columns is deleted and missing, it will throw an error.
if (any(!DCF_required%in%colnames(df))){
  stop("\n\nThe input file is missing one of the required columns for indexing: acl, file_size, md5sum, file_url_in_cds.\n\n")
}

#Will stop the code if there are required file columns that have missing data, and not all values in that row are missing, a row that doesn't have a file. This will not count against the acl property, as it is likely that will have a value even if there is not a file for that entry.
for (property in DCF_required_file_info){
  for (position in 1:dim(df[property])[1])
    if(is.na(df[property][position,])){
      if(!all(is.na(df[DCF_required_file_info][position,]))){
        stop("\n\nThere are missing portions of required file information (file_size, md5sum, file_url_in_cds) that are needed for manifest creation.\nPlease make sure to validate the submission using the CDS-SubmissionValidationR.\n\n")
      }
    }
}

#Will stop the code if the required file columns are present but the ACL value(s) are missing for the corresponding entry.
for (position in 1:dim(df)[1]){
  if (is.na(df['acl'][position,]) & !all(is.na(df[DCF_required_file_info][position,]))){
    stop("\n\nThere are missing portions of required file information (acl) that are needed for manifest creation.\nPlease make sure to validate the submission using the CDS-SubmissionValidationR.\n\n")
  }
}

#Do a comparison of the file_url_in_cds and file name to determine if the url contains the file name or if that needs to be added onto the file_url_in_cds
for (rownum in 1:dim(df)[1]){
  if (!is.na(df$file_url_in_cds[rownum])){
    if (basename(df$file_url_in_cds[rownum])!=df$file_name[rownum]){
      if (substr(x = df$file_url_in_cds[rownum],start = nchar(df$file_url_in_cds[rownum]), stop = nchar(df$file_url_in_cds[rownum]))!="/"){
        df$file_url_in_cds[rownum]=paste(df$file_url_in_cds[rownum],"/",sep = "")
      }
      df$file_url_in_cds[rownum]=paste(df$file_url_in_cds[rownum],df$file_name[rownum],sep = "")
    }
  }
}


#Take the data frame, clean up the property name for url, and bring those columns to the front.
df=df%>%
  mutate(url=file_url_in_cds)%>%
  select(file_size,md5sum,url,acl,everything())

#Make a subset data frame for DCF, since they only need those 5 columns for indexing.
df_dcf=df%>%
  mutate(GUID="")%>%
  select(GUID,file_size,md5sum,url,acl)

#Change 'file_url_in_cds' to 'url'
DCF_required_file_info[3]<-'url'

#Remove rows that do not have file specific info located in the required columns.
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

#For each unique file, apply a uuid to the GUID column. There is logic to handle this in both OSx and Linux, as the UUID call is different from R to the console.
for (x in 1:dim(df_dcf)[1]){
  if (get_os()=="osx"){
    uuid=tolower(system(command = "uuidgen", intern = T))
  }else{
    uuid=system(command = "uuid", intern = T)
  }
  df_dcf$GUID[x]=uuid
}

#Take the uuids in the GUID column and paste on the 'dg.4DCF/' prefix to create GUIDs for all the files.
df_dcf=mutate(df_dcf,GUID=paste("dg.4DFC/",GUID,sep = ""))

#Take the DCF indexing manifest and join it back to the full manifest. This will fill in any duplicate files with the same GUID value, ensuring that only one file has one GUID value.
df_sb=suppressMessages(left_join(df,df_dcf)%>%
                         select(GUID, file_size, md5sum, url, acl, everything()))

#write out TSV manifest.
write_tsv(x = df_sb, file = paste(path,output_file,".tsv",sep = ""),na="")

#An end message for the user that the manifest files have been created.
cat(paste("\n\nProcess Complete.\n\nThe output file can be found here: ",path,"\n\n",sep = "")) 
