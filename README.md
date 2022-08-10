# CancerDataServices-Indexing_Manifest_CreatoR
A script that takes in a [validated](https://github.com/CBIIT/CancerDataServices-SubmissionValidationR) CDS submission file and creates indexing manifests.

To run the script on a CDS v1.3.1 template, run the following command in a terminal where R is installed for help.

```
Rscript --vanilla CDS-Indexing_Manifest_CreatoR.R -h
```

```
Usage: CDS-Indexing_Manifest_CreatoR.R [options]

CDS-Submission_ValidationR v.1.3.3

Options:
	-f CHARACTER, --file=CHARACTER
		A validated dataset file (.xlsx, .tsv, .csv) based on the template CDS_submission_metadata_template-v1.3.1.xlsx

	-h, --help
		Show this help message and exit
```
