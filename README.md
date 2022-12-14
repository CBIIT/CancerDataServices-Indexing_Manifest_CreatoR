# CancerDataServices-Indexing_Manifest_CreatoR
A script that takes in a [validated](https://github.com/CBIIT/CancerDataServices-SubmissionValidationR) [CDS submission template](https://github.com/CBIIT/cds-model/tree/main/metadata-manifest) and creates indexing manifests.

To run the script on a CDS v1.3.1 template, run the following command in a terminal where R is installed for help.

```
Rscript --vanilla CDS-Indexing_Manifest_CreatoR.R -h
```

```
Usage: CDS-Indexing_Manifest_CreatoR.R [options]

CDS-Submission_ValidationR v2.0.0

Options:
	-f CHARACTER, --file=CHARACTER
		A validated dataset file (.xlsx, .tsv, .csv) based on the template CDS_submission_metadata_template-v1.3.1.xlsx

	-h, --help
		Show this help message and exit
```

When running the command on the test file:
```
Rscript --vanilla CDS-Indexing_Manifest_CreatoR.R -f test_files/a_all_pass-v1.3.1.xlsx 
The manifest files are being made at this time.
The manifest files are located in the same directory as your input file.
```

The output will be one file that contains the information for DCF indexing as well as all the metadata information for Seven Bridges indexing.
