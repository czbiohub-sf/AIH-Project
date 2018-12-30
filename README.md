# AIH-Project
Github repo for AIH data analysis

# Organization
AIH-metadata include R scripts and CSVs/excel tables summarizing information about the patient cohort and manipulating the RedCap metadata

AIH-NovaSeq-Gene-Counts includes the raw gene count tables concatentated from STAR alignment from IDSeq and a version of the script used to concatenate them. Filenames indicate if the samples were DASHed or not, what filtering criteria was used, if cutting adaptors was performed and if a subsampled version of sample 67 was used.

AIH-PriceSeqFilter includes scripts, presentations and figures for analyzing effects of various PriceSeqFilter parameters and cutadapt on differential gene expression analysis

DifferentialGeneExpression-R-Scripts includes DESeq2, EdgeR, WGCNA and various plotting scripts (ie PCA, MA etc) for AIH differential gene expression analysis

DifferentialGeneExpression-Output includes figures, CSVs and Excel tables produced which display differential gene expression analysis, and can be used for IPA analysis.

Pegivirus-Analysis contains scripts and figures related to phylogeny of pegivirus in our samples
