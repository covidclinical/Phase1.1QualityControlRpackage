# 1. Install the package
```R
library(devtools)
install_github("ChuanHong/phase1.1.qc.test")
```
# 2. Conduct quality control
```R
library("phase1.1.qc.test")
data("icd.list")
data("lab.range")
# need to specify the input directly
# dir.input: the input directory (e.g.,"/Users/") is where you put the phase1.1 data (e.g., ClinicalCourse-MGB.csv)

site.nm="RIVHS"
dat.DailyCounts=read.csv(paste0(dir.input,"/DailyCounts-",site.nm,".csv"))
dat.ClinicalCourse=read.csv(paste0(dir.input,"/ClinicalCourse-",site.nm,".csv"))
dat.Demographics=read.csv(paste0(dir.input,"/Demographics-",site.nm,".csv"))
dat.Diagnoses=read.csv(paste0(dir.input,"/Diagnoses-",site.nm,".csv"))
dat.Labs=read.csv(paste0(dir.input,"/Labs-",site.nm,".csv"))
dat.Medications=read.csv(paste0(dir.input,"/Medications-",site.nm,".csv"))
nm.report.file=paste0("phase1.1.qc.report.", site.nm,".doc")

report_qc_word_site(dat.DailyCounts, dat.ClinicalCourse, dat.Demographics, dat.Diagnoses, dat.Labs, dat.Medications, nm.report.file, icd.list, lab.range, site.nm)
```
