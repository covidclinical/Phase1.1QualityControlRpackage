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
# need to specify the input and output directly
# dir.input: the input directory (e.g.,"/Users/") is where you put the phase1.1 data (e.g., ClinicalCourse-MGB.csv)
# nm.report.file: the directory and file name of the qc report

site.nm="RIVHS"
report_qc_word_site(dir.input, nm.report.file=paste0("phase1.1.qc.report.", site.nm,".doc"), icd.list, lab.range, site.nm)
```
