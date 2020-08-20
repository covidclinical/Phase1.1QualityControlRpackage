\name{report_qc_word_site}
\alias{report_qc_word_site}
\title{Quality control for site data}
\description{
Create the QC report in word file}
\usage{
report_qc_word_site(dat.DailyCounts, dat.ClinicalCourse, dat.Demographics, dat.Diagnoses, dat.Labs, dat.Medications, nm.report.file, icd.list, lab.range, site.nm)
}
\arguments{
  \item{dat.DailyCounts}{the csv data file for DailyCounts}
  \item{dat.ClinicalCourse}{the csv data file for ClinicalCourse}
  \item{dat.Demographics}{the csv data file for Demographics}
  \item{dat.Diagnoses}{the csv data file for Diagnoses}
  \item{dat.Labs}{the csv data file for Labs}
  \item{dat.Medications}{the csv data file for Medications}

  \item{nm.report.file}{the directory and file naem for the output report}
  \item{icd.list}{the list of icd codes}
  \item{lab.range}{the range of each lab for detecting the lab unit issue}
  \item{site.nm}{the siteID (e.g., "MGB")}
}

\author{Chuan Hong}

\examples{
library("phase1.1.qc.test")
data("icd.list")
data("lab.range")
dir.input="~/4ce_data/input/"
nm.report.file=paste0("phase1.1.qc.report.", site.nm,".doc")
site.nm="RIVHS"
report_qc_word_site(dir.input, nm.report.file, icd.list, lab.range, site.nm)
}

\keyword{quality control}
