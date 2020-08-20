report_qc_word_site=function(dat.DailyCounts, dat.ClinicalCourse, dat.Demographics, dat.Diagnoses, dat.Labs, dat.Medications, nm.report.file,icd.list,lab.range,site.nm){
qc.res=qc_site(dat.DailyCounts, dat.ClinicalCourse, dat.Demographics, dat.Diagnoses, dat.Labs, dat.Medications, icd.list,lab.range,site.nm)
colnames(qc.res$qc.dm$err.report)=
  colnames(qc.res$qc.cc$err.report)=
  colnames(qc.res$qc.dc$err.report)=
  colnames(qc.res$qc.crossover$err.report)=
  colnames(qc.res$qc.icd$err.report)=
  colnames(qc.res$qc.med$err.report)=
  colnames(qc.res$qc.lab$err.report)=
  colnames(qc.res$qc.lab.unit$err.report)=
c("SiteID", "Possible Issues")

file.nm=nm.report.file
rtffile <- RTF(file.nm)  # this can be an .rtf or a .doc
tryCatch(addParagraph(rtffile, "QC Metrics\n"), error=function(e) NA)
tryCatch(addParagraph(rtffile, paste0(Sys.Date(),"\n")), error=function(e) NA)
tryCatch(addParagraph(rtffile, "Demographic\n"), error=function(e) NA)
if(dim(qc.res$qc.dm$err.report)[1]!=0){
tryCatch(addTable(rtffile, as.data.frame(qc.res$qc.dm$err.report)), error=function(e) NA)}else{
addParagraph(rtffile, "no issue identified\n")
}
tryCatch(addParagraph(rtffile, "\n\nClinicalCourse:\n"), error=function(e) NA)
if(dim(qc.res$qc.cc$err.report)[1]!=0){
tryCatch(addTable(rtffile, as.data.frame(qc.res$qc.cc$err.report)), error=function(e) NA)}else{
addParagraph(rtffile, "no issue identified\n")
}
tryCatch(addParagraph(rtffile, "\n\nDailyCounts:\n"), error=function(e) NA)
if(dim(qc.res$qc.dc$err.report)[1]!=0){
tryCatch(addTable(rtffile, as.data.frame(qc.res$qc.dc$err.report)), error=function(e) NA)}else{
addParagraph(rtffile, "no issue identified\n")
}
tryCatch(addParagraph(rtffile, "\n\nCrossover:\n"), error=function(e) NA)
if(dim(qc.res$qc.crossover$err.report)[1]!=0){
tryCatch(addTable(rtffile, as.data.frame(qc.res$qc.crossover$err.report)), error=function(e) NA)}else{
addParagraph(rtffile, "no issue identified\n")
}
tryCatch(addParagraph(rtffile, "\n\nDiagnoses:\n"), error=function(e) NA)
if(dim(qc.res$qc.icd$err.report)[1]!=0){
tryCatch(addTable(rtffile, as.data.frame(qc.res$qc.icd$err.report)), error=function(e) NA)}else{
addParagraph(rtffile, "no issue identified\n")
}
tryCatch(addParagraph(rtffile, "\n\nMedications:\n"))
if(dim(qc.res$qc.med$err.report)[1]!=0){
tryCatch(addTable(rtffile, as.data.frame(qc.res$qc.med$err.report)), error=function(e) NA)}else{
addParagraph(rtffile, "no issue identified\n")
}
tryCatch(addParagraph(rtffile, "\n\nLabs:\n"))
if(dim(qc.res$qc.lab$err.report)[1]!=0){
tryCatch(addTable(rtffile, as.data.frame(qc.res$qc.lab$err.report)), error=function(e) NA)}else{
addParagraph(rtffile, "no issue identified\n")
}
tryCatch(addParagraph(rtffile, "\n\nLabs unit:\n"))
if(dim(qc.res$qc.lab.unit$err.report)[1]!=0){
  tryCatch(addTable(rtffile, as.data.frame(qc.res$qc.lab.unit$err.report)), error=function(e) NA)}else{
    addParagraph(rtffile, "no issue identified\n")
  }
done(rtffile)
}

