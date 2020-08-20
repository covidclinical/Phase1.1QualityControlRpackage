qc_site=function(dat.DailyCounts, dat.ClinicalCourse, dat.Demographics, dat.Diagnoses, dat.Labs, dat.Medications, icd.list,lab.range,site.nm){
qc.dm=err_report_demographics_site(dat.Demographics, site.nm)
qc.cc=err_report_clinicalcourse_site(dat.ClinicalCourse, site.nm)
qc.dc=err_report_dailycounts_site(dat.DailyCounts, site.nm)
qc.crossover=err_report_crossover_site(dat.ClinicalCourse, dat.Demographics, dat.DailyCounts, site.nm)
qc.icd=err_report_diagnosis_site(dat.Diagnoses, dat.ClinicalCourse, dat.Demographics, dat.DailyCounts, icd.list, site.nm)
qc.med=err_report_med_site(dat.ClinicalCourse, dat.Demographics, dat.DailyCounts, dat.Medications, site.nm)
qc.lab=err_report_lab_site(dat.ClinicalCourse, dat.Demographics, dat.DailyCounts, dat.Labs, site.nm)
qc.lab.unit=err_report_lab_unit_site(dat.Labs, lab.range, site.nm)
list(qc.dm=qc.dm, qc.cc=qc.cc, qc.dc=qc.dc, qc.crossover=qc.crossover, qc.icd=qc.icd, qc.med=qc.med, qc.lab=qc.lab, qc.lab.unit=qc.lab.unit)
}
