err_report_demographics_site=function(dat.Demographics, site.nm){
  err.label=
    c("missing (sex,age,race)=all",
      "N_all<N_ever_severe",
      "negative N (not -999 or -99)"
      )

    err=NULL
    dat.site=dat.Demographics
    colnames(dat.site)=tolower(colnames(dat.site))

    nm.check=c("sex", "age_group", "race")
    dat.check=dat.site[,nm.check]
    err=c(err,0%in%apply(dat.check,1, function(x) sum(x!="all"))!=1)

    id.nomiss=which(dat.site[,"num_patients_all"]>0 & dat.site[,"num_patients_ever_severe"]>0)
    err=c(err, any(dat.site[id.nomiss,"num_patients_all"]<dat.site[id.nomiss,"num_patients_ever_severe"]))

    dat.check=dat.site[,c("num_patients_all", "num_patients_ever_severe")]
    err=c(err, any(dat.check[dat.check<0]%in%c(-999,-99)!=1))
    dat.check=unique(dat.site[,"age_group"])
    report=data.frame(site.nm, label=err.label, err)

  err.report=report[report[,"err"]==T,c("site.nm", "label")]
  list(err.report=err.report, err.label=err.label)
}

err_report_clinicalcourse_site=function(dat.ClinicalCourse, site.nm){
  err.label=c(
    "no days0",
    "N_all at day 0 is not the largest",
    "N_all<N_ever_severe",
    "negative N (not -999 or -99)")

    err=NULL
    dat.site=dat.ClinicalCourse
    colnames(dat.site)=tolower(colnames(dat.site))

    err=c(err,0%in%dat.site[,"days_since_admission"]!=1)
    err=c(err,0%in%dat.site[which(dat.site[,"num_patients_all_still_in_hospital"]==max(dat.site[,"num_patients_all_still_in_hospital"])),"days_since_admission"]!=1)
    id.nomiss=which(dat.site[,"num_patients_all_still_in_hospital"]>0 & dat.site[,"num_patients_ever_severe_still_in_hospital"]>0)
    id3=which(dat.site[id.nomiss,"num_patients_all_still_in_hospital"]<dat.site[id.nomiss,"num_patients_ever_severe_still_in_hospital"])
    err=c(err,length(id3)>1)

    dat.check=dat.site[,c("num_patients_all_still_in_hospital", "num_patients_ever_severe_still_in_hospital")]
    id4=which(dat.check[dat.check<0]%in%c(-999,-99)!=1)
    err=c(err,length(id4)>1)
    report=data.frame(site.nm, label=err.label, err)

  err.report=report[report[,"err"]==TRUE,c("site.nm", "label")]
  list(err.report=err.report, err.label=err.label)
}

err_report_dailycounts_site=function(dat.DailyCounts,site.nm){
  err.label=
    c("cumulative_patients_all is not largest in last date",
      "cumulative_patients_severe is not largest in last date",
      "cumulative_patients_dead is not largest in last date"
      )
    err=NULL
    dat.site=dat.DailyCounts
    colnames(dat.site)=tolower(colnames(dat.site))
    dat.site=dat.site[order(as.Date(as.character(dat.site$calendar_date),format='%Y-%m-%d')),]

    err=dim(dat.site)[1]%in%which(dat.site[,"cumulative_patients_all"]==max(dat.site[,"cumulative_patients_all"]))!=1
    err=c(err, dim(dat.site)[1]%in%which(dat.site[,"cumulative_patients_severe"]==max(dat.site[,"cumulative_patients_severe"]))!=1)
    err=c(err, dim(dat.site)[1]%in%which(dat.site[,"cumulative_patients_dead"]==max(dat.site[,"cumulative_patients_dead"]))!=1)

    report=data.frame(site.nm, label=err.label, err)

  err.report=report[report[,"err"]==T,c("site.nm", "label")]
  list(err.report=err.report, err.label=err.label)
}


err_report_crossover_site=function(dat.ClinicalCourse, dat.Demographics, dat.DailyCounts, site.nm){

  err.label=c(
    "missing ClinicalCourse or Demographics or DailyCounts",
    "N_all in the Demographics and DailyCounts not match",
    "N_all in the ClincalCourse and DailyCounts not match",
    "N_ever_severe in Demographics and DailyCounts not match",
    "N_ever_severe in ClinicalCourse and DailyCounts not match"
    )


    exist.cc=is.null(dat.ClinicalCourse)!=1
    exist.dm=is.null(dat.Demographics)!=1
    exist.dc=is.null(dat.DailyCounts)!=1

    if(exist.cc*exist.dm*exist.dc==0){err1=T; err2=F; err3=F; err4=F; err5=F}else{
    err1=F
    dat.site.cc=dat.ClinicalCourse
    colnames(dat.site.cc)=tolower(colnames(dat.site.cc))
    dat.site.dm=dat.Demographics
    colnames(dat.site.dm)=tolower(colnames(dat.site.dm))
    dat.site.dc=dat.DailyCounts
    colnames(dat.site.dc)=tolower(colnames(dat.site.dc))


    n.dm=dat.site.dm[which(apply(dat.site.dm[,c("sex", "age_group", "race")],1, function(x) all(x=="all"))),c("num_patients_all")]
    n.cc=dat.site.cc[dat.site.cc$days_since_admission==0,"num_patients_all_still_in_hospital"]
    n.dc=max(dat.site.dc[,"cumulative_patients_all"])

    if(all(c(length(n.dm), length(n.dc))>0)){err2=n.dm!=n.dc}else{err2=FALSE}
    if(all(c(length(n.cc), length(n.dc))>0)){err3=n.cc!=n.dc}else{err3=FALSE}

    n.dm=dat.site.dm[which(apply(dat.site.dm[,c("sex", "age_group", "race")],1, function(x) all(x=="all"))),c("num_patients_ever_severe")]
    n.cc=max(dat.site.cc[,"num_patients_ever_severe_still_in_hospital"])
    n.dc=max(dat.site.dc[,"cumulative_patients_severe"])

    if(all(c(length(n.dm), length(n.dc))>0)){err4=n.dm!=n.dc}else{err4=FALSE}
    if(all(c(length(n.cc), length(n.dc))>0)){err5=n.cc!=n.dc}else{err5=FALSE}
    }
    err=c(err1, err2, err3, err4, err5)
    report=data.frame(site.nm, label=err.label, err)

  err.report=report[report[,"err"]==TRUE,c("site.nm", "label")]
  list(err.report=err.report, err.label=err.label)
}

err_report_diagnosis_site=function(dat.Diagnoses, dat.ClinicalCourse, dat.Demographics, dat.DailyCounts, icd.list, site.nm){
  icd.list0=icd.list
  err.label1="N_all_before > N_all at day0"
  err.label2="N_all_since > N_all at day0"
  err.label3= "N_all_before < N_ever_severe_before"
  err.label4= "N_all_since < N_ever_severe_since"
  err.label5= "negative N (not -999 or -99)"
  err.label6= "ICD not belong to dictionary"
  err.label7= "N_ever_severe<N_ever_severe_before with diagnosis"
  err.label8= "N_ever_severe<N_ever_severe_since with diagnosis"

  err.label=c(err.label1, err.label2, err.label3, err.label4, err.label5, err.label6, err.label7, err.label8)

    dat.site.cc=dat.ClinicalCourse
    colnames(dat.site.cc)=tolower(colnames(dat.site.cc))
    dat.site.dm=dat.Demographics
    colnames(dat.site.dm)=tolower(colnames(dat.site.dm))
    dat.site.dc=dat.DailyCounts
    colnames(dat.site.dc)=tolower(colnames(dat.site.dc))
    dat.site=dat.Diagnoses
    colnames(dat.site)=tolower(colnames(dat.site))

    n.all.cc=dat.site.cc[dat.site.cc$days_since_admission==0,"num_patients_all_still_in_hospital"]
    n.all.dm=tryCatch(dat.site.dm[dat.site.dm$sex=="all"& dat.site.dm$race=="all" & dat.site.dm$age_group=="all", "num_patients_all"], error=function(e) NA)
    n.all.dc=max(dat.site.dc[, "cumulative_patients_all"])
    n.all=max(n.all.cc, n.all.dm, n.all.dc, na.rm=T)
    n.icd=max(dat.site[,"num_patients_all_before_admission"])
    err1=n.icd>n.all

    n.icd=max(dat.site[,"num_patients_all_since_admission"])
    err2=n.icd>n.all

    id.nomiss=which(dat.site[,"num_patients_all_before_admission"]>0 & dat.site[,"num_patients_ever_severe_before_admission"]>0)
    err3=any(dat.site[id.nomiss,"num_patients_all_before_admission"]<dat.site[id.nomiss,"num_patients_ever_severe_before_admission"])

    id.nomiss=which(dat.site[,"num_patients_all_since_admission"]>0 & dat.site[,"num_patients_ever_severe_since_admission"]>0)
    err4=any(dat.site[id.nomiss,"num_patients_all_since_admission"]<dat.site[id.nomiss,"num_patients_ever_since_before_admission"])

    dat.check=dat.site[,setdiff(colnames(dat.site), c("siteid", "icd_code_3chars", "icd_version"))]
    err5=any(unique(dat.check[dat.check<0])%in%c(-99, -999)!=1)

    icd.list=unique(as.character(dat.site[,"icd_code_3chars"]))
    err6=paste(icd.list[icd.list%in%icd.list0!=1],collapse=";")

    err7=any(dat.site[id.nomiss,"num_patients_ever_severe_before_admission"]>max(dat.site.dc[, "cumulative_patients_severe"]))
    note7=dat.site[id.nomiss[which(dat.site[id.nomiss,"num_patients_ever_severe_before_admission"]>max(dat.site.dc[, "cumulative_patients_severe"]))],c("icd_code_3chars","num_patients_ever_severe_since_admission")]
    err8=any(dat.site[id.nomiss,"num_patients_ever_severe_since_admission"]>max(dat.site.dc[, "cumulative_patients_severe"]))
    note8=dat.site[id.nomiss[which(dat.site[id.nomiss,"num_patients_ever_severe_since_admission"]>max(dat.site.dc[, "cumulative_patients_severe"]))],c("icd_code_3chars","num_patients_ever_severe_since_admission")]

    err=c(err1, err2, err3, err4, err5, err6, err7, err8)
    err.label[6]=paste0("ICD not in dictionary:", err6)
    if(dim(note7)[1]!=0){
    err.label[7]=paste0(paste(note7, collapse=";"), "")}

    if(dim(note8)[1]!=0){
    err.label[8]=paste0(err.label[8],":",max(dat.site.dc[, "cumulative_patients_severe"])," vs. ", note8[2], "(", note8[1], ")")}
    report=data.frame(site.nm, label=err.label, err)

  err.report=report[as.character(report[,"err"])%in%c(FALSE,"")!=1,c("site.nm", "label")]
  list(err.report=err.report, err.label=err.label)
}

err_report_lab_site=function(dat.ClinicalCourse, dat.Demographics, dat.DailyCounts, dat.Labs, site.nm){
  err.label=c(
    "N_all > N_all at day0",
    "N_all < N_ever_severe",
    "negative N (not -999 or -99)",
    "day 0+ not included",
    "Inf or -Inf",
    "N_ever_severe (in DailyCount)<N_ever_severe (in Labs) for lab")

    dat.site.cc=dat.ClinicalCourse
    colnames(dat.site.cc)=tolower(colnames(dat.site.cc))
    dat.site.dm=dat.Demographics
    colnames(dat.site.dm)=tolower(colnames(dat.site.dm))
    dat.site.dc=dat.DailyCounts
    colnames(dat.site.dc)=tolower(colnames(dat.site.dc))
    dat.site=dat.Labs
    colnames(dat.site)=tolower(colnames(dat.site))

    n.all.cc=dat.site.cc[dat.site.cc$days_since_admission==0,"num_patients_all_still_in_hospital"]
    n.all.dm=tryCatch(dat.site.dm[dat.site.dm$sex=="all"& dat.site.dm$race=="all" & dat.site.dm$age_group=="all", "num_patients_all"], error=function(e) NA)
    n.all.dc=max(dat.site.dc[, "cumulative_patients_all"])
    n.all=max(n.all.cc, n.all.dm, n.all.dc, na.rm=T)

    n.lab=max(dat.site[dat.site$days_since_admission==0,"num_patients_all"])
    err1=n.lab>n.all

    id.nomiss=which(dat.site[,"num_patients_all"]>0 & dat.site[,"num_patients_ever_severe"]>0)
    err2=any(dat.site[id.nomiss,"num_patients_all"]<dat.site[id.nomiss,"num_patients_ever_severe"])

    dat.check=dat.site[,setdiff(colnames(dat.site), c("siteid", "loinc", "units", "days_since_admission", colnames(dat.site)[grepl("log",colnames(dat.site))]))]
    err3=any(unique(dat.check[dat.check<0])%in%c(-99, -999)!=1)

    err4=sum(dat.site[,"days_since_admission"]>0)<1

    err5=any(dat.site%in%c(Inf, -Inf))
    err6=any(dat.site[id.nomiss,"num_patients_ever_severe"]>max(dat.site.dc[, "cumulative_patients_severe"]))
    if(err6==T){
      label6=paste(err.label[6],paste(as.character(dat.site[id.nomiss[which(dat.site[id.nomiss,"num_patients_ever_severe"]>max(dat.site.dc[, "cumulative_patients_severe"]))],"loinc"]),collapse = ";"),sep=" ")
      err.label[6]=label6
      }

    err=c(err1, err2, err3, err4, err5, err6)
    report=data.frame(site.nm, label=err.label, err)

  err.report=report[report[,"err"]==TRUE,c("site.nm", "label")]
  list(err.report=err.report, err.label=err.label)
}

err_report_med_site=function(dat.ClinicalCourse, dat.Demographics, dat.DailyCounts, dat.Medications, site.nm){
  err.label1="N_all_before > N_all at day0"
  err.label2="N_all_since > N_all at day0"
  err.label3= "N_all_before < N_ever_severe_before"
  err.label4= "N_all_since < N_ever_severe_since"
  err.label5= "negative N (not -999 or -99)"
  err.label6= "N_ever_severe<N_ever_severe_before with med"
  err.label7= "N_ever_severe<N_ever_severe_since with med"
  err.label8= "no data"

  err.label=c(err.label1, err.label2, err.label3, err.label4, err.label5, err.label6, err.label7, err.label8)
  if(is.null(dat.Medications)==1){
    err.report=data.frame(site.nm=site.nm, label="no input data file for medication")}else{
    dat.site.cc=dat.ClinicalCourse
    colnames(dat.site.cc)=tolower(colnames(dat.site.cc))
    dat.site.dm=dat.Demographics
    colnames(dat.site.dm)=tolower(colnames(dat.site.dm))
    dat.site.dc=dat.DailyCounts
    colnames(dat.site.dc)=tolower(colnames(dat.site.dc))
    dat.site=dat.Medications
    colnames(dat.site)=tolower(colnames(dat.site))

    if(dim(dat.site)[1]!=0){
    err8=FALSE
    n.all.cc=dat.site.cc[dat.site.cc$days_since_admission==0,"num_patients_all_still_in_hospital"]
    n.all.dm=tryCatch(dat.site.dm[dat.site.dm$sex=="all"& dat.site.dm$race=="all" & dat.site.dm$age_group=="all", "num_patients_all"], error=function(e) NA)
    n.all.dc=max(dat.site.dc[, "cumulative_patients_all"])
    n.all=max(n.all.cc, n.all.dm, n.all.dc, na.rm=T)
    n.med=max(dat.site[,"num_patients_all_before_admission"])
    err1=n.med>n.all

    n.med=max(dat.site[,"num_patients_all_since_admission"])
    err2=n.med>n.all

    id.nomiss=which(dat.site[,"num_patients_all_before_admission"]>0 & dat.site[,"num_patients_ever_severe_before_admission"]>0)
    err3=any(dat.site[id.nomiss,"num_patients_all_before_admission"]<dat.site[id.nomiss,"num_patients_ever_severe_before_admission"])

    id.nomiss=which(dat.site[,"num_patients_all_since_admission"]>0 & dat.site[,"num_patients_ever_severe_since_admission"]>0)
    err4=any(dat.site[id.nomiss,"num_patients_all_since_admission"]<dat.site[id.nomiss,"num_patients_ever_since_before_admission"])

    dat.check=dat.site[,setdiff(colnames(dat.site), c("siteid", "med_class"))]
    err5=any(unique(dat.check[dat.check<0])%in%c(-99, -999)!=1)

    err6=any(dat.site[id.nomiss,"num_patients_ever_severe_before_admission"]>max(dat.site.dc[, "cumulative_patients_severe"]))
    err7=any(dat.site[id.nomiss,"num_patients_ever_severe_since_admission"]>max(dat.site.dc[, "cumulative_patients_severe"]))

    err=c(err1, err2, err3, err4, err5, err6, err7, err8)}else{
    err1=err2=err3=err4=err5=err6=err7=FALSE; err8=TRUE
    err=c(err1, err2, err3, err4, err5, err6, err7, err8)
    }
    report=data.frame(site.nm, label=err.label, err)

  err.report=report[report[,"err"]==TRUE,c("site.nm", "label")]}
  list(err.report=err.report, err.label=err.label)
}


err_report_lab_unit_site=function(dat.Labs, lab.range, site.nm){
  dat=dat.Labs
  colnames(dat)=tolower(colnames(dat))

  dat$siteid=toupper(dat$siteid)
  nm.day="days_since_admission"
  comb.lab=dat
  comb.lab$loinc=trimws(comb.lab$loinc, which = c("both", "left", "right"))
  nm.lab.all = unique(comb.lab$loinc)

  comb.lab=comb.lab[which(comb.lab$days_since_admission%in%c(0:30)),]
  err.report=NULL
  for(nm.lab in nm.lab.all){
    tmp=comb.lab[comb.lab$loinc%in%nm.lab,c("days_since_admission","mean_log_value_all")]
    tmp$mean_log_value_all[tmp$mean_log_value_all%in%c(-99,-999, -Inf, Inf)]=NA
    tmp=tmp[which(is.na(tmp$mean_log_value_all)!=1),]
    if(dim(tmp)[1]>=5){
    tmp.range=lab.range[,c(1,which(grepl(gsub("-",".",nm.lab),colnames(lab.range))==1))]
    colnames(tmp.range)[2:3]=c("LB", "UB")
    tmp=left_join(tmp, tmp.range, by="days_since_admission")
    err.tmp=1*(sum(tmp$mean_log_value_all<tmp$LB)>(0.9*dim(tmp)[1])|sum(tmp$mean_log_value_all>tmp$UB)>(0.9*dim(tmp)[1]))
    err.tmp=c(site.nm, err.tmp,paste0("lab unit issue for ", nm.lab))
    err.report=rbind(err.report, err.tmp)}
  }
  if(is.null(err.report)!=1){
  err.report=data.frame(err.report)}else{err.report=data.frame(matrix(NA,1,3))}
  colnames(err.report)=c(site.nm, "err","label")
  err.report=err.report[err.report[,"err"]==1,c(site.nm, "label")]
  list(err.report=err.report)
}



