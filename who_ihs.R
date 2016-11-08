WHO_ISH_Risk<- function(age,gdr, smk, sbp, dm, chl, subregion){
#load required data
df<-data.frame(age,gdr, smk, sbp, dm, chl)
ref<-read.csv(file="WHO_ISH_Risk.csv", header = T)

#Age
df$age<-ifelse(df$age > 17 & df$age<50, 40, df$age)
df$age<-ifelse(df$age>=50 & df$age<60, 50, df$age)
df$age<-ifelse(df$age>=60 & df$age<70, 60, df$age)
df$age<-ifelse(df$age>=70, 70, df$age)

#cholesterol
df$chl<-ifelse(df$chl>0 & df$chl<4.5, 4, df$chl)
df$chl<-ifelse(df$chl>=4.5 & df$chl<5.5, 5, df$chl) 
df$chl<- ifelse(df$chl>=5.5 & df$chl<6.5, 6, df$chl)
df$chl<- ifelse(df$chl>=6.5 & df$chl<7.5, 7, df$chl) 
df$chl<-ifelse(df$chl>=7.5, 8, df$chl)

#systolic Blood Pressure
df$sbp<- ifelse(df$sbp >0 & df$sbp<140, 120, df$sbp)
df$sbp<-ifelse(df$sbp>=140 & df$sbp<160, 140, df$sbp) 
df$sbp<-ifelse(df$sbp>=160 & df$sbp<180,160, df$sbp)
df$sbp<-ifelse(df$sbp>=180, 180, df$sbp) 

#combine everything into new variable for lookup value  luv
df$luv<-paste(df$age, df$gdr, df$dm, df$smk, df$sbp, df$chl, sep= " ")

# combine same variable from the reference table ref 
ref$refv<- paste(ref$age, ref$gdr, ref$dm, ref$smk, ref$sbp, ref$chl, sep = " ")

#Match the look up value with reference value 
if (subregion == "AFR_D"){
df$AFR_D<-ref$AFR_D[match(df$luv, ref$refv)]
return(df$AFR_D)
}

if (subregion == "AFR_E"){
df$AFR_E<-ref$AFR_E[match(df$luv, ref$refv)]
return(df$AFR_E)
}

if (subregion == "AMR_A"){
df$AMR_A<-ref$AMR_A[match(df$luv, ref$refv)]
return(df$AMR_A)
}

if (subregion == "AMR_B"){
df$AMR_B<-ref$AMR_B[match(df$luv, ref$refv)]
return(df$AMR_B)
}

if (subregion == "AMR_D"){
df$AMR_D<-ref$AMR_D[match(df$luv, ref$refv)]
return(df$AMR_D)
}

if (subregion == "EMR_B"){
df$EMR_B<-ref$EMR_B[match(df$luv, ref$refv)]
return(df$EMR_B)
}

if (subregion == "EMR_D"){
df$EMR_D<-ref$EMR_D[match(df$luv, ref$refv)]
return(df$EMR_D)
}


if (subregion == "EUR_A"){
df$EUR_A<-ref$EUR_A[match(df$luv, ref$refv)]
return(df$EUR_A)
}

if (subregion == "EUR_B"){
df$EUR_B<-ref$EUR_B[match(df$luv, ref$refv)]
return(df$EUR_B)
}


if (subregion == "EUR_C"){
df$EUR_C<-ref$EUR_C[match(df$luv, ref$refv)]
return(df$EUR_C)
}

if (subregion == "SEAR_B"){
df$SEAR_B<-ref$SEAR_B[match(df$luv, ref$refv)]
return(df$SEAR_B)
}


if (subregion == "SEAR_D"){
df$SEAR_D<-ref$SEAR_D[match(df$luv, ref$refv)]
return(df$SEAR_D)
}

if (subregion == "WPR_A"){
df$WPR_A<-ref$WPR_A[match(df$luv, ref$refv)]
return(df$WPR_A)
}

}
