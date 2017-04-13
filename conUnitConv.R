concUnitConv<-function(from,to, MW=0) {
  #from<-"mg/L" ;  to<-"mmol/L" ;  MW <-180.164
  
  a.mass<-strsplit(from,"/")[[1]][1]
  a.vol<-strsplit(from,"/")[[1]][2]
  b.mass<-strsplit(to,"/")[[1]][1]
  b.vol<-strsplit(to,"/")[[1]][2]
  
  if (a.mass == "g") {a.mass <- 1}
  else if (a.mass == "mg") {a.mass <- 0.001}
  else if (a.mass == "ug") {a.mass <- 0.000001}
  else if (a.mass == "ng") {a.mass <- 0.000000001}
  else if (a.mass == "pg") {a.mass <- 0.000000000001}
  else if (a.mass == "mmol") {
    if ( MW <= 0 ) { stop("Positive molecular weight should be given")}
    else {
      a.mass <- 0.001 * MW
    }
      
  }
  else {stop("111Source amount is not supported")}
  
  if (a.vol == "mL") {a.vol <- 1}
  else if (a.vol == "L") {a.vol <-1000}
  else {stop("Volume unit is not supported")}
  
  if (b.mass == "g") {b.mass <- 1}
  else if (b.mass == "mg") {b.mass <- 0.001}
  else if (b.mass == "ug") {b.mass <- 0.000001}
  else if (b.mass == "ng") {b.mass <- 0.000000001}
  else if (b.mass == "pg") {b.mass <- 0.000000000001}
  else if (b.mass == "mmol") {
    if ( MW <= 0 ) {stop("Positive molecular weight should be given")}
    else {
      b.mass <- 0.001 * MW
    }
    
  }
  else {stop("Target concentration unit is not valid.")}  
  
  if (b.vol == "mL") {b.vol <- 1}
  else if (b.vol == "L") {b.vol <-1000}
  else {stop("Volume unit is not supported")} 
  
 
  
  
  con <- (a.mass / b.mass) * ( b.vol / a.vol)
  
  return(con)
}

Theoph$conc * concUnitConv("mg/L", "ug/L")
Theoph$conc * concUnitConv("mg/L", "mg/mL")
Theoph$conc * concUnitConv("mg/L", "mmol/L") # Wrong input
Theoph$conc * concUnitConv("mg/L", "mmol/L", MW=-100) # Wrong input
Theoph$conc * concUnitConv("mg/L", "mM", MW=180.164) # Wrong input
Theoph$conc * concUnitConv("mg/L", "mmol/L", MW=180.164)
Theoph$mM = Theoph$conc * concUnitConv("mg/L", "mmol/L", MW=180.164)
Theoph$mM * concUnitConv("mmol/L", "ug/L", MW=180.164)
Theoph$mM * concUnitConv("mmol/L", "ug/mL", MW=180.164)

