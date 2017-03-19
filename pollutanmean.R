pollutanmean<-function(directory,pollutant,id=1:332) {
    files <- list.files(path = paste0(directory,"/"));
    f<-list() ; total = 0 ;  observations = 0
    for(i in id) {
          f[[i]]<-data.frame() ;
          f[[i]]<-read.csv(paste0(directory,"/", files[i])) ;
          if(pollutant=="sulfate") { 
                      data<-f[[i]]$sulfate ;
                      total = total + sum(data[!is.na(data)]) ;
                      observations = observations + length(data[!is.na(data)])
                     }
          else { 
                      data<-f[[i]]$nitrate ;
                      total = total + sum(data[!is.na(data)]) ;
                      observations = observations + length(data[!is.na(data)])
                 }
                  
          
    }
    print(total/observations)
}
    
