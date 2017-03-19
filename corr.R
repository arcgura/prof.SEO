corr<-function(directory,threshold=0) {
    files <- list.files(path = paste0(directory,"/"));
    f<-list() ; co<-vector()
    for(i in 1:length(files)) {
          f[[i]]<-data.frame() ;
          f[[i]]<-read.csv(paste0(directory,"/", files[i])) ;
          f[[i]]<-na.omit(f[[i]]) ;
          if(nrow(f[[i]])>threshold)
               {co[i]<-cor(f[[i]]$nitrate, f[[i]]$sulfate)}
     }   

   cr<-co[!is.na(co)]
 }
          
