complete<-function(directory, id = 1:332) {
    files <- list.files(path = paste0(directory,"/"));
    f<-list() ; total<-data.frame();
    for(i in id) {
          f[[i]]<-data.frame() ;
          f[[i]]<-read.csv(paste0(directory,"/", files[i])) ;
          data<-na.omit(f[[i]]) ;
          s<-data.frame("id"=i,"nobs"=nrow(data))
          total<-rbind(total,s)
    }
    print(total)
}

