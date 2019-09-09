
include_months <- function(quarter){
  
  if(quarter == "1"){
    c(4:6)
  }else{
    if(quarter == "2"){
      c(4:9)
    }else{
      if(quarter == "3"){
        c(4:12)
      }else{
        if(quarter == "4"){
          c(1:12)
        }
      }
    }
  }
  
}

