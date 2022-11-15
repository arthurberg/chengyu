rm(list=ls())
setwd("~/Library/Mobile Documents/com~apple~CloudDocs/courses/2022 Spring/chengyu/shiny")
load("mydict2.RData")

compare_words=function(target_str,guess_str){
  if ((nchar(target_str) != 4) | (nchar(guess_str)!=4)){
    stop("target and guess string must be length 4.")
  }
  
  target = strsplit(target_str,"")[[1]]
  guess = strsplit(guess_str,"")[[1]]
  result = character(nchar(guess_str))
  
  for(i in 1:4){
    if(guess[i]==target[i]){
      result[i]="correct"
      target[i]=""
      guess[i]=""
    }
  }
  for(i in 1:4){
    if(guess[i]!=""){
      if(is.element(guess[i],target)){
        result[i]="in-word"
        target[which(target==guess[i])[1]]=""
        next
      }
      result[i]="not-in-word"
    }
  }
  result
}

check_words = function(target_str,guess_str){
  compare_result = compare_words(target_str,guess_str)
  result = "keep guessing"
  if ( all(compare_result == "correct")){
    result = "solved"
  }
  result
}



###### FUNCTIONS

f.init1 = function(word_num,score,words){"1234"}
f.init2 = function(word_num,score,words){"1324"}
f.init3 = function(word_num,score,words){"1243"}
f.init4 = function(word_num,score,words){"1233"}
f.init5 = function(word_num,score,words){"1142"}


f1 = function(word_num,scores.current,words,guess_number,myguesses,myfeedback){

  correct=rep(as.character(NA),4)
  chars = NA
  
  mymat=matrix("maybe",nrow=5,ncol=4)
  rownames(mymat)=as.character(1:5)
  colnames(mymat)=as.character(1:4)
  
  for(i in 1:guess_number){
    nums=strsplit(myguesses[i],"")[[1]]
    pos = myfeedback[i,]
    
    cor=which(pos=="correct")
    iw=which(pos=="in-word")
    
    niw=setdiff(unique(nums[pos=="not-in-word"]),c(nums[cor],nums[iw]))
    niw2=intersect(c(nums[cor],nums[iw]),unique(nums[pos=="not-in-word"]))
    
    if(length(niw2)>0){
      for(j in 1:length(niw2)){
        mymat[niw2[j],which(pos=="not-in-word" & nums==niw2[j])[1]]="no"
      }
    }
    
    if(length(niw)>0){
      for(j in 1:length(niw)){
        mymat[niw[j],]="no"
      }
    }
    
    
    
    if(length(cor)>0){
      correct[cor]=nums[cor]
      if(sum(is.na(chars))>0){chars=nums[cor]} else{chars=append(chars,nums[cor])}
      for(j in 1:length(cor)){
        mymat[,cor[j]]="no"
        mymat[nums[cor[j]],cor[j]]="yes"
      }
    }
    
    
    
    if(length(iw)>0){
      if(sum(is.na(chars))>0){chars=nums[iw]} else{chars=append(chars,nums[iw])}
      
      for(j in 1:length(iw)){
        mymat[nums[iw[j]],iw[j]]="no"
      }
    }    
    
    chars=setdiff(chars,correct)
    
  } #end of i loop 
  
  

  chars.og=chars
  mymat.og=mymat
  done=FALSE
  
  while(!done){
  mymat=mymat.og
  chars=chars.og
  out=correct
  ind=which(is.na(correct))
  
  if(length(ind)>0){
    for(j in 1:length(ind)){
      
      if(length(chars)==0){
        mypos=which(is.na(out))[1]
        mynum=as.character(which(mymat[,mypos]=="maybe"))[1]
        out[mypos]=mynum
        mymat[,mypos]="X"
        }
      
      if(length(chars)>0){
        n.maybe=sum(mymat[chars[1],]=="maybe")
        if(n.maybe>0){
        mypos=sample(as.character(which(mymat[chars[1],]=="maybe")),1)
        out[as.numeric(mypos)]=chars[1]
        mymat[,as.numeric(mypos)]="X"
        chars=setdiff(chars,chars[1])
        }
      }
      
    }
  }

  if(sum(!is.na(out))==4){done=TRUE}
  } #end of while  

paste(out,collapse="")
  
}

f2=f3=f4=f5=f1

##################



myseed=31428212 # seed will be changed for competition
set.seed(myseed)
n=10
comp_words=mydict[sample(1:dim(mydict)[1],n,prob=mydict$freq,replace=FALSE),"sig"]
guesses=array(as.character(NA),dim=c(n,10,5))
feedback=array(as.character(NA),dim=c(n,10,4,5))
scores=matrix(0,nrow=n,ncol=5)

for(word_num in 1:n){
  words=if(word_num==1){NA} else{words=comp_words[1:(word_num-1)]}
  
  for(i in 1:5){
    guess_number=1
    f.init = get(paste0("f.init",i))
    guess = f.init(word_num,score,words)
    guesses[word_num,1,i]=guess
    res=check_words(comp_words[word_num],guess)
    feedback[word_num,guess_number,,i]=compare_words(comp_words[word_num],guess)
    
    while(res=="keep guessing" & guess_number<=9){
      scores.current=apply(matrix(scores[1:word_num,],ncol=5),2,sum)
      myguesses=guesses[word_num,1:guess_number,i]
      myfeedback=matrix(feedback[word_num,1:guess_number,,i],nrow=guess_number,ncol=4)
      f = get(paste0("f",i))
      guess=f(word_num,scores.current,words,guess_number,myguesses,myfeedback)
      guess_number=guess_number+1
      guesses[word_num,guess_number,i]=guess
      res=check_words(comp_words[word_num],guess)
      feedback[word_num,guess_number,,i]=compare_words(comp_words[word_num],guess)
      res
    }
    
    scores[word_num,i]=guess_number
  }
  
}

apply(scores,2,mean)
