l = read.csv("section_c.csv")
p<-0
a<-0
b <- c()
for(j in 1:49){
  for(i in l[j,1:ncol(l)-1]){
    if (i=="p") { p <- (p+1) 
    }else if (i=="a") { a <-(a+1)
      }
    }
  b[j]=p*100/(a+p)
  #print(a+p)
  a<-0
  p<-0
}
d <- data.frame(l$reg_no,l$name,b,l$Q1)
colnames(d)<- c("Roll_Number","Name","Attandence_percentage","Quiz_marks_12")
colnames(d)<- c("Roll Number","Name","Attandence percentage","Quiz marks (out of 12)")
write.csv(d,"att.csv")
