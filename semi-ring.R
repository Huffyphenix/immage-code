f<-function(name, value){
  xsize=200
  plot(0, 0, xlab = "", ylab = "", axes = FALSE, xlim = c(-xsize,xsize), ylim = c(-xsize, xsize))
  for(i in 1:length(name)){
    info=name[i]
    percent=value[i]
    k=1:percent/100
    r=xsize*(length(name)-i+1)/length(name)
    x=r*sin(k*2*pi)
    y=r*cos(k*2*pi)
    text(-18,r,info,pos=2,cex=1,col = cols[i])
    text(-9,r,paste(percent,"%"),cex=1,col = cols[i])
    lines(x,y,col=cols[i],lwd=(length(name)-i+1)*1.5)
  }
}
country<-c("a","b","c","d","e","f","g","h")
percent<-c(10,50,40,30,90,20,30,80)
d<-data.frame(country,percent)
cols<-rainbow(length(country))
f(country,sort(percent,decreasing=T))
