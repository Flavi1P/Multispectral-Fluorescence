function [wavv,outt]=PullOceanOptics4MAT(instru)
  
  ppp=dir(instru);
  nfil=length(ppp);
  j=1;
 for i=7:nfil%3:nfil 
    fnam=ppp(i).name;
    [wavv,dat1]=ReadOceanOpticsData(fnam);
    plot(wavv,dat1)
    keyp=input('Keep this one =1; discard = 0: ');
    if keyp==1
      outt(j,:)=dat1;
      j=j+1;
     end
  end
      
        
        
      