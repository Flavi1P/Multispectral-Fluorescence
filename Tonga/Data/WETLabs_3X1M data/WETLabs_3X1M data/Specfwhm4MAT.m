function [nbdat,smaxwave,mnbdat,snbdat,centerwave,wid]=Specfwhm4MAT(wav,dat)

[xx,yy]=size(dat);

bdat=dat;
nbdat=dat;

for i=1:xx
  basel=mean(dat(i,1634:1970));  %use 900-1000nm average as baseline value
  bdat(i,:)=dat(i,:)-basel;
  maxval=max(bdat(i,:));
  hh=find(bdat(i,:) == maxval);
  %smaxwave(i)=wav(hh);
  nbdat(i,:)=bdat(i,:)./maxval;
end

mnbdat=mean(nbdat);
snbdat=std(nbdat);
mmaxval=max(mnbdat);
h2=find(mnbdat == mmaxval);
centerwave=wav(h2);

wid=fwhm(wav,mnbdat);
  
figure
plot(wav,mnbdat)
axis([350 600 0 1])
ylabel('transmission')
xlabel('wavelength(nm)')
figure
plot(wav,snbdat)
axis([350 600 0 max(snbdat)*1.1])
ylabel('std dev of transmission')
xlabel('wavelength(nm)')
  
  
