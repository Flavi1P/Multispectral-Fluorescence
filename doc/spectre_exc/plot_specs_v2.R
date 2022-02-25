rm(list=ls())

# Info on files :
# Aug 31, 2019 Orrico, Cristina <corrico@seabird.com> sent :
# Julia,
# I had to send you text files of this, my mail server won t let out a .MAT file.
# Mnbdat is spectral information; snbdat is stdev.
# Let me know if I can help with anything else.
# Cheers,
# Cris

postscript(file='plot_specs_3x1m.eps', width=8.27, height=11.69, horiz=F)
nf <- layout(matrix(1:4,2,2, byrow=T), widths=rep(8,4), heights=rep(6,4), respect=T)
par(mar=c(2,3,2,4))

# Reading specs files
bspec <- scan(file='B_mnbdat.txt', what=numeric())
bfwhm <- scan(file='B_FWHM.txt', what=numeric())
bcenter <- scan(file='B_centerwave.txt', what=numeric())
bsnbdat <- scan(file='B_snbdat.txt', what=numeric())
# --> seems all files are identical !!

vcenter <- scan(file='V_centerwave.txt', what=numeric())
vfwhm <- scan(file='V_FWHM.txt', what=numeric()) 
vspec <- scan(file='V_mnbdat.txt', what=numeric())
vsnbdat <- scan(file='V_snbdat.txt', what=numeric()) 

gcenter <- scan(file='G_centerwave.txt', what=numeric())
gwd <- scan(file='G_wid.txt', what=numeric()) 
gspec <- scan(file='G_mnbdat.txt', what=numeric())
gsnbdat <- scan(file='G_snbdat.txt', what=numeric()) 

vwav <- scan(file='V_wav.txt', what=numeric())

nvalues = 2048
bspec.x1 <- bspec[3:(nvalues+2)]
bspec.x2 <- bspec[(nvalues+3):(nvalues+2+nvalues)]
wl <- bspec[(nvalues+3+nvalues):(nvalues+2+nvalues+nvalues)]

vspec.x1 <- vspec[3:(nvalues+2)]
vspec.x2 <- vspec[(nvalues+3):(nvalues+2+nvalues)]

# Plot data B_FWHM.txt
plot(wl,bspec.x1, xlim=c(320,630), type='l', xlab='', ylab='', lwd=2, col='darkblue')
grid()
par(new=T)
plot(wl,bspec.x2, xlim=c(320,630), axes=F, xlab='', ylab='', type='l', lty=2, lwd=2, col='darkblue')
axis(4)
abline(v=bspec[2], col='red')
legend('topright', c(paste('Center =', round(bspec[2],3), 'nm', sep=' '), paste('FWHM =', round(bspec[1],3), 'nm', sep=' ')), bty='n', cex=0.7)

# Plots data V_FWHM.txt
#par(new=T)
plot(wl,vspec.x1, xlim=c(320,630), axes=T, xlab='', ylab='', type='l', lwd=2, col='turquoise')
grid()
par(new=T)
plot(wl,vspec.x2, xlim=c(320,630), axes=F, xlab='', ylab='', type='l', lty=2, lwd=2, col='turquoise')
axis(4)
abline(v=vspec[2], col='red')
legend('topright', c(paste('Center =', round(vspec[2],3), 'nm', sep=' '), paste('FWHM =', round(vspec[1],3), 'nm', sep=' ')), bty='n', cex=0.7)

# Plots data Green channels
#par(new=T)
plot(wl,gspec, xlim=c(320,630), axes=T, xlab='', ylab='', type='l', lwd=2, col='seagreen3')
grid()
par(new=T)
plot(wl,gsnbdat, xlim=c(320,630), axes=F, xlab='', ylab='', type='l', lty=2, lwd=2, col='seagreen3')
axis(4)
abline(v=gcenter, col='red')
legend('topleft', c(paste('Center =', round(gcenter,3), 'nm', sep=' '), paste('FWHM =', round(gwd,3), 'nm', sep=' ')), bty='n', cex=0.7)

graphics.off()