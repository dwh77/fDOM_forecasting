# Watras, C. J., Morrison, K. A., Rubsam, J. L., Hanson, P. C., Watras, A. J., LaLiberte, G. D., & Milewski, P. (2017). 
# A temperature compensation method for chlorophyll and phycocyanin fluorescence sensors in freshwater. 
# Limnology and Oceanography: Methods, 15(7), 642-652.

# This script uses the temperature compensation equation from Watras et al
#    to test the sensitivity of the correction to the parameter, rho.
#    Data (T and corresponding Fm) were picked from Fig. 1A, and multiple
#    values for rho are tested and plotted.

rhos = c(-0.008,-0.010,-0.012) # ranges from about -0.008 to -0.012
Tr = 20 # reference T, 20C

# data estimated from Watras et al. 2018, Fig. 1A
Tm = c(5,10,15, 20,25,30,35)
Fm = c(1275,1225,1150,1100,1050,1000,950) # these are RFU values for the same [chl] across different Ts

myCols = c('red','blue','green')

# Plot original data
plot(Tm,Fm,type='b',ylim=c(900,1300),ylab='Fm, Fr')

for (i in 1:length(rhos)){
  rho = rhos[i]
  # Temperature correction
  Fr = Fm/(1+rho*(Tm-Tr))
  # Plot it
  lines(Tm,Fr,type='b',col = myCols[i])
}
legendTxt = c('Fm',as.character(rhos))
legend('topright',legendTxt,lty=rep(1,length(rhos)+1),col=c('black',myCols))



