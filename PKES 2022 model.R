# A Monetary Circuit SFC Model
#
# Author: Marco Veronese Passarella
#
# Last change: 08/03/2022
#
# Note: this R code reproduces the experiments discussed in: Veronese Passarella,
# M. (2022) "It is not la vie en rose. New insights from Graziani's theory of
# monetary circuit", PKES Working Paper No. 2209, March 2022.
#
# Errata corrige:
# alpha0w = 23 (instead of 5) under the baseline scenario; 
# alpha0w = 24 (instead of 7) under scenario 1;
# alpha0z = 2.5 (instead of 5) under the baseline scenario; 
# alpha0z = 3.5 (instead of 7) under scenario 1.
# 
# The above changes are necessary to keep vz from falling below 0 under the
# steady state, while matching Model BMW baseline values. Notice that qualitative
# results are unaffected. 


################################################################################

#Clear Environment
rm(list=ls(all=TRUE))

#Clear Plots
if(!is.null(dev.list())) dev.off()

#Clear Console
cat("\014")

#Import steady-state values
data00 <- read.csv("https://www.dropbox.com/s/u4mmt2v6b29h68o/data00.csv?dl=1") 

#Select type of investment function 
inv_opt = 0                                            #Note: 0 = standard SFC; 1 = circuit 

#Number of periods
nPeriods = 200

#Number of scenarios
nScenarios=6 

#Set parameters and exogenous variables 
delta=0.1                                              #Depreciation rate of capital stock
pr=1                                                   #Labor productivity
r_bar=0.04                                             #Policy rate
w=0.72                                                 #Wage rate 
alpha1w=0.757621                                       #Propensity to consume out of wages
alpha2w=0.1                                            #Propensity to consume out of workers' wealth
alpha1z=0.69                                           #Propensity to consume out of profits 
alpha2z=0.1                                            #Propensity to consume out of capitalists' wealth
lambdaw=0.5                                            #Workers' share of deposits to total wealth (liquidity preference)  
lambdaz=0.5                                            #Capitalists' share of deposits to total wealth (liquidity preference)  
gamma=0.15                                             #Speed of adjustment of current investment to target level
psi0=0                                                 #Coefficient of adaptive expectations: fixed
psi1=0.1                                               #Coefficient of adaptive expectations: correction

#Set values of coefficients that are shocked
alpha0w=matrix(data=23,nrow=nScenarios,ncol=nPeriods)               #Autonomous consumption of workers
alpha0z=matrix(data=2.5,nrow=nScenarios,ncol=nPeriods)              #Autonomous consumption of capitalists
beta=matrix(data=0.10,nrow=nScenarios,ncol=nPeriods)                #Target investment share to total income
sigma=matrix(data=1,nrow=nScenarios,ncol=nPeriods)                  #Coefficient of real supply function (note: 1 = full adjustment to demand; 0 = fully exogenous)
iota=matrix(data=1,nrow=nScenarios,ncol=nPeriods)                   #Coefficient of additional interest payments (0 = no extra payments; 1 = extra payments)
kappa=matrix(data=1,nrow=nScenarios,ncol=nPeriods)                  #Target capital to output ratio
par_id=matrix(data=1,nrow=nScenarios,ncol=nPeriods)                 #Coefficient defining type of investment (1 = endogenous; 0 = exogenous)
alpha3=matrix(data=0,nrow=nScenarios,ncol=nPeriods)                 #Exogenous investment
gammay=matrix(data=0.15,nrow=nScenarios,ncol=nPeriods)              #Speed of adjustment of potential output to current one  

#Define and set initial value of variables
y_n=matrix(data=data00$y_r_0,nrow=nScenarios,ncol=nPeriods)         #Real output: exogenous supply
p=matrix(data=1,nrow=nScenarios,ncol=nPeriods)                      #Unit price

#Define variables
af=matrix(data=data00$af_0,nrow=nScenarios,ncol=nPeriods)           #Amortization funds
c=matrix(data=data00$c_0,nrow=nScenarios,ncol=nPeriods)             #Total demand of consumption goods
cw=matrix(data=data00$cw_0,nrow=nScenarios,ncol=nPeriods)           #Consumption goods demanded by workers
cz=matrix(data=data00$cz_0,nrow=nScenarios,ncol=nPeriods)           #Consumption goods demanded by capitalists
da=matrix(data=data00$da_0,nrow=nScenarios,ncol=nPeriods)           #Depreciation allowances (real)
k=matrix(data=data00$k_0,nrow=nScenarios,ncol=nPeriods)             #Actual stock of capital
kt=matrix(data=data00$k_0,nrow=nScenarios,ncol=nPeriods)            #Target stock of capital
ld=matrix(data=data00$ld_0,nrow=nScenarios,ncol=nPeriods)           #Demand for bank loans 
ls=matrix(data=data00$ls_0,nrow=nScenarios,ncol=nPeriods)           #Supply of bank loans 
id=matrix(data=data00$id_0,nrow=nScenarios,ncol=nPeriods)           #Demand for investment
ms=matrix(data=data00$ms_0,nrow=nScenarios,ncol=nPeriods)           #Supply of bank deposits
n=matrix(data=data00$n_0,nrow=nScenarios,ncol=nPeriods)             #Employed workers (direct labor time)
wb=matrix(data=data00$wb_0,nrow=nScenarios,ncol=nPeriods)           #Wage bill 
y=matrix(data=data00$y_0,nrow=nScenarios,ncol=nPeriods)             #Total income
ydw=matrix(data=data00$ydw_0,nrow=nScenarios,ncol=nPeriods)         #Disposal income of workers
ydz=matrix(data=data00$ydz_0,nrow=nScenarios,ncol=nPeriods)         #Disposal income of capitalists
yd=matrix(data=data00$yd_0,nrow=nScenarios,ncol=nPeriods)           #Total disposal income of households
fin_i=matrix(data=data00$fin_i_0,nrow=nScenarios,ncol=nPeriods)     #Initial finance to production
fin_f=matrix(data=data00$fin_f_0,nrow=nScenarios,ncol=nPeriods)     #Final finance
bs=matrix(data=data00$bs_0,nrow=nScenarios,ncol=nPeriods)           #Supply for private securities
rb=matrix(data=r_bar,nrow=nScenarios,ncol=nPeriods)                 #Rate of return on private securities
rl=matrix(data=r_bar,nrow=nScenarios,ncol=nPeriods)                 #Rate of interest on banks loans
rm=matrix(data=r_bar,nrow=nScenarios,ncol=nPeriods)                 #Rate of interest on bank deposits
vw=matrix(data=data00$vw_0,nrow=nScenarios,ncol=nPeriods)           #Wealth of workers
bw=matrix(data=data00$bw_0,nrow=nScenarios,ncol=nPeriods)           #Demand for private securities by workers
mw=matrix(data=data00$mw_0,nrow=nScenarios,ncol=nPeriods)           #Bank deposits held by workers by workers
vz=matrix(data=data00$vz_0,nrow=nScenarios,ncol=nPeriods)           #Wealth of capitalists
bz=matrix(data=data00$bz_0,nrow=nScenarios,ncol=nPeriods)           #Demand for private securities by capitalists
mz=matrix(data=data00$mz_0,nrow=nScenarios,ncol=nPeriods)           #Bank deposits held by workers by capitalists
vh=matrix(data=data00$vh_0,nrow=nScenarios,ncol=nPeriods)           #Wealth of households
bh=matrix(data=data00$bh_0,nrow=nScenarios,ncol=nPeriods)           #Total demand for private securities
mh=matrix(data=data00$mh_0,nrow=nScenarios,ncol=nPeriods)           #Total bank deposits held by workers
idt=matrix(data=data00$idt_0,nrow=nScenarios,ncol=nPeriods)         #Target investment
p_e=matrix(data=data00$p_e_0,nrow=nScenarios,ncol=nPeriods)         #Expected price
paym_l=matrix(data=data00$paym_l_0,nrow=nScenarios,ncol=nPeriods)   #Interest payments on loans
paymw_m=matrix(data=data00$paymw_m_0,nrow=nScenarios,ncol=nPeriods) #Interest payments on deposits paid to workers
paymz_m=matrix(data=data00$paymz_m_0,nrow=nScenarios,ncol=nPeriods) #Interest payments on deposits paid to capitalists
paym_m=matrix(data=data00$paym_m_0,nrow=nScenarios,ncol=nPeriods)   #Total interest payments on deposits
paymw_b=matrix(data=data00$paymw_b_0,nrow=nScenarios,ncol=nPeriods) #Interest payments on private securities paid to workers
paymz_b=matrix(data=data00$paymz_b_0,nrow=nScenarios,ncol=nPeriods) #Interest payments on private securities paid to capitalists
paym_b=matrix(data=data00$paym_m_0,nrow=nScenarios,ncol=nPeriods)   #Total interest payments on securities
pb=matrix(data=data00$pb_0,nrow=nScenarios,ncol=nPeriods)           #Bank profit
cw_r=matrix(data=data00$cw_r_0,nrow=nScenarios,ncol=nPeriods)       #Real consumption of workers
cw_r_t=matrix(data=data00$cw_r_t_0,nrow=nScenarios,ncol=nPeriods)   #Expected real consumption of workers
cz_r=matrix(data=data00$cz_r_0,nrow=nScenarios,ncol=nPeriods)       #Real consumption of capitalists
c_r=matrix(data=data00$c_r_0,nrow=nScenarios,ncol=nPeriods)         #Real consumption of households
c_r_t=matrix(data=cw_r_t+cz_r,nrow=nScenarios,ncol=nPeriods)        #Total expected real consumption 
id_r=matrix(data=data00$id_r_0,nrow=nScenarios,ncol=nPeriods)       #Real investment
y_r=matrix(data=data00$y_r_0,nrow=nScenarios,ncol=nPeriods)         #Real output
y_s=matrix(data=data00$y_s_0,nrow=nScenarios,ncol=nPeriods)         #Real output: actual supply
y_g=matrix(data=data00$y_g_0,nrow=nScenarios,ncol=nPeriods)         #Real output gap
pf=matrix(data=data00$pf_0,nrow=nScenarios,ncol=nPeriods)           #Profit of firms
slt=matrix(data=data00$slt_0,nrow=nScenarios,ncol=nPeriods)         #Surplus labor
nlt=matrix(data=data00$nlt_0,nrow=nScenarios,ncol=nPeriods)         #Necessary labor time
expl=matrix(data=data00$expl_0,nrow=nScenarios,ncol=nPeriods)       #Exploitation rate

#Begin the model

#Choose scenario
for (j in 1:nScenarios){
  
  #Define time loop
  for (i in 2:nPeriods){
    
    #Define iterations
    for (iterations in 1:200){
      
      #Shock to autonomous consumption
      if (i>=10 && j==2){alpha0w[j,i]=24
      alpha0z[j,i]=3.5}
      
      #Shock to investment (quantity adjustment)
      if (i>=10 && j==3){alpha3[j,i]=25
      par_id[j,i]=0}            
      
      #Shock to investment (mixed adjustment)
      if (i>=10 && j==4){alpha3[j,i]=25
      par_id[j,i]=0
      sigma[j,i]=0}
      
      #Shock to investment (price adjustment)
      if (i>=10 && j==5){alpha3[j,i]=25
      par_id[j,i]=0
      sigma[j,i]=0
      gammay[j,i]=0
      }
      
      #Different baseline when interest payments are calculated in standard way
      if (j==6){iota[j,i]=0}
      
      
      #Model equations     
      
      #Households: workers and capitalists
      ydw[j,i] = wb[j,i] + paymw_m[j,i] + paymw_b[j,i]                         #Disposable income of workers
      ydz[j,i] =  pb[j,i] + pf[j,i] + paymz_m[j,i] + paymz_b[j,i]              #Disposable income of capitalists
      yd[j,i] =  ydw[j,i] + ydz[j,i]                                           #Total disposable income of households
      vw[j,i] = vw[j,i-1] + ydw[j,i] - cw[j,i]                                 #Stock of wealth of workers
      vz[j,i] = vz[j,i-1] + ydz[j,i] - cz[j,i]                                 #Stock of wealth of capitalists
      vh[j,i] = vw[j,i-1] + vz[j,i]                                            #Total stock of wealth of workers
      bw[j,i] = (1-lambdaw)*vw[j,i]                                            #Demanded stock of private securities by workers
      bz[j,i] = (1-lambdaz)*vz[j,i]                                            #Demanded stock of private securities by capitalists
      bh[j,i] = bw[j,i] + bz[j,i]                                              #Total demanded stock of private securities
      mw[j,i] = vw[j,i] - bw[j,i]                                              #Demanded stock of deposits (hoarding) by workers 
      mz[j,i] = vz[j,i] - bz[j,i]                                              #Demanded stock of deposits (hoarding) by capitalists 
      mh[j,i] = mw[j,i] + mz[j,i]                                              #Total demanded stock of deposits (hoarding)
      cw_r_t[j,i] = alpha0w[j,i] + alpha1w*ydw[j,i-1]/p_e[j,i] + alpha2w*vw[j,i-1]/p_e[j,i] #Expected real consumption of workers
      c_r_t[j,i] = cw_r_t[j,i] + cz_r[j,i]                                     #Total expected real consumption 
      if (sigma[j,i]==1){cw_r[j,i] = cw_r_t[j,i]}                                           #Real consumption (fixed price) of workers
      else{cw_r[j,i] = y_r[j,i] - cz_r[j,i] - id_r[j,i]}                                    #Real consumption (market-clearing price) of workers
      cz_r[j,i] = alpha0z[j,i] + alpha1z*ydz[j,i-1]/p_e[j,i] + alpha2z*vz[j,i-1]/p_e[j,i]   #Real consumption of capitalists
      c_r[j,i] = cw_r[j,i] + cz_r[j,i]                                         #Total real consumption of households
      cw[j,i] = cw_r[j,i]*p[j,i]                                               #Nominal consumption by workers
      cz[j,i] = cz_r[j,i]*p[j,i]                                               #Nominal consumption by capitalists
      c[j,i] = cw[j,i] + cz[j,i]                                               #Total nominal consumption 
      
      #Firms
      if (sigma[j,i]==1){y_r[j,i] = c_r[j,i] + id_r[j,i]}                      #Real output: expenditure approach
      else{y_r[j,i] = y_s[j,i]}
      y[j,i] = c[j,i] + id[j,i]                                                #Nominal demand
      if (inv_opt==0){
        
        #Standard SFC investment function
        kt[j,i] = kappa[j,i]*y_r[j,i-1]                                        #Target capital stock
        id_r[j,i] = par_id[j,i]*(gamma*(kt[j,i] - k[j,i-1]) + da[j,i]/p[j,i]) + (1-par_id[j,i])*alpha3[j,i] } #Real gross current investment (including capital depreciation)
      
      else{  
        #Circuit-based investment function
        idt[j,i] = beta[j,i]*y_r[j,i]                                          #Real target investment
        id_r[j,i] = par_id[j,i]*(id_r[j,i-1] + gamma*(idt[j,i] - id_r[j,i-1])) + (1-par_id[j,i])*alpha3[j,i] } #Real gross current investment (without capital depreciation)
      
      id[j,i] = id_r[j,i]*p[j,i]                                               #Nominal gross current investment 
      da[j,i] = delta*k[j,i-1]                                                 #Real capital depreciation
      af[j,i] = da[j,i]*p[j,i]                                                 #Nominal amortization funds (retained profits)
      k[j,i] = k[j,i-1] + id_r[j,i] - da[j,i]                                  #Real capital stock
      pf[j,i] = y[j,i] - paym_l[j,i] - af[j,i] - paymz_b[j,i] - paymw_b[j,i] - wb[j,i]         #Total profit
      bs[j,i] = bh[j,i]                                                        #Stock of securities issued by firms
      rb[j,i] = r_bar                                                          #Rate of interest on private securities
      
      #Market-clearing price and supply adjusting to demand
      y_s[j,i] = sigma[j,i]*y_r[j,i] + (1-sigma[j,i])*y_n[j,i]                 #Real supply
      p[j,i] = (c_r_t[j,i]*p_e[j,i])/c_r[j,i]                                  #Unit price of output
      y_n[j,i] = y_n[j,i-1] + gammay[j,i]*(y[j,i-1] - y_n[j,i-1])              #Potential output (endogenous)
      y_g[j,i] = y_s[j,i-1] - y_n[j,i]                                         #Output gap
      
      #Banks, initial finance and final finance (funding)
      fin_i[j,i] = y[j,i]                                                      #Initial finance to production
      fin_f[j,i] = c[j,i] + af[j,i] + (bs[j,i] - bs[j,i-1])                    #Final finance obtained by firms
      ld[j,i] = ld[j,i-1] + fin_i[j,i] - fin_f[j,i]                            #Stock of debt (bank loans) of firms at the end of the period
      ls[j,i] = ls[j,i-1] + (ld[j,i] - ld[j,i-1])                              #Supply of bank loans
      ms[j,i] = ms[j,i-1] + (ls[j,i] - ls[j,i-1])                              #Supply of bank deposits    
      pb[j,i] = paym_l[j,i] - paymw_m[j,i] - paymz_m[j,i]                      #Bank profit
      rm[j,i] = r_bar                                                          #Rate of interest on deposits
      rl[j,i] = r_bar                                                          #Rate of interest on bank loans
      
      #Employment and wages
      wb[j,i] = w*n[j,i]                                                       #Wage bill  
      n[j,i] = y_r[j,i]/pr                                                     #Labor demand
      
      #Circuit-based interest payments 
      paym_l[j,i] = rl[j,i-1]*ld[j,i-1] + iota[j,i]*rl[j,i-1]*fin_f[j,i-1]/2   #Interest payments on bank loans (including average interest payments on repaid share of loans)
      paymw_m[j,i] = rm[j,i-1]*mw[j,i-1] + iota[j,i]*(mw[j,i-1]/mh[j,i-1])*rm[j,i-1]*fin_f[j,i-1]/2  #Interest payments on bank deposits (see above) to workers
      paymz_m[j,i] = rm[j,i-1]*mz[j,i-1] + iota[j,i]*(mz[j,i-1]/mh[j,i-1])*rm[j,i-1]*fin_f[j,i-1]/2  #Interest payments on bank deposits (see above) to capitalists
      paym_m[j,i] = paymw_m[j,i] + paymz_m[j,i]                                #Total interest payments on bank deposits
      paymw_b[j,i] = rb[j,i-1]*bw[j,i-1]                                       #Interest payments on private securities to workers
      paymz_b[j,i] = rb[j,i-1]*bz[j,i-1]                                       #Interest payments on private securities to capitalists
      paym_b[j,i] = paymw_b[j,i] + paymz_b[j,i]                                #Total interest payments on private securities
      
      #Labor-value categories
      nlt[j,i] = cw_r[j,i]/pr                                                  #Necessary labor time
      slt[j,i] = (cz_r[j,i]+id_r[j,i])/pr                                      #Surplus labor time
      expl[j,i] = slt[j,i]/nlt[j,i]                                            #Exploitation rate
      
      #Expectations
      if(i<=3){p_e[j,i] = 1}
      else{p_e[j,i] = p[j,i-1] + psi0 + psi1*(p[j,i-1] - p_e[j,i-1])}          #Expected price (adaptive)
      
      
    }
  }
}

################################################################################
#Figure 2 - Baseline

layout(matrix(c(1,2,3,4), 2, 2, byrow = TRUE))
par(mar = c(5, 4, 4, 4) + 0.3)  

#Figure 2a
plot(y_r[2,2:100],type="l",col=1,lwd=2,lty=1,font.main=1.5,cex.main=1.5,cex.axis=1.5,cex.lab=1.5,main="(a) Production, consumption, and investment",ylab = '$ (const. p)',xlab = '',ylim=range(min(id[2,2:100]),max(y_r[2,2:100])))
abline(h=data00$y_r_0,col=1,lwd=1,lty=2)
lines(c_r[2,2:100],type="l",col=2,lwd=2,lty=1)
abline(h=data00$c_r_0,col=2,lwd=1,lty=2)
lines(id_r[2,2:100],type="l",col=3,lwd=2,lty=1)
abline(h=data00$id_r_0,col=3,lwd=1,lty=2)
legend("right",c("Real output","Real consumption","Real investment"),  bty = "n", cex = 1.5, lty=c(1,1,1), lwd=c(2,2,2,1), col = c(1,2,3), box.lwd=0)

#Figure 2b
plot(fin_i[2,2:100],type="l",col=4,lwd=2,lty=1,font.main=1.5,cex.main=1.5,cex.axis=1.5,cex.lab=1.5,main="(b) The monetary circuit",ylab = '$ (flow)',xlab = '',ylim=range(min(fin_i[2,2:100]),max(fin_i[2,2:100])))
abline(h=data00$fin_i_0,col=8,lwd=1,lty=2)
lines(fin_f[2,2:100],type="l",col=5,lwd=2,lty=2)
par(new = TRUE)
plot(ld[2,2:100],type="l",col=8,lwd=2,lty=1,axes = FALSE, bty = "n", xlab = "", ylab = "",ylim=range(min(ld[2,2:100]),max(ld[2,2:100])))
axis(4)
mtext("$ (stock)", side=4, line=3)
legend("bottomright",c("Initial finance","Final finance","Outstanding debt (right axis)"),  bty = "n", cex = 1.5, lty=c(1,2,1), lwd=c(2,2,2,1), col = c(4,5,8), box.lwd=0)

#Figure 2c
plot(p[1,2:100],type="l",col=2,lwd=2,lty=1,font.main=1.5,cex.main=1.5,cex.axis=1.5,cex.lab=1.5,main="(c) Price level",ylab = '$',xlab = '',ylim=range(0.97,1.03))
legend("bottomright",c("Unit price of output"),  bty = "n", cex = 1.5, lty=c(1), lwd=c(2), col = c(2), box.lwd=0)

#Figure 2d
plot(mh[1,2:100]-ms[1,2:100],type="l",col="#009999",lwd=2,lty=1,font.main=1.5,cex.main=1.5,cex.axis=1.5,cex.lab=1.5,main="(d) Redundant equation",ylab = '$',xlab = '',ylim=range(-1,1))
legend("bottomright",c("Supply of deposits - \n demand for deposits"),  bty = "n", cex = 1.5, lty=c(1), lwd=c(2), col = c("#009999"), box.lwd=0)

################################################################################
#Figure 3 - Shock to investment (quantity adjustment)

layout(matrix(c(1,2,3,4), 2, 2, byrow = TRUE))
par(mar = c(5, 4, 4, 4) + 0.3)  

#Figure 3a
plot(100*y_r[3,2:100]/y_r[1,2:100],type="l",col=1,lwd=2,lty=1,font.main=1.5,cex.main=1.5,cex.axis=1.5,cex.lab=1.5,main="(a) Production, consumption, and investment (real)",ylab = 'Index',xlab = '',ylim=range(96,126))
lines(100*cw_r[3,2:100]/cw_r[1,2:100],type="l",col=2,lwd=2,lty=1)
lines(100*cz_r[3,2:100]/cz_r[1,2:100],type="l",col=3,lwd=2,lty=1)
lines(100*id_r[3,2:100]/id_r[1,2:100],type="l",col=4,lwd=2,lty=1)
legend("topright",c("Output","Workers' consumption","Capitalists' consumption","Real investment"),  bty = "n", cex = 1.5, lty=c(1,1,1,1), lwd=c(2,2,2,2), col = c(1,2,3,4), box.lwd=0)

#Figure 3b
plot(100*fin_i[3,2:100]/fin_i[1,2:100],type="l",col=4,lwd=2,lty=1,font.main=1.5,cex.main=1.5,cex.axis=1.5,cex.lab=1.5,main="(b) The monetary circuit",ylab = 'Index (flow)',xlab = '',ylim=range(100,115))
lines(100*fin_f[3,2:100]/fin_f[1,2:100],type="l",col=5,lwd=2,lty=2)
par(new = TRUE)
plot(100*ld[3,2:100]/ld[1,2:100],type="l",col=8,lwd=2,lty=1,axes = FALSE, bty = "n", xlab = "", ylab = "")
axis(4)
mtext("Index (stock)", side=4, line=3)
legend("right",c("Initial finance","Final finance","Outstanding debt (right axis)"),  bty = "n", cex = 1.5, lty=c(1,2,1), lwd=c(2,2,2,1), col = c(4,5,8), box.lwd=0)

#Figure 3c
plot(p[3,2:100],type="l",col=2,lwd=2,lty=1,font.main=1.5,cex.main=1.5,cex.axis=1.5,cex.lab=1.5,main="(c) Price level",ylab = '$',xlab = '',ylim=range(0.98,1.02))
legend("bottomright",c("Unit price of output"),  bty = "n", cex = 1.5, lty=c(1), lwd=c(2), col = c(2), box.lwd=0)

#Figure 3d
plot(100*cw_r[3,2:100]/cw_r[1,2:100],type="l",col="purple",lwd=2,lty=1,font.main=1.5,cex.main=1.5,cex.axis=1.5,cex.lab=1.5,main="(d) Real consumption of workers",ylab = 'Index',xlab = '',ylim=range(100,115))
lines(100*cw_r_t[3,2:100]/cw_r_t[1,2:100],type="l",col="orange",lwd=2,lty=2)
lines(100*cw_r_t[3,2:100]/cw_r_t[1,2:100],type="l",col="red1",lwd=2,lty=3)
legend("right",c("Actual","Expected","Planned"),  bty = "n", cex = 1.5, lty=c(1,2,3), lwd=c(2,2,2), col = c("purple","orange","red1"), box.lwd=0)

################################################################################
#Figure 4 - Shock to investment (mixed adjustment)

layout(matrix(c(1,2,3,4), 2, 2, byrow = TRUE))
par(mar = c(5, 4, 4, 4) + 0.3)  

#Figure 4a
plot(100*y_r[4,2:100]/y_r[1,2:100],type="l",col=1,lwd=2,lty=1,font.main=1.5,cex.main=1.5,cex.axis=1.5,cex.lab=1.5,main="(a) Production, consumption, and investment (real)",ylab = 'Index',xlab = '',ylim=range(90,150))
lines(100*cw_r[4,2:100]/cw_r[1,2:100],type="l",col=2,lwd=2,lty=1)
lines(100*cz_r[4,2:100]/cz_r[1,2:100],type="l",col=3,lwd=2,lty=1)
lines(100*id_r[4,2:100]/id_r[1,2:100],type="l",col=4,lwd=2,lty=1)
legend("topright",c("Output","Workers' consumption","Capitalists' consumption","Investment"),  bty = "n", cex = 1.5, lty=c(1,1,1,1), lwd=c(2,2,2,2), col = c(1,2,3,4), box.lwd=0)

#Figure 4b
plot(100*fin_i[4,2:100]/fin_i[1,2:100],type="l",col=4,lwd=2,lty=1,font.main=1.5,cex.main=1.5,cex.axis=1.5,cex.lab=1.5,main="(b) The monetary circuit",ylab = 'Index (flow)',xlab = '',ylim=range(100,115))
lines(100*fin_f[4,2:100]/fin_f[1,2:100],type="l",col=5,lwd=2,lty=2)
par(new = TRUE)
plot(100*ld[4,2:100]/ld[1,2:100],type="l",col=8,lwd=2,lty=1,axes = FALSE, bty = "n", xlab = "", ylab = "")
axis(4)
mtext("Index (stock)", side=4, line=3)
legend("right",c("Initial finance","Final finance","Outstanding debt (right axis)"),  bty = "n", cex = 1.5, lty=c(1,2,1), lwd=c(2,2,2,1), col = c(4,5,8), box.lwd=0)

#Figure 4c
plot(p[4,2:100],type="l",col=2,lwd=2,lty=1,font.main=1.5,cex.main=1.5,cex.axis=1.5,cex.lab=1.5,main="(c) Price level",ylab = '$',xlab = '',ylim=range(1,1.1))
legend("right",c("Unit price of output"),  bty = "n", cex = 1.5, lty=c(1), lwd=c(2), col = c(2), box.lwd=0)

#Figure 4d
plot(100*cw_r[4,2:100]/cw_r[1,2:100],type="l",col="purple",lwd=2,lty=1,font.main=1.5,cex.main=1.5,cex.axis=1.5,cex.lab=1.5,main="(d) Real consumption of workers",ylab = 'Index',xlab = '',ylim=range(93,115))
lines(100*cw_r_t[4,2:100]/cw_r_t[1,2:100],type="l",col="orange",lwd=2,lty=2)
lines(100*cw_r_t[3,2:100]/cw_r_t[1,2:100],type="l",col="red1",lwd=2,lty=3)
legend("right",c("Actual","Expected","Planned"),  bty = "n", cex = 1.5, lty=c(1,2,3), lwd=c(2,2,2), col = c("purple","orange","red1"), box.lwd=0)

################################################################################
#Figure 5 - Shock to investment (price adjustment)

layout(matrix(c(1,2,3,4), 2, 2, byrow = TRUE))
par(mar = c(5, 4, 4, 4) + 0.3)  

#Figure 5a
plot(100*y_r[5,2:100]/y_r[1,2:100],type="l",col=1,lwd=2,lty=1,font.main=1.5,cex.main=1.5,cex.axis=1.5,cex.lab=1.5,main="(a) Production, consumption, and investment (real)",ylab = 'Index',xlab = '',ylim=range(70,250))
lines(100*cw_r[5,2:100]/cw_r[1,2:100],type="l",col=2,lwd=2,lty=1)
lines(100*cz_r[5,2:100]/cz_r[1,2:100],type="l",col=3,lwd=2,lty=1)
lines(100*id_r[5,2:100]/id_r[1,2:100],type="l",col=4,lwd=2,lty=1)
legend("right",c("Output","Workers' consumption","Capitalists' consumption","Investment"),  bty = "n", cex = 1.5, lty=c(1,1,1,1), lwd=c(2,2,2,2), col = c(1,2,3,4), box.lwd=0)

#Figure 5b
plot(100*fin_i[5,2:100]/fin_i[1,2:100],type="l",col=4,lwd=2,lty=1,font.main=1.5,cex.main=1.5,cex.axis=1.5,cex.lab=1.5,main="(b) The monetary circuit",ylab = 'Index (flow)',xlab = '',ylim=range(100,130))
lines(100*fin_f[5,2:100]/fin_f[1,2:100],type="l",col=5,lwd=2,lty=2)
par(new = TRUE)
plot(100*ld[5,2:100]/ld[1,2:100],type="l",col=8,lwd=2,lty=1,axes = FALSE, bty = "n", xlab = "", ylab = "")
axis(4)
mtext("Index (stock)", side=4, line=3)
legend("right",c("Initial finance","Final finance","Outstanding debt (right axis)"),  bty = "n", cex = 1.5, lty=c(1,2,1), lwd=c(2,2,2,1), col = c(4,5,8), box.lwd=0)

#Figure 5c
plot(p[5,2:100],type="l",col=2,lwd=2,lty=1,font.main=1.5,cex.main=1.5,cex.axis=1.5,cex.lab=1.5,main="(c) Price level",ylab = '$',xlab = '',ylim=range(1,1.30))
legend("right",c("Unit price of output"),  bty = "n", cex = 1.5, lty=c(1), lwd=c(2), col = c(2), box.lwd=0)

#Figure 5d
plot(100*cw_r[5,2:100]/cw_r[1,2:100],type="l",col="purple",lwd=2,lty=1,font.main=1.5,cex.main=1.5,cex.axis=1.5,cex.lab=1.5,main="(d) Real consumption of workers",ylab = 'Index',xlab = '',ylim=range(75,115))
lines(100*cw_r_t[5,2:100]/cw_r_t[1,2:100],type="l",col="orange",lwd=2,lty=2)
lines(100*cw_r_t[3,2:100]/cw_r_t[1,2:100],type="l",col="red1",lwd=2,lty=3)
legend("right",c("Actual","Expected","Planned"),  bty = "n", cex = 1.5, lty=c(1,2,3), lwd=c(2,2,2), col = c("purple","orange","red1"), box.lwd=0)

################################################################################

#Figure 6 - Forced saving

layout(matrix(c(1,2,3,4), 2, 2, byrow = TRUE))
par(mar = c(5, 4, 4, 4) + 0.3)  

#Figure 6a
plot(100*cw_r[5,2:100]/cw_r[1,2:100],type="l",col="purple",lwd=2,lty=1,font.main=1.5,cex.main=1.5,cex.axis=1.5,cex.lab=1.5,main="(a) Consumption of workers",ylab = 'Index',xlab = '',ylim=range(75,115))
lines(100*cw[5,2:100]/cw[1,2:100],type="l",col="orange",lwd=2,lty=2)
lines(100*cw_r[5,2:100]/cw_r[1,2:100],type="l",col="purple",lwd=2,lty=1)
legend("topright",c("Real","Nominal"),  bty = "n", cex = 1.5, lty=c(1,2), lwd=c(2,2), col = c("purple","orange"), box.lwd=0)

#Figure 6b
plot((ydw[5,2:100]-cw[5,2:100])/p[5,2:100]-(ydw[1,2:100]-cw[1,2:100])/p[1,2:100],type="l",col="purple",lwd=2,lty=1,font.main=1.5,cex.main=1.5,cex.axis=1.5,cex.lab=1.5,main="(b) Saving of workers (difference with baseline)",ylab = '$',xlab = '',ylim=range(-3,2))
lines((ydw[5,2:100]-cw[5,2:100])-(ydw[1,2:100]-cw[1,2:100]),type="l",col="orange",lwd=2,lty=2)
lines((ydw[5,2:100]-cw[5,2:100])/p[5,2:100]-(ydw[1,2:100]-cw[1,2:100])/p[1,2:100],type="l",col="purple",lwd=2,lty=1)
legend("topright",c("Real","Nominal"),  bty = "n", cex = 1.5, lty=c(1,2), lwd=c(2,2), col = c("purple","orange"), box.lwd=0)

#Figure 6c
plot(100*cz_r[5,2:100]/cz_r[1,2:100],type="l",col="purple",lwd=2,lty=1,font.main=1.5,cex.main=1.5,cex.axis=1.5,cex.lab=1.5,main="(c) Consumption of capitalists",ylab = 'Index',xlab = '',ylim=range(100,300))
lines(100*cz[5,2:100]/cz[1,2:100],type="l",col="orange",lwd=2,lty=2)
lines(100*cz_r[5,2:100]/cz_r[1,2:100],type="l",col="purple",lwd=2,lty=1)
legend("bottomright",c("Real","Nominal"),  bty = "n", cex = 1.5, lty=c(1,2), lwd=c(2,2), col = c("purple","orange"), box.lwd=0)

#Figure 6d
plot((ydz[5,2:100]-cz[5,2:100])/p[5,2:100]-(ydz[1,2:100]-cz[1,2:100])/p[1,2:100],type="l",col="purple",lwd=2,lty=1,font.main=1.5,cex.main=1.5,cex.axis=1.5,cex.lab=1.5,main="(d) Saving of capitalists (difference with baseline)",ylab = '$',xlab = '',ylim=range(0,6))
lines((ydz[5,2:100]-cz[5,2:100])-(ydz[1,2:100]-cz[1,2:100]),type="l",col="orange",lwd=2,lty=2)
lines((ydz[5,2:100]-cz[5,2:100])/p[5,2:100]-(ydz[1,2:100]-cz[1,2:100])/p[1,2:100],type="l",col="purple",lwd=2,lty=1)
legend("topright",c("Real","Nominal"),  bty = "n", cex = 1.5, lty=c(1,2), lwd=c(2,2), col = c("purple","orange"), box.lwd=0)

################################################################################

#Figure 7 - Exploitation of workers

layout(matrix(c(1,2), 1, 2, byrow = TRUE))
par(mar = c(5, 4, 4, 4) + 0.3)  

#Figure 7a
plot(slt[1,2:49], type="l",lwd=2,lty=1,font.main=1.5,cex.main=1.5,cex.axis=1.5,cex.lab=1.5,main="(a) Surplus-labour time",ylab = 'N',xlab = '',ylim=range(min(slt[1,2:nPeriods]),max(slt[5,2:49])))
lines(slt[2,2:49], type="l",col=2,lwd=2,lty=1)
lines(slt[3,2:49], type="l",col=3,lwd=2,lty=1)
lines(slt[4,2:49], type="l",col=4, lwd=2,lty=1)
lines(slt[5,2:49], type="l",col="orange", lwd=2,lty=1)
lines(slt[1,2:49], type="l",col=1, lwd=2,lty=1)
legend("right",c("Baseline","Scenario 2","Scenario 3","Scenario 4","Scenario 5"),  bty = "n", cex = 1.5, lty=c(1,1,1,1,1), lwd=c(2,2,2,2,2), col = c(1,2,3,4,"orange"), box.lwd=0)

#Figure 7b
plot(100*expl[1,2:49], type="l",lwd=2,lty=1,font.main=1.5,cex.main=1.5,cex.axis=1.5,cex.lab=1.5,main="(b) Exploitation rate",ylab = '%',xlab = '',ylim=range(100*min(expl[1,2:nPeriods]),100*max(expl[5,2:49])))
lines(100*expl[2,2:49], type="l",col=2,lwd=2,lty=1)
lines(100*expl[3,2:49], type="l",col=3,lwd=2,lty=1)
lines(100*expl[4,2:49], type="l",col=4, lwd=2,lty=1)
lines(100*expl[5,2:49], type="l",col="orange", lwd=2,lty=1)
lines(100*expl[1,2:49], type="l",col=1, lwd=2,lty=1)
legend("right",c("Baseline","Scenario 2","Scenario 3","Scenario 4","Scenario 5"),  bty = "n", cex = 1.5, lty=c(1,1,1,1,1), lwd=c(2,2,2,2,2), col = c(1,2,3,4,"orange"), box.lwd=0)
