library(ggplot2);library(cowplot)
#setwd(my_dir)

# Fig. 1: Colocation scale correction

## Function to generate correction
MF_corr2 <- function(x,CV_MB,CV_C,rho_CMB) {
  corr <- 1 - (x/((1+x)^2))*(CV_MB^2) + (1/(1+x))*(rho_CMB*CV_MB*CV_C);
  return(corr);
}

corr_plots <- list()
CV_MBs <- c(0.25,0.5,1)
names <- c("CVMB0.25", "CVMB0.5", "CVMB1")
CV_C <- 0.5
nudge_fac <- -0.5
y_range <- matrix(c(0.98,1.14,0.85,1.25,0.75,1.5),nrow=2,ncol=3)

for(j in 1:length(CV_MBs)){
  CV_MB <- CV_MBs[j]
  corr_plots[[j]] <- ggplot(data = data.frame(x=c(0,5)),aes(x)) + 
    stat_function(fun = MF_corr2, args = list(CV_MB=CV_MB,CV_C=CV_C,rho_CMB=0)) + 
    geom_text(x = 0.5, y = MF_corr2(x=0.5,CV_MB=CV_MB,CV_C=CV_C,rho_CMB=0), label = paste(lambda = 0),nudge_y=nudge_fac) + 
    xlab("Lambda") + ylab("Scale Transition Correction") + 
    geom_hline(aes(yintercept = 1),color="red") + theme_bw(base_size=14) + 
    #  scale_y_continuous(limits = c(y_range[,j])) + 
    scale_y_continuous(limits = c(0.75,1.5))
  
  for(i in 1:5){
    corr_plots[[j]] <- corr_plots[[j]] + stat_function(fun = MF_corr2, args = list(CV_MB=CV_MB,CV_C=CV_C,rho_CMB=0.2*i)) + 
      geom_text(x = 0.5, y = MF_corr2(x=0.5,CV_MB=CV_MB,CV_C=CV_C,rho_CMB=0.2*i), label = paste(lambda = 0.2*i),nudge_y=nudge_fac)
  }
  
  #print(corr_plots[[j]])
  
}


## Variety of options for saving

pdf(file = "Scale_Corr.pdf", width = 10, height = 6)
plot_grid(corr_plots[[1]] , corr_plots[[2]], corr_plots[[3]], nrow = 1,labels="auto", rel_heights= c(1,1,1))
dev.off()

jpeg(file = "Scale_Corr.jpg", width = 8, height = 2.5, res = 720, units = "in")
plot_grid(corr_plots[[1]] , corr_plots[[2]], corr_plots[[3]], nrow = 1,labels="auto", rel_heights= c(1,1,1))
dev.off()

ggsave("Scale_Corr.jpeg",plot_grid(corr_plots[[1]] , corr_plots[[2]], corr_plots[[3]], nrow = 1,labels="auto", rel_heights= c(1,1,1))
       ,height = 6, width = 10)


# Figure 2: Q10 figure 

Q10_func <- function(x,Q_10=2.5,T_bar=25){
  scale_corr = 1 + ((1/200)*(log(Q_10)^2)*((T_bar*x)^2));
  return(scale_corr);
}

Q10_plot <- ggplot(data = data.frame(x=c(0,0.5)),aes(x=x)) + stat_function(fun = Q10_func) +
  xlab("CV(T)") + ylab("Scale Transition Correction") +
  theme_bw(base_size=15) + ggtitle("")

## Saving 
jpeg(file = "Q10_Corr.jpg",width=4, height = 3, res = 720, units = "in")
Q10_plot
dev.off()

# Figure 3: Soil Moisture Figure 


smc <- function(x,x_bar=x_bar){
  return(4*(x_bar-(x_bar^2)*(1+x^2)));
}

moist_func <- function(x){
  return(4*x-4*(x^2)); 
}

moistF_fig <- ggplot(data=data.frame(x=c(0,1)),aes(x)) + 
  stat_function(fun = moist_func) + 
  theme_bw(base_size = 18) + 
  xlab("Soil moisture") + 
  ylab("Relative respiration rate")


moist_fig <- ggplot(data=data.frame(x=c(0,1)),aes(x)) + 
  stat_function(fun = smc,args=list(x_bar=0.5)) + 
  stat_function(fun = smc,args=list(x_bar=0.25)) + 
  theme_bw(base_size=18) + 
  annotate("text", label = "soil moisture = 0.5",x=0.55,y=0.95,size=4) + 
  annotate("text", label = "soil moisture = 0.25",x=0.25,y=0.65,size=4) + 
  ylab("") +
  xlab("CV(soil moisture)")

plot_by2 <- plot_grid(moistF_fig,moist_fig,labels = "auto")
plot_by2

ggsave("soil_moistby2_081121.jpeg",plot_by2,height = 4, width = 8)

# Figure 4: Substrate Affinity 

keff_f <- function(x,cv_k = cv_k){
  return((1 + (1/((1+x)^2))*(cv_k^2))); 
}

k_trans <- ggplot(data = data.frame(x=c(0,5)),aes(x)) + 
  stat_function(fun = keff_f, args = list(cv_k=1)) +
  stat_function(fun = keff_f, args = list(cv_k=0.5)) + 
  theme_bw(base_size=18) + 
  xlab("Lambda") + 
  ylab("Scale Transition Correction") + 
  annotate("text", label = "CV(k)=1",x=1,y=1.375,size=4) + 
  annotate("text", label = "CV(k)=0.5",x=0.5,y=1.15,size=4)


ggsave("Keff_Fig_081521.jpeg",k_trans,height = 4, width = 4)



## Fig 5: Model complexity
mod_complex <- ggplot(data = data.frame(x = c(2,10)),aes(x)) + stat_function(fun = function(x){choose(x,2)}) + 
  theme_bw(base_size=15) + scale_x_continuous(breaks = seq(2,10,1)) + 
  scale_y_continuous(breaks=seq(0,50,5)) +
  ylab("# correlation terms") + 
  xlab("# spatially varying parameters")

print(mod_complex)

jpeg(file = "ScaleTransition_Complexity.jpg",width = 4, height = 3, res = 720,
     units = "in")
