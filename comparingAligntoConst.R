const<-read.table("TPM_brain.txt",sep='\t', stringsAsFactors = FALSE)
const<-const[,-c(6,22,23,24)]
align<-t(TPM_BayesTraits)

const1<-const[which( const$V1 %in% row.names(align)),c(1:6)]
align1<-align[which(row.names(align) %in% const$V1),c(1:5)]
combo<-cbind(align[which(row.names(align) %in% const$V1),c(1:5)],const[which( const$V1 %in% row.names(align)),c(2:6)])


write.table(const1, "const1.txt", sep='\t')
write.table(align1, "align1.txt", sep='\t')


combo<-read.table("alignconstcombo.txt",sep='\t', stringsAsFactors = F, header=T)
sqrtcombo<-sqrt(combo[,2:11])

p1<-ggplot(data=sqrtcombo, mapping=aes(x=align_M_1,y=const_M_1)) + 
  theme_bw() +
  theme(plot.title= element_text(size=20, face="bold"), axis.title.x=element_text(size=20),axis.title.y=element_text(size=20)) +
  ggtitle("Constitiutive v Aligned") +
  labs( x="Aligned TPM", y="Const TPM", face="bold", size=20) +
  geom_point() +
  geom_abline(intercept=0,slope=1) + 
  scale_y_log10() + scale_x_log10()
p2<-ggplot(data=sqrtcombo, mapping=aes(x=align_M_2,y=const_M_2)) + 
  theme_bw() +
  theme(plot.title= element_text(size=20, face="bold"), axis.title.x=element_text(size=20),axis.title.y=element_text(size=20)) +
  ggtitle("Constitiutive v Aligned") +
  labs( x="Aligned TPM", y="Const TPM", face="bold", size=20) +
  geom_point() +
  geom_abline(intercept=0,slope=1) + 
  scale_y_log10() + scale_x_log10()

p3<-ggplot(data=sqrtcombo, mapping=aes(x=align_M_3,y=const_M_3)) + 
  theme_bw() +
  theme(plot.title= element_text(size=20, face="bold"), axis.title.x=element_text(size=20),axis.title.y=element_text(size=20)) +
  ggtitle("Constitiutive v Aligned") +
  labs( x="Aligned TPM", y="Const TPM", face="bold", size=20) +
  geom_point() +
  geom_abline(intercept=0,slope=1) + 
  scale_y_log10() + scale_x_log10()

p4<-ggplot(data=sqrtcombo, mapping=aes(x=align_M_4,y=const_M_4)) + 
  theme_bw() +
  theme(plot.title= element_text(size=20, face="bold"), axis.title.x=element_text(size=20),axis.title.y=element_text(size=20)) +
  ggtitle("Constitiutive v Aligned") +
  labs( x="Aligned TPM", y="Const TPM", face="bold", size=20) +
  geom_point() +
  geom_abline(intercept=0,slope=1) + 
  scale_y_log10() + scale_x_log10()

p5<-ggplot(data=sqrtcombo, mapping=aes(x=align_F_1,y=const_F_1)) + 
  theme_bw() +
  theme(plot.title= element_text(size=20, face="bold"), axis.title.x=element_text(size=20),axis.title.y=element_text(size=20)) +
  ggtitle("Constitiutive v Aligned") +
  labs( x="Aligned TPM", y="Const TPM", face="bold", size=20) +
  geom_point() +
  geom_abline(intercept=0,slope=1) + 
  scale_y_log10() + scale_x_log10()

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}




multiplot(p1,p2,p3,p4,p5, cols=2)
