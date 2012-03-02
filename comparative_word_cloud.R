# File-Name:       comparative_word_cloud.R           
# Date:            2012-03-01                              
# Author:          Winter Mason
# Email:           m@winteram.com                                   
# Purpose:         Attempt to improve on Drew Conway's word-cloud
# Packages Used:   tm, ggplot2  
# Input:		       sourcedir: directory that contains two text files to be compared
#				           imgname: the name of the output image file
#				           add.stops: a list of words to ignore in the text
#     			       nwords: the maximum number of most frequent words to plot
#				           imgwidth,imgheight: dimensions of the resulting image
#				           mintext,maxtext: the range of word sizes in the plot
# Output File:     word_cloud.pdf

# Copyright (c) 2012, under the Simplified BSD License.  
# For more information on FreeBSD see: http://www.opensource.org/licenses/bsd-license.php
# All rights reserved.                                                         

# Load libraries and data
library(tm)
library(ggplot2)

get.spaces <- function(df, mintext, maxtext) {
  df <- df[with(df, order(freq.dif)),]
  df$w <- 0
  df$y.val <- rep(0,nrow(df))
  left.line <- data.frame(top=0,bottom=0,left=min(df$freq.dif)-1000,right=min(df$freq.dif)-1000)
  for (name in row.names(df)) {
    ht.factor <- (df[name,"freq.sum"]-min(df$freq.sum))*(maxtext-mintext)/diff(range(df$freq.sum)) + mintext
    wordlen <- strwidth(name,"figure")*diff(range(df$freq.dif))*ht.factor/5
    x.left <- df[name,"freq.dif"]-wordlen/2
    x.right <- df[name,"freq.dif"]+wordlen/2
    df[name,"w"] <- abs(x.right - x.left)*1.2
    bad.spaces <- left.line[left.line$right>x.left,]
   
    # try top first
    top.lim.up = df[name,"freq.sum"]/2
    bottom.lim.up = -df[name,"freq.sum"]/2
    while(nrow(bad.spaces[(bad.spaces$top>=top.lim.up & bad.spaces$bottom<=top.lim.up) | 
            (bad.spaces$top>=bottom.lim.up & bad.spaces$bottom<=bottom.lim.up) |
            (bad.spaces$top>=top.lim.up & bad.spaces$bottom<=bottom.lim.up) |
            (bad.spaces$top<=top.lim.up & bad.spaces$bottom>=bottom.lim.up),]) > 0) {
              top.lim.up = top.lim.up + diff(range(df$freq.sum))/100
              bottom.lim.up = bottom.lim.up + diff(range(df$freq.sum))/100
    }
    # try bottom next    
    top.lim.dn = df[name,"freq.sum"]/2
    bottom.lim.dn = -df[name,"freq.sum"]/2
    while(nrow(bad.spaces[(bad.spaces$top>=top.lim.dn & bad.spaces$bottom<=top.lim.dn) | 
              (bad.spaces$top>=bottom.lim.dn & bad.spaces$bottom<=bottom.lim.dn) |
              (bad.spaces$top>=top.lim.dn & bad.spaces$bottom<=bottom.lim.dn) |
              (bad.spaces$top<=top.lim.dn & bad.spaces$bottom>=bottom.lim.dn),]) > 0) {
                top.lim.dn = top.lim.dn - diff(range(df$freq.sum))/100
                bottom.lim.dn = bottom.lim.dn - diff(range(df$freq.sum))/100
    }
    # find best position
    if (abs(top.lim.dn)+abs(bottom.lim.dn)<abs(top.lim.up)+abs(bottom.lim.up))
    {
      left.line <- rbind(left.line,c(top.lim.dn,bottom.lim.dn,x.left,x.right))
      df[name,"y.val"] <- top.lim.dn - df[name,"freq.sum"]/2
    } else {
      left.line <- rbind(left.line,c(top.lim.up,bottom.lim.up,x.left,x.right))
      df[name,"y.val"] <- top.lim.up - df[name,"freq.sum"]/2
    }
  }
  return(df)
}


comparative.word.cloud <- function(sourcedir,
									imgname="word_cloud.pdf",
									add.stops=c("http://"),
									nwords=100,
									imgwidth=8,
									imgheight=4,
									mintext=2,
									maxtext=10,
                  figtitle="") 
{
	### Step 1: Load in text data, clean, and analyze overlapping terms
	sources<-Corpus(DirSource(sourcedir))

	if (length(sources) != 2) stop("There must be exactly two files in the source directory")
	
	# Get word counts
	first.wc<-length(unlist(strsplit(sources[[1]], " ")))
	second.wc<-length(unlist(strsplit(sources[[2]], " ")))

	# Create a Term-Document matrix
	speech.control=list(stopwords=c(stopwords(),add.stops), removeNumbers=TRUE, removePunctuation=TRUE)
	sources.matrix<-TermDocumentMatrix(sources, control=speech.control)

	# Create data frame from matrix
  sink("/dev/null")
	sources.df<-as.data.frame(inspect(sources.matrix))
  sink()
  src.names <- names(sources.df)
	names(sources.df) <- c("source1","source2")
	sources.df<-transform(sources.df, freq.dif=source1-source2, freq.sum=source1+source2) 
  src1.name <- gsub(".txt","",src.names[1])
  src2.name <- gsub(".txt","",src.names[2])
  figlabels <- c(paste("More Often\n",src2.name,sep=""),"Equally Often",paste("More Often\n",src1.name,sep=""))

  # Only show _nwords_ most frequent words
	if (nwords <= 0 || nwords>nrow(sources.df)) {
	  nwords <- nrow(sources.df) # show all words
	}
	sources.df<-subset(sources.df, freq.sum>=quantile(sources.df$freq.sum, probs=(1-nwords/nrow(sources.df))))
	#sources.df<-transform(sources.df, freq.dif=(source1/first.wc)-(source2/second.wc))    

	# get x,y coordinates for terms
	sources.df <- get.spaces(sources.df, mintext, maxtext)
  sources.df <- transform(sources.df, term.labels=row.names(sources.df))

  
	word.cloud <- ggplot(sources.df, aes(x=freq.dif, y=y.val)) + 
	  geom_rect(aes(xmin = freq.dif - w/2, xmax = freq.dif + w/2, ymin = y.val - freq.sum/2, ymax = y.val + freq.sum/2, fill = freq.dif), alpha=0.3) +
	  geom_text(aes(size=freq.sum, label=term.labels, colour=freq.dif)) + 
	  scale_size(to=c(mintext,maxtext)) +
	  scale_colour_gradient(low="darkred", high="darkblue", legend=FALSE) +
	  scale_fill_gradient(low="red", high="blue", legend=FALSE) +
	  scale_x_continuous(breaks=c(min(sources.df$freq.dif),0,max(sources.df$freq.dif)),labels=figlabels) +
	  scale_y_continuous(breaks=c(0),labels=c(""))+xlab("")+ylab("")+theme_bw()+
	  opts(panel.grid.major=theme_blank(),panel.grid.minor=theme_blank(), legend.position="none",title=figtitle)
	ggsave(plot=word.cloud,filename=imgname,width=imgwidth,height=imgheight)
}
