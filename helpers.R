library(Biobase)
library(IRanges)
library(ggplot2)

tx2gene <- readRDS("data/tx2gene.rds")

geneNames <- tx2gene$hgnc_symbol

plotCovs <- function(data,txID,exps,cntl){

    tx.id <- tx2gene$ensembl_transcript_id[tx2gene$hgnc_transcript_name == txID]
    
    ## For some reason, substeing the feat.covs first takes for ever...
    ## Better to compute on more samples then subset the df
    covs <- try(sapply(data,function(covs) as.vector(covs[[tx.id]])),silent=TRUE)

    if(class(covs) == 'try-error')
        return(NULL)
    
    ### Pick only the experiment of interest
    covs <- covs[,exps,drop=FALSE]
    
    ## Compute the median over the length of the transcript cut in 100 pieces
    ## Might breack for transcript shorter than 100 nt...
    ## Ok... if there is only one samples, I can't figure out how to return same structure as when there is more than one
    ## Let's deal with that...
    if(length(exps) > 1){
        med.covs <- t(sapply(split(data.frame(covs),cut(1:nrow(covs),100,FALSE)),function(x) rowMedians(t(x))))
    } else {
        med.covs <- matrix(sapply(split(data.frame(covs),cut(1:nrow(covs),100,FALSE)),function(x) rowMedians(t(x))))
    }
    
    med.covs <- t(t(med.covs)/colSums(med.covs))

    ## put the cntl as last level, use this to order factor in exp
    levels.order <- c(grep(cntl,colnames(covs),value=TRUE,invert=TRUE),
                      grep(cntl,colnames(covs),value=TRUE))
    
    d.f <-data.frame(x=rep(1:nrow(med.covs),ncol(med.covs)),
                     cov=as.vector(med.covs),
                     exp=factor(rep(colnames(covs),each=nrow(med.covs)),levels=levels.order))
    
    p <- ggplot(d.f,aes(x=x,y=cov))+facet_grid(exp~.)
    p <- p+geom_histogram(stat='identity')
    p <- p+labs(title=paste('Relative coverage density over',txID),
                x='Relative postition',y="Relative read coverage")

    return(p)
}

getTx <- function(geneID){
    tx2gene$hgnc_transcript_name[tx2gene$hgnc_symbol %in% geneID]    
}
