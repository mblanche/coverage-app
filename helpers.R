library(Biobase)
library(IRanges)
library(ggplot2)


tx2gene <- readRDS("data/tx2gene.rds")
geneNames <- tx2gene$external_gene_id

exps <- c('DRB A1/A2 RNAi'='data/DRB_A1_feat_covs.rds',
          'C8 treated cells'='data/C8_featCovs.RDS')

plotCovs <- function(data,txID,exps,cntl){

    tx.id <- tx2gene$ensembl_transcript_id[tx2gene$external_transcript_id == txID]

    
    ## For some reason, substeing the feat.covs first takes for ever...
    ## Better to compute on more samples then subset the df
    covs <- sapply(data,function(covs) as.vector(covs[[tx.id]]))

    ### Pick only the experiment of interest
    covs <- covs[,exps]
    
    ## Compute the median over the length of the transcript cut in 100 pieces
    ## Might breack for transcript shorter than 100 nt...
    med.covs <- t(sapply(split(data.frame(covs),cut(1:nrow(covs),100,FALSE)),function(x) rowMedians(t(x))))
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
    tx2gene$external_transcript_id[tx2gene$external_gene_id %in% geneID]    
}

getExperiments <- function(){
    names(data)
}
    
