north1 = read.csv("results/pval_blocks_north1.csv", stringsAsFactors = F, header = F)[ , 1]
east1 = read.csv("results/pval_blocks_east1.csv", stringsAsFactors = F, header = F)[ , 1]
ne1 = read.csv("results/pval_blocks_ne1.csv", stringsAsFactors = F, header = F)[ , 1]

fisher <- function(pvals){
  xsquared = -2 * sum(log(pvals))
  dof = 2*length(pvals)
  return(1-pchisq(xsquared, df = dof))
}

length(north1[north1 <= .05]) / length(north1)
length(east1[east1 <= .05]) / length(east1)
length(ne1[ne1 <= .05]) / length(ne1)

fisher(north1)
fisher(east1)
fisher(ne1)