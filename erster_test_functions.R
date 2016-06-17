make.formula <- function(response, fixed, pars, nonpars)
{
  fm <- paste(response, " ~ ", sep="")
  psstring <- ""
  if(length(fixed)>0)
  {
    fm <- paste(fm, paste(fixed, collapse= "+"), sep="")
    psstring <- " + "
  }
  if(length(pars)>0)
  {
    fm <- paste(fm, psstring, paste(pars, collapse= "+"), sep="")
    psstring <- " + "
  }
  if(length(nonpars)>0)
  {
    fm <- paste(fm, psstring, paste("s(",nonpars, ", bs=\"ps\")", collapse= "+", sep=""), sep="")
  }
  return(as.formula(fm))
}