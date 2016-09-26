ggplot.model <- function(model, type="conditional", res=FALSE, variables, param = F,
                         col.line="black", col.point="darkblue", size.line=1, size.point=1) {
  require(visreg)
  require(plyr)
  plotdata <- visreg(model, xvar = variables, type = type, plot = FALSE)
  smooths <- ldply(plotdata, function(part)   
    data.frame(Variable = part$meta$x, 
               x=part$fit[[part$meta$x]], 
               smooth=part$fit$visregFit, 
               lower=part$fit$visregLwr, 
               upper=part$fit$visregUpr))
  residuals <- ldply(plotdata, function(part)
    data.frame(Variable = part$meta$x, 
               x=part$res[[part$meta$x]], 
               y=part$res$visregRes))
  if (param)
    ggplot(smooths, aes(x, smooth)) + geom_hline(yintercept = 0, color = 'red') +
    geom_errorbar(aes(x = x, ymin = lower, ymax= upper), width=.5, position=position_dodge(.05)) +
    geom_point(shape = 21, fill = 'darkblue', size = 5) + labs(x = NULL, y = NULL) +
    facet_grid(. ~ Variable, scales = "free_x") + theme_bw(11)
  else
    ggplot(smooths, aes(x, smooth)) + geom_hline(yintercept = 0, color = 'red') +
    geom_ribbon(aes( ymin= lower, ymax = upper), alpha=.35, fill = 'darkblue') +
    geom_line(col=col.line, size=size.line) + labs(x = NULL, y = NULL) +
    #geom_line(aes(y=lower), linetype="dashed", col=col.line, size=size.line) +
    #geom_line(aes(y=upper), linetype="dashed", col=col.line, size=size.line) +
    facet_grid(. ~ Variable, scales = "free_x") + theme_bw(11)

}