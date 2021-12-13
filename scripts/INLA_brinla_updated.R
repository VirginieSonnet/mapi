##################################
# Modified brinla plot functions #
##################################

# Reference: brinla package, https://github.com/julianfaraway/brinla
require(data.table)

# Fitted values and Random parameters plot -------------------------------------

# changes only in the ggplot at the end 

bri.band.ggplot <- function (result, name = NULL, x = NULL, alpha = 0.05, ind = NULL, design=1, 
          type = c("random", "fitted", "linear", "lincomb"), xlab = NULL, 
          ylab = NULL, main = NULL, hpd = FALSE, date, date_labels = "%b\n%y", date_breaks="2 months",
          family = "Lato") 
{
    require(ggplot2)
    require(inlatools)
    if (!requireNamespace("ggplot2", quietly = TRUE)) {
        stop("Package ggplot2 needed for this function to work. Please install it.", 
             call. = FALSE)
    }
    result
    if (is.null(ind) == TRUE) {
        if (type == "random") {
            post.summary <- result$summary.random[[name]]
            marg <- result$marginals.random[[name]]
        }
        if (type == "fitted") {
            post.summary <- result$summary.fitted.values
            marg <- result$marginals.fitted.values
        }
        if (type == "linear") {
            post.summary <- result$summary.linear.predictor
            marg <- result$marginals.linear.predictor
        }
        if (type == "lincomb") {
            post.summary <- result$summary.lincomb.derived
            marg <- result$marginals.lincomb.derived
        }
    } else {
        if (type == "random") {
            post.summary <- result$summary.random[[name]][ind, 
                                                          ]
            marg <- result$marginals.random[[name]][ind]
        }
        if (type == "fitted") {
            post.summary <- result$summary.fitted.values[ind, 
                                                         ]
            marg <- result$marginals.fitted.values[ind]
        }
        if (type == "linear") {
            post.summary <- result$summary.linear.predictor[ind, 
                                                            ]
            marg <- result$marginals.linear.predictor[ind]
        }
        if (type == "lincomb") {
            post.summary <- result$summary.lincomb.derived[ind, 
                                                           ]
            marg <- result$marginals.lincomb.derived[ind]
        }
    }
    if (hpd == TRUE) {
        pp <- 1 - alpha
        tmp <- sapply(marg, function(x) inla.hpdmarginal(pp, 
                                                         x))
        fhat.lb <- tmp[1, ]
        fhat.ub <- tmp[2, ]
    } else {
        p.min <- alpha/2
        p.max <- 1 - alpha/2
        fhat.lb <- sapply(marg, function(x) inla.qmarginal(p.min, 
                                                           x))
        fhat.ub <- sapply(marg, function(x) inla.qmarginal(p.max, 
                                                           x))
        fhat.lb <- as.vector(fhat.lb)
        fhat.ub <- as.vector(fhat.ub)
    }
    fhat <- post.summary$mean
    if (is.null(name) == FALSE) {
        xx <- result$summary.random[[name]]$ID
    } else {
        xx <- 1:length(fhat)
    }
    if (is.null(x) == FALSE) {
        xx <- x
    }
    data.plot <- data.frame(x = xx, fhat = fhat, f.lb = fhat.lb, 
                            f.ub = fhat.ub)
    if (type=="fitted"){
        if (is.null(ind)){
            obs <- get_observed(result)
        } else {
            obs <- get_observed(result)[ind]
        }
        if (design==1){
            ggplot(data.plot, aes(x = date)) + 
                # plot
                geom_line(aes(y = obs),col="black") + 
                geom_ribbon(aes(ymin = f.lb, ymax = f.ub), color="blue", linetype="dashed", alpha = 0, size=0.2) + 
                geom_line(aes(y = fhat),col="red") + 
                # theme
                theme_bw(base_size = 20) + 
                theme(text = element_text(size = 15, family = family)) +
                # axis
                scale_x_datetime(date_breaks=date_breaks, date_labels=date_labels,expand=c(0,0)) + 
                labs(x = xlab, y = ylab) + 
                ggtitle(main)
        } else if (design==2){
            ggplot(data.plot, aes(x = date)) + 
                # plot
                geom_point(aes(y = obs), size = 2, col="grey") +
                geom_ribbon(aes(ymin = f.lb, ymax = f.ub), color="blue", linetype="dashed", alpha = 0, size=0.2) + 
                geom_line(aes(y = fhat),col="black") + 
                # theme
                theme_bw() +
                theme(text = element_text(size = 15, family = family)) +
                theme(panel.grid.major.x = element_line(size = 0.5, linetype = 'dashed', color="grey77"),
                      panel.grid.minor = element_blank(),
                      panel.grid.major.y = element_blank()) + 
                # axis
                scale_x_datetime(date_breaks=date_breaks, date_labels=date_labels,expand=c(0,0)) +
                labs(x = xlab, y = ylab) + 
                ggtitle(main)
        }
    } else {
        ggplot(data.plot, aes(x = date)) + 
            # plot
            geom_ribbon(aes(ymin = f.lb, ymax = f.ub), color="blue", linetype="dashed", alpha = 0, size=0.2) + 
            geom_line(aes(y = fhat),col="black") + 
            # theme
            theme_bw(base_size = 20) + 
            theme(text = element_text(size = 15, family = family)) +
            # axis
            scale_x_datetime(date_breaks=date_breaks, date_labels=date_labels,expand=c(0,0)) + 
            labs(x = xlab, y = ylab) + 
            ggtitle(main)
    }
}


# Fixed parameters  ---------------------------------------------------------------------------

# add scales, main, label (e.g.: c("1"="PC1","2"="PC2","3"="PC3","4"="PC4","5"="PC5","6"="PC6")) as input 
# add theme 

bri.fixed.plot <- function (r, together = FALSE, scales="free", label=NULL,
                            main = "Posterior densities of the fixed effects",
                            family = "Lato") 
{
    if (!require("ggplot2")) 
        stop("Function requires ggplot2 package. Please install this first.")
    rmf = r$marginals.fixed
    cf = data.frame(do.call(rbind, rmf))
    cf$parameter = rep(names(rmf), times = sapply(rmf, nrow))
    if (together) {
        p = ggplot(cf, aes(x = x, y = y, linetype = parameter)) + 
            geom_line() + geom_vline(xintercept = 0) + ylab("density") + 
            theme_bw() + 
            theme(text = element_text(size = 15, family = family)) + 
            labs(title=main)
        print(p)
    }
    else {
        cf$xmin <- r$summary.fixed[cf$parameter,3]
        cf$xmax <- r$summary.fixed[cf$parameter,5]
        p = ggplot(cf, aes(x = x, y = y)) + 
            # plot
            geom_rect(aes(xmin=xmin, xmax=xmax, ymin=-Inf, ymax=Inf), alpha=0.005) + 
            geom_line() + 
            facet_wrap(~parameter, 
                       scales = scales,
                       labeller = labeller(parameter=label)) + 
            geom_vline(xintercept = 0, color="red", linetype="dashed", size=1.5) + 
            # theme
            theme_bw() + 
            theme(strip.background = element_rect(color="black", fill="firebrick", size=1.5, linetype="solid"),
                  strip.text = element_text(colour = 'white'),
                  text = element_text(size = 15, family = family)) + 
            # axis 
            labs(x="", y = "Density", title=main) 
        print(p)
    }
    return(p)
}


bri.fixed.plot.facet <- function (r, together = FALSE, scales="free", label=NULL,
                            main = "Posterior densities of the fixed effects",
                            family = "Lato") 
{

    rmf = r$marginals.fixed
    cf = data.frame(do.call(rbind, rmf))
    cf$parameter = rep(names(rmf), times = sapply(rmf, nrow))
    cf$xmin <- r$summary.fixed[cf$parameter,3]
    cf$xmax <- r$summary.fixed[cf$parameter,5]
    cf$var <- var 
    if (together) {
        p = ggplot(cf, aes(x = x, y = y, linetype = parameter)) + 
            geom_line() + geom_vline(xintercept = 0) + ylab("density") + 
            theme_bw() + 
            theme(text = element_text(size = 15, family = family)) + 
            labs(title=main)
        print(p)
    }
    else {
        cf$xmin <- r$summary.fixed[cf$parameter,3]
        cf$xmax <- r$summary.fixed[cf$parameter,5]
        p = ggplot(cf, aes(x = x, y = y)) + 
            # plot
            geom_rect(aes(xmin=xmin, xmax=xmax, ymin=-Inf, ymax=Inf), alpha=0.005) + 
            geom_line() + 
            facet_wrap(~parameter, 
                       scales = scales,
                       labeller = labeller(parameter=label)) + 
            geom_vline(xintercept = 0, color="red", linetype="dashed", size=1.5) + 
            # theme
            theme_bw() + 
            theme(strip.background = element_rect(color="black", fill="firebrick", size=1.5, linetype="solid"),
                  strip.text = element_text(colour = 'white'),
                  text = element_text(size = 15, family = family)) + 
            # axis 
            labs(x="", y = "Density", title=main) 
        print(p)
    }
    return(p)
}


# Random effects -------------------------------------------

# hyperparameters posteriors 

bri.hyperpar.plot <- function (r, together = TRUE, scales="free", label=NULL,
          main = "Hyperparameters posteriors",family = "Lato") 
{
    if (!require("ggplot2")) 
        stop("Function requires ggplot2 package. Please install this first.")
    irp = r$internal.marginals.hyperpar
    hrp = r$marginals.hyperpar
    hypnames = names(irp)
    iip = grep("precision", hypnames)
    for (i in 1:length(irp)) {
        if (i %in% iip) {
            irp[[i]] = bri.hyper.sd(irp[[i]], internal = TRUE)
        }
        else {
            irp[[i]] = hrp[[i]]
            hypnames[i] = names(hrp)[i]
        }
    }
    hypnames = sub("Log precision", "SD", hypnames)
    hypnames = sub("the Gaussian observations", "error", hypnames)
    names(irp) = hypnames
    cf = data.frame(do.call(rbind, irp))
    cf$parameter = rep(hypnames, times = sapply(irp, nrow))
    if (together) {
        p = ggplot(cf, aes(x = x, y = y, linetype = parameter)) + 
            geom_line() + ylab("density") + xlab("")
        print(p)
    }
    else {
        p = ggplot(cf, aes(x = x, y = y)) + 
            # plot
            geom_line() + 
            facet_wrap(~parameter, ncol=3, 
                       scales = scales,
                       labeller = labeller(parameter=label)) + 
           # theme
           theme_bw() + 
           theme(strip.background = element_rect(color="black", fill="firebrick", size=1.5, linetype="solid"),
                 strip.text = element_text(colour = 'white'),
                 text = element_text(size = 15, family = family)) + 
           # axis 
           labs(x="", ylab = "density", title=main) 
        print(p)
    }
    invisible(cf)
}

# plots of beta coefficients 

# Reference: INLAutils, https://github.com/timcdlucas/INLAutils/

plot_random_effects <- function(x, scales="free_y", date=NULL, label=NULL, date_breaks="2 months", date_labels = "%b\n%Y",
                                family = "Lato", title="Posterior median for the fixed effects",
                                legend=TRUE){
    
    if(length(x$summary.random) == 0) stop('No random effects to plot')
    allSummary <- lapply(seq_len(length(x$summary.random)), 
                         function(p) data.frame(x$summary.random[[p]], var = names(x$summary.random)[p]))
    allSummary <- do.call(rbind, allSummary)
    allSummary <- allSummary %>% 
        mutate(ID=as.numeric(factor(ID)),
               date = rep(date,length(x$summary.random)),
               var = factor(var, levels=unique(var))) %>% 
        mutate(color = case_when(X0.025quant > 0 ~ "red",
                                 X0.975quant < 0 ~ "blue",
                                 TRUE ~ "grey")) %>% 
        mutate(color = factor(color))
    allSummary$group <- rleid(allSummary$color)
    # doesn't seem to be necessary to add same coordinates at end of each group to link
    # allSummary_plot <- head(do.call(rbind, by(allSummary, allSummary$group, rbind, NA)), -1)
    # allSummary_plot[,c("group", "color")] <- lapply(allSummary_plot[,c("group", "color")], na.locf)
    # allSummary_plot[] <- lapply(allSummary_plot, na.locf, fromLast = TRUE)
    
    # plot
    if (is.null(date)){
        p <- ggplot(allSummary, aes_string(x = 'ID', y = 'X0.5quant')) +
                # plot 
                geom_line(color="red") +
                geom_ribbon(aes_string(ymin = '`X0.025quant`', ymax = '`X0.975quant`'), alpha = 0, color="blue") + 
                facet_wrap(~var, scales = scales, ncol = 1,
                           labeller=labeller(var=label)) +
                geom_hline(aes(yintercept=0), color="black") + 
                # theme 
                theme_bw() + 
                theme(strip.background = element_rect(color="white", fill="white", size=0, linetype="solid"),
                  text = element_text(size = 15, family = family)) + 
                # axis 
                labs(x='ID', y=bquote(beta),title=title)
    } else {
        p <- ggplot(allSummary, aes_string(x = 'date', y = 'X0.5quant',color = 'color')) +
            # plot 
            geom_ribbon(aes_string(ymin = '`X0.025quant`', ymax = '`X0.975quant`',
                                   fill='color', group='group'), 
                        alpha = 0.3, color = NA) + 
            geom_point(size = 0.5) +
            facet_wrap(~var, scales = scales, ncol = 1,
                       labeller=labeller(var=label),
                       strip.position = "left") +
            geom_hline(aes(yintercept=0), color="black") + 
            scale_color_manual(values=c("steelblue4","grey80", "red4"), name = "Effect", labels = c("Negative","Non-significant",
                                                                                                    "Positive"),
                               guide = guide_legend(reverse=TRUE)) + 
            scale_fill_manual(values=c("steelblue4","grey80", "red4"),name = "Effect", labels = c("Negative","Non-significant",
                                                                                                  "Positive"),
                              guide = guide_legend(reverse=TRUE)) + 
            # theme 
            theme_bw() + 
            theme(strip.background = element_rect(color="white", fill="white", size=0, linetype="solid"),
                  strip.text = element_text(face = "bold"), 
                  strip.placement="outside",
                  panel.border = element_blank(),
                  axis.line=element_line(colour="black"),
                  text = element_text(size = 15, family = family),
                  legend.position="bottom",
                  panel.grid.minor.y = element_blank(),
                  panel.grid.major.x = element_line(size = 1, color="grey90")) + 
            # axis 
            labs(x="", y=bquote(beta),title=title,
                 color = "", fill="") + 
            scale_x_datetime(date_breaks=date_breaks, date_labels=date_labels, minor_breaks = "1 month", expand=c(0,0))
        if (legend==FALSE){
            p <- p + theme(legend.position = "none")
        }
    }
    return(p)
}


# Residuals --------------------------------------------------------------------

# Reference: INLAutils 


plot_inla_residuals <- function(inla.model, observed, lag=100, date, date_breaks="2 months", date_labels = "%b\n%y",
                                family = "Lato"){
    if(is.null(inla.model$marginals.fitted.values)) stop('No fitted values to plot')
    if(any(is.na(inla.model$misc$linkfunctions$link))){ 
        warning('Fitted values from the INLA model may have been returned on the linear, rather than link scale. Use `control.predictor = list(link = 1)` to make sure all fitted values are on the natural scale.')
    }
    
    resid <- observed - fitted(r)
    p1 <- ggplot() + 
        geom_line(aes(x=date, y=resid)) + 
        theme_bw() + 
        theme(text = element_text(size = 15, family = family)) + 
        # axis 
        labs(x="", y="Residuals",title="Residuals") + 
        scale_x_datetime(date_breaks=date_breaks, date_labels=date_labels, expand=c(0,0))
     
    # acf 
    p2 <- ggAcf(resid, type="correlation", lag.max=lag) + 
        theme_bw() + 
        theme(text = element_text(size = 15, family = family)) + 
        labs(title="") + 
        scale_x_continuous(expand=c(0.01,0))
    
    # histogram of residuals 
    p3 <- ggplot() + 
        geom_histogram(aes(resid), color="black", fill="grey") + 
        theme_classic() + 
        theme(text = element_text(size = 15, family = family)) + 
        labs(x="Residuals", y="Counts") + 
        scale_y_continuous(expand=c(0,0))
     
    # pacf 
    p4 <- ggPacf(resid, lag.max=lag) + 
        theme_bw() + 
        theme(text = element_text(size = 15, family = family)) + 
        labs(title="") + 
        scale_x_continuous(expand=c(0.01,0))  
    
    # predicted.p.value <- c()
    # n <- length(observed)
    # for(i in (1:n)){
    #     predicted.p.value[i] <- INLA::inla.pmarginal(q = observed[i], marginal = inla.model$marginals.fitted.values[[i]])
    # }
    # 
    # # Plot histogram of predicted p values
    # p2 <- ggplot() + 
    #     geom_histogram(aes(predicted.p.value), color="black", fill="grey") + 
    #     theme_classic() + 
    #     theme(text = element_text(size = 15, family = "Lato")) + 
    #     labs(x="Posterior predictive probability", y="Counts") + 
    #     scale_y_continuous(expand=c(0,0))
    
    # Plot observed vs residuals 
    p5 <- ggplot() + 
        geom_point(aes(x = inla.model$summary.fitted.values$mean[1:length(observed)], y = observed), shape=1) + 
        geom_abline(intercept=0) + 
        theme_bw() + 
        theme(text = element_text(size = 15, family = family)) + 
        labs(x="Fitted", y="Observed") + 
        scale_x_continuous(expand=c(0,0)) + 
        scale_y_continuous(expand=c(0,0))
    
    pa <- plot_grid(p2, p3, rel_widths=c(2,1), ncol=2, align="vh")
    pb <- plot_grid(p4, p5, rel_widths=c(2,1), ncol=2, align="vh")
    p <- plot_grid(p1,pa,pb,nrow=3)
}



# Time series decomposition  ------------------------------------------------------------------

ts_decomposition <- function(x, df, date=NULL, scales="free", label=NULL,date_breaks="2 months", date_labels = "%b\n%y",
                             family = "Lato"){
    if(length(x$summary.random) == 0) stop('No random effects to plot')
    allSummary <- lapply(seq_len(length(x$summary.random)), 
                         function(p) data.frame(x$summary.random[[p]], var = names(x$summary.random)[p]))
    allSummary <- do.call(rbind, allSummary)
    allSummary <- allSummary %>% 
        mutate(ID=as.numeric(factor(ID)),
               date = rep(date,length(x$summary.random)))
    allSummary <- left_join(allSummary,df) %>% 
        mutate(beta.obs = case_when(var %in% c("ind1","j") ~ mean, 
                                    !(var %in% c("ind1","j")) ~ mean*obs),
               beta.obs.l = case_when(var %in% c("ind1","j") ~ X0.025quant, 
                                    !(var %in% c("ind1","j")) ~ X0.025quant*obs),
               beta.obs.h = case_when(var %in% c("ind1","j") ~ X0.975quant, 
                                    !(var %in% c("ind1","j")) ~ X0.975quant*obs),
               var = factor(var, levels=unique(var)))
    
    ggplot(allSummary, aes(x = date, y = beta.obs)) +
        # plot 
        geom_line(color="black") +
        geom_ribbon(aes(ymin = beta.obs.l, ymax = beta.obs.h), alpha = 0, color="blue") + 
        facet_wrap(~var, scales = scales, ncol = 1,
                   labeller=labeller(var=label)) +
        # theme 
        theme_bw() + 
        theme(strip.background = element_rect(color="white", fill="white", size=0, linetype="solid"),
              text = element_text(size = 12, family = family)) + 
        # axis 
        labs(x="", y=bquote(beta*obs),title="Time series decomposition") + 
        scale_x_datetime(date_breaks=date_breaks, date_labels=date_labels, expand=c(0,0))
    
}



# Plot fixed effects with ggregplot 
# 
Efxplot <- function (ModelList, Sig = TRUE, StarLoc = NULL, Alpha1 = 1, 
          Alpha2 = 1, PointOutline = F, ModelNames = NULL, VarNames = NULL, 
          VarOrder = NULL, Intercept = TRUE, Size = 1, tips = 0.2) 
{
    require(dplyr)
    require(ggplot2)
    require(INLA)
    require(MCMCglmm)
    Graphlist <- list()
    if (!class(ModelList) == "list") {
        ModelList <- list(ModelList)
    }
    for (i in 1:length(ModelList)) {
        model <- ModelList[[i]]
        Graph <- as.data.frame(model$summary.fixed)
        colnames(Graph)[which(colnames(Graph) %in% c("0.025quant", 
                                                     "0.975quant"))] <- c("Lower", "Upper")
        colnames(Graph)[which(colnames(Graph) %in% c("0.05quant", 
                                                     "0.95quant"))] <- c("Lower", "Upper")
        colnames(Graph)[which(colnames(Graph) %in% c("mean"))] <- c("Estimate")
        Graph$Model <- i
        Graph$Factor <- rownames(Graph)
        Graphlist[[i]] <- Graph
    }
    Graph <- bind_rows(Graphlist)
    Graph$Sig <- with(Graph, ifelse(Lower * Upper > 0, "*", 
                                    ""))
    Graph$Model <- as.factor(Graph$Model)
    if (!is.null(ModelNames)) {
        levels(Graph$Model) <- ModelNames
    }
    position <- ifelse(length(unique(Graph$Model)) == 1, "none", 
                       "right")
    if (is.null(VarOrder)) 
        VarOrder <- rev(unique(Graph$Factor))
    if (is.null(VarNames)) 
        VarNames <- VarOrder
    Graph$Factor <- factor(Graph$Factor, levels = VarOrder)
    levels(Graph$Factor) <- VarNames
    Graph %<>% as.data.frame %>% filter(!is.na(Factor))
    if (!Intercept) {
        VarNames <- VarNames[!str_detect(VarNames, "ntercept")]
        Graph <- Graph %>% filter(Factor %in% VarNames)
    }
    Graph$starloc <- NA
    min <- min(Graph$Lower, na.rm = T)
    max <- max(Graph$Upper, na.rm = T)
    if (Sig == TRUE) {
        Graph$starloc <- max + (max - min)/10
    }
    if (!is.null(StarLoc)) {
        Graph$starloc <- StarLoc
    }
    Graph$Alpha <- with(Graph, ifelse(Lower * Upper > 0, Alpha1, 
                                      Alpha2))
    Graph <- Graph %>% mutate(SigAlpha = factor(as.numeric(Lower * 
                                                               Upper > 0), levels = c(1, 0)))
    if (PointOutline) {
        PointOutlineAlpha <- Alpha1
    }
    else {
        PointOutlineAlpha <- 0
    }
    ggplot(Graph, aes(x = as.factor(Factor), y = Estimate, group = Model, 
                      colour = Model, alpha = SigAlpha)) + geom_point(position = position_dodge(w = 0.5), 
                                                                      size = Size) + geom_errorbar(position = position_dodge(w = 0.5), 
                                                                                                   aes(ymin = Lower, ymax = Upper), size = 0.3, width = tips) + 
        geom_hline(aes(yintercept = 0), lty = 2) + labs(x = NULL) + 
        coord_flip() + theme(legend.position = position) + geom_text(aes(label = Sig, 
                                                                         y = starloc), position = position_dodge(w = 0.5), show.legend = F) + 
        scale_alpha_manual(values = c(Alpha1, Alpha2)) + guides(alpha = "none") + 
        geom_point(colour = "black", aes(group = Model), position = position_dodge(w = 0.5), 
                   size = 4, alpha = PointOutlineAlpha) + geom_errorbar(aes(ymin = Lower, 
                                                                            ymax = Upper, group = Model), width = 0.1, position = position_dodge(w = 0.5), 
                                                                        colour = "black", alpha = PointOutlineAlpha) + geom_point(position = position_dodge(w = 0.5), 
                                                                                                                                  size = 3, alpha = PointOutlineAlpha)
}

