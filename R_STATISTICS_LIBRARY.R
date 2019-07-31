# =================================== #
# =================================== #
#         R STATISTICS LIBRARY        #
#                                     #
#              Peter Lyu              #
#         (plyu@g.harvard.edu)        #
#         (plyu2400@gmail.com)        #
#                                     #
# ----------------------------------- #
# Last Updated: 05/13/2019            #
# =================================== #
# =================================== #


#*******************************************************************************************************#
###### Table of Contents ######
# The library consists of the following sections, organized by purpose:
#   (I)   MANAGEMENT
#   (II)  EXPLORATION
#   (III) MODELING
#   (IV)  DIAGNOSTICS/EVALUATION
#   (V)   REPORTING

# Packages with useful example datasets
library(car)
library(faraway)
#*******************************************************************************************************#


#*******************************************************************************************************#
##### SECTION I: MANAGEMENT #####
##### Part A: Reading Data #####
  # Read in different data types
    # SAS data ("read_sas" imports data as tables)
      library(haven)
      dat <- as.data.frame(read_sas("dat.sas7bdat"))
    # Stata data
      # Note: 'foreign' package can't read beyond Stata 12 data
      library(foreign)
      dat <- read.dta("dat.dta")
      library(haven)
      dat <- as.data.frame(read_dta("dat.dta"))
    # CSV data
      dat <- read.csv(file="dat.csv", header=T, sep=",")

  # Read in series of data files, all with same prefix, as a list object
    lapply(Sys.glob("data*.csv"),read.csv)

  # Read in series of data files, separately
    for(i in 1:4) {
      nam <- paste("data_", i, sep = "")
      assign(nam, read.csv(paste("data_", i,".csv", sep="")))
    }

  # Determine class of all variables of a dataframe
    lapply(dat,class)

##### Part B: Transposing Data #####
  # Long-to-Wide
    # NOTE: Requires "timevar" who's values will be appended to transposed variables
    # IMPORTANT: reshape() requires a DATAFRAME argument! Will not work correctly with DATATABLE objects
    # Adding time variable (i.e. counter)
      data$i <- with(data,ave(id,id,FUN=seq_along))
      reshape(data,idvar="id",timevar="i",direction="wide")
      
  # Wide-to-Long
    # NOTE: Much easier when dat is limited to only those variables which are being transposed
      reshape(dat,idvar="id",varying=c("var_1","var_2","var_3"),v.names="var",sep="_",direction="long")
      reshape(dat,idvar="id",varying=c(2:4),v.names="var",sep="_",direction="long")
      
##### Part C: Loops #####
  # For [index] in vector of variables
      for (i in c(1,2,3,4)) {
        var = i
      }
  # For [index] in sequence
      for (i in 1:4) {
        var = i
      }
      
##### Part D: Parallel Processing #####
  # Using dopar
      library(foreach)
      library(doParallel)
    # STEP 1: Data cleaning/setup before processes that require parallelizing
    # STEP 2: Set number of cores to use (below code uses all minus one cores)
      no_cores <- detectCores() - 1
    # STEP 3: Generate n_cores clusters based on local environment
      cl <- makeCluster(no_cores)
    # STEP 4: Register clusters (to identify which cluster set to be used for parallelized processing)
      registerDoParallel(cl)
    # STEP 5: Loop processes, which assign different independent loop iterations to different cores
      # Example 1: Parallel Fits 
      #   Fits different models with different formulas (contained in list and defined
      #   prior to makeCluster() and outputs a nested list. E.g. all_fits[[1]][[1]] contains the estimates
      #   from Model 1 and all_fits[[1]][[2]] contains the robust var-cov matrix of Model 1.
      all_fits <- foreach(i=1:n_models,
                          .packages = c("sandwich","lmtest","foreach","doParallel"),
                          .combine = list,
                          .multicombine = T) %dopar% {
        model <- lm(formulas[[i]], data=dat)
        cluster_vcov <- vcovHC(model, type="HC1", cluster="clustervar")
        list(model,cluster_vcov)
                          }
      # Example 2: Parallel Prints
      #   Prints different sets of models together using stargazer package.
      combos_models <- list(list(all_fits[[1]][[1]], all_fits[[2]][[1]], all_fits[[3]][[1]]),
                            list(all_fits[[4]][[1]]),
                            list(all_fits[[5]][[1]]))
      combos_se <- list(list(sqrt(diag(all_fits[[1]][[2]])), sqrt(diag(all_fits[[2]][[2]])), sqrt(diag(all_fits[[3]][[2]]))),
                        list(sqrt(diag(all_fits[[4]][[2]]))),
                        list(sqrt(diag(all_fits[[5]][[2]]))))
      foreach(1:3, .packages=c("stargazer"),.export=c("combos_models","combos_se")) %dopar% {
        stargazer(combo_models[[i]],
                  align=T,
                  omit.stat="f",
                  type="html",
                  se=combos_se[[i]],
                  out=paste("address/Model Results - Set ",i,".html",sep=""))
                        }
    # STEP 6: Stop/Close clusters
      stopCluster(cl)
      
##### Part E: Saving & Exporting Data #####
  # Saving R object
    # Saving in Rdata form (simplest)
      save(obj,file="Object.Rdata")
      load("Object.Rdata")
    # Saving in RDS form (allows to be read in with different name)
      saveRDS(obj,file="Object.rds")
      objnew <- readRDS("Object.rds")
      
  # Saving R workspace
    save.image(file="Workspace.Rdata")
    
  # Exporting dataframe as CSV
    write.csv(dat,file="dat.csv",row.names=F)
      # To replace missing (NA) with blanks
        write.csv(dat,file="dat.csv",row.names=F,na="")
      
##### Part F: Miscellaneous #####
  # Identify Duplicates
    # For specific variable in dataset
      dat$dup <- duplicated(dat[,c("var")])
    # Of entire observations/rows
      dat$dup <- duplicated(dat)

#*******************************************************************************************************#


#*******************************************************************************************************#
##### SECTION II: EXPLORATION #####
#*******************************************************************************************************#
##### Part A: Missingness #####
  # Count Missingness Function
    misscount <- function(data){
      n <- rep(NA, ncol(data))
      for (i in 1:ncol(data)){
        n[i] <- sum(is.na(data[,i])) 
      }
      table <- rbind(n)
      colnames(table) <- c(colnames(data))
      rownames(table) <- c("# of missing values")
      return(table)
    }
    
  # Frequency Table Function + View Frequency Tables Function:
    # Creates a LIST where each element contains a frequency DATAFRAME table for each variable of the
    # input dataframe.
    freq.tables <- function(data) {
      nvar <- ncol(data)
      list <- vector("list",nvar)
      for (i in 1:nvar) {
        if (class(data[,i]) == "factor") data[,i] <- droplevels(data[,i])
        list[[i]] <- as.data.frame(table(data[,i],useNA="always"))
      }
      names(list) <- colnames(data)
      return(list)
    }
    
    # Reads LIST output from freq.tables() and calls the R Viewer to VIEW frequency tables selected
    # by vector argument
    view.freq.tables <- function(list,vector) {
      vector_length <- length(vector)
      for (i in 1:vector_length) {
        j <- vector[i]
        View(list[[j]],names(list)[j])
      }
    }
    
  # Missingness Combinations Grid:
    # Creates a GGPLOT tile plot of missing data pattern sorted by increasing frequency of missingness
    # (left to right, bottom to top); dataframe must have missing data set as NA
    miss_comb_plot <- function(data) {
      require(ggplot2)
      mdpattern <- md.pattern(data)
      ncombs <- nrow(mdpattern)
      nmiss <- ncol(mdpattern)
      rownames(mdpattern) <- 1:ncombs
      patternorder <- colnames(mdpattern)[-(ncol(mdpattern))]
      plotmatrix <- t(mdpattern[-ncombs,-nmiss])
      x1 <- melt(plotmatrix)
      names(x1) <- c("x","y","Present")
      x1$Present <- factor(x1$Present>0)
      x1$x <- factor(x1$x,levels=patternorder)
      levels(x1$Present) = c("0","1")
      ggplot(x1, aes(x=x,y=y)) + geom_tile(aes(fill=Present)) + 
        labs(x="Variable Name", 
             y="Missingness Combination \n(Least Frequent -> Most Frequent)") +
        theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5), 
              axis.text.y=element_blank())
    }
    
  # Missing Category Plots - Non-Weighted + Survey Weighted
    # Non-Weighted: creates GGPLOT bar plot(s) with each bar representing a category of quoted variable(s) 
    # in vector_x1 (including an NA category) and each bar subsequently broken percentage-wise by quoted
    # variable(s) in vector_x2. These proportions are UNWEIGHTED and based entirely on the input dataframe.
    # ARGUMENTS:
      # data = dataframe with missing data set as NA
      # vector_x1 = vector of quoted variable names (e.g. c("RACE","REGION")); 
        # for each x1, the data will be divided by the categories of x1 (i.e. bars by x1 levels)
      # vector_x2 = same as vector_x1; for each level of x1, the distribution for each x2 will be broken 
        # out (i.e. stacks by x2 levels)
      # ngridcol = number of columns for grid of plots (e.g. ngridcol=2 arranges 4 plots in 2x2 grid)
    misscat_plot_nowt <- function(data,vector_x1,vector_x2,ngridcol) {
      require(gridExtra)
      require(reshape)
      gridarrange_args <- list()
      nx1 <- length(vector_x1)
      nx2 <- length(vector_x2)
      for (j in 1:nx1) {
        x1 <- vector_x1[j]
        data[,x1] <- droplevels(as.factor(data[,x1]))
        for (i in 1:nx2) {
          var_x2 <- vector_x2[i]
          data[,var_x2] <- droplevels(as.factor(data[,var_x2]))
        }
        for (i in 1:nx2) {
          x2 <- vector_x2[i]
          df <- as.data.frame(prop.table(table(data[,x1],
                                               data[,x2],
                                               useNA="always"),margin=1))
          df$Freq <- df$Freq*100
          df2 <- melt(df,id.vars=c("Var1","Var2"))
          gridarrange_args[[nx2*(j-1)+i]] <- ggplot(df2,aes(x=Var1,
                                                            y=value,
                                                            fill=Var2)) + 
            geom_bar(stat="identity") +
            labs(x=x1,y="Percent") + scale_fill_discrete(name=x2) +
            theme(axis.text.x=element_text(angle=45,hjust=1,vjust=1))
        }
      }
      plot <- do.call("grid.arrange",c(gridarrange_args,ncol=ngridcol))
      return(plot)
    }
    
    # Same as above function, EXCEPT these proportions are WEIGHTED and based on the predefined survey 
    # design object (see survey package) input. 
    # IMPORTANT: the svyobject argument must be built on a dataframe where <NA> are recognized as factor 
    # levels because some survey package functions don't account for missing observations (e.g. 
    # svytable() doesn't have a "useNA" option like table()). See code at bottom for more details. 
    # The NAlevel() function directly below can be used to do this.
    # ARGUMENTS:
      # svyobject = svydesign() output built from dataframe and the appropriate survey design features 
        # (e.g. clustering structure, weighting); requires that factor variables in original dataframe 
        # have NA values set as an actual level of those variables (see supplementary code below)
    misscat_plot_wt <- function(svyobject,vector_x1,vector_x2,ngridcol) {
      require(gridExtra)
      require(reshape)
      gridarrange_args <- list()
      nx1 <- length(vector_x1)
      nx2 <- length(vector_x2)
      for (j in 1:nx1) {
        x1 <- vector_x1[j]
        for (i in 1:nx2) {
          x2 <- vector_x2[i]
          df <- as.data.frame(prop.table(do.call(svytable,
                                                 list(formula=as.formula(paste("~",
                                                                               x1,"+",x2,sep="")),
                                                      design=svyobject)),margin=1))
          df$Freq <- df$Freq*100
          df2 <- melt(df,id.vars=c(x1,x2))
          colnames(df2) <- c("Var1","Var2","variable","value")
          gridarrange_args[[nx2*(j-1)+i]] <- ggplot(df2,aes(x=Var1,
                                                            y=value,fill=Var2)) + 
            geom_bar(stat="identity") +
            labs(x=x1,y="Percent") + scale_fill_discrete(name=x2) +
            theme(axis.text.x=element_text(angle=45,hjust=1,vjust=1))
        }
      }
      plot <- do.call("grid.arrange",c(gridarrange_args,ncol=ngridcol))
      return(plot)
    }
    
    NAlevel <- function(x) {
      if(is.factor(x)) return(addNA(x)) else x
      return(x)}
    dataframe_corrected <- as.data.frame(lapply(dataframe,NAlevel))

    
##### Part B: Outliers #####
  # Find Outlier Function
    find.outliers <- function(data){
      boxplot(data, main="Assessing Outliers")
      all.out<-list()
      for(i in 1:dim(data)[2]){
        if (is.factor(data[,i]) == TRUE){
          save.out <- "Factor"
        } else if (length(boxplot(data[,i], plot=FALSE)$out) == 0){
          save.out <- "No outlier"
        } else {
          save.out<-boxplot(data[,i], plot=FALSE)$out
        }
        all.out<-list(all.out, save.out) 
      }
      return(all.out)
    }

    
##### Part C: Summarizing Data #####
  # IQR+Mean
      summary(data)
    
  # Mean and Standard Deviation
      cbind.data.frame(Mean=sapply(dat,mean),
                       SD=sapply(dat,mean))
    
  # Means by Categorical Variable
    # Using Base R (slower)
      aggregate(data_variables,by=list(dat$by_variable),mean)
    # Using data.table (much faster)
      # A note about the data.table() syntax: DTs take three arguments that resemble SQL language:
      #   i : "where"
      #   j : "select" or "update"
      #   by: "group by"
      library(data.table)
      dat[,list(mean_of_var=mean(data_variables)),by=list(by_variable)]
      
  # Frequency Table with Proportions and Cumulative Proportions
      proc.freq <- function(x){ 
        Table <- data.frame( table(x) ) 
        Table$Prop <- prop.table( Table$Freq ) 
        Table$CumProp <-  cumsum( Table$Prop ) 
        Table
      }
    
      
##### Part D: Visualizing Data #####
  # GGPLOT COLORS: http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/#palettes-color-brewer
    
  # Overlapping Density Plots
      library(ggplot2)
      ggplot(dat,aes(x=propensity,fill=as.factor(treat)))+
       geom_density(alpha=0.3)+
       scale_fill_discrete(name="Treated?",breaks=c(0,1),labels=c("No","Yes"))+
       labs(x="Predicted Propensity",y="Density")
    
  # Histogram (bottom) + Boxplot (top)
      hist(var,main="Title",xlab="Var",xlim=c(40,90),ylim=c(0,300))
      boxplot(var,horizontal=T,frame=F,xaxt="n",ylim=c(40,90),add=T,at=300,boxwex=30)
    
  # Grouped Bar Plot
      ggplot(dat_long, aes(var_grouping,var_outcome,fill=var_category)) +
        geom_bar(stat="identity",position="dodge") +
        scale_fill_brewer(palette = "Set2") +
        xlab("X Axis") + ylab("Y Axis")
    
  # Line Smoothers
    # Running-mean
    # Running-line (local OLS)
    # Kernal Smoothing (running-mean at x_i weighted by distance from x_i)
    # LOWESS (Locally-Weighted Scatterplot Smoothing)
      line(lowess(x,y))
      
  # Binned Scatterplots
    # Using ARM Package
      library(arm)
      binnedplot(x,y)
    # Manually - allows binned scatterplots by group
      # Note: number (e.g. 20) specifies number of bins, otherwise determined automatically
      library(arm)
      df <- as.data.frame(binned.resids(x,y,20)$binned)
      plot(df$xbar,df$ybar)
      
  # GIF - Animated line graph
    # Using gganimate, notes:
        # gganimate is an extension to ggplot2
        # requires rendering package(s): gifski, magick, png
      library(ggplot2)
      library(gganimate)
      library(magick)
      library(gifski)
      library(png)
      p <- ggplot(dat, aes(x=x_var, y=y_var, group=cohort)) +
            geom_line() +
            #This geom_segment statement connects datapoint to group label
            geom_segment(aes(xend=7.2, yend=n_aco), linetype=2, colour="grey") +
            geom_point(size=2) +
            #This geom_text statement adjusts group label
            geom_text(aes(x=7.3, label=cohort), hjust=0) +
            #Set x-axis interval
            scale_x_continuous(breaks=seq(1,7,1)) +
            #Animate by tracking x-y coordinates by group over years
            transition_reveal(year) +
            #Establsh bounds of graph space
            coord_cartesian(clip="off", xlim=c(1,7.1), ylim=c(48,225)) +
            labs(title="Year is {frame_along}", 
                 caption="This is the caption",
                 x="X Variable", 
                 y="Y Variable") +
            theme_minimal() +
            #Margins will need to be set such that labels and axis titles fall within margins
            theme(plot.margin=margin(5.5,100,50,5.5),
                  panel.grid.minor=element_blank(),
                  axis.title.x=element_text(vjust=-11),
                  plot.caption=element_text(vjust=-17),
                  plot.title=element_text(size=22)) +
            #This extra text manually inserts x-axis subgroup labels (i.e. if X variable has groupings)
            annotate(geom="text",x=c(2,5,7),y=25,label=c("AP1","AP2","AP3"),colour="grey") +
            #These extra segments help visually separate x-axis values into the subgroups labeled in annotate
            geom_segment(aes(x=0.8,xend=3.3,y=29,yend=29),colour="grey") +
            geom_segment(aes(x=3.7,xend=6.3,y=29,yend=29),colour="grey") +
            geom_segment(aes(x=6.7,xend=7.3,y=29,yend=29),colour="grey")
      #Animate
      anim_plot <- animate(p,fps=8,end_pause=20,height=600,width=600)
      #Save
      anim_save("My GIF.gif")
      
#*******************************************************************************************************#

    
#*******************************************************************************************************#
##### SECTION III: MODELING #####
#*******************************************************************************************************#
##### Part A: Tests #####
  # T-Test
    # Independent 2-group (Y is NUMERIC, X is binary FACTOR)
      t.test(Y~X,data=dat)
    # Independent 2-group (Y and X are NUMERIC)
      t.test(dat$Y,dat$X)
  
  # Chi-Squared Test
    # Test (Y and X are FACTOR)
      chisq.test(table(dat$Y,dat$X))
    # Check expected counts (if any cells <5, use Fisher's exact test)
      chisq.test(table(dat$Y,dat$X))$expected
      
  # Fisher's Exact Test
    # Test
      fisher.test(table(dat$Y,dat$X))
      
  # Profile Likelihood Confidence Interval (values for which LRT is not significant)
      confint(model)
      
  # Hosmer-Lemeshow Test
    # (per Tsugawa & Rokicki) Goodness-of-fit often used for logistic regression models. It partitions
    # the observations into "g" equal sized groups according to their predicted probabilities. Then, it
    # compares the difference between the observed number of cases and expected number of cases to the
    # Chi-Squared distribution.
      hosmerlem <- function (y, yhat, g = 10) {
        cutyhat <- cut(yhat, breaks = quantile(yhat, probs = seq(0, 1, 1/g)), include.lowest = TRUE)
        obs    <- xtabs(cbind(1 - y, y) ~ cutyhat)
        expect <- xtabs(cbind(1 - yhat, yhat) ~ cutyhat)
        chisq  <- sum((obs - expect)^2 / expect)
        P      <- 1 - pchisq(chisq, g - 2)
        c("X^2" = chisq, Df = g - 2, "P(>Chi)" = P)
      }
      
  # One-Way ANOVA
      aov(Y~X,data=dat)
      
  # Rank Sum Test (aka: Mann-Whitney test, Wilcoxon rank-sum test)
    # Notes: Non-parametric test
    # H0: True quantiles/distribution of ranks between groups are the same
    # H1: True quantiles/distribution of ranks between groups are not the same
      wilcox.test(formula,data=dat)
    
  # Sign Test (aka: binomial test)
    # Notes: Non-parametric test, for paired data, tests proportion that falls under zero-difference line;
    #         limited meaningful use
    # H0: p=0.5; H1: p!=0.5
      binom.test(x,n)
    
  # Signed-Rank Test (aka: Wilcoxon signed-rank test)
    # Notes: Non-parametric test, for paired data
    # H0: sign of rank is unrelated to magnitude; H1: ...is related...
      wilcox.test(formula,data=dat,paired=T)
      
##### Part B: LSE & MLE Models #####
  # Ordinary Least Squares (OLS)
    # Note: Using conventional OLS to estimate linear probability models ignores heteroscedasticity
    #       (via variance function) that is inherent to these types of models. Recall, for normally
    #       distributed data, means are independent of variance.
    # Fitting
      lm(formula,data=dat)
      
    # Robust Standard Error Estimation
      library(sandwich)
      library(lmtest)
      # Getting robust variance-covariance matrix
        vcovHC(model,type="HC0")
          # "HC0" or "HC" => omega_i = u_hat_i^2
            # White Estimator; the conventional sandwich estimator
          # "HC1" => omega_i = n/(n-k) * u_hat_i^2; the default in Stata
          # "HC2" => omega_i = 1/(1-h_i) * u_hat_i^2
          # "HC3" => omega_i = 1/(1-h_i)^2 * u_hat_i^2
            # MacKinnon-White Estimators (HC1-HC3) incorporate diag of hat matrix (h) and degrees of 
              # freedom to perform better in small sample sizes
          # "HC4" => omega_i = 1/(1-h_i)^delta_i * u_hat_i^2, where delta_i = min{4,h_i/h_bar}; 
            # Cribari-Neto Estimator
      # Clustered SE on specific cluster variable
        vcovHC(model,type="HC0",cluster="variable")
      # Fitting model with robust variance estimation
        coeftest(model,vcov=vcovHC(model,type="HC0"))
        
    # Step-wise Model Selection (for variable selection)
      step(initialmodel,scope=list(lower=formula,upper=formula),direction="forward")
  
  # Generalized Linear Models (GLM)
    # Binomial - Logit Link (Logistic Model)
        glm(formula,data=dat,family=binomial(link="logit"))
        # Getting OR Estimates
          exp(summary(model)$coef[,1])
        # Getting 95% OR Confident Intervals via Profile Likelihood (LRT-based)
          # Remember: OR standard errors are useless!
          exp(confint(model))
          
    # Binomial - Probit Link
        glm(formula,data=dat,family=binomial(link="probit"))
        
    # Poisson - Log Link (Log-Linear Model)
        # Note: see Goodness-of-Fit section for tests of overdispersion (but tweak specification
        #         before changing to negative binomial); V(mu)=mu
        glm(formula,data=dat,family=poisson(link="log"))
        # Using rate parameter offset (T_i = denominator time for counts for obs i)
          glm(y~offset(log(T_i))+x1+x2,family=poisson(link="log"),data=dat)
        # Dispersion-adjusted standard errors (normally, phi=1; here, use empirical estimate)
          dp <- sum(residuals(model,type="pearson")^2)/model$df.residual
          summary(model,dispersion=dp)
          
    # Quasi-Poisson
      # Note: useful when overdispersion detected
        glm(formula,data=dat,family=quasipoisson)
        
    # Quasi-Binomial (for true proportions)
      # Note: useful when overdispersion detected
        glm(formula,data=dat,family=quasibinomial(link="logit"))
        
    # Zero-Inflated Count Models
        library(pscl)
        # Zero-Inflated Poisson (mixed distribution of binomial + Poisson)
          # Note: w's are part 1 predictors (binomial portion)
          #       x's are part 2 predictors (Poisson portion)
          zeroinfl(y~x1+x2+x3|w1+w2,data=dat,dist="pois")
        # Hurdle Poisson (separate distributions of binomial + Poisson, i.e. zeros have separate dist)
          hurdle(y~x1+x2+x3|w1+w2,data=dat,dist="pois")
        # Zero-Inflated Negative Binomial
          zeroinfl(y~x1+x2+x3|w1+w2,data=dat,dist="negbin")
        # Hurdle Negative Binomial
          hurdle(y~x1+x2+x3|w1+w2,data=dat,dist="negbin")
          
    # Gamma
      # Note: useful when continuous outcome is positive skewed; V(mu)=mu^2
        glm(formula,data=dat,family=Gamma(link="log"))
        
    # Inverse Gaussian
      # Note: useful when continuous outcome is *very* positive skewed: V(mu)=mu^3
        glm(formula,data=dat,family=inverse.gaussian(link="log"))
        
    # Tweedie
      # Note: useful when continuous outcome is *extremely* positive skewed: V(mu)=mu^p
        library(mgcv)
        gam(formula,data=dat,family=tw(link="log"))
        
  # Bias-Reduced Logistic Regression (BRGLM)
    # Note: for GLMs with perfect or near-perfect separation in data; effectively generates
    #       a "prior" by adding small contributions of successes/failures
    # IMPORTANT: MLE inference no longer valid -> must use bootstrapping to estimate CIs
      library(brglm)
      brglm(formula,family=binomial(link="logit"),data=dat)
      
    
  # Marginal Effects (for non-linear models)
    # Margins (type="response" (default) or "link")
      library(margins)
      # AME - Average Marginal Effects; evaluates average of MFXs at each value of dataset
        summary(margins(model,type="response"))
      # MEM - Marginal Effects at the Mean; evaluates MFX at the means of each variable
        summary(margins(model,type="response",atmeans=T))
      # MFX at specific values
        summary(margins(model,type="response",at=list(var1=0:1,var2=fivenum(dat$var2))))
    # MFX (warning - does not handle interactions well)
      library(mfx)
      # Logit
        logitmfx(formula,data=dat,atmean=T)
      # Probit
        probitmfx(formula,data=dat,atmean=T)
      # Other model forms: negbinmfx(), betamfx(), poissonmfx()
        
  # Negative Binomial Model
    # Note: useful when overdispersion detected; "weighted avg of Poisson distributions"
    # IMPORTANT: Avoid using "NULL deviance" from output bc fixes negbin alpha parameter
      library(MASS)
      # Fit, allowing k parameter to vary and be estimated using ML
      glm.nb(formula,data=dat)
      # Fit, fixing k parameter (k=1: geometric response)
      glm(formula,data=dat,negative.binomial(1))
      
  # Beta (for true proportions)
    # Option 1
      library(betareg)
      betareg(formula,data=dat)
    # Option 2 (should be same as betareg())
      library(mgcv)
      gam(formula,data=dat,family=betar(link="logit"))
      
  # Multinomial Logistic Model (where Y is FACTOR)
      library(nnet)
      # Change reference level of outcome
        within(data,Y<-relevel(Y,ref="factorlevel"))
      # Fitting
        multinom(formula=factor(Y)~X1+X2+X3,data=dat)
        
  # Ordered Multinomial Logistic Model (where Y is FACTOR and ordered)
      library(MASS)
    # Convert factor into ordered factor
      ordered_Y <- ordered(factor_Y,levels=c("lowest level","middle level","highest level"))
    # Proportional Odds Model (logit link)
    # Note: Estimates thresholds between factor levels (i.e. for discrete choice decision).
    #       Estimated coefficients interpreted as change in log-odds of falling in higher category
      polr(ordered_Y~X1+X2+X3,data=dat,Hess=T)
    # Ordered Probit Model (probit link)
      polr(ordered_Y~X1+X2+X3,data=dat,method="probit")
    # Proportional Hazards Model (complementary log-log link)
      polr(ordered_Y~X1+X2+X3,data=dat,method="cloglog")
        
  # Multilevel/Hierarchical Models
      # Linear Mixed-Effects Model (LMM)
        library(lme4)
        # Varying Intercept
          lmer(y~var1+var2+var3+(1|ClusterVar),data=dat)
        # Varying Slope
          lmer(y~var1+var2+var3+(var4|ClusterVar),data=dat)
        # Varying Intercept & Slope
          lmer(y~var1+var2+var3+(1+var4|ClusterVar),data=dat)
        # Nested Structure (ClusterVar2 nested within ClusterVar1)
          lmer(y~var1+var2+var3+(1|ClusterVar1/ClusterVar2),data=dat)
          lmer(y~var1+var2+var3+(1|ClusterVar1)+(1|ClusterVar2:ClusterVar2),data=dat)
        # Crossed Structure (ClusterVar2 not nested within ClusterVar1)
          lmer(y~var1+var2+var3+(1|ClusterVar1)+(1|ClusterVar2),data=dat)
      # Generalized Linear Mixed-Effects Model (GLMM)
        library(lme3)
        glmer(y~var1+var2+var3+(1|ClusterVar),data=dat,family=binomial(link="logit"))
          
        
##### Part C: Non-Parametric Models #####
  # Generalized Additive Model (GAM)
      # Note: k limits "wiggliness" of smooth for given term
        library(mgcv)
        gam(y~s(x1)+s(x2)+s(x3,k=5),data=dat,family=binomial)
      # With simultaneous smooth of two variables (ti=tensor interaction)...
        gam(y~s(x1)+s(x2)+ti(x1,x2),data=dat,family=binomial)
        gam(y~te(x1,x2),data=dat,familiy=binomial)
      # With different smooth by factor variable...
        gam(y~w1+s(x1,by=w1),data=dat,family=binomial)
        
      # Check smoothness parameters
        gam.check(model_gam)
        
  # Regression/Classification Trees (for quantitative/categorical outcomes)
      # Classification Tree - Split based on reduction in deviance stat.
        library(rpart)
        rpart(Y~X1+X2+X3,data=dat,method="class",parms=list(split="information"))
      # Classification Tree - Split based on reduction in Gini stat.
        library(rpart)
        rpart(Y~X1+X2+X3,data=dat,method="class",parms=list(split="gini"))
      # Regression Tree
        library(rpart)
        rpart(Y~X1+X2+X3,data=dat,method="anova")
      # Penalized Tree
        library(rpart)
        # Step 1: Fit tree w/o penalty
          rpart(Y~X1+X2+X3,data=dat,method="class",parms=list(split="information"))
        # Step 2: Independent cross-validation w/ penalized deviance to estimate alpha (penalty parameter)
          printcp(model)
          # CHOOSE LARGEST CP W/ XERROR W/IN 1-SE OF SMALLEST XERROR
          # Note: "cp" = candidate complexity parameter; alpha_hat/[deviance of null model]
          # Note: "xerror" = deviance statistic
          # Note: "xstd" = standard error of deviance statistic
        # Step 3: Prune original tree from Step 1 using deviance w/ estimated alpha
          prune(model,cp=chosen_cp)

  # Random Forests (i.e. Tree Averaging)
      # Estimate number of variables to try at each tree node
        cvr <- rfcv(data_predictors_only,outcome_var,step=0.9)
        cbind(nvars=cvr$n.var,error.rate=cvr$error.cv)
        # CHOOSE NVAR WITH LOWEST ERROR RATE
      # Fitting - Classification
        randomForest(Y~X1+X2+X3,m.try=nvar,na.action=na.roughfix,method="class")

       
##### Part D: Penalized Regression #####
  # Ridge Regression
    # Note: Creates linear reg model penalized with L2-norm (sum of squared coefficients);
    #       Effectively shrinks coefficients, particularly those with less contribution
      library(glmnet)
      glmnet(x,y,family="gaussian",alpha=0,lambda=0.001)
        
  # LASSO (Least Absolute Shrinkage and Selection Operator)
    # Note: Creates reg model penalized with L1-norm (sum of abs val coefficients);
    #       Similar effect as ridge regression, but actually sets some coefficients to zero
      library(glmnet)
      glmnet(x,y,family="gaussian",alpha=1,lambda=0.001)
      
  # Elastic Net
    # Note: Creates reg model penalized with both L1- and L2-norm (ridge + LASSO)
      library(glmnet)
      glmnet(x,y,family="gaussian",alpha=0.5,lambda=0.001)
      
        
##### Part E: Survival Models #####
  # Kaplan-Meier (KM) Curves
    # Plot KM Survival Curves
      library(survminer)
      # survfit: estimates survival proportion across time
      ggsurvplot(survfit(Surv(time,status)~Var1,data=dat),
                 conf.int=T,risk.table=T,risk.table.col="strata")
      plot(survfit,conf.int=T)
    # Plot KM Cumulative Event Curves (probability of any event by time t)
      ggsurvplot(survfit(Surv(time,status)~Var1,data=dat),
                 conf.int=T,risk.table=T,risk.table.col="strata",fun="event")
    # Plot KM Cumulative Hazard Curves (expected number of events by time t)
      ggsurvplot(survfit(Surv(time,status)~Var1,data=dat),
                 conf.int=T,risk.table=T,risk.table.col="strata",fun="cumhaz")
    # Plot Confidence BANDS (ensures 5% alpha across all time points, i.e. 95% coverage of entire curve)
    # NOTE: unclear whether conf.int option on survfit calculates point-wise CIs vs. confidence bands
      library(OIsurv)
      surv0 <- Surv(dat[treat==0,"time"],dat[treat==0,"status"])
      surv1 <- Surv(dat[treat==1,"time"],dat[treat==1,"status"])
      cb0 <- confBands(surv0,confLevel=0.95,type="hall")
      cb1 <- confBands(surv1,confLevel=0.95,type="hall")
      plot(survfit)
      lines(cb0$time,cb0$lower,lty=3,type="s")
      lines(cb0$time,cb0$upper,lty=3,type="s")
      lines(cb1$time,cb1$lower,lty=3,type="s")
      lines(cb1$time,cb1$upper,lty=3,type="s")
      
  # Cox Proportional Hazards Model (Semi-parametric bc h_0 unspecified function -> h(t,X)=h_0(t)exp(XB))
    # Fitting
      # Method: method for handling ties (default = "efron", others = "brewslow", "exact")
      # Time: time to event
      # Event: event status, s.t. 0 = alive/censored, 1 = dead
      library(survival)
      coxph(Surv(time,event)~Var1+Var2+Var3,data=dat,method="efron")
    # Plotting Predicted Survival Curves
      library(survminer)
      # At mean covariate values:
      ggsurvplot(survfit(cox_model),color="",ggtheme=theme_minimal())
      # By a specific covariate's values and at means elsewhere (via new dataframe as with "predict()"):
      ggsurvplot(survfit(cox_model,newdata=datnew),conf.int=T,legend.labs=c("Treatment","Control"))
        
      
##### Part F: Modeling Correlation #####  
  # Generalized Least Squares (GLS)
    # Autoregressive Correlation
      library(nlme)
      gls(formula,correlation=corAR1(form=~time_var),data=dat)
      
  # Generalized Estimating Equations (GEE)
    # geeglm() correlation structure forms:
      # "independence": cor(y_i,y_j) = 0
      # "exchangeable": cor(y_i,y_j) = alpha
      # "ar1": cor(y_i,y_j) = alpha^(|t_i-t_j|)
      # "unstructured" = cor(y_i,y_j) = alpha_ij
    # Clustered Structure
      # IMPORTANT: Data must be sorted by cluster (so rows in same cluster are contiguous)
        library(geepack)
        geeglm(formula,data=dat,family=binomial,id="cluster_var",corstr="exchangeable")
    # Autoregressive (AR1) Structure
      # IMPORTANT: Data must by sorted by id and time variable
        library(geepack)
        geeglm(formula,data=dat,family=binomial,id="id_var",corstr="ar1")
      
##### Part G: Matching & IPW #####
  # CAUTION: Standard errors from estimating on matched/weighted data do not take into account the
  #           uncertainty from the propensity score estimation -> solution? bootstrap entire PSM procedure
        
  # Propensity Score Matching
    # Single Nearest-Neighbor Matching
      library(arm)
      # Step 1: Predict propensity scores
        pscores <- predict(glm(formula,data=dat,family=binomial(link="logit")),type="response")
      # Step 2: Match via nearest neighbor
        matches <- matching(z=dat$treat,score=pscores)
      # Step 3: Subset data to matched data ONLY
        dat_matched <- dat[matches$matched,]
        
    # MatchIt
      # NOTE: If #treated >> #control, then reverse indicators so controls=1 and so treated are matched
      # to controls!
      # Why? Matching algorithm starts with values with largest distance by default; alternative option
      # is to adjust m.order="random", so picks random treated=1 and finds control match. However, it's
      # better to find matches for the smaller group, rather than find matches for larger group until
      # smaller group runs out of observations.
      # REFERENCE: https://cran.r-project.org/web/packages/MatchIt/vignettes/matchit.pdf
      library(MatchIt)
      # STEP 1: Match Observations
        # Exact Matching (match exactly on variables)
          matchit(formula,data=dat,method="exact")
        # Subclassification Matching (match distribution of covariates)
          matchit(formula,data=dat,method="subclass")
        # Nearest Neighbor Matching (smallest distance, but sequence-dependent)
          matchit(formula,data=dat,method="nearest",distance="logit",m.order="largest")
        # Nearest Neighbor Matching w/ Exact Matching on Specific Variables (~blocking)
          matchit(formula,data=dat,method="nearest",distance="logit",exact=c("var1"))
        # Optimal Matching (minimizes global average absolute distance between matched pairs)
          matchit(formula,data=dat,method="optimal",ratio=2)
        # Full Matching (global version of Subclassification Matching)
          matchit(formula,data=dat,method="full")
        # Genetic matching (uses genetic search algorithm?)
          matchit(formula,data=dat,method="genetic")
        # Coarsened Exact Matching (CEM: matches based on ex ante balance objectives)
          matchit(formula,data=dat,method="cem")
      # STEP 2: Check Balance
        summary(matchit_object,interactions=T)
        summary(matchit_object,standardize=T)
        plot(summary(matchit_object,standardize=T),type=jitter)
      # STEP 3: Create Matched (i.e. subsetted) Dataset
        match.data(matchit_object,distance="pscore")
        
    # Inverse Probability Weighting (IPW)
      # STEP 1: Estimate Propensities
      # STEP 2: Calculate Weights
        # For Treated
          weight = 1/pscore
        # For Comparison
          weight = 1/(1-pscore)

##### Part H: Instrumental Variables #####
      # 2-Stage Least Squares (2SLS)
        library(AER)
        formula <- y ~ x1 + x2 | z1 + z2
        ivreg(formula,data=dat)
        
##### Part I: Bayesian Modeling #####
  # FIRST: Code up RStan script 
      # Notes:
          # Saved as a .stan file (may need to manually change file extension)
          # Statements end with ";"
          # Comment lines start with "//"
          # Case-sensitive
          # To read in code from separate .stan, use "#include /file.stan"
    # Example: Linear Normal Model (with flat priors)
      data {
        int<lower=1> n; // sample size (nrow of design matrix)
        int<lower=1> k; // number of parameters (ncol of design matrix)
        vector[n] Y; // observed outcome data
        matrix[n,k] X; // observed design matrix
      }
      parameters {
        vector[K] beta; // coefficient parameters
        real<lower=0> sigma; // variance parameter
      }
      model {
        // priors (flat when not specified):
        increment_log_prob(-log(sigma));
        // data model:
        Y ~ normal(X*beta,sigma);
      }
    # Example: One-Parameter Binomial Model (with flat prior)
      data {
        int<lower=1> n; // rows of data
        int y[n];  // data
      }
      parameters {
        // binomial probability:
        real<lower=0, upper=1> theta;
      }
      model {
        // flat prior
        theta ~ uniform(0, 1);
        // data model:
        y ~ bernoulli(theta);
      }
    # Example: One-Parameter Poisson Model (with gamma prior)
      data {
        int<lower=1> n; // rows of data
        int y[n]; // data
      }
      parameters {
        // poisson rate
        real<lower=0> theta;
      }
      model {
        // priors:
        theta ~ gamma(1, 1);
        // data model:
        y ~ poisson(theta);
      }
    # Example: Hierarchical Logistic Model (with hyperpriors)
      data {
        int<lower=1> N; // Sample size
        int<lower=1> M; // Number of clusters
        int<lower=0> K1; // Number of random coefficient parameters
        int<lower=0> K2; // Number of fixed coefficient parameters
        int<lower=0,upper=1> Y[N]; // Observed dependent variable data
        matrix[N,K1] X1; // Design matrix for random coefficient parameters
        matrix[N,K2] X2; // Design matrix for fixed coefficient parameters
        int<lower=0> clustervar[N]; // Variable of standardized cluster identifiers (i.e. 1 to M)
        vector[K1] prior_theta;
        cov_matrix[K1] prior_Sigma;
      }
      parameters {
        vector[K2] mu_0; // Mean hyperhyperparameter for hyperprior MVN distribution (fixed parameters)
        real<lower=0> sigma_0; // Variance hyperhyperparameter for hyperprior MVN vcov matrix (fixed parameters)
        
        vector[K1] theta; // Mean for population MVN distribution (random parameters)
        cov_matrix[K1] Sigma; // Covariance for population MVN distribution (random parameters)
        
        vector[K1] alpha[M]; // Vector of random coefficient parameters for each cluster compiled into array
        vector[K2] beta; // Vector of fixed coefficient parameters 
      }
      transformed parameters {
        real eta[N]; // Cluster-specific parameters combining individual-specific X with cluster-specific and/or individual-specific model parameters
        real sigma2_0;
        for (i in 1:N) {
          eta[i] = X1[i,]*alpha[clustervar[i]] + X2[i,]*beta;
        }
        sigma2_0 = sigma_0^2;
      }
      model {
        // Hyperprior Distribution
        mu_0 ~ normal(0,100);
        sigma2_0 ~ inv_gamma(0.001,0.001);
        // Prior Distribution
        theta ~ multi_normal(prior_theta, prior_Sigma);
        Sigma ~ inv_wishart(3, prior_Sigma);
        beta ~ normal(mu_0,sigma_0);
        // Population Distribution
        for (m in 1:M) {
          alpha[m] ~ multi_normal(theta,Sigma);
        }
        // Data Distribution
        Y ~ bernoulli_logit(eta);
      }
  # SECOND: Prep data for input and run .stan file
      # Notes:
          # stan_data is a list of the necessary data elements in .stan
          # iter: number of draws/iterations
          # warmup: number of initial draws/iterations to discard
          # thin: keep every X draw (to avoid autodependancy)
          # chains: number of times to repeat sampling iterations
    library(rstan)
    stan_data <- list(n = nrow(dat), 
                      J = max(dat$county), 
                      y = dat$y,
                      x1 = dat$x1, 
                      x2 = dat$x2, 
                      county = dat$county)
    stan_fit <- stan("Program.stan",
                     data = stan_data,
                     iter = 10000,
                     warmup = 1000,
                     thin = 5,
                     chains = 3)
  # THIRD: Evaluate fit
    traceplot(stan_fit, inc_warmup = FALSE)
    print(stan_fit)
  # FOURTH: Extract posteriors
    theta <- extract(stan_fit, pars = 'theta')$theta

#*******************************************************************************************************#
    
    
#*******************************************************************************************************#
##### SECTION IV: DIAGNOSTICS/EVALUATION #####
#*******************************************************************************************************#
##### Part A: Prediction #####
      # For OLS models: confidence intervals AND standard error for predicted MEANS
        predict(model,data=newdata,interval="confidence",se.fit=T)
        
      # For OLS models: prediction intervals for predicted OBSERVATIONS
        predict(model,data=newdata,interval="predict")
        # standard error for predicted OBSERVATIONS
        pred <- predict(model,data=newdata,se.fit=T)
        sqrt(pred$se.fit^2 + pred$residual.scale^2)
        
      # For GLM models: link scale standard error of predicted MEANs
        # (i.e. fitted value represents mean, not individual obs)
        predict(model,data=newdata,type="response",se.fit=T)
        
      # For GLM models: response scale standard error of predicted MEAN
        # Note: to calculate CIs on response scale, should calculate CI bounds on link scale first, 
        #         then transform back
        predict(model,data=newdata,type="link",se.fit=T)
        
      # Prediction 
        
      # Cross-validation
        # K-fold CV
          # STEP 1: Randomy split data into k sets
            fold <- sample(cut(seq(1,nrow(dat)),breaks=k,labels=F),replace=F)
          # STEP 2: Fit on training set, predict on test set, calculate loss (e.g. MSE), repeat...
            mse <- rep(NA,k)
            for (i in 1:k) {
              train_set <- dat[which(fold!=i),]
              test_set <- dat[which(fold==i),]
              model <- lm(formula,data=train_set)
              pred <- predict(model,newdata=test_set)
              mse[i] <- mean((test_set[,"y"]-pred)^2)
            }
          # STEP 3: Summarize model's predictive performance (mean and se of MSEs)
            mean(mse)
            sd(mse)/sqrt(k)
        
        
##### Part B: Residuals #####
      # GUIDANCE:
          # Correlated pattern (linear or non-linear): non-linearities
          # Fan pattern (small left to large right): heteroscedasticity, data skewness

      # Sum of Squared Residuals
        anova(model)
        deviance(ols_model)
        
      # Studentized/Jackknifed Residuals
        studres(model)
        
      # Response Residuals (y-fitted)
        residuals(model,type="response")
        
      # GLM Residuals (only use deviance or Pearson bc variance is not constant for most GLMs)
        # Note: Generally, better to plot against fitted values on link scale (Faraway)
        # Deviance (default; tend to be more normally distributed than Pearson)
          residuals(model,type="deviance")
        # Pearson
          residuals(model,type="pearson")
          
      # Binned Residuals (for binary outcomes; plots averages within bins)
        library(arm)
        binnedplot(predicted_y,residuals)
 
               
##### Part C: Data Point Influence/Leverage #####
      # Leverage (potential to affect fit)
        influence(model)$hat
        
      # Influence (assesses the effect of each case on fit)
        influence(model)$coef
        
      # Cook's Distance (values >= 1.0 "overly influential")
        # Rule of Thumb: CD >1.0 indicates very influential data point
        cooks.distance(model)
        
##### Part D: Assessing Linearity #####
      # Partial Regression Plot
        library(car)
        avPlot(model)
        
        
##### Part E: Assessing Normality #####
      # Comparing distribution of residuals to normal distribution
        dist.sresid <- function(s_resid) {
          xfit <- seq(min(s_resid),max(s_resid),length=40)
          yfit <- dnorm(xfit)
          hist(s_resid,freq=F,main="Distribution of Studentized Residuals")
          lines(xfit,yfit)
        }
      # Quantile-Quantile (QQ) Plot
        # Note: envelope option adds point-wise confidence intervals (NOT joint bands)
        library(car)
        qqPlot(dat$arearate,grid=F,envelope=F,id=F,col.lines="red",main="Q-Q Plot")
          # "Heavy-tailed" = right end above 45-degree line; left end below
          # "Light-tailed" = right end below 45-degree line; left end above
        
        
##### Part F: Assessing Homoscedasticity #####
      # Residual Plots
        library(car)
        spreadLevelPlot(model)
        
      # Breush-Pagan Test for Heteroscedasticity
        model <- lm(y~x1+x2,data=dat)
        resid <- residual(model)
        lm(resid~x1+x2,data=dat)
          # reg of residuals on x1 and x2 should not yield significant coefficients
        
      # White Test for Heteroscedasticity (based on weaker assumptions)
        model <- lm(y~x1+x2,data=dat)
        resid <- residual(model)
        pred <- predict(model)
        lm(resid~pred+I(pred^2))
          # reg of residuals on fitted y and square of fitted y should not yield sig coefs
        
##### Part G: Assessing Collinearity #####
      # Correlations (where dat has only numeric variables)
        cor(dat)
        
      # Condition Number (kappa)
        # Note: may need to standardize variables first
        dat_center <- scale(dat,scale=T)
        kappa(dat_center)
        # Manually...
        EVs <- eigen(t(dat_center)%*%dat_center)$values
        sqrt(max(EVs)/min(EVs))
        
      # Variance Inflation Factor (VIF) / Generalized VIF (GVIF)
        # Rule of thumb: VIF_j >10 indicates collinearity involving X_j
        # Note: with GVIF, square last column and compare to 10
        library(car)
        vif(model)
        
  
##### Part H: Assessing Correlation #####
      # Autocorrelation
        # Autocorrelation Function Estimation & Plot
          acf(x)
        # Regress estimated residuals on lagged estimated residuals
          resid <- residual(lm(y~x1+x2+x3),type="response")
          resid_lead1 <- resid[-1]
          lm(resid_lead1~resid[-length(resid)]+x1+x2+x3)
        
        
##### Part H: Misc. Goodness-of-Fit #####
      # Goodness-of-Fit
        # Deviance-based (Deviance appx~Chi-squared for distributions with fixed dispersion parameter)
          pchisq(deviance(model),df.residual(model),lower.tail=F)
          # Note: for Gaussian GLM, do not know dispersion parameter value, so chi-sq apprx cannot be
          #       used; instead use F-test with estimate of dispersion parameter (X^2/(n-p))
        
      # Overdispersion (for Poisson/Negative Binomial/Gamma
        # Note: See GLM section for how to get dispersion-adjusted standard errors
        # Deviance-based (resid dev approx. Chi-Squared if correctly specified, H_alt=saturated model)
          pchisq(deviance(model),df.residual(model),lower.tail=F)
        # Pearson-based
          pchisq(sum(resid(model,type="pearson")^2),df.residual(model),lower.tail=F)
          
      # Matching Balance Table
        balance(raw_data,matched_data,pscore_model,factor=T)
        
      # Comparing Nested Models
        # F test (default): for OLS and GLMs with unknown dispersion parameters (gaussian,
        #                   quasibinomial, quasipoisson, gamma, IG)
          anova(model_base,model_full,test="F")
        # LRT (~Chi-square test): for GLMs with known dispersion parameters (binomial, poisson);
        #                         two test options below are equivalent
        # Note on LRT for comparing GLMs: OK to compare GLMS with different link functions as long as
        #                                 predictor sets are nested (link fxns just generate probs, so
        #                                 saturated models' L(.) are identical)
          anova(model_base,model_full,test="Chisq")
          anova(model_base,model_full,test="LRT")
          # Alternative LRT (also chi-squared test, but test statistic calculated slightly differently)
            library(lmtest)
            lrtest(model_base,model_full)
        
      # Hosmer-Lemeshow Test (SEE TESTS SECTION)
        
#*******************************************************************************************************#
        
        
#*******************************************************************************************************#
##### SECTION V: REPORTING #####
##### Part A: Non-Linear Effects #####
      # Conditional Plots (Predictions/Effects over variable(s) conditional on other variables)
        # Over 1 Variable (cplot)
          library(margins)
          cplot(model,"variable",what="prediction")
          cplot(model,"variable",what="effect")
        # Over 2 Variables
          persp(model,"var1","var2",what="effect",phi=30)
          image(model,"var1","var2",what="effect")
          
          
##### Part B: Printing Data #####
      # Resetting R's use of scientific notation
        options("scipen"=100,"digits"=4)

      # Manuscript-style tables (stargazer package)
          library(stargazer)
        # Summary Tables
          stargazer(dat,type="html")
            
        # Portion of Dataframe
          stargazer(dat[1:10,],summary=F,rownames=F,type="html")
          
        # Model Estimates
          stargazer(model1,model2,model3,align=T,type="html")
          
        # GETTING TABLES INTO MS WORD
          # Step 1: Get HTML Code
            stargazer(model,type="html")
          # Step 2: Copy HTML Code into new, temporary R HTML script (place between <html> and </html>)
          # Step 3: Knit
          # Step 4: Open Rhtml file from MS word to copy table
          
#*******************************************************************************************************#
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
