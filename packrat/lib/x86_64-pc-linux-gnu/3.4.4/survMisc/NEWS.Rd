\name{NEWS}
\title{NEWS file for the survMisc package}

\section{07/03/18: Version 0.5.5}{
  \itemize{
    \item Removing \code{plot} method for \code{Surv} objects, so as to
  avoid overwriting the new \code{plot.Surv} method in
  \code{package:survival}.
   \item A substantial revision to this package in planned in the months
  ahead and is planned to occur with version 0.6.0.
  }
}

\section{11/22/16: Version 0.5.4}{
  \itemize{
    \item Minor update to take account of the release of \code{package:ggplot2 2.2.0}.
  }
}

\section{7/6/16: Version 0.5.3}{
  \itemize{
    \item Corrected error in \code{sf}. Thanks to Walton Jones for
  bringing this to my attention. 
    \item Added some \code{xtable} methods applicable to survival data.
  }
}

\section{5/16/16: Version 0.5.2}{
  \itemize{
    \item Corrected error in \code{comp} when comparing two groups. This was
    present in Versions 0.5.0 and 0.5.1. Thanks to onesandzeroes and drcanak for pointing this out.
    \item Also re-named the vignette again.
  }
}

\section{5/11/16: Version 0.5.1}{
  \itemize{
    \item Have included \code{profLik} function from version 0.4.6
    again.
    \item Re-named the vignette.
  }
}

\section{2/16/16: Version 0.5.0}{
  \itemize{ 
    \item This is a re-write 'from the ground up'. A more 'oject-oriented'
    approach has been adopted. The 'ten' function/ object has replaced the
    former 'tne' method. (Motivated in part by the user who commented that
  "I'm afraid the code in the  function `.getTne` appears somewhat cobbled together".)
  
  \item A number of functions and datasets have been left out temporarily for this release
  in order to focus on the 'core' package elements. These will be added
  back in due course, most likely version 0.6.0.
  
  \item
   \describe{
    \item{\code{cutp}}{Error in calculation of log-rank test in some in
      cases has been fixed. Code also simplified to make better use of \code{data.table}.
      Thanks in particular to Rached Alkallas for her helpful
      work on this. As suggested:
      \cr
      \emph{There was an problem with the previous version [0.4.6], which
	may have affected the scientific results of those using this
	function}
    }

    \item{\code{comp}}{Problems when calculating the 'test for trend'
      have been fixed. Thanks to Karl Ove Hufthammer for pointing this
      out. I have also modified the help for \code{comp} which was using
      the words 'groups' and 'strata' interchangably. This has been
      corrected elsewhere in the manual. \code{comp} does *not* yet work
      with stratified models, but this will be implemented in due course.
      \cr
      Thanks also to Mario Ouwens for input on the Renyi function and
      Haitao Yang for comments on the log-rank test.
    }
    
    \item{\code{autoplot.survfit}}{Function has been improved. This will
      likely be deprecated in due course in favor of \code{autoplot.ten}.
      \cr
      The returned object will now \code{print} using
      \code{autoplot.tableAndPlot}.
      \cr
      Examples of changing the output are now given in the 'plots'
      vignette. These include \eqn{y} axis limits and the order of the
      legend labels and position. Thanks to Line Heylen for these suggestions.
      \cr
      Thanks also to Jason Kennedy for bringing up the problem of legend
      re-ordering.
    }
    
    \item{\code{rsq.coxph}}{Typo fixed. Thanks to Nir Y Krakauer for bringing this to my attention.}
  }
}}

\section{12/21/14: Version 0.4.4}{ 
  \itemize{
    \item Error in use of \code{dQuote} in \code{comp} now fixed.
    Thanks to all who pointed this out: J Smith, M Fiocco, D Winsemius, 
    J Bienkowska, A Calcareo, N Villanueva, A Lover, R Perry, K
    Karakostas.
    
    \item Typo in \code{autoplot.survfit} fixed. Thanks to D Menne.
    
    \item Example in \code{autoplot.survfit} now includes method to customize the
    output further. Thanks to S Melov and L Neves.
    
    \item Scoping in \code{comp} fixed. Thanks to A. Atkinson.
}}

\section{Version 0.4 and before}{
  \itemize{
    \item 8/14/14: Add \code{cf6.c} to \code{src}; use this instead of depending on survival package.
    \item 7/6/2014: Add \code{gof} method.
    \item 7/2/2014: Fix \code{.getTne} to work on Win and Linux.
    \item 6/21/2014: Change name of \code{autoplot} and push new version
    to github.
    \item 6/9/2014: Improve \code{autoplot.survfit}; coloring now
    consistent for line and fill.
    \cr
    Add \code{ci} method.
    \cr
    Move documentation from \code{quantile} to \code{ci}.
    \item 5/21/14: Fixed \code{autoplot.survfit}: arbitrary alpha now
    works with upper box in filled survival lines.
    \item 6/6/2014: Fixed \code{.getTne}, was not calculating values correctly;
    comp() now works correctly.
    \item 5/20/14: Fixed \code{comp}, \code{compNSurv} for cases where
    last element in 
    \code{covMatSurv} is \code{NaN}.
    \cr
    Fixed \code{sig} so likelihood-ratio test works for factors
    (needs call to \code{model.matrix}).
    \cr
    Fixed \code{cutp} - need to convert \code{data.table} to 
    \code{data.frame} in function body for now.
}}
