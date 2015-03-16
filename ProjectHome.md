![http://disp.googlecode.com/hg/site/logo.png](http://disp.googlecode.com/hg/site/logo.png)

# Diffraction Image Statistics Package #

This [R](http://www.r-project.org/) package provides functions that simulate the operation of a CCD area detector of the type commonly used in macromolecular X-ray crystallography. In addition to the simulation, functions allowing display, integration and statistical analysis of diffraction spot images are also included.

`DISP` was written to investigate possible improvements to error estimation for diffraction spot integration of data recorded on CCD detectors. That work was published in [Waterman & Evans](http://dx.doi.org/10.1107/S0021889810033418). **Estimation of errors in diffraction data measured by CCD area detectors.** _Journal of Applied Crystallography_ 43(6), 2010.

## Installation ##
The [Downloads](http://code.google.com/p/disp/downloads/list) page contains links to source tarballs for Unix-like operating systems, and Windows binaries of `DISP`. I haven't built a binary for Macs - you're welcome to do so and let me know so I can post it here.

`DISP` needs an R version >= 2.11.1 (the latest version may need R >= 2.14.0). It requires the package `gpclib` and suggests `mvtnorm`. These are available on [CRAN](http://cran.r-project.org/), so you can obtain them from within R using the command
```
install.packages(c("gpclib","mvtnorm"))
```

Note that `DISP` version 0.1-4 is the version at the head of the repository, but a tarball is not available for download here. This is because Google Code has removed the option to add new downloads. This version is essentially the same as 0.1-3, but was prepared to work with R 3.0.0.

To install `DISP`, download the relevant file to the current working directory and issue the appropriate R command, e.g.
```
install.packages("./DISP_0.1-3.tar.gz",repo=NULL) #Source install
install.packages("./DISP_0.1-3.zip",repo=NULL) #Windows binary install
```

## Usage notes ##
`DISP` has some idiosyncrasies, some of which probably classify as flaws. Please excuse them, I learned R while writing this package and it is an example of evolution rather than design. One of the most noticeable are the use of global variables `detector` and `processing` to describe parameters and calibration for the simulated detector hardware and software image corrections (this is also why the package is big). Many of the simulation functions expect these objects to exist somewhere in the search path and will fail if they don't. This would be avoided if I had set the package to use the `LazyData` mechanism, however I found that made installation prohibitively slow. Also, I wanted a simple way to replace the default `detector` and `processing` with custom objects, so starting with nothing and explicitly requiring the objects to be loaded as needed seemed a simple way to do that. The upshot is that in order to use all the simulation functions in `DISP` for the default detector, you first need to enter
```
data(detector)
data(processing)
```
in the interpreter or in your script. The list `processing` is huge, and takes a few seconds to load. This is almost all down to the element `undistortArray` contained therein.

For more general documentation on the use of `DISP` there is a basic [Overview](http://code.google.com/p/disp/wiki/Overview) available in the wiki. Apart from that, the most comprehensive resource is the online help, but this has no obvious entry point for getting started. However, try
```
?spotSimulations
```
for examples and data discussed in [our paper](http://dx.doi.org/10.1107/S0021889810033418).