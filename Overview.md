# Getting started #

Having installed `DISP`, load it within R by typing the command
```
library(DISP)
```
This will also load the required package `gpclib`. We can now see the names of `DISP` functions exposed to the user by typing
```
ls("package:DISP")
```
Online help for any of these functions can be viewed by typing its name preceded by '?', for example `?disp`. In this overview page we'll group these functions into categories and look at each group separately. Not all `DISP` functions will be mentioned. As always, check the documentation for more details.

# Simulation parameters #

The simulated detector parameters are stored in a list called `detector` and parameters determining image corrections after read-out are in a list called `processing`. These are not functions so do not appear in the list we printed above. Nevertheless default `detector` and `processing` lists _are_ provided with the package, but must be explicitly loaded. So to generate corrected images with the default detector, we would start our session with
```
data(detector)
data(processing)
```
View the documentation for these objects, with `?detector` or `?processing`, for a description of the elements of these lists.

# Quantum images #

Operation of the simulated detector begins with providing input. For `DISP` this consists of a two-column matrix providing X (horizontal) and Y (vertical) X-ray photon positions on the plane of the detector face. Such a 2D spatial distribution of photon positions is sometimes called a 'quantum image', particularly in the medical physics literature. The coordinates are given in millimetres, with the acceptance window of the detector face determined by `detector$xDetRange` and `detector$yDetRange`. For the default detector module, that is a square viewed from the X-ray source with its bottom left corner at (0,0) and its top right at (75,75).

`DISP` provides some functions for constructing input quantum images, namely `spot`, `flood`, `spotGrid` and `pencil`. More details are given in the online documentation, but by way of a simple example here
```
q1 <- spot(10000,50,50)
```
assigns to `q1` a matrix of X-ray photon positions with, on average, 10'000 rows (with Poisson variance in that number) that form an X-ray spot with default size and shape, centred at (50,50) mm on the detector face. Note the first use of `spot` attaches the package `mvtnorm` to the search list, in order to use its function `rmvnorm`.
```
q2 <- flood(10000, c(48,52), c(48,52))
```
assigns to `q2` approximately 10'000 X-ray photon positions uniformly distributed inside the window between 48 and 52 mm in both the X and Y directions.

## Note ##

Simulation of the X-ray phosphor in `DISP` makes the implicit assumption that X-ray photons represented by the quantum image have an energy of 12'000 eV. In addition each X-ray is assumed to arrive orthogonal to the detector face.

# Detector hardware #

The chain of processes forming the simulated _taper-phosphor-CCD_ area detector operation are wrapped at the highest level by two functions:

  * `expose` - operate on a quantum image to produce a matrix of electron counts on the CCD chip.
  * `read` - simulate read-out noise and ADC operation to produce a 'raw' digital image of values in Analogue-to-Digital Units (ADU).

Typing the names of these functions in at the R prompt prints the source code, from which you can see that `expose` wraps lower level functions `phosphor`, `taper` and `accumulate`. If you are interested, view the help for these functions to see what each is doing and how it can be modified. The function `read` delegates to `dark` to generate a 'dark image' of thermally generated electrons. The parameters to all of these functions can be changed either as function arguments or by modifying relevant elements of the `detector` list.

To continue our simulation example, we form a raw image with
```
eImage1 <- expose(q1)
eImage2 <- expose(q2)
eImage3 <- eImage1 + eImage2
rawIm <- read(eImage3)
```
We can add the electron count 'images' together because we have not yet added any read-out noise or dark current. After calling `read` the image is fixed in a form that incorporates these additive components. The result is then a 'raw' image of the type produced as output of the detector hardware.

# Image corrections #

As long as the list called `processing` is loaded, a raw image produced by `read` can be corrected for dark signal, non-uniformity and spatial distortion. Unsurprisingly, the function for doing this is called `correct`, which wraps two lower level functions, `normaliseImage` and `undistortImage`.
```
corrIm <- correct(rawIm)
```

# Image display & manipulations #

Images in `DISP` are simply matrices, nothing more. The first dimension is treated as the X direction, the second as the Y direction. A few useful functions for working with images have been defined in `DISP`. The most important of these is `disp`, an eponymous image display function. This plots an image (in fact, any matrix) in a format familiar to crystallographers - greyscale with darker shades indicating greater intensity. The function attempts to set the contrast to a reasonable level automatically (but this can also be set manually). Many other graphics parameters can be set by calls to R's `par` function. High quality images can be produced by opening an appropriate graphics device, such as `pdf` or `postscript`. A few examples of the use of `disp`:
```
disp(rawIm)
disp(corrIm)
disp(corrIm[600:800,600:800], origin=c(600,600))
disp(corrIm[600:800,600:800], white=90, black=110, origin=c(600,600))
disp(corrIm[600:800,600:800], origin=c(600,600), units="mm")
```

As a complement to 2D image display, there is also a 1D line plot function, called `linePlot`. The simplest way to use it is to ensure an image is already displayed with `disp`, then call `linePlot` with the same input image. This enters an interactive mode in which the ends of the line are determined by left mouse clicks on the displayed image.
```
disp(corrIm)
linePlot(corrIm) #Left click ends of the line
```

There are some bugs (actually inadequacies) in the implementation so the interactive mode might not work properly if the displayed image matrix is subset with `[`, if the `origin` argument to `disp` is set to something other than `c(1,1)` (the default), and certainly not if `units` are not `"pixels"`. A plain call as above is safest.

See the documentation `?linePlot` for more advanced usage, including calculation and plotting of full width at x `*` maximum across a peak.

Other image plotting functions are
  * `dispContour` - display an image using a contour plot.
  * `addBox` - overlay a measurement box on the current image.
  * `dispBox` - a combination of the above.

As well as image display, there are facilities for manipulating images. Matrix subsetting, vectorised operations and all other R built-ins for handling matrices work as expected. In addition `DISP` defines some fairly self-explanatory image (matrix) manipulation functions, namely `rotate`, `flipVertical` and `flipHorizontal`.

# Working with image files #

Working with real diffraction images might be considered the second _raison d'être_ of `DISP` and is essentially separable from the simulation functions (and possibly should have been a separate package). Being able to load diffraction images into R as matrices and do calculations on them has proven extremely useful. Despite that, due to lack of time these functions remain incomplete, covering just a few image formats for reading, limited scope for writing, and poor handling of header metadata. It's only a start.

The read functions for files created by real detectors take a single argument, a string giving the path to the file, and return a matrix (the image), with information from the image header retained as a list or character vector written to the object's attributes with the name `header`. This can be accessed with the command `attr(imageMatrix, "header")`, replacing `imageMatrix` with the name of the image matrix. The read functions are

  * `readSMV` - for reading images in the [Super Marty View](http://strucbio.biologie.uni-konstanz.de/ccp4wiki/index.php/SMV_file_format) format, used by ADSC detectors.
  * `readMar` - for reading images in the TIFF format used by Rayonix detectors.
  * `readCBF` - was written specifically to read images from the Dectris Pilatus P6M detector installed at the I24 beamline at [Diamond Light Source](http://www.diamond.ac.uk/), however it has been tested successfully with other miniCBF format images that use the byte-offset compression method.

Prior to implementing the above, images were imported into R by converting them to a tab-separated ASCII table and reading with `readImage`. The convention used is that the orientation of this table matches the display orientation of `disp`, that is, column number increases with X, row number decreases with Y. That was done so that portions of the ASCII file could be viewed side-by-side with an image displayed by `disp`, `dispContour`, etc. For `readImage` it is necessary to specify whether the ASCII file contains integers or floating point numbers.

The image write functions currently included in `DISP` include `writeImage` - to write out ASCII in the format read by `readImage`, and `writeCBF` - to write an image in miniCBF format, similar to that used by Dectris Pilatus detectors.

# Image set statistics #

`DISP` provides four functions for performing statistics over sets of images: `pxMean`, `pxVar`, `pxCov` and `pxCor`. This is meaningful only for a set of related images, such as replicates from a simulation. A set of images (in other contexts, known as a 'stack') is here defined as a three dimensional array. The function `makeImageSet` is provided to simplify the creation of a suitable array.

# Diffraction spot integration #

`pxIntegrate` is the most complicated function in the package and in time-honoured tradition is therefore the one with the least documentation here. I advise to look at the online help by typing `?pxIntegrate` and `?spotSimulations`.

# Miscellaneous utilities #

`DISP` eats up memory. It is quite easy with R to fill your workspace with large objects. The convenience functions `mem` and `size` query the Global Environment and allow the easy identification of large objects that `ls` lacks.