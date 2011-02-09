#1024*1024 pixel detector module. Version from 09/03/2010

# Set global constants describing the detector
detector <- list()

# Name
detector$type = "Rayonix MX type, 2*2 binning, 1024*1024 px"

# Position of the centre of the module in mm
detector$xCentre = 37.5
detector$yCentre = 37.5

# Position of the bottom left of the chip in mm
detector$ccdX = 22.14
detector$ccdY = 22.14

# Pseudopixel size on the chip in mm
detector$xPxSize = 0.03
detector$yPxSize = 0.03

# Number of pseudopixels
detector$xPxNum = 1024
detector$yPxNum = 1024

# the region over which the phosphor and taper input face extends. Need not be centred
# on xCentre, yCentre if only part of a module is being simulated
detector$xDetRange = c(0,75)
detector$yDetRange = c(0,75)

# pseudopixel size on the face of the detector
detector$xPxSizeFace = (detector$xDetRange[2] - detector$xDetRange[1]) / detector$xPxNum
detector$yPxSizeFace = (detector$yDetRange[2] - detector$yDetRange[1]) / detector$yPxNum

# type of Point Spread Function
detector$PSF = as.matrix(read.table(system.file("phosphorModel", "PSF.dat", package = "DISP")))

# taper radial distortion polynomial coefficients
detector$taperA = 0.3485
detector$taperB = 0.0005
detector$taperC = 0

# taper bleed factor describing FOT contribution to the PSF
detector$taperBleed = 0.02

# 'ADC bias' to avoid negative pixel values
detector$bias = 500

# dark parameters
detector$darkRate = 0.01
detector$darkAccumulationTime = 1

# the phosphor Quantum Detection Efficiency
detector$QDE = 0.85

# the phosphor Pulse Height Spectrum
detector$PHS = as.matrix(read.table(system.file("phosphorModel", "PHS.dat", package = "DISP")))

# the expected number of light photons generated per absorbed X-ray, calculated from the
# PHS data
detector$phosphorAmplification = weighted.mean(detector$PHS[,1], detector$PHS[,2])

# the noise in that quantity
detector$phosphorNoise = sqrt(sum((detector$PHS[,1] - detector$phosphorAmplification)^2 * detector$PHS[,2]))

# transmission probability of a light photon. Assume the FOT accepts 50% and transmits 14%,
# then 70% get through a taper-stub interface onto the CCD, which in turn has a quantum
# efficiency of 35%, the rest being lost by reflection or absorption in gates. Thus the
# probability of a photon being measured is 0.5 * 0.14 * 0.7 * 0.35 = 0.01715.
detector$transmission = 0.01715

# gain of the ADC, i.e. number of ADUs per light photon
detector$ADCgain = 1/5.0

# dynamic range of the ADC
detector$ADCbits = 16

# ADC read noise (dependent on read time, but independent of integration time. By default
# this is set such that the s.d. of the normal distribution is 10e/pixel for a 1s read time)
detector$readNoise = 10

