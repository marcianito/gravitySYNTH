Repository containing scripts related to _Mikolaj and Reich., (2018, ???)_ publication
======================================================================
**Article:**  
Michal Mikolaj<sup>1</sup>, Marvin Reich<sup>1</sup>: **_TITLE_**, published in: [???](http://link.com), **2018**

**Affiliation**  
<sup>1</sup>Helmholtz Centre Potsdam GFZ German Research Centre for Geosciences, Section 5.4 Hydrology, 14473 Potsdam, Germany  
> _Please cite this article when using here provided scripts_

## Description

This is a small R-package which aims at supporting the synthetic uncertainty and error anaylsis about
gravity component time series, presented in the above mentioned paper.
With this package you can..

For bug fixes, comments or further development please contact: mreich@posteo.de.

## Installation

1. Start R
2. Install package-dependence via devtools: 
`devtools::install_github("marcianito/UmbrellaEffect")`
`devtools::install_github("marcianito/gravityInf")`

3. Install package via devtools: 
`devtools::install_github("marcianito/gravitySynth")`

4. load packages: 
`library(UmbrellaEffect)`; 
`library(gravityInf)`;
`library("marcianito/gravitySynth")`

## Dependencies

### Computationally
* r-base version 3.3.1
* for further dependencies of UmbrellaEffect package, please visit [here](http://github.com/marcianito/UmbrellaEffect)
* for further dependencies of UmbrellaEffect package, please visit [here](http://github.com/marcianito/gravityInf)
* following R-packages: devtools, dplyr, ggplot2, gstat, ncdf4, viridis, zoo, ...
* system libraries for devtools

in debian install using: 
`apt-get update && apt-get install libssl-dev`

### Data-wise
It is necessary to have *hourly* time series of following gravity components:
* Atmosphere
* Non tidal ocean loading
* Global hydrology
* Tides
* Precipitation
* Discharge
* Evapotranspiration

Furthermore a digital elevation model (DEM) of the surroundings of the gravimeter is needed.

## Processing

.. comming soon ..
