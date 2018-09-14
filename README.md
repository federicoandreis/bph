BPH-Scotland 
===

Benign Prostate Hyperplasia drugs prescription trends in Scotland

Contributors: [Federico Andreis](mailto:federico.andreis@stir.ac.uk?subject=[GitHub]%20BPH), Emanuele Giorgi, Ashleigh Ward

This repository is intended for storing and sharing code and analysis results on our attempt to describe spatio-temporal trends in BPH-related drugs prescriptions. The initial dataset refers to gp-level data in Scotland, and is publically available [note to self: there seems to be some additional (and more recent) information on the [website](https://www.opendata.nhs.scot/dataset/prescriptions-in-the-community) check with Ashleigh].

### The data
df.csv contains the relevant variables.

- year 
- gp_code [gp unique identifier]
- drug [bph drug, 4 categories]
- n_items [total prescriptions per year, gp, and drug]
- Latitude/Longitude or Easting/Northing [georeferenced from POSTCODE]
- URxFOLD, x=2,3,6,8 [covariates, 
  [scottish rurality index](https://www.gov.scot/urbanrural) ]
- DZ [datazone, for further linkage]
- SIMD16_x, x=\{Rank, Vigintile, Decile, Quintile\} [covariate, Scottish Index of Multiple Deprivation]
- hydrochloride=\{0, 1\} [hydrocloride drugs yes/no]
- hydro_n [sum of n_items grouped by hydrochloride]

