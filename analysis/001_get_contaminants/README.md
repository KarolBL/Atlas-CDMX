# Get contaminants

## Idea

Use ``aire.zmvm`` to gather:

- Contaminant stations
- Contaminants: SO2, CO, NO2, NO, NOX, O3, PM10 and PM25.
- IMECA index
- Temperature
- Relative Humidity
- Wind: Speed and Direction
- Time span: 2009‑01‑01 up to 2018 by hour

## Goal

Parallel independent data contaminants,temp and wind outputs + station location
- Independent result files per contaminats and so on.

## Requirements

R package: ``aire.zmvm`` 

## Results files
### Contaminats
- 2.6M	CO.RData
- 3.1M	NO2.RData
- 3.1M	NO.RData
- 3.6M	NOX.RData
- 3.5M	O3.RData
- 2.9M	PM10.RData
- 1.9M	PM25.RData
- 1.2M	PMCO.RData
- 2.7M	SO2.RData

### Radiation
- 1.1M	UVA.RData
- 924K	UVB.RData

### Station data
- 12K	stations.tab
- 8.0K	zones.tab

