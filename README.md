# review-SRS-CARBEL

Contact: Katherine Hulting, <hultingk@msu.edu>

**File name: CARBEL-arthropods.csv**\
Each row contains data for one interaction on one plant. 

| Variable          | Description                                                                                                                                                                             |
| :---------------- | :-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| block   | Site name (8, 10, 52, 57, 53N, 53S, or 54S) |
| patch         | Patch Identifier ( B = Connected, C = Wing or Rectangle, D = Wing or Rectangle, E = Wing or Rectangle (labeled clock-wise from patch B)    |
| corner              | Corner of patch where plant is located. With back to patch A, I = top left, II = top right, III = bottom left, IV = bottom right     |
| distance              | Categorical distance of plant from edge. 0 = closest to edge, 3 = furthest from edge   |
| plant\_ID              | Unique ID for each focal plant  |
| sampling\_round         | Sampling round of observation (1-4)            |
| temp        | Air temperature during observation (C)   |
| wind    | Windspeed during observation (km/hr)      |
| cloud\_cover  | Percent cloud cover during observation |
| date    | Date of observation (mm/dd/yyyy)  |
| start\_time | Start time of survey in which observation took place (hh:mm) |
| visitor\_species | Species identification to lowest taxanomic identification. 0 = no observation made during survey |
| visitor\_type | Catagorization of observation as pollinator, spider, or florivore. 0 = no observation made during survey |
| edge\_type | Catagorization of distance to edge as edge (0-10m) or interior (>10m) |



**File name: CARBEL-floral.csv**\
Each row contains data for one species observed during a floral survey. All floral species were surveyed within 5m of a focal Carphephorus plant. 

| Variable          | Description                                                                                                                                                                             |
| :---------------- | :-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| block   | Site name (8, 10, 52, 57, 53N, 53S, or 54S) |
| patch         | Patch Identifier ( B = Connected, C = Wing or Rectangle, D = Wing or Rectangle, E = Wing or Rectangle (labeled clock-wise from patch B)    |
| corner              | Corner of patch where plant is located. With back to patch A, I = top left, II = top right, III = bottom left, IV = bottom right     |
| distance              | Categorical distance of plant from edge. 0 = closest to edge, 3 = furthest from edge   |
| plant\_ID              | Unique ID for each focal plant  |
| sampling\_round         | Sampling round of observation (1-4)            |
| date    | Date of observation (mm/dd/yyyy)  |
| start\_time | Start time of survey in which observation took place (hh:mm) |
| focal\_count | Number of flowering inflorescences on focal plant at time of survey |
| floral\_species | Species identification of flowering plant within 5m of focal plant |
| floral\_code | Six letter code of species name |
| number\_individuals | Number of flowering individuals of one species in survey plot |
| avg\_1-avg\_10 | Count of flowers on an individual plant. Number of flowers were counted for 10 individuals per species. NA indicates no count, due to less than 10 individuals present in the plot |


**File name: CARBEL-seeds.csv**\
Each row contains data for one inflorescence on one focal plant (up to 3 inflorescences/plant)

| Variable          | Description                                                                                                                                                                             |
| :---------------- | :-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| block   | Site name (8, 10, 52, 57, 53N, 53S, or 54S) |
| patch         | Patch Identifier ( B = Connected, C = Wing or Rectangle, D = Wing or Rectangle, E = Wing or Rectangle (labeled clock-wise from patch B)    |
| corner              | Corner of patch where plant is located. With back to patch A, I = top left, II = top right, III = bottom left, IV = bottom right     |
| distance              | Categorical distance of plant from edge. 0 = closest to edge, 3 = furthest from edge   |
| plant\_ID              | Unique ID for each focal plant  |
| seed\_head         | Letter assigned to one of three seed heads per plant (A, B, C)             |
| collection\_date    | Date of seed head collection from plant (mm/dd/yyyy)  |
| count\_date | Date that seeds were inspected and counted (mm/dd/yyyy) |
| viable | Number of achenes containing a viable seed |
| nonviable | Number of achenes not containing a viable seed |
| no\_predation | Number of achenes with evidence of predispersal seed predation damage |


**File name: Patch\_type.csv**\
Each row contains data for one patch per block in experimentally fragmented landscapes. 

| Variable          | Description                                                                                                                                                                             |
| :---------------- | :-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| Block   | Site name (8, 10, 52, 57, 53N, 53S, or 54S) |
| Patch         | Patch Identifier (B = Connected, C = Wing or Rectangle, D = Wing or Rectangle, E = Wing or Rectangle (labeled clock-wise from patch B)    |
| Type              |  Corresponding patch type (Connected, Winged, Rectangular)   |
