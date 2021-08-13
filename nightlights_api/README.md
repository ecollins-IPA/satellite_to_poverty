# Overview

`nightlights_radiance_api.py` computes mean nightlight radiance for clusters defined by lat lng pairs.

`process_eth_clusters.py` executes the `nightlights_radiance_api` for the specific use case of `eth_cluster.csv`.

countries-bounding-boxes.json was download from [here](https://github.com/sandstrom/country-bounding-boxes)

# How to run

`python process_eth_clusters.py eth_cluster.csv ET 1,5,10`

# Setup

1. Install gdal dependencies

`sudo apt-get install libgdal-dev`

`export CPLUS_INCLUDE_PATH=/usr/include/gdal`

`export C_INCLUDE_PATH=/usr/include/gdal`

retrieve the version with gdal-config --version. and then add version number to `GDAL==version` in `requirements.txt`

2. Clone repo and `cd` to its folder

3. Create virtual environment if you havent yet

`python3 -m venv venv`

Run `sudo apt-get install python3-venv` if you dont have venv installed.

4. Activate venv

`source venv/bin/activate`

5. Install python libraries

`pip install -r requirements.txt`

# References

Most of the code of `nightlights_radiance_api.py` was extracted and adapted from https://github.com/jmather625/predicting-poverty-replication
