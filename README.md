Adapting code from https://github.com/jmather625/predicting-poverty-replication at the moment.


# Data download

To get the data, you need to do three things:

1) download nightlights data from https://www.ngdc.noaa.gov/eog/viirs/download_dnb_composites.html. Use the 2015 annual composite in the 75N/060W tile and the 00N/060W tile. Choose the .tif file that has "vcm-orm-ntl" in the name. Save them to `viirs_2015_<tile_descriptor>.tif`, where tile_descriptor is 75N/060W or 00N/060W. As of December 2, 2020, these seemed to have been archived. I've uploaded a copy here: https://drive.google.com/drive/folders/1gZZ1NoKaq43znWIBjzmrLuMQh4uzu9qn?usp=sharing.
2) get the LSMS survey data from the world bank. Download the 2016-2017 Malawi survey data, 2015-2016 Ethiopia data, and the 2015-2016 Nigeria data from https://microdata.worldbank.org/index.php/catalog/lsms. The World Bank wants to know how people use their data, so you will have to sign in and explain why you want their data. Make sure to download the CSV version. Unzip the downloaded data into `countries/<country name>/LSMS/`. Country name should be either `malawi_2016`, `ethiopia_2015`, or `nigeria_2015`.
3) get an api key from either Planet or Google's Static Maps API service. Both of these should be free, but Planet may take some time to approve and require you to list a research project to be eligible for the free tier. Google's service should be free if you download under 100k per month. Save the api keys to `planet_api_key.txt` or `google_api_key.txt` in the root directory. I used Planet's API because then I could download images from 2015 and 2016, whereas Google's service only offers recent images over the last year. The code will show how to get the images from either API.

# Scripts

Run the Jupyter files in the following order:
```
scripts/process_survey_data.ipynb
```
To process LSMS data and create consumption and nightlight index per country

```
scripts/download_images.ipynb
```
To download daylight images
