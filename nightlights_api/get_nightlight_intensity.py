import numpy as np
import os
import geoio
import math
import boto3
import json

import aws_credentials

BUCKET = 'noaa-nightlights'
COUNTRY_TO_TIF_KEY = {'ET':'viirs_2015_75N060W.tif'}

def create_space(lat, lon, s=10):
    """Creates a s km x s km square centered on (lat, lon)"""
    v = (180/math.pi)*(500/6378137)*s # roughly 0.045 for s=10
    return lat - v, lon - v, lat + v, lon + v

def get_country_bounding_box(country):
    with open('countries-bounding-box.json') as f:
        coutries_bounding_box = json.load(f)

    if country in coutries_bounding_box:
        min_lon, min_lat, max_lon, max_lat = coutries_bounding_box[country][1]
        return min_lat, min_lon, max_lat, max_lon
    else:
        raise ValueError(f"{country} not in 'countries-bounding-box.json'")



def load_tif_from_s3(country):

    s3 = boto3.client('s3',
        aws_access_key_id = aws_credentials.ACCESS_KEY,
        aws_secret_access_key = aws_credentials.SECRET_KEY
        )

    # An alternative to downloading the file locally is to read it directly to ram memory, but for some reason that method is eating all the available ram
    # response = s3.get_object(Bucket=bucket, Key=tif_key)
    # tif_file = response['Body'].read()

    #Download tif file associated to this country
    country_tif_key = COUNTRY_TO_TIF_KEY[country]
    file_name = country_tif_key
    s3.download_file(BUCKET, country_tif_key, file_name)

    #Load tif
    tif = geoio.GeoImage(file_name)

    #Delete local file
    os.remove(file_name)

    return tif

def compute_metric_of_bounding_box(tif, metric_func, min_lat, min_lon, max_lat, max_lon):

    #Transform tif data to array
    tif_array = np.squeeze(tif.get_data())

    #Find pixels for given lat lon pairs
    xminPixel, ymaxPixel = tif.proj_to_raster(min_lon, min_lat)
    xmaxPixel, yminPixel = tif.proj_to_raster(max_lon, max_lat)

    #Parse pixel values to integers
    xminPixel, yminPixel, xmaxPixel, ymaxPixel = int(xminPixel), int(yminPixel), int(xmaxPixel), int(ymaxPixel)

    #Validate values obtained for space are inside tif
    if xminPixel < 0 or xmaxPixel >= tif_array.shape[1]:
        print(f"no match for {r.cluster_lat}, {r.cluster_lon}")
        raise ValueError()
    elif yminPixel < 0 or ymaxPixel >= tif_array.shape[0]:
        print(f"no match for {r.cluster_lat}, {r.cluster_lon}")
        raise ValueError()

    #Compute metric over grid values
    metric_value = metric_func(tif_array[yminPixel:ymaxPixel,xminPixel:xmaxPixel])

    return metric_value

def get_max_radiance(country, tif):
    """
    Input: country abbreviation and country's tif

    Method will first look if max radiance has already been computed and saved in max_radiance_per_country.json. If not, it will compute it.
    """

    #Check if precomputed max_radiance already exists in local file
    with open('max-radiance-per-country.json') as f:
        precomputed_radiance_per_country = json.load(f)

    if country in precomputed_radiance_per_country.keys():
        return float(precomputed_radiance_per_country[country])

    #If not, compute it

    #Get coordinates of grid of length grid_length around point of interest
    min_lat, min_lon, max_lat, max_lon = get_country_bounding_box(country)

    #Compute max radiance of grid
    max_radiance = compute_metric_of_bounding_box(tif, np.max, min_lat, min_lon, max_lat, max_lon)
    print(f'max_radiance :{max_radiance}')
    #Save value in dict and json file of precomputed radiances
    precomputed_radiance_per_country[country] = str(max_radiance)
    with open('max_radiance_per_country.json', 'w') as f:
        json.dump(precomputed_radiance_per_country, f)

    return max_radiance



def get_mean_radiance(tif, lat, lon, grid_length):

    #Get coordinates of grid around point of interest
    min_lat, min_lon, max_lat, max_lon = create_space(lat, lon, s=grid_length)

    mean_radiance = compute_metric_of_bounding_box(tif, np.mean, min_lat, min_lon, max_lat, max_lon)

    return mean_radiance



def get_normalized_mean_radiance(
    country, lat, lon, grid_length=10):
    """
    Outputs the mean normalized nightlight intensity of a grid around the
    point defined by (lat, lon)

    Normalization:
        output = ln(mean(point_grid) + 1)/ln(max_radiance_in_country)
        Maps to [0,1] and reduces outsize influence of outliers.
    Input:
        country: Country where point lives
        lat, lon: Coordinates of point whose nightlight intensity we want to compute
    Returns float
    """

    #Load tif file for given country
    tif = load_tif_from_s3(country)

    #Compute mean radiance of grid around point of interest
    mean_radiance = get_mean_radiance(tif, lat, lon, grid_length)

    #Compute max radiance in country
    max_radiance = get_max_radiance(country, tif)

    #Compute normalized mean nightlight
    normalized_mean_nightlight = np.log(mean_radiance+1)/np.log(max_radiance)

    return normalized_mean_nightlight

if __name__=='__main__':

    country = 'ET'
    eth_random_point_lat = 8.95748548658
    eth_random_point_lon = 38.7621280316

    print(get_normalized_mean_radiance(
                    country = country,
                    lat = eth_random_point_lat,
                    lon = eth_random_point_lon))
