import sys
import pandas as pd
import nightlight_radiance_api as nra

def compute_radiance_in_file(input_file, country, grids_lengths):
    '''
    Inputs:
    - csv input_file with cluster_lat, cluster_lon columns
    - list of grids_lengths for which to compute radiance around points of interest

    Outputs new csv with extra columns with nighlight radiance for every coordinate point
    '''
    df = pd.read_csv(input_file)

    #Create list of tuples based on entries of df
    #Expecting lat and lon
    coordinates_list = list(df.to_records(index=False))
    print('coordinates_list')
    print(coordinates_list)


    for grid_length in grids_lengths:

        df[f'radiance_around_{grid_length}_kms'] = \
            nra.get_normalized_mean_radiance(country, coordinates_list, float(grid_length))

        print(f'Finished computing for grid_length: {grid_length}')
        print()


    df.to_csv('computed_eth.csv', index=False)

if __name__ == '__main__':

    input_file = sys.argv[1]
    country = sys.argv[2]
    grids_lengths_by_commas = sys.argv[3]

    compute_radiance_in_file(input_file, country, grids_lengths_by_commas.split(','))
