import os
import requests
import pandas as pd


def get_city_state_from_zipcode(zipcode, GOOGLE_API_KEY):
    if pd.isna(zipcode) or zipcode == 'NaN' or zipcode == 'nan':
        return None, None, None

    url = f'https://maps.googleapis.com/maps/api/geocode/json?address={zipcode}&key={GOOGLE_API_KEY}'
    response = requests.get(url)
    if response.status_code == 200:
        data = response.json()
        if data['status'] == 'OK' and data['results']:
            city = state = country = None
            for component in data['results'][0]['address_components']:
                if 'locality' in component['types']:
                    city = component['long_name']
                if 'administrative_area_level_1' in component['types']:
                    state = component['short_name']
                if 'country' in component['types']:
                    country = component['short_name']
            return city, state, country
        else:
            print(f"Error: {data.get('error_message', 'Unknown error')}")
    else:
        print(f"HTTP Error: {response.status_code}")
    return None, None, None

if __name__ == '__main__':
    import argparse
    import pandas as pd

    parser = argparse.ArgumentParser(description='Fill in city and state from ZIP code')

    #default is addresses.csv, as -f or file
    parser.add_argument('-f', '--file', type=str, default='addresses.csv', help='CSV file with addresses (default: addresses.csv)')

    #default is None, as -k or key
    parser.add_argument('-k', '--key', type=str, default=None, help='Google API key')

    #default is addresses_filled.csv, as -o or output
    parser.add_argument('-o', '--output', type=str, default='addresses_filled.csv', help='Output CSV file (default: addresses_filled.csv)')

    args = parser.parse_args()

    addresses = pd.read_csv(args.file)
    GOOGLE_API_KEY = os.getenv('GOOGLE_API_KEY') if args.key is None else args.key
    if GOOGLE_API_KEY is None:
        raise ValueError("Google API key not provided")

    City = []
    State = []
    ZIP_Code = []
    Country = []
    counter = 0
    for i, add in addresses.iterrows():
        if add.Country == 'NA or No Country':
            counter += 1
            city, state, country = get_city_state_from_zipcode(add['ZIP Code'])
        else:
            city = add.City
            state = add.State
            country = add.Country
        City.append(city)
        State.append(state)
        Country.append(country)
        ZIP_Code.append(add['ZIP Code'])

    addresses['City'] = City
    addresses['State'] = State
    addresses['Country'] = Country
    addresses.to_csv(args.output, index=False)