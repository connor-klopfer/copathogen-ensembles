# The main file for rewiring
import pandas as pd

def read_in_data():
    all_data = pd.read_csv('data/simple_all.csv')
    return all_data


def get_pathogens():
    names = pd.read_csv('data/name_key.csv', header = 0)
    return names


if __name__ == "__main__":
    data = read_in_data()
    names = get_pathogens()
    print(names.columns)
    print(names['actual'].loc[names['ispathogen'] > 0].values)