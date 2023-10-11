from pandas import Timestamp

MISSING_VALUE = 9999

# Dates: Bangladesh Specific

seasons = [
    {
        'name': 'basanta',
        'start' : Timestamp(2020, 2, 15),
        'end' : Timestamp(2020, 4, 15)

    },
    {
        'name': 'grisma',
        'start' : Timestamp(2020, 4, 15),
        'end' : Timestamp(2020, 6, 15)

    },
    {
        'name': 'barsa',
        'start' : Timestamp(2020, 6, 15),
        'end' : Timestamp(2020, 8, 15)

    },
    {
        'name': 'sharat',
        'start' : Timestamp(2020, 8, 15),
        'end' : Timestamp(2020, 10, 15)

    },
    {
        'name': 'hemanta',
        'start' : Timestamp(2020, 10, 15),
        'end' : Timestamp(2020, 12, 15)

    },
    {
        'name': 'shit',
        'start' : Timestamp(2020, 12, 15),
        'end' : Timestamp(2020, 2, 15)

    }
]

# Timepoint partitions for the specific stools. 
timepoints = {
        'Asymptomatic':{
            'maled': {
                'upper_bound': 14, 
                'lower_bound' : 7, 
                'timepoints': {
                    '1month': 4 * 7, # 6 weeks 
                    '3month': 13 * 7, # 15 Weeks 
                    '24weeks': 26 * 7 # 24 Weeks 
                }
            },
            'provide': {
                'upper_bound': 21, 
                'lower_bound' : 7, 
                'timepoints': {
                    '1month': 6 * 7, # 6 weeks 
                    '3month': 15 * 7, # 15 Weeks 
                    '24weeks': 24 * 7 # 24 Weeks 
                }
            }
        },
        # TBD
        'Diarrhea':{
            'maled': {
                'upper_bound': 15, 
                'lower_bound' : 15, 
                'timepoints': {
                    '1month': 4 * 7, # 6 weeks 
                    '3month': 13 * 7, # 15 Weeks 
                    '24weeks': 26 * 7 # 24 Weeks 
                }
            },
            'provide': {
                'upper_bound': 15, 
                'lower_bound' : 15, 
                'timepoints': {
                    '1month': 6 * 7, # 6 weeks 
                    '3month': 15 * 7, # 15 Weeks 
                    '24weeks': 24 * 7 # 24 Weeks 
                }
            }
        }
    }