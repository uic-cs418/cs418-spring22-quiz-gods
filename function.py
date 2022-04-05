import seaborn as sns
import matplotlib.pyplot as plt


def plot(avgDf):
    fig = plt.gcf()
    fig.set_size_inches(15, 5)
    plot = sns.barplot(x='Race', y='Average Number of Deaths', data=avgDf)
    plt.title("Average Numbers of Death per Race")


def dataFrameReturn(dataframe):
    RaceCounter = {}
    for row in dataframe.iterrows():
        (index, loc) = row

    if loc['Race/Ethnicity'] in RaceCounter:
        (currSum, currDeath) = RaceCounter[loc['Race/Ethnicity']]
        currSum = currSum + 1
        currDeath = currDeath + loc['Deaths per 100,000']

        RaceCounter[loc['Race/Ethnicity']] = (currSum, currDeath)
    else:
        RaceCounter[loc['Race/Ethnicity']] = (1, loc['Deaths per 100,000'])

    avgRace = {}
    for key in RaceCounter:
        (divisor, numerator) = RaceCounter[key]
        tempAvg = numerator/divisor
        avgRace[key] = tempAvg

    output = []
    for key in avgRace:
        tempList = []
        tempList.append(key)
        tempList.append(avgRace[key])
        output.append(tempList)

    avgDf = pd.DataFrame(output, columns=['Race', 'Average Number of Deaths'])
    return avgDf
