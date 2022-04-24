import pandas as pd
import numpy as np
import string
import seaborn as sns 
import matplotlib.pyplot as plt

def plotted(Df):
    sns.set_theme(style="darkgrid")
    fig = plt.gcf()
    fig.set_size_inches(15, 5)
    plot = sns.barplot(x='Race', y='Average Number of Deaths', data=Df)
    plt.title("Average Numbers of Death per Race")


def avgDf(df):
    RaceCounter = {}
    for row in df.iterrows():
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

    avgD = pd.DataFrame(output, columns=['Race', 'Average Number of Deaths'])
    
    sns.set_theme(style="darkgrid")
    fig = plt.gcf()
    fig.set_size_inches(15, 5)
    plot = sns.barplot(x='Race', y='Average Number of Deaths', data=avgD, hue = 'Race', palette = "rocket")
    plt.title("Average Numbers of Death per Race")

def removeC(tempDf, NotUsed):
    
    useCols = []
    data = {}
    for col in tempDf.columns:
        if col not in NotUsed:
            data[col] = tempDf[col]

    newDf = pd.DataFrame(data) 
    return newDf

def getSufficientData(df):
    df = df[df['Sufficiency?'] != 'Insufficient Data']
    df = df[df['Race/Ethnicity'] != 'Overall']
    df = df[df['Gender'] != 'Female']
    df = df[df['Gender'] != 'Male']
    newDf = df
    return newDf