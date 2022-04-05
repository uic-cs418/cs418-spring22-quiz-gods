def plot(avgDf):
    fig = plt.gcf()
    fig.set_size_inches(15, 5)
    plot = seaborn.barplot(x='Race', y='Average Number of Deaths', data=avgDf)
    plt.title("Average Numbers of Death per Race")
