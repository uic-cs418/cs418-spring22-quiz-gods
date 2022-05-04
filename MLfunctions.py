import numpy as np
import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt
from scipy.stats import zscore
from sklearn.preprocessing import StandardScaler
from sklearn import preprocessing
from sklearn.metrics import accuracy_score, classification_report, confusion_matrix
from sklearn.model_selection import train_test_split
from sklearn.ensemble import RandomForestClassifier
from sklearn.tree import DecisionTreeClassifier
from sklearn.linear_model import LogisticRegression
from sklearn.naive_bayes import BernoulliNB, GaussianNB
from sklearn.svm import SVC
from sklearn.neighbors import KNeighborsClassifier
from sklearn.metrics import accuracy_score, confusion_matrix, roc_curve, r2_score, auc
import matplotlib.colors as mcolors
from DFfunctions import *

def showHeatMap(heartML):
    heartMLNotUsed = ['age', 'sex']
    heartML = removeC(heartML, heartMLNotUsed)
    heartML = heartML.rename(columns = {'cp':'chest_pain','trtbps': 'resting_bp(mmhg)','chol': 'cholestoral(mg/dl)','fbs': 'fasting_blood_sugar(1/0)',
                                        'thalachh': 'max_heart_rate','exng': 'exercise_anigna(1/0)','caa': 'num_major_vessels(0-3)'
                                        })
    heartML.drop_duplicates(inplace=True)
    plt.figure(figsize = (15,10))
    sns.heatmap(heartML.corr(), annot = True)
    plt.show()
    return heartML

def test:
    