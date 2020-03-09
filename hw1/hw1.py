# Imports
import pyreadr
import pandas as pd
import urllib.request

# Read data
url = 'https://github.com/jakob-ra/Unsupervised-ML/raw/master/hw1/FIFA2017_NL.RData'
data = urllib.request.urlopen(url)
data = pyreadr.read_r(url) # also works for Rds
pd.read_csv('FIFA2017_NL.RData')
