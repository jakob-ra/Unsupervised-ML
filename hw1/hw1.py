# Imports
import pyreadr
import pandas as pd

# Read data
url = 'https://github.com/jakob-ra/Unsupervised-ML/raw/master/hw1/FIFA2017_NL.RData'
data = pyreadr.read_r(url) # also works for Rds
data = pyreadr.read_r('../hw1/fifa.RData') # also works for Rds
