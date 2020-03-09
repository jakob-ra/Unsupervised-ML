# Imports
import pandas as pd
import numpy as np
from sklearn.preprocessing import scale
from sklearn.decomposition import PCA
from matplotlib import pyplot as plt

# Read data
url = 'https://github.com/jakob-ra/Unsupervised-ML/raw/master/fifa.xlsx'
df = pd.read_excel(url)

# drop non-skill variables
df.drop(columns=['name', 'club', 'Position', 'eur_value', 'eur_wage', 'eur_release_clause'], inplace=True)

# scale
df = pd.DataFrame(scale(df), columns=df.columns)

# histogram of all variables
fig, ax = plt.subplots(1, 1, figsize=(20, 20))
df.hist(ax=ax)
plt.show()

# summary stats
print(df.describe())

# own PCA implementation
def pca(X,r):
    """ Returns r principal components of X """
    # SVD decomposition
    U,S,V = np.linalg.svd(df, full_matrices=False, compute_uv=True)

    # Take the first r loadings in V
    V_reduced = V[:r]

    return X@V_reduced.T

# Compare histograms
X_pca = pd.DataFrame(pca(df,3))
X_pca.hist()
plt.show()

pca_package = PCA(n_components=3)
X_pca_package = pd.DataFrame(pca_package.fit_transform(df))
X_pca_package.hist()
plt.show()