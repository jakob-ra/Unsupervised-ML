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
    U,S,V = np.linalg.svd(X, full_matrices=False, compute_uv=True)

    # Take the first r loadings in V
    V_reduced = V[:r]

    return X@V_reduced.T

# Compare histograms for 3 components
# X_pca = pd.DataFrame(pca(df,2))
# X_pca.hist()
# plt.show()
#
# pca_package = PCA(n_components=2)
# X_pca_package = pd.DataFrame(pca_package.fit_transform(df))
# X_pca_package.hist()
# plt.show()

# scatter
plt.scatter(X_pca[0], -X_pca[1])
plt.show()

# scatter
plt.scatter(X_pca_package[0], X_pca_package[1])
plt.show()

# own sparse PCA implementation
def soft_threshold(a,lambda_par):
    """ Returns soft threshold value of vector a for given lambda """

    return [np.sign(elem)*(abs(elem)-lambda_par > 0)*(abs(elem)-lambda_par) for elem in a]

# soft_threshold([2,0.3,-0.7], 0.5)

def soft_threshold_update(a,lambda_par):
    """ Returns updated u and v for given lambda and input vector a """

    return soft_threshold(a,lambda_par)/np.linalg.norm(soft_threshold(a,lambda_par), ord=2)

constraint = {'type': 'ineq', 'fun': -np.linalg.norm(soft_threshold_update(X@v,x), ord=1) + c1}
scipy.optimize.minimize(lambda x: x, 1, method='Newton-CG', constraints=(constraint))

def pen_r1_SVD(X,c1,c2,max_iter):
    """ Penalized rank 1 SVD """
    _,_,V = np.linalg.svd(X, full_matrices=False, compute_uv=True)
    v = V[0] # initialize v as first right singular vector of X

    for i in range(max_iter):
        # set lambda to 0 if constraint is not binding
        if np.linalg.norm(soft_threshold_update(X@v,0), ord=1) < c1:
            lambda_1 = 0
        else:


def sparse_PCA(X,c1,c2,max_iter):
    """ Returns """