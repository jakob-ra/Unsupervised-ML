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

# Compare own implementation vs package
X_pca = pd.DataFrame(pca(df,2))

pca_package = PCA(n_components=2)
X_pca_package = pd.DataFrame(pca_package.fit_transform(df))

# X_pca.hist()
# plt.show()

# X_pca_package.hist()
# plt.show()

# Scatterplot
fig, axes = plt.subplots(1, 2, figsize=(10, 5))
axes[0].scatter(X_pca[0], -X_pca[1])
axes[0].set_title('Own implementation')
axes[0].set_xlabel('PC1')
axes[0].set_ylabel('PC2')

# scatter
axes[1].scatter(X_pca_package[0], X_pca_package[1])
axes[1].set_title('Package skicit-learn')
axes[1].set_xlabel('PC1')
axes[1].set_ylabel('PC2')

plt.show()

# Plot number of principal components vs explained variance
pca_fin = PCA(n_components=10)
pca_fin.fit(X)
plt.scatter(range(1,11),pca_fin.explained_variance_ratio_*100)
plt.xticks(range(1,11))
plt.title('Variance explained by PCA')
plt.xlabel('Principal component')
plt.ylabel('Percent of variance')
plt.show()


# own sparse PCA implementation
def soft_threshold(a,lambda_par):
    """ Returns soft threshold value of vector a for given lambda """

    return [np.sign(elem)*(abs(elem)-lambda_par) if (abs(elem)-lambda_par > 0) else 0 for elem in a]

def soft_threshold_update(a,lambda_par):
    """ Returns update for given lambda and input vector a """

    return soft_threshold(a,lambda_par)/np.linalg.norm(soft_threshold(a,lambda_par), ord=2)

def binary_search(a, target, n_iter=10, low=0, high=3):
    """ Performs a binary search for value of lambda that leads to a L1-norm close to target
    value (fixed number of iterations n_iter) for given vector a """
    for i in range(n_iter):
        middle = (high-low)/2
        tar_middle = np.linalg.norm(soft_threshold_update(a,middle), ord=1)
        if target - tar_middle > 0:
            high = middle
        else:
            low = middle

    return middle

def pen_r1_SVD(X,c1,c2,max_iter=50):
    """ Penalized rank 1 SVD """
    _,_,V = np.linalg.svd(X, full_matrices=False)
    v = V[0] # initialize v as first right singular vector of X

    for i in range(max_iter):
        lambda_1 = binary_search(X@v,c1)
        u = soft_threshold_update(X@v,lambda_1)
        lambda_2 = binary_search(X.T@u,c2)
        v = soft_threshold_update(X.T@u,lambda_2)

    sigma = u.T@X@v

    return u, v, sigma

def deflation(X,c1,c2,n_components=1):
    """ Penalized rank 1 SVD """
    R = X
    u_list = []
    v_list = []
    sigma_list = []

    for i in range(n_components):
        u,v,sigma = pen_r1_SVD(R,c1,c2)
        R = R - sigma*np.outer(u,v.T)

        u_list.append(u)
        v_list.append(v)
        sigma_list.append(sigma)

    return np.vstack(u_list), np.vstack(v_list), sigma_list

U, V, Sigma = deflation(df,2,2,n_components=10)

plt.scatter((df.values@V.T)[:,0], -(df.values@V.T)[:,1])
plt.show()

print(np.round(V[0], 3))
print(np.round(V[1], 3))


def sparse_PCA(X,c1,c2,max_iter):
    """ Returns """