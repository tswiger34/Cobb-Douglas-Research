from sklearn.cluster import KMeans
import numpy as np
import csv 

with open("Batting Data.csv") as batting_data_csv:
  reader = csv.reader(batting_data_csv, delimiter=",")

  X_raw = [line for line in reader]
  X_raw.pop(0)

  X = np.array(X_raw)[:150, 2:]

  rows, cols =  X.shape

  X_with_correct_typing = []
  for i in range(rows):
    X_with_correct_typing.append([])
    for j in range(cols):
      X_with_correct_typing[i].append(float(X[i][j]))

  X = np.array(X_with_correct_typing)
  

  kmeans = KMeans(n_clusters=7, random_state=0, n_init="auto").fit(X)
  print("cluster labels", kmeans.labels_)
  print(len(kmeans.labels_))


  # kmeans.predict([[0, 0], [12, 3]])

  print("cluster centers", kmeans.cluster_centers_)