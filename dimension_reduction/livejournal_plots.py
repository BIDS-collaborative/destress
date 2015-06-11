# Now I will hopefully get some interesting plots set up
# (c) June 2015 by Daniel Seita

import matplotlib.pyplot as plt
import numpy as np

# This is the 132 x 2 matrix of "weights" for non-negative matrix factorization
dir = "/Users/danielseita/BIDMach/moods_data/"
data = np.loadtxt(dir + 'NMF_6_result.txt')

labels = open('sorted_moodIDs').read().splitlines()
plt.figure(num=None, figsize=(30,20), dpi=240)
plt.plot(data[:,0], data[:,1], 'ro')
for (label, x, y) in zip(labels, data[:, 0], data[:, 1]):
    plt.annotate(
        label, # What actually shows up on the plot
        xy = (x, y), xycoords = 'data', # Coordinates of this point
        xytext = (x+0.0025, y+0.01), textcoords = 'data', # Coordinates of the text
        bbox = dict(boxstyle = 'round,pad=0.1', fc = 'yellow', alpha = 0.5),
        arrowprops = dict(arrowstyle = '->', connectionstyle = 'arc3')
                )
plt.savefig("test.png")
