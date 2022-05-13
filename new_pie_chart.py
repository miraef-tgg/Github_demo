import csv
import numpy  as np
import pandas as pd
from matplotlib import pyplot as plt

#py -m pip install pandas
#(should be in virutal_env_1)

#get wide data
wide = pd.read_csv("bike_data_wide.csv")


#make a pie chart!


categoires = [len(wide.loc[wide['member_gender'] == "Male"]),len(wide.loc[wide['member_gender'] == "Female"])]
labels  = ["male", "female"]


plt.pie(categoires, labels = labels, colors = ["darkblue", "blue"])
plt.show()


