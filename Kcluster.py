#import libraries
import pandas as pd
import matplotlib.pyplot as plt
from sklearn.cluster import KMeans
from sklearn.preprocessing import MinMaxScaler
import plotly.express as px
import numpy as np
import plotly.graph_objects as go

#dataframe
df1=pd.read_csv("https://raw.githubusercontent.com/Rheyhan/TukhirAED/main/Mentah.csv")
df1=df1.dropna()
n=(df1.shape)[0] #getting n
df1.info()

#scaling
scale = MinMaxScaler()
df2=df1.drop(["Country Name"], axis=1)
df = scale.fit_transform(df2)
df=pd.DataFrame(df)
axiscol=[(df2.axes)[1][i] for i in range(0, (df2.shape)[1])]   
df=df.set_axis(axiscol, axis='columns')
df=df.set_index(df1["Country Name"])

#exploration
    #plot
fig = px.scatter_matrix(df, width=1200, height=1600)
fig.show()
    #matrixcorr
matrixcorr = df.corr()
print("Correlation matrix is : ")
print(matrixcorr)

    #elbow method
cs = []
for i in range(1, 11):
    kmeans = KMeans(n_clusters = i, init = 'k-means++', n_init=10)
    kmeans.fit(df)
    cs.append(kmeans.inertia_)
    
fig = go.Figure(data=go.Scatter(x=np.arange(1,11),y=cs))
fig.update_layout(title="Inertia vs Cluster Number",xaxis=dict(range=[0,11],title="Cluster Number"),
                yaxis={'title':'Inertia'},
                annotations=[
    dict(
        x=3,
        y=cs[2],
        xref="x",
        yref="y",
        text="Elbow!",
        showarrow=True,
        arrowhead=7,
        ax=20,
        ay=-40
    )])
fig.show()

#visualize (polar)
kmeans = KMeans(n_clusters = 3, init = 'k-means++', n_init=10)
kmeans.fit(df)
df['label']=kmeans.labels_
polar=df.groupby("label").mean().reset_index()
polar=pd.melt(polar,id_vars=["label"])
fig4 = px.line_polar(polar, r="value", theta="variable", color="label", line_close=True,height=800,width=1400)
fig4.show()

pie=df.groupby('label').size().reset_index()
pie.columns=['label','value']
fig2=px.pie(pie,values='value',names='label',color=['blue','red','green'])
fig2.show()