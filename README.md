# Comprehensive-analysis-of-network-traffic-data
Code resource (R) for the paper: https://onlinelibrary.wiley.com/doi/abs/10.1002/cpe.4181

The data set link is: https://drive.google.com/open?id=0B6Qbk6GWTk7NdEJtVHRJQ3ZqSVE (.RData)

Classification codes using R tool: using Random Forest (RF), C50, KNN with PCA and without PCA

Install R firstly. Put the dataset in the folder. Then run corresponding code.

Paper Abstract:
With the large volume of network traffic flow, it is necessary to preprocess raw data before classification to gain the accurate results speedily. Feature selection is an essential approach in preprocessing phase. The principal component analysis (PCA) is recognized as an effective and efficient method. In this paper, we classify network traffic flows by using the PCA technique together with 6 machine learning algorithms—Naive Bayes, decision tree, 1‐nearest neighbor, random forest, support vector machine, and H2O. We analyzed the impact of PCA on the classification results by applying each algorithm with and without PCA onto the data set. Experiments were set out by varying the size of input data sets, and the performances were measured from 2 aspects, including average overall accuracy and F‐measure. The computational time was also considered in analyzing the performance. Our results showed that random forest and 1‐nearest neighbor were the top 2 algorithms among all the 6 regarding the 2 metrics mentioned above. Then we continued the study of PCA impact on per class level with these 2 algorithms as examples. And the positive correlation between overall impact and the number of class with significant impact was revealed. Lastly, the visualization was used in exploring the reasons of the impacts caused by PCA. Two factors are considered in PCA's impact on per class level: benefit for classes grouped by PCA and mislabeled error interfered by nearby groups. 
