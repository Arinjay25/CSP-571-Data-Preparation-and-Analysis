from Train_Test_Split import Train_Test_Split
class ML(Train_Test_Split):
    # Importing relevant packages.
    import pandas as pd
    import pickle
    import matplotlib.pyplot as plt
    from IPython.display import display
    
    def __init__(self):
        # Inheriting Train_Test_Split class.
        super().__init__()
        # Initializing with inherited data.
        self.xtrain = super().xy_train()[0]
        self.ytrain = super().xy_train()[1]
        self.xtest = super().xy_test()[0]
        self.ytest = super().xy_test()[1]
        self.test_dat = super().test_dat()
        
    
    # will return Decision Tree model.
    def decision_tree(self):        
        from sklearn import tree
        from sklearn.metrics import accuracy_score
        self.clf = tree.DecisionTreeClassifier(criterion = "entropy", random_state = 2)
        self.clf.fit(self.xtrain, self.ytrain.values.ravel())
        self.clf_predict = self.clf.predict(self.xtest)
        self.accuracy = accuracy_score(self.ytest, self.clf_predict)
        return self.accuracy, self.clf_predict, self.clf
    
    # will return Random Forest model.
    def random_forest(self):
        from sklearn.ensemble import RandomForestClassifier
        from sklearn.metrics import accuracy_score
        self.clf = RandomForestClassifier(random_state = 3000)
        self.clf.fit(self.xtrain, self.ytrain.values.ravel())
        self.clf_predict = self.clf.predict(self.xtest)
        self.accuracy = accuracy_score(self.ytest, self.clf_predict)
        return self.accuracy, self.clf_predict, self.clf
    
    # will return Random Forest model.
    def svm(self):
        from sklearn import svm
        from sklearn.metrics import accuracy_score
        self.clf = svm.SVC()
        self.clf.fit(self.xtrain, self.ytrain.values.ravel())
        self.clf_predict = self.clf.predict(self.xtest)
        self.accuracy = accuracy_score(self.ytest, self.clf_predict)
        return self.accuracy, self.clf_predict, self.clf
    
    # will return K-Nearest Neighbor model.
    def knn(self):
        from sklearn.neighbors import KNeighborsClassifier
        from sklearn.metrics import accuracy_score
        self.clf = KNeighborsClassifier(n_neighbors=50)
        self.clf.fit(self.xtrain, self.ytrain.values.ravel())
        self.clf_predict = self.clf.predict(self.xtest)
        self.accuracy = accuracy_score(self.ytest, self.clf_predict)
        return self.accuracy, self.clf_predict, self.clf
    
    # will return the best out of 4 models.
    def all_models(self):
        self.dict = {'Decision Tree': self.decision_tree(), 'Random Forest': self.random_forest(),
                'Support vector machine(SVM)': self.svm(), 'K-Nearest Neighbor': self.knn()}
        self.data = [["Decision Tree", self.dict["Decision Tree"][0]], ["Random Forest", self.dict["Random Forest"][0]],
                    ["Support vector machine(SVM)", self.dict["Support vector machine(SVM)"][0]], ["K-Nearest Neighbor", self.dict["K-Nearest Neighbor"][0]]]
        self.df = self.pd.DataFrame(self.data, columns = ["Model", "Accuracy"])
        self.d = self.df.style.apply(lambda x: ['background: lightblue' if i == max(self.df["Accuracy"]) else '' for i in self.df["Accuracy"]])
        self.best_model = self.dict[self.df[self.df["Accuracy"] == max(self.df["Accuracy"])]["Model"].values[0]]
        self.bm = self.best_model[2]
        self.display(self.d)
        return self.best_model[2]
    
    # saves the best model in the local system as .pkl file.
    # Initially we are not saving it as it requires around 2.3 GB of storage to store the model, but you can save it using the function save_best_model
    def save_best_model(self):
        self.pkl_filename = "best_model.pkl"
        with open(self.pkl_filename, 'wb') as file:
            self.pickle.dump(self.all_models(), file)
    
    # reads the saved model.
    def read_saved_model(self):
        self.pkl_filename = "best_model.pkl"
        with open(self.pkl_filename, 'rb') as file:
            self.pickle_model = self.pickle.load(file)
        return self.pickle_model
    
    # return the predicted rating of the test data.
    def after_predictions(self):
        # remove the below comment if you want to save the model in your current working directory.
        # self.save_best_model
        self.predict = self.all_models().predict(self.xtest)
        self.xtest.insert(10, "Predicted Overall Rating", self.predict)
        self.xtest.insert(11, "Actual Overall Rating", self.ytest)
        self.xtest.insert(1, "Name", self.test_dat["Name"].values)
        self.xtest.insert(2, "Team", self.test_dat["Team"].values)
        self.xtest.insert(4, "Position", self.test_dat["Position"].values)
        self.xtest.insert(0, "ID", self.test_dat["ID"].values)
        self.xtest.insert(8, "Goal-Keeper stats", self.test_dat["Goal-Keeper stats"].values)
        print("Showing Predicted Overall Rating of first 10 rows.")
        self.display(self.xtest.head(10))
        
        self.ax = self.xtest[["Actual Overall Rating", "Predicted Overall Rating"]][:10].plot(kind='bar')
        self.ax.set_xticklabels(self.xtest["Name"][:10],rotation=45)
        self.ax.set_xlabel("Player Name")
        
        self.ax = self.xtest[["Actual Overall Rating", "Predicted Overall Rating"]][-10:].plot(kind='bar', colormap='Accent')
        self.ax.set_xticklabels(self.xtest["ID"][-10:],rotation=45)
        self.ax.set_xlabel("Player ID")
        self.plt.show()
        return self.xtest
