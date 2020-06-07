from Data_Analyze import Data_Analyze
class Train_Test_Split(Data_Analyze):
    # Importing relevant packages.
    import pandas as pd
    
    def __init__(self):
        # Inheriting Data_Analyze class.
        super().__init__()
        self.data = super().all_graphs()
        # Initializing with inherited data.
        self.test_data = self.data[1] 
        self.train_data = self.data[0]
        self.final_data = self.pd.concat([self.train_data, self.test_data])
        
    # will return test data.
    def test_dat(self):
        return self.test_data
    
    # will return train data.
    def train_dat(self):
        return self.train_data
    
    # will return feature variables.
    def features(self):
        self.x = self.final_data[["Age", "Best Overall", "Attacking stats", "Movement stats", "Power stats", "Defence stats", "Skill sets", "Total potential", "Market_value", "Wage"]]
        return self.x
    
    # will return traget variable.
    def target(self):
        self.y = self.final_data[["Overall Rating"]]
        return self.y
    
     # Calculating percent for the split of the data.
    def percenta(self):
        self.percent = (len(self.train_data) / len(self.final_data)) * 100
        return self.percent
    
    # Calculating the length of train and test data.
    def calculations(self):
        self.percent = (len(self.train_data) / len(self.final_data)) * 100
        self.train_len = int((self.percent * len(self.final_data)) / 100)
        self.test_len = len(self.final_data) - self.train_len
        return self.train_len, self.test_len
    
    # will return train-feature and train-target variables separately.
    def xy_train(self):
        a = self.calculations()
        self.x_train, self.y_train = self.features().head(a[0]), self.target().head(a[0])
        return self.x_train, self.y_train
    
    # will return test-feature and test-target variables separately.
    def xy_test(self):
        a = self.calculations()
        self.x_test, self.y_test = self.features().tail(a[1]), self.target().tail(a[1])
        return self.x_test, self.y_test
