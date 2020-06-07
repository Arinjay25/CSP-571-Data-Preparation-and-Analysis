from Data_Clean import Data_Clean
class Data_Analyze(Data_Clean):
    # Importing relevant packages.
    import matplotlib.pyplot as plt
    from IPython.display import display
    import seaborn as sn
    
    def __init__(self):
        # Inheriting Data_Clean class.
        super().__init__()
        self.data = super().data_cleaning()
        # Initializing with inherited data.
        self.train_data = self.data[0]
        self.test_data = self.data[1] 

    def graph_1(self):
        # Plot1: Age v/s number of players.
        ax = self.train_data['Age'].value_counts().sort_index().plot(kind = 'bar')
        ax.set(xlabel = "Age")
        ax.set(ylabel = "No. of players")
        self.plt.show()
    
    def graph_2(self):
        # Plot2: Age v/s average of Overall Rating.
        ax = self.train_data.groupby("Age")["Overall Rating"].mean().plot(marker = "o")
        ax.set(ylabel = "Avg. Overall Rating")
        self.plt.show()
    
    def graph_3(self):
        # Plot3: Age v/s average of Best Overall.
        ax = self.train_data.groupby("Age")["Best Overall"].mean().plot(marker = "o")
        ax.set(ylabel = "Avg. Best Overall")
        self.plt.show()
    
    def graph_4(self):
        # Plot4: Age v/s average of Total Potential.
        ax = self.train_data.groupby("Age")["Total potential"].mean().plot(marker = "o")
        ax.set(ylabel = "Avg. Total potential")
        self.plt.show()
    
    def graph_5(self):
        # Plot5: Age v/s average of Skill sets.
        ax = self.train_data.groupby("Age")["Skill sets"].mean().plot(marker = "o")
        ax.set(ylabel = "Avg. Skill sets")
        self.plt.show()
    
    def graph_6(self):
        # Plot6: Age v/s average of Power stats.
        ax = self.train_data.groupby("Age")["Power stats"].mean().plot(marker = "o")
        ax.set(ylabel = "Avg. Power stats")
        self.plt.show()
    
    def graph_7(self):
        # Plot7: Age v/s average of Defence stats.
        ax = self.train_data.groupby("Age")["Defence stats"].mean().plot(marker = "o")
        ax.set(ylabel = "Avg. Defence stats")
        self.plt.show()
        
    def data_description(self):
        # Returning the insights of the data.
        describe = self.train_data[self.train_data.columns.difference(["ID", "Year"])].describe()
        self.display(describe)
    
    def cor_matrix(self):
        # Returning the correlation matrix of all the columns.
        self.cor = self.train_data[self.train_data.columns.difference(["ID", "Year"])].corr()
        self.plt.figure(figsize = (12,6))
        return self.cor
    
    def all_graphs(self):
        self.graph_1()
        self.graph_2()
        self.graph_3()
        self.graph_4()
        self.graph_5()
        self.graph_6()
        self.graph_7()
        # Generating the heatmap of the correlation matrix.
        self.sn.heatmap(self.cor_matrix(), annot=True)
        self.display(self.cor_matrix())
        self.data_description()
        return self.train_data, self.test_data
        