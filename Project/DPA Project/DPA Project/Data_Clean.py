class Data_Clean():
    # Importing relevant packages.
    import pandas as pd
    import re
    def __init__(self):
        # Reading the web-scrapped csv files from the local system.
        self.df_09 = self.pd.read_csv("2009.csv")
        self.df_10 = self.pd.read_csv("2010.csv")
        self.df_11 = self.pd.read_csv("2011.csv")
        self.df_12 = self.pd.read_csv("2012.csv")
        self.df_13 = self.pd.read_csv("2013.csv")
        self.df_14 = self.pd.read_csv("2014.csv")
        self.df_15 = self.pd.read_csv("2015.csv")
        self.df_16 = self.pd.read_csv("2016.csv")
        self.df_17 = self.pd.read_csv("2017.csv")
        self.df_18 = self.pd.read_csv("2018.csv")
        self.df_19 = self.pd.read_csv("2019.csv")
        self.df_20 = self.pd.read_csv("raw_data.csv")
        
        # Dropping the redundant rows.
        self.df_09.drop_duplicates("ID", inplace = True)
        self.df_10.drop_duplicates("ID", inplace = True)
        self.df_11.drop_duplicates("ID", inplace = True)
        self.df_12.drop_duplicates("ID", inplace = True)
        self.df_13.drop_duplicates("ID", inplace = True)
        self.df_14.drop_duplicates("ID", inplace = True)
        self.df_15.drop_duplicates("ID", inplace = True)
        self.df_16.drop_duplicates("ID", inplace = True)
        self.df_17.drop_duplicates("ID", inplace = True)
        self.df_18.drop_duplicates("ID", inplace = True)
        self.df_19.drop_duplicates("ID", inplace = True)
        self.df_20.drop_duplicates("ID", inplace = True)
        
        # Removing unwanted columns from the data-frames.
        del(self.df_09["Unnamed: 0"])
        del(self.df_10["Unnamed: 0"])
        del(self.df_11["Unnamed: 0"])
        del(self.df_12["Unnamed: 0"])
        del(self.df_13["Unnamed: 0"])
        del(self.df_14["Unnamed: 0"])
        del(self.df_15["Unnamed: 0"])
        del(self.df_16["Unnamed: 0"])
        del(self.df_17["Unnamed: 0"])
        del(self.df_18["Unnamed: 0"])
        del(self.df_19["Unnamed: 0"])
        del(self.df_20["Unnamed: 0"])

        # Adding extra columns for the data analysis purpose.
        self.df_09["Year"] = 2009
        self.df_10["Year"] = 2010
        self.df_11["Year"] = 2011
        self.df_12["Year"] = 2012
        self.df_13["Year"] = 2013
        self.df_14["Year"] = 2014
        self.df_15["Year"] = 2015
        self.df_16["Year"] = 2016
        self.df_17["Year"] = 2017
        self.df_18["Year"] = 2018
        self.df_19["Year"] = 2019
        
        # Creating train_dataframe by concatenating previous 10 years data that is from 2009 to 2019.
        self.train_data = self.pd.concat([self.df_09, self.df_10, self.df_11, self.df_12, self.df_13, self.df_14,
                                     self.df_15, self.df_16, self.df_17, self.df_18, self.df_19], ignore_index = True)
        # Creating test_dataframe from 2020 data.
        self.test_data = self.df_20
    
    # Function to return finalized cleaned train and test data. 
    def data_cleaning(self):
        tr_data = self.training_data(self.train_data)
        te_data = self.training_data(self.test_data)
        return tr_data, te_data
        
        
    def training_data(self, data):
        # Converting shorthand notations to real numbers.
        dg_mv = []
        for i in data['Market value']:
            digi_mv = self.re.findall(r'\d+', i)
            if digi_mv[0].isdigit():
                if i[-1] == 'M':
                    d_mv = int(digi_mv[0]) * 1000000
                    dg_mv.append(d_mv)
                elif i[-1] == 'K':
                    k_mv = int(digi_mv[0]) * 1000
                    dg_mv.append(k_mv)
                else:
                    k_mv = int(digi_mv[0])
                    dg_mv.append(k_mv)
                    
        
        dg_wage = []
        for i in data['Wage']:
            digi_wage = self.re.findall(r'\d+', i)
            if digi_wage[0].isdigit():
                if i[-1] == 'M':
                    d_wage = int(digi_wage[0]) * 1000000
                    dg_wage.append(d_wage)
                elif i[-1] == 'K':
                    k_wage = int(digi_wage[0]) * 1000
                    dg_wage.append(k_wage)
                else:
                    k_wage = int(digi_wage[0])
                    dg_wage.append(k_wage)
                    
        
        del(data["Market value"])
        del(data["Wage"])
        
        data["Market_value"] = dg_mv
        data["Wage"] = dg_wage
        
        data = data[~data["Team"].str.contains("[0-9]")]
        data = data[~data["Name"].str.contains("[0-9]")]
        data = data[~data["Position"].str.contains("[0-9]")]
        data = data[data["Market_value"]!=0]
        data = data[data["Wage"]!=0]
        return data
