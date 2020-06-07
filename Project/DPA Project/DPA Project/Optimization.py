from ML import ML
class Optimization(ML):
    # Importing relevant packages.
    import pandas as pd
    import numpy as np
    from IPython.display import display
    
    def __init__(self):
        # Inheriting Train_Test_Split class.
        super().__init__()
        # Initializing with inherited data.
        self.xtest = super().after_predictions()
    
    # This will return all the top playing teams.
    def top_teams(self):
        self.testing = self.xtest.copy()
        self.teams = self.testing.groupby("Team").mean()
        
        # Filtering all the teams whose average rating is greater than 75.
        self.top = self.teams[self.teams["Predicted Overall Rating"] > 75.0].sort_values("Predicted Overall Rating", ascending=False)
        self.top_qualifying_teams = self.top.index.values
        
        self.top_qualifying_team = []
        for i in self.top_qualifying_teams:
            # Filtering all the teams whose total number of players are between 11 and 15.
            if (len(self.testing[self.testing["Team"] == i]["Position"].unique()) >= 11) & (len(self.testing[self.testing["Team"] == i]["Position"].unique()) <= 15):
                self.top_qualifying_team.append(i)
        
        self.team_dict = {}
        for i in self.top_qualifying_team:
            self.team = self.testing[self.testing["Team"] == i].sort_values(['Position','Predicted Overall Rating'],ascending=False).groupby('Position').head(1)
            self.team_dict.update({i:self.team})
            
        self.all_team = self.pd.concat([self.team_dict[list(self.team_dict.keys())[0]],
                                   self.team_dict[list(self.team_dict.keys())[1]],
                                   self.team_dict[list(self.team_dict.keys())[2]],
                                   self.team_dict[list(self.team_dict.keys())[3]],
                                   self.team_dict[list(self.team_dict.keys())[4]],
                                   self.team_dict[list(self.team_dict.keys())[5]],
                                   self.team_dict[list(self.team_dict.keys())[6]],
                                   self.team_dict[list(self.team_dict.keys())[7]],
                                   self.team_dict[list(self.team_dict.keys())[8]],
                                   self.team_dict[list(self.team_dict.keys())[9]],
                                   self.team_dict[list(self.team_dict.keys())[10]],
                                   self.team_dict[list(self.team_dict.keys())[11]],
                                   self.team_dict[list(self.team_dict.keys())[12]]],)
        return self.all_team
    
    # Selecting best team within a budget.
    def taking_team(self):
        self.top_team = self.top_teams()
        self.mark_val = self.top_team.groupby("Team").sum()
        self.mark_val.sort_values('Market_value', ascending=False)
        
        # Filtering all the teams with total market value less than 500000000.
        self.mark_val[self.mark_val["Market_value"]<500000000].sort_values("Predicted Overall Rating", ascending=False)
        
        self.playing_13 = self.top_team.groupby("Team").agg({'Market_value':'sum', 'Predicted Overall Rating':'mean'})
        self.playing_13 = self.playing_13.sort_values("Predicted Overall Rating", ascending=False)
        
        # Picking the best team with in the actual budget, manager wants to spent in buying a team.
        self.team_name = self.playing_13[self.playing_13["Market_value"]<300000000].index[0]
        
        # Showing selected team standing.
        self.playing_13 = self.playing_13.style.apply(lambda x: ['background: lightblue' if i == self.team_name else '' for i,_ in x.iteritems()])
        self.display(self.playing_13)
        return self.playing_13, self.top_team
    
    # Getting my finalized team after some tweaks.
    def my_teamm(self):
        self.kj = self.taking_team()
        self.jk = self.kj[1]
        # Selecting all the players of selected team.
        self.my_team = self.jk[self.jk["Team"] == "Inter"]
        self.all_teams = self.jk[self.jk["Team"]!="Inter"]
        self.best = self.all_teams.sort_values(["Position", "Predicted Overall Rating"], ascending=False).groupby("Position").head(1)
        print("My team-'Inter' before swap.")
        self.display(self.my_team)
        return self.my_team, self.all_teams, self.best
    
    # Swapping players to make my team more.
    def swapping(self):
        self.al = self.my_teamm()
        self.mt = self.al[0]
        self.at = self.al[1]
        self.bt = self.al[2]
        self.pd.options.mode.chained_assignment = None
        # My actual amount after spending on buying a team.
        self.remaining = 190000000
        self.minimum = self.bt["Market_value"].min()
        self.ery = 1
        # We are allowed only 5 swaps as directed by the organizers.
        while(self.ery<6):
            self.dif = []
            for i in self.mt["Position"].values:
                self.a = self.bt["Predicted Overall Rating"][self.bt["Position"]==i].values[0]
                self.b = self.mt["Predicted Overall Rating"][self.mt["Position"]==i].values[0]
                self.diff = self.a - self.b
                self.dif.append(self.diff)
            self.indx = self.np.argmax(self.dif)
            self.posi = self.mt["Position"].values[self.indx]
            
            # Checking for remaining amount after each swaps.
            if(self.remaining > self.bt[self.bt["Position"] == str(self.posi)]["Market_value"].values[0]):
                print("True")
                self.tem = str(self.bt["Team"][self.bt["Position"] == str(self.posi)].values[0])
                self.id_ = self.bt["ID"][self.bt["Position"] == str(self.posi)].values[0]
                self.dfr = self.mt[self.mt["Position"] == str(self.posi)]
                self.dfr["Team"] = self.bt.loc[self.bt["Position"] == str(self.posi), ["Team"]].values[0]
                self.at.drop(self.at[self.at["ID"] == self.id_].index, inplace=True)
                self.at = self.at.append(self.dfr)   
                self.dt = self.mt[self.mt["Position"] == str(self.posi)]
                self.mt.drop(self.mt[self.mt["Position"] == str(self.posi)].index, inplace=True)
                self.mt = self.mt.append(self.bt[self.bt["Position"] == str(self.posi)], ignore_index=True)
                self.td = self.bt[self.bt["Position"] == str(self.posi)]
                
                # Showing what swaps are made at each iteration.
                print("(" + str(self.dt["ID"].values[0]) + ", " + str(self.dt["Name"].values[0]) + ", " + str(self.dt["Team"].values[0]) + ")" +
                     " <----> (" + str(self.td["ID"].values[0]) + ", " + str(self.td["Name"].values[0]) + ", " + str(self.td["Team"].values[0]) + ")")
                self.remaining = (self.remaining + self.dt["Market_value"].values[0]) - self.td["Market_value"].values[0]
                print("remaining =",self.remaining)
                self.ery+=1
            else:
                print("False")
                break
        return self.at, self.mt
    
    # My team after swapping.
    def after_swap(self):
        self.st = self.swapping()
        self.all_teams = self.st[0]
        self.my_tem = self.st[1]
        # Changing the swapped player's team name.
        self.my_tem.loc[self.my_tem["Team"] != "Inter", ["Team"]] = "Inter"
        self.all_teams = self.all_teams.append(self.my_tem)
        
        self.playing = self.all_teams.groupby("Team").agg({'Market_value':'sum', 'Predicted Overall Rating':'mean'})
        self.playing = self.playing.sort_values("Predicted Overall Rating", ascending=False)
        
        # Higlighting my team's standing.
        self.playingg = self.playing.style.apply(lambda x: ['background: lightblue' if i == "Inter" else '' for i,_ in x.iteritems()])
        print("My team-'Inter' after swap.")
        self.display(self.playingg)
        self.display(self.my_tem)
        return self.playing, self.my_tem, self.all_teams 
    
    