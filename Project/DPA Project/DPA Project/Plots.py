from Optimization import Optimization
class Plots(Optimization):
    # Importing relevant packages.
    import matplotlib.pyplot as plt 
    def __init__(self):
        # Inheriting Optimization class.
        super().__init__()
        # Initializing with inherited data.
        self.aft_swap = super().after_swap()
        self.playing = self.aft_swap[0]
        self.my_team = self.aft_swap[1]
        self.all_team = self.aft_swap[2]
    
    def team_plot(self):
        # Plotting each playing team's Total Market value.
        self.ax_1 = self.playing["Market_value"].plot(kind = 'bar', title = "Team's Market value")
        self.ax_1.set(ylabel = "Total Market value", xlabel = "Team")
        self.ax_1.patches[self.playing.index.get_loc("Inter")].set_facecolor('orange')
        self.plt.show()
        # Plotting each playing team's predicted overall rating.
        self.ax_2 = self.playing["Predicted Overall Rating"].plot(kind = 'bar', title = "Team's Predicted Overall Rating")
        self.ax_2.set(ylabel = "Avg. Predicted Overall Rating", xlabel = "Team")
        self.ax_2.patches[self.playing.index.get_loc("Inter")].set_facecolor('orange')
        self.plt.show()
    
    # Various plots showing each category of player's predicted overall rating of all playing teams.
    def position_plot(self):
        self.team_plot()
        self.dict = {'GK':'Goal-keeper', 'ST':'Striker', 'RW':'Right winger', 'LWB':'Left Wing Back', 'RWB':'Right Wing Back',
                    'LB':'Left back', 'CF':'Center forward', 'CDM':'Central defensive midfielder', 'CB':'Centre back',
                    'CAM':'Central attacking midfielder', 'RM':'Right midfielder', 'CM':'Centre midfielder', 'LW':'Left winger',
                    'LM':'Left midfielder', 'RB':'Right back'}
        for i in self.my_team["Position"]:
            self.pos = self.all_team[self.all_team["Position"] == i].sort_values("Predicted Overall Rating", ascending=False)
            self.pos_group = self.pos.groupby("Team").first().sort_values("Predicted Overall Rating", ascending=False)
            self.ax = self.pos_group["Predicted Overall Rating"].plot(kind = 'bar', title = "For " + self.dict[i])
            self.ax.set(ylabel = "Predicted Overall Rating", xlabel = "Team")
            self.ax.patches[self.pos_group.index.get_loc("Inter")].set_facecolor('orange')
            self.plt.show()
    
    # Showing performance of my team's player throughout their career.
    def stats_plot(self):
        self.position_plot()
        self.color = ["red", "blue", "green", "orange", "brown", "black", "red", "magenta", "blue", "orange", "black"]
        for i in range(0, len(self.my_team)):
            self.ax = self.train_data[self.train_data["ID"] == self.my_team["ID"][i]].reset_index().plot(x='Year',
                                                                        y='Overall Rating',
                                                                        marker='o',
                                                                        title = self.my_team["Name"][i], color = self.color[i])
            self.ax.locator_params(integer=True)
            self.ax.set_ylabel("Overall Rating")
            self.plt.show()