# Storing the each year data in different csv file.
class Data_Process():
    #asd = Web_scrapping()
    def pre_process(self):
        dicti = {'ID':asd._id, 'Name':asd.name, "Team":asd.team, "Age":asd.age, "Overall Rating":asd.overall_rating,
                 'Best Overall':asd.best_overall, 'Position':asd.position, 'Market value':asd.market_value, 'Wage':asd.wage,
                 'Attacking stats':asd.attacking_stats,'Movement stats':asd.movement_stats, 'Power stats':asd.power_stats,
                 'Defence stats':asd.defence_stats,'Goal-Keeper stats':asd.goalkeeper_stats, 'Skill sets':asd.skillSets,
                 'Total potential':asd.total_potential}
        df = pd.DataFrame(dicti)
        df.to_csv("2009.csv")