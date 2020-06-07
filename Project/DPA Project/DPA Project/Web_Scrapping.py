class Web_Scrapping():
    # Creating dictionary to save fetched data in key-value format.
    crawled_data = {}
    def scrap(self):
        #crawled_data['table' + str(k)] = []
        # Iterating through all the available pages of the table.
        for h in range(0, 100000, 60):
            # Iterating through all the rows for the first page.
            if(h == 0):
                Dependency.clear_output(wait=True)
                print(h)
                url = "https://sofifa.com/?r=090002&set=true&showCol%5B%5D=pi&showCol%5B%5D=ae&showCol%5B%5D=oa&showCol%5B%5D=pt&showCol%5B%5D=bo&showCol%5B%5D=bp&showCol%5B%5D=vl&showCol%5B%5D=wg&showCol%5B%5D=ta&showCol%5B%5D=ts&showCol%5B%5D=to&showCol%5B%5D=tp&showCol%5B%5D=td&showCol%5B%5D=tg"
                r = Dependency.requests.get(url)
                soup = Dependency.bs(r.content, "html5lib")
                # Getting all the table body present in the html page.
                for k, body in enumerate(soup.findAll('tbody')):
                    # Getting all the table present in the table body.
                    self.crawled_data['table' + str(k)] = []
                    # Fetching all the rows present in the table.
                    for tr in body.find_all('tr'):
                        tmp = tuple()
                        # Scanning throgh all the available columns in the table.
                        th = tr.find('th')
                        if th:
                            th = tr.find('th').text.strip()
                            tmp += (th,)
                            # Getting all the row values for each column.
                        for td in tr.find_all('td'):
                            tmp += (td.text.strip(),)
                        self.crawled_data['table' + str(k)].append(tmp)
            else:
                # Iterating through all the rows for the remaining pages.
                Dependency.clear_output(wait=True)
                print(h)
                url = "https://sofifa.com/?r=090002&set=true&showCol%5B0%5D=pi&showCol%5B1%5D=ae&showCol%5B2%5D=oa&showCol%5B3%5D=pt&showCol%5B4%5D=bo&showCol%5B5%5D=bp&showCol%5B6%5D=vl&showCol%5B7%5D=wg&showCol%5B8%5D=ta&showCol%5B9%5D=ts&showCol%5B10%5D=to&showCol%5B11%5D=tp&showCol%5B12%5D=td&showCol%5B13%5D=tg&offset="+str(h)
                r = Dependency.requests.get(url)
                soup = Dependency.bs(r.content, "html5lib")
                for k, body in enumerate(soup.findAll('tbody')):
                    #dados['table' + str(k)] = []
                    for tr in body.find_all('tr'):
                        tmp = tuple()
                        th = tr.find('th')
                        if th:
                            th = tr.find('th').text.strip()
                            tmp += (th,)
                        for td in tr.find_all('td'):
                            tmp += (td.text.strip(),)
                        self.crawled_data['table' + str(k)].append(tmp)
    
    # Storing each column data in specific array.                  
    def after_scrap(self):
        self._id = []
        self.name = []
        self.team = []
        self.age = []
        self.overall_rating = []
        self.best_overall = []
        self.position = []
        self.market_value = []
        self.wage = []
        self.attacking_stats = []
        self.movement_stats = []
        self.power_stats = []
        self.defence_stats = []
        self.goalkeeper_stats = []
        self.skillSets = []
        self.total_potential = []

        # Iterating to crawled data dictionary to store separate columns in separate array.
        for key, value in self.crawled_data.items():
            for i in range(0,len(value)):
                # fetching id
                _id_sd = int(value[i][6])
                self._id.append(_id_sd)

                # fetching name
                name_sd = str(value[i][1])
                name_fg = str(name_sd.splitlines()[0])
                self.name.append(name_fg)

                # fetching team
                team_sd = str(value[i][5])
                team_fg = str(team_sd.splitlines()[0])
                self.team.append(team_fg)

                # fetching age
                self.age.append(int(value[i][2]))

                # fetching overall rating
                self.overall_rating.append(int(value[i][3]))

                # fetching best_overall
                self.best_overall.append(int(value[i][7]))

                # fetching position
                self.position.append(str(value[i][8]))

                # fetching market_value
                self.market_value.append(str(value[i][9]))

                # fetching wage
                self.wage.append(str(value[i][10]))

                # fetching attacking_stats
                self.attacking_stats.append(int(value[i][11]))

                # fetching movement_stats
                self.movement_stats.append(value[i][13])

                # fetching power_stats
                self.power_stats.append(int(value[i][14]))

                # fetching defencs_stats
                self.defence_stats.append(int(value[i][15]))

                # fetching goalKeeper_stats
                self.goalkeeper_stats.append(int(value[i][16]))

                # fetching skill_sets
                self.skillSets.append(int(value[i][12]))

                # fetching total_potential
                self.total_potential.append(int(value[i][4]))
