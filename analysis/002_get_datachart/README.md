## 002_summary_data_points

For each contaminant, summary de data points according to the following critearia:

1.- Average day if more than 18 hours (75%).
2.- Average week if more than 5.25 days (75%).
3.- Average month if more than 22 days (75%).
4.- Average trimester if more than 67.5 days (75%).
5.- Average semester if more than 135 days (75%).
6.- Average year if more than 273.75 days (75%).

Actually, we are only going to get week basis data. After, kriging-week data will be summarized!!!

Week data will be considered by dividing days by 7. It will start from year-01-01 and labelling it according to the consecutive week number.

## Example data output
            date station_code pollutant unit    value week year
1     2009-01-01          LAG        CO  ppm 2.017121    1 2009
1869  2009-01-07          LAG        CO  ppm 2.039000    2 2009
