## Print calendar of any year.
import calendar
def printcalendar(year):
    print(calendar.calendar(year))
year = int(input('Enter the year: '))
printcalendar(year)