# Print out the epoch for every year from year 1 to 9999
# And do this 1000 times to account for startup slowdown


# (for year 1 9999
#   (print (os/mktime {:year-day 0 :minutes 0 :month 0 :dst false :seconds 0
#                      :year (+ year 1):month-day 0 :hours 0 :week-day 0})))

# About 4 seconds without print, 17 seconds with print
(def dt @{:year-day 0 :minutes 0 :month 0 :dst false :seconds 0
          :year 0 :month-day 0 :hours 0 :week-day 0})

# (for year 1 9999
#   (put dt :year year)
#   (print (os/mktime dt)))

# With mutable struct, 1.7 without print, with print: 13.369
(for x 1 100
  (for year 1 9999
    (put dt :year year)
    (print (os/mktime dt))))
