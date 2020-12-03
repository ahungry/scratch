# Ensure parity among epoch->dt->epoch conversions

(def dt @{:year-day 0 :minutes 0 :month 0 :dst false :seconds 0
          :year 0 :month-day 0 :hours 0 :week-day 0})

# Start at 0001-01-01 epoch and work the way up to 1970-01-01
(for x -62135596800 0
  (let [dt (os/date x)]
    (def mktime (os/mktime dt))
    (def cmd (string/format "./ahudate '%04d-%02d-%02d %02d:%02d:%02d'"
                            (dt :year)
                            (+ 1 (dt :month))
                            (+ 1 (dt :month-day))
                            (+ 0 (dt :hours))
                            (+ 0 (dt :minutes))
                            (+ 0 (dt :seconds))
                            ))
    (def cmd2 (string (string/format "date -d '%04d-%02d-%02d %02d:%02d:%02d+0'"
                                     (dt :year)
                                     (+ 1 (dt :month))
                                     (+ 1 (dt :month-day))
                                     (+ 0 (dt :hours))
                                     (+ 0 (dt :minutes))
                                     (+ 0 (dt :seconds))
                                     )
                      " '+%s'"))
    (def ahudate (:read (file/popen cmd) :all))
    (def date (:read (file/popen cmd2) :all))
    (pp cmd)
    # (pp cmd)
    # (pp cmd2)
    # (pp x)
    # (pp mktime)
    # (pp ahudate)
    # (pp date)
    (assert (deep= ahudate date))))
