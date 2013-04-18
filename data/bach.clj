(ns data.bach)

(def bach1
  [{:time 0 :pitch 57} {:time 0 :pitch 60} {:time 0 :pitch 69} {:time 0 :pitch 76}
   {:time 1000 :pitch 59} {:time 1000 :pitch 62} {:time 1000 :pitch 67} {:time 1000 :pitch 79}
   {:time 2000 :pitch 60} {:time 2000 :pitch 64} {:time 2000 :pitch 76}
   {:time 2500 :pitch 59} {:time 2500 :pitch 62} {:time 2500 :pitch 68} {:time 2500 :pitch 76}
   {:time 3000 :pitch 57} {:time 3000 :pitch 60} {:time 3000 :pitch 69} {:time 3000 :pitch 76}
   {:time 3500 :pitch 56} {:time 3500 :pitch 59} {:time 3500 :pitch 71} {:time 3500 :pitch 76}
   {:time 4000 :pitch 57} {:time 4000 :pitch 60} {:time 4000 :pitch 69} {:time 4000 :pitch 77}
   {:time 4500 :pitch 59} {:time 4500 :pitch 62} {:time 4500 :pitch 67}
   {:time 5000 :pitch 60} {:time 5000 :pitch 76} {:time 5500 :pitch 57} {:time 5500 :pitch 60}
   {:time 5500 :pitch 69} {:time 6000 :pitch 53} {:time 6000 :pitch 60} {:time 6000 :pitch 69} {:time 6000 :pitch 74}
   {:time 6500 :pitch 50} {:time 7000 :pitch 55} {:time 7000 :pitch 59} {:time 7000 :pitch 67} {:time 7000 :pitch 74}
   {:time 8000 :pitch 48} {:time 8000 :pitch 64} {:time 8000 :pitch 67} {:time 8000 :pitch 72} {:time 10000 :pitch 60}
   {:time 10000 :pitch 60} {:time 10000 :pitch 67} {:time 10000 :pitch 76} {:time 11000 :pitch 48}
   {:time 12000 :pitch 55} {:time 12000 :pitch 59} {:time 12000 :pitch 67} {:time 12000 :pitch 74}
   {:time 12500 :pitch 59} {:time 12500 :pitch 62} {:time 12500 :pitch 67} {:time 12500 :pitch 74} {:time 13000 :pitch 62}
   {:time 13000 :pitch 57} {:time 13000 :pitch 65} {:time 13000 :pitch 74} {:time 13500 :pitch 53} {:time 13500 :pitch 62}
   {:time 13500 :pitch 69} {:time 13500 :pitch 69} {:time 14000 :pitch 52} {:time 14000 :pitch 64} {:time 14000 :pitch 69}
   {:time 14000 :pitch 72} {:time 14500 :pitch 50} {:time 15000 :pitch 52} {:time 15000 :pitch 68} {:time 15000 :pitch 71}
   {:time 15500 :pitch 62} {:time 16000 :pitch 45} {:time 16000 :pitch 60} {:time 16000 :pitch 64} {:time 16000 :pitch 69}
   {:time 18000 :pitch 57} {:time 18000 :pitch 60} {:time 18000 :pitch 69} {:time 18000 :pitch 76}
   {:time 19000 :pitch 55} {:time 19000 :pitch 62} {:time 19000 :pitch 71} {:time 19000 :pitch 79} {:time 20000 :pitch 60}
   {:time 20000 :pitch 64} {:time 20000 :pitch 67} {:time 20000 :pitch 76} {:time 20500 :pitch 62} {:time 20500 :pitch 62}
   {:time 20500 :pitch 76} {:time 21000 :pitch 60} {:time 21000 :pitch 64} {:time 21000 :pitch 76} {:time 21500 :pitch 59}
   {:time 21500 :pitch 68} {:time 21500 :pitch 76} {:time 22000 :pitch 64} {:time 22000 :pitch 64} {:time 22000 :pitch 69}
   {:time 22000 :pitch 77} {:time 22500 :pitch 62} {:time 22500 :pitch 62} {:time 22500 :pitch 71} {:time 23000 :pitch 60}
   {:time 23000 :pitch 55} {:time 23000 :pitch 72} {:time 23000 :pitch 76} {:time 23500 :pitch 52} {:time 23500 :pitch 67}
   {:time 24000 :pitch 53} {:time 24000 :pitch 69} {:time 24000 :pitch 74} {:time 24500 :pitch 50} {:time 24500 :pitch 72}
   {:time 25000 :pitch 55} {:time 25000 :pitch 67} {:time 25000 :pitch 72} {:time 25000 :pitch 74} {:time 25500 :pitch 62}
   {:time 25500 :pitch 71} {:time 26000 :pitch 48} {:time 26000 :pitch 64} {:time 26000 :pitch 67} {:time 26000 :pitch 72}
   {:time 28000 :pitch 48} {:time 28000 :pitch 76} {:time 28500 :pitch 50} {:time 29000 :pitch 52} {:time 29000 :pitch 55}
   {:time 29000 :pitch 79} {:time 29500 :pitch 53} {:time 29500 :pitch 57} {:time 30000 :pitch 55} {:time 30000 :pitch 59}
   {:time 30000 :pitch 67} {:time 30000 :pitch 74} {:time 30500 :pitch 60} {:time 30500 :pitch 69} {:time 30500 :pitch 74}
   {:time 31000 :pitch 62} {:time 31000 :pitch 71} {:time 31000 :pitch 74} {:time 31500 :pitch 57} {:time 31500 :pitch 72}
   {:time 31500 :pitch 74} {:time 32000 :pitch 59} {:time 32000 :pitch 74} {:time 32000 :pitch 74} {:time 32500 :pitch 60}
   {:time 32500 :pitch 64} {:time 33000 :pitch 62} {:time 33000 :pitch 66} {:time 33000 :pitch 62} {:time 33000 :pitch 69}
   {:time 33500 :pitch 68} {:time 33500 :pitch 71} {:time 34000 :pitch 48} {:time 34000 :pitch 69} {:time 34000 :pitch 64}
   {:time 34000 :pitch 72} {:time 34500 :pitch 50} {:time 35000 :pitch 52} {:time 35000 :pitch 67} {:time 35000 :pitch 64}
   {:time 35000 :pitch 71} {:time 35500 :pitch 53} {:time 35500 :pitch 65} {:time 35500 :pitch 62} {:time 36000 :pitch 55}
   {:time 36000 :pitch 64} {:time 36000 :pitch 61} {:time 36000 :pitch 69} {:time 36500 :pitch 61} {:time 36500 :pitch 64}
   {:time 37000 :pitch 53} {:time 37000 :pitch 62} {:time 37000 :pitch 69} {:time 37500 :pitch 52} {:time 37500 :pitch 64}
   {:time 37500 :pitch 67} {:time 38000 :pitch 50} {:time 38000 :pitch 65} {:time 38500 :pitch 52} {:time 38500 :pitch 57}
   {:time 39000 :pitch 53} {:time 39000 :pitch 62} {:time 39500 :pitch 55} {:time 40000 :pitch 57} {:time 41000 :pitch 61}
   {:time 41000 :pitch 64} {:time 41500 :pitch 59} {:time 41500 :pitch 62} {:time 42000 :pitch 45} {:time 42000 :pitch 61}
   {:time 42000 :pitch 64} {:time 42000 :pitch 69}])