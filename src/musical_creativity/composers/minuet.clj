(ns musical-creativity.composers.minuet
  (:require
   [musical-creativity.events :as events]))

(def minuet-table
  [[96  22  141 41  105  122 11  30  70  121 26  9   112 49  109 14 ]
   [32  6   128 63  146  46  134 81  117 39  126 56  174 18  116 83 ]
   [69  95  158 13  153  55  110 24  66  139 15  132 73  58  145 79 ]
   [40  17  113 85  161  2   159 100 90  176 7   34  67  160 52  170]
   [148 74  163 45  80   97  36  107 25  143 64  125 76  136 1   93 ]
   [104 157 27  167 154  68  118 91  138 71  150 29  101 162 23  151]
   [152 60  171 53  99   133 21  127 16  155 57  175 43  168 89  172]
   [119 84  114 50  140  86  169 94  120 88  48  166 51  115 72  111]
   [98  142 42  156 75   129 62  123 65  77  19  82  137 38  149 8  ]
   [3   87  165 61  135  47  147 33  102 4   31  164 144 59  173 78 ]
   [54  130 10  103 28   37  106 5   35  20  108 92  12  124 44  131]])

(defn minuet-pitch [measure number]
  (nth (nth minuet-table number) measure))

(defn compose-minuet []
  (map #(minuet-pitch % (rand-int 11)) (range 0 15)))

(defn compose []
  (events/make (compose-minuet)))
