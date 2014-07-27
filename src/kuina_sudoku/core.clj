(set! *warn-on-reflection* true)
(ns kuina-sudoku.core)
(use 'clojure.set)

;;; トランプの型
(def suites {:spade "S", :heart "H", :diamond "D", :club "C"})
(def numbers {1 "A", 2 "2", 3 "3", 4 "4", 5 "5", 6 "6", 7 "7" 8 "8", 9 "9", 10 "T", 11 "J", 12 "Q", 13 "K"})
(defrecord Card [suite number])

;;; 初期盤面
(def initial-board [nil nil nil (->Card :diamond 4) (->Card :diamond 5) nil (->Card :club 1) (->Card :club 13) nil nil (->Card :heart 10) nil (->Card :club 3)
                    (->Card :club 6) nil nil (->Card :heart 2) nil (->Card :diamond 3) nil nil (->Card :spade 11) (->Card :club 9) nil (->Card :spade 1) (->Card :heart 8)
                    nil (->Card :heart 11) (->Card :heart 9) nil (->Card :club 7) nil (->Card :heart 3) (->Card :heart 5) nil (->Card :diamond 6) (->Card :club 12) (->Card :club 8) nil
                    (->Card :diamond 7) nil (->Card :spade 10) nil (->Card :spade 13) nil (->Card :diamond 12) nil (->Card :heart 6) nil (->Card :spade 2) nil (->Card :spade 5)])

;;; 全てのカード
(def all-cards (set (for [s (keys suites) n (keys numbers)]
                      (->Card s n))))

(defn card-string
  "カードを表す文字列を返す"
  [^Card card]
  (if card
    (str (suites (:suite card)) (numbers (:number card)))
    ".."))

(defn board-string
  "盤面を表す文字列を返す"
  [board]
  (->>
    board
    (map card-string)
    (reduce #(str %1 %2 " ") "")
    (partition (* 13 3))
    (map #(apply str %))
    (clojure.string/join "\n")))

(defn print-board
  "盤面をprintする"
  [board]
  (println (board-string board)))

(defn check
  "盤面の状態が正しいかチェックする"
  [board]
  ; 同じカードを使っていたらダメ
  ;  (print-board board)
  ;  (println "check1")
  (if (->>
        board
        (filter #(not (nil? %)))
        (group-by card-string)
        (filter #(> (count (second %)) 1))
        (not-empty))
    false
    (let [board_ (map-indexed (comp vec list) board)]
      (if (or
            ;            (do (println "check2") false)
            ; 横に同じ数字があってはならない
            (->>
              board_
              (map (fn [[x y]] [(quot x 13) y]))
              (filter #(not (nil? (second %))))
              (group-by (fn [[x y]] [x (:number y)]))
              (filter #(> (count (second %)) 1))
              (not-empty))
            ; 縦に同じマークがあってはならない
            ;            (do (println "check3") false)
            (->>
              board_
              (map (fn [[x y]] [(mod x 13) y]))
              (filter #(not (nil? (second %))))
              (group-by (fn [[x y]] [x (:suite y)]))
              (filter #(> (count (second %)) 1))
              (not-empty))
            ; 太枠内に同じ数字があってはならない
            ;            (do (println "check4") false)
            (->>
              board_
              (map (fn [[x y]] [(quot (mod x 13) 3) y]))
              (filter #(not (nil? (second %))))
              (group-by (fn [[x y]] [x (:number y)]))
              (filter #(> (count (second %)) 1))
              (not-empty)))
        false
        true))))

(defn finished?
  "ゲームが終わったかチェックする"
  [board]
  (= (count initial-board) (count (filter #(not (nil? %)) board))))

(defn available-cards
  "使用できるカード(未使用のカード)を返す"
  ([board]
    (difference
      all-cards
      (->>
        board
        (filter #(not (nil? %)))
        (set))))
  ([i board]
    (let [suites #{:spade :heart :diamond :club}
          numbers (set (range 1 (inc 13)))
          c (board i)
          suite (:suite c)
          number (:number c)]
      ; 横にある数字を排除
      (let [numbers_ (difference numbers
                       (set (->>
                              ((->>
                                 initial-board
                                 (map-indexed (comp vec list))
                                 (map (fn [[x y]] [(quot x 13) y]))
                                 (filter #(not (nil? (second %))))
                                 (group-by (fn [[x y]] x))) (quot i 13))
                              (map #(:number (second %))))))]
        ; 縦にあるマークを排除
        (let [suites_ (difference suites
                        (set (->>
                               ((->>
                                  initial-board
                                  (map-indexed (comp vec list))
                                  (map (fn [[x y]] [(mod x 13) y]))
                                  (filter #(not (nil? (second %))))
                                  (group-by (fn [[x y]] x))) (mod i 13))
                               (map #(:suite (second %))))))]
          ; 大枠内の数字を排除
          (let [numbers__ (difference (set numbers_)
                            (set (->>
                                   ((->>
                                      initial-board
                                      (map-indexed (comp vec list))
                                      (map (fn [[x y]] [(quot (mod x 13) 3) y]))
                                      (filter #(not (nil? (second %))))
                                      (group-by (fn [[x y]] x))) (quot (mod i 13) 3))
                                   (map #(:number (second %))))))]
            (difference (set (for [s suites_ n numbers__]
                               (->Card s n)))
              board)))))))

(defn process
  "総当たり"
  [board]
  (letfn [(process_ [i c board]
            (if (check board)
              (do
                (println "process_ = " i (card-string c))
                (let [new-board (assoc board i c)]
                  (print-board new-board)
                  (if (check new-board)
                    (if (finished? new-board)
                      (do
                        (println "Finished!!")
                        (print-board new-board)
                        (System/exit 0))
                      (let [i (first (first (filter #(nil? (second %)) (map-indexed list new-board))))
                            r (do
                                (println "callingC process__" i)
                                (process__ i new-board))]
                        (println "C" r)
                        r))
                    (do
                      (println "Check Error")
                      ;                      (System/exit 0)
                      false))))
              false))
          (process__ [i board]
            (if (nil? (board i))
              (do
                (println "process__ = " i (available-cards i board))
                (let [r (some #(do (println "callingB process_" i %) (process_ i % board)) (available-cards i board))]
                  (println "B" r)
                  r))
              false))]
    (let [i (first (first (filter #(nil? (second %)) (map-indexed list initial-board))))
          r (do
              (println "callingA process__" i)
              (process__ i board))]
      (println "A" r)
      r)))

(defn -main
  "Kuina-chan Sudoku"
  []
  (let [answer (process initial-board)]
    (println "answer = ")
    (print-board answer)
    (println (check answer))))
