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

(defn get-card-group
  "横・縦・大枠内にあるカードをgroup-funcで分類する"
  [index-func group-func board]
  (->>
    board
    (map-indexed (comp vec list))
    (map (fn [[x y]] [(index-func x y) y]))
    (filter (fn [x] (not (nil? (second x)))))
    (group-by group-func)))

(defn get-rows-index
  "横方向のインデックス"
  ([x]
    (get-rows-index x 0))
  ([x y]
    (quot x 13)))

(defn get-columns-index
  "縦方向のインデックス"
  ([x]
    (get-columns-index x 0))
  ([x y]
    (mod x 13)))

(defn get-boxes-index
  "大枠内のインデックス"
  ([x]
    (get-boxes-index x 0))
  ([x y]
    (quot (mod x 13) 3)))

(defn check
  "盤面の状態が正しいかチェックする"
  [board]
  (letfn [(check_ [col]
            (->>
              col
              (filter #(> (count (second %)) 1))
              (not-empty)))]
    ; 同じカードを使っていたらダメ
    (if (->>
          board
          (filter #(not (nil? %)))
          (group-by card-string)
          (check_))
      false
      (if (or
            ; 横に同じ数字があってはならない
            (->>
              board
              (get-card-group get-rows-index (fn [[x y]] [x (:number y)]))
              (check_))
            ; 縦に同じマークがあってはならない
            (->>
              board
              (get-card-group get-columns-index (fn [[x y]] [x (:suite y)]))
              (check_))
            ; 太枠内に同じ数字があってはならない
            (->>
              board
              (get-card-group get-boxes-index (fn [[x y]] [x (:number y)]))
              (check_)))
        false
        true))))

(defn finished?
  "ゲームが終わったかチェックする"
  [board]
  (= (count initial-board) (count (filter #(not (nil? %)) board))))

(defn available-cards
  "使用できるカード(未使用のカード)を返す"
  [i board]
  (let [suites #{:spade :heart :diamond :club}
        numbers (set (range 1 (inc 13)))
        c (board i)
        suite (:suite c)
        number (:number c)]
    ; 横にある数字を排除
    (let [numbers_ (difference numbers
                     (set
                       (->>
                         ((get-card-group get-rows-index (fn [[x y]] x) board) get-rows-index)
                         (map #(:number (second %))))))]
      ; 縦にあるマークを排除
      (let [suites_ (difference suites
                      (set
                        (->>
                          ((get-card-group get-columns-index (fn [[x y]] x) board) get-columns-index)
                          (map #(:suite (second %))))))]
        ; 大枠内の数字を排除
        (let [numbers__ (difference (set numbers_)
                          (set
                            (->>
                              ((get-card-group get-boxes-index (fn [[x y]] x) board) get-boxes-index)
                              (map #(:number (second %))))))]
          ; 結果から盤面上のカードを除く
          (difference (set (for [s suites_ n numbers__]
                             (->Card s n)))
            board))))))

(defn process
  "総当たり"
  [board]
  (letfn
    ; ある場所にあるカードを置いて、置けたら次の場所に置く
    [(process_ [i c board]
       (if (check board)
         (do
;           (println "process_ = " i (card-string c))
           (let [new-board (assoc board i c)]
;             (print-board new-board)
             (if (check new-board)
               (if (finished? new-board)
                 ; クリア！！
                 (do
                   (println "Finished!!")
                   (print-board new-board)
;                   (System/exit 0)
                   false
                   )
                 ; 次に何も置かれてない場所から
                 (let [i (first (first (filter #(nil? (second %)) (map-indexed list new-board))))
                       r (do
;                           (println "callingC process__" i)
                           (process__ i new-board))]
;                   (println "C" r)
                   r))
               (do
;                 (println "Check Error")
                 false))))
         false))
     ; ある場所にすべてのカードを置いてみる
     (process__ [i board]
       (if (nil? (board i))
         (do
;           (println "process__ = " i (available-cards i board))
           (let [r (some #(do
;                            (println "callingB process_" i %)
                            (process_ i % board))
                     (available-cards i board))]
;             (println "B" r)
             r))
         false))]
    ; 最初に何も置かれてない場所から始める
    (let [i (first (first (filter #(nil? (second %)) (map-indexed list initial-board))))
          r (do
;              (println "callingA process__" i)
              (process__ i board))]
;      (println "A" r)
      r)))

(defn -main
  "Kuina-chan Sudoku"
  []
  (let [answer (process initial-board)]
;    (println "answer = ")
;    (print-board answer)
;    (println (check answer))
    ))
