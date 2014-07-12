(declare main-loop get-e display empty-board replace-n get-move update-board computer-move win? main win-helper check-win)

(defn main-loop [board]
    (do 
      (display board)
      (let [move (get-move) 
            board (update-board board move)
            win (win? board)]
        (cond (= 1 win) (do (println "You won!") (main))
              (= 2 win) (do (println "You lose :(") (main))
              :else  (main-loop (computer-move board))))))

(defn replace-n [matrix x y r]
  (cond (zero? y) 
        (let [row (first matrix)
             new-row (concat (take x row) (list r) (nthnext row (inc x)))]
             (cons new-row (rest matrix)))
        :else (cons (first matrix) (replace-n (rest matrix) x (dec y) r))))

(defn update-board [board move]
  (replace-n board (first move) (nth move 1) (nth move 2)))

(defn get-move []
  (let [input (read-line) move (map read-string (re-seq #"[\d.]+" input))]
    (concat move '(1))))

(def win-checks '(((0 0)(0 1)(0 2))
                  ((1 0)(1 1)(1 2))
                  ((2 0)(2 1)(2 2))
                  ((0 0)(1 0)(2 0))
                  ((0 1)(1 1)(2 1))
                  ((0 2)(1 2)(2 2))
                  ((0 0)(1 1)(2 2))
                  ((0 2)(1 1)(2 0))))

(defn win? [board]
  (win-helper board (dec (count win-checks))))

(defn win-helper [board n]
  (if (= n 0)
    0
    (let [win (check-win board (nth win-checks n))]
      (cond (= win 0) (win-helper board (dec n))
            (= win 1) 1
            (= win 2) 2))))

(defn third [ls] (nth ls 2))

(defn check-win [board locs]
  (if (= (get-e board (first (first locs)) (second (first locs)))
         (get-e board (first (second locs)) (second (second locs)))
         (get-e board (first (third locs)) (second (third locs))))
    (get-e board (first (first locs)) (second (first locs)))
    0))

(defn computer-move [board]
  (let [x (rand-int 3) y (rand-int 3)]
    (if (zero? (get-e board x y))
      (replace-n board x y 2)
      (computer-move board))))

(defn get-e [matrix x y]
  (nth (nth matrix y) x))

(defn display [board]
  (println board))

(defn empty-board [] '((0 0 0)(0 0 0)(0 0 0)))

(defn main [& args]
  (main-loop (empty-board)))

