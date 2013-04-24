(ns tictac-lant.core
  (:import [com.googlecode.lanterna TerminalFacade]
           [com.googlecode.lanterna.screen Screen ScreenWriter ScreenCharacterStyle]))

(defprotocol Device
  "This defines an abstract device that can display a game board and get input"
  (display-board [device board] "displays the game board")
  (get-input [device] "gets the user's next move"))

(defn get-num-key
  "returns when a number key is pressed with the value of the key"
  [screen]
  (Thread/sleep 10)
  (let [k (.readInput screen)]
    (if-not k
      (recur screen)
      (let [c (.getCharacter k)
            v (- (int c) (int \0))]
        (if (and (>= v 0) (<= v 9))
          v
          (recur screen))))))

(defn draw-string
  "draws a string using screen writer, hiding the ugly varargs crap"
  [sw x y s]
  (.drawString sw x y s (into-array ScreenCharacterStyle [])))

(defn board-row-to-string
  [board row]
  (let [idx (* 3 row)
        board-part (take 3 (drop idx board))
        board-chars (map {:X \X :O \O :SPC \space} board-part)]
    (apply str (cons \| (interleave board-chars (repeat \|))))))

(deftype TerminalDevice
    [^Screen screen]
  Device
  (display-board [device board]
     (let [screen (.screen device)
           sw (ScreenWriter. screen)]
       (draw-string sw 2 2 "-------")
       (draw-string sw 2 3 (board-row-to-string board 0))
       (draw-string sw 2 4 "-------")
       (draw-string sw 2 5 (board-row-to-string board 1))
       (draw-string sw 2 6 "-------")
       (draw-string sw 2 7 (board-row-to-string board 2))
       (draw-string sw 2 8 "-------")
       (.refresh screen)))

  (get-input [device] (get-num-key (.screen device))))

(defn new-terminal-device
  "creates a new terminal device"
  []
  (let [screen (TerminalFacade/createScreen)]
       (.startScreen screen)
       (->TerminalDevice screen)))

(deftype DebugDevice
    [moves-atom]
  Device
  (display-board [_ board] (clojure.pprint/pprint board))
  (get-input [device]
    (let [moves-atom (.moves-atom device)
          move (first @moves-atom)]
      (swap! moves-atom rest)
      move)))

(defprotocol Player
  "This defines an abstract player"
  (get-next-move [player board ^tictac_lant.core.Device device] "gets the next move from the player"))

(deftype HumanPlayer
    []
  Player
  (get-next-move [player board device]
    (display-board device board)
    (get-input device)))

(deftype AIPlayer
    []
  Player
  (get-next-move [player board device]
    1))

(defrecord Game
    [board
     ^tictac_lant.core.Device device
     ^tictac_lant.core.Player playerX
     ^tictac_lant.core.Player playerO])

(defn new-board
  "creates a new, empty board"
  []
  (vec (repeat 9 :SPC)))

(defn init-game
  "initialises a new game"
  ([]
     (init-game (new-terminal-device) (->HumanPlayer) (->AIPlayer)))
  ([device playerX playerO]
     (map->Game {:board (new-board) :device device :playerX playerX :playerO playerO})))

(defn get-available-moves
  "gets the available moves on the board"
  [board]
  (filter identity
          (map-indexed (fn [i val] (when (= val :SPC) i)) board)))

(defn make-move
  "applies a move to the board"
  [board move token]
  (if (= :SPC (board move))
    (assoc-in board [move] token)
    (throw (IllegalArgumentException. "move already taken"))))

(def winners
  "the winning combinations of locations"
  (concat
   (map (fn [i] (range i (+ i 7) 3)) (range 3))
   (map (fn [i] (range i (+ i 3))) (range 0 7 3))
   [[0 4 8] [2 4 6]]))

(defn get-winner-token
  "returns the winning token if the winner index seq is a won game on the board"
  [board winner-combo]
  (let [vals (map (fn [i] (board i)) winner-combo)]
    (when (apply = vals)
      (when-not (= :SPC (first vals)) (first vals)))))

(defn get-all-winning-tokens
  "returns any and all winning tokens from any winning combo"
  [board]
  (filter identity (map (partial get-winner-token board) winners)))

(defn is-game-over?
  "looks at the board and returns true if it is won or has no more moves"
  [board]
  (or (not-empty (get-all-winning-tokens board))
      (empty? (get-available-moves board))))

(defn eval-board
  "evaluates the board from the give token perspective"
  [board token]
  (let [winning-tokens (get-all-winning-tokens board)
        total-wins (count winning-tokens)
        num-good-wins (count (filter #{token} winning-tokens))
        num-bad-wins (- total-wins num-good-wins)
        total-tokens (count (filter #{:X :O} board))
        num-good-tokens (count (filter #{token} board))
        num-bad-tokens (- total-tokens num-good-tokens)]
    (+ (* 100 (- num-good-wins num-bad-wins)) (- num-good-tokens num-bad-tokens))))
