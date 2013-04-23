(ns tictac-lant.core
  (:import [com.googlecode.lanterna TerminalFacade]
           [com.googlecode.lanterna.screen Screen]))

(defprotocol Device
  "This defines an abstract device that can display a game board and get input"
  (display-board [device board] "displays the game board")
  (get-input [device] "gets the user's next move"))

(deftype TerminalDevice
    [^Screen screen]
  Device
  (display-board [device board] nil)
  (get-input [device] nil))

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
     ^tictac_lant.core.Player player1
     ^tictac_lant.core.Player player2])

(defn new-board
  "creates a new, empty board"
  []
  (vec (repeat 9 :SPC)))

(defn init-game
  "initialises a new game"
  ([]
     (let [screen (TerminalFacade/createScreen)]
       (.startScreen screen)
       (init-game (->TerminalDevice screen) (->HumanPlayer) (->AIPlayer))))
  ([device player1 player2]
     (map->Game {:board (new-board) :device device :player1 player1 :player2 player2})))

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
