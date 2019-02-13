module Graph where

import WhileGrammar

type Graph a = [(Location,(a -> a),Location)]

type Location = Integer