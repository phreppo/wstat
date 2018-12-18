import WhileGrammar

data Label = Ass Stmt | Beto BExpr | Empty

type Graph = [(Node,Node,Label)] 

type Node = Integer