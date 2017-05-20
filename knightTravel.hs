module Knight where

-- Data Types used in program
-- Declaración de los tipos usados
-- para el contenido del Nodo
type File   = [Bool]
type Board  = [File]
type Square = (Int,Int)
type Path   = [Square]

-- Nodo del árbol de búsqueda de soluciones:
  -- Tamaño del tablero, Longitud camino construido,
  -- Estado tablero, Casilla actual caballo, Camino construido
type Node = (Int,Int,Board,Square,Path)

------------------------
-- Main Functions
------------------------
-- Comprueba si un nodo contiene, o no, un camino completo 
-- (el que recorre todo el tablero)
-- node     Nodo a comprobar su longitud
-- Devuelve True si ha recorrido todo el tablero 
fullPath :: Node -> Bool
fullPath (size,len,_,_,_) = len == (size*size)

-- Recibe un nodo y devuelve lista de compleciones de dicho nodo, 
-- nodos en los que se visita una nueva casilla válida 
-- node     Nodo a revisar sus saltos
-- Devuelve la lista de nodos [Node] no visitados y
  -- dentro del tablero
validJumps :: Node -> [Node]
validJumps (size,len,board,(c,r),path) =
  return (jumps (size,len,board,(c,r),path)) -- listado de saltos
    where       
      -- añade las casillas válidas instanciando cada nodo
      return []     = [] -- si no existen saltos
      return (x:xs) = 
        [(size,len+1,markSquare board x,x,addSquare path x)] ++ return xs

-- Función Principal 
-- size        tamaño del tablero 
-- initSquare  casilla desde la que el caballo inicia recorrido y
-- Devuelve el camino que recorre todo el tablero comenzando
   -- por la casilla indicada. 
   -- Si no solución, camino devuelto igual a vacío.
knightTravel :: Int -> Square -> Path
knightTravel size initSquare 
    -- nodo inicial no se encuentra dentro del tablero
  | not (inBoard size initSquare) = [] 
  | otherwise                     = getFirstPath nodeList
  where
    --nodo inicial
    node = (size, 1, markSquare (initBoard size) initSquare,
            initSquare, addSquare [] initSquare)
    --lista de nodos con soluciones
    nodeList = bt (fullPath) (validJumps) node

-- Backtracking scheme
-- isSol  función que comprueba si un nodo es, o no, solución. [fullPath]
-- comp   función que, dado un nodo, devuelve sus compleciones. [validJumps]
-- node   nodo a partir del cual explorar
-- Devuelve lista de TODOS los nodos que contengan soluciones al problema
bt :: (a -> Bool) -> (a -> [a]) -> a -> [a]
bt isSol comp node
  | isSol node = [node]
  | otherwise  = concat (map (bt isSol comp) (comp node))

-------------------------
-- Auxiliary Functions
-------------------------
-- Estado inicial del tablero 
-- size    tamaño del tablero 
-- Devuelve tablero con todas casillas a True
initBoard :: Int -> Board
initBoard size = replicate size (replicate size True)

-- Marca una casilla como visitada
-- board tablero a marcar
-- (c,r) casilla a marcar
-- Devuelve el tablero con la casilla marcada
markSquare :: Board -> Square -> Board
markSquare board (c,r) = 
    take (r-1) board ++ 
    [( take (c-1) (board !! (r-1)) ++ [False] ++ drop (c) (board !! (r-1)) )]
    ++ drop (r) board 

-- Revisa si la casilla está dentro del tablero
-- size  tamaño del tablero
-- (c,r) casilla a revisar
-- Devuelve True si se encuentra en el tablero
inBoard :: Int -> Square -> Bool
inBoard size (c,r)
  |  (c > size) || (r > size) = False
  |  (c < 1)    || (r < 1)    = False
  |  otherwise                = True

-- Añade una casilla, al final del camino creado
-- hasta ahora
-- path  camino almacenado
-- sq    casilla a añadir
-- Devuelve el camino actualizado
addSquare :: Path -> Square -> Path
addSquare path sq = path ++ [sq]

-- Comprueba si el salto, para la casilla, puede realizarse,
-- revisando que se encuentra dentro del tablero,
-- y no se encuentra visitada (false)
-- size  tamaño del tablero
-- board tablero a revisar
-- (c,r) casilla a revisar
-- Devuelve el camino actualizado
validJump :: Int ->  Board -> Square -> Path
validJump size board (c,r) 
  | inBoard size (c,r) && (board !! (r-1)) !! (c-1) = [(c,r)]
  | otherwise                                       = []

-- Revisa un nodo y todos sus saltos válidos,
-- haciendo el cálculo con cada uno de los posibles 
-- saltos del caballo
-- node  nodo a revisar
-- Devuelve un camino de todas las casillas válidas
jumps :: Node -> Path
jumps (size,len,board,(c,r),path) = 
  concat (map (validJump size board) [(x1,y1),
                                     (x2,y2),
                                     (x3,y2),
                                     (x4,y1),
                                     (x4,y3),
                                     (x3,y4),
                                     (x2,y4),
                                     (x1,y3)])
  -- saltos posibles del caballo
    where
      x1 = c + 2
      y1 = r + 1
      x2 = c + 1
      y2 = r + 2
      x3 = c - 1
      y3 = r - 1
      x4 = c - 2
      y4 = r - 2

-- Consigue el camino del primer nodo
-- [node]  nodo/s del que obtener el camino
-- Devuelve el camino del primer nodo si hay,
--        si no devuelve el camino vacio
getFirstPath :: [Node] -> Path
getFirstPath nodeList
  | nodeList /= [] = getPath (nodeList!!0)
  | otherwise      = []
    where
      getPath (_,_,_,_,path) = path