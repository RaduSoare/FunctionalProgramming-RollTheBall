{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE EmptyDataDecls, MultiParamTypeClasses,
             TypeSynonymInstances, FlexibleInstances,
             InstanceSigs #-}
module RollTheBall where
import Pipes
import ProblemState
import Data.Array as A
{-
    Direcțiile în care se poate mișca o piesa pe tablă
-}

-- Unknown pentru prima mutare fiindca nu are o directie anterioara
data Directions = North | South | West | East | Unknown
    deriving (Show, Ord)

instance Eq Directions where
	North == North = True
	South == South = True
	West == West = True
	East == East = True
	Unknown == Unknown = True
	_ == _ = False

{-
    Sinonim tip de date pentru reprezetarea unei perechi (Int, Int)
    care va reține coordonatele celulelor de pe tabla de joc
-}

type Position = (Int, Int)

{-
    Tip de date pentru reprezentarea celulelor tablei de joc
-}

data Cell = HorPipe | VerPipe | TopLeft | BotLeft | BotRight | TopRight | EmptySpace |
			EmptyCell | StartUp | StartDown | StartLeft | StartRight | WinUp | WinDown | WinLeft |
			WinRight
			deriving ( Ord)

instance Eq Cell where
	HorPipe == HorPipe = True
	VerPipe == VerPipe = True
	TopLeft == TopLeft = True
	BotLeft == BotLeft = True
	BotRight == BotRight = True
	TopRight == TopRight = True
	EmptySpace == EmptySpace = True
	EmptyCell == EmptyCell = True
	StartUp == StartUp = True
	StartDown == StartDown = True
	StartLeft == StartLeft = True
	StartRight == StartRight = True
	WinUp == WinUp = True
	WinDown == WinDown = True
	WinLeft == WinLeft = True
	WinRight == WinRight = True
	_  == _ = False


{-
    Tip de date pentru reprezentarea nivelului curent
-}
data Level = EmptyLevel | Level {cells :: (A.Array Position Cell)}
	deriving Eq

instance Ord Level where 
	(Level cells1) `compare` (Level cells2) = (snd $ bounds cells1) `compare` (snd $ bounds cells2)
	(Level _) `compare` EmptyLevel = GT
	EmptyLevel `compare` _ = LT
{-
    *** Optional *** 
  
    Dacă aveți nevoie de o funcționalitate particulară,
    instantiați explicit clasele Eq și Ord pentru Level.
    În cazul acesta, eliminați deriving (Eq, Ord) din Level.
-}

{-
    *** TODO ***

    Instanțiati Level pe Show. 
    Atenție! Fiecare linie este urmată de \n (endl in Pipes).
-}

instance Show Cell where
		show HorPipe = [horPipe]
		show VerPipe = [verPipe]
		show TopLeft = [topLeft]
		show BotLeft = [botLeft]
		show BotRight = [botRight]
		show TopRight = [topRight]
		show EmptySpace = [emptySpace]
		show EmptyCell = [emptyCell]
		show StartUp = [startUp]
		show StartDown = [startDown]
		show StartLeft = [startLeft]
		show StartRight = [startRight]
		show WinUp = [winUp]
		show WinDown = [winDown]
		show WinLeft = [winLeft]
		show WinRight = [winRight]


{--
parcurg fiecare celula din Level, 
- daca sunt la prima celula din nivel, adaug \n inainte
- daca sunt la ultima celula de pe o linie, adaug \n la sfarsit de linie
- altfel doar concatenez urmatoarea celula
--}
instance Show Level where
	show EmptyLevel = "" 
	show (Level level) = foldl print_helper "" cellList
		where
			size = snd $ snd $ (bounds level)
			cellList = (assocs level)
			print_helper out cell
				| (fst $ fst $ cell) == 0 &&  (snd $ fst $ cell) == 0 = out ++ "\n" ++ show (snd cell)
				| (snd $ fst $ cell) == size = out ++ show (snd cell) ++ "\n"
				| otherwise = out ++ show (snd cell)

		
					



{-
    *** TODO ***
    Primește coordonatele colțului din dreapta jos a hărții.
    Intoarce un obiect de tip Level în care tabla este populată
    cu EmptySpace. Implicit, colțul din stânga sus este (0,0)
-}

{-
- pun pe fiecare celula din nivel un EmptySpace
-}
emptyLevel :: Position -> Level
emptyLevel (x, y) = Level emptySpaces
	where 
		emptySpaces = A.array ((0,0), (x, y)) [((i,j), EmptySpace) | i <- [0..x], j <- [0..y]]


{-
    *** TODO ***

    Adaugă o celulă de tip Pipe în nivelul curent.
    Parametrul char descrie tipul de tile adăugat: 
        verPipe -> pipe vertical
        horPipe -> pipe orizontal
        topLeft, botLeft, topRight, botRight -> pipe de tip colt
        startUp, startDown, startLeft, startRight -> pipe de tip initial
        winUp, winDown, winLeft, winRight -> pipe de tip final
    Parametrul Position reprezintă poziția de pe hartă la care va fi adaugată
    celula, dacă aceasta este liberă (emptySpace).
-}

addCell :: (Char, Position) -> Level -> Level
addCell (_, _) EmptyLevel = EmptyLevel 
addCell (pipeType, position) (Level currentLevel) = if (checkValidPosition position max_line max_column) then (Level newCells) else (Level currentLevel)
	where 
		newCell  = case pipeType of 
				'═' -> HorPipe
				'║' -> VerPipe
				'╔' -> TopLeft
				'╚' -> BotLeft
				'╝' -> BotRight
				'╗' -> TopRight
				'▓' -> EmptyCell
				'┴' -> StartUp
				'┬' -> StartDown
				'┤' -> StartLeft
				'├' -> StartRight
				'╨' -> WinUp
				'╥' -> WinDown
				'╡' -> WinLeft
				'╞' -> WinRight
				_ -> EmptySpace
		newCells = currentLevel A.// [(position, newCell)]
		max_line = fst $ snd $ (bounds currentLevel)
		max_column = snd $ snd $ (bounds currentLevel)
		
{-
- functie care sa verifice daca o pozitie se afla in interiorul hartii
-}
checkValidPosition :: Position -> Int -> Int -> Bool
checkValidPosition position max_line max_column = if ( ((fst $ position) < 0) || ((fst $ position) > max_line) || 
			((snd $ position) < 0) || ((snd $ position) > max_column)) then False
			else True
				


{-
    *** TODO *** 

    Primește coordonatele colțului din dreapta jos al hărții și o listă de 
    perechi de tipul (caracter_celulă, poziția_celulei).
    Întoarce un obiect de tip Level cu toate celeule din listă agăugate pe
    hartă.
    Observatie: Lista primită ca parametru trebuie parcursă de la dreapta 
    la stanga.
-}
 
createLevel :: Position -> [(Char, Position)] -> Level
createLevel position cellList = foldr addCell newLevel cellList
	where 
		newLevel = emptyLevel position
		


{-
    *** TODO ***

    Mișcarea unei celule în una din cele 4 direcții 
    Schimbul se poate face doar dacă celula vecină e goală (emptySpace).
    Celulele de tip start și win sunt imutabile.

    Hint: Dacă nu se poate face mutarea puteți lăsa nivelul neschimbat.
-}

moveCell :: Position -> Directions -> Level -> Level
moveCell _ _ EmptyLevel = EmptyLevel
moveCell position direction (Level currentLevel) = 
	if (movableCell cellPipe && 
		(checkValidPosition updatePosition max_line max_column) && 
		checkEmptySpace) then (Level newCells) 
	else (Level currentLevel)
	where 
		max_line = fst $ snd $ (bounds currentLevel)
		max_column = snd $ snd $ (bounds currentLevel)

		-- celula pe care vreau sa o mut
		cellPipe = currentLevel A.! position
		-- pozitia celulei ce trebuie mutata
		cell_x = fst $ position
		cell_y = snd $ position
		
		-- pozitia unde trebuie mutata celula
		updatePosition = case direction of 
			North -> (cell_x - 1, cell_y)
			South -> (cell_x + 1, cell_y)
			West -> (cell_x, cell_y - 1)
			East -> (cell_x, cell_y + 1)
			_ -> (-1, -1)


		emptyOldCell = currentLevel A.// [((cell_x, cell_y), EmptySpace)]
		newCells = emptyOldCell A.// [(updatePosition, cellPipe)]
		-- verificare daca locul unde vreau sa mut celula este gol
		checkEmptySpace = if ((currentLevel A.! updatePosition) == EmptySpace) then True else False

-- verificare daca celula poate fi mutata
movableCell :: Cell -> Bool
movableCell cell = case cell of
			StartUp -> False
			StartDown -> False
			StartLeft -> False
			StartRight -> False
			WinUp -> False
			WinDown -> False
			WinLeft -> False
			WinRight -> False
			EmptySpace -> False
			_ -> True
-- functie care gaseste celula de inceput a jocului
searchStartCell :: Level -> ((Int, Int), Cell)
searchStartCell EmptyLevel = ((-1, -1), EmptyCell)
searchStartCell (Level level) = head $ filter checkStartCell (assocs level)
	where
		checkStartCell x = case snd x of
			StartUp -> True
			StartDown -> True
			StartLeft -> True
			StartRight -> True
			_ -> False
-- functie care gaseste celula de sfarsit a jocului
searchWinCell :: Level -> ((Int, Int), Cell)
searchWinCell EmptyLevel = ((-1, -1), EmptyCell)
searchWinCell (Level level) = head $ filter checkWinCell (assocs level)
	where
		checkWinCell x = case snd x of
			WinUp -> True
			WinDown -> True
			WinLeft -> True
			WinRight -> True
			_ -> False

{-
    *** HELPER ***

    Verifică dacă două celule se pot conecta.
    Atenție: Direcția indică ce vecin este a
    doua celulă pentru prima.

    ex: connection botLeft horPipe East = True (╚═)
        connection horPipe botLeft East = False (═╚)
-}
connection :: Cell -> Cell -> Directions -> Bool
connection HorPipe cell West = if cell `elem` westConnections then True else False 
connection HorPipe cell East = if cell `elem` eastConnections then True else False 


connection VerPipe cell North = if cell `elem` northConnections then True else False 
connection VerPipe cell South = if cell `elem` southConnections then True else False 


connection TopLeft cell East = if cell `elem` eastConnections then True else False 
connection TopLeft cell South = if cell `elem` southConnections then True else False 

connection BotLeft cell North = if cell `elem` northConnections then True else False 
connection BotLeft cell East = if cell `elem` eastConnections then True else False 

connection BotRight cell North = if cell `elem` northConnections then True else False 
connection BotRight cell West = if cell `elem` westConnections then True else False 

connection TopRight cell West = if cell `elem` westConnections then True else False 
connection TopRight cell South = if cell `elem` southConnections then True else False

connection StartUp cell North = if cell `elem` northConnections then True else False
connection StartDown cell South = if cell `elem` southConnections then True else False
connection StartLeft cell West = if cell `elem` westConnections then True else False
connection StartRight cell East = if cell `elem` eastConnections then True else False

connection WinUp cell North = if cell `elem` northConnections then True else False
connection WinDown cell South = if cell `elem` southConnections then True else False
connection WinLeft cell West = if cell `elem` westConnections then True else False
connection WinRight cell East = if cell `elem` eastConnections then True else False

connection _ _ _ = False 

northConnections :: [Cell]
northConnections = [VerPipe, TopLeft, TopRight, StartDown, WinDown]
southConnections :: [Cell]
southConnections = [VerPipe, BotLeft, BotRight, StartUp, WinUp]
eastConnections :: [Cell]
eastConnections = [HorPipe, BotRight, TopRight, StartLeft, WinLeft]
westConnections :: [Cell]
westConnections = [HorPipe, BotLeft, TopLeft, StartRight, WinRight]


{-
    *** TODO ***

    Va returna True dacă jocul este câștigat, False dacă nu.
    Va verifica dacă celulele cu Pipe formează o cale continuă de la celula
    de tip inițial la cea de tip final.
    Este folosită în cadrul Interactive.
-}
wonLevel :: Level -> Bool
wonLevel EmptyLevel = False
wonLevel (Level level) = wonLevelHelper (fst $ startCell) (Level level) Unknown
	where 
		startCell = searchStartCell (Level level) 
		
{--
- functie recursiva care primeste pozitia celulei pe care vreau sa o mut, nivelul 
si ultima directie considerata
--}
wonLevelHelper :: Position -> Level -> Directions -> Bool		 
wonLevelHelper _ EmptyLevel _ = False
wonLevelHelper pos (Level currentLevel) direction
	| (checkValidPosition northPos max_line max_column) && 
		(connection (currentLevel A.! pos) (currentLevel A.! northPos) North) && 
		((direction /= North) || (direction == Unknown)) = wonLevelHelper northPos (Level currentLevel) South
	| (checkValidPosition southPos max_line max_column) && 
		(connection (currentLevel A.! pos) (currentLevel A.! southPos) South) && 
		((direction /= South) || (direction == Unknown)) = wonLevelHelper southPos (Level currentLevel) North
	| (checkValidPosition eastPos max_line max_column) && 
		(connection (currentLevel A.! pos) (currentLevel A.! eastPos) East) && 
		((direction /= East) || (direction == Unknown)) =  wonLevelHelper eastPos (Level currentLevel) West
	| (checkValidPosition westPos max_line max_column) && 
		(connection (currentLevel A.! pos) (currentLevel A.! westPos) West) && 
		((direction /= West) || (direction == Unknown)) = wonLevelHelper westPos (Level currentLevel) East 
	| ((fst $ fst $ winCell) == (fst $ pos)) && ((snd $ fst $ winCell) == (snd $ pos)) = True
	| otherwise = False
		where
			northPos = ((fst $ pos) - 1, snd $ pos)
			southPos = ((fst $ pos) + 1, snd $ pos)
			westPos = (fst $ pos, (snd $ pos) - 1)
			eastPos = (fst $ pos, (snd $ pos) + 1)
			winCell = searchWinCell (Level currentLevel)
			max_line = fst $ snd $ (bounds currentLevel)
			max_column = snd $ snd $ (bounds currentLevel)


successorsHelper :: Level -> Position -> Directions -> [((Position, Directions), Level)]
successorsHelper EmptyLevel _ _ = [] 
successorsHelper (Level level) pos direction
	| ((direction == South) && (movableCell currentCell) && (checkValidPosition southPos max_line max_column) &&
		(checkEmptySpace southPos)) = outputList ++ [((pos, South), (moveCell pos South (Level level)))] 
	| ((direction == North) && (movableCell currentCell) && (checkValidPosition northPos max_line max_column) &&
		(checkEmptySpace northPos)) = outputList ++ [((pos, North), (moveCell pos North (Level level)))] 
	| ((direction == East) && (movableCell currentCell) && (checkValidPosition eastPos max_line max_column) &&
		(checkEmptySpace eastPos)) = outputList ++ [((pos, East), (moveCell pos East (Level level)))] 
	| ((direction == West) && (movableCell currentCell) && (checkValidPosition westPos max_line max_column) &&
		(checkEmptySpace westPos)) = outputList ++ [((pos, West), (moveCell pos West (Level level)))] 
	| otherwise = [] 
	where
		outputList = []
		northPos = ((fst $ pos) - 1, snd $ pos)
		southPos = ((fst $ pos) + 1, snd $ pos)
		westPos = (fst $ pos, (snd $ pos) - 1)
		eastPos = (fst $ pos, (snd $ pos) + 1)

		max_line = fst $ snd $ (bounds level)
		max_column = snd $ snd $ (bounds level)

		currentCell = level A.! pos		

		checkEmptySpace position = if ((level A.! position) == EmptySpace) then True else False

successorsHelperGeneral :: Level -> Position -> [((Position, Directions), Level)]
successorsHelperGeneral EmptyLevel _ = []
successorsHelperGeneral (Level level) pos  = 
	successorsHelper (Level level) pos North ++ 
	successorsHelper (Level level) pos South ++ 
	successorsHelper (Level level) pos East ++
	successorsHelper (Level level) pos West

instance ProblemState Level (Position, Directions) where
    successors :: Level -> [((Position, Directions), Level)]
    successors EmptyLevel = []
    successors (Level level) = foldr (\x y -> ((successorsHelperGeneral (Level level) (fst $ x)) ++ y)) [] (assocs level) 
    isGoal = undefined
    reverseAction = undefined