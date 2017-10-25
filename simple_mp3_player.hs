import System.IO
import Control.Monad
import Data.Char
import System.Exit
import Data.Binary

main = do
	appHeading
	putStrLn "\nLoad previous configuration? \nType 'load' to do so. Press enter to create a new Player" 
	userInput <- getLine
	if (userInput == "load") then do
		readList <- readFile "mp3List.dat"
		readSizing <- readFile "mp3Player_Sizing.dat"
		let fReadList = read readList
		let fReadSizing = read readSizing
		player fReadList fReadSizing 

	else do	
		putStrLn "\nWelcome!\nTo get started enter the size of the MP3 player in megabytes"
		isValidSize
		
isValidSize = do
	putStrLn "\nMP3 Player Size:\t"
	size <- getLine
	let storageSize = read size

	if isNumeric size
		then do player [] [storageSize]
	else do
		putStrLn "ERROR -> Not a legal size, must be a number!"
		isValidSize	
		
player :: [(String, String, String, Int, Double)] -> [Double] -> IO ()
player mp3Store mp3PlayerSizeList = do
	mainMenu
  	userInput <- getLine
  	if (userInput == "1") then do 
  		addMP3 mp3Store mp3PlayerSizeList
  	else if (userInput == "2") then do
  		removeMP3 mp3Store mp3PlayerSizeList
	else if (userInput == "3") then do
		searchForMP3s mp3Store
		player mp3Store mp3PlayerSizeList 
	else if (userInput == "4") then do 
		displayAllStoredMP3s mp3Store
		player mp3Store mp3PlayerSizeList	
	else if (userInput == "5") then do
		displayTotalSpaceUsed mp3PlayerSizeList
		player mp3Store mp3PlayerSizeList
	else if (userInput == "6") then do
		displayTotalSpaceAvailable mp3PlayerSizeList
		player mp3Store mp3PlayerSizeList	
	else if (userInput == "7") then 
		 playMP3 mp3Store mp3PlayerSizeList
	else if (userInput == "8") then do
		displayMostPlayedMP3 mp3Store
		player mp3Store mp3PlayerSizeList
	else if (userInput == "9") then do
		displayLeastPlayedMP3 mp3Store
		player mp3Store mp3PlayerSizeList

	else if (userInput == "10") then exitApplication mp3Store mp3PlayerSizeList
  	else player mp3Store mp3PlayerSizeList
	
addMP3 :: [(String, String, String, Int, Double)] -> [Double] -> IO ()
addMP3 mp3List sizeList = 
	do
		menuOptionSpacer
		putStrLn "ADD MP3"
		putStrLn "________________________________________________\n"
		putStrLn "Please enter the Title:"
		title <- getLine
		putStrLn "Please enter the Artist:"
		artist <- getLine
		putStrLn "Please enter the Genre:"
		genre  <- getLine
		putStrLn "Please enter the Size:"
		size <- getLine
		
		let parsedPlayCount = 0
		let mp3Size = read size
		let mp3 =  [(title, genre , artist, parsedPlayCount, mp3Size)]
		
		if(last sizeList - mp3Size < 0) then do
			 putStrLn "Error, the file is too large to be added to the player..." 
			 addMP3 mp3List sizeList
			 else return()

		let mp3Store = mp3List ++ mp3
		let sizeCalculation = last sizeList - mp3Size
		let mp3PlayerSizeList = sizeList ++ [sizeCalculation]

		putStrLn "\n\tSuccessfuly added the following mp3"
		print mp3Store 
		print mp3PlayerSizeList
		player mp3Store mp3PlayerSizeList

removeMP3 :: [(String, String, String, Int, Double)] -> [Double] -> IO ()
removeMP3 mp3List mp3PlayerSizeList = do
	putStrLn "Enter the title of the MP3 you wish to delete!"	
	searchTerm <- getLine
	let fSearchTerm = searchTerm

	-- creating a new list out of the tuples which were not equal to the search term.
	let newStore = [(title,b,c,d,e)| (title,b,c,d,e) <- mp3List, title /= fSearchTerm]
	player newStore mp3PlayerSizeList


searchForMP3s :: [(String, String, String, Int, Double)] -> IO ()
searchForMP3s mp3Store = do
	searchByMenu
	userInput <- getLine
	if (userInput == "1") then do
		putStrLn "Enter the title of the song"
		userTitle <- getLine
		let titleResult = [(title,b,c,d,e)| (title,b,c,d,e) <- mp3Store, title == userTitle]
		printer titleResult
	else if (userInput == "2") then do
		putStrLn "Enter the artist"
		userArtist <- getLine
		let titleResult = [(a,artist,c,d,e)| (a,artist,c,d,e) <- mp3Store, artist  == userArtist]
		printer titleResult
	else if (userInput == "3") then do
		putStrLn "Enter the genre"
		userGenre <- getLine
		let titleResult =  [(a,b,genre,d,e)| (a,b,genre,d,e) <- mp3Store, genre == userGenre]
		printer titleResult		
	else do
		searchForMP3s mp3Store

displayAllStoredMP3s :: [(String, String, String, Int, Double)] -> IO ()
displayAllStoredMP3s mp3Store = printer mp3Store

displayTotalSpaceUsed :: (Num a, Show a) => [a] -> IO ()
displayTotalSpaceUsed memoryList = do
	let calSpaceLeft = head memoryList - last memoryList
	putStr "Total Memory Used: "
	print calSpaceLeft 
		
displayTotalSpaceAvailable :: (Num a, Show a) => [a] -> IO ()	
displayTotalSpaceAvailable memoryList = do
	let remainSpace = last memoryList
	putStr "Remaining Memory: "
	print remainSpace 

playMP3 :: [(String, String, String, Int, Double)] -> [Double] -> IO ()
playMP3 mp3List sizeList = do
	menuOptionSpacer
	putStrLn "\n\n\tPlay MP3 ->...\n\t1. Enter the song position in the playlist\n\t2. Enter the song by title"
	userSelection <-getLine
	if (userSelection == "1") then do
		putStrLn "Enter the index of Song: "
		indexNum <- getLine
		let posNum = (read indexNum ::Int)
		let newList = [(a,b,c,playCount+1,e)| (a,b,c,playCount,e) <- mp3List, (a,b,c,playCount,e) == mp3List !! (posNum - 1)]
		let newTuple = head(newList)
		putStrLn "Now Playing...."
		printer newList
		let modifiedList = replaceNth (posNum) newTuple mp3List
		player modifiedList sizeList

	else if (userSelection == "2") then do
		putStrLn "Enter the Title of Song: "
		songTitle <- getLine
		let songTitleP = songTitle
		let foundResults = [(title,b,c,d,e)| (title,b,c,d,e) <- mp3List, title == songTitleP]
		let cloneResult = [(title,b,c,d+1,e)| (title,b,c,d,e) <- mp3List, title == songTitleP]
		let newTuple = head(foundResults)
		let cloneTuple = head(cloneResult)
		let indexOfNewTuple = getIndexTuple newTuple mp3List 0
		putStrLn "Now Playing...."
		let modifiedList = replaceNth (indexOfNewTuple) cloneTuple mp3List
		printer modifiedList
		player modifiedList sizeList
	else
		player mp3List sizeList

    
getIndexTuple :: (Eq a, Num a1) => a -> [a] -> a1 -> a1
getIndexTuple _ [] count	= count
getIndexTuple x (y:ys) count
	| x == y	= count
	| otherwise = getIndexTuple x ys count+1
	
	
replaceNth :: (Eq a, Num a) => a -> a1 -> [a1] -> [a1]	
replaceNth n newVal (x:xs)
     | n == 0 = newVal:xs
     | otherwise = x:replaceNth (n-1) newVal xs


displayLeastPlayedMP3 :: [(String, String, String, Int, Double)] -> IO ()
displayLeastPlayedMP3 toPrint = printer([getLeastPlayed toPrint])

displayMostPlayedMP3 :: [(String, String, String, Int, Double)] -> IO ()
displayMostPlayedMP3 toPrint = printer([getMostPlayed toPrint])

exitApplication :: [(String, String, String, Int, Double)] -> [Double] -> IO ()
exitApplication mp3Store mp3PlayerSizeList = do
	menuOptionSpacer
	putStrLn "EXIT"
	putStrLn "________________________________________________\n"
	putStrLn "You have chosen to exit the application.\nType 'Yes' to exit 'No' to return to the main menu"
	exitChoice <- getLine
	if (exitChoice `elem` ["Yes","Y","y","1","Yep","yes"]) then do 
		writeFile "mp3List.dat" (show mp3Store)
		writeFile "mp3Player_Sizing.dat" (show mp3PlayerSizeList)

	else if (exitChoice `elem` ["No","N","n","0","Nope","no"]) 
		then do
		putStrLn "Redirecting..."	
		player mp3Store mp3PlayerSizeList
	else exitApplication mp3Store mp3PlayerSizeList

appHeading :: IO ()
appHeading = do 
	putStrLn "  __  __ _____ ____    _____  _                       "
	putStrLn " |  \\/  |  __ \\___ \\  |  __ \\| |                      "
	putStrLn " | \\  / | |__) |__) | | |__) | | __ _ _   _  ___ _ __ "
	putStrLn " | |\\/| |  ___/|__ <  |  ___/| |/ _` | | | |/ _ \\ '__|"
	putStrLn " | |  | | |    ___) | | |    | | (_| | |_| |  __/ |   "
	putStrLn " |_|  |_|_|   |____/  |_|    |_|\\__,_|\\__, |\\___|_|   "
	putStrLn "                                       __/ |          "
	putStrLn "                                      |___/           "
	putStrLn "     /\\                "
	putStrLn "    /  \\   _ __  _ __  "
	putStrLn "   / /\\ \\ | '_ \\| '_ \\ "
	putStrLn "  / ____ \\| |_) | |_) |"
	putStrLn " /_/    \\_\\ .__/| .__/ "
	putStrLn "          | |   | |    "
	putStrLn "          |_|   |_|    "


mainMenu :: IO ()
mainMenu = do
	menuOptionSpacer
	putStrLn "MAIN MENU"
	putStrLn "________________________________________________\n"
	putStrLn "1. Add MP3"
	putStrLn "2. Remove MP3"
	putStrLn "3. Search for MP3"
	putStrLn "4. Display all MP3's stored on device"
	putStrLn "5. Display total space used on device"
	putStrLn "6. Display total space available on device"
	putStrLn "7. Play MP3"
	putStrLn "8. Display most played MP3"
	putStrLn "9. Display least played MP3"
	putStrLn "10. Exit"
	putStrLn "________________________________________________\n"


searchByMenu :: IO ()
searchByMenu = do
	menuOptionSpacer
	putStrLn "SEARCH BY : TITLE, ARTIST, GENRE"
	putStrLn "________________________________________________\n"
	putStrLn "1. Title"
	putStrLn "2. Artist"
	putStrLn "3. Genre"
	putStrLn "________________________________________________\n"


menuOptionSpacer :: IO ()
menuOptionSpacer = do
	putStrLn "\n\n\t"

displayAllStoredMP3s_Helper :: IO ()
displayAllStoredMP3s_Helper = do
	menuOptionSpacer
	putStrLn "DISPLAY ALL MP3s"
	putStrLn "________________________________________________\n"

printer :: [(String, String, String, Int, Double)] -> IO ()
printer xs = putStrLn . unlines . map printer_helper $ xs

printer_helper :: (String, String, String, Int, Double) -> String
printer_helper (title, artist, genre, playCount, size) = 
	"\n" ++
	"Title    : " ++ title ++ "\n" ++ 
	"Artist   : " ++ artist ++ "\n" ++ 
	"Genre    : " ++ genre ++ "\n" ++ 
	"PlayCount: " ++ show playCount ++ "\n" 
	++ "Size  : " ++ show size ++ "\n"


getMostPlayed :: Ord playCount => [(title, artist, genre, playCount, fileSize)] -> (title, artist, genre, playCount, fileSize)
getMostPlayed []     = error "maximum of empty list"
getMostPlayed (x:xs) = maxTail x xs
  where maxTail currentMax [] = currentMax
        maxTail (title, artist, genre, playCount, fileSize) (p:ps)
          | playCount < (getPlayCount p) = maxTail p ps
          | otherwise   = maxTail (title, artist, genre, playCount, fileSize) ps




getLeastPlayed :: Ord playCount => [(title, artist, genre, playCount, fileSize)] -> (title, artist, genre, playCount, fileSize)
getLeastPlayed []     = error "maximum of empty list"
getLeastPlayed (x:xs) = maxTail x xs
  where maxTail currentMax [] = currentMax
        maxTail (title, artist, genre, playCount, fileSize) (p:ps)
          | playCount > (getPlayCount p) = maxTail p ps
          | otherwise   = maxTail (title, artist, genre, playCount, fileSize) ps


getTitle :: (title, artist, genre, playCount, fileSize) -> title
getTitle (title, artist, genre, playCount, fileSize) = title

getArtist :: (title, artist, genre, playCount, fileSize) -> artist
getArtist (title, artist, genre, playCount, fileSize) = artist

getGenre :: (title, artist, genre, playCount, fileSize) -> genre
getGenre (title, artist, genre, playCount, fileSize) = genre

getPlayCount :: (title, artist, genre, playCount, fileSize) -> playCount
getPlayCount (title, artist, genre, playCount, fileSize) = playCount

getSize :: (title, artist, genre, playCount, fileSize) -> fileSize
getSize (title, artist, genre, playCount, fileSize) = fileSize


getPlaycount :: (String, String, String, Int, Double) -> (String, String, String, Int, Double)
getPlaycount (a,b,c,playCount,d) = (a,b,c,playCount + 1,d)



isNumeric :: String -> Bool
isNumeric s = isInteger s || isDouble s

isInteger :: String -> Bool
isInteger s = case reads s :: [(Integer, String)] of
  [(_, "")] -> True
  _         -> False
 
isDouble :: String -> Bool 
isDouble s = case reads s :: [(Double, String)] of
  [(_, "")] -> True
  _         -> False
  
