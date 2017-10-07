data Day = Lundi | Mardi | Mercredi | Jeudi | Vendredi | Samedi | Dimanche deriving (Eq,Show,Ord)
data Month = Janvier | Fevrier | Mars | Avril | Mai | Juin | Juillet | Aout | Septembre | Octobre | Novembre | Decembre deriving (Eq,Show,Ord)


succm :: Month -> Month
succm Janvier = Fevrier
succm Fevrier = Mars
succm Mars = Avril
succm Avril = Mai
succm Mai = Juin
succm Juin = Juillet
succm Juillet = Aout
succm Aout = Septembre
succm Septembre = Octobre
succm Octobre = Novembre
succm Novembre = Decembre
succm Decembre = Janvier

succJ :: Day -> Day
succJ Lundi = Mardi
succJ Mardi = Mercredi
succJ Mercredi = Jeudi
succJ Jeudi = Vendredi
succJ Vendredi = Samedi
succJ Samedi = Dimanche
succJ Dimanche = Lundi

count :: Month -> Int
count month = go Janvier 0
    where go m acc
            | m == month = acc
            | otherwise = go (succm m) (acc+1)



type Year = Int

newtype Date = Date { runDate :: (Int,Month,Year,Day) } deriving (Show,Eq)

isSunday :: Date -> Bool
isSunday (Date (_,_,_,d)) = d == Dimanche

isSundayFirst :: Date -> Bool
isSundayFirst (Date (j,_,_,d)) = j == 1 && d == Dimanche

isLeapYear :: Year -> Bool
isLeapYear y
    | y `rem` 100 == 0 = y `rem` 400 == 0
    | otherwise = y `rem` 4 == 0

daysMonth :: (Month,Year) -> Int
daysMonth (Aout,_) = 31
daysMonth (Fevrier,y)
    | isLeapYear y = 29
    | otherwise = 28
daysMonth (m,y)
    | m > Aout = if (odd (count m)) then 31 else 30
    | otherwise = if (even (count m)) then 31 else 30

happyNewYear :: Date -> Bool
happyNewYear (Date (31,Decembre,_,_)) = True
happyNewYear _ = False

nextMonth :: Date -> Date
nextMonth dn@(Date d)
    | happyNewYear dn = Date (1,Janvier,year + 1,succJ day)
    | j == (daysMonth (month,year)) = Date (1,succm month,year,succJ day)
    | otherwise = Date (succ j,month,year,succJ day)
        where (j,month,year,day) = d

findDate :: (Int,Month,Year) -> Date
findDate (j,m,y) = go firstDate
    where go d@(Date (jd,md,yd,_)) | (j,m,y) == (jd,md,yd) = d | otherwise = go (nextMonth d)

firstDate :: Date
firstDate = Date (1,Janvier,1900,Lundi)

findSunday :: Date -> (Int,Month,Year) -> Int
findSunday beg (j,m,y) = go beg 0
    where go date@(Date (a,b,c,d)) acc
            | (j,m,y) == (a,b,c) = if isSundayFirst date then acc+1 else acc
            | isSundayFirst date = go (nextMonth date) (acc+1)
            | otherwise = go (nextMonth date) (acc)
