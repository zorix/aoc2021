import Debug.Trace

{-

xMin = 20
xMax = 30
yMin = -10
yMax = -5

trace ("d " ++ show [x,y,maxH]) $
-}


xMin = 281
xMax = 311
yMin = -74
yMax = -54

iff c t f
    | c         = t
    | otherwise = f

{-
PartA
-}

doStepMaxY yPos yv maxH = doStepIfMaxY (yPos+yv) (yv-1) (max maxH ((yPos+yv)))

doStepIfMaxY yPos yv maxH 
    | ((yPos+yv) >= yMin) = doStepMaxY yPos yv maxH
    | otherwise           = iff (yPos >= yMin && yPos <= yMax) maxH 99999999


startMaxY yv = doStepMaxY 0 yv 0
partA = maximum $ filter (/=99999999) $ map startMaxY [1..100]

{-
PartB
-}

doStepY yPos yv startY = doStepIfY (yPos+yv) (yv-1) startY

doStepIfY yPos yv startY
    | ((yPos+yv) >= yMin) = doStepY yPos yv startY
    | otherwise           = iff (yPos >= yMin && yPos <= yMax) startY 99999999


startY yv = doStepY 0 yv yv

correctYPos = filter (/=99999999) $ map startY [-400..400]


doStepX xPos xv startX = doStepIfX (xPos+xv) (max 0 (xv-1)) startX

doStepIfX xPos xv startX
    | ((xPos+xv) <= xMax && xv > 0) = doStepX xPos xv startX
    | otherwise                     = iff (xPos >= xMin && xPos <= xMax) startX 99999999


startX xv = doStepX 0 xv xv

correctXPos = filter (/=99999999) $ map startX [-400..400]


doStep xPos yPos xv yv = doStepIf (xPos+xv) (yPos+yv) (max (xv-1) 0) (yv-1)
doStepIf  xPos yPos xv yv
    | ((xPos+xv) <= xMax && (yPos+yv) >= yMin) = doStep xPos yPos xv yv
    | otherwise                                = iff (xPos <= xMax && xPos >= xMin && yPos >= yMin && yPos <= yMax) 1 0

startXY xv yv = doStep 0 0 xv yv
partB = sum $ map (\xv -> sum $ map (startXY xv) correctYPos) correctXPos

main = do {
  putStr "Part A: ";
  print $ partA;
  putStr "Part B: ";
  print $ partB;
}
