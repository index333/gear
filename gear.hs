import Graphics.UI.Gtk
import MySpinBox
import Round
main = do
    c <- getContents
    let [_,_,a,_] = words c
    let t = read a :: Double
    let names = ["タイヤ周長(mm)",
                    "frontA(T)",
                    "frontB(T)",
                    "最大コグ(T)",
                    "cadens(/m)"]
    let spmods = [(t,2000,2300,1,10), 
                    (39,20,60,1,10), 
                    (52,20,60,1,10), 
                    (23,21,32,1,10), 
                    (90,50,160,1,10)]
    initGUI
    window  <- windowNew
    hbox <- hBoxNew False 0
    vbox <- vBoxNew False 0
    boxPackStart vbox hbox PackNatural 0
    adjs <- mkAdjustments spmods
    update adjs
    spins <- myAddSpinButtons hbox names adjs
    mapM_ (`set` [spinButtonDigits := 0]) spins
    mapM_ (flip onValueChanged  (update adjs)) adjs
    containerAdd window vbox
    widgetShowAll window
    window `on` unrealize $ mainQuit
    mainGUI
update adjs = do
    l:fa:fb:m:c:_ <- mapM (`get` adjustmentValue) adjs
    putStr "タイヤ周長(mm) = " 
    print l
    mapM_ (disp l fa fb c) [11..m]
disp l fa fb c x = do
    disp' l fa x c
    putStr "   "
    disp' l fb x c
    putStrLn ""
disp' l f r c = do
    putStr $ show $ round f
    putStr "x"
    putStr $ show $ round r
    putStr "="
    putStr $ show (kph l f r c)
    putStr "km/h "
kph l f r c = round1 $ (l/1000^2) * (f / r) * (c * 60) 