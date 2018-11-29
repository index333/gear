import Graphics.UI.Gtk
main = do
    c <- getContents
    let texts = lines c
    initGUI
    w <- windowNew
    w `set` [windowDefaultWidth := 1000, windowDefaultHeight := 800]
    sw <- scrolledWindowNew Nothing Nothing
    vb <- vBoxNew True 0
    bs <- mapM (buttonNewWithLabel) texts
    mapM_ (\b -> b `set` [buttonXalign := 0, buttonYalign := 0]) bs
    mapM_ (\b -> (b `on` buttonActivated) (func b)) bs
    mapM_ (containerAdd vb) bs
    scrolledWindowAddWithViewport sw vb
    containerAdd w sw
    widgetShowAll w
    (w `on`unrealize) mainQuit
    mainGUI
func b = b `get` buttonLabel >>= putStrLn >> mainQuit
