module MySpinBox where
import Graphics.UI.Gtk
import Control.Monad
mkAdjustment :: (Double, Double, Double, Double, Double) -> IO Adjustment
mkAdjustment (v,l,u,s,p) = adjustmentNew v l u s p 0
{-
:: Double value - the initial value.
-> Double lower - the minimum value.
-> Double upper - the maximum value.
-> Double stepIncrement - the step increment.
-> Double pageIncrement - the page increment.
-> Double pageSize - the page size.
-}
mkAdjustments :: [(Double, Double, Double, Double, Double)] -> 
                    IO [Adjustment]
mkAdjustments = mapM mkAdjustment 
myAddSpinButtons :: HBox -> [String] ->[Adjustment] -> IO [SpinButton]
myAddSpinButtons box names adjustments = do 
    zipWithM (\x y -> myAddSpinButton box x y) names adjustments
myAddSpinButton :: HBox -> String -> Adjustment -> IO SpinButton
myAddSpinButton box name adj = do
    vbox  <- vBoxNew False 0
    boxPackStart box vbox PackRepel 0
    label <- labelNew (Just name)
    miscSetAlignment label 0.0 0.5
    boxPackStart vbox label PackNatural 0
    spinb <- spinButtonNew adj 10 1
    boxPackStart vbox spinb PackGrow 0
    return spinb
