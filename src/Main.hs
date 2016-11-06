import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

main :: IO ()
main = startGUI defaultConfig setup

setup :: Window -> UI ()
setup w = do
    return w # set title "Holz"

    out  <- UI.span # set text "Coordinates: "
    wrap <- card
    wrap2 <- card
    oppBottom <- UI.row $ replicate 5 card
    oppMiddle <- UI.row $ replicate 5 card
    oppTop <- UI.row $ space : replicate 3 card
    midRow <- UI.row $ replicate 5 card
    meTop <- UI.row $ space : replicate 3 card
    meBottom <- UI.row $ replicate 5 card
    meMiddle <- UI.row $ replicate 5 card
    spacer <- space
    col' <- column [element oppTop, element oppMiddle, element oppBottom,
                    element spacer, element midRow, element spacer,
                    element meTop, element meMiddle, element meBottom]
    getBody w #+ [element col']

    on UI.mousemove wrap $ \xy ->
        element out # set text ("Coordinates: " ++ show xy)
    on UI.keydown   wrap $ \c ->
        element out # set text ("Keycode: " ++ show c)

card = UI.div #. "wrap"
        # set style [("width","52px"),("height","76px"),("border","solid black 1px"),("border-radius","5px")]
        # set (attr "tabindex") "1" -- allow key presses

space = UI.div #. "wrap"
        # set style [("width","52px"),("height","76px")]
