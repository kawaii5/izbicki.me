import Control.Monad
import System.IO

main = do
    let range=100::Double
    forM [0..range] $ \x -> do
        forM [0..range-x] $ \y -> do
            let z = range-x-y
--         forM [0..range] $ \y -> do
--             let z = max 0 $ range-x-y
            putStrLn $ show (x/range) ++ "  " ++ show (y/range) ++ "  " ++ show (z/range)
            
