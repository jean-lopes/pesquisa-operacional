{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
module TableauSimplex
where
import           Control.Lens
import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Either
import           Control.Monad.Trans.State
import qualified Data.List                  as List
import           Data.Matrix                (Matrix, (!))
import qualified Data.Matrix                as Matrix
import qualified Data.Maybe                 as Maybe
import           Data.Vector                (Vector)
import qualified Data.Vector                as Vector

data PPL = PPL
    { _pivo       :: Rational
    , _linhaPivo  :: Int
    , _colunaPivo :: Int
    , _iteracao   :: Int
    , _matriz     :: Matrix Rational
    }

makeLenses ''PPL

instance Show PPL where
    show (PPL p l c i m) =  "\nPivo..............: " ++ show p
                         ++ "\nLinha de trabalho.: " ++ show l
                         ++ "\nColuna de trabalho: " ++ show c
                         ++ "\nTableau nº" ++ show i ++ "\n"
                         ++ show m

algoritmo :: StateT PPL IO Rational
algoritmo = do
    get >>= liftIO . putStrLn . show
    m <- use matriz
    if isSolucaoOtima m
        then return $ m ! tamanho m
        else do
            iteracao %= (+1)
            let menor = menorValorFO m
                coluna = escolherColunaTrabalho menor m
                linha = escolherLinhaTrabalho coluna m
                valor = m ! (linha, coluna)
            colunaPivo .= coluna
            linhaPivo .= linha
            pivo .= valor
            matriz %= Matrix.mapRow (\_ x -> x / valor) linha
            matriz %= gauss (linha, coluna)
            algoritmo

-- | Tamanho da matriz
tamanho :: Matrix a -> (Int, Int)
tamanho m = (Matrix.nrows m, Matrix.ncols m)

-- | É a solução ótima, quando na linha da função objetivo (última linha)
-- não existir nenhum número maior ou igual a zero
isSolucaoOtima :: (Ord a, Num a) => Matrix a -> Bool
isSolucaoOtima m = (>=0) . menorValorFO $ m

-- | Linha da função objetivo (última linha)
linhaFO :: Matrix a -> Vector a
linhaFO m = Matrix.getRow r m
  where
    r = fst . tamanho $ m

-- | Menor valor na linha da função objetivo (última linha)
menorValorFO :: (Ord a) => Matrix a -> a
menorValorFO = Vector.minimum . linhaFO

-- | Escolher coluna de trabalho (Pivo)
escolherColunaTrabalho :: Eq a => a -> Matrix a -> Int
escolherColunaTrabalho p m = (+1)
                           . Maybe.fromJust
                           . List.elemIndex p
                           . Vector.toList
                           $ linhaFO m

-- | Escolher linha de trabalho (Pivo)
escolherLinhaTrabalho :: (Fractional a, Ord a) => Int -> Matrix a -> Int
escolherLinhaTrabalho n m = (+1) . Maybe.fromJust $ Vector.elemIndex menor calc
  where
    c = snd . tamanho $ m
    colV = Matrix.getCol n m
    colB = Matrix.getCol c m
    calc = Vector.zipWith (\x y -> if y == 0 then 0 else x / y) colB colV
    menor = Vector.minimum $ Vector.filter (>0) calc

gauss :: (Num a, Eq a) => (Int, Int) -> Matrix a -> Matrix a
gauss (i,j) m = foldl (gauss' (i,j)) m linhas
  where
    linhas = filter (/=i) [1 .. Matrix.nrows m]

gauss' :: (Num a, Eq a) => (Int, Int) ->  Matrix a -> Int -> Matrix a
gauss' (linha, coluna) m n = Matrix.mapRow (\j x -> x + z * m ! (linha, j)) n m
  where
    z = m ! (n, coluna) * (-1)

resolver :: IO ()
resolver = evalStateT algoritmo ppl >>= \n -> putStrLn $ "Solução ótima: " ++ show n

ppl :: PPL
ppl = PPL (-1) (-1) (-1) 0 $ Matrix.fromLists
    [ [    1,    1,    1,    0,    0,    0,   50 ]
    , [    3,    2,    0,    1,    0,    0,  120 ]
    , [    1,    0,    0,    0,    1,    0,   30 ]
    , [    0,    1,    0,    0,    0,    1,   40 ]
    , [ -300, -400,    0,    0,    0,    0,    0 ]
    ]
