  
import Debug.Trace ( trace )
import Data.Function ( (&) )
import Data.List
data Car = Car  { pass :: Int
                , mxp :: Int
                , gas :: Int
                , mxg :: Int
                , km :: Int
                } deriving (Eq, Show, Read)

data Op = Op { name :: String
             , result :: Bool
             } deriving (Eq, Show, Read)

data Info = Info { car :: Car
                 , op  :: Op
                 } deriving (Eq, Show, Read)

toString (Info (Car pass mxp gas mxg km) (Op name result)) =
                "Car pass: " ++ show pass ++ "/" ++ show mxp
                 ++ " gas: " ++ show gas  ++ "/" ++ show mxg
                 ++  " km: " ++ show km
                 ++ " Operation: " ++ name ++ " Result: " ++ show result

resume :: Info -> Info
resume info = trace (toString info) info

-- cria um carro passando maxPass e maxGas - retorna sempre true.
createCar :: Int -> Int -> Info
createCar mxp mxg = Info (Car 0 mxp 0 mxg 0) (Op "create" True)
-- enche o tanque passando a qtd de gas. Retorna falso apenas se o tanque já estiver completamente cheio.
fuel :: Int -> Info -> Info
fuel gasoline (Info (Car pass mxp gas mxg km) _) = (Info(Car pass mxp gasQtd mxg km) (Op "encher" teste))
    where
     teste = mxg > gas
     gasatual = if teste then gas + gasoline else gas
     gasQtd = if gasatual > mxg then mxg else gasatual
-- Faz entrar uma pessoa no carro. Retorna false se já estiver lotado.  
embark :: Info -> Info 
embark (Info(Car pass mxp gas mxg km) _) = (Info(Car passAtual mxp gas mxg km) (Op "embarque" teste))
    where
     teste = pass < mxp
     passAtual = if teste then pass + 1 else pass
-- Retira uma pessoa do carro, retorna false se não tiver ninguém no carro
disembark :: Info -> Info
disembark (Info(Car pass mxp gas mxg km) _) = (Info(Car passAtual mxp gas mxg km) (Op "desembarque" teste))
    where
     teste = pass > 0
     passAtual = if teste then pass - 1 else pass

-- dirige diminuindo a gasolina e aumentando km. 
-- Só é possível dirigir se houver alguém no carro e houver alguma gasolina.
-- Aumenta a km da gasolina gasta.
-- retorna false se não há ninguém no carro ou se não tinha gasolina para completar a viagem.
drive :: Int -> Info -> Info
drive kmR (Info(Car pass mxp gas mxg km) _) = Info(Car pass mxp gastt mxg kmtt) (Op "drive" teste)
    where
     tt = (pass >= 1 && gas > 0)
     teste = tt && gas > kmR
     reduz = gas - kmR
     kmtt = if (teste) then (km + kmR) 
                       else (km + kmR + reduz)
     gastt = if (teste) then reduz else 0

-- main = print $ resume . embark . resume. embark . resume $ createCar 2 50    
main = do 
    let res = createCar 2 50 
            & resume & embark
            & resume & disembark
            & resume & disembark
            & resume & drive 10
            & resume & embark
            & resume & embark
            & resume & embark
            & resume & drive 10
            & resume & fuel 30
            & resume & fuel 30
            & resume & fuel 30
            & resume & drive 30
            & resume & drive 30
            & resume
    print res 

{-
Car pass: 0/2 gas: 0/50 km: 0 Operation: create Result: True
Car pass: 1/2 gas: 0/50 km: 0 Operation: embark Result: True
Car pass: 0/2 gas: 0/50 km: 0 Operation: disembark Result: True
Car pass: 0/2 gas: 0/50 km: 0 Operation: disembark Result: False
Car pass: 0/2 gas: 0/50 km: 0 Operation: drive Result: False
Car pass: 1/2 gas: 0/50 km: 0 Operation: embark Result: True
Car pass: 2/2 gas: 0/50 km: 0 Operation: embark Result: True
Car pass: 2/2 gas: 0/50 km: 0 Operation: embark Result: False
Car pass: 2/2 gas: 0/50 km: 0 Operation: drive Result: False
Car pass: 2/2 gas: 30/50 km: 0 Operation: fuel Result: True
Car pass: 2/2 gas: 50/50 km: 0 Operation: fuel Result: True
Car pass: 2/2 gas: 50/50 km: 0 Operation: fuel Result: False
Car pass: 2/2 gas: 20/50 km: 30 Operation: drive Result: True
Car pass: 2/2 gas: 0/50 km: 50 Operation: drive Result: False
Info {car = Car {pass = 2, maxPass = 2, gas = 0, maxGas = 50, km = 50}, op = Op {name = "drive", result = False}}
-}