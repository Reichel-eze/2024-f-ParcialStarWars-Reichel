module Library where
import PdePreludat

-- 1) Modelar las naves espaciales mencionadas y agregar una nueva nave, con un poder especial sutilmente 
-- diferente a alguna de las anteriores, en el que se aproveche las otras implementaciones.

data Nave = UnaNave {
    nombre :: String,
    durabilidad :: Number,
    escudo :: Number,
    ataque :: Number,
    poder :: Poder
} deriving (Show, Eq)

-- NAVES

tieFighter :: Nave 
tieFighter = UnaNave "TIE Fighter" 200 100 50 movimientoTurbo

xWing :: Nave
xWing = UnaNave "X Wing" 300 150 100 reparacionDeEmergencia

naveDeDarthVade :: Nave
naveDeDarthVade = UnaNave "Nave de Darth Vade" 500 300 200 movimientoSuperTurbo

millenniumFalcon :: Nave
millenniumFalcon = UnaNave "Millennium Falcon" 1000 500 50 (reparacionDeEmergencia . incrementarEscudosEn 100)

naveReichel :: Nave
naveReichel = UnaNave "Nave Reichel" 1000 1000 1000 poderFlashero

-- Algunas funciones de ayuda

aumentarAtaque :: Number -> Nave -> Nave 
aumentarAtaque valor nave = nave {ataque = max 0 (ataque nave + valor)} 

aumentarDurabilidad :: Number -> Nave -> Nave 
aumentarDurabilidad valor nave = nave {durabilidad = max 0 (durabilidad nave + valor)} 

incrementarEscudosEn :: Number -> Nave -> Nave
incrementarEscudosEn valor nave = nave {escudo = max 0 (escudo nave + valor)} 

cambiarNombre :: String -> Nave -> Nave
cambiarNombre nuevoNombre nave = nave {nombre = nuevoNombre} 

--aumentarPropiedad :: (Nave -> Number) -> Number -> Nave -> Nave
--aumentarPropiedad propiedad valor = nave {propiedad = propiedad nave + valor}

-- PODERES de las naves

type Poder = Nave -> Nave

movimientoTurbo :: Poder
movimientoTurbo = aumentarAtaque 25  

reparacionDeEmergencia :: Poder
reparacionDeEmergencia = aumentarAtaque (-30) . aumentarDurabilidad 50

movimientoSuperTurbo :: Poder
movimientoSuperTurbo =  aumentarDurabilidad (-45) . movimientoTurbo . movimientoTurbo . movimientoTurbo

poderFlashero :: Poder
poderFlashero = cambiarNombre "xd" 

-- 2) Calcular la durabilidad total de una flota, formada por un conjunto de naves, que es la suma de la durabilidad
-- de todas las naves que la integran.

durabilidadTotal :: [Nave] -> Number
durabilidadTotal = sum . map durabilidad

-- 3) Saber cómo queda una nave luego de ser atacada por otra. 
-- Cuando ocurre un ataque ambas naves primero activan su poder especial y luego la nave atacada reduce 
-- su durabilidad según el daño recibido, que es la diferencia entre el ataque de la atacante y el escudo de la atacada. 
-- (si el escudo es superior al ataque, la nave atacada no es afectada). La durabilidad, el escudo y el ataque nunca 
-- pueden ser negativos, a lo sumo 0.

intentarAtaque :: Nave -> Nave -> Nave
intentarAtaque naveAtacante naveDefensora = intentarAtacar (activarPoder naveAtacante) (activarPoder naveDefensora) 

activarPoder :: Nave -> Nave
activarPoder nave = poder nave nave 

intentarAtacar :: Nave -> Nave -> Nave
intentarAtacar naveAtacante naveDefensora 
    | ataqueSuperiorAlEscudo naveAtacante naveDefensora = atacar naveAtacante naveDefensora
    | otherwise                                         = naveDefensora    -- (si el escudo es superior al ataque, la nave atacada no es afectada)

ataqueSuperiorAlEscudo :: Nave -> Nave -> Bool
ataqueSuperiorAlEscudo naveAtacante = (< ataque naveAtacante) . escudo 

atacar :: Nave -> Nave -> Nave
atacar naveAtacante naveDefensora = (flip aumentarDurabilidad naveDefensora . (*(-1)) . danioRecibido naveAtacante) naveDefensora 

atacar' :: Nave -> Nave -> Nave
atacar' naveAtacante naveDefensora = aumentarDurabilidad ((-1) * danioRecibido naveAtacante naveDefensora) naveDefensora

danioRecibido :: Nave -> Nave -> Number
danioRecibido naveAtacante naveDefensora = ataque naveAtacante - escudo naveDefensora

-- 4) Averiguar si una nave está fuera de combate, lo que se obtiene cuando su durabilidad llegó a 0. 

estaFueraDeCombate :: Nave -> Bool
estaFueraDeCombate = (== 0) . durabilidad

-- 5) Averiguar cómo queda una flota enemiga luego de realizar una misión sorpresa con una nave siguiendo una estrategia.
-- Una estrategia es una condición por la cual la nave atacante decide atacar o no una cierta nave de la flota. 
-- Por lo tanto la misión sorpresa de una nave hacia una flota significa atacar todas aquellas naves de la flota que 
-- la estrategia determine que conviene atacar. 

realizarMisionSorpresa :: Nave -> Estrategia -> [Nave] -> [Nave]
realizarMisionSorpresa naveAtacante estrategia  = map (intentarAtaque naveAtacante) . filter estrategia
-- 1ero. Filtro las naves de la flota que la estrategia determina conveniente a atacar
-- 2dos. Ataco todas aquellas naves que fueron filtradas y me devuelve como quedo la flota atacada

-- Podria utilizarlo
intentarAtaqueFlota :: Nave -> [Nave] -> [Nave]
intentarAtaqueFlota naveAtacante = map (intentarAtaque naveAtacante) 

-- Algunas estrategias que existen, y que deben estar reflejadas en la solución, son:

type Estrategia = Nave -> Bool

-- 1. Naves débiles: Son aquellas naves que tienen menos de 200 de escudo.

naveDebil :: Estrategia
naveDebil = (< 200) . escudo 

-- 2. Naves con cierta peligrosidad: Son aquellas naves que tienen un ataque mayor a un valor dado. 
-- Por ejemplo, en alguna misión se podría utilizar una estrategia de peligrosidad mayor a 300, y 
-- en otra una estrategia de peligrosidad mayor a 100.

naveConCiertaPeligrosidad :: Number -> Estrategia
naveConCiertaPeligrosidad valor = (> valor) . ataque

-- 3. Naves que quedarían fuera de combate: Son aquellas naves de la flota que luego del ataque de la nave atacante 
-- quedan fuera de combate. 

naveQueQuedariaFueraDeCombate :: Nave -> Estrategia
naveQueQuedariaFueraDeCombate naveAtacante = estaFueraDeCombate . atacar naveAtacante

-- 4. Inventar una nueva estrategia

naveQueComienzaConNave :: Estrategia
naveQueComienzaConNave = (=="Nave") . take 4 . nombre

-- 6) Considerando una nave y una flota enemiga en particular, dadas dos estrategias, determinar 
-- cuál de ellas es la que minimiza la durabilidad total de la flota atacada y llevar adelante una misión con ella.

realizarMisionConEstratategiaMasOptima :: Nave -> Estrategia -> Estrategia -> [Nave] -> [Nave]
realizarMisionConEstratategiaMasOptima naveAtacante estrategia1 estrategia2 flotaEnemiga = realizarMisionSorpresa naveAtacante (minimizaLaDurabilidad naveAtacante estrategia1 estrategia2 flotaEnemiga) flotaEnemiga

minimizaLaDurabilidad :: Nave -> Estrategia -> Estrategia -> [Nave] -> Estrategia
minimizaLaDurabilidad naveAtacante estrategia1 estrategia2 flotaEnemiga
    | durabilidadTotal (realizarMisionSorpresa naveAtacante estrategia1 flotaEnemiga) <= durabilidadTotal (realizarMisionSorpresa naveAtacante estrategia2 flotaEnemiga) = estrategia1
    | otherwise                                                                                                                                                          = estrategia2       
    
-- 7) Construir una flota infinita de naves. ¿Es posible determinar su durabilidad total? 
-- ¿Qué se obtiene como respuesta cuando se lleva adelante una misión sobre ella? Justificar conceptualmente   

flotaInfinita :: [Nave]
flotaInfinita = tieFighter : flotaInfinita      

-- > durabilidadTotal flotaInfinita 
-- ERROR

-- No es posible determinar la durabilidad total de una flota infinita porque para realizar el calculo de dicha durabilidad
-- necesito la durabilidad de cada una de las naves que conforman a la flota para luego realizar la sumatoria. Por lo tanto,
-- al ser una lista infinita nunca voy a llegar a sumar todas las durabilidades

-- > realizarMisionSorpresa nave estrategia flotaInfinita
-- ERROR

-- Tampoco sera posible porque la estrategia analizara/filtrara las naves que determine conveniente atacar y como es una flota
-- infinita "nunca" terminara de filtrar dichas naves. Ademas ninguna de las estrategias definidas impilica por ej que TODAS o ALGUNAS 
-- de las naves cumplan tal requesito, por lo tanto "nunca" dejara de chequear si la siguiente nave en la lista cumple o no