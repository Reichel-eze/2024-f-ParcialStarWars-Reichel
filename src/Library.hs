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