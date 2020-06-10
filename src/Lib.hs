module Lib where
import Text.Show.Functions

data Heroe = UnHeroe {
    nombreH :: String,
    epiteto:: String,
    reconocimiento :: Int,
    artefactos :: [Artefacto],
    tareas :: [Tarea]
}

data Artefacto = UnArtefacto {
    nombreA :: String,
    rareza :: Int
}

type Condicion = Heroe -> Bool

data Bestia  = UnaBestia {
   nombreB :: String,
   debilidad :: Condicion
} deriving Show


lanzaDelOlimpo :: Artefacto
lanzaDelOlimpo = UnArtefacto {nombreA = "lanza del olimpo", rareza = 100}

xiphos :: Artefacto
xiphos = UnArtefacto {nombreA = "Xiphos", rareza = 50}

relampagoDeZeus :: Artefacto
relampagoDeZeus = UnArtefacto {nombreA ="El relampago de Zeus", rareza = 500}

pistola::Artefacto
pistola = UnArtefacto {nombreA="pistola", rareza = 1000}

cambiarEpiteto :: String-> Heroe -> Heroe
cambiarEpiteto epitetoNuevo heroe = heroe {epiteto = epitetoNuevo}

cambiarArtefactos :: ([Artefacto] -> [Artefacto])-> Heroe -> Heroe
cambiarArtefactos f heroe = heroe {artefactos = f (artefactos heroe)}

cambiarReconocimiento :: (Int->Int) ->Heroe -> Heroe
cambiarReconocimiento f heroe = heroe {reconocimiento = f (reconocimiento heroe)}

agregarTareaRealizada :: (Heroe -> Heroe) -> Heroe -> Heroe
agregarTareaRealizada tarea heroe = heroe {tareas = [tarea]++ (tareas heroe)}

--------2
pasarALaHistoria :: Heroe -> Heroe
pasarALaHistoria heroe 
    |mayorQue 1000 heroe = cambiarEpiteto "El mitico" heroe
    |mayorQue 500 heroe = cambiarEpiteto "EL magnifico" .cambiarArtefactos (agregarArtefacto lanzaDelOlimpo)  $ heroe
    |mayorQue 100 heroe = cambiarEpiteto "Hoplita" . cambiarArtefactos (agregarArtefacto xiphos) $ heroe
    |otherwise = heroe

mayorQue :: Int ->Heroe -> Bool
mayorQue limite heroe = reconocimiento heroe > limite

agregarArtefacto :: Artefacto -> [Artefacto] -> [Artefacto]
agregarArtefacto nuevo actuales = actuales ++ [nuevo]

-----3
type Tarea = Heroe -> Heroe

encontrarUnArtefacto :: Artefacto -> Tarea
encontrarUnArtefacto artefacto = cambiarReconocimiento (agregarReconocimiento (rareza artefacto)) . cambiarArtefactos (agregarArtefacto artefacto)

agregarReconocimiento :: Int -> Int -> Int
agregarReconocimiento rareza actual = actual + rareza

escalarElOlimpo :: Tarea
escalarElOlimpo = cambiarArtefactos (agregarArtefacto relampagoDeZeus).cambiarReconocimiento (agregarReconocimiento 500). cambiarArtefactos modificarArtefactosOlimpo 

modificarArtefactosOlimpo :: [Artefacto] -> [Artefacto]
modificarArtefactosOlimpo artefactos = filter ((>1000).rareza) $ map rarezaPor3 artefactos

rarezaPor3 :: Artefacto -> Artefacto
rarezaPor3 artefacto = artefacto {rareza = ((*3).rareza)artefacto}

ayudarACruzarLaCalle :: Int -> Tarea
ayudarACruzarLaCalle cuadras = cambiarEpiteto ("Gros"++(replicate cuadras 'o'))

matarUnaBestia :: Bestia -> Tarea
matarUnaBestia bestia heroe
    | (debilidad bestia) $ heroe = cambiarEpiteto ("El asesino de" ++ (nombreB bestia)) $ heroe
    | otherwise = cambiarEpiteto "El cobarde". cambiarArtefactos perderPrimero $ heroe

perderPrimero :: [Artefacto]->[Artefacto]
perderPrimero = drop 1

--------4
heracles :: Heroe
heracles = UnHeroe {nombreH = "Heracles", epiteto = "Guardian del Olimpo", reconocimiento = 700, artefactos = [pistola, relampagoDeZeus], tareas = [matarLeonNemea]}

-----5
leonDeNemea = UnaBestia {nombreB = "Leon de Nemea", debilidad = ((>=20).length.epiteto)}

matarLeonNemea :: Tarea
matarLeonNemea heracles = matarUnaBestia leonDeNemea heracles

--------6
hacerTarea :: Tarea -> Heroe -> Heroe
hacerTarea tarea = agregarTareaRealizada tarea . tarea

------7
presumir :: Heroe -> Heroe -> (Heroe,Heroe)
presumir heroe1 heroe2 
    | ganaheroe heroe1 heroe2 = (heroe1,heroe2)
    | ganaheroe heroe2 heroe1 = (heroe2,heroe1)
    | otherwise = presumir (aplicarTareasOtro heroe1 heroe2) (aplicarTareasOtro heroe2 heroe1)

aplicarTareasOtro :: Heroe -> Heroe -> Heroe
aplicarTareasOtro heroeACambiar = foldr hacerTarea heroeACambiar . tareas

ganaheroe :: Heroe -> Heroe -> Bool
ganaheroe heroe1 heroe2 = (reconocimiento heroe1 > reconocimiento heroe2) || ((reconocimiento heroe1 == reconocimiento heroe2) && (sumatoriaRarezas heroe1 > sumatoriaRarezas heroe2))
 
sumatoriaRarezas :: Heroe -> Int
sumatoriaRarezas = sum . map rareza .artefactos

------------8
{-
se quedaria colgado, ya que como ambos tienen el mismo reconocimiento, se iria al otherwise donde no
se aplica ninguna tarea, por lo que quedan iguales, y asi entra en un ciclo.
-}

--------9
type Labor = [Tarea]

realizarLabor :: Heroe -> Labor ->  Heroe
realizarLabor  heroe = foldr hacerTarea heroe

---------10
{-
no, no se podra conocer el estado final del heroe ya que se realizarian las tareas infinitamente sin
un corte. se colgaria en esa funcion.
-}