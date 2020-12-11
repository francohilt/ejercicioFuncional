-- ¿Y donde está mi hamburguesa? --

------------- TIPOS DE DATOS -------------

data Combo = Combo {
    hamburguesa :: [Ingrediente],
    bebida :: Bebida,
    acompaniamiento :: String 
    } deriving Show

data Bebida = Bebida {
    tipoBebida :: String,
    tamanioBebida :: Tamanio,
    light :: Bool
    } deriving (Show, Eq)

type Ingrediente = String
type Tamanio = Int

------------- DATOS PARA CONSULTAS -------------

cajitaFeliz, dobleMcBacon :: Combo
cajitaFeliz = Combo ["Carne", "Queso", "Pan"] aguaSinGas "Papas"
dobleMcBacon = Combo ["Carne", "Queso", "Pan", "Cheddar", "Panceta", "Cebolla", "Mostaza", "Ketchup"] cocaColaLight "Papas"

aguaSinGas, cocaColaLight :: Bebida
aguaSinGas = Bebida "Agua Mineral" regular False
cocaColaLight = Bebida "Gaseosa" mediano True

regular, mediano, grande :: Tamanio
regular = 1
mediano = 2
grande = 3

informacionNutricional :: [(Ingrediente, Integer)]
informacionNutricional = [
    ("Carne", 250), 
    ("Queso", 50), 
    ("Pan", 20), 
    ("Panceta", 541),
    ("Lechuga", 5), 
    ("Tomate", 6),
    ("Cebolla", 4),
    ("Cheddar", 75)
    ]

condimentos :: [String]
condimentos = ["Barbacoa", "Mostaza", "Mayonesa", "Ketchup"]

----------------------------------------------------------------------

-- 1. Queremos saber cuántas calorías tiene un ingrediente, 
-- esto puede obtenerse a partir de la información nutricional, 
-- a menos que sea un condimento, en cuyo caso la cantidad de calorías es 10.

cuantasCaloriasTiene :: Ingrediente -> Integer
cuantasCaloriasTiene unIngrediente
    | unIngrediente `elem` condimentos = 10
    | otherwise = calorias unIngrediente

calorias :: Ingrediente -> Integer
calorias unIngrediente = (head.map snd) (filter(\x-> (fst x) == unIngrediente) informacionNutricional)

-- EJEMPLOS DE CONSULTAS:
--      cuantasCaloriasTiene "Barbacoa" --> 10
--      cuantasCaloriasTiene "Carne" --> 250
--      cuantasCaloriasTiene "Panceta" --> 541  
--      cuantasCaloriasTiene "Mostaza" --> 10

----------------------------------------------------------------------

-- 2. Se quiere saber si un combo esMortal. 
-- Esto se cumple cuando la bebida no es dietética y la hamburguesa es una bomba 
-- (si tiene entre sus ingredientes al menos uno que tenga más de 300 calorías, 
-- o si en total la hamburguesa supera las 1000 calorías).

esMortal :: Combo -> Bool
esMortal unCombo = (tipoBebida.bebida) unCombo /= "Dietetica" && esUnaBomba (hamburguesa unCombo)

esUnaBomba :: [Ingrediente] -> Bool
esUnaBomba unaHamburguesa
    | any(\x-> calorias x > 300) unaHamburguesa || tieneMasDe1000Calorias unaHamburguesa = True
    | otherwise = False

tieneMasDe1000Calorias :: [Ingrediente] -> Bool
tieneMasDe1000Calorias unaHamburguesa = (sum.map cuantasCaloriasTiene) unaHamburguesa > 1000

-- EJEMPLOS DE CONSULTAS:
--      esMortal dobleMcBacon  --> True
--      esMortal cajitaFeliz --> False

----------------------------------------------------------------------

-- 3. Definir una función que permita obtener a partir de un combo y una lista de alteraciones, 
-- el combo resultante de alterar el combo con todas las alteraciones indicadas. 
-- Las alteraciones puedan ser las siguientes:

-- a. cambiarAcompaniamientoPor: retorna el combo con otro acompaniamiento elegido por el cliente.

cambiarAcompaniamientoPor :: String -> Combo -> Combo
cambiarAcompaniamientoPor unAcompaniamiento unCombo = unCombo {acompaniamiento = unAcompaniamiento}

-- b. agrandarBebida: retorna el combo agrandando la bebida al tamanio siguiente 
-- (teniedo en cuenta que el máximo es el tamanio grande, 
-- no importa cuánto se lo trate de seguir agrandando).

agrandarBebida :: Combo -> Combo
agrandarBebida unCombo
    | (tamanioBebida.bebida) unCombo == regular = unCombo{bebida = cambiarA mediano (bebida unCombo)} 
    | (tamanioBebida.bebida) unCombo == mediano = unCombo{bebida = cambiarA grande (bebida unCombo)} 
    | otherwise = unCombo

cambiarA :: Tamanio -> Bebida -> Bebida
cambiarA tamanio unaBebida = unaBebida{tamanioBebida = tamanio}

-- c. peroSin: 
-- retorna el combo de modo que su hamburguesa no incluya ingredientes que cumplan con
-- una determinada restricción. En principio nos interesan las siguientes restricciones, 
-- pero podría haber otras:

peroSin :: ([Ingrediente] -> [Ingrediente]) -> Combo -> Combo
peroSin unaRestriccion unCombo = unCombo{hamburguesa = unaRestriccion (hamburguesa unCombo)}

--condimento unaHamburguesa = 

queCondimentoTiene :: [Ingrediente] -> [Ingrediente]
queCondimentoTiene unaHamburguesa = filter(\x-> x `elem` condimentos) unaHamburguesa

quitarElemento :: Eq a => [a] -> a -> [a]
quitarElemento [] _ = []
quitarElemento (x:xs) e 
    | x == e = quitarElemento xs e
    | otherwise = x:( quitarElemento xs e )

-- i. esCondimento: 
-- un ingrediente cumple esta restricción si es igual a alguno de los condimentos conocidos.

esCondimento :: Ingrediente -> Bool
esCondimento unIngrediente = unIngrediente `elem` condimentos

--ii. masCaloricoQue: 
-- se cumple esta restricción si las calorías del ingrediente superan un valor dado.

masCaloricoQue :: Ingrediente -> Integer -> Bool
masCaloricoQue unIngrediente cal = (cuantasCaloriasTiene unIngrediente) > cal