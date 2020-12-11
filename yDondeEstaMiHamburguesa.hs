-- ¿Y donde está mi hamburguesa? --

------------- TIPOS DE DATOS -------------

data Combo = Combo {
    hamburguesa :: [Ingrediente],
    bebida :: Bebida,
    acompaniamiento :: String 
    }

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

