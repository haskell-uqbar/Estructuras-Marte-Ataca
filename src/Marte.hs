module Marte where
import PdePreludat

data Planeta = UnPlaneta {
    habitantes :: Number,
    continentes :: [String],
    medidas:: [String]
}deriving Show

data Enfermedad = UnaEnfermedad {
    mortalidad:: Number,  
    medidaDeCombate:: String
} deriving Show

{-
combinarEnfermedades:: Enfermedad -> Enfermedad-> Enfermedad
combinarEnfermedades primerEnfermedad segundaEnfermedad = 
     UnaEnfermedad (mortalidad primerEnfermedad + mortalidad segundaEnfermedad) 
     (medidaPrincipal primerEnfermedad)

combinarEnfermedades:: Enfermedad -> Enfermedad-> Enfermedad
combinarEnfermedades (UnaEnfermedad mortalidad1 medida1) (UnaEnfermedad mortalidad2 medida2) = 
     UnaEnfermedad (mortalidad1 + mortalidad2) medida1
-}




--punto 1
tierra :: Planeta
tierra = UnPlaneta 77773826  ["América", "Asia", "África", "Europa", "Oceanía", "Antártida"] ["lavarse las manos"]

planetoide::Planeta 
planetoide = UnPlaneta 456123 ["Banana", "Manzana", "Naranja"] ["Vacunarse"]

saturno::Planeta
saturno = UnPlaneta 456221 [] []

covid::Enfermedad
covid = UnaEnfermedad 3.5 "vacuna"

fiebreRoja::Enfermedad
fiebreRoja = UnaEnfermedad 0.7 "vacunacion con 3 dosis"

gripeAlienigena ::Enfermedad
gripeAlienigena = UnaEnfermedad 9.1 "sopa de caracol cuarentena "


--punto 2
combinarEnfermedades:: Enfermedad->Enfermedad->Enfermedad 
combinarEnfermedades uno dos = UnaEnfermedad (mortalidad uno + mortalidad dos) (medidaDeCombate uno)

--punto 3
tieneMedida:: Planeta->Enfermedad->Bool
tieneMedida planeta enfermedad = elem (medidaDeCombate enfermedad) (medidas planeta)

agregarMedidas:: Planeta->Enfermedad-> Planeta
agregarMedidas planeta enfermedad 
    | tieneMedida planeta enfermedad = planeta 
    | otherwise = agregar planeta enfermedad

agregar :: Planeta->Enfermedad->Planeta
agregar (UnPlaneta hab conts meds) (UnaEnfermedad pob medida) = UnPlaneta hab conts (medida:meds) 

victimasEnfermedad::Planeta-> Enfermedad-> Number
victimasEnfermedad planeta enfermedad  = ((habitantes planeta)*(mortalidad enfermedad))/100

--punto 4
planetaEnElHorno:: Planeta->Enfermedad->Bool
planetaEnElHorno planeta enfermedad =  not (tieneMedida planeta enfermedad) && 
    victimasEnfermedad  planeta enfermedad > 1000000 

--punto 5necesito sacar log de ambas listas y comparar

planetaAtacado:: Enfermedad-> Planeta-> Planeta
planetaAtacado enfermedad planeta 
    | not (tieneMedida planeta enfermedad) = sufrirAtaque planeta (victimasEnfermedad planeta enfermedad)
    | otherwise = planeta

sufrirAtaque:: Planeta -> Number -> Planeta
sufrirAtaque (UnPlaneta hab conts meds) victimas =
     UnPlaneta (hab- victimas) conts meds


combinacionMasPotente :: Enfermedad -> Enfermedad -> Planeta -> Bool
combinacionMasPotente enfermedad otraEnfermedad planeta 
     = combinacionAtaca enfermedad otraEnfermedad planeta > combinacionAtaca otraEnfermedad enfermedad planeta 
    
combinacionAtaca :: Enfermedad -> Enfermedad -> Planeta -> Number
combinacionAtaca enfermedad otraEnfermedad planeta = 
    habitantes (planetaAtacado otraEnfermedad (planetaAtacado enfermedad planeta))