type Resto = (String, Int, [Comida]) -- Nombre, Mozos, Comida
type Comida = (Int, Int, Int) -- Sal, Ingre, Temp

get1st (a,_,_) = a

get3rd (_,_,a) = a

--------------------
--INICIA ANTON EGO--
--------------------

sal :: [Comida] ->Int
sal comidas = sum (map (get1st) comidas)

aprobadoAE :: Resto -> Bool
aprobadoAE resto = sal (get3rd resto) <30

listaAprobados resto = map (aprobadoAE) resto

aprobadoFinal resto = zip (listaAprobados resto) resto

aprobadoFinalFinalAE resto = map (snd) (filter ((==True).fst) (aprobadoFinal resto))

-------------------
---FIN ANTON EGO---
-------------------

------------------------
--INICIA COLLETE TATOU--
------------------------
temp :: [Comida]->Bool
temp comida = all (>40) (map (get3rd) comida) 

cantSal :: [Comida]->Bool
cantSal comida = all (>10) (map (get1st) comida)

condicion :: Resto -> Bool 
condicion resto = (temp (get3rd resto)) && (cantSal (get3rd resto))

aprobadoCT resto = map (condicion) resto 

aprobadoFinalCT resto = zip (aprobadoCT resto) resto

aprobadoFinalFinalCT resto = map (snd) (filter ((==True).fst) (aprobadoFinalCT resto))

-----------------------
---FIN COLLETE TATOU---
-----------------------