type Resto = (String, Int, [Comida])
type Comida = (Int, Int, Int)

--restaurantesAprobadosAE :: String -> Int -> [Comida] -> String
--restaurantesAprobadosAE nombre mozos comidas = "El restaurant " ++ nombre ++ " tiene " ++ mozos ++ " mozos y " ++ sum (map (fst) comidas)++ "sal en total"

get1st (a,_,_) = a

get3rd (_,_,a) = a

sal :: [Comida] ->Int
sal comidas = sum (map (get1st) comidas)

aprobadoAE :: Resto -> Bool
aprobadoAE resto = sal (get3rd resto) <30

listaAprobados resto = map (aprobadoAE) resto

aprobadoFinal resto = zip (listaAprobados resto) resto

aprobadoFinalFinal resto = map (snd) (filter ((==True).fst) (aprobadoFinal resto))