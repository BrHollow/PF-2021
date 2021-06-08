sublist inicio fim xs = drop inicio' . take fim' $ xs
    where 
        size = length xs
        inicio'= if inicio<0 then inicio+size else inicio
        fim'= if fim<0 then fim +size else fim