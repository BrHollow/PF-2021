--tentei fazer essa separando os elementos da lista, somando com sum e depois criando outra lista mas deu errado até demais
listacc [] = []
listacc u = (listacc (init u))++[sum u]