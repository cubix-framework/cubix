def rd(lista):
    lista2 = []
    if lista:
        for item in lista:
            if item not in lista2:
                lista2.append(item)
    else:
        return lista
    return lista2
print(rd([1,2,3,3]))
