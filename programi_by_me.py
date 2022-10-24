############################
#1. REGULARNI IZRAZI


#page source odpres html cmd + a - select all,

"zunanje_datoteke/podatki_o_filmih.html"
with open("zunanje_datoteke/podatki_o_filmih.html") as f:
    vsebina = f.read()

#ločimo iskanje in prikaz podatkov
def poišči_vse_pojavitve(niz: str, vzorec): #očitno tk določiš tip
    začetek = 0
    while True:
        začetek = niz.find(vzorec, začetek + 1) #find najde sam index prve pojavitve Blonde v nizu. startswith with pove a se nek niz, find ma neobvezn argument od kje naprej iskat
            # niz.find se izvaja tud če je enačaj
        if začetek == -1:
            break
        yield začetek #tk nardiš generator ko ti to vse vn da

#walrus operator spr := vrednost
#npr. : 3 + (x:=4) ---> vrne 7 in nastavi x na 4

def vse_pojavitve(niz,vzorec, kontekst=5):
    for začetek in poišči_vse_pojavitve(niz, vzorec):
        print(niz[začetek - kontekst:začetek + len(vzorec)+ kontekst])
        print(kontekst * " " + len(vzorec) * "^")

print(vse_pojavitve(vsebina, "Blonde"))


#...

"abc" "efg"
#lahk napišeš dva KONKRETNA NIZA, ki se pol združita