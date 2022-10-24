###############################################################################
# Hvaležni medved
#
# Pri tej nalogi bomo napisali nekaj funkcij, ki nam bodo v pomoč pri analizi
# literarnih besedil, kot je na primer koroška narodna pripovedka *Hvaležni
# medved*.
###############################################################################

test_text = """Gori nekje v gorah, ne ve se več, ali je bilo pri Macigoju ali
Naravniku, je šivala gospodinja v senci pod drevesom in zibala otroka. Naenkrat
prilomasti - pa prej ni ničesar opazila - medved in ji moli taco, v kateri je
tičal velik, debel trn. Žena se je prestrašila, a medved le milo in pohlevno
godrnja. Zato se žena ojunači in mu izdere trn iz tace. Mrcina kosmata pa zvrne
zibel, jo pobaše in oddide. Čez nekaj časa pa ji zopet prinese zibel, a zvhano
napolnjeno s sladkimi hruškami . Postavil jo je na tla pred začudeno mater in
odracal nazaj v goščavo. "Poglej no", se je razveselila mati, "kakšen hvaležen
medved. Zvrhano zibelko sladkih hrušk mi je prinesel za en sam izdrt trn"."""

###############################################################################
# 1) Sestavite funkcijo [find_words], ki vrne množico vseh besed, ki se
#    pojavijo v nizu in vsebujejo dan podniz.
#
# Namig: Pomagajte si z regex znakom za mejo [\b].
#
# >>> find_words(test_text, 'de')
# {'izdere', 'debel', 'oddide', 'začudeno'}
###############################################################################
import re
def find_words(niz, podniz):
    vzorec = "\\b\\w*" + podniz + r"\w*\b" #\b dva skupi ubistvu mesto kjer se zamenjata šrkovni in nečrkovni znak, če daš r"kar tu not piše se normalno tretira \n nima pomena, not pa lahk pišeš te stvari pa bo našlo"  na
    return set(re.findall(vzorec, niz)) #re.findall(f"\\W.*{podniz}.*\\W", niz)
    
print(find_words(test_text, 'de'))
            




###############################################################################
# 2) Sestavite funkcijo [find_prefix], ki vrne množico vseh besed, ki se
#    pojavijo v nizu in imajo dano predpono.
#
# >>> find_prefix(test_text, 'zi')
# {'zibala', 'zibel', 'zibelko'}
###############################################################################
def find_prefix(test_text, predpona):
    vzorec = "\\b" + predpona + r"\w+\b"
    return set(re.findall(vzorec, test_text))

print(find_prefix(test_text, "zi"))

###############################################################################
# 3) Sestavite funkcijo [find_suffix], ki vrne množico vseh besed, ki se
#    pojavijo v nizu in imajo dano pripono.
#
# >>> find_suffix(test_text, 'la')
# {'zibala', 'razveselila', 'prestrašila', 'šivala', 'opazila', 'tla'}
###############################################################################
def find_suffix(test_text, pripona):
    vzorec = f"\\b\\w+{pripona}\\b"
    return set(re.findall(vzorec, test_text))

print(find_suffix(test_text, "la"))

###############################################################################
# 4) Sestavite funkcijo [double_letters], ki sprejme niz in vrne množico vseh
#    besed, ki vsebujejo podvojene črke.
#
# >>> double_letters('A volunteer is worth twenty pressed men.')
# {'volunteer', 'pressed'}
###############################################################################

def double_letters(niz):
    vzorec = r"\b\w*(\w)\1\w*"
    return set(re.findall(vzorec, test_text))
    
print(double_letters('A volunteer is worth twenty pressed men.'))








#pattern = r"\b(\w*(\w)\2\w)\b"