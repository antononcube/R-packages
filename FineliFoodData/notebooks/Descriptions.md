---
title: "Descriptions"
output: html_notebook
---

Ravintoarvopaketti. N‰ringsdata paket. Nutrient data package.
Fineli. Elintarvikkeiden koostumustietokanta. Livsmedelsdatabas. Finnish food composition database. 
Versio. Version. Release. 20.0
 
Copyright 2015 Terveyden ja hyvinvoinnin laitos (THL). K‰yttˆehdot: Creative Commons Nime‰ 4.0 (CC-BY 4.0).
Copyright 2015 Institutet fˆr h‰lsa och v‰lf‰rd (THL). Licens: Creative Commons, Erk‰nnande  4.0 (CC-BY 4.0). 
Copyright 2015 National Institute for Health and Welfare (THL). Licence: Creative Commons Attribution 4.0 (CC-BY 4.0). 
 
THL ei vastaa tiedoissa mahdollisesti esiintyvist‰ virheist‰. Tietojen tulkinta ja niist‰ teht‰v‰t johtop‰‰tˆkset ovat k‰ytt‰j‰n omalla vastuulla.  
THL ansvarar inte fˆr eventuella fel i uppgifterna. Tolkningen av informationen och slutsatserna av dem ‰r helt pÂ anv‰ndarens eget ansvar.
THL is not responsible for any errors or inaccuracies found on this web site. THL is not liable for any consequences due to misinterpretations of the data.
 
Kaikki tiedot ovat ASCII-tekstitiedostoja. Alla filerna ‰r ASCII-textfiler. All files are ASCII-text files.
 
TIEDOSTOT. FILER. FILES.
component_value.csv Ravintoarvo. N‰ringsv‰rde. Component value.
food.csv Elintarvike. Livsmedel. Food.
component.csv Ravintotekij‰. N‰rings‰mne. Component. 
publication.csv Julkaisu. Publikation. Publication.
foodaddunit.csv Elintarvikkeen ruokamitat. Livsmedels enheter. Food additional units. 
contribfood.csv Elintarvikkeen reseptirivit. Recepts livsmedel. Recipe foods. 
foodname_FI.csv Elintarvikkeen nimet. (FI)
foodname_SV.csv Livsmedels namn. (SV)
foodname_EN.csv Food names. (EN)
foodname_TX.csv Tieteelliset nimet, vetenskapliga namn, scientific names (TX)
acqtype_FI.csv Luokitus:Tietol‰hde. 
acqtype_SV.csv Klassificering: Data k‰lla. 
acqtype_EN.csv Classification: Acquisition type. 
methtype_FI.csv Luokitus:Menetelm‰tyyppi. 
methtype_SV.csv Klassificering: Metod typ. 
methtype_EN.csv Classification: Method type. 
methind_FI.csv Luokitus:Analyysimenetelm‰. 
methind_SV.csv Klassificering: Analysmetod. 
methind_EN.csv Classification: Method of analysis (method headline). 
cmpclass_FI.csv Luokitus:Ravintotekij‰luokka. 
cmpclass_SV.csv Klassificering: N‰rings‰mnegrupp. 
cmpclass_EN.csv Classification: Component group. 
compunit_FI.csv Luokitus:Ravintotekij‰n yksikkˆ. 
compunit_SV.csv Klassificering: Enhet(n‰rings‰mne). 
compunit_EN.csv Classification: Unit (component). 
foodtype_FI.csv Luokitus:Elintarviketyyppi. 
foodtype_SV.csv Klassificering: Livsmedel typ. 
foodtype_EN.csv Classification: Food type. 
foodunit_FI.csv Luokitus:Elintarvikkeen yksikkˆ. 
foodunit_SV.csv Klassificering: Enhet(livsmedel). 
foodunit_EN.csv Classification: Unit (food). 
process_FI.csv Luokitus:Valmistustapa. 
process_SV.csv Klassificering: Livsmedelshantering. 
process_EN.csv Classification: Processing method. 
igclass_FI.csv Luokitus:Raaka-aineluokka. 
igclass_SV.csv Klassificering: RÂvara klass. 
igclass_EN.csv Classification: Ingredient class. 
fuclass_FI.csv Luokitus:Ruoank‰yttˆluokka. 
fuclass_SV.csv Klassificering: Livsmedel bruk klass. 
fuclass_EN.csv Classification: Food use class. 
specdiet_FI.csv Luokitus:Erityisruokavelio. 
specdiet_SV.csv Klassificering: Specialkost. 
specdiet_EN.csv Classification: Special diet. 
eufdname_FI.csv Luokitus:Ravintotekijˆiden nimet. 
eufdname_SV.csv Klassificering:N‰rings‰mnes namn. 
eufdname_EN.csv Classification:Component names. 
descript.txt Tietojen kuvaus (t‰m‰ tiedosto). Beskrivningen av data (denna fil). Data description (this file).

KENTTƒEROTIN. KOLUMN FR≈NSKILJARE. OUTPUT SEPARATOR.  ";"

TIETOTYYPIT. DATATYPER. DATATYPES.
Puuttuvan tiedon koodit. Koderna fˆr bristande data. Codes for missing data. 
   Numero, yleinen. Nummer, allm‰nt. Number, general. 
   Numero, ravintoarvo.  Nummer, n‰ringsv‰rde. Number, component value. 
   Teksti, yleinen. Text, allm‰n. Text, general. 
   Teksti, koodi. Text, kod. Text, code. 

YHTEYDET. RELATIONER. RELATIONS.
 Elintarvike, p‰‰avain. Livsmedel, prim‰rnyckel. Food, primary key. FOODID
 
    Valinnaisten luokitusten viiteavain. LUOKAN KOODI <<-- LUOKITUKSEN THSCODE
    Esimerkki: Raaka-aineluokka IGCLASS <<-- Raaka-aineluokka THSCODE
    Valinnaiset luokan ylempi taso tai luokan ylin taso liitet‰‰n samalla tavalla

    Valfria klassificeringarnas referensnyckel. KLASS KOD <<-- KLASSIFICERINGARNAS THSCODE
    Exempel: Ingrediens klass IGCLASS <<-- Ingrediens klass THSCODE
    ÷vre nivÂ av valfri klass eller ˆversta nivÂn av klass kombineras pÂ samma s‰tt

    Reference key of optional classifications CODE OF CLASS<<-- THSCODE OF CLASSIFICATION
    Example: Ingredient class IGCLASS <<-- Ingredient class THSCODE
    Higher level of optional class or highest level of class are joined in the same way

    Valinnainen. Valfri. Optional.  PUBLID <<-- Julkaisu. Publikation. Publication. PUBLID

 Ravintoarvo. N‰ringsv‰rde. Component value 
    Viiteavain. Referensnyckel. Reference key. FOODID <<-- food.FOODID
    Viiteavain. Referensnyckel. Reference key. EUFDNAME <<-- Component.EUFDNAME
    Valinnaisten luokitusten viiteavain LUOKAN KOODI <<-- LUOKITUKSEN THSCODE
    Esimerkki: Ravintoarvotyyppi VALTYPE <<-- valtype.THSCODE
    Valfria klassificeringarnas referensnyckel. KLASS KOD <<-- KLASSIFICERINGARNAS THSCODE
    Exempel: N‰ringsv‰rdes typ VALTYPE <<-- valtype.THSCODE
    Reference key of optional classifications CODE OF CLASS<<-- THSCODE OF CLASSIFICATION
    Example: Component value type VALTYPE <<-- valtype.THSCODE
    Valinnainen viiteavain. Valfri referensnyckel. Optional reference key. PUBLID <<-- publication.PUBLID

 Ravintotekij‰, p‰‰avain. N‰rings‰mne, primarnyckel. Component, primary key. EUFDNAME
    Valinnaisten luokitusten viiteavain LUOKAN KOODI <<-- LUOKITUKSEN THSCODE
    Esimerkki: Ravintotekij‰luokka CMPCLASS <<-- cmpclass.THSCODE
    Valinnaiset luokan ylempi taso tai luokan ylin taso liitet‰‰n samalla tavalla
    Valfria klassificeringarnas referensnyckel. KLASS KOD <<-- KLASSIFICERINGARNAS THSCODE
    Exempel: n‰rings‰mnegrupp CMPCLASS <<-- cmpclass.THSCODE
    ÷vre nivÂ av valfri klass eller ˆversta nivÂn av klass kombineras pÂ samma s‰tt
    Reference key of optional classifications CODE OF CLASS<<-- THSCODE OF CLASSIFICATION
    Example: Component group CMPCLASS <<-- cmpclass.THSCODE
    Higher level of optional class or highest level of class are joined in the same way

 Julkaisu, p‰‰avain. Publikation, prim‰rnyckel. Publication, primary key. PUBLID
    Valinnaisten luokitusten viiteavain LUOKAN KOODI <<-- LUOKITUKSEN THSCODE
    Esimerkki: Julkaisutyyppi REFTYPE <<-- reftype.THSCODE
    Valfria klassificeringarnas referensnyckel. KLASS KOD <<-- KLASSIFICERINGARNAS THSCODE
    Exempel: Publikations typ REFTYPE <<-- reftype.THSCODE
    Reference key of optional classifications CODE OF CLASS<<-- THSCODE OF CLASSIFICATION
    Example: Publication type REFTYPE <<-- reftype.THSCODE

 Elintarvikkeen ruokamitat. Livsmedels enheter. Food additional units. (FOOD ADDITIONAL UNIT)
    Viiteavain. Referensnyckel. Reference key. FOODID <<-- food.FOODID
    Viiteavain, yksikˆn koodi. Referensnyckel, enhet kod.  Reference key, unit code. FOODUNIT <<--  foodunit.THSCODE

 Elintarvikkeen erityisruokavaliot. Livsmedels specialkoster. Special diets of a food item. (SPECDIET)
    Viiteavain. Referensnyckel. Reference key. FOODID <<-- FOODID <<-- food.FOODID
    Viiteavain, erityisruokavalion koodi. Referensnyckel,  specialkost, kod.  Reference key, Special diet code. SPECDIET <<-- specdiet.THSCODE

 Elintarvikkeen resepti (rivit). Recepts livsmedel (rader). Recipe foods (rows). (CONTRIB FOOD)
    Elintarvike, jonka reseptiin rivit kuuluvat, viiteavain. Receptets huvudlivsmedel. Mainfood of recipe, reference key.. FOODID <<-- food.FOODID
    Reseptirivin elintarvike; viiteavain. Receptrads livsmedel, referensnyckel. Recipe row food, reference key. CONFDID <<--  food.FOODID
    Viiteavain Yksikˆn koodi. Referensnyckel, enhet. Unit, reference key. FOODUNIT <<-- foodunit.THSCODE

 Elintarvikkeen nimet (foodname_FI). Livsmedels namn (foodname_SV). Food names (foodname_EN)
 Elintarvikkeen tieteelliset nimet (foodname_TX). Livsmedels vetenskapliga namn (foodname_TX). Food names, scientific (foodname_TX)
    Viiteavain. Referensnyckel. Reference key. FOODID <<-- food.FOODID
 
 
component_value.csv
FOODID (Elintarvikkeen tunnus, numero. Livsmedelsnummer, nummer. Food id, number)
EUFDNAME (Ravintotekij‰n koodi, teksti. N‰ringsv‰rdenummer, nummer. Component id, number.)
BESTLOC (Ravintoarvo, numero. N‰ringsv‰rde, number. Component value, number.)
ACQTYPE (Tietol‰hteen koodi, teksti. Data k‰llas kod, text. Acquisition type code, text.
METHTYPE (Menetelm‰tyypin koodi, teksti. Metod typs kod,text. Method type code, text)
METHIND (Analyysimenetelm‰n koodi, teksti. Analysmetods kod, text. Method headline, text)
 
component.csv
EUFDNAME (Ravintotekij‰n koodi, teksti. N‰rings‰mneskod, text. Component code, text.)
COMPUNIT (Yksikˆn koodi, teksti. Enhets kod, text. Unit code, text)
CMPCLASS (Ravintotekij‰luokan koodi, teksti. N‰rings‰mnes typkod, text. Component group code, text.)
CMPCLASSP (Ravintotekij‰luokan koodi, luokan ylempi taso, teksti. N‰rings‰mnes typkod, ˆvre nivÂ,text. Component group code, higher level,text.)
 
food.csv
FOODID (Elintarvikkeen tunnus, numero. Livsmedelsnummer, nummer. Food id, number)
FOODNAME (Elintarvikkeen nimi, teksti. Livsmedels namn, text. Foodname, text.)
FOODTYPE (Elintarvikkeen tyypin koodi, teksti. Livsmedels typ, text. Food type code, text.)
PROCESS (Valmistustavan koodi, teksti. Livsmedelshanterings kod, text. Process type code, text.)
EDPORT (Syˆt‰v‰ osuus, prosentti. ƒtlig del, procent. Edible portion, percentage.)
IGCLASS (Raaka-aineluokan koodi, teksti. RÂvara klass kod, text. Ingredient class, text.)
IGCLASSP (Raaka-aineluokan koodi, luokan ylempi taso, teksti. RÂvara klass kod, ˆvre nivÂ, text. Ingredient class, upper level, text.)
FUCLASS (Ruoank‰yttˆluokan koodi, teksti. Livsmedel bruk class, text. Food use class, text.)
FUCLASSP (Ruoank‰yttˆluokan koodi, luokan ylempi taso, teksti. Livsmedel bruk class, ˆvre nivÂ, text. Food use class, higher level, text)
 
contribfood.csv
FOODID (Elintarvikkeen tunnus, numero. Livsmedelsnummer, nummer. Food id, number)
CONFDID (Reseptirivin elintarvikkeen tunnus, numero. Livsmedelsnummer (receptrad), nummer. Food id (recipe row), number)
AMOUNT (M‰‰r‰, numero. M‰ngd, number. Amount, number)
FOODUNIT (Yksikˆn koodi, teksti. Enhets kod, text. Unit code, text)
MASS (Massa (g), numero. Massa (g), nummer. Mass (g), number)
EVREMAIN (Haihtumisj‰‰nnˆs (%), numero. Avdunstningsrest (%), nummer. Remained after evaporation (%), number)
RECYEAR (Reseptin vuosi, teksti. Receptets Âr, text. Recipe year, text)
 
foodaddunit.csv
FOODID (Elintarvikkeen tunnus, numero. Livsmedelsnummer, nummer. Food id, number)
FOODUNIT (Yksikˆn koodi, teksti. Enhets kod, text. Unit code, text)
MASS (Massa (g), numero. Massa (g), nummer. Mass (g), number)
 
specdiet.csv
FOODID (Elintarvikkeen tunnus, numero. Livsmedelsnummer, nummer. Food id, number)
SPECDIET (Erityisruokavalion koodi, teksti., Specialkost,text. Special diet, text)
 
foodname_FI.csv
foodname_SV.csv
foodname_EN.csv
foodname_TX.csv
FOODID (Elintarvikkeen tunnus, numero. Livsmedelsnummer, nummer. Food id, number)
FOODNAME (Elintarvikkeen nimi), teksti
LANG (Kielen koodi, ISO639), teksti
 
publication.csv
PUBLID (Julkaisun tunnus, numero. Publikationsnummer, nummer. Publication id, number)
CITATION (Viite, teksti . Referens, text. Reference, text.)
REFTYPE (Julkaisutyyppi, teksti. Publikations typkod, text. Publication type code, text.
ACQTYPE (Tietol‰hteen koodi, teksti. Data k‰llas kod, text. Acquisition type code, text.
DOI (Document object identifier (DOI), text. Document object identifier (DOI), text. Document object identifier (DOI), text.)
 
LUOKITUKSET. KLASSIFICERINGAR. CLASSIFICATIONS
Samassa tiedostossa on luokituksen kaikki eri kieliversiot. Ota huomioon kielikoodi (LANG) tiedostojen yhdistelyss‰
Alla sprÂkversioner av en klassificering i en fil. Ta sprÂkkod (LANG) till h‰nsyn n‰r filer kopplas ihop.
All language versions of classifications in a single file. Take language code (LANG) into account when combining files.
THSCODE (Koodi, teksti. Kod, text. Code, text)
DESCRIPT (Kuvaus, teksti. Beskrivning,text. Description, text.)
LANG (Kielikoodi ISO639, teksti. SprÂkkod ISO639, text. Language code ISO639, text.)
