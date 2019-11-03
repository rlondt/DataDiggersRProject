library(DataDiggersPackage)
#flog.threshold(DEBUG)
#startPreparation(workdir = "D:/datafiles2", dataframesToGlobalEnvironment = TRUE, rebuild = TRUE)
initializeDQScoringFramework()

## COMPLEETHEID
#1. Mogelijkheden voor kruistellingen 
addScoreToDQFramework(COMPLEETHEID, weging=3	, waarde = 3);
#2. Zijn alle onderdelen van VWT-data vertegenwoordigd 
addScoreToDQFramework(COMPLEETHEID, weging=3	, waarde = 1);
#3. Missing values analyse 
addScoreToDQFramework(COMPLEETHEID, weging=5, waarde=2)
#4. Heeft medewerker altijd een woonplaats 
addScoreToDQFramework(COMPLEETHEID, weging=4, waarde=2)
#5. Overcompleetheid, dataoverload 
#6. Komen alle features voor die nodig zijn om een analyse te doen 
addScoreToDQFramework(COMPLEETHEID, weging=5	, waarde = 3);

## CONSISTENTIE
#1. Komt elke ordernummer van het tijdschrijven voor in de workflowDF, en vice versa
addScoreToDQFramework(CONSISTENTIE, weging=3, waarde = 3);
addScoreToDQFramework(COMPLEETHEID, weging=3, waarde=1)
addScoreToDQFramework(CONSISTENTIE, waarde=4, weging=5)
#2.1. order zonder workflow
addScoreToDQFramework(CONSISTENTIE, weging=1	, waarde = 4);
#2.2. workflow zonder order
addScoreToDQFramework(CONSISTENTIE, weging=1	, waarde = 5);
#2.3. tijdschrijven zonder medewerker
addScoreToDQFramework(CONSISTENTIE, weging=1	, waarde = 2);
#2.4. medewerker zonder tijdschrijven
addScoreToDQFramework(CONSISTENTIE, weging=1	, waarde = 2);
#2.5. tijdschrijven zonder dienst
addScoreToDQFramework(CONSISTENTIE, weging=1	, waarde = 3);
#2.6. dienst zonder tijdschrijven
addScoreToDQFramework(CONSISTENTIE, weging=1	, waarde = 4);
#2.7. tijdschrijven zonder order
addScoreToDQFramework(CONSISTENTIE, weging=1	, waarde = 4);
#2.8. order zonder tijdschrijven
addScoreToDQFramework(CONSISTENTIE, weging=1	, waarde = 5);
#3. Is bij tijdschrijven, de begintijd altijd kleiner dan de eindtijd 
addScoreToDQFramework(CONSISTENTIE, weging=3, waarde=4)
#4. Bevinden de begintijd en eindtijd van het tijdschrijven zich binnen de begintijd en eindtijd van het rooster 
addScoreToDQFramework(CONSISTENTIE, weging=5	, waarde = 4);
#5. De relatie tussen rooster en medewerker en tijdschrijven en medewerker loopt "dubbel". Komen daar inconsistenties in voor  
addScoreToDQFramework(CONSISTENTIE, weging=5	, waarde = 3);
#6. Datumvelden als datum te behandelen Score 5
addScoreToDQFramework(CONSISTENTIE, weging=1	, waarde = 2);
#7. Zijn de order oplopend 
addScoreToDQFramework(CONSISTENTIE, weging=5	, waarde = 5);
#8. Naamgeving kolommen 
addScoreToDQFramework(CONSISTENTIE, weging=5	, waarde = 4);
#9. Controle postcode-plaatsnaam zowel voor persoon (kan niet!) als order (mogelijk? (Jeroen → Zie uitwerking)
addScoreToDQFramework(CONSISTENTIE, weging=3	, waarde = 4);
#10. Indien Ordertype leeg dan Opdrachttype start met NLS 
addScoreToDQFramework(CONSISTENTIE, weging=3	, waarde = 5);
#11. Postcode4(Orders) is een 4 cijferig getal 
addScoreToDQFramework(CONSISTENTIE, weging=5	, waarde = 4);

## UNICITEIT
#1. Geen dubbele records  pk + functioneel Alle dataframes
addScoreToDQFramework(UNICITEIT, weging=5	, waarde = 3);
addScoreToDQFramework(UNICITEIT, waarde=4.5, weging=3)
#2. Zijn er medewerkers die tegelijkertijd aan meerdere orders werken ? 
addScoreToDQFramework(UNICITEIT, weging=2	, waarde = 4);
addScoreToDQFramework(UNICITEIT, waarde=4.5, weging=1)
#3. Zijn er medewerkers die tegelijkertijd voor meerdere orders reizen? 
addScoreToDQFramework(UNICITEIT, weging=3	, waarde = 5);
addScoreToDQFramework(UNICITEIT, waarde=5, weging=1)
#4. Overlappen de workflowstappen (kan zijn hoor) 
addScoreToDQFramework(UNICITEIT, weging=3	, waarde = 4);

##VALIDITEIT
#1. Is er daarbinnen nog verschil tussen werk en reistijd? Hoe gaan we om met overwerk? 
addScoreToDQFramework(VALIDITEIT, weging=5	, waarde = 5);
#2. Vervallen orders waar wel geakkoordeerd is tijdgeschreven 
addScoreToDQFramework(VALIDITEIT, weging=3	, waarde = 4);
#3. Anomaly detection/outlier verklaring.. 
addScoreToDQFramework(VALIDITEIT, weging=4	, waarde = 3);
#5. Heeft elke order een plaats 
addScoreToDQFramework(VALIDITEIT, weging=5	, waarde = 4);
#6. Orders die binnen een periode 'x' (vb een minuut) zijn uitgevoerd 
addScoreToDQFramework(VALIDITEIT, weging=3	, waarde = 3);
#7. Verdeling van de reistijd over de medewerkers
addScoreToDQFramework(VALIDITEIT, weging=5	, waarde = 4);
#8. Aantal tijdschrijvers per order
addScoreToDQFramework(VALIDITEIT, weging=3	, waarde = 5);

##ACCURAATHEID
#1. Verdeling aantal orders per over de tijd uitgezet.(Heatmap timeline ) 
addScoreToDQFramework(ACCURAATHEID, weging=5	, waarde = 4);
#2. Spreiding / Standaarddeviatie (NEW 16-10-2019 JV) -> Gemiddelde afwijking tov normtijd
addScoreToDQFramework(ACCURAATHEID, weging=3	, waarde = 5);

plotDQ()
