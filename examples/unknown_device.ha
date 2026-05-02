sensor Temp in bedroom:int

rule test:
if Temp < 10
then Heater.on