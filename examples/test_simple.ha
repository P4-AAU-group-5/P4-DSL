device Heater in bedroom
sensor Temp in bedroom : int

rule heat_on:
if Temp < 20
then Heater.on