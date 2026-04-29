device Light in living_room
sensor Temp in bedroom : int

rule test:
if Temp < 10
then Light.on
