device Light in living_room

sensor Motion in living_room:bool

rule test:
if Motion < 5
then Light.on
