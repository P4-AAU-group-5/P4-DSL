device Light in living_room
device Heater in bedroom
sensor Temp in bedroom : int
sensor Motion in living_room : bool

rule morning_heat:
if Temp < 20 and time between 06:00..09:00
then Heater.on

rule night_light:
if time == 22:00
then Light.off

rule welcome_home:
if Motion == true and time between 17:00..23:00
then Light.on