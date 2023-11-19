# Luashitacast-Scripts
This repository contains my Luashitacast scripts that I have tailored to work on the FFXI private server HorizonXI

	This implementation of a Luashitacast script is based on the code that GetAwayCoxn has published in
	GitHub. I have done a fair amount of rewriting as well as dropping features so that the features are 
 	appropriate to the current expansion found on HorizonXI private server. I have also added new code 
  	to support functuality that my friends and I wanted. You can use this code "as is" or change as you 
   	desire for your own needs.
	
	Player: Paiine
	Date:   October 2023
	Server: HorizonXI (https://horizonxi.com/)

## Supported commands

The following commands are supported by this implementation of luashitacast.

**/gswap**: Turns on/off automatic gear swapping. Useful when doing something not supported.  
**/wsdistance [#]**: Turns on/off distance check on melee weapon skill. Also supports changing the
the default distance from 4.7 yalms to whatever you want.  
**/dt**: Turns on/off automatic equipping of damage taken gear. Type depends on the setting of dt_type.  
**/dt_type [Physical|Magical|Breath]**: Defines type of damage taken gear to equip when /DT turned on.  
**/kite**: Turns on/off equipping movement gear.  
**/acc**: Turns on/off equipping accuracy gear. This includes both physical and magical.  
**/eva**: Turns on/off equipping evasion gear.  
**/wswap**: Turns on/off whether weapon swapping is permitted. Useful when casting spells.  
**/region**: Swaps between *owned/not* owned indicating if player's nation controls the
area from the last conquest tally. Used with some conditional gear.  
**/petfood [name]**: Equips either named petfood or determines best pet food and equips that.  
**/gearset "name" [on]**: Equips the specified gear set and turns off automatic gears swapping if 
optional parameter isn't included.  
**/craftset [AL|BN|CL|CO|GS|LT|SM|WW]**: Equips the crafting gear set specified and turns off gear
swapping. AL - Alchemy, BN - Bonecraft, CL - Clothcraft, CO - Cooking, GS - Goldsmithing, 
LT - Leathercraft, SM - Smithing, WW - Woodworking.  
**/gatherset [HELM|DIG|CLAM]**: Equips the gathering set specified and turns off gear swapping.
HELM - Harvest, Excavation, Logging, Mining, DIG - Digging, CLAM - Clamming.  
**/fishset**: Equips the fishing gear and turns off automatic gear swapping.  
**/maxspell "name"**: Determines the highest level spell player can cast that has the common root
name (e.g., Cure gets you Cure III if you're 75 and /whm.)  
**/maxsong "name"**: Determines the highest level song player can perform that has either the
common root name (see /maxsong) or the common buff name (e.g., paeon when
performing one of the army paeon songs.)  
**/doring "code|name" [1|2]**: Equips specified ring (coded or actual name) in the specified slot.
Turns of /GSWAP.  
**/dowep "code|name"**: Equips specified weapon (coded or actual name) and turns off /GSWAP.
**/th**: Turns on/off equipping treasure hunter gear.  
**/help [command]**: List description of "command" or lists all commands if no parameter specified.  

## Useful luashitacast commands

**/lac disable**: Disables all equipment slots.  
**/lac enable**: Enables all equipment slots.  
**/lac load**: Loads the luashitacast job file for the current job.  
**/lac unload**: Unloads the current luashitacast job definitions.  
**/lac reload**: Unloads and the loads the current luashitacast job definitions.  
**/lac addset "name"**: Stores the currently equipped gear into the job file either adding new or
replacing currently stored set by that name. Easy way to update your gear
sets. Make sure that capitalization of gear set matches stored name.  
**/lac list**: Lists all the stored gear sets.  
