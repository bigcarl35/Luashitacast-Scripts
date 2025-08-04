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

**Please Note: Main** is now back to the current branch. The Karma branch has an older version. Please pull only  
from the main.  

**Boxcar** is a new experimental branch that will hold the reorganized version of luashitacast V2.0. Refer  
to "Planned for 2.0" in the Documentation subdirectory for more details.  

## Supported commands

The following commands are supported by this implementation of luashitacast.

**/GC**: Performs a gear check. Has to be run for Luashitacast to work.  
**/dt: [Physical|Magical|Breath]**: Defines type of damage taken gear to equip  
**/kite**: Turns on/off equipping movement gear.  
**/acc [?|stage #]**: Indicates the progressive accuracy stage to equipped.  
**/equipit** or **/ei "code|name" [1|2]**: Equips specified ring (coded or actual name) in the specified slot.
Turns off /GSWAP.  
**/debug**: Turns on/off displaying of all gear being equipped.  
**/dt [P|M|B]**: Indicates type of damage to mitigate: P-Physical, M-Magical, or B-Breath.  
**/eva**: Turns on/off equipping evasion gear.  
**/gearset** or **/gs: name [on]** Equips specified gear set and turns off /gswap unless on specified 
    HELM|DIG|CLAM|FISH for appropriate gather set.  
    AL|BN|CL|CO|GS|LT|SM|WW for appropriate crafting set.  
**/gswap**: Turns on/off automatic gear swapping. Useful when doing something not supported.  
**/help [command]**: List description of "command" or lists all commands if no parameter specified.  
**/idle**: Turns on/off whether travel gear will equip when player idle. Only available 
to PLD,NIN,DRK,WAR,THF,RUN,RDM.  
**/kite**: Equips movement and evasion gear for kiting.  
**/lock: [all|#'s|names]**: Lock equipment slot(s) so luashitacast can't change item.  
**/unlock [all,#'s|names]**: Unlocks the specified equipment slots so luashitacast can use.  
**/macc**: Enabled/Disables equipping magical accuracy gear.  
**/maxsong "name"**: Determines the highest level song player can perform that has either the
common root name (see /maxsong) or the common buff name (e.g., paeon when
performing one of the army paeon songs.)  
**/maxspell "name"**: Determines the highest level spell player can cast that has the common root
name (e.g., Cure gets you Cure III if you're 75 and /whm.)  
**/racc [?|stage #]**: Indicates the progressive ranged accuracy stage to equipped.  
**/rc**: Displays list of all regions and who controls them.  
**/petfood [name]**: Equips the named pet food.  
**/slot name|position gear**: Equips the passed piece of gear and locks the equipment slot.  
**/tank**: Turns on/off equipping TP tank gear. Only available to: PLD,NIN,DRK,WAR,THF,RDM,RUN.  
**/unlock: [all|#'s|names]**: Unlock equipment slot(s) so luashitacast can change item.  
**/wsdistance [#]**: Turns on/off distance check on melee weapon skill. Also supports changing the
the default distance from 4.7 yalms to whatever you want.  
**/wswap**: Turns on/off whether automatic gear swapping allowed. All jobs except SMN and BLM
where this is always true.  
**/smg [gs=|slot=]** Displays details about all gear found in gear sets for either the specified gear set of slot.  
**/AJug**: Toggles on/off whether automatic jug pet routine should be enabled. BST only.  
**/DB: Norm|BPP|WSS**: Indicates which debuff reward should clear on pet. BST only.  
**/Horn**: Indicates a horn instrument is equipped. BRD only.  
**/sBP**: Toggles on/off whether Blood Pact message should be set to /p. SMN only.  
**/String**: Indicates a string instrument is equipped. BRD only.  
**/TH**: Toggles on/off equipping treasure hunter gear. THF only.   

## Useful luashitacast commands

**/lac addset "name"**: Stores the currently equipped gear into the job file either adding new or
replacing currently stored set by that name. Easy way to update your gear
sets. Make sure that capitalization of gear set matches stored name.  
**/lac disable**: Disables all equipment slots.  
**/lac enable**: Enables all equipment slots.  
**/lac list**: Lists all the stored gear sets.  
**/lac load**: Loads the luashitacast job file for the current job.  
**/lac reload**: Unloads and the loads the current luashitacast job definitions.  
**/lac unload**: Unloads the current luashitacast job definitions.  
