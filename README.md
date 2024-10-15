This branch is a work in progress. I am rebasing my code while adding new features. Refer to the Main branch for a
stable version or try this Karma branch if you like. The configuration script has not been updated yet. Once this
version is complete and tested throughly, it will be merged into the main branch.
									October 15, 2024
	 
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

**/dt: [Physical|Magical|Breath]**: Defines type of damage taken gear to equip  
**/kite**: Turns on/off equipping movement gear.  
**/acc [all|#'s|names|-code]**: Indicates which slots from the accuracy set should be equipped.  
**/nac [all|#'s|names|-code]**: Turns off the specified accuracy slots.  
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
**/maxsong "name"**: Determines the highest level song player can perform that has either the
common root name (see /maxsong) or the common buff name (e.g., paeon when
performing one of the army paeon songs.)  
**/maxspell "name"**: Determines the highest level spell player can cast that has the common root
name (e.g., Cure gets you Cure III if you're 75 and /whm.)  
**/petfood [name]**: Equips either named petfood or determines best pet food and equips that.  
**/slot name|position gear**: Equips the passed piece of gear and locks the equipment slot.  
**/tank**: Turns on/off equipping TP tank gear. Only available to: PLD,NIN,DRK,WAR,THF,RDM,RUN.  
**/unlock: [all|#'s|names]**: Unlock equipment slot(s) so luashitacast can change item.  
**/wsdistance [#]**: Turns on/off distance check on melee weapon skill. Also supports changing the
the default distance from 4.7 yalms to whatever you want.  
**/wswap**: Turns on/off whether automatic gear swapping allowed. All jobs except SMN and BLM
where this is always true.  

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
