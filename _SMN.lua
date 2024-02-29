local profile = {};
gcinclude = gFunc.LoadFile('common\\gcinclude.lua');

--[[
	This file contains all the gear sets associated with the SMN job. While it isn't outside of the realm of 
	possibility that the subjob might be able to use gear sets too, that is not the emphasis of this program. 
	It is tailored to handle all the aspects of SMN. If you desire a gear set change to strengthen an ability 
	from your subjob that is not supported by this program, you probably will have to make a custom gear set 
	and use the /gearset command to use it.
--]]

local sets = {
--[[
	The gear sets are usually defined by a pair of sets: the "main" one and an associated "conditional" set. The
	"main" is loaded when appropriate and the conditional is processed to see if any of the entries should be
	equipped too. ("Conditional" entries consist of gear that need to meet certain conditions before they will be
	equipped.) "main" sets contain your standard gear slot='gear piece' combinations and the "conditional" entries
	of the piece of gear, a description of the condition, the slot the piece equips into, the minimum level the 
	player must be to equip the piece, what jobs can equip the piece, and the conditional code with potentially
	associated information needed to determine if you can wear the piece. (All conditional gear's definitions
	can be found in "Conditional gear master list.txt found in ../common as well as some user-defined conditionals
	known to work. Just copy the entry from that file into the appropriate "conditional" set.
	
	It is recommended that "main" sets not include any gear found in the top line of your equipment grid (main hand,
	off hand, ranged weapon, ammo). Doing so will mean that TP will be reset to 0 whenever gear is changed which can
	be very frustrating. Further, any time you do change a weapon, it will convert back to what was defined in a set.
	Believe me, it's no fun	fighting Luashitacast!

	Also, not all sets need to be defined. There is nothing wrong with leaving a set "empty", but don't delete any
	of the sets. All the ones listed here (except for any custom sets) are expected to exist by Luashitacast.
	
	*** Note ***
	Unlike when summoner is used as a subjob, /bst's pets are charmed at the max level of your BST or the level
	of your SMN, whichever is lower. That means you can charm higher level mobs than you would expect with /bst.
	Just note though that you can't have two pets, so if you have charmed a pet with /bst, you can't summon your
	avatar and visa versa.
--]]

--[[
	Unlike most jobs, summoner's emphasis is fighting with your avatar. For consistency with other
	jobs, ['Accuracy'] is for the player's accuracy and ['Pet_Accuracy'] is for the pet. I originally
	implemented the two combined, but it is now separated. The same is for ['Evasion']/['Pet Evasion'].
	['Macc'] is for the player since pet Macc is applied during a blood pact and ['TP'] is still 
	combined though and emphasizes the pet since summoner's are bad fighters.
--]]

	['TP'] = {
        Head  = { 'Austere Hat', 'Shep. Bonnet', 'Silver Hairpin' },
		Neck  = { 'Star Necklace', 'Spirit Torque', 'Justice Badge' },
		Ears  = { 'Loquac. Earring', 'Bat Earring', 'Black Earring', 'Onyx Earring' },
        Body  = { 'Vermillion Cloak//CARBY','Austere Robe', 'Seer\'s Tunic', 'Angler\'s Tunica' }, 
        Hands = { 'Carbuncle Mitts//CARBY', 'Errant Cuffs', 'Carbuncle Mitts' },
		Rings = { 'Evoker\'s Ring', 'Tamas Ring', 'Ether Ring', 'Astral Ring', 'Astral Ring' },
        Back  = { 'Blue Cape', 'White Cape' },
        Waist = { 'Hierarch Belt', 'Powerful Rope', 'Friar\'s Rope' },
        Legs  = { 'Evoker\'s Spats', 'Shep. Hose', 'Fisherman\'s Hose' }, 
        Feet  = { 'Summoner\'s Pgch.', 'Mannequin Pumps', 'Seer\'s Pumps', 'Waders'},
    },
	['TP_Conditional'] = {
		{'Uggalepih Pendant','Equip at night','Neck',70,'ALL','TIME','Nighttime'},
		{'Fenrir\'s Torque','Equip during day','Neck',70,'ALL','TIME','Daytime'},	
	},
	
--[[
	While it is concievable that other pet jobs might fight w/o a pet, it's unusual
	for a smn to fight alone. What's more likely is that your pet died. The TP_No_Pet
	gearset is if you want to change gear in that specific situation.
--]]
	
	['TP_No_Pet'] = {
		Body  = { 'Vermillion Cloak', 'Austere Robe', 'Seer\'s Tunic', 'Angler\'s Tunica' },
	},
	['TP_No_Pet_Conditional'] = {
		{'Uggalepih Pendant','Equip at night','Neck',70,'ALL','TIME','Nighttime'},
		{'Fenrir\'s Torque','Equip during day','Neck',70,'ALL','TIME','Daytime'},	
	},

--[[
	If an accuracy emphasis is desired, the following set will replace the gear appropriately.
	Remember that DEX converts to accuracy: for every 1 point of DEX you get 0.75 points
	of accuracy.
--]]
	
	['Accuracy'] = {
        Head  = 'Optical Hat',
		Neck  = 'Peacock Amulet',
		Body  = 'Austere Robe',			-- Needed to offset the "possible" Vermillion Cloak		
		Hands = 'Battle Gloves',
		Rings = { 'Toreador\'s Ring', 'Jaeger Ring', 'Balance Ring', 'Bastokan Ring' },
		Waist = { 'Life Belt', 'Tilt Belt' },
    },
	['Accuracy_Conditional'] = {
	},

	['Pet_Accuracy'] = {
        Head  = 'Shep. Bonnet',
		Ears  = 'Beastly Earring',
		Body  = 'Austere Robe',			-- Needed to offset the "possible" Vermillion Cloak	
		Legs  = 'Evoker\'s Spats',
    },
	['Pet_Accuracy_Conditional'] = {
	},
	
--[[
	If evasion wanted, equip evasion gear. Remember that AGI converts to evasion: for every
	2 points of AGI you get 1 point of evasion
--]]
	
	['Evasion'] = {
		Head  = { 'Optical Hat', 'Empress Hairpin' },
		Ears  = { 'Bat Earring//BLIND', 'Ethereal Earring', 'Genin Earring//SJNIN', 'Drone Earring' },
		Neck  =  'Spirit Torque',
		Body  = 'Austere Robe',			-- Needed to offset the "possible" Vermillion Cloak	
		Hands = 'Battle Gloves',
		Legs  = 'Evoker\'s Spats',	
    },
	['Evasion_Conditional'] = {
	},
	
	['Pet_Evasion'] = {
		Legs  = 'Shep. Hose',
    },
	['Pet_Evasion_Conditional'] = {
	},
	
--[[
	The Idle_Regen and Idle_Refresh gear sets are used to restore a player's HP or MP that	goes below
	a set percentage (defined in gcinclude.lua, but can be overriden in profile.OnLoad 	function).
--]]
	
	['Idle_With_Pet'] = {
        Head  = { 'Austere Hat', 'Shep. Bonnet', 'Silver Hairpin' },
		Body  = { 'Vermillion Cloak//CARBY', 'Austere Robe', 'Seer\'s Tunic', 'Angler\'s Tunica' },
        Hands = 'Carbuncle Mitts',
        Rings = 'Evoker\'s Ring',
    },
	['Idle_With_Pet_Conditional'] = {			-- Conjurer's ring seems the only possibility
	},
	
--[[
	The Idle_Regen and Idle_Refresh gear sets replace the normal Idle set when the player's HP or MP
	go below a set percentage (defined in gcinclude.lua, but can be overriden in profile.OnLoad function).
--]]
	
	['Idle_Regen'] = {
	},
	['Idle_Regen_Conditional'] = {
	},
	
	['Idle_Refresh'] = {
        Body = 'Vermillion Cloak',
	},
	['Idle_Refresh_Conditional'] = {
	},
	
--[[
	When you are resting (kneeling down), your HP 'Resting' set will be equipped. If your subjob
	uses MP and your MP is below the set threshhold (defined by gcinclude.settings.RefreshGearMP), 
	your MP 'Resting_Refresh' gear set will be equipped. Regardless of which set is equipped, 
	assuming that your subjob uses magic, you have a Dark/Pluto staff accessible, weapon swapping 
	is enabled (/wswap), and your MP is not at maximum, the Dark/Pluto staff will automatically be 
	equipped.
--]]
	
	['Resting_Regen'] = {
        Waist = 'Hierarch Belt',
    },
	['Resting_Conditional'] = {
	},
	
	['Resting_Refresh'] = {
		Main  = { 'Dark Staff', 'Kukulcan\'s Staff', 'Pilgrim\'s Wand' },
        Body  = { 'Vermillion Cloak', 'Seer\'s Tunic' },
		Waist = 'Hierarch Belt',
	},
	['Resting_Refresh_Conditional'] = {
	},
	
	-- If you have any Spell Interruption Rate down gear, put them into the "SIR" gear set.
	-- This gear set is equipped in the HandleMidcast function that all spells go through.
	['SIR'] = {
	},
	['SIR_Conditional'] = {
	},
	
	-- Blood pacts go through a simulated process that mimics spell casting. The precast
	-- happens when the blood pack is invoked (either rage or ward), loading the 'BP'
	-- gear set. You want gear that has Blood Pact Ability Delay, Blood Pact Recast, and
	-- avatar perpetuation cost abilities defined here. (Once summoning skill is working
	-- on HorizonXI, that is also a desirable attribute.) The midcast happens when the
	-- actual blood pact goes off.
	['BP'] = {
        Head  = 'Austere Hat',
        Neck = 'Smn. Torque',
        Body  = 'Austere Robe',
        Hands = 'Carbuncle Mitts',
        Rings = 'Evoker\'s Ring',
		Feet = 'Summoner\'s Pgch.',
    },
	['BP_Conditional'] = {
	},
	
--[[
	Rage blood pacts are devided by type: physical, magical, summoning skill, accuracy, 
	and hybrid. (Ward blood pacts do not have this type of distiction.) Each blood pact 
	though is of a fixed type and can be looked up. The following gear sets named
	SmnXXX where XXX is the type define the gear to be equipped when the blood pact
	goes off. Look to the specific gear set type for what gear stats are wanted.
--]]
	
	-- Physical rage blood pact: pet attack, pet accuracy, pet critical hit, blood pact 
	-- physical damage
	['SmnPhysical'] = {
        Head  = 'Shep. Bonnet',
		Ears  = 'Beastly Earring',
		Body  = 'Austere Robe',			-- Needed to offset the "possible" Vermillion Cloak	
		Legs  = 'Evoker\'s Spats',
    },
	['SmnPhysical_Conditional'] = {
	},

	-- Magical rage blood pact: pet magic attack burst, pet magical attack, pet magical
	-- accuracy, and blood Pact magical damage
	['SmnMagical'] = {
	    Head = 'Shep. Bonnet',
    },
	['SmnMagical_Conditional'] = {
	},

	-- Summoning skill rage blood pact. 
	['SmnSkill'] = {
        Head = 'Austere Hat',
        Neck = 'Smn. Torque',
        Rings = 'Evoker\'s Ring',
    },
	['SmnSkill'] = {
	},
	
	-- Accuracy blood pact: pet accuracy, pet magic accuracy
    ['SmnAccuracy'] = {
        Head  = 'Shep. Bonnet',
		Ears  = 'Beastly Earring',
		Body  = 'Austere Robe',			-- Needed to offset the "possible" Vermillion Cloak	
		Legs  = 'Evoker\'s Spats',
    },
	['SmnAccuracy_Conditional'] = {
	},
	
	-- Hybrid blood pact: 2x physical and 1x magical
    ['SmnHybrid'] = {
        Head  = 'Shep. Bonnet',
		Ears  = 'Beastly Earring',
		Body  = 'Austere Robe',			-- Needed to offset the "possible" Vermillion Cloak	
		Legs  = 'Evoker\'s Spats',
    },
	['SmnHybrid_Conditional'] = {
	},
	
--[[
	Start weapons are where you define what you want the first row of equipment to look like when you
	either log in as a SMN or you switch your main job to SMN. 
--]]

	['Start_Weapons'] = {
	    Main = { 'Light Staff', 'Kukulcan\'s Staff', 'Solid Wand', 'Yew Wand' },
		Ammo = { 'Fortune Egg' },
 	},
	['Start_Weapons_Conditional'] = {
	},
	
--[[
	What do you want to wear around town? You can define a full set or just an item or two, it is up to you.
	(Please note that a nation's aketon is considered conditional gear, so no need to place here unless you
	want the aketon equipped regardless if it is your home nation's city or not. Due to some of the 
	complexities of lua, the Town_Conditional set is found in the gcinclude.lua file.)
--]]
	
	['Town'] = {
        Head = 'Lilac Corsage',
		Body = 'Austere Robe',
    },
	
--[[
	Damage reduction gear depends on the type of damage. The most common is physical, but there's times when
	you'll want to reduce magic damage or breath damage. The three gear sets are defined below. The correct
	one will be equipped depending on how DT is set. Please consider not including gear that doesn't have 
	any damage taken property so other wanted stats can shine through.
--]]

	['DT_Physical'] = {
	},
	['DT_Physical_Conditional'] = {
	},
	
	['DT_Magical'] = {
        Ears  = 'Coral Earring',
    },
	['DT_Magical_Conditional'] = {
	},
	
	['DT_Breath'] = { 
	},
	['DT_Breath_Conditional'] = {
	},	
	
--[[
	Magic accuracy gear
--]]
	
	['Macc'] = {							-- This is player magical accuracy/attack, pet macc/matt goes in SmnAccuracy
		Rings = 'Tamas Ring',
    },
	['Macc_Conditional'] = {
	},
	
--[[
	Magic Attack Bonus (MAB) is used for more than just spells, so it is broken out
--]]

	['MAB'] = {
		Neck = 'Uggalepih Pendant//MP.SUB.50P',
	},
	['MAB_Conditional'] = {
	},
	
--[[
	Preshot is the first stage of when a ranged shot is being performed. This is where you place any 
	Ranged Accuracy or Ranged Attack Speed gear. 
--]]

	['Preshot'] = {
        Head  = 'Optical Hat',
		Neck  = 'Peacock Amulet',
		Body  = 'Austere Robe',
        Rings = { 'Jaeger Ring', 'Beetle Ring +1', 'Beetle Ring +1' },
		},
	['Preshot_Conditional'] = {
	},
	
--[[
	Midshot is the second stage of a ranged shot. This is where you place Ranged Attack or Ranged 
	Damage gear
--]]

	['Midshot'] = {
    },
	['Midshot_Conditional'] = {
	},
	
--[[
	Spells are a bit different. Each type of spell can have it's own enhancement gear as well as 
	stat based gear. (In some cases individual spells have special entries.) These sets do not 
	include elemental gear which is	dependent on day/weather/weapon skill.

	The first stage is Precast. This is where you place any Fast Cast, cast time reduction, and 
	quick cast gear 
--]]

	['Precast'] = {	
		Ears = { 'Loquac. Earring' },
	},
	['Precast_Conditional'] = {
	},
	
--[[
	The second stage is Midcast. This is where you'll want to equip magic attack, or magic enhancing 
	gear. (Magic Attack Bonus also happens here, but is broken out into it's own gear set. See MAB.)
--]]	

	['Midcast'] = {
	},
	['Midcast_Conditional'] = {
	},

--[[
	Further, there is a break out for each type of spell. I've included a comment on the type of attributes
	the piece of gear should have. While the spell might have other attributes than those listed, the ones I have
	listed have gear that a SMN or anyone can wear.
--]]

	-- Healing: Healing Magic Skill, cure potency. A few pieces that affect healing magic skill for either a SMN
	-- or "any jobs" exist. Also, gear with MND bonuses will boost cure spell's potency, but MND  gear is 
	-- automatically equipped prior to the Healing set being equipped in the HandleMidcast function. There's no 
	-- need to include MND gear here. As to items that add cure potency directly there are a few pieces both for
	-- smn and "all jobs". So, include healing magic skill items and cure potency items here.
	['Healing'] = {
    },
	['Healing_Conditional'] = {
	},
	
	-- Dark: Dark Magic Skill. Currently only gear equippable by any job gives is applicable here. There's no gear that's 
	-- specific for SMN that gives any dark magic skill.
	['Dark'] = {
	},
	['Dark_Conditional'] = {
	},
	
	-- Divine: Divine Magic Skill.
	['Divine'] = {
	},
	['Divine_Conditional'] = {
	},
	
	-- Enfeebling: Enfeebling Magic Skill.
	['Enfeebling'] = {
	},
	['Enfeebling_Conditional'] = {
	},
	
	-- Enhancing: There is no gear that a SMN can wear to enhance any magic spell. Leave the Enhancing gear sets empty.
	['Enhancing'] = {
	},
	['Enhancing_Conditional'] = {
	},
	
	-- Elemental: Elemental Magic Skill. Note: don't include elemental staves or elemental obis/gorgets here,
	-- that is done automatically in the HandlePrecast/HandleMidcast functions.
	['Elemental'] = {
	},
	['Elemental_Conditional'] = {
	},
	
	-- Ninjitsu: There is no gear that a SMN can wear to add Ninjitsu skill. Leave the following two
	-- gear sets empty.
	['Ninjitsu'] = {
	},
	['Ninjitsu_Conditional'] = {
	},
	
	-- Summoning: Summoning Magic Skill, Avatar Perpetuation Cost, Blood Pact Ability Delay. Please 
	-- note that currently on HorizonXI summoning skill is ignored. Any gear piece that only gives 
	-- summoning skill will be commented out
	['Summoning'] = {
		Head  = 'Austere hat',
        Neck = 'Smn. Torque',
		Body  = 'Austere Robe',
		Hands = 'Carbuncle Mitts',
        Rings = 'Evoker\'s Ring',
    },
	['Summoning_Conditional'] = {
	},
	
--[[
	Next is stat-based gear, (in this case intelligence or mind)
--]]

	['INT'] = {
        Hands = { 'Errant Cuffs', 'Seer\'s Mitts' },
        Rings = { 'Tamas Ring', 'Windurstian Ring' },
        Waist = 'Mrc.Cpt. Belt',
        Legs  = 'Seer\'s Slacks',
        Feet  = 'Mannequin Pumps',
    },
	['INT_Conditional'] = {
	},
	
	['MND'] = {
        Neck  = 'Justice Badge',
        Body  = 'Wonder Kaftan',
        Hands = 'Seer\'s Mitts',
		Rings = { 'Tamas Ring', 'Tranquility Ring', 'San d\'Orian Ring' },
        Back  = 'White Cape',
        Waist = { 'Mrc.Cpt. Belt', 'Friar\'s Rope' },
        Legs  = 'Wonder Braccae',
        Feet  = { 'Mannequin Pumps', 'Seer\'s Pumps' },
    },
	['MND_Conditional'] = {
	},
	
--[[
	And some spells are special cases, so they have individual gears sets.
--]]
	
	-- Stoneskin: Stoneskin Enhancement, Mind, and Enhancing Magic Skill. Mind is 3x more important than enhancing
	-- Magic Skill. The only piece of gear a SMN can wear to enhance stoneskin is a Stone Gorget. There's no gear
	-- that a SMN (or any job) can wear to enhance magic. Don't include mind gear here, that's already equipped.
	-- This gear set has no effect on Titan's Earthen Ward blood pact.
	['Stoneskin'] = {
	},	
	['Stoneskin_Conditional'] = {
	},
	
	-- Drain: Drain Enhancement, Dark Magic Skill, Dark Magic Accuracy. Currently no gear equippable by a
	-- SMN enhances Drain. Drain is part of Dark Magic, so Potency which is based on dark magic skill will
	-- already be loaded in HandleMidcast function and need not be repeated here. No current gear supports
	-- dark magic accuracy for any job. Magic attack bonus and magic critical hit have no effect on potency.
	-- Leave the two Drain gear sets empty.
	['Drain'] = {
    },
	['Drain_Conditional'] = {
	},
	
	-- Aspir: Aspir Enhancement, Dark Magic Skill, Dark Magic Accuracy. Currently no gear equippable by a
	-- SMN enhances Aspir. Aspir is part of Dark Magic, so potency which is based on dark magic skill will
	-- already be loaded in HandleMidcast function and need not be repeated here. No current gear supports
	-- dark magic accuracy for any job. Magic attack bonus and magic critical hit have no effect on potency.
	-- Leave the two Aspir gear sets empty.
	['Aspir'] = {
    },
	['Aspir_Conditional'] = {
	},
	
	-- Sneak: Enhances Sneak and Enhances Stealth. Currently only Dream Boots +1 enhances sneak and is equippable
	-- by any job. (Attained through the Starlight Celebration.) No gear for any job supports Enhances Stealth
	-- yet.	
	['Sneak'] = {
		Feet = 'Dream Boots +1',				-- enhances sneak
	},
	['Sneak_Conditional'] = {
	},
	
	-- Invisible: Enhances Invisible Effect. Currently only Dream Mittens +1 enhances invisible and is equippable
	-- by any job. (Attained through the Starlight Celebration.)
	['Invisible'] = {
		Hands = 'Dream Mittens +1',				-- enhances invisibility
	},
	['Invisible_Conditional'] = {
	},
	
	-- Note: Phalanx does have gear that supports the spell, but it is out of era
	
--[[
	The following weapon skill gearsets are defined by the stat they emphasize. Listed are all of the sets that
	you will need to use every weapon skill that your job can do. The leading comment defines what weapon/weapon
	skill combination the set applies to.
	
	SMN can use the following weapons: staff (B), Club (C+), dagger (E). Any other weapon will have 
	no weaponskill available. Weapon skill sets are named based on stat(s) used, regardless of weapon
--]]

--[[
		* Strength based or just skill based *
		
		Staff: Heavy Swing,Shell Crusher,Full Swing
		Club: Brainshaker,Skullbreaker,True Strike
-]]
	
	['WS_STR'] = {
        Head  = 'Mrc.Cpt. Headgear',
        Neck  = 'Spike Necklace',
        Body  = 'Wonder Kaftan',
        Hands = 'Wonder Mitts',
		Rings = { 'Sun Ring', 'Sun Ring', 'Courage Ring', 'San d\'Orian Ring' },
        Waist = 'Mrc.Cpt. Belt',
        Legs  = 'Wonder Braccae',
        Feet  = { 'Creek F Clomps', 'Wonder Clumps' },
    },
	['WS_STR_Conditional'] = {
	},
	
--[[
		* Strength and Intelligence based, even weighting *
		
		Staff: Rock Crusher,Earth Crusher,Cataclysm
--]]
	
	['WS_STRINT'] = {
        Head  = 'Mrc.Cpt. Headgear',
        Neck  = 'Spike Necklace',
        Body  = 'Wonder Kaftan',
        Hands = 'Wonder Mitts',
		Rings = { 'Tamas Ring', 'Sun Ring', 'Courage Ring', 'San d\'Orian Ring' },
        Waist = 'Mrc.Cpt. Belt',
        Legs  = 'Seer\'s Slacks',
        Feet  = { 'Creek F Clomps', 'Wonder Clumps' },
    },
	['WS_STRINT_Conditional'] = {
	},

--[[
		* Strength and Mind based, even weighting *
		
		Club: Shining Strike,Seraph Strike,Judgement
		Staff: Starburst,Sunburst,Retribution
--]]
	
	['WS_STRMND'] = {
        Head  = 'Mrc.Cpt. Headgear',
        Neck  = 'Justice Badge',
        Body  = 'Wonder Kaftan',
        Hands = 'Wonder Mitts',
		Rings = { 'Tamas Ring', 'Sun Ring', 'Courage Ring', 'Tranquility Ring', 'San d\'Orian Ring', 'Windurstian Ring' },
        Back  = 'White Cape',
        Waist = 'Mrc.Cpt. Belt',
        Legs  = 'Wonder Braccae',
        Feet  = { 'Creek F Clomps', 'Wonder Clumps' },
    },
	['WS_STRMND_Conditional'] = {
	},

--[[
		* Strength and Mind based, 30% to 50% weighting *
		
		Club: Black Halo
--]]

	['WS_STRMND_30_50'] = {
        Head  = 'Mrc.Cpt. Headgear',
        Neck  = 'Justice Badge',
        Body  = 'Wonder Kaftan',
        Hands = 'Wonder Mitts',
		Rings = { 'Tamas Ring', 'Sun Ring', 'Courage Ring', 'Tranquility Ring', 'San d\'Orian Ring', 'Windurstian Ring' },
        Back  = 'White Cape',
        Waist = 'Mrc.Cpt. Belt',
        Legs  = 'Wonder Braccae',
        Feet  = { 'Creek F Clomps', 'Wonder Clumps' },
    },
	['WS_STRMND_30_50_Conditional'] = {
	},
	
--[[
		* Dexterity based *
		
		Dagger: Wasp Sting,Viper Bite^

		^ Subjob must be one of: RDM,THF,BRD,RNG,NIN
--]]
	
	['WS_DEX'] = {
        Head  = 'Empress Hairpin',
        Neck  = 'Spike Necklace',
        Body  = 'Mrc.Cpt. Doublet',
        Rings = { 'Balance Ring', 'Bastokan Ring' },
        Waist = 'Mrc.Cpt. Belt',
    },
	['WS_DEX_Conditional'] = {
	},

--[[
		* Dexterity and Intelligence based *

		Dagger: Gust Slash,Cyclone^

		^ Subjob must be one of: RDM,THF,BRD,RNG,NIN
--]]
	
	['WS_DEXINT'] = {
        Head  = 'Empress Hairpin',
        Neck  = 'Spike Necklace',
        Body  = 'Mrc.Cpt. Doublet',
        Hands = 'Seer\'s Mitts',
		Rings  = { 'Tamas Ring', 'Balance Ring', 'Bastokan Ring' },
        Waist = 'Mrc.Cpt. Belt',
        Legs  = 'Seer\'s Slacks',
        Feet  = 'Mannequin Pumps',
    },
	['WS_DEXINT_Conditional'] = {
	},

--[[
		* Intellegence *

		Staff: Gate of Tartarus
--]]
	
	['WS_INT'] = {
        Hands = 'Seer\'s Mitts',
        Rings = { 'Tamas Ring', 'Windurstian Ring' },
        Waist = 'Mrc.Cpt. Belt',
        Legs  = 'Seer\'s Slacks',
        Feet  = 'Mannequin Pumps',
    },
	['WS_INT_Conditional'] = {
	},
	
--[[
		* Intellegence and Mind based, even weighting *

		Staff: Spirit Taker
--]]
	
	['WS_INTMND'] = {
        Neck  = 'Justice Badge',
        Body  = 'Wonder Kaftan',
        Hands = 'Seer\'s Mitts',
		Rings = { 'Tamas Ring', 'Tranquility Ring', 'San d\'Orian Ring', 'Windurstian Ring' },
        Waist = 'Mrc.Cpt. Belt',
        Legs  = 'Wonder Braccae',
        Feet  = 'Mannequin Pumps',
    },
	['WS_INTMND_Conditional'] = {
	},
	
--[[
		* Charisma based *

		Dagger: Shadowstitch
--]]
	
	['WS_CHR'] = {
        Head  = 'Entrancing Ribbon',
        Neck  = 'Flower Necklace',
		Ears  = 'Beastly Earring',
        Rings = { 'Moon Ring', 'Moon Ring' },
        Waist = { 'Corsette', 'Mrc.Cpt. Belt' },
    },
	['WS_CHR_Conditional'] = {
	},

--[[
		* Mind based *

		Dagger: Energy Steal, Energy Drain^
		
		^ Subjob must be RDM,THF,BRD,RNG, or NIN
--]]

	['WS_MND'] = {
        Neck  = 'Justice Badge',
        Body  = 'Wonder Kaftan',
        Hands = 'Seer\'s Mitts',
		Rings = { 'Tamas Ring', 'Tranquility Ring', 'San d\'Orian Ring' },
        Back  = 'White Cape',
        Waist = { 'Mrc.Cpt. Belt', 'Friar\'s Rope' },
        Legs  = 'Wonder Braccae',
        Feet  = 'Mannequin Pumps',
    },
	['WS_MND_Conditional'] = {
	},
	
--[[
		* Skill based *
		
		Club: Starlight,Moonlight
--]]
	
	['WS_Skill'] = {
    },
	['WS_Skill_Conditional'] = {
	},
	
--[[
	Movement tends to be used for kiting. Emphasis should be placed on gear that increases movement speed, but you 
	might also want gear that has evasion. The choice is yours.
--]]

	['Movement'] = { 
	},
	['Movement_Conditional'] = {
	},

--[[	
	 Might seem strange to include BST gear sets, but a SMN/BST can charm a monster
	(not sure why you'd want to) and the level of the pet will be the level of the
	charmed monster.
--]]
	-- BST ability, CHR gear.
	['Charm'] = {
	    Head  = 'Entrancing Ribbon',
        Neck  = 'Flower Necklace',
		Ears  = 'Beastly Earring',
        Rings = { 'Moon Ring', 'Moon Ring' },
        Waist = { 'Corsette', 'Mrc.Cpt. Belt' },
    },
	['Charm_Conditional'] = {
	},
		
	['Pet_Attack'] = {					-- Pet's strength, not accuracy
	},
	['Pet_Attack_Conditional'] = {
	},
	
	['Pet_Macc'] = {					-- Pet's Magical Accuracy
		Head = 'Shep. Bonnet',
	},
	['Pet_Macc_Conditional'] = {
	},
	
	['Pet_Matt'] = {					-- Pet's Magical Attack
	},
	['Pet_Matt_Conditional'] = {
	},

	--* DRK *--
	['WeaponBash'] = {
	},
	['WeaponBash_Conditional'] = {
	},
	
	--* DRG *--
	['Jumps'] = {		-- Jump and High Jump, Super is too high a level
	},
	['Jumps_Conditional'] = {
	},

--[[
	The following set is used to dynamically create a gear set to be displayed once rather
	than in a piecemeal manner. It is hoped that this will cut down on flickering gear and
	possibly speed up the code. *** This set is to be left empty by the player ***. Please
	do not modify it.
--]]

	['CurrentGear'] = { },
	
--[[
								*** Custom Sets Go below this comment ***
--]]

};

profile.Sets = sets;
profile.sjb = nil;			-- Tracks subjob name
profile.sPetAction = nil;	-- what was the last action by your avatar
profile.bAmmo = false;		-- /BST specific. Is ammo equipped?
profile.sAmmo = nil;		-- /BST specific. Name of ammo equipped

--[[
	HandlePetAction equips the appropriate gear set based on the type of action
	the pet is trying to perform. (This is specifically rage blood pacts.)
	
	Note: This version of HandlePetAction has two parameters instead of the one
	that is more commonly found in the other job files. This is because SMN/BST
	can possibly have either type of pet.
--]]

local function HandlePetAction(Pet,PetAction)
	local bSmn = false;
	
	Pet.Name = string.lower(Pet.Name);
	if string.find(gcinclude.MagicSkill['Summoning'],Pet.Name) ~= nil then
		bSmn = true;
		
		-- if the action is a BP: rage, print out what happened in party chat
		if (profile.sPetAction == nil or profile.sPetAction ~= PetAction.Name) and gcdisplay.GetToggle('sBP') == true then
			local sMsg;
			if string.find(gcinclude.SmnBPRageList,PetAction.Name) ~= nil then
				sMsg = '/p  Blood Pact [<pet>]:  ' .. PetAction.Name .. ' >> <t>.';
			else
				sMsg = '/echo [<pet>]: Blood Pact - ' .. PetAction.Name;
			end
			AshitaCore:GetChatManager():QueueCommand(-1, sMsg);
			profile.sPetAction = PetAction.Name;
		end
	end
	
	-- Only gear swap if this flag is true
	if gcdisplay.GetToggle('GSwap') == false then
		return;
	end
	
	if bSmn == true then
		-- Check to see if the pet is a smn's avatar or a /bst pet
		-- All SMN pet actions are blood pacts. Address accordingly
		if (gcinclude.SmnSkill:contains(PetAction.Name)) then			-- summoning skill based blood pact?
			gcinclude.MoveToCurrent(sets.SmnSkill,sets.CurrentGear);
			gcinclude.ProcessConditional(sets.SmnSkill_Conditional,nil,sets.CurrentGear);			
		elseif (gcinclude.SmnMagical:contains(PetAction.Name)) then		-- magical based blood pact?
			gcinclude.MoveToCurrent(sets.SmnMagical,sets.CurrentGear);
			gcinclude.ProcessConditional(sets.SmnMagical_Conditional,nil,sets.CurrentGear);			
			-- If /acc flagged, load accuracy set (for magical accuracy)
			if gcdisplay.GetToggle('Acc') == true then
				gcinclude.MoveToCurrent(sets.SmnAccuracy,sets.CurrentGear);
				gcinclude.ProcessConditional(sets.SmnAccuracy_Conditional,nil,sets.CurrentGear);				
			end
		elseif (gcinclude.SmnHybrid:contains(PetAction.Name)) then		-- hybrid blood pact (2x physical, 1x magical)?
			gcinclude.MoveToCurrent(sets.SmnHybrid,sets.CurrentGear);
			gcinclude.ProcessConditional(sets.SmnHybrid_Conditional,nil,sets.CurrentGear);			
			-- lastly check on accuracy
			if gcdisplay.GetToggle('Acc') == true then
				gcinclude.MoveToCurrent(sets.SmnAccuracy,sets.CurrentGear);
				gcinclude.ProcessConditional(sets.SmnAccuracy_Conditional,nil,sets.CurrentGear);
			end				
		else																-- physical	blood pact
			-- if /acc flagged, load accuracy set (for physical accuracy)
			gcinclude.MoveToCurrent(sets.SmnPhysical,sets.CurrentGear);
			gcinclude.ProcessConditional(sets.SmnPhysical_Conditional,nil,sets.CurrentGear);			
			if gcdisplay.GetToggle('Acc') == true then
				gcinclude.MoveToCurrent(sets.SmnAccuracy,sets.CurrentGear);
				gcinclude.ProcessConditional(sets.SmnAccuracy_Conditional,nil,sets.CurrentGear);
			end
		end
	else
		-- Must be a /BST charmed pet.
		if (gcinclude.BstPetAttack:contains(PetAction.Name)) then				-- Pet Attack
			gcinclude.MoveToCurrent(sets.Pet_Attack,sets.CurrentGear);
			gcinclude.ProcessConditional(sets.Pet_Attack_Conditional,nil,sets.CurrentGear);			
			-- If /acc enabled equip pet accuracy gear
			if gcdisplay.GetToggle('acc') == true then
				gcinclude.MoveToCurrent(sets.Pet_Accuracy,sets.CurrentGear);
				gcinclude.ProcessConditional(sets.Pet_Accuracy_Conditional,nil,sets.CurrentGear);
			end
		elseif (gcinclude.BstPetMagicAttack:contains(PetAction.Name)) then		-- Pet Magical Attack
			gcinclude.MoveToCurrent(sets.Pet_Matt,sets.CurrentGear);
			gcinclude.ProcessConditional(sets.Pet_Matt_Conditional,nil,sets.CurrentGear);			
		elseif (gcinclude.BstPetMagicAccuracy:contains(PetAction.Name)) then	-- Pet Magical Accuracy Attack
			gcinclude.MoveToCurrent(sets.Pet_Macc,sets.CurrentGear);
			gcinclude.ProcessConditional(sets.Pet_Macc_Conditional,nil,sets.CurrentGear);			
		end
	end
	gcinclude.EquipTheGear(sets.CurrentGear);
end		-- HandlePetAction

--[[
	SetSubjobSet is used to pick the appropriate set for the loaded macrobook based on
	which subjob is current. (If no change has occurred since the last time it was called,
	nothing is checked/changed.)
--]]

local function SetSubjobSet(chkSJ)
	-- "subs" is the key for what toolbar is shown. Each job listed in the array is a possible subjob.
	-- If the associated value for a subjob is greater than zero, that indicates what macro set from the
	-- macro book should be displayed. (The value associated with SMN, your current main job, is nil
	-- since you can't be a smn/smn.) A value of 0 means the subjob is not configured. All other values
	-- imply that the subjob is expected and shows what macro set to show.
	local subs = {['WAR'] = 0, ['MNK'] = 0, ['WHM'] = 1, ['BLM'] = 3, ['RDM'] = 2, ['THF'] = 0,
				 ['PLD'] = 0, ['DRK'] = 0, ['BST'] = 0, ['BRD'] = 0, ['RNG'] = 0, ['SMN'] = nil,
				 ['SAM'] = 0, ['NIN'] = 0, ['DRG'] = 0, ['BLU'] = 0, ['COR'] = 0, ['PUP'] = 0,
				 ['DNC'] = 0, ['SCH'] = 0, ['GEO'] = 0, ['RUN'] = 0};
	local sj = nil;

	if (profile.sjb == nil or (chkSJ ~= nil and chkSJ ~= 'NON' and chkSJ ~= profile.sjb)) then	-- Compare the stored subjob with the current subjob
		if subs[chkSJ] ~= nil and subs[chkSJ] > 0 then
			sj = subs[chkSJ];
		else
			sj = 1;					-- Default set
		end

		AshitaCore:GetChatManager():QueueCommand(1, '/macro set '..tostring(sj));
		if chkSJ ~= nil and chkSJ ~= 'NON' then
			profile.sjb = chkSJ;
		end
	end
end

--[[
	OnLoad is run whenever you log into your SMN or change your job to SMN
--]]

profile.OnLoad = function()
	local player = gData.GetPlayer();

	-- Initialize settings
	gSettings.AllowAddSet = true;
	gcinclude.Initialize();
	gcinclude.settings.RegenGearHPP = 50;
    gcinclude.settings.RefreshGearMPP = 60;
	gcinclude.settings.bSummoner = true;
	
	-- Coded order of operation override
	gcinclude.settings.priorityEngaged = 'CEFGH';
	gcinclude.settings.priorityMidCast = 'ABCDEFGH';
	gcinclude.settings.priorityWeaponSkill = 'ABDE';	
	
	-- Determine if subjob uses magic and if the maximum MP is > 50.
	gcinclude.CheckMagic50(player);
	
	-- Set which macro book should be displayed. Which macro set within the macro book to
	-- display depends on what your subjob is.
	AshitaCore:GetChatManager():QueueCommand(1, '/macro book 13');		-- SMN macro book
	SetSubjobSet(player.SubJob);
	
	-- Load up the weapons bar.
	gcinclude.MoveToCurrent(sets.Start_Weapons,sets.CurrentGear);
	gcinclude.ProcessConditional(sets.Start_Weapons_Conditional,nil,sets.CurrentGear);	
	gcinclude.EquipTheGear(sets.CurrentGear);
	
		-- Make sure the saved weapons are the starting weapons
	gcinclude.weapon = sets.CurrentGear['Main'];
	if sets.CurrentGear['Sub'] == nil then
		gcinclude.offhand = nil;
	else
		gcinclude.offhand = sets.CurrentGear['Sub'];
	end
end

--[[
	OnUnload is run when you change to another job
--]]

profile.OnUnload = function()
	gcinclude.Unload();
end

--[[
	HandleCommand is run when you type in a command defined in LUASHITACAST. The commands handled here instead
	of in gcinclude.HandleCommands are specific to SMN or the help system.
--]]

profile.HandleCommand = function(args)
	if args[1] == 'help' then
		gcdisplay.ShowHelp(args);
	elseif args[1] == 'petfood' then
		gcinclude.doPetFood(args[2],args[3]);		
	else
		gcinclude.HandleCommands(args);
	end
end

--[[
	HandleDefault is run when some action happens. This emphasizes pet actions
--]]
	
profile.HandleDefault = function()
	local pet = gData.GetPet();
	local petAction = gData.GetPetAction();
	local player = gData.GetPlayer();
	local zone = gData.GetEnvironment();
	local ew = gData.GetEquipment();
	local eWeap = nil;
	local cKey;
	local myLevel = AshitaCore:GetMemoryManager():GetPlayer():GetMainJobLevel();
	
	-- Note the player's current level
	if (myLevel ~= gcinclude.settings.iCurrentLevel) then
        gcinclude.settings.iCurrentLevel = myLevel;
    end
		
	-- Make sure that the global magic settings for the player are known. The secoond clause in
	-- the if statement takes care of a bizarre case. Turns out if you change the player.MainJob
	-- from a job where there is not a luashitacast script, it initially remembers the old main
	-- job. by including the second call, a subsequent invocation occurs getting it right.
	if gcinclude.settings.bMagicCheck == false or gcinclude.settings.sMJ ~= player.MainJob then
		gcinclude.CheckMagic50(player);
	end
	
	-- A pet action takes priority over a player's action. Only SMN avatar actions supported
	if pet ~= nil then
		local sLName = string.lower(pet.Name);
		if petAction ~= nil and (string.find(gcinclude.MagicSkill['Summoning'],sLName) ~= nil) then
			HandlePetAction(pet,petAction);
			return;
		end
	end
	
	profile.sPetAction = nil;
		
	-- Save the name of the main weapon
	if ew['Main'] ~= nil then
		eWeap = ew['Main'].Name;	
	end
	
	-- Make sure the macro set is shown and that the display on the top of the screen is correct
	-- in case the subjob was changed.
	SetSubjobSet(player.SubJob);
	
	-- Only gear swap if this flag is true
	if gcdisplay.GetToggle('GSwap') == false then
		return;
	end

	-- Clear out the CurrentGear in case of leftovers
	gcinclude.ClearSet(sets.CurrentGear);
	
	-- If player is not resting and has a pet, make sure they're holding the correct staff 
	-- (assuming they own the correct staff)
	if player.Status ~= 'Resting' and pet ~= nil then
		local pName = string.lower(pet.Name);
		local pEle = gcinclude.SummonStaves[pName];

		if string.find(gcinclude.MagicSkill['Summoning'],pet.Name) ~= nil then
			if eWeap ~= nil and (eWeap ~= gcinclude.elemental_staves[pEle][1] or eWeap ~= gcinclude.elemental_staves[pEle][3]) then
				gcinclude.SwapToStave(pEle,true,sets.CurrentGear);
			end
		end
	end
	
	-- The default set is the TP gear set. Load it up
	gcinclude.MoveToCurrent(sets.TP,sets.CurrentGear);
	gcinclude.ProcessConditional(sets.TP_Conditional,nil,sets.CurrentGear);
	
	if pet == nil then
		gcinclude.MoveToCurrent(sets.TP_No_Pet,sets.CurrentGear);
		gcinclude.ProcessConditional(sets.TP_No_Pet_Conditional,nil,sets.CurrentGear);	
	end
	
	-- Now process the pet/player statuses accordingly.
	gcdisplay.SetLocksAction(gcinclude.LocksNumeric,player.Status);	
	if (pet ~= nil and pet.Status == 'Engaged') or (player ~= nil and player.Status == 'Engaged') then
		gcinclude.settings.priorityEngaged = string.upper(gcinclude.settings.priorityEngaged);
		for i = 1,string.len(gcinclude.settings.priorityEngaged),1 do
			cKey = string.sub(gcinclude.settings.priorityEngaged,i,i);
			if cKey == 'C' then		-- Evasion
				if gcdisplay.GetToggle('Eva') == true then
					if pet ~= nil and pet.Status == 'Engaged' then
						gcinclude.MoveToCurrent(sets.Pet_Evasion,sets.CurrentGear);
						gcinclude.ProcessConditional(sets.Pet_Evasion_Conditional,nil,sets.CurrentGear);
					else
						gcinclude.MoveToCurrent(sets.Evasion,sets.CurrentGear);
						gcinclude.ProcessConditional(sets.Evasion_Conditional,nil,sets.CurrentGear);
					end
				end
			elseif cKey == 'E' then		-- Accuracy
				if gcdisplay.GetToggle('Acc') == true then 
					if pet ~= nil and pet.Status == 'Engaged' then
						gcinclude.MoveToCurrent(sets.Pet_Accuracy,sets.CurrentGear);
						gcinclude.ProcessConditional(sets.Pet_Accuracy_Conditional,nil,sets.CurrentGear);					
					else
						gcinclude.MoveToCurrent(sets.Accuracy,sets.CurrentGear);
						gcinclude.ProcessConditional(sets.Accuracy_Conditional,nil,sets.CurrentGear);
					end
				end
			elseif cKey == 'F' then		-- Kiting
				if (gcdisplay.GetToggle('Kite') == true) then
					gcinclude.MoveToCurrent(sets.Movement,sets.CurrentGear);
					gcinclude.ProcessConditional(sets.Movement_Conditional,nil,sets.CurrentGear);
				end
			elseif cKey == 'G' then		-- common buffs/debuffs
				gcinclude.CheckCommonDebuffs();
			elseif cKey == 'H' then		-- Damage Taken gear
				if (gcdisplay.GetCycle('DT') ~= gcinclude.OFF) then
					if gcdisplay.GetCycle('DT') == 'Physical' then
						gcinclude.MoveToCurrent(sets.DT_Physical,sets.CurrentGear);
						gcinclude.ProcessConditional(sets.DT_Physical_Conditional,nil,sets.CurrentGear);
					elseif gcdisplay.GetCycle('DT') == 'Magical' then
						gcinclude.MoveToCurrent(sets.DT_Magical,sets.CurrentGear);
						gcinclude.ProcessConditional(sets.DT_Magical_Conditional,nil,sets.CurrentGear);
					elseif gcdisplay.GetCycle('DT') == 'Breath' then
						gcinclude.MoveToCurrent(sets.DT_Breath,sets.CurrentGear);
						gcinclude.ProcessConditional(sets.DT_Breath_Conditional,nil,sets.CurrentGear);
					end
				end
			end
		end
	elseif player.Status == 'Resting' then	
		-- Player kneeling. Priority (low to high): regen, refresh
		
		if player.HP < player.MaxHP then		
			gcinclude.MoveToCurrent(sets.Resting_Regen,sets.CurrentGear);
			gcinclude.ProcessConditional(sets.Resting_Regen_Conditional,nil,sets.CurrentGear);
		end

		if player.MP < player.MaxMP then
			gcinclude.MoveToCurrent(sets.Resting_Refresh,sets.CurrentGear);
			gcinclude.ProcessConditional(sets.Resting_Refresh_Conditional,nil,sets.CurrentGear);
		end
	
		-- Check for common debuffs
		gcinclude.CheckCommonDebuffs();
	else
		-- Assume idling. There's no idle set, just idle conditions
		
		-- See if in a town
		if zone.Area ~= nil and gcinclude.Towns:contains(zone.Area) then
			gcinclude.MoveToCurrent(sets.Town,sets.CurrentGear);
			gcinclude.ProcessConditional(gcinclude.sets.Town_Conditional,nil,sets.CurrentGear);
		else
			-- Check to see if there's an avatar							
			if pet ~= nil then	
				pet.Name = string.lower(pet.Name);		
				if string.find(gcinclude.MagicSkill['Summoning'],pet.Name) ~= nil then
					gcinclude.MoveToCurrent(sets.Idle_With_Pet,sets.CurrentGear);
					gcinclude.ProcessConditional(sets.Idle_With_Pet_Conditional,nil,sets.CurrentGear);
				end
			end
		end
		
		-- if the player's HP is below the threshold setting, equip the idle regen gear
		if player.HPP < gcinclude.settings.RegenGearHPP then
			gcinclude.MoveToCurrent(sets.Idle_Regen,sets.CurrentGear);
			gcinclude.ProcessConditional(sets.Idle_Regen_Conditional,nil,sets.CurrentGear);
		end
		-- if the player's MP is below the threshold setting, equip the idle refresh gear
		if player.MPP < gcinclude.settings.RefreshGearMPP then
			gcinclude.MoveToCurrent(sets.Idle_Refresh,sets.CurrentGear);
			gcinclude.ProcessConditional(sets.Idle_Refresh_Conditional,nil,sets.CurrentGear);
		end
		-- Check for common debuffs
		gcinclude.CheckCommonDebuffs();
	end
	
	-- Make sure to equip the appropriate elemental staff for the current pet
	if (pet ~= nil) then
		local pName = string.lower(pet.Name);
		if string.find(gcinclude.MagicSkill['Summoning'],pName) ~= nil then
			local pEle = gcinclude.SummonStaves[pet.Name];
			gcinclude.SwapToStave(pEle,false,sets.CurrentGear);
		end
	end
	
	-- And make sure a weapon equipped. (Going into a capped area can cause no weapon to be equipped.)
	local gear = gData.GetEquipment();
	if gear.Main == nil then
		gcinclude.MoveToCurrent(sets.Start_Weapons,sets.CurrentGear,true);
		gcinclude.ProcessConditional(sets.Start_Weapons_Conditional,nil,sets.CurrentGear);
	end
	
	-- Equip the composited HandleDefault set
	gcinclude.EquipTheGear(sets.CurrentGear);
	
	-- Lastly, update the display, just in case
	gcdisplay.Update();
end

--[[
	HandleAbility is used to change the player's gear appropriately for the specified avatar ability.
--]]

profile.HandleAbility = function()
	local ability = gData.GetAction();
	local eq = gData.GetEquipment();
		
	-- gear swapping is turned off or the ability is release, assault, or retreat, no specific gear set
	-- needs to be loaded. Exit function if encountered.
	if ((gcdisplay.GetToggle('GSwap') == false) or
		(ability.Name == 'Release') or 
		(ability.Name == 'Assault') or
		(ability.Name == 'Retreat')) then 
		return 
	end

	-- Store the name of the ammo. This is used when the ammo slot is automatically
	-- populated so that the original ammo can be re-equipped.
	if eq.Ammo ~= nil then
		profile.sAmmo = eq.Ammo.Name;
	else
		profile.sAmmo = nil;
	end
	
	-- Clear out the CurrentGear in case of leftovers
	gcinclude.ClearSet(sets.CurrentGear);
	
	-- Check for abilities first that are not associated with smn.
	-- Start with BST
	if string.match(ability.Name, 'Reward') then
		-- Pet reward. Make sure that pet food already equipped
		if profile.sAmmo == nil or string.find(string.lower(profile.sAmmo),'pet f') == nil then		-- something else equipped
			profile.bAmmo = gcinclude.doPetFood('max',nil);
		end
		gcinclude.MoveToCurrent(sets.Reward,sets.CurrentGear);
		gcinclude.ProcessConditional(sets.Reward_Conditional,nil,sets.CurrentGear);
	elseif string.match(ability.Name, 'Charm') then
		-- Trying to charm a beast. 
		gcinclude.MoveToCurrent(sets.Charm,sets.CurrentGear);
		gcinclude.ProcessConditional(sets.Charm_Conditional,nil,sets.CurrentGear);
		gcinclude.SwapToStave('light',false,sets.CurrentGear);	
	elseif string.match(ability.Name, 'Weapon Bash') then		-- assumes /drk
		gcinclude.MoveToCurrent(sets.WeaponBash,sets.CurrentGear);
		gcinclude.ProcessConditional(sets.WeaponBash_Conditional,nil,sets.CurrentGear);		
	elseif string.find(ability.Name, 'Jump') then		-- assumes /drk
		gcinclude.MoveToCurrent(sets.Jumps,sets.CurrentGear);
		gcinclude.ProcessConditional(sets.Jumps_Conditional,nil,sets.CurrentGear);		
	else
		-- Since we got here, the action has to be a SMN Blood Pact
		gcinclude.MoveToCurrent(sets.BP,sets.CurrentGear);
		gcinclude.ProcessConditional(sets.BP_Conditional,nil,sets.CurrentGear);
--[[
	Abilities associated with subjobs go here. The following subjobs have
	no ability entries because of lack of gear or just doesn't make sense: 
	DRK,PLD(out of era),WAR,MNK,WHM,BLM,RDM,BRD,RNG,SAM,DRG,THF
		
	Note: for /THF, sneak attack gets no bonus from DEX and trick attack gets
	no bonus from AGI
--]]

	end
	gcinclude.EquipTheGear(sets.CurrentGear);		-- Equip the composited HandleAbility set
end

--[[
	HandleItem is the place to equip gear when a special item is used. Currently only 'Holy Water' 
	is supported
--]]

profile.HandleItem = function()
	local item = gData.GetAction();
	local bShow = false;

	-- Clear out the CurrentGear in case of leftovers
	gcinclude.ClearSet(sets.CurrentGear);
	
	if gcdisplay.GetToggle('GSwap') == true then		-- Only gear swap if this flag is true
		if string.match(item.Name, 'Holy Water') then 
			gcinclude.MoveToCurrent(gcinclude.sets.Holy_Water,sets.CurrentGear);
			gcinclude.ProcessConditional(gcinclude.sets.Holy_Water_Conditional,nil,sets.CurrentGear);
			bShow = true;
		elseif string.match(item.Name, 'Silent Oil') then
			gcinclude.MoveToCurrent(sets.Sneak,sets.CurrentGear);
			gcinclude.ProcessConditional(sets.Sneak_Conditional,nil,sets.CurrentGear);
			bShow = true;
		elseif string.match(item.Name, 'Prism Powder') then
			gcinclude.MoveToCurrent(sets.Invisible,sets.CurrentGear);
			gcinclude.ProcessConditional(sets.Invisible_Conditional,nil,sets.CurrentGear);
			bShow = true;
		end
		
		if bShow == true then
			gcinclude.EquipTheGear(sets.CurrentGear);
		end
	end
end

--[[
	HandlePrecast is invoked when the player casts a spell. It is the first step of two where you load any
	Fast Cast, cast time reduction, and quick cast gear in anticipation of a spell
--]]

profile.HandlePrecast = function()
    local spell = gData.GetAction();
	local obi;
	local mSet;
	
	-- Only process if /gswap is turned on
	if gcdisplay.GetToggle('GSwap') == false then
		return;
	end
	
	-- Equip the precast gear set
	gcinclude.MoveToCurrent(sets.Precast,sets.CurrentGear);
	gcinclude.ProcessConditional(sets.Precast_Conditional,nil,sets.CurrentGear);	
		
	-- See if an elemental obi should be equipped
	if gcinclude.settings.bEleObis == false then
		gcinclude.CheckForObisGorgets();
	end	
	if gcinclude.settings.bEleObis == true then
		obi = gcinclude.CheckEleSpells(spell.Name,gcinclude.MagicEleAcc,gcinclude.OBI,nil);
		if obi ~= nil then
			sets.CurrentGear['Waist'] = obi;
		end
	end	
	gcinclude.EquipTheGear(sets.CurrentGear);
end

--[[
	HandleMidcast is the second function invoked when a player casts a spell. It equips gear appropriate for 
	magic skill, duration, magic attack bonus, magic accuracy, and potency. There's an order to how the pieces 
	are loaded: INT/MND, spell specific, macc, magic skill, obi, ele swap	
--]]

profile.HandleMidcast = function()
	local player = gData.GetPlayer();
	local spell = gData.GetAction();
	local obi;
	local sSet;
	local cKey;

	if gcdisplay.GetToggle('GSwap') == false then		-- Only gear swap if this flag is true	
		return;
	end

	-- Clear out the CurrentGear in case of leftovers
	gcinclude.ClearSet(sets.CurrentGear);
	
	gcinclude.settings.priorityMidCast = string.upper(gcinclude.settings.priorityMidCast);
	for i = 1,string.len(gcinclude.settings.priorityMidCast),1 do
		cKey = string.sub(gcinclude.settings.priorityMidCast,i,i);

		if cKey == 'A' then				-- midcast gear
			gcinclude.MoveToCurrent(sets.Midcast,sets.CurrentGear);
			gcinclude.ProcessConditional(sets.Midcast_Conditional,nil,sets.CurrentGear);
		elseif cKey == 'B' then			-- Spell Interruption Rate gear
			gcinclude.MoveToCurrent(sets.SIR,sets.CurrentGear);
			gcinclude.ProcessConditional(sets.SIR_Conditional,nil,sets.CurrentGear);
		elseif cKey == 'C' then			-- INT/MND gear?
			sSet = gcinclude.WhichStat(spell.Name);
			if sSet ~= nil then
				if sSet == 'MND' then
					gcinclude.MoveToCurrent(sets.MND,sets.CurrentGear);
					gcinclude.ProcessConditional(sets.MND_Conditional,nil,sets.CurrentGear);
				elseif sSet == 'INT' then
					gcinclude.MoveToCurrent(sets.INT,sets.CurrentGear);
					gcinclude.ProcessConditional(sets.INT_Conditional,nil,sets.CurrentGear);
				end
			end
		elseif cKey == 'D' then			-- Magic Skill Type
			mSet = gcinclude.WhichMagicSkill(spell.Name);

			if mSet ~= nil then
				if mSet == 'Healing' then
					gcinclude.MoveToCurrent(sets.Healing,sets.CurrentGear);
					gcinclude.ProcessConditional(sets.Healing_Conditional,nil,sets.CurrentGear);
				elseif mSet == 'Dark' then
					gcinclude.MoveToCurrent(sets.Dark,sets.CurrentGear);
					gcinclude.ProcessConditional(sets.Dark_Conditional,nil,sets.CurrentGear);
				elseif mSet == 'Divine' then
					gcinclude.MoveToCurrent(sets.Divine,sets.CurrentGear);
					gcinclude.ProcessConditional(sets.Divine_Conditional,nil,sets.CurrentGear);
				elseif mSet == 'Enfeebling' then
					gcinclude.MoveToCurrent(sets.Enfeebling,sets.CurrentGear);
					gcinclude.ProcessConditional(sets.Enfeebling_Conditional,nil,sets.CurrentGear);
				elseif mSet == 'Enhancing' then
					gcinclude.MoveToCurrent(sets.Enhancing,sets.CurrentGear);
					gcinclude.ProcessConditional(sets.Enhancing_Conditional,nil,sets.CurrentGear);
				elseif mSet == 'Elemental' then
					gcinclude.MoveToCurrent(sets.Elemental,sets.CurrentGear);				
					gcinclude.ProcessConditional(sets.Elemental_Conditional,nil,sets.CurrentGear);
				elseif mSet == 'Ninjitsu' then
					gcinclude.MoveToCurrent(sets.Ninjitsu,sets.CurrentGear);
					gcinclude.ProcessConditional(sets.Ninjitsu_Conditional,nil,sets.CurrentGear);
				elseif mSet == 'Summoning' then
					gcinclude.MoveToCurrent(sets.Summoning,sets.CurrentGear);
					gcinclude.ProcessConditional(sets.Summoning_Conditional,nil,sets.CurrentGear);
				end
				-- See if Magic Attack Bonus useful. Note: Spells like bio/dia initial damage is
				-- affected by MAB, but not the tic. Not worth trying to support here. Also, Ninjitsu
				-- is affected by Ninjitsu Magic Attack Bonus and not just Magic Bonus.
				if string.find('Healing,Enfeebling,Enhancing,Ninjitsu,Summoning',mSet) == nil then
					gcinclude.MoveToCurrent(sets.MAB,sets.CurrentGear);
					gcinclude.ProcessConditional(sets.MAB_Conditional,nil,sets.CurrentGear);
				end
			end
		elseif cKey == 'E' then			--Magical accuracy
			if gcdisplay.GetToggle('acc') == true then
				gcinclude.MoveToCurrent(sets.Macc,sets.CurrentGear);
				gcinclude.ProcessConditional(sets.Macc_Conditional,nil,sets.CurrentGear);
			end
		elseif cKey == 'F' then			-- Spell specific gear
			if string.match(spell.Name, 'Stoneskin') then
				-- Mind has a large affect on Stoneskin, so equip it here
				gcinclude.MoveToCurrent(sets.MND,sets.CurrentGear);
				gcinclude.ProcessConditional(sets.MND_Conditional,nil,sets.CurrentGear);
				-- Now load the specific stoneskin set	
				gcinclude.MoveToCurrent(sets.Stoneskin,sets.CurrentGear);
				gcinclude.ProcessConditional(sets.Stoneskin_Conditional,nil,sets.CurrentGear);
			elseif string.match(spell.Name, 'Drain') then
				gcinclude.MoveToCurrent(sets.Drain,sets.CurrentGear);
				gcinclude.ProcessConditional(sets.Drain_Conditional,nil,sets.CurrentGear);
			elseif string.match(spell.Name, 'Aspir') then
				gcinclude.MoveToCurrent(sets.Aspir,sets.CurrentGear);
				gcinclude.ProcessConditional(sets.Aspir_Conditional,nil,sets.CurrentGear);
			elseif string.match(spell.Name, 'Sneak') then
				gcinclude.MoveToCurrent(sets.Sneak,sets.CurrentGear);
				gcinclude.ProcessConditional(sets.Sneak_Conditional,nil,sets.CurrentGear);
			elseif string.match(spell.Name, 'Invisible') then
				gcinclude.MoveToCurrent(sets.Invisible,sets.CurrentGear);
				gcinclude.ProcessConditional(sets.Invisible_Conditional,nil,sets.CurrentGear);
			end
		elseif cKey == 'G' then				-- Elemental Obi
			if gcinclude.settings.bEleObis == false then
				gcinclude.CheckForObisGorgets();
			end	
			if gcinclude.settings.bEleObis == true then
				obi = gcinclude.CheckEleSpells(spell.Name,gcinclude.MagicEleDmg,gcinclude.OBI);
				if obi ~= nil then
					sets.CurrentGear['Waist'] = obi;
				end
			end
		elseif cKey == 'H' then				-- Elemental Stave
			if gcinclude.settings.bStave == false then
				gcinclude.CheckForStaves();
			end
			if gcinclude.settings.bEleStaves == true then
				if mSet == 'Summoning' then
					stat = gcinclude.CheckSummons(spell.Name);
				else
					stat = gcinclude.CheckEleSpells(spell.Name,gcinclude.MagicEleDmg,gcinclude.ELEMENT);
				end
			end
		
			if stat ~= nil then
				gcinclude.SwapToStave(stat,false,sets.CurrentGear);
			end
			stat = nil;
		end
	end
	gcinclude.EquipTheGear(sets.CurrentGear);		-- Equip the composited midcast set
end

--[[
	HandlePreshot is similar to HandlePrecast, but for ranged actions. It loads Ranged Accuracy 
	and Ranged Shot Speed Gear for a ranged attack
--]]

profile.HandlePreshot = function()
	if gcdisplay.GetToggle('GSwap') == false then
		return;
	end
	-- Clear out the CurrentGear in case of leftovers
	gcinclude.ClearSet(sets.CurrentGear);
		
	gcinclude.MoveToCurrent(sets.Preshot,sets.CurrentGear);
	gcinclude.ProcessConditional(sets.Preshot_Conditional,nil,sets.CurrentGear);
	gcinclude.EquipTheGear(sets.CurrentGear);
end

--[[
	HandleMidshot is similar to HandleMidcast, but for ranged actions. It loads Ranged Attack 
	and Damage gear for a ranged attack
--]]

profile.HandleMidshot = function()
	-- Only gear swap if this flag is true
	if gcdisplay.GetToggle('GSwap') == false then
		return;
	end
	
	-- Clear out the CurrentGear in case of leftovers
	gcinclude.ClearSet(sets.CurrentGear);
	
	gcinclude.MoveToCurrent(sets.Midshot,sets.CurrentGear);
	gcinclude.ProcessConditional(sets.Midshot_Conditional,nil,sets.CurrentGear);
	
	-- Equip the composited Midshot set
	gcinclude.EquipTheGear(sets.CurrentGear);
end

--[[
	HandleWeaponskill loads the gear appropriately for the weapon skill you're doing
--]]

profile.HandleWeaponskill = function()
	local ws = gData.GetAction();
	local canWS = gcinclude.CheckWsBailout();
	local cKey;

	-- If conditions would cause the weaponskill to fail, the action will be
	-- cancelled so you do not lose your TP.
	if (canWS == false) then 
		gFunc.CancelAction();
		return;
	end

	if gcdisplay.GetToggle('GSwap') == false then
		return;
	end

	-- Clear out the CurrentGear in case of leftovers
	gcinclude.ClearSet(sets.CurrentGear);
	
	gcinclude.settings.priorityWeaponSkill = string.upper(gcinclude.settings.priorityWeaponSkill);
	for i = 1,string.len(gcinclude.settings.priorityWeaponSkill),1 do
		cKey = string.sub(gcinclude.settings.priorityWeaponSkill,i,i);
		if cKey == 'A' then			-- weaponskill set
			-- Equip appropriate gear for weapon skill
			local sWS = gcinclude.WsStat(ws.Name,'STR');
						
			if sWS == 'WS_CHR' then
				gcinclude.MoveToCurrent(sets.WS_CHR,sets.CurrentGear);
				gcinclude.ProcessConditional(sets.WS_CHR_Conditional,nil,sets.CurrentGear);
			elseif sWS == 'WS_DEX' then
				gcinclude.MoveToCurrent(sets.WS_DEX,sets.CurrentGear);
				gcinclude.ProcessConditional(sets.WS_DEX_Conditional,nil,sets.CurrentGear);
			elseif sWS == 'WS_DEXINT' then
				gcinclude.MoveToCurrent(sets.WS_DEXINT,sets.CurrentGear);
				gcinclude.ProcessConditional(sets.WS_DEXINT_Conditional,nil,sets.CurrentGear);
			elseif sWS == 'WS_STR' then
				gcinclude.MoveToCurrent(sets.WS_STR,sets.CurrentGear);
				gcinclude.ProcessConditional(sets.WS_STR_Conditional,nil,sets.CurrentGear);
			elseif sWS == 'WS_MND' then
				gcinclude.MoveToCurrent(sets.WS_MND,sets.CurrentGear);
				gcinclude.ProcessConditional(sets.WS_MND_Conditional,nil,sets.CurrentGear);
			elseif sWS == 'WS_INT' then
				gcinclude.MoveToCurrent(sets.WS_INT,sets.CurrentGear);
				gcinclude.ProcessConditional(sets.WS_INT_Conditional,nil,sets.CurrentGear);
			elseif sWS == 'WS_INTMND' then
				gcinclude.MoveToCurrent(sets.WS_INTMND,sets.CurrentGear);
				gcinclude.ProcessConditional(sets.WS_INTMND_Conditional,nil,sets.CurrentGear);
			elseif sWS == 'WS_STRMND' then
				gcinclude.MoveToCurrent(sets.WS_STRMND,sets.CurrentGear);
				gcinclude.ProcessConditional(sets.WS_STRMND_Conditional,nil,sets.CurrentGear);
			elseif sWS == 'WS_STRMND_30_50' then
				gcinclude.MoveToCurrent(sets.WS_STRMND_30_5,sets.CurrentGear);
				gcinclude.ProcessConditional(sets.WS_STRMND_30_50_Conditional,nil,sets.CurrentGear);
			elseif sWS == 'WS_STRINT' then
				gcinclude.MoveToCurrent(sets.WS_STRINT,sets.CurrentGear);			
				gcinclude.ProcessConditional(sets.WS_STRINT_Conditional,nil,sets.CurrentGear);
			elseif sWS == 'WS_Skill' then
				gcinclude.MoveToCurrent(sets.WS_Skill,sets.CurrentGear);			
				gcinclude.ProcessConditional(sets.WS_Skill_Conditional,nil,sets.CurrentGear);
			end
		elseif cKey == 'B' then		-- elemental gorget	
			if gcinclude.settings.bEleGorgets == false then
				gcinclude.CheckForObisGorgets();
			end
			if gcinclude.settings.bEleGorgets == true then
				local sGorget = gcinclude.CheckEleGorget(ws.Name);
				if sGorget ~= nil then
					sets.CurrentGear['Neck'] = sGorget;
				end
			end
		elseif cKey == 'D' then		-- accuracy
			if gcdisplay.GetToggle('acc') == true then
				gcinclude.MoveToCurrent(sets.Accuracy,sets.CurrentGear);
				gcinclude.ProcessConditional(sets.Accuracy_Conditional,nil,sets.CurrentGear);
			end
		elseif cKey == 'E' then		-- elemental obi
--[[
			If the weaponskill is elemental and is closing a skillchain, then if the
			conditions for equipping an elemental obi are advantageous, it should be
			equipped now. Unfortunately I have no idea how to detect the closing of
			a skillchain and the automatic equipping of an elemental obi could 
			adversely affect the damage, so this section is not implemented. If I can
			ever figure out how to detect closing a skillchain, I will readdress this.
															- CCF, 1/12/2024
--]]		
		end
	end
	
	-- Special case(s) for specific weapon skills go here
	ws.Name = string.lower(ws.Name);
	if string.find('red lotus blade,sanguine blade',ws.Name) ~= nil then
		gcinclude.MoveToCurrent(sets.MAB,sets.CurrentGear);
		gcinclude.ProcessConditional(sets.MAB_Conditional,nil,sets.CurrentGear);	
	end
	
	-- Equip the composited weaponskill set
	gcinclude.EquipTheGear(sets.CurrentGear);
end

return profile;