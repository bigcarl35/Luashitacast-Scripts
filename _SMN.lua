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
	The gear sets are self contained, a mixture of direct gear assignments and conditional
	assignments. Each set contains entries identified by the gear slot. If it's a single
	value, it's a direct assignment like: Body = 'Austere Robe', but there can be multiple
	items identified, usually ordered by level: Body = { 'Vermillion Cloak//CARBY','Austere Robe' },
	Any item that has a // appended to it contains an inline conditional. The // code is a test
	to see if the item should be equipped. The level is still checked, but if the inline coded
	test is successful, that piece of gear will be loaded. Currently nothing checks to see
	if that item can be equipped by the job it's associated with let alone whether the player
	even has it accessible. Those are all planned for the future. In the mean time the onus is
	on the player to create the correct definitions.
	
	It is recommended that the gear sets not include any gear found in the top line of your 
	equipment grid (main hand, off hand, ranged weapon, ammo). Doing so will mean that TP will be 
	reset to 0 whenever gear is changed which can be very frustrating. Further, any time you do 
	change a weapon, it will convert back to what was defined in a set.	Believe me, it's no fun	
	fighting Luashitacast!

	Also, not all sets need to be defined. There is nothing wrong with leaving a set "empty", but 
	don't delete any of the sets. All the ones listed here (except for any custom sets) are expected 
	to exist by Luashitacast.
	
	*** Note ***
	Unlike when summoner is used as a subjob, /bst's pets are charmed at the max level of your BST or the level
	of your SMN, whichever is lower. That means you can charm higher level mobs than you would expect with /bst.
	Just note though that you can't have two pets, so if you have charmed a pet with /bst, you can't summon your
	avatar and visa versa.
	
	*** Note 2 ***
	No gear that supports bard songs can be worn by any job except a bard, no there's no support given here for
	/BRD.
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
		Neck  = { 'Uggalepih Pendant//NIGHTTIME', 'Fenrir\'s Torque//DAYTIME', 'Star Necklace', 'Spirit Torque', 'Justice Badge' },
		Ears  = { 'Loquac. Earring', 'Bat Earring', 'Black Earring', 'Onyx Earring' },
        Body  = { 'Vermillion Cloak//CARBY','Austere Robe', 'Seer\'s Tunic', 'Angler\'s Tunica' }, 
        Hands = { 'Carbuncle Mitts//CARBY', 'Errant Cuffs', 'Carbuncle Mitts' },
		Rings = { 'Evoker\'s Ring', 'Tamas Ring', 'Ether Ring', 'Astral Ring', 'Astral Ring' },
        Back  = { 'Blue Cape', 'White Cape' },
        Waist = { 'Hierarch Belt', 'Powerful Rope', 'Friar\'s Rope' },
        Legs  = { 'Evoker\'s Spats', 'Shep. Hose', 'Fisherman\'s Hose' }, 
        Feet  = { 'Summoner\'s Pgch.', 'Mannequin Pumps', 'Seer\'s Pumps', 'Waders'},
    },
	
--[[
	While it is concievable that other pet jobs might fight w/o a pet, it's unusual
	for a smn to fight alone. What's more likely is that your pet died. The TP_No_Pet
	gearset is if you want to change gear in that specific situation.
--]]
	
	['TP_No_Pet'] = {
		Neck  = { 'Uggalepih Pendant//NIGHTTIME', 'Fenrir\'s Torque//DAYTIME' },
		Body  = { 'Vermillion Cloak', 'Austere Robe', 'Seer\'s Tunic', 'Angler\'s Tunica' },
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

	['Pet_Accuracy'] = {
        Head  = 'Shep. Bonnet',
		Ears  = 'Beastly Earring',
		Body  = 'Austere Robe',			-- Needed to offset the "possible" Vermillion Cloak	
		Legs  = 'Evoker\'s Spats',
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
	
	['Pet_Evasion'] = {
		Legs  = 'Shep. Hose',
    },
	
	['Idle_With_Pet'] = {
        Head  = { 'Austere Hat', 'Shep. Bonnet', 'Silver Hairpin' },
		Body  = { 'Vermillion Cloak//CARBY', 'Austere Robe', 'Seer\'s Tunic', 'Angler\'s Tunica' },
        Hands = 'Carbuncle Mitts//CARBY',
        Rings = 'Evoker\'s Ring',
    },
	
--[[
	The Idle_Regen and Idle_Refresh gear sets replace the normal Idle set when the player's HP or MP
	go below a set percentage (defined in gcinclude.lua, but can be overriden in profile.OnLoad function).
--]]
	
	['Idle_Regen'] = {
	},
	
	['Idle_Refresh'] = {
        Body = 'Vermillion Cloak',
	},
	
--[[
	When you are resting (kneeling down), if your HP is not full, your HP 'Resting' set will be 
	equipped. If your MP is below maximum value, your MP 'Resting_Refresh' gear set will be equipped. 
	Regardless of which set is equipped, if you have a Dark/Pluto staff accessible, it will be
	equipped automatically.
--]]
	
	['Resting_Regen'] = {
        Waist = 'Hierarch Belt',
    },
	
	['Resting_Refresh'] = {
		Main  = { 'Dark Staff', 'Kukulcan\'s Staff', 'Pilgrim\'s Wand' },
        Body  = { 'Vermillion Cloak', 'Seer\'s Tunic' },
		Waist = 'Hierarch Belt',
	},
	
	-- If you have any Spell Interruption Rate down gear, put them into the "SIR" gear set.
	-- This gear set is equipped in the HandleMidcast function that all spells go through.
	['SIR'] = {
	},
	
	-- Blood pacts go through a simulated process that mimics spell casting. The precast
	-- happens when the blood pack is invoked (either rage or ward), loading the 'BP'
	-- gear set. You want gear that has Blood Pact Ability Delay, Blood Pact Recast, and
	-- avatar perpetuation cost abilities defined here. The midcast happens when the
	-- actual blood pact goes off.
	['BP'] = {
        Head  = 'Austere Hat',
        Neck = 'Smn. Torque',
        Body  = 'Austere Robe',
        Hands = 'Carbuncle Mitts//CARBY',
        Rings = 'Evoker\'s Ring',
		Feet = 'Summoner\'s Pgch.',
    },
	
--[[
	Rage blood pacts are devided by type: physical, magical, summoning skill, accuracy, 
	and hybrid. (Ward blood pacts do not have this type of distinction.) Each blood pact 
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

	-- Magical rage blood pact: pet magic attack burst, pet magical attack, pet magical
	-- accuracy, and blood Pact magical damage
	['SmnMagical'] = {
	    Head = 'Shep. Bonnet',
    },

	-- Summoning skill rage blood pact. 
	['SmnSkill'] = {
        Head = 'Austere Hat',
        Neck = 'Smn. Torque',
        Rings = 'Evoker\'s Ring',
    },
	
	-- Accuracy blood pact: pet accuracy, pet magic accuracy
    ['SmnAccuracy'] = {
        Head  = 'Shep. Bonnet',
		Ears  = 'Beastly Earring',
		Body  = 'Austere Robe',			-- Needed to offset the "possible" Vermillion Cloak	
		Legs  = 'Evoker\'s Spats',
    },
	
	-- Hybrid blood pact: 2x physical and 1x magical
    ['SmnHybrid'] = {
        Head  = 'Shep. Bonnet',
		Ears  = 'Beastly Earring',
		Body  = 'Austere Robe',			-- Needed to offset the "possible" Vermillion Cloak	
		Legs  = 'Evoker\'s Spats',
    },
	
--[[
	Start weapons are where you define what you want the first row of equipment to look like when you
	either log in as a SMN or you switch your main job to SMN. 
--]]

	['Start_Weapons'] = {
	    Main = { 'Light Staff', 'Kukulcan\'s Staff', 'Solid Wand', 'Yew Wand' },
		Ammo = { 'Fortune Egg' },
 	},
	
--[[
	Specify what you want to wear around town.
--]]
	
	['Town'] = {
        Head = 'Lilac Corsage',
		Body = { 'Ducal Aketon//AK:OMNI', 'Austere Robe' },
    },

--[[
	Damage reduction gear depends on the type of damage. The most common is physical, but there's times when
	you'll want to reduce magic damage or breath damage. The three gear sets are defined below. The correct
	one will be equipped depending on how DT is set. Please consider not including gear that doesn't have 
	any damage taken property so other wanted stats can shine through.
--]]

	['DT_Physical'] = {
	},
	
	['DT_Magical'] = {
        Ears  = 'Coral Earring',
    },
	
	['DT_Breath'] = { 
	},
	
--[[
	Magic accuracy gear
--]]
	
	['Macc'] = {							-- This is player magical accuracy/attack, pet macc/matt goes in SmnAccuracy
		Rings = 'Tamas Ring',
    },
	
--[[
	Magic Attack Bonus (MAB) is used for more than just spells, so it is broken out
--]]

	['MAB'] = {
		Neck = 'Uggalepih Pendant//MP.SUB.50P',
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
	
--[[
	Midshot is the second stage of a ranged shot. This is where you place Ranged Attack or Ranged 
	Damage gear
--]]

	['Midshot'] = {
    },
	
--[[
	Spells are a bit different. Each type of spell can have it's own enhancement gear as well as 
	stat based gear. (In some cases individual spells have special entries.) These sets do not 
	include elemental gear which is	dependent on day/weather/weapon skill.

	The first stage is Precast. This is where you place any Fast Cast, cast time reduction, and 
	quick cast gear 
--]]

	['Precast'] = {	
		Ears = 'Loquac. Earring',
	},
	
--[[
	The second stage is Midcast. This is where you'll want to equip magic attack, or magic enhancing 
	gear. (Magic Attack Bonus also happens here, but is broken out into it's own gear set. See MAB.)
--]]	

	['Midcast'] = {
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
	
	-- Dark: Dark Magic Skill. Currently only gear equippable by any job gives is applicable here. There's no gear that's 
	-- specific for SMN that gives any dark magic skill.
	['Dark'] = {
	},
	
	-- Divine: Divine Magic Skill.
	['Divine'] = {
	},
	
	-- Enfeebling: Enfeebling Magic Skill.
	['Enfeebling'] = {
	},
	
	-- Enhancing: There is no gear that a SMN can wear to enhance any magic spell. Leave the Enhancing gear sets empty.
	['Enhancing'] = {
	},
	
	-- Elemental: Elemental Magic Skill. Note: don't include elemental staves or elemental obis/gorgets here,
	-- that is done automatically in the HandlePrecast/HandleMidcast functions.
	['Elemental'] = {
	},
	
	-- Ninjitsu: There is no gear that a SMN can wear to add Ninjitsu skill. Leave the following two
	-- gear sets empty.
	['Ninjitsu'] = {
	},
	
	-- Summoning: Summoning Magic Skill, Avatar Perpetuation Cost, Blood Pact Ability Delay.
	['Summoning'] = {
		Head  = 'Austere hat',
        Neck = 'Smn. Torque',
		Body  = 'Austere Robe',
		Hands = { 'Errant Cuffs', 'Carbuncle Mitts' },
        Rings = 'Evoker\'s Ring',
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
	
--[[
	And some spells are special cases, so they have individual gears sets.
--]]
	
	-- Stoneskin: Stoneskin Enhancement, Mind, and Enhancing Magic Skill. Mind is 3x more important than enhancing
	-- Magic Skill. The only piece of gear a SMN can wear to enhance stoneskin is a Stone Gorget. There's no gear
	-- that a SMN (or any job) can wear to enhance magic. Don't include mind gear here, that's already equipped.
	-- This gear set has no effect on Titan's Earthen Ward blood pact.
	['Stoneskin'] = {
	},	
	
	-- Drain: Drain Enhancement, Dark Magic Skill, Dark Magic Accuracy. Currently no gear equippable by a
	-- SMN enhances Drain. Drain is part of Dark Magic, so Potency which is based on dark magic skill will
	-- already be loaded in HandleMidcast function and need not be repeated here. No current gear supports
	-- dark magic accuracy for any job. Magic attack bonus and magic critical hit have no effect on potency.
	-- Leave the two Drain gear sets empty.
	['Drain'] = {
    },
	
	-- Aspir: Aspir Enhancement, Dark Magic Skill, Dark Magic Accuracy. Currently no gear equippable by a
	-- SMN enhances Aspir. Aspir is part of Dark Magic, so potency which is based on dark magic skill will
	-- already be loaded in HandleMidcast function and need not be repeated here. No current gear supports
	-- dark magic accuracy for any job. Magic attack bonus and magic critical hit have no effect on potency.
	-- Leave the two Aspir gear sets empty.
	['Aspir'] = {
    },
	
	-- Sneak: Enhances Sneak and Enhances Stealth. Currently only Dream Boots +1 enhances sneak and is equippable
	-- by any job. (Attained through the Starlight Celebration.) No gear for any job supports Enhances Stealth
	-- yet.	
	['Sneak'] = {
		Feet = 'Dream Boots +1',				-- enhances sneak
	},
	
	-- Invisible: Enhances Invisible Effect. Currently only Dream Mittens +1 enhances invisible and is equippable
	-- by any job. (Attained through the Starlight Celebration.)
	['Invisible'] = {
		Hands = 'Dream Mittens +1',				-- enhances invisibility
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
	
--[[
		* Skill based *
		
		Club: Starlight,Moonlight
--]]
	
	['WS_Skill'] = {
    },
	
--[[
	Movement tends to be used for kiting. Emphasis should be placed on gear that increases movement speed, but you 
	might also want gear that has evasion. The choice is yours.
--]]

	['Movement'] = { 
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
		
	['Pet_Attack'] = {					-- Pet's strength, not accuracy
	},
	
	['Pet_Macc'] = {					-- Pet's Magical Accuracy
		Head = 'Shep. Bonnet',
	},
	
	['Pet_Matt'] = {					-- Pet's Magical Attack
	},

	--* /DRK *--
	['WeaponBash'] = {
	},
	
	--* /DRG *--
	['Jumps'] = {		-- Jump and High Jump, Super is too high a level
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
	if string.find(gcinclude.SummonSkill,Pet.Name) ~= nil then
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
		elseif (gcinclude.SmnMagical:contains(PetAction.Name)) then		-- magical based blood pact?
			gcinclude.MoveToCurrent(sets.SmnMagical,sets.CurrentGear);	
			-- If /acc flagged, load accuracy set (for magical accuracy)
			if gcdisplay.GetToggle('Acc') == true then
				gcinclude.MoveToCurrent(sets.SmnAccuracy,sets.CurrentGear);			
			end
		elseif (gcinclude.SmnHybrid:contains(PetAction.Name)) then		-- hybrid blood pact (2x physical, 1x magical)?
			gcinclude.MoveToCurrent(sets.SmnHybrid,sets.CurrentGear);		
			-- lastly check on accuracy
			if gcdisplay.GetToggle('Acc') == true then
				gcinclude.MoveToCurrent(sets.SmnAccuracy,sets.CurrentGear);
			end				
		else																-- physical	blood pact
			-- if /acc flagged, load accuracy set (for physical accuracy)
			gcinclude.MoveToCurrent(sets.SmnPhysical,sets.CurrentGear);			
			if gcdisplay.GetToggle('Acc') == true then
				gcinclude.MoveToCurrent(sets.SmnAccuracy,sets.CurrentGear);
			end
		end
	else
		-- Must be a /BST charmed pet.
		if (gcinclude.BstPetAttack:contains(PetAction.Name)) then				-- Pet Attack
			gcinclude.MoveToCurrent(sets.Pet_Attack,sets.CurrentGear);		
			-- If /acc enabled equip pet accuracy gear
			if gcdisplay.GetToggle('acc') == true then
				gcinclude.MoveToCurrent(sets.Pet_Accuracy,sets.CurrentGear);
			end
		elseif (gcinclude.BstPetMagicAttack:contains(PetAction.Name)) then		-- Pet Magical Attack
			gcinclude.MoveToCurrent(sets.Pet_Matt,sets.CurrentGear);		
		elseif (gcinclude.BstPetMagicAccuracy:contains(PetAction.Name)) then	-- Pet Magical Accuracy Attack
			gcinclude.MoveToCurrent(sets.Pet_Macc,sets.CurrentGear);		
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
	gcinclude.settings.bWSOverride = true;
	
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
		if petAction ~= nil and (string.find(gcinclude.SummonSkill,sLName) ~= nil) then
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

		if string.find(gcinclude.SummonSkill,pet.Name) ~= nil then
			if eWeap ~= nil and (eWeap ~= gcinclude.elemental_staves[pEle][1] or eWeap ~= gcinclude.elemental_staves[pEle][3]) then
				gcinclude.SwapToStave(pEle,true,sets.CurrentGear);
			end
		end
	end
	
	-- The default set is the TP gear set. Load it up
	gcinclude.MoveToCurrent(sets.TP,sets.CurrentGear);
	
	if pet == nil then
		gcinclude.MoveToCurrent(sets.TP_No_Pet,sets.CurrentGear);
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
					else
						gcinclude.MoveToCurrent(sets.Evasion,sets.CurrentGear);
					end
				end
			elseif cKey == 'E' then		-- Accuracy
				if gcdisplay.GetToggle('Acc') == true then 
					if pet ~= nil and pet.Status == 'Engaged' then
						gcinclude.MoveToCurrent(sets.Pet_Accuracy,sets.CurrentGear);					
					else
						gcinclude.MoveToCurrent(sets.Accuracy,sets.CurrentGear);
					end
				end
			elseif cKey == 'F' then		-- Kiting
				if (gcdisplay.GetToggle('Kite') == true) then
					gcinclude.MoveToCurrent(sets.Movement,sets.CurrentGear);
				end
			elseif cKey == 'G' then		-- common buffs/debuffs
				gcinclude.CheckCommonDebuffs(sets.CurrentGear);
			elseif cKey == 'H' then		-- Damage Taken gear
				if (gcdisplay.GetCycle('DT') ~= gcinclude.OFF) then
					if gcdisplay.GetCycle('DT') == 'Physical' then
						gcinclude.MoveToCurrent(sets.DT_Physical,sets.CurrentGear);
					elseif gcdisplay.GetCycle('DT') == 'Magical' then
						gcinclude.MoveToCurrent(sets.DT_Magical,sets.CurrentGear);
					elseif gcdisplay.GetCycle('DT') == 'Breath' then
						gcinclude.MoveToCurrent(sets.DT_Breath,sets.CurrentGear);
					end
				end
			end
		end
	elseif player.Status == 'Resting' then	
		-- Player kneeling. Priority (low to high): regen, refresh
		
		if player.HP < player.MaxHP then		
			gcinclude.MoveToCurrent(sets.Resting_Regen,sets.CurrentGear);
		end

		if player.MP < player.MaxMP then
			gcinclude.MoveToCurrent(sets.Resting_Refresh,sets.CurrentGear);
		end
	
		-- Check for common debuffs
		gcinclude.CheckCommonDebuffs(sets.CurrentGear);
	else
		-- Assume idling. There's no idle set, just idle conditions
		
		-- See if in a town
		if (zone.Area ~= nil and table.find(gcinclude.Towns,zone.Area)) then
			gcinclude.MoveToCurrent(sets.Town,sets.CurrentGear);			
		else
			-- Check to see if there's an avatar							
			if pet ~= nil then	
				pet.Name = string.lower(pet.Name);		
				if string.find(gcinclude.SummonSkill,pet.Name) ~= nil then
					gcinclude.MoveToCurrent(sets.Idle_With_Pet,sets.CurrentGear);
				end
			end
		end
		
		-- if the player's HP is below the threshold setting, equip the idle regen gear
		if player.HPP < gcinclude.settings.RegenGearHPP then
			gcinclude.MoveToCurrent(sets.Idle_Regen,sets.CurrentGear);
		end
		-- if the player's MP is below the threshold setting, equip the idle refresh gear
		if player.MPP < gcinclude.settings.RefreshGearMPP then
			gcinclude.MoveToCurrent(sets.Idle_Refresh,sets.CurrentGear);
		end
		-- Check for common debuffs
		gcinclude.CheckCommonDebuffs(sets.CurrentGear);
	end
	
	-- Make sure to equip the appropriate elemental staff for the current pet
	if (pet ~= nil) then
		local pName = string.lower(pet.Name);	
		if string.find(gcinclude.SummonSkill,pName) ~= nil then
			local pEle = gcinclude.SummonStaves[pet.Name];		
			gcinclude.SwapToStave(pEle,false,sets.CurrentGear);
		end
	end
	
	-- And make sure a weapon equipped. (Going into a capped area can cause no weapon to be equipped.)
	local gear = gData.GetEquipment();
	if gear.Main == nil then
		gcinclude.MoveToCurrent(sets.Start_Weapons,sets.CurrentGear,true);
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
	elseif string.match(ability.Name, 'Charm') then
		-- Trying to charm a beast. 
		gcinclude.MoveToCurrent(sets.Charm,sets.CurrentGear);
		gcinclude.SwapToStave('light',false,sets.CurrentGear);	
	elseif string.match(ability.Name, 'Weapon Bash') then		-- assumes /drk
		gcinclude.MoveToCurrent(sets.WeaponBash,sets.CurrentGear);	
	elseif string.find(ability.Name, 'Jump') then		-- assumes /drk
		gcinclude.MoveToCurrent(sets.Jumps,sets.CurrentGear);		
	else
		-- Since we got here, the action has to be a SMN Blood Pact
		gcinclude.MoveToCurrent(sets.BP,sets.CurrentGear);
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
			bShow = true;
		elseif string.match(item.Name, 'Silent Oil') then
			gcinclude.MoveToCurrent(sets.Sneak,sets.CurrentGear);
			bShow = true;
		elseif string.match(item.Name, 'Prism Powder') then
			gcinclude.MoveToCurrent(sets.Invisible,sets.CurrentGear);
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

	-- Clear out the CurrentGear in case of leftovers
	gcinclude.ClearSet(sets.CurrentGear);
	
	-- Equip the precast gear set
	gcinclude.MoveToCurrent(sets.Precast,sets.CurrentGear);	
		
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

	if gcdisplay.GetToggle('GSwap') == false then		-- Only gear swap if this flag is true	
		return;
	end

	-- Clear out the CurrentGear in case of leftovers
	gcinclude.ClearSet(sets.CurrentGear);
	
	-- Call the common HandleMidcast now
	gcinclude.HandleMidcast();
	
	gcinclude.EquipTheGear(sets.CurrentGear);		-- Equip the composited midcast set
end		-- gcinclude.HandleMidcast

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
	
	-- Equip the composited Midshot set
	gcinclude.EquipTheGear(sets.CurrentGear);
end		-- HandleMidshot

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

	-- Call the common weaponskill handler
	gcinclude.HandleWeaponskill();
	
	-- Equip the composited weaponskill set
	gcinclude.EquipTheGear(sets.CurrentGear);
end		-- HandleWeaponskill

return profile;