local profile = {};
gcinclude = gFunc.LoadFile('common\\gcinclude.lua');

--[[
	This file contains all the gear sets associated with the THF job. While it isn't outside of the realm of 
	possibility that the subjob might be able to use gear sets too, that is not the emphasis of this program. 
	It is tailored to handle all the aspects of THF. If you desire a gear set change to strengthen an ability
	from your subjob that is not supported by this program, you probably will have to make a custom gear set 
	and use the /gearset command to use it.
	
	Gear Sets last updated: June 21, 2024
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
	fighting Luashitacast!	Also, not all sets need to be defined. There is nothing wrong with leaving a set "empty", but don't delete any
	of the sets. All the ones listed here (except for any custom sets) are expected to exist by Luashitacast.
		
	*** Note ***
	Unlike when summoner is used as a subjob, /bst's pets are charmed at the max level of your BST or the level
	of your PLD, whichever is lower. That means you can charm higher level mobs than you would expect with /bst.
	Just note though that you can't have two pets, so if you have charmed a pet with /bst, you can't summon your
	avatar and visa versa.
	
	*** Note 2 ***
	No gear that supports bard songs can be worn by any job except a bard, no there's no support given here for
	/BRD.	
--]]

--[[
	The TP sets are used when you are fighting. The accuracy set will be used if /acc is specified
	and the evasion set if /eva is specified. Please note that if you have a subjob that can use a
	pet, none of the abilities are supported here. Only main jobs that have pets (SMN,BST) support
	pet actions.
--]]

	['TP'] = {
        Head  = 'Empress Hairpin',
        Neck  = { 'Peacock Amulet', 'Spike Necklace' },
        Ears  = { 'Fang Earring', 'Fang Earring', 'Physical Earring', 'Reraise Earring', 'Energy Earring +1//MSJ', 'Energy Earring +1//MSJ' },
        Body  = { 'Rapparee Harness', 'Brigandine', 'Mrc.Cpt. Doublet', 'Beetle Harness', 'Angler\'s Tunica' },
        Hands = 'Battle Gloves',
        Rings = { 'Kshama Ring No.2', 'Balance Ring', 'Courage Ring' },
        Back  = { 'Raptor Mantle', 'Ram Mantle' },
        Waist = { 'Swift Belt', 'Mrc.Cpt. Belt' },
        Legs  = { 'Noct Brais', 'Wonder Braccae', 'San. Trousers', 'Ryl.Ftm. Trousers' },
        Feet  = 'Bounding Boots',
    },

	['Tank_TP'] = {
        Head  = 'Empress Hairpin',
        Neck  = { 'Peacock Amulet', 'Spike Necklace' },
        Ears  = { 'Physical Earring', 'Reraise Earring', 'Energy Earring +1//MSJ', 'Energy Earring +1//MSJ' },
        Body  = { 'Rapparee Harness', 'Brigandine', 'Mrc.Cpt. Doublet', 'Beetle Harness', 'Angler\'s Tunica' },
        Hands = 'Battle Gloves',
        Rings = { 'Kshama Ring No.2', 'Balance Ring', 'Courage Ring' },
        Back  = { 'Raptor Mantle', 'Ram Mantle' },
        Waist = { 'Swift Belt', 'Mrc.Cpt. Belt' },
        Legs  = { 'Noct Brais', 'Wonder Braccae', 'San. Trousers', 'Ryl.Ftm. Trousers' },
        Feet  = 'Bounding Boots',	
	},
	
--[[
	If an accuracy emphasis is desired, the following set will replace the gear appropriately.
	Remember that DEX converts to accuracy: (horizon) for every 1 point of DEX you get 
	0.70 points of accuracy if wielding a 2H weapon, 0.65 for a 1H weapon, and 0.60 for H2H. 
	Tank_Accuracy is a subset of Accuracy. It lets you specify what accuracy gear to equip 
	that doesn't compromise your tanking set as much as a full-blown accuracy set would.
--]]
	
	['Accuracy'] = {
        Head  = 'Empress Hairpin',
        Neck  = { 'Peacock Amulet', 'Spike Necklace' },
        Body  = { 'Brigandine', 'Mrc.Cpt. Doublet' },
        Hands = 'Battle Gloves',
        Rings = { 'Woodsman Ring', 'Jaeger Ring', 'Kshama Ring No.2', 'Balance Ring' },
        Waist = { 'Tilt Belt', 'Mrc.Cpt. Belt' },
        Feet  = 'Bounding Boots',
    },

	['Tank_Accuracy'] = {
        Head  = 'Empress Hairpin',
        Neck  = { 'Peacock Amulet', 'Spike Necklace' },
        Body  = { 'Brigandine', 'Mrc.Cpt. Doublet' },
        Hands = 'Battle Gloves',
        Rings = { 'Woodsman Ring', 'Jaeger Ring', 'Kshama Ring No.2', 'Balance Ring' },
        Waist = { 'Tilt Belt', 'Mrc.Cpt. Belt' },
        Feet  = 'Bounding Boots',	
	},
	
--[[
	If evasion wanted, equip evasion gear
--]]
	
	['Evasion'] = {
        Head  = 'Empress Hairpin',
        Ears  = { 'Reraise Earring', 'Genin Earring//SJNIN', 'Drone Earring' },
        Body  = 'Mrc.Cpt. Doublet',
        Hands = 'Battle Gloves',
        Waist = 'Mrc.Cpt. Belt',
        Legs  = 'San. Trousers',
        Feet  = 'Bounding Boots',
    },

	['Tank_Evasion'] = {
        Head  = 'Empress Hairpin',
        Ears  = { 'Reraise Earring', 'Genin Earring//SJNIN', 'Drone Earring' },
        Body  = 'Mrc.Cpt. Doublet',
        Hands = 'Battle Gloves',
        Waist = 'Mrc.Cpt. Belt',
        Legs  = 'San. Trousers',
        Feet  = 'Bounding Boots',	
	},
	
--[[
	The Idle_Regen and Idle_Refresh gear sets replace the normal Idle set when the player's HP or MP
	go below a set percentage (defined in gcinclude.lua, but can be overriden in profile.OnLoad function).
--]]
	
	['Idle_Regen'] = {
	},
	
	['Idle_Refresh'] = {
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
	},
	
	['Resting_Refresh'] = {
	},
	
	-- If you have any Spell Interruption Rate down gear, put them into the "SIR" gear set.
	-- This gear set is equipped in the HandleMidcast function that all spells go through.
	-- Currently only gear equippable by any job is applicable here. There's no gear that's 
	-- specific for THF that gives spell interruption rate down.
	['SIR'] = {
	},
	
--[[
	Start weapons are where you define what you want the first row of equipment to look like when you
	either log in as a SMN or you switch your main job to SMN. Any other gear you mention will be overridden
	by the Idle or Town set, so no need to include here.
--]]

	['Start_Weapons'] = {
		Main  = { 'Corsair\'s Knife', 'Bone Knife' },
		Sub   = { 'Hornetneedle', 'Marauder\'s Knife//ACCESSIBLE' },
		Range = 'Thug\'s Zamburak',
		Ammo  = 'Acid Bolt',
    },
	
--[[
	Specify what you want to wear around town.
--]]
	
	['Town'] = {
		Head = 'Lilac Corsage',
		Body = 'Ducal Aketon//AK:OMNI',			
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
    },
	
	['DT_Breath'] = { 
	},
	
--[[
	Magic accuracy gear
--]]

	['Macc'] = {
        Ring1 = 'Tamas Ring',
    },

--[[
	Magic Attack Bonus (MAB) is used for more than just spells, so it is broken out
--]]

	['MAB'] = {
	},
	
--[[
	Preshot is the first stage of when a ranged shot is being performed. This is where you place any 
	gear that reduces the time it takes to shoot (snap shot, rapid shot, haste).
--]]

	['Preshot'] = {
    },
	
--[[
	Midshot is the second stage of a ranged shot. This is where you place Ranged Accuracy, Ranged 
	Attack or Ranged Damage gear.
--]]

	['Midshot'] = {
		Neck  = 'Peacock Amulet',
		Body  = 'Rapparee Harness',
		Rings = { 'Jaeger Ring', 'Beetle Ring +1', 'Beetle Ring +1' },
		Legs  = 'Noct Brais',
    },

--[[
	Spells are a bit different. Each type of spell can have it's own enhancement gear as well as 
	stat based gear. (In some cases individual spells have special entries.) These sets do not 
	include elemental gear which is	dependent on day/weather/weapon skill.

	The first stage is Precast. This is where you place any Fast Cast, cast time reduction, quick 
	cast gear, and spell interruption rate
--]]

	['Precast'] = {							
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
	listed have gear that a THF or anyone can wear.
--]]

	-- Healing: Healing Magic Skill. Currently only a Healing Earring affects healing spells from a sub job. No other gear
    -- gives bonuses to Healing magic from a sub job. Also, gear with MND bonuses will boost cure spell's potency, but MND 
	-- gear is automatically equipped prior to the Healing set being equipped in the HandleMidcast function. There's no need 
	-- to include MND gear here.
	['Healing'] = {
    },
	
	-- Dark: Dark Magic Skill. Currently only gear equippable by any job gives is applicable here. There's no gear that's 
	-- specific for THF that gives any dark magic skill.	
	['Dark'] = {
    },

	-- Divine: Divine Magic Skill.	
	['Divine'] = {
	},
	
	-- Enfeebling Magic Skill.	Currently only gear equippable by any job gives is applicable here. There's no gear that's 
	-- specific for THF that gives any enfeebling magic skill.	
	['Enfeebling'] = {
	},
	
	-- Enhancing: There is no gear that a THF can wear to enhance any magic spell. Leave the Enhancing gear sets empty.
	['Enhancing'] = {
	},
	
	-- Elemental: Elemental Magic Skill. Currently only gear equippable by any job gives is applicable here. There's no gear
	-- that's specific for THF that gives any elemental magic skill. Note: don't include elemental staves or elemental 
	-- obis/gorgets here, that is done automatically in the HandlePrecast/HandleMidcast functions (if /wswap is enabled).
	['Elemental'] = {
	},

	-- Ninjutsu: There is no gear that a THF can wear to add Ninjutsu skill. Leave the following two
	-- gear sets empty.	
	['Ninjutsu'] = {			-- Ninjutsu Skill, magic burst bonus, magic attack bonus
	},
	
	-- Summoning: Summoning Magic Skill and Avatar Perpetuation Cost. Currently only gear equippable by any job gives
	-- is applicable here. There's no gear that's specific for THF that gives any summoning skill. Note: currently on 
	-- HorizonXI summoning skills are ignored. Any gear piece that only gives summoning skill will be commented out	
	['Summoning'] = {
	},
	
--[[
	Next is stat-based gear, (in this case intelligence or mind)
--]]

	['INT'] = {
        Rings = 'Tamas Ring',
        Waist = 'Mrc.Cpt. Belt',
        Feet = 'Mannequin Pumps',
    },

	['Tank_INT'] = {
        Rings = 'Tamas Ring',
        Waist = 'Mrc.Cpt. Belt',
        Feet = 'Mannequin Pumps',	
	},
	
	['MND'] = {
        Neck = { 'Promise Badge', 'Justice Badge' },
        Body = 'Wonder Kaftan',
        Rings = { 'Tamas Ring', 'Kshama Ring No.9', 'Tranquility Ring' },
        Waist = 'Mrc.Cpt. Belt',
        Legs = 'Wonder Braccae',
        Feet = 'Mannequin Pumps',
    },

	['Tank_MND'] = {
        Neck = { 'Promise Badge', 'Justice Badge' },
        Body = 'Wonder Kaftan',
        Rings = { 'Tamas Ring', 'Kshama Ring No.9', 'Tranquility Ring' },
        Waist = 'Mrc.Cpt. Belt',
        Legs = 'Wonder Braccae',
        Feet = 'Mannequin Pumps',	
	},
	
--[[
	And some spells are special cases, so they have individual gears sets.
--]]
	
	-- Stoneskin: Stoneskin Enhancement, Mind, and Enhancing Magic Skill. Mind is 3x more important than enhancing
	-- Magic Skill. The only piece of gear a THF can wear to enhance stoneskin is a Stone Gorget. There's no gear
	-- that a THF (or any job) can wear to enhance magic. Note: This gear set has no effect on Titan's Stoneskin
	-- blood pact.
	['Stoneskin'] = {
	},
	
	-- Drain: Drain Enhancement, Dark Magic Skill, Dark Magic Accuracy. Currently no gear supports Drain enhancement.
	-- Drain is part of Dark Magic, so Potency which is based on dark magic skill will already be loaded in HandleMidcast 
	-- function and need not be repeated here. No current gear supports dark magic accuracy for any job. Magic attack 
	-- bonus and magic critical hit have no effect on potency. Leave the two Drain gear sets empty.
	['Drain'] = {
    },
	
	-- Aspir: Aspir Enhancement, Dark Magic Skill, Dark Magic Accuracy. Currently no gear equippable by a
	-- THF enhances Aspir. Aspir is part of Dark Magic, so potency which is based on dark magic skill will
	-- already be loaded in HandleMidcast function and need not be repeated here. No current gear supports
	-- dark magic accuracy for any job. Magic attack bonus and magic critical hit have no effect on potency.
	-- Leave the two Aspir gear sets empty.
	['Aspir'] = {
    },
	
	-- Sneak: Enhances Sneak and Enhances Stealth. Currently on Dream Boots +1 enhances sneak and is equippable
	-- by any job. (Attained through the Starlight Celebration.) No gear for any job supports Enhances Stealth
	-- yet.
	['Sneak'] = {
		Feet = 'Dream Boots +1',					-- Enhances sneak
	},
	
	-- Invisible: Enhances Invisible Effect. Currently only Dream Mittens +1 enhances invisible and is equippable
	-- by any job. (Attained through the Starlight Celebration.)	
	['Invisible'] = {
		Hands = 'Dream Mittens +1',					-- Enhances invisibility
	},
	
	-- Note: Phalanx does have gear that supports the spell, but it is out of era
	
--[[
	The following weapon skill gearsets are defined by the stat they emphasize. Listed are all of the sets that
	you will need to use every weapon skill that your job can do. The leading comment defines what weapon/weapon
	skill combination the set applies to.
	
	THF can use the following weapons: Dagger (A-), Sword (D), Club (E), H2H (E), Marksmanship (C+), Archery (C-)
	Any other weapon will have no weaponskill available. Weapon skill sets are named based on stat(s) used, 
	regardless of weapon
--]]

--[[
		* Strength based *
		
		Sword: Flat Blade,Circle Blade,Vorpal Blade,Spirits Within,Mercy Stroke
		Club: Starlight,Skull Breaker,True Strike
		H2H: Spinning Attack
-]]
	
	['WS_STR'] = {
        Head = 'Mrc.Cpt. Headgear',
        Neck = 'Spike Necklace',
        Body = 'Wonder Kaftan',
        Hands = 'Wonder Mitts',
        Rings = 'Courage Ring',
        Waist = 'Mrc.Cpt. Belt',
        Legs = 'Wonder Braccae',
        Feet = 'Wonder Clomps',
    },

--[[
		* Strength and Agility based *
		
		Archery: Flaming Arrow^,Piercing Arrow^,Dulling Arrow^,Sidewinder^
		
		^ Subjob must be RNG
--]]

	['WS_STRAGI'] = {
        Head = 'Mrc.Cpt. Headgear',
        Neck = 'Spike Necklace',
        Ears = { 'Genin Earring//SJNIN', 'Drone Earring' },
        Body = 'Wonder Kaftan',
        Hands = 'Wonder Mitts',
        Rings = 'Courage Ring',
        Waist = 'Mrc.Cpt. Belt',
        Legs = 'Wonder Braccae',
        Feet = 'Bounding Boots',
    },
	
--[[
		* Strength and Dexterity based, even weighting *
		
		Sword: Fast Blade
		H2H: Combo,Backhand Blow,Raging Fist^
		
		^ Subjob must be MNK
--]]

	['WS_STRDEX'] = {
        Head  = 'Empress Hairpin',
        Neck  = 'Spike Necklace',
        Body  = 'Brigandine',
        Hands = 'Wonder Mitts',
        Rings = { 'Kshama Ring No.2', 'Balance Ring', 'Courage Ring' },
        Waist = 'Mrc.Cpt. Belt',
        Legs  = { 'Wonder Braccae', 'Noct Brais' },
        Feet  = 'Bounding Boots',
    },

--[[
		* Strength and Intelligence based, even weighting *
		
		Sword: Burning Blade,Red Lotus Blade
--]]
	
	['WS_STRINT'] = {
        Head = 'Empress Hairpin',
        Neck = 'Spike Necklace',
        Body = 'Wonder Kaftan',
        Hands = 'Wonder Mitts',
        Rings = { 'Tamas Ring', 'Courage Ring' },
        Waist = 'Mrc.Cpt. Belt',
        Legs = 'Wonder Braccae',
        Feet = 'Wonder Clomps',
    },

--[[
		* Strength and Mind based, even weighting *
		
		Sword: Shining Blade,Seraph Blade
		Club: Shining Strike
--]]

	['WS_STRMND'] = {
        Head = 'Mrc.Cpt. Headgear',
        Neck = { 'Promise Badge', 'Justice Badge' },
        Body = 'Wonder Kaftan',
        Hands = 'Wonder Mitts',
        Rings = { 'Tamas Ring', 'Kshama Ring No.9', 'Courage Ring', 'Tranquility Ring' },
        Waist = 'Mrc.Cpt. Belt',
        Legs = 'Wonder Braccae',
        Feet = 'Wonder Clomps',
    },

--[[
		* Agility based *
		
		Marksmanship: Hot Shot^,Split Shot^,Sniper Shot^,Slug Shot^
		
		^ Subjob must be RNG
--]]

	['WS_AGI'] = {
        Head = 'Empress Hairpin',
        Ears = { 'Genin Earring//SJNIN', 'Drone Earring' },
        Body = 'Mrc.Cpt. Doublet',
        Waist = 'Mrc.Cpt. Belt',
        Feet = 'Bounding Boots',
    },
	
--[[
		* Charisma based *
		
		Dagger: Shadowstitch
--]]
	
	['WS_CHR'] = {
        Head = 'Entrancing Ribbon',
        Neck = 'Flower Necklace',
        Waist = 'Corsette',
    },

--[[
		* Dexterity based *
		
		Dagger: Wasp Sting,Viper Bite,Eviseration
--]]
	
	['WS_DEX'] = {
        Head  = 'Empress Hairpin',
        Neck  = 'Spike Necklace',
        Body  = 'Brigandine',
        Rings = { 'Kshama Ring No.2', 'Balance Ring' },
        Waist = 'Mrc.Cpt. Belt',
		Legs  = 'Noct Brais',
        Feet  = 'Bounding Boots',
    },

--[[
		* Dexterity and Agility based *
		
		Dagger: Shark Bite
--]]
	
	['WS_DEXAGI'] = {
        Head  = 'Empress Hairpin',
        Neck  = 'Spike Necklace',
        Ears  = { 'Genin Earring//SJNIN', 'Drone Earring' },
        Body  = 'Brigandine',
        Rings = { 'Kshama Ring No.2', 'Balance Ring' },
        Waist = 'Mrc.Cpt. Belt',
		Legs  = 'Noct Brais',
        Feet  = 'Bounding Boots',
    },
	
--[[
		* Dexterity and Charisma based *
		
		Dagger: Dancing Edge
--]]
	
	['WS_DEXCHR'] = {
        Head  = 'Empress Hairpin',
        Neck  = 'Spike Necklace',
        Body  = 'Mrc.Cpt. Doublet',
        Rings = { 'Kshama Ring No.2', 'Balance Ring' },
        Waist = 'Corsette',
		Legs  = 'Noct Brais',
        Feet  = 'Bounding Boots',
    },
	
--[[
		* Dexterity and Intelligence based *
		
		Dagger: Gust Slash,Cyclone
--]]
	
	['WS_DEXINT'] = {
        Head  = 'Empress Hairpin',
        Neck  = 'Spike Necklace',
        Body  = 'Brigandine',
        Rings = { 'Tamas Ring', 'Kshama Ring No.2', 'Balance Ring' },
        Waist = 'Mrc.Cpt. Belt',
		Legs  = 'Noct Brais',
        Feet  = 'Bounding Boots',
    },

--[[
		* Mind based *
		
		Dagger: Energy Steal,Energy Drain
--]]

	['WS_MND'] = {
        Neck  = { 'Promise Badge', 'Justice Badge' },
        Body  = 'Brigandine',
        Rings = { 'Tamas Ring', 'Kshama Ring No.9', 'Tranquility Ring' },
        Waist = 'Mrc.Cpt. Belt',
        Legs  = 'Wonder Braccae',
        Feet  = 'Mannequin Pumps',
    },
	
--[[
		* Vitality based *
		
		H2H: Shoulder Tackle,One Inch Punch^
		
		^ Subjob must be MNK
--]]

	['WS_VIT'] = {
        Body  = 'Brigandine',
        Waist = 'Warrior\'s Belt',
        Legs  = { 'Wonder Braccae', 'San. Trousers', 'Ryl.Ftm. Trousers' },
    },

	['WS_Skill'] = {
    },

--[[
		* HP based *
		
		Sword: Spirits Within
--]]

	['WS_HP'] = {
        Ears  = 'Physical Earring',
        Body  = 'Wonder Kaftan',
        Hands = 'Wonder Mitts',
        Waist = 'Powerful Rope',
        Legs  = 'Wonder Braccae',
    },
	
--[[
	Movement tends to be used for kiting. Emphasis should be placed on gear that increases movement speed, but you 
	might also want gear that has evasion. The choice is yours.
--]]

	['Movement'] = { 
	},

--[[
	The following are abilities affected by gear
--]]
	['Perfect Dodge'] = {
	},
	
	['Steal'] = {
	},
	
	['SneakAttack'] = {
        Head  = 'Empress Hairpin',
        Neck  = 'Spike Necklace',
        Body  = { 'Brigandine', 'Mrc.Cpt. Soublet' },
        Rings = { 'Kshama Ring No.2', 'Balance Ring' },
        Waist = 'Mrc.Cpt. Belt',
		Legs  = 'Noct Brais',
        Feet  = 'Bounding Boots',	
	},
	
	['Flee'] = {
	},

	['TrickAttack'] = {
        Head = 'Empress Hairpin',
        Ears = { 'Genin Earring//SJNIN', 'Drone Earring' },
        Body = 'Mrc.Cpt. Doublet',
        Waist = 'Mrc.Cpt. Belt',
        Feet = 'Bounding Boots',	
	},
	
	['Mug'] = {
	},
	
	['Bully'] = {
	},
	
	['Hide'] = {
	},
	
	['Accomplice'] = {
	},
	
	['Collaborator'] = {
	},
	
--[[
	Some subjobs really make no sense when combined with dragoon, but all abilities across all jobs that
	have gear that can be equipped by a PLD are included here.
--]]
	--* BST *--
	-- CHR and Charm + gear. (Every +1 Charm adds 5% Charm duration)
	['Charm'] = {
	    Head = 'Entrancing Ribbon',
        Neck = 'Flower Necklace',
        Waist = 'Corsette',
    },

	['Reward'] = {
	},
	
	['Tame'] = {						-- Remember that higher if your INT is higher than the target's INT, you're less likely to be resisted
	},
	
	['Pet_Attack'] = {					-- Pet's strength, not accuracy
	},
	
	['Pet_Macc'] = {					-- Pet's Magical Accuracy
	},
	
	['Pet_Matt'] = {					-- Pet's Magical Attack
	},
	
	--* /DRK *--
	['ArcaneCircle'] = {
	},
	
	['Last_Resort'] = {
	},
	
	['WeaponBash'] = {
	},
	
	['Souleater'] = {
	},

	--* /WAR *--
	['Provoke'] = {
	},
	
	['Berserk'] = {
	},
	
	['Defender'] = {
	},
	
	['Warcry'] = {
	},
	
	--* /MNK *--
	['Boost'] = {
	},
	
	['Focus'] = {
	},
	
	['Dodge'] = {
	},
	
	['Chakra'] = {
	},

	--* /WHM *--
	['DivineSeal'] = {
	},
	
	--* /BLM *--
	['ElementalSeal'] = {
	},

	--* /RDM *--
	-- No skills
	
	--* /DRK *--
	['ArcaneCircle'] = {
	},
	
	['LastResort'] = {
	},
	
	['WeaponBash'] = {
	},

	['Souleater'] = {
	},

	--* /BRD *--
	-- No skills
	
	--* /PLD *--
	['HolyCircle'] = {
    },
	
	['ShieldBash'] = {
    },
	
	['Sentinel'] = {
    },

	['Cover'] = {
    },

	--* /RNG *--
	['Sharpshot'] = {
	},
	
	['Scavenge'] = {
	},
	
	['Camouflage'] = {
	},
	
	['Barrage'] = {
	},

	--* /SAM *--
	['WardingCircle'] = {
	},
	
	['ThirdEye'] = {
	},
	
	['Hasso'] = {
	},
	
	['Meditate'] = {
	},
	
	['Seigan'] = {
	},
	
	--* /NIN *--
	-- No skills
	
	--* /DRG *--
	['AncientCircle'] = {
	},
	
	['Jump'] = {
	},

	['HighJump'] = {
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

-- There's no way to consistently identify the type of weapon you're currently
-- using by just looking at the name. (Ex: Maneater is an axe. The name does
-- not give that away.) The following table lists weapons by type that you're
-- likely to use. Add the weapon names accordingly. You only need the names of
-- the weapons if you want to conditionally equip an item with a weapon skill
-- attribute.
profile.WeaponType = {
	['DAGGER'] = { 'Thief\'s Knife', 'Corsair\'s Knife', 'Hornetneedle', 'Bone Knife', 'Marauder\'s Knife' },
	['CLUB']   = { 'Warp Cudgel' },
	['STAVE'] =  { 'Fire Staff', 'Vulcan\'s Staff', 'Ice Staff', 'Aquilo\'s Staff',
				  'Wind Staff', 'Auster\'s Staff', 'Earth Staff', 'Terra\'s Staff',
				  'Thunder Staff', 'Jupiter\'s Staff', 'Water Staff', 'Neptune\'s Staff',
				  'Light Staff', 'Apollo\'s Staff', 'Dark Staff', 'Pluto\'s Staff' },	
};

-- Accuracy Sets are predefined /acc commands. You identify them by a name and
-- a comma delimited list of slots. It's just a convenient shortcut mechanism.
profile.AccuracySet = {
	['base'] = 'Rings,Body',
};

-- RegionControlGear identifies a piece of gear that can be used to automatically
-- determine if the player is currently in a zone controlled by their nation or
-- not. The definition includes the name of the piece of gear, what slot does it
-- equip in (do not use RINGS or EARS, use RING1 and EAR1), whether HP or MP 
-- is affected, and whether the piece requires region ownership or not the region 
-- ownership.
profile.RegionControlGear = { 'Republican Gold Medal','Neck','MP',false };
profile.NeutralControlGear = 'Smn. Torque';	-- has to match RegionControlGear slot

profile.Sets = sets;
profile.sjb = nil;
profile.bAmmo = false;
profile.sAmmo = nil;

--[[
	HandlePetAction equips the appropriate gear set based on the type of action
	the pet is trying to perform.
--]]

local function HandlePetAction(PetAction)
	local pet = gData.GetPet();
	
	-- Only gear swap if this flag is true
	if gcdisplay.GetToggle('GSwap') == false or string.find(gcinclude.SummonSkill,pet.Name) ~= nil then
		return;
	end

	-- Only /BST pet attacks have associated gear sets because /smn pets are at most half the
	-- level of your BST level
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
	gcinclude.EquipTheGear(sets.CurrentGear);
end		-- HandlePetAction

--[[
	SetSubjobSet is used to pick the appropriate set for the loaded macrobook based on
	which subjob is current. (If no change has occurred since the last time it was called,
	nothing is checked/changed.)
--]]

local function SetSubjobSet(chkSJ)
	local subs = {['WAR'] = 0, ['MNK'] = 0, ['WHM'] = 0, ['BLM'] = 0, ['RDM'] = 0, ['THF'] = nil,
				 ['PLD'] = 0, ['DRK'] = 0, ['BST'] = 0, ['BRD'] = 0, ['RNG'] = 0, ['SMN'] = 0,
				 ['SAM'] = 0, ['NIN'] = 1, ['DRG'] = 0, ['BLU'] = 0, ['COR'] = 0, ['PUP'] = 0,
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
end		-- SetSubjobSet

--[[
	OnLoad is run whenever you log into your BST or change your job to BST
--]]

profile.OnLoad = function()
	local player = gData.GetPlayer();

	gSettings.AllowAddSet = true;
	gcinclude.Initialize();
	gcinclude.settings.RegenGearHPP = 50;
    gcinclude.settings.RefreshGearMPP = 60;
	
	-- Coded order of operation override
	gcinclude.settings.priorityEngaged = 'CEFGH';
	--gcinclude.settings.priorityMidCast = 'ABCDEFGH';
	gcinclude.settings.priorityWeaponSkill = 'ABDE';
	gcdisplay.SetToggle('Tank',false);		-- Assume THF is not a tank
	
	-- Set your job macro toolbar defaults here. Which set depends on the subjob
	AshitaCore:GetChatManager():QueueCommand(1, '/macro book 11');		-- THF
	SetSubjobSet(player.SubJob);
	
	-- Load up the weapons bar. (This need only be done once.)
	gcinclude.MoveToCurrent(sets.Start_Weapons,sets.CurrentGear);	
	gcinclude.EquipTheGear(sets.CurrentGear);
	
	-- Make sure the saved weapons are the starting weapons
	gcinclude.weapon = sets.CurrentGear['Main'];
	if sets.CurrentGear['Sub'] == nil then
		gcinclude.offhand = nil;
	else
		gcinclude.offhand = sets.CurrentGear['Sub'];
	end	
end		-- OnLoad

--[[
	OnUnload is run when you change to another job
--]]

profile.OnUnload = function()
	gcinclude.Unload();
end		-- OnUnload

--[[
	HandleCommand is run when you type in a command defined in LUASHITACAST. The commands handled here instead
	of in gcinclude.HandleCommands are specific to BST or the help system, which has been tailored to BST.
--]]

profile.HandleCommand = function(args)
	if args[1] == 'help' then
		gcdisplay.ShowHelp();
	elseif args[1] == 'petfood' then			-- Supported since pet food is not job specific, but very niche
		gcinclude.doPetFood(args[2],args[3]);
	else
		gcinclude.HandleCommands(args);
	end
end		-- HandleCommand

--[[
	HandleDefault is run when some action happens. This includes both actions by the player and by
	their pet.
--]]
	
profile.HandleDefault = function()
	local player = gData.GetPlayer();
	local pet = gData.GetPet();
	local petAction = gData.GetPetAction();	
	local ew = gData.GetEquipment();
	local zone = gData.GetEnvironment();	
	local eWeap = nil;
	local cKey;

	-- Only pet actions from BST are supported.
	if (petAction ~= nil and player.SubJob == 'BST') then
		HandlePetAction(petAction);
		return;
	end
	
	-- Save the name of the main weapon		
	if ew['Main'] ~= nil then
		eWeap = ew['Main'].Name;
	end;

	-- Make sure the macro set is shown and that the display on the top of the screen is correct
	-- in case the subjob was changed.	
	SetSubjobSet(player.SubJob);

	-- Only gear swap if this flag is true
	if gcdisplay.GetToggle('GSwap') == false then
		return;
	end

	-- Assuming you're /bst, when you want to reward your pet and you do not have pet food equipped 
	-- or when you want to summon a pet and a jug is not equipped, the current item in the ammo slot 
	-- is saved. The following will set it back to what you had before either of those two items 
	-- were equipped.
	if player.SubJob == 'BST' and profile.bAmmo then
		gFunc.ForceEquip('Ammo',profile.sAmmo);
		profile.sAmmo = nil;
		profile.bAmmo = false;
	end
	
	-- Clear out the CurrentGear in case of leftovers
	gcinclude.ClearSet(sets.CurrentGear);
	
	-- If player is not resting and has MP and has swapped weapons, set the weapon back to what 
	-- they had before the switch
	if player.Status ~= 'Resting' and 
			gcdisplay.GetToggle('WSwap') == true and
			gcinclude.weapon ~= nil and 
			eWeap ~= gcinclude.weapon then
		sets.CurrentGear['Main'] = gcinclude.weapon;
		sets.CurrentGear['Sub'] = gcinclude.offhand;
	end
	
	-- The default set is the TP gear set. Load it up
	gcinclude.MoveToCurrent(sets.TP,sets.CurrentGear);

	if bTank == true then
		gcinclude.MoveToCurrent(sets.Tank_TP,sets.CurrentGear);	
	end
	
	-- Now process the player status accordingly	
	if player.Status == 'Engaged' then
		gcinclude.settings.priorityEngaged = string.upper(gcinclude.settings.priorityEngaged);
		for i = 1,string.len(gcinclude.settings.priorityEngaged),1 do
			cKey = string.sub(gcinclude.settings.priorityEngaged,i,i);
			if cKey == 'C' then		-- Evasion			
				if gcdisplay.GetToggle('Eva') == true then
					if bTank == true then
						gcinclude.MoveToCurrent(sets.Tank_Evasion,sets.CurrentGear);
					else			
						gcinclude.MoveToCurrent(sets.Evasion,sets.CurrentGear);
					end				
				end			
			elseif cKey == 'E' then		-- Accuracy	
				gcinclude.FractionalAccuracy(sets.Accuracy,sets.Tank_Accuracy);
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
		-- Player kneeling. Priority (low to high): Resting,refresh	
		if player.HP < player.MaxHP then
			gcinclude.MoveToCurrent(sets.Resting_Regen,sets.CurrentGear);
		end
		
		if gcinclude.MagicalJob('S') == true and player.MP < player.MaxMP then	
			gcinclude.MoveToCurrent(sets.Resting_Refresh,sets.CurrentGear);
			gcinclude.SwapToStave('dark',false,sets.CurrentGear);
		end
		
		-- Check for common debuffs
		gcinclude.CheckCommonDebuffs(sets.CurrentGear);
	else									
		-- Assume idling. Priority (low to high): regen,refresh

		-- See if in a town		
		if (zone.Area ~= nil and table.find(gcinclude.Towns,zone.Area)) then	
			gcinclude.MoveToCurrent(sets.Town,sets.CurrentGear);
		else			
			if gcdisplay.GetToggle('Idle') == true then
				gcinclude.MoveToCurrent(sets.Travel,sets.CurrentGear);
				
				-- if the player's HP is below the threshold setting, equip the idle regen gear
				if player.HPP < gcinclude.settings.RegenGearHPP then
					gcinclude.MoveToCurrent(sets.Idle_Regen,sets.CurrentGear);
				end
		
				-- if the player's MP is below the threshold setting, equip the idle refresh gear
				if gcinclude.MagicalJob('S') == true and player.MPP < gcinclude.settings.RefreshGearMPP then		-- if the player's MP is below the threshold setting, equip the idle refresh gear
					gcinclude.MoveToCurrent(sets.Idle_Refresh,sets.CurrentGear);
				end	
		
				-- Check for common debuffs
				gcinclude.CheckCommonDebuffs(sets.CurrentGear);	
			end
		end
	end
	
	-- Make sure to equip the appropriate elemental staff for the current pet (/smn only)
	if (pet ~= nil and player.SubJob == 'SMN') then
		local pName = string.lower(pet.Name);
		if string.find(gcinclude.SummonSkill,pName) ~= nil then
			local pEle = gcinclude.SummonStaves[string.lower(pet.Name)];
			gcinclude.SwapToStave(pEle,false,sets.CurrentGear);
		end
	end
	
	-- And make sure a weapon equipped. (Going into a capped area can cause no weapon to be equipped.)
	local gear = gData.GetEquipment();
	if gear.Main == nil then
		gcinclude.MoveToCurrent(sets.Start_Weapons,sets.CurrentGear,true);
	end
	
	gcinclude.EquipTheGear(sets.CurrentGear);		-- Equip the composited HandleDefault set
					
	-- Lastly, update the display, just in case
	gcdisplay.Update();
end		-- HandleDefault

--[[
	HandleAbility is used to change the player's gear appropriately.
--]]

profile.HandleAbility = function()
	local ability = gData.GetAction();
			
	if gcdisplay.GetToggle('GSwap') == false then
		return;
	end

	-- Clear out the CurrentGear in case of leftovers
	gcinclude.ClearSet(sets.CurrentGear);
	
	-- Now process the appropriate job ability. Start with abilities associated with THF
	if string.match(ability.Name, 'Perfect Dodge') then
		gcinclude.MoveToCurrent(sets.PerfectDodge,sets.CurrentGear);
	elseif string.match(ability.Name, 'Steal') then
		gcinclude.MoveToCurrent(sets.Steal,sets.CurrentGear);
	elseif string.match(ability.Name,'Sneak Attack') then
		gcinclude.MoveToCurrent(sets.SneakAttack,sets.CurrentGear);
	elseif string.match(ability.Name, 'Flee') then
		gcinclude.MoveToCurrent(sets.Flee,sets.CurrentGear);
	elseif string.match(ability.Name,'Trick Attack') then
		gcinclude.MoveToCurrent(sets.TrickAttack,sets.CurrentGear);
	elseif string.match(ability.Name,'Mug') then
		gcinclude.MoveToCurrent(sets.Mug,sets.CurrentGear);
	elseif string.match(ability.Name,'Bully') then
		gcinclude.MoveToCurrent(sets.Bully,sets.CurrentGear);
	elseif string.match(ability.Name,'Hide') then
		gcinclude.MoveToCurrent(sets.Hide,sets.CurrentGear);
	elseif string.match(ability.Name,'Accomplice') then
		gcinclude.MoveToCurrent(sets.Accomplice,sets.CurrentGear);
	elseif string.match(ability.Name,'Collaborator') then
		gcinclude.MoveToCurrent(sets.Collaborator,sets.CurrentGear);		
		
	-- And now the subjob abilities
	-- /BST
	elseif string.match(ability.Name, 'Charm') then
		gcinclude.MoveToCurrent(sets.Charm,sets.CurrentGear);
		gcinclude.SwapToStave('light',false,sets.CurrentGear);
	elseif string.match(ability.Name, 'Reward') then
		-- Pet reward. Make sure that pet food already equipped
		if profile.sAmmo == nil or string.find(string.lower(profile.sAmmo),'pet f') == nil then		-- something else equipped
			profile.bAmmo = gcinclude.doPetFood('max',nil);
		end	
		gcinclude.MoveToCurrent(sets.Reward,sets.CurrentGear);
	elseif string.match(ability.Name, 'Tame') then
		gcinclude.MoveToCurrent(sets.Tame,sets.CurrentGear);
	-- /WAR
	elseif string.match(ability.Name, 'Provoke') then
		gcinclude.MoveToCurrent(sets.Provoke,sets.CurrentGear);
	elseif string.match(ability.Name, 'Berserk') then
		gcinclude.MoveToCurrent(sets.Berserk,sets.CurrentGear);
	elseif string.match(ability.Name, 'Defender') then
		gcinclude.MoveToCurrent(sets.Defender,sets.CurrentGear);
	elseif string.match(ability.Name, 'Warcry') then
		gcinclude.MoveToCurrent(sets.Warcry,sets.CurrentGear);
	--* /MNK *--
	elseif string.match(ability.Name, 'Boost') then
		gcinclude.MoveToCurrent(sets.Boost,sets.CurrentGear);
	elseif string.match(ability.Name, 'Focus') then
		gcinclude.MoveToCurrent(sets.Focus,sets.CurrentGear);
	elseif string.match(ability.Name, 'Dodge') then
		gcinclude.MoveToCurrent(sets.Dodge,sets.CurrentGear);
	elseif string.match(ability.Name, 'Chakra') then
		gcinclude.MoveToCurrent(sets.Chakra,sets.CurrentGear);
	-- /WHM
	elseif string.match(ability.Name, 'Divine Seal') then
		gcinclude.MoveToCurrent(sets.DivineSeal,sets.CurrentGear);
	-- /BLM
	elseif string.match(ability.Name, 'Elemental Seal') then
		gcinclude.MoveToCurrent(sets.ElementalSeal,sets.CurrentGear);
	-- /RNG
	elseif string.match(ability.Name, 'Sharpshot') then
		gcinclude.MoveToCurrent(sets.Sharpshot,sets.CurrentGear);
	elseif string.match(ability.Name, 'Scavenge') then
		gcinclude.MoveToCurrent(sets.Scavenge,sets.CurrentGear);
	elseif string.match(ability.Name, 'Camouflage') then
		gcinclude.MoveToCurrent(sets.Camouflage,sets.CurrentGear);
	elseif string.match(ability.Name, 'Barrage') then
		gcinclude.MoveToCurrent(sets.Barrage,sets.CurrentGear);	
	-- /SAM
	elseif string.match(ability.Name, 'Warding Circle') then
		gcinclude.MoveToCurrent(sets.WardingCircle,sets.CurrentGear);
	elseif string.match(ability.Name, 'Third Eye') then
		gcinclude.MoveToCurrent(sets.ThirdEye,sets.CurrentGear);
	elseif string.match(ability.Name, 'Hasso') then
		gcinclude.MoveToCurrent(sets.Hasso,sets.CurrentGear);
	elseif string.match(ability.Name, 'Meditate') then
		gcinclude.MoveToCurrent(sets.Meditate,sets.CurrentGear);
	elseif string.match(ability.Name, 'Seigan') then
		gcinclude.MoveToCurrent(sets.Seigan,sets.CurrentGear);
	-- /PLD
	elseif string.match(ability.Name, 'Holy Circle') then
		gcinclude.MoveToCurrent(sets.HolyCircle,sets.CurrentGear);
	elseif string.match(ability.Name, 'Shield Bash') then
		gcinclude.MoveToCurrent(sets.ShieldBash,sets.CurrentGear);
	elseif string.match(ability.Name, 'Sentinel') then
		gcinclude.MoveToCurrent(sets.Sentinel,sets.CurrentGear);	
	elseif string.match(ability.Name, 'Cover') then
		gcinclude.MoveToCurrent(sets.Cover,sets.CurrentGear);	
	-- /DRG
	elseif string.match(ability.Name, 'Ancient Circle') then
		gcinclude.MoveToCurrent(sets.AncientCircle,sets.CurrentGear);	
	elseif string.match(ability.Name, 'Jump') then
		gcinclude.MoveToCurrent(sets.Jump,sets.CurrentGear);
	elseif string.match(ability.Name, 'High Jump') then
		gcinclude.MoveToCurrent(sets.HighJump,sets.CurrentGear);		
	-- /DRK
	elseif string.match(ability.Name, 'Arcane Circle') then
		gcinclude.MoveToCurrent(sets.ArcaneCircle,sets.CurrentGear);
	elseif string.match(ability.Name, 'Last Resort') then
		gcinclude.MoveToCurrent(sets.LastResort,sets.CurrentGear);
	elseif string.match(ability.Name, 'Weapon Bash') then
		gcinclude.MoveToCurrent(sets.WeaponBash,sets.CurrentGear);
	elseif string.match(ability.Name, 'Souleater') then
		gcinclude.MoveToCurrent(sets.Souleater,sets.CurrentGear);		
	end
	gcinclude.EquipTheGear(sets.CurrentGear);		-- Equip the composited HandleAbility set
end		-- HandleAbility
	
--[[
	HandleItem is the place to equip gear when a special item is used. Currently only 'Holy Water' 
	is supported
--]]

profile.HandleItem = function()
	local item = gData.GetAction();
	local bShow = false;
	
	-- Clear out the CurrentGear in case of leftovers
	gcinclude.ClearSet(sets.CurrentGear);
	
	if gcdisplay.GetToggle('GSwap') == false then
		return;
	end
	
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
end		-- HandleItem

--[[
	HandlePrecast loads Fast Cast, cast time reduction, and quick cast gear in anticipation of a spell
--]]

profile.HandlePrecast = function()
    local spell = gData.GetAction();
	local obi;
	local mSet;
	
	-- Only gear swap if this flag is true
	if gcdisplay.GetToggle('GSwap') == false then
		return;
	end

	-- Clear out the CurrentGear in case of leftovers
	gcinclude.ClearSet(sets.CurrentGear);
	
	-- Equip the precast gear set
	gcinclude.MoveToCurrent(sets.Precast,sets.CurrentGear);
		
	-- See if an elemental obi should be equipped
	obi = gcinclude.CheckEleSpells(spell.Name,gcinclude.MagicEleAcc,gcinclude.OBI,nil);
	if obi ~= nil then
		sets.CurrentGear['Waist'] = obi;
	end
	gcinclude.EquipTheGear(sets.CurrentGear);
end		-- HandlePrecast

--[[
	HandleMidcast is the second function invoked when a player casts a spell. It equips gear appropriate for 
	magic skill, duration, magic attack bonus, magic accuracy, and potency. There's an order to how the pieces 
	are loaded: INT/MND, spell specific, macc, magic skill, obi, ele swap	
--]]

profile.HandleMidcast = function()
	local bTank = gcdisplay.GetToggle('Tank');
	
	if gcdisplay.GetToggle('GSwap') == false then		-- Only gear swap if this flag is true	
		return;
	end

	-- Clear out the CurrentGear in case of leftovers
	gcinclude.ClearSet(sets.CurrentGear);
	
	-- Call the common HandleMidcast now
	gcinclude.HandleMidcast(bTank);

	if bTank then
		gcinclude.settings.priorityMidCast = 'ACBDEFGH';
	else
		gcinclude.settings.priorityMidCast = 'ABCDEFGH';	
	end	
	
	gcinclude.EquipTheGear(sets.CurrentGear);		-- Equip the composited midcast set
end		-- HandleMidcast

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
end		-- HandlePreshot

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
	-- cancelled so you do not lose your tp.
	if (canWS == false) then 
		gFunc.CancelAction();
		return;
	end
	
	-- Only gear swap if this flag is true
	if gcdisplay.GetToggle('GSwap') == false then
		return;
	end

	-- Clear out the CurrentGear in case of leftovers
	gcinclude.ClearSet(sets.CurrentGear);

	-- Call the common weaponskill handler
	gcinclude.HandleWeaponskill(false);
	
	-- Equip the composited weaponskill set		
	gcinclude.EquipTheGear(sets.CurrentGear);
end		-- HandleWeaponskill

return profile;