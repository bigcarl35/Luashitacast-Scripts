local profile = {};
gcinclude = gFunc.LoadFile('common\\gcinclude.lua');

--[[
	This file contains all the gear sets associated with the BST job. While it isn't outside of the realm of 
	possibility that the subjob might be able to use gear sets too, that is not the emphasis of this program. 
	It is tailored to handle all the aspects of BST. It also supports magic use which is obviously a possibility
	from the subjob. If you desire a gear set change to strengthen an ability from your subjob that is not 
	supported by this program, you probably will have to make a custom gear set and use the /gearset command to
	use it.
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

	Also, not all sets need to be defined. There is nothing wrong with leaving a set "empty", but don't delete any
	of the sets. All the ones listed here (except for any custom sets) are expected to exist by Luashitacast.
		
	*** Note ***
	/SMN has a problem in that their pet is the level of the subjob, which is not very useful. As a
	result, /SMN pet actions are not supported in this implementation. As for /DRG, the wyvern can't be summoned.
	Just an FYI.
	
	*** Note 2 ***
	No gear that supports bard songs can be worn by any job except a bard, no there's no support given here for
	/BRD.	
--]]

--[[
	The TP sets are used when you and/or your pet are fighting,	The accuracy set will be used if ACC is specified 
	and the evasion set if EVA is specified.
--]]

	['TP'] = {
        Head  = { 'Shep. Bonnet//PET', 'Panther Mask', 'Monster Helm', 'Beast Helm', 'Shep. Bonnet', 'Empress Hairpin', 'Silver Hairpin//MSJ' },
        Neck  = { 'Peacock Amulet', 'Spike Necklace' },
		Ears  = { 'Ethereal Earring', 'Beastly Earring', 'Fang Earring', 'Genin Earring//SJNIN', 'Bat Earring//MSJ', 'Energy Earring +1//MSJ', 'Energy Earring +1//MSJ', 'Physical Earring//SJNIN', 'Reraise Earring', 'Physical Earring' },
        Body  = { 'Gaudy Harness//MP.LT.50', 'Narasimha\'s Vest', 'Scorpion Harness', 'Gaudy Harness', 'Wonder Kaftan', 'Mrc.Cpt. Doublet', 'Beetle Harness', 'Angler\'s Tunica' },
        Hands = { 'Thick Mufflers', 'Beast Gloves', 'Wonder Mitts', 'Battle Gloves', 'Ryl.Ftm. Gloves' },
        Rings = { 'Sun Ring', 'Tamas Ring//MSJ', 'Sun Ring', 'Courage Ring', 'Balance Ring', 'San d\'Orian Ring' },
        Back  = { 'Psilos Mantle', 'Raptor Mantle', 'Ram Mantle' },
        Waist = { 'Swift Belt', 'Tilt Belt', 'Warrior\'s Belt' },
        Legs  = { 'Thick Breeches', 'Beast Trousers', 'Shep. Hose', 'San. Trousers', 'Ryl.Ftm. Trousers' },
        Feet  = { 'Thick Sollerets', 'Beast Gaiters', 'Wonder Clomps', 'Bounding Boots' },
    },
	
--[[
	If an accuracy emphasis is desired, the following set will replace the gear appropriately.
	Remember that DEX converts to accuracy: (horizon) for every 1 point of DEX you get 
	0.70 points of accuracy if wielding a 2H weapon, 0.65 for a 1H weapon, and 0.60 for H2H. 
	Tank_Accuracy is a subset of Accuracy. It lets you specify what accuracy gear to equip 
	that doesn't compromise your tanking set as much as a full-blown accuracy set would.
--]]

	['Accuracy'] = {
        Head  = { 'Optical Hat', 'Shep. Bonnet//PETF' } ,
        Neck  = 'Peacock Amulet',
        Body  = { 'Gaudy Harness//MP.LT.50', 'Scorpion Harness', 'Narasimha\'s Vest' },
        Hands = { 'Thick Mufflers', 'Battle Gloves' },
		Ears  = { 'Beastly Earring//PETF', 'Pilferer\'s Earring//SJTHF' },
		Rings = { 'Toreador\'s Ring', 'Jaeger Ring', 'Balance Ring', 'Bastokan Ring' },
        Back = 'Psilos Mantle',
        Waist = { 'Life Belt', 'Monster Belt', 'Tilt Belt' },
        Legs = 'Thick Breeches',
        Feet = 'Thick Sollerets',
    },	

--[[
	If evasion wanted, equip evasion gear
--]]

	['Evasion'] = {
        Head  = { 'Optical Hat', 'Empress Hairpin' },
		Ears  = { 'Bat Earring//BLIND', 'Ethereal Earring', 'Reraise Earring' },
        Body  = { 'Scorpion Harness', 'Narasimha\'s Vest' },
		Hands = 'Battle Gloves',
        Legs  = { 'San. Trousers', 'Shep. Hose//PETF' },
		Feet  = 'Bounding Boots',	-- default gear is thick sollerets which are -2 eva
    },

--[[
	The "Travel" gear set is what is worn when you're not fighting (either
	you or your pet), you're not resting. It's a good place to put gear 
	that increases your movement speed. (Not to be confused with the 
	['Movement'] gear set which is used when you're kiting.) This is also 
	where you put gear that is adventageous if you have a pet present 
	(i.e., lower perpetuation cost, etc.)
--]]
		
	['Travel'] = {
	},
	
--[[
	The Idle_Regen and Idle_Refresh gear sets are used to restore a player's HP or MP that goes 
	below a set percentage (defined in gcinclude.lua, but can be overriden in profile.OnLoad
	function).
--]]
	
	['Idle_Regen'] = {
	},
	
	['Idle_Refresh'] = {
		Body = 'Gaudy Harness//MP.LT.50',
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
		Legs = 'Monster trousers',
	},

	['Resting_Refresh'] = {
		Body = 'Gaudy Harness//MP.LT.50',	
	},
	
	-- If you have any Spell Interruption Rate down gear, put them into the "SIR" gear set.
	-- This gear set is equipped in the HandleMidcast function that all spells go through.
	-- Only BST gear that has this attribute is a Woodsville Axe. All other gear has to be 
	-- equippable for any job.
	['SIR'] = {
	},
	
--[[
	Start weapons are where you define what you want the first row of equipment to look like when you
	either log in as a SMN or you switch your main job to SMN. Any other gear you mention will be overridden
	by the Idle or Town set, so no need to include here.
--]]

	['Start_Weapons'] = {
        Main = { 'Maneater', 'Barbaroi Axe', 'Ryl.Arc. Sword' },	-- Sword added for low level option
        Sub  = { 'Tabarzin//SJNIN', 'Tatami Shield', 'War Pick//SJNIN' },
        Ammo = { 'Hedgehog Bomb', 'S. Herbal Broth' },
    },

--[[
	Specify what you want to wear around town.
--]]
	
	['Town'] = {
        Head = 'Lilac Corsage',
		Body = { 'Ducal Aketon//AK:OMNI', 'Narasimha\'s Vest' },
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
        Ears = 'Coral Earring',
    },
	
	['DT_Breath'] = { 
	},
	
--[[
	Magic accuracy gear
--]]

	['macc'] = {
		Rings = 'Tamas Ring//MSJ',
	},
	
--[[
	Magic Attack Bonus (MAB) is used for more than just spells, so it is broken out
--]]

	['MAB'] = {
		Neck = 'Uggalepih Pendant//MPP.LE.50P',
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
		Head  = 'Optical Hat',
		Neck  = 'Peacock Amulet',
		Rings = { 'Woodsman Ring', 'Jaeger Ring', 'Beetle Ring +1', 'Beetle Ring +1' },	
        Back = 'Psilos Mantle',
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

--[[
	The second stage is Midcast. This is where you'll want to equip magic attack, or magic enhancing 
	gear. (Magic Attack Bonus also happens here, but is broken out into it's own gear set. See MAB.)
--]]	

	['Midcast'] = {
	},

--[[
	Further, there is a break out for each type of spell. I've included a comment on the type of attributes
	the piece of gear should have. While the spell might have other attributes than those listed, the ones I have
	listed have gear that a BST or anyone can wear.
--]]

	-- Healing: Healing Magic Skill. Currently only a Healing Earring affects healing spells from a sub job. No other gear
    -- gives bonuses to Healing magic from a sub job. Also, gear with MND bonuses will boost cure spell's potency, but MND 
	-- gear is automatically equipped prior to the Healing set being equipped in the HandleMidcast function. There's no need 
	-- to include MND gear here. As to items that add cure potency directly there are a few pieces for "all jobs". So, 
	-- include healing magic skill items and cure potency items here.
	['Healing'] = {
    },
	
	-- Dark: Dark Magic Skill. Currently only gear equippable by any job gives is applicable here. There's no gear that's 
	-- specific for BST that gives any dark magic skill.	
	['Dark'] = {
    },
	
	-- Divine: Divine Magic Skill.
	['Divine'] = {
	},

	-- Enfeebling: Enfeebling Magic Skill. Currently only gear equippable by any job gives is applicable here. There's no gear 
	-- that's specific for BST that gives any enfeebling magic skill.
	['Enfeebling'] = {
	},

	-- Enhancing: There is no gear that a BST can wear to enhance any magic spell. Leave the Enhancing gear sets empty.
	['Enhancing'] = {
	},
	
	-- Elemental: Elemental Magic Skill. Currently only gear equippable by any job gives is applicable here. There's no gear
	-- that's specific for BST that gives any elemental magic skill. Note: don't include elemental staves or elemental 
	-- obis/gorgets here, that is done automatically in the HandlePrecast/HandleMidcast functions (if /wswap is enabled).
	['Elemental'] = {
	},
	
	-- Ninjutsu: There is no gear that a BST can wear to add Ninjutsu skill. Leave the following two
	-- gear sets empty.	
	['Ninjutsu'] = {
	},
	
	-- Summoning: Summoning Magic Skill and Avatar Perpetuation Cost. Currently only gear equippable by any job gives
	-- is applicable here. There's no gear that's specific for BST that gives any summoning skill. Note: currently on 
	-- HorizonXI summoning skills are ignored. Any gear piece that only gives summoning skill will be commented out	
	['Summoning'] = {
	},
	
--[[
	Next is stat-based gear, (in this case intelligence or mind)
--]]

	['INT'] = {
        Head  = 'Beast helm',
		Body  = 'Monster Jackcoat',
        Rings = { 'Tamas Ring', 'Windurstian Ring' },
        Feet = 'Mannequin Pumps',
    },
	
	['MND'] = {
        Neck  = 'Justice Badge',
        Body  = 'Wonder Kaftan',
        Rings = { 'Tamas Ring', 'Tranquility Ring', 'San d\'Orian Ring' },
        Waist = 'Friar\'s Rope',
        Legs  = 'Wonder Braccae',
        Feet  = 'Mannequin Pumps',
	},
	
--[[
	And some spells are special cases, so they have individual gears sets.
--]]
	-- Stoneskin: Stoneskin Enhancement, Mind, and Enhancing Magic Skill. Mind is 3x more important than enhancing
	-- Magic Skill. The only piece of gear a BST can wear to enhance stoneskin is a Stone Gorget. There's no gear
	-- that a SMN (or any job) can wear to enhance magic. Note: This gear set has no effect on Titan's Earthen
	-- ward blood pact.
	['Stoneskin'] = {	
	},	
	
	-- Drain: Drain Enhancement, Dark Magic Skill, Dark Magic Accuracy. Currently no gear equippable by a
	-- BST enhances Drain. Drain is part of Dark Magic, so Potency which is based on dark magic skill will
	-- already be loaded in HandleMidcast function and need not be repeated here. No current gear supports
	-- dark magic accuracy for any job. Magic attack bonus and magic critical hit have no effect on potency.
	-- Leave this gear set empty.
	['Drain'] = {
    },

	-- Aspir: Aspir Enhancement, Dark Magic Skill, Dark Magic Accuracy. Currently no gear equippable by a
	-- BST enhances Aspir. Aspir is part of Dark Magic, so potency which is based on dark magic skill will
	-- already be loaded in HandleMidcast function and need not be repeated here. No current gear supports
	-- dark magic accuracy for any job. Magic attack bonus and magic critical hit have no effect on potency.
	-- Leave this gear set empty.
	['Aspir'] = {
    },

	-- Sneak: Enhances Sneak and Enhances Stealth. Currently on Dream Boots +1 enhances sneak and is equippable
	-- by any job. (Attained through the Starlight Celebration.) No gear for any job supports Enhances Stealth
	-- yet.
	['Sneak'] = {
		Feet = 'Dream Boots +1',
	},

	-- Invisible: Enhances Invisible Effect. Currently only Dream Mittens +1 enhances invisible and is equippable
	-- by any job. (Attained through the Starlight Celebration.)	
	['Invisible'] = {
		Hands = 'Dream Mittens +1',
	},
	
	-- Note: Phalanx does have gear that supports the spell, but it is out of era
	
--[[
	The following weapon skill gearsets are defined by the stat they emphasize. Listed are all of the sets that
	you will need to use every weapon skill that your job can do. The leading comment defines what weapon/weapon
	skill combination the set applies to.
	
	BST can use the following weapons: axe (A-), scythe (B-), dagger (C+), club(D), sword (E). Any other weapon
	will have no weaponskill available. Weapon skill sets are named based on stat(s) used, regardless of weapon
--]]

--[[
		* Strength based *
		
		Axe: Raging Axe,Smash Axe,Gale Axe,Avalanche Axe,Spinning Axe,Rampage,
			 Mistral Axe,Decimation
		Scythe: Slice,Spinning Scythe,Vorpal Scythe
		Club: Brainshaker,Skullbreaker,True Strike
		Sword: Flat Blade,Circle Blade,Vorpal Blade
-]]
	
	['WS_STR'] = {
        Head  = 'Mrc.Cpt. Headgear',
        Neck  = 'Spike Necklace',
		Ears  = 'Beastly Earring',		-- Should be conditional: using an axe
        Body  = 'Narasimha\'s Vest',
        Hands = { 'Ogre Gloves', 'Wonder Mitts' },
        Rings = { 'Sun Ring', 'Sun Ring', 'Courage Ring', 'San d\'Orian Ring' },						-- +3 STR
        Back  = 'Amemet Mantle',
        Legs  = 'Wonder Braccae',
        Feet  = { 'Creek F Clomps', 'Wonder Clomps' },
	},
	
--[[
		* Strength and Dexterity based, even weighting *
		
		Sword: Fast Blade
--]]

	['WS_STRDEX'] = {
        Head  = 'Empress Hairpin',
        Neck  = 'Spike Necklace',
        Ears  = 'Beastly Earring',		-- Should be conditional: using an axe
        Body  = 'Narasimha\'s Vest',
        Hands = 'Wonder Mitts',
        Rings = { 'Sun Ring', 'Sun Ring' },
        Back  = 'Amemet Mantle',
        Legs  = 'Wonder Braccae',
        Feet  = { 'Creek F Clomps', 'Wonder Clomps' },
    },

--[[
		* Strength and Intelligence based, even weighting *
		
		Scythe: Dark Harvest,Shadow of Death,Nightmare Scythe,Spiral Hell
		Sword: Burning Blade
--]]
	
	['WS_STRINT'] = {
		Head  = 'Beast helm',
		Neck  = 'Spike necklace',
		Ears  = 'Beastly Earring',		-- Should be conditional: using an axe
		Body  = { 'Monster Jackcoat', 'Narasimha\'s vest' },
		Hands = 'Ogre gloves',
		Rings = { 'Sun Ring', 'Sun Ring' },
		Waist = 'Barbarian\'s belt',
		Legs  = 'Wonder braccae',
		Feet  = { 'Creek F Clomps', 'Wonder Clomps' },
	},

--[[
		* Strength and Intelligence based, 30%/20% respectively *
		
		Sword: Red Lotus Blade
--]]
	
	['WS_STRINT_30_20'] = {
		Head  = 'Beast helm',
		Neck  = 'Spike necklace',
		Ears  = 'Beastly Earring',		-- Should be conditional: using an axe
		Body  = { 'Monster Jackcoat', 'Narasimha\'s vest' },
		Hands = 'Ogre gloves',
		Rings = { 'Sun Ring', 'Sun Ring' },
		Waist = 'Barbarian\'s belt',
		Legs  = 'Wonder braccae',
		Feet  = { 'Creek F Clomps', 'Wonder Clomps' },
	},

--[[
		* Strength and Mind based, even weighting *
		
		Scythe: Guillotine,Cross Reaper
		Club: Shining Strike,Seraph Strike,Judgement
		Sword: Shining Blade,Seraph Blade
--]]

	['WS_STRMND'] = {
		Head  = 'Mrc.Cpt. Headgear',
		Neck  = 'Justice badge',
        Ears  = 'Beastly Earring',		-- Should be conditional: using an axe
		Body  = 'Narasimha\'s vest',
		Hands = 'Ogre gloves',
		Rings = { 'Sun ring', 'Sun ring' },
		Waist = 'Barbarian\'s belt',
		Legs  = 'Wonder braccae',
		Feet  = { 'Creek F Clomps', 'Wonder Clomps' },
	},

--[[
		* Strength and Vitality based, even weighting *
		
		Axe: Calamity (32%/32%)
--]]
	
	['WS_STRVIT'] = {
		Head  = 'Mrc.Cpt. Headgear',	
		Neck  = 'Spike necklace',
        Ears  = 'Beastly Earring',		-- Should be conditional: using an axe
		Body  = 'Narasimha\'s vest',
		Hands = 'Ogre gloves',
		Ring1 = { 'Sun ring', 'Sun ring' },
		Waist = 'Barbarian\'s belt',
		Legs  = 'Wonder braccae',
		Feet  = 'Creek F clomps',
	},

--[[
		* Dexterity based *
		
		Axe: Onslaught
		Dagger: Wasp Sting,Viper Bite^,Eviseration

		^ Subjob must be one of: RDM,THF,BRD,RNG,NIN
--]]
	
	['WS_DEX'] = {
        Head = 'Empress Hairpin',
        Neck = 'Spike Necklace',
        Ears = 'Beastly Earring',
        Body = 'Brigandine',
        Hands = 'Beast Gloves',
        Rings = { 'Balance Ring', 'Bastokan Ring' },
        Feet = 'Bounding Boots',
    },

--[[
		* Dexterity and Intelligence based *
		
		Dagger: Gust Slash,Cyclone^

		^ Subjob must be one of: RDM,THF,BRD,RNG,NIN
--]]
	
	['WS_DEXINT'] = {
        Head = 'Beast Helm',
        Neck = 'Spike Necklace',
        Ears = 'Beastly Earring',
        Body = { 'Monster Jackcoat', 'Brigandine' },
        Hands = 'Beast Gloves',
        Rings = 'Balance Ring',
        Feet = 'Bounding Boots',
    },

--[[
		* Charisma based *
		
		Dagger: Shadowstitch
--]]
	
	['WS_CHR'] = {
		Head = 'Panther mask',
		Neck = 'Flower Necklace',
		Ears = 'Beastly earring',
		Body = 'Gaudy harness',
		Rings = { 'Moon ring', 'Moon ring' },
		Waist = { 'Monster Belt', 'Corsette' },
		Legs = 'Beast trousers',
		Feet = 'Beast gaiters',
	},

--[[
		* Mind based *
		
		Dagger: Energy Steal, Energy Drain^
		
		^ Subjob must be RDM,THF,BRD,RNG, or NIN
--]]

	['WS_MND'] = {
        Neck = 'Justice Badge',
        Ears = 'Beastly Earring',
        Body = 'Wonder Kaftan',
        Rings = { 'Tamas Ring', 'Tranquility Ring' },
        Waist = 'Friar\'s Rope',
        Legs = 'Wonder Braccae',
    },

--[[
		* Skill based *
		
		Club: Starlight,Moonlight
--]]

	['WS_Skill'] = {
    },

--[[
		* HP based *
		
		Sword: Spirits Within
--]]

	['WS_HP'] = {
        Head = 'Monster Helm',
        Ears = { 'Ethereal Earring', 'Physical Earring' },
        Body = 'Monster Jackcoat',	
        Hands = 'Wonder Mitts',	
        Rings = 'Toreador\'s Ring',	
        Waist = 'Powerful Rope',
        Legs = 'Wonder Braccae',
        Feet = 'Creek F Clomps',
    },

--[[
	Movement tends to be used for kiting. Emphasis should be placed on gear that increases movement speed, but you 
	might also want gear that has evasion. The choice is yours.
--]]

	-- Movement speed gear, does not include nation aketons which are found in conditional gear for home town
	['Movement'] = { 
	},
	
--[[
	The following sets are used with pet abilities/pet commands
--]]
	
	['Familiar'] = {
	},
	
	['CallBeast'] = {			-- or bestial loyalty, augmented call beast gear
	},
	
	['Gauge'] = {
	},
	
	-- Reward potency, reward augment, reward enhancement, and MND gear
	['Reward'] = {
        Neck = 'Justice Badge',
        Body = { 'Monster Jackcoat//DB:WSS', 'Beast Jackcoat//DB:BPP', 'Monster Jackcoat','Beast Jackcoat' },
        Hands = 'Ogre Gloves',
        Rings = { 'Tamas Ring',	'Tranquility Ring' },
        Waist = 'Friar\'s Rope',
        Legs = 'Wonder Braccae',
        Feet = { 'Beast Gaiters', 'Mannequin Pumps' },
	},
	
	-- Tame success rate. Resistence depends on your INT vs target's INT
	['Tame'] = {
        Head = 'Beast Helm',
		Body = 'Monster Jackcoat',
		Rings = 'Tamas Ring',
		Feet = 'Mannequin Pumps',
    },
	
	-- CHR and Charm + gear. (Every +1 Charm adds 5% Charm duration
	['Charm'] = {
        Head = 'Monster Helm',
        Neck = 'Flower Necklace',
        Ears = 'Beastly Earring',
        Body = 'Monster Jackcoat',
        Hands = 'Beast Gloves',
        Rings = { 'Moon Ring', 'Moon Ring' },
        Waist = { 'Monster Belt', 'Corsette' },
        Legs = 'Beast Trousers',
        Feet = 'Beast Gaiters',
    },
	
	['Pet_Attack'] = {					-- Pet's strength, not accuracy
	},

	['Pet_Macc'] = {					-- Pet's Magical Accuracy
	},
	
	['Pet_Matt'] = {					-- Pet's Magical Attack
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

	--* /THF *--
	['Steal'] = {
	},
	
	['SneakAttack'] = {
	},
	
	['Flee'] = {
	},
	
	['TrickAttack'] = {
	},
	
	['Mug'] = {
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
	
	['Jumps'] = {		-- Jump and High Jump, Super is too high a level
	},
	
	--* /BRD *--
	-- No skills
	
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
-- list of all jug pets available on HorizonXI.
-- what,name,min level,max level,duration,have,favored
profile.JugPetsIndices = T {['JUG'] = 1, ['MIN'] = 2, ['MAX'] = 3, ['DUR'] = 4, ['HAV'] = 5, ['FAV'] = 6};
profile.JugPets = T {
	['carrot broth'] = {'Hare Familiar',23,35,90,false,false},
	['herbal broth'] = {'Sheep Familiar',23,35,60,false,false},
	['humus'] = {'Flowerpot Bill',28,40,60,false,false},
	['meat broth'] = {'Tiger Familiar',28,40,60,false,false},
	['grasshopper broth'] = {'Flytrap Familiar',28,40,60,false,false},
	['carrion broth'] = {'Lizard Familiar',33,45,60,false,false},
	['bug broth'] = {'Mayfly Familiar',33,45,60,false,false},
	['mole broth'] = {'Eft Familiar',33,45,60,false,false},
	['tree sap'] = {'Beetle Familiar',38,45,60,false,false},
	['antica broth'] = {'Antlion Familiar',38,50,60,false,false},
	['fish broth'] = {'Crab Familiar',23,55,30,false,false},
	['blood bath'] = {'Mite Familiar',43,55,60,false,false},
	['f. carrot broth'] = {'Keeneared Steffi',43,75,90,false,false},
	['s. herbal broth'] = {'Lullaby Melodia',43,75,60,false,true},
	['rich humus'] = {'Flowerpot Ben',51,75,60,false,false},
	['w. meat broth'] = {'Saber Siravarde',51,75,60,false,false},
	['seedbed soil'] = {'Funguar Familiar',33,65,60,false,false},
	['qdv. bug broth'] = {'Shellbuster Orob',53,75,60,false,false},
	['c. carrion broth'] = {'Coldblood Como',53,75,60,false,false},
	['fish oil broth'] = {'Courier Carrie',23,75,30,false,false},
	['alchemist water'] = {'Homunculus',23,75,60,false,false},
	['n. grasshopper broth'] = {'Voracious Audrey',53,75,60,false,false},
	['l. mole broth'] = {'Ambusher Allie',58,75,60,false,false},
	['scarlet sap'] = {'Panzer Galahad',63,75,60,false,false},
	['c. blood broth'] = {'Lifedrinker Lars',63,75,60,false,false},
	['f. antica broth'] = {'Chopsuey Chucky',63,75,60,false,false},
	['sun water'] = {'Amigo Sabotender',75,75,30,false,false},
};

profile.Sets = sets;
profile.sjb = nil;			-- Tracks subjob name
profile.bAmmo = false;		-- BST specific. Is ammo equipped?
profile.sAmmo = nil;		-- BST specific. Name of ammo equipped

--[[
	HandlePetAction equips the appropriate gear set based on the type of action
	the pet is trying to perform.
--]]

local function HandlePetAction(PetAction)
	local pet = gData.GetPet();
	
	-- Only gear swap if this flag is true and the pet is a summoned pet
	if gcdisplay.GetToggle('GSwap') == false or string.find(gcinclude.SummonSkill,pet.Name) ~= nil then
		return;
	end

	-- Only BST pet attacks have associated gear sets because /smn pets are at most half the
	-- level of your BST level
	if (gcinclude.BstPetAttack:contains(PetAction.Name)) then				-- Pet Attack
		gcinclude.MoveToCurrent(sets.Pet_Attack,sets.CurrentGear);
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
	-- "subs" is the key for what toolbar is shown. Each job listed in the array is a possible subjob.
	-- If the associated value for a subjob is greater than zero, that indicates what macro set from the
	-- macro book should be displayed. (The value associated with BST, your current main job, is nil
	-- since you can't be a bst/bst.) A value of 0 means the subjob is not configured. All other values
	-- imply that the subjob is expected and shows what macro set to show.
	local subs = {['WAR'] = 0, ['MNK'] = 0, ['WHM'] = 1, ['BLM'] = 3, ['RDM'] = 0, ['THF'] = 2,
				 ['PLD'] = 0, ['DRK'] = 0, ['BST'] = nil, ['BRD'] = 0, ['RNG'] = 0, ['SMN'] = 0,
				 ['SAM'] = 0, ['NIN'] = 4, ['DRG'] = 0, ['BLU'] = 0, ['COR'] = 0, ['PUP'] = 0,
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
	gcinclude.settings.priorityMidCast = 'ABCDEFGH';
	gcinclude.settings.priorityWeaponSkill = 'ABDE';	
	
	-- Set which macro book should be displayed. Which macro set within the macro book to
	-- display depends on what your subjob is.
	AshitaCore:GetChatManager():QueueCommand(1, '/macro book 10');		-- BST macro book
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
	findJugPets traverses the master list of jugs and determines if any are accessible. The appropriate 
	indicator is updated accordingly. Returned is whether any jug pets were found.
	
	Note: I did look into adapting gcinclude.FindString since there is a similarity, but it was more 
	work than it was worth doing, so this routine will look similar.
--]]

profile.findJugPets = function()
	local inventory = AshitaCore:GetMemoryManager():GetInventory();
	local resources = AshitaCore:GetResourceManager();
	local player = gData.GetPlayer();
	local iCount = 0;
	
	-- Clear the table ownership settings
	for k,v in pairs(profile.JugPets) do
		v[5] = false;
	end
	
	-- Now walk the equipable (in the field) storage areas
	for k,v in pairs(gcinclude.EQUIPABLE) do
		containerID = v[1];

		-- then loop through the selected container looking for a jug pet's broth
		for j = 1,inventory:GetContainerCountMax(containerID),1 do
			local itemEntry = inventory:GetContainerItem(containerID, j);
			if (itemEntry.Id ~= 0 and itemEntry.Id ~= 65535) then
                local item = resources:GetItemById(itemEntry.Id);
				-- then check the master list of jug pets
				for kk,tpf in pairs(profile.JugPets) do
					if kk == string.lower(item.Name[1]) then
						if (tpf[2] <= player.MainJobSync) and (tpf[3] >= player.MainJobSync) then
							-- finally, this one is possible to be selected
							profile.JugPets[kk][5] = true;
							iCount = iCount + 1;
						end
					end
				end
			end
		end
	end
	
	-- assuming any were found, return true or false
	return (iCount > 0);
end		-- findJugPets

--[[
	findMaxEquipableJugPet determines what is the best jug pet to load and equips it. The success is returned.
	The way this function works is by searching for available jug pets and determining which are level
	appropriate. Of those check if any are favored. If so, equip that one, else equip first one on list.
--]]

profile.findMaxEquipableJugPet = function()
	local sPos = nil;

	-- find any equipable jug pets
	if profile.findJugPets() == true then
		-- then cycle through the list and find the favored one. If none favored, the first jug found will be used.
		for k,v in pairs(profile.JugPets) do
			if v[5] == true then		-- One of the found pets
				sPos = k;
				if v[6] == true then	-- Favored
					break;
				end
			end
		end
		if sPos ~= nil then
			gFunc.ForceEquip('Ammo',sPos);
			return true;
		else
			print(chat.header('findMaxEquipableJugPet'):append(chat.message('Error: Found jug, but none equipped!')));
			return false;
		end
	else
		print(chat.header('findMaxEquipableJugPet'):append(chat.message('Error: No jug pets found to equip')));
		return false;
	end
end		-- findMaxEquipableJugPet

--[[
	HandleCommand is run when you type in a command defined in LUASHITACAST. The commands handled here instead
	of in gcinclude.HandleCommands are specific to PLD or the help system.
--]]

profile.HandleCommand = function(args)
	if args[1] == 'help' then
		gcdisplay.ShowHelp();
	elseif (args[1] == 'ajug') then			-- Turns on/off whether Automated Jug Pets supported
		gcdisplay.AdvanceToggle('AJug');
	elseif args[1] == 'petfood' then
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
	local pet = gData.GetPet();
	local petAction = gData.GetPetAction();
	local player = gData.GetPlayer();
	local zone = gData.GetEnvironment();	
	local ew = gData.GetEquipment();
	local eWeap = nil;
	local cKey;
	
	-- A pet action takes priority over a player's action as long as it is a BST pet action.
	-- /SMN pet's actions are ignored.
	if pet ~= nil then
		local sLName = string.lower(pet.Name);
		if petAction ~= nil and (string.find(gcinclude.SummonSkill,sLName) == nil) then
			HandlePetAction(petAction);
			return;
		end
	end
	
	profile.sPetAction = nil;
	
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
	
	-- When you want to reward your pet and you do not have pet food equipped or when you 
	-- want to summon a pet and a jug is not equipped, the current item in the ammo slot 
	-- is saved. The following will set it back to what you had before either of those two 
	-- items were equipped.
	if profile.bAmmo then
		gFunc.ForceEquip('Ammo',profile.sAmmo);
		profile.sAmmo = nil;
		profile.bAmmo = false;
	end

	-- Clear out the CurrentGear in case of leftovers
	gcinclude.ClearSet(sets.CurrentGear);
	
	-- If player is not resting and has swapped weapons, set the weapon back to what 
	-- they had before the switch

	if player.Status ~= 'Resting' and 
			gcinclude.weapon ~= nil and 
			gcdisplay.GetToggle('WSwap') == true and 
			eWeap ~= gcinclude.weapon then
		sets.CurrentGear['Main'] = gcinclude.weapon;
		sets.CurrentGear['Sub'] = gcinclude.offhand;
	end

	-- The default set is the TP gear set. Load it up
	gcinclude.MoveToCurrent(sets.TP,sets.CurrentGear);
				
	-- Now process the player status accordingly
	gcdisplay.SetLocksAction(gcinclude.LocksNumeric,player.Status);
	if (player ~= nil and player.Status == 'Engaged') or (pet ~= nil and pet.Status == 'Engaged') then
		gcinclude.settings.priorityEngaged = string.upper(gcinclude.settings.priorityEngaged);
		for i = 1,string.len(gcinclude.settings.priorityEngaged),1 do
			cKey = string.sub(gcinclude.settings.priorityEngaged,i,i);
			if cKey == 'C' then		-- Evasion			
				if gcdisplay.GetToggle('Eva') == true then
					gcinclude.MoveToCurrent(sets.Evasion,sets.CurrentGear);
				end
			elseif cKey == 'E' then		-- Accuracy	
				if gcdisplay.GetToggle('Acc') == true then 				
					gcinclude.MoveToCurrent(sets.Accuracy,sets.CurrentGear);
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
		-- Player kneeling. Priority (low to high): regen,refresh
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
		-- Assume idling. There's no idle set, just idle conditions. 
		
		-- See if in a town
		if (zone.Area ~= nil and table.find(gcinclude.Towns,zone.Area)) then
			gcinclude.MoveToCurrent(sets.Town,sets.CurrentGear);	
		else
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
	
	-- In case the pet is a summoned pet, /smn, make sure to equip the appropriate elemental staff
	if (pet ~= nil and player.SubJob == 'SMN') then
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
	
	gcinclude.EquipTheGear(sets.CurrentGear);		-- Equip the composited HandleDefault set
	
	-- Lastly, update the display, just in case
	gcdisplay.Update();
end		-- HandleDefault

--[[
	bAmmoIsJug determines if the item in the Ammo slot is a jug pet or not.
--]]

profile.bAmmoIsJug = function(sAmmo)
	local bFound = false;
	
	if sAmmo == nil then
		return nil;
	else
		sAmmo = string.lower(sAmmo)
		for k,v in pairs(profile.JugPets) do
			if string.find(sAmmo,string.lower(k)) ~= nil then
				bFound = true;
				break;
			end
		end		
	end
	return bFound;
end		-- bAmmoIsJug

--[[
	HandleAbility is used to change the player's gear appropriately for the specified pet ability.
--]]

profile.HandleAbility = function()
	local ability = gData.GetAction();
	local eq = gData.GetEquipment();
	
	-- Only gear swap if this flag is true
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
	
	-- Now process the appropriate job ability. Start with abilities associated with BST
	if string.match(ability.Name, 'Call Beast') or string.match(ability.Name, 'Bestial Loyalty') then
		-- First make sure player wants the automated jug pet funtionality
		if gcdisplay.GetToggle('AJug') == true then
			-- Ok, now see if a jug pet already equipped
			local bJugFound = profile.bAmmoIsJug(profile.sAmmo);
			if bJugFound == nil or (bJugFound ~= nil and bJugFound == false) then
				profile.bAmmo = profile.findMaxEquipableJugPet();
			end
		end
		gcinclude.MoveToCurrent(sets.CallBeast,sets.CurrentGear);
	elseif string.contains(ability.Name, 'Familiar') then
		gcinclude.MoveToCurrent(sets.Familiar,sets.CurrentGear);
	elseif string.contains(ability.Name, 'Gauge') then
		gcinclude.MoveToCurrent(sets.Gauge,sets.CurrentGear);
	elseif string.match(ability.Name, 'Reward') then
		-- Pet reward. Make sure that pet food already equipped
		if profile.sAmmo == nil or string.find(string.lower(profile.sAmmo),'pet f') == nil then		-- something else equipped
			profile.bAmmo = gcinclude.doPetFood('max',nil);
		end
		gcinclude.MoveToCurrent(sets.Reward,sets.CurrentGear);
	elseif string.match(ability.Name, 'Tame') then
		-- Trying to tame a beast. (Someone's charm failed.)
		gcinclude.MoveToCurrent(sets.Tame,sets.CurrentGear);		
	elseif string.match(ability.Name, 'Charm') then
		-- Trying to charm a beast. 
		gcinclude.MoveToCurrent(sets.Charm,sets.CurrentGear);
		gcinclude.SwapToStave('light',false,sets.CurrentGear);
	-- And now the subjob abilities
	-- /WAR
	elseif string.contains(ability.Name, 'Provoke') then
		gcinclude.MoveToCurrent(sets.Provoke,sets.CurrentGear);
	elseif string.contains(ability.Name, 'Berserk') then
		gcinclude.MoveToCurrent(sets.Berserk,sets.CurrentGear);
	elseif string.contains(ability.Name, 'Defender') then
		gcinclude.MoveToCurrent(sets.Defender,sets.CurrentGear);
	elseif string.contains(ability.Name, 'Warcry') then
		gcinclude.MoveToCurrent(sets.Warcry,sets.CurrentGear);
	--* /MNK *--
	elseif string.contains(ability.Name, 'Boost') then
		gcinclude.MoveToCurrent(sets.Boost,sets.CurrentGear);
	elseif string.contains(ability.Name, 'Focus') then
		gcinclude.MoveToCurrent(sets.Focus,sets.CurrentGear);
	elseif string.contains(ability.Name, 'Dodge') then
		gcinclude.MoveToCurrent(sets.Dodge,sets.CurrentGear);
	elseif string.contains(ability.Name, 'Chakra') then
		gcinclude.MoveToCurrent(sets.Chakra,sets.CurrentGear);
	-- /THF
	elseif string.contains(ability.Name, 'Steal') then
		gcinclude.MoveToCurrent(sets.Steal,sets.CurrentGear);
	elseif string.contains(ability.Name, 'Sneak Attack') then
		gcinclude.MoveToCurrent(sets.SneakAttack,sets.CurrentGear);
	elseif string.contains(ability.Name, 'Flee') then
		gcinclude.MoveToCurrent(sets.Flee,sets.CurrentGear);
	elseif string.contains(ability.Name, 'Trick Attack') then
		gcinclude.MoveToCurrent(sets.TrickAttack,sets.CurrentGear);
	elseif string.contains(ability.Name, 'Mug') then
		gcinclude.MoveToCurrent(sets.Mug,sets.CurrentGear);
	-- /WHM
	elseif string.contains(ability.Name, 'Divine Seal') then
		gcinclude.MoveToCurrent(sets.DivineSeal,sets.CurrentGear);
	-- /BLM
	elseif string.contains(ability.Name, 'Elemental Seal') then
		gcinclude.MoveToCurrent(sets.ElementalSeal,sets.CurrentGear);
	-- /DRK
	elseif string.contains(ability.Name, 'Arcane Circle') then
		gcinclude.MoveToCurrent(sets.ArcaneCircle,sets.CurrentGear);
	elseif string.contains(ability.Name, 'Last Resort') then
		gcinclude.MoveToCurrent(sets.LastResort,sets.CurrentGear);
	elseif string.match(ability.Name, 'Weapon Bash') then
		gcinclude.MoveToCurrent(sets.WeaponBash,sets.CurrentGear);
	elseif string.contains(ability.Name, 'Souleater') then
		gcinclude.MoveToCurrent(sets.Souleater,sets.CurrentGear);	
	-- /RNG
	elseif string.contains(ability.Name, 'Sharpshot') then
		gcinclude.MoveToCurrent(sets.Sharpshot,sets.CurrentGear);
	elseif string.contains(ability.Name, 'Scavenge') then
		gcinclude.MoveToCurrent(sets.Scavenge,sets.CurrentGear);
	elseif string.contains(ability.Name, 'Camouflage') then
		gcinclude.MoveToCurrent(sets.Camouflage,sets.CurrentGear);
	elseif string.contains(ability.Name, 'Barrage') then
		gcinclude.MoveToCurrent(sets.Barrage,sets.CurrentGear);	
	-- /SAM
	elseif string.contains(ability.Name, 'Warding Circle') then
		gcinclude.MoveToCurrent(sets.WardingCircle,sets.CurrentGear);
	elseif string.contains(ability.Name, 'Third Eye') then
		gcinclude.MoveToCurrent(sets.Third_Eye,sets.CurrentGear);
	elseif string.contains(ability.Name, 'Hasso') then
		gcinclude.MoveToCurrent(sets.Hasso,sets.CurrentGear);
	elseif string.contains(ability.Name, 'Meditate') then
		gcinclude.MoveToCurrent(sets.Meditate,sets.CurrentGear);
	elseif string.contains(ability.Name, 'Seigan') then
		gcinclude.MoveToCurrent(sets.Seigan,sets.CurrentGear);
	-- /DRG
	elseif string.contains(ability.Name, 'Ancient Circle') then
		gcinclude.MoveToCurrent(sets.AncientCircle,sets.CurrentGear);
	elseif string.contains(ability.Name, 'Jump') then
		gcinclude.MoveToCurrent(sets.Jumps,sets.CurrentGear);
	-- /PLD
	elseif string.match(ability.Name, 'Holy Circle') then
		gcinclude.MoveToCurrent(sets.HolyCircle,sets.CurrentGear);
	elseif string.match(ability.Name, 'Shield Bash') then
		gcinclude.MoveToCurrent(sets.ShieldBash,sets.CurrentGear);
	elseif string.contains(ability.Name, 'Sentinel') then
		gcinclude.MoveToCurrent(sets.Sentinel,sets.CurrentGear);	
	elseif string.contains(ability.Name, 'Cover') then
		gcinclude.MoveToCurrent(sets.Cover,sets.CurrentGear);
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