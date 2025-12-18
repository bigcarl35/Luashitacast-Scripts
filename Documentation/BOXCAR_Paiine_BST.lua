local profile = {};

local crossjobs = require('common.crossjobs');
local displaybar = require('common./displaybar');
local gear = require('common.gear');
local help = require('common.help');
local locks = require('common.locks');
local magic = require('common.magic');
local pets = require('common.pets');
local utilities = require('common.utilities');

--[[
	This file contains all the gear sets associated with the BST job.
	
	Gear Sets last updated: December 3, 2025
	Code update: December 3, 2025

	Intended Role: 50+
--]]

local sets = {
--[[
	Gear sets are self contained, a mixture of direct gear assignments and conditional
	assignments. Before gear swapping can occur, you must run /gc so the system can learn
	the gear from all your gear sets. Each set contains entries identified by the gear slot.
	If it's a single value, it's a direct assignment like: Body = 'Austere Robe', but there
	can be multiple	items identifying a priority order, usually ordered by level:

			Body = { 'Vermillion Cloak//PETNAME:Carbuncle',	'Austere Robe' },

	Any item that has a // appended to it contains an inline conditional. The // code defines
	a test to see if the item should be equipped. Even if normal checks pass (job, level,
	accessibility, etc), if the attached conditional(s) evaluates to false, the piece of gear
	will not be equipped.

	Not all sets included in this file need to be defined. There is nothing wrong with leaving
	a set "empty", but don't delete any empty sets. All the ones listed here (except for any
	custom sets) are expected to exist by Luashitacast.

	You'll find there are two types of sets defined in this file: Gear Sets and Reference
	Gear Sets. Both look very similar and contain gear listings, but are treated in different ways.
	Gear Sets are what Luashitacast equips based on actions that the code tracks. So things like
	are you fighting, casting a spell, resting, etc. Reference Gear Sets will never be directly
	equipped by Luashitacast except as subsets found in Gear Sets. For example, ['rEnmity_plus']
	is a Reference Gear Set since Luashitacast will not load it directly whereas ['TP'] is a Gear
	Set that is equipped when the player is "engaged" (fighting, weapon drawn, etc). Now, if
	you are evasion tanking, you might want to equip enmity+ gear when you're engaged, but
	Luashitacast will not address it directly. You must include it in a gear set via the Subset
	command. (Note: the "r" prefix I included in the reference gear set is a convention I use
	to make the Reference Gear Set stand out.)

	When processing a Gear Set (or Reference Gear Set), Luashitacast first processes all the
	Subsets at the current level, then the Groups, and finally the rest of the definition.
	(Levels within a gear set identify a depth in the definition.) Most gear sets only have
	one level. The exception is any that contain Groups. The level within a Group is self-
	contained, so processing a Group is separate from the main level.

			['Example'] = {
				Subset = 'XXX',						-- Main level
				Group//TANK = {
					Subset = 'YYY',					-- //TANK level
				},
				Group//NOT_TANK = {
					Subset = 'ZZZ',					-- //NOT_TANK level
					Group//NIGHTTIME = {
						Neck = 'Uggalepih Pendant,	-- this is at another level
					},
				},
			}

	A Group is treated like a mini gear set. When a Group is processed, any Subsets within
	that group will be processed first, followed by Groups, and lastly slot definitions. So,
	in the example: first Subset 'XXX' will processed and then depending on whether TANK
	is enabled or not, the appropriate Group will be processed. Let's assume NOT_TANK is true.
	Subset 'ZZZ' will be processed next and then Group NIGHTTIME. Assuming it's night time, the
	Group doesn't have any Subsets or Groups, so the slot definition would be processed.

	If you're going to have multiple groups (like in the example) in the same gear set, it's
	important that the different group definitions do not overlap. //TANK and //NOT_TANK are
	mutually exclusive, one or the other will be true, but if you have GROUP//TANK and
	GROUP//NIGHTTIME, it's possible that neither will be equipped nor both will be equipped.
	Since you can't guarantee which will be processed first, it's highly doubtful that what you
	expect to happen actually will happen. Now, if your groups contain different slots, then
	this is not a problem since you'll not have overlap. Just be conscious of this issue when
	you're defining your gear sets.
	
	Horizon changes from retail:
		- Moderate changes to AF gear
		- Significant changes to Gutter
		- Night flowers that need to be examined in "Save My Son" can now be examined as early as
		  16:00 and as late as 6:00
		- Job ability Ready available (this is out of era). Also, changes are gained every 45 
		  seconds as opposed to 60 seconds found in retail. Max charges are still 3.
		- Every physical and magic damage dealing TP moves are no longer imbued with
		  skillchain/magic burst properties
		- Pets will persist through player zoning
		- All HQ jug pets now cap at level 75
		- Pet food has been buffed to provide increased HP/stats to pets
--]]

--[[
	The "default" gear set is what is worn when you're not fighting (neither you nor a pet)
	and you're not resting. It covers everything else: idling, traveling, in town, etc. This
	set displays what your character looks like most of the	time. This set does not
	distinguish the type of activities you're doing by default, so use inlines accordingly.

	Note: The inclusion of the non-group entries will not cause an issue since the slots
	are not found in either group, so order of process doesn't matter.

	Priority:
		Recovery (while standing), movement, defense, fashion
--]]

	['Default'] = {
		GROUP//TOWN = {
			-- You're in town, show your fancy duds
			SUBSET = 'rFancyAttire',
		},
		GROUP//NOT_TOWN = {
			GROUP//KITE = {
				SUBSET = 'Evasion',
			},
			GROUP//NOT_KITE = {
				GROUP//RIDING = {
					SUBSET = 'rFancyAttire',
				},
				GROUP//NOT_RIDING = {
					Head   = { 'President. Hairpin//NOT_OWN//HPP.LT.94', 'Empress Hairpin' },
					Neck   = { 'Opo-opo necklace//SLEPT', 'Peacock Amulet' },
					Ears   = { 'Bat Earring//BLINDED', 'Ethereal Earring', 'Coral Earring//DT_MAGICAL', 'Beastly Earring', 'Brutal Earring', 'Genin Earring//SJNIN', 'Bat Earring//MSJ', 'Fang Earring',
							   'Energy Earring +1//MSJ', 'Energy Earring +1//MSJ' },
					Body   = { 'Gaudy Harness//MSJ//MP.LT.50', 'Narasimha\'s Vest' },
					Hands  = { 'Beast Gloves', 'Ogre Gloves' },
					Rings  = { 'Flame Ring', 'Tamas Ring//MSJ', 'Kshama Ring No.8'  },
					Back   = { 'Forager\'s Mantle', 'Amemet Mantle' },
					Waist  = { 'Swift Belt', 'Tilt Belt' },
					Legs   = { 'Monster Trousers', 'Ryl.Kgt. Breeches', 'Shep. Hose' },
					Feet   = { 'Monster Gaiters', 'Beast Gaiters', 'Wonder Clomps', 'Bounding Boots' },
				},
			},
		},
	},

--[[
	rFancyAttire is for when you want to look snazzy. It's intended to be used when you're not engaged and don't anticipate
	being engaged in the near future (ex: in town or riding.) This is an optional reference set.
--]]

	['rFancyAttire'] = {
		Head  = { 'Lilac Corsage', 'Monster Helm', 'Shep. Bonnet' }.
		Neck  = 'Love Torque',
		Ears  = { 'Brutal Earring', 'Ethereal Earring' },
		Body  = { 'Ducal Aketon//TOWN-AK', 'Gaudy Harness//MSJ//MP.LT.50', 'Narasimha\'s Vest' },
		Hands = { 'Beast Gloves', 'Ogre Gloves' },
		Rings = { 'Flame Ring', 'Kshama Ring No.8' },
		Back  = 'Forager\'s Mantle',
		Waist = 'Warwolf Belt',
		Legs  = { 'Monster Trousers', 'Ryl.Kgt. Breeches' },
		Feet  = { 'Monster Gaiters', 'Bounding Boots' }
	},

--[[
	The TP set is used when you and/or your pet are fighting. The accuracy set will be applied
	in a fractional manner. The evasion set if equipped if /eva is specified.
--]]

	['TP'] = {
		SUBSET = 'Default',
		GROUP//KITE = {
			SUBSET = 'Evasion',
		},
		GROUP//NOT_KITE = {
			Head  = { 'Shep. Bonnet//PET', 'Empress Hairpin' },
			Body  = { 'Gaudy Harness//MSJ//MP.LT.50', 'Narasimha\'s Vest', 'Scorpion Harness', 'Gaudy Harness' },
			Hands = { 'Shep. Bracers//PET', 'Beast Gloves', 'Ogre Gloves' },
			Back  = { 'Forager\'s Mantle', 'Psilos Mantle', 'Amemet Mantle' },
			Legs  = { 'Shep. Hose//PET', 'Monster Trousers', 'Ryl.Kgt. Breeches' },
		}.
    },
	
--[[
	If an accuracy emphasis is desired, the following set will replace the gear, appropriately.
	Unlike TP though, accuracy is applied one slot at a time in a fractionalized manner using
	the /acc command.
	
	Include equipment with accuracy bonus and DEX. Remember, DEX converts to accuracy: (horizon) 
	for every 1 point of DEX you get 0.70 points of accuracy if wielding a 2H weapon, 0.65 for 
	a 1H weapon, and 0.60 for H2H.

	Note: With BST, accuracy should favor a pet if player not fighting
--]]

	['rAccuracy'] = {
		Ammo  = 'Orphic Egg//PJPBRD',															-- +1 Acc if BRD in your party
        Head  = { 'Shep. Bonnet//PETFNPF', 'Optical Hat', 'Empress Hairpin' } ,					-- +10 Pet Acc, +10 Acc, +3 DEX
        Neck  = { 'Peacock Amulet', 'Reraise Gorget', 'Spike Necklace' },						-- +10/3 Acc, +3 DEX
        Body  = { 'Scorpion Harness', 'Narasimha\'s Vest', 'Beast Jackcoat', 'Brigandine' },	-- +10/4/3 Acc, +2 DEX
        Hands = 'Battle Gloves',																-- +3 Acc
		Ears  = 'Beastly Earring//PETF'},														-- +10 Pet Acc
		Rings = { 'Toreador\'s Ring', 'Toreador\'s Ring', 'Woodsman Ring', 'Woodsman Ring' },	-- +7/7/5/5 Acc
        Back  = 'Psilos Mantle',																-- +1 Acc
        Waist = { 'Life Belt', 'Monster Belt', 'Tilt Belt', 'Swift Belt' },						-- +10/7/5/3 Acc
        Legs  = 'Monster Trousers',																-- +4 DEX
        Feet  = 'Bounding Boots',																-- +3 DEX
    },	

--[[
	Similar to the accuracy set, the Ranged_Accuracy set will be used on ranged attacks
--]]

	['rRanged_Accuracy'] = {
		Head = 'Optical Hat',							-- +10 RAcc
		Neck = { 'Peacock Amulet', 'Reraise Gorget' },	-- +10/3 RAcc
		Rings = { 'Woodsman Ring', 'Woodsman Ring' },	-- +5/5 RAcc
		Back = 'Psilos Mantle',							-- +1 RAcc
	},

--[[
	The Progressive structure is gear set-like, offering a way to group slot definitions
	into stages that can be applied in a progressive manner. There are four valid types
	that can be defined in the structure: Accuracy, Tank_Accuracy, Ranged_Accuracy, and
	Tank_Ranged_Accuracy. (If the "Tank" entries are missing, Luashitacast will use the
	non-Tank versions instead.)

	Each section can be broken out into stages. The number of stages is defined by the
	player. A stage is identified by a number and contains one or more slot/subset
	entries. (It is best to keep the numbers in order and not to skip any in the
	sequence. When represented in the display bar, the actual numbers are not used.
	Instead Luashitacast recognizes the number of stages and just numbers 1 to that
	number.) What is recommended for each slot definition is a reference to a slot in
	one of the special reference gear sets: rAccuracy or rRanged_Accuracy. However, there
	is nothing stopping you from listing explicitly a gear list like you can in normal
	gear sets. Which approach you do is up to the player.

	Stages are applied in a progressive manner. That means that each stage is
	additive. You use the /acc or /racc commands to indicate which collective stage(s)
	should be applied. (By that I mean, if you type /acc 2, both stage 1 and 2 will
	be applied.) To turn off the acc/racc, just use the appropriate commands without
	identifying a stage. Verification of what's the current stage can be seen in the
	display	bar. Please note that rAccuracy and rTank_Accuracy will be displayed in
	the Acc: section and that rRanged_Accuracy and rTank_Ranged_Accuracy will be
	displayed in the Racc: section. (While you can have a different number of stages
	between the	tank and non-tank versions, the number of stages displayed in the toolbar
	reflects whether TANK is enabled or not. Also note that the TANK versions are
	separate from the non-TANK versions even if they use the same definitions.)
--]]

	['Progressive'] = {
		['Accuracy'] = {
			[1] = {
				['Head'] = 'rAccuracy::Head',
				['Neck'] = 'rAccuracy::Neck',
				['Body'] = 'rAccuracy::Body',
			},
			[2] = {
				['Body']  = 'rAccuracy::Body',
				['Hands'] = 'rAccuracy::Hands',
				['Legs']  = 'rAccuracy::Legs',
				['Feet']  = 'rAccuracy::Feet',
			},
			[3] = {
				['Rings'] = 'rAccuracy::Rings',
			},
			[4] = {
				['Subset'] = 'rAccuracy',
			},
		},
		['Ranged_Accuracy'] = {
			[1] = {
				['Head'] = 'rRanged_Accuracy::Head',
				['Neck'] = 'rRanged_Accuracy::Neck',
				['Legs'] = 'rRanged_Accuracy::Legs'
			},
			[2] = {
				['Subset'] = 'rRanged_Accuracy',
			},
		},
	},

--[[
	If evasion wanted, equip evasion gear. Remember that AGI converts to evasion: for 
	every 2 points of AGI you get 1 point of evasion. Note that if you leave the body
	slot empty, but designate a piece of head gear and the previous body slot had a
	multi-slot body piece (like Vermillion Cloak), then body slot will be left empty.	
--]]

	['Evasion'] = {
		Ammo  = 'Orphic Egg//PJPBRD',							-- +1 Eva if BRD in party
        Head  = { 'Optical Hat', 'Empress Hairpin' },			-- +10/10 Eva
		Ears  = { 'Bat Earring//BLINDED', 'Ethereal Earring', 'Reraise Earring', 'Genin Earring//SJNIN', 'Drone Earring' },	-- +15 Eva while blinded, +5/2 Eva, +4/3 AGI
        Body  = { 'Scorpion Harness', 'Narasimha\'s Vest' },	-- +10/4 Eva
		Hands = 'Battle Gloves',								-- +3 Eva
        Legs  = 'Shep. Hose//PETFNPF',							-- +3 Pet Eva
		Waist = 'Life Belt//IF:TILT Belt',						-- filler, removes -5 eva on tilt belt
		Feet  = { 'Dance Shoes', 'Bounding Boots' },			-- +6 Eva, +3 AGI
    },

--[[
	rDamageTaken set is not equipped directly but rather from subsets since it's a reference set. It's a
	way to reduce a specific type of damage. As such it's optional and up to the player to decide where
	it should be included via a Subset. (Prior versions had three separate sets.)
--]]

	['rDamage_Taken'] = {
		GROUP//DT_PHYSICAL = {
			Main//WSWAP = 'Earth Staff',			-- -20% damage reduction from physical
		},
		GROUP//DT_BREATH = {
		},
		GROUP//DT_MAGICAL = {
			Ears = 'Coral Earring',					--  -1% damage reduction from magic
		},
	},

--[[
	When you are resting (kneeling down), if your HP is not full, your HP 'Resting' 
	set will be equipped. If your MP is below maximum value, your MP 'Resting_Refresh' 
	gear set will be equipped. 
		
	The Damage_Taken_* sets are added as a subset to reduce damage accordingly because
	you're in a vulnerable state.
--]]
	
	['Resting_Refresh'] = {
		SUBSET = 'rDamage_Taken',
		Main   = 'Pluto\'s Staff//WSWAP//MSJ',							-- +10 MP/tic while healing
		Body   = 'Gaudy Harness//MSJ//MP.LT.50',						-- +1 MP/tic while MP < 50
	},

	['Resting_Regen'] = {
		SUBSET = 'rDamage_Taken',
		Head   = 'President. Hairpin//NOT_OWN',							-- Adds 'regen' if zone not controlled by nation
		Hands  = { 'Carbuncle Cuffs//SHINING_RUBY', 'Shep. Bracers' },	-- +5 HP/tic while player has 'Shining Ruby' buff, +1 HP/tic
		Legs   = 'Monster trousers',									-- +3 HP/tic
	},
		
--[[
	Start weapons are where you define what you want the first row of equipment to look 
	like when you either log in as a BST or switch your main job to BST. Any other gear 
	you mention will be overridden by the Idle or Town set, so no need to include here.
--]]

	['Start_Weapons'] = {
        Ammo = { 'Hedgehog Bomb//MSJ', 'S. Herbal Broth' },
		Main = { 'Maneater', 'Barbaroi Axe' },
		GROUP//DUALWIELD = {
			Sub = 'Tabarzin',
		},
		GROUP//NOT_DUALWIELD = {
			Sub = 'Tatami Shield',
		},
    },
	
--[[
	Preshot is the first stage of when a ranged shot is being performed. This is where 
	you place any gear that reduces the time it takes to shoot (snap shot, rapid shot, 
	quick shot, shot delay reduction, and ranged haste).
--]]

	['Preshot'] = {
    },

--[[
	Midshot is the second stage of a ranged shot. This is where you place
	Ranged Accuracy, Ranged Attack, Ranged Damage, Crit. Rate, Crit. Damage,
	Store TP, recycle, etc.
--]]

	['Midshot'] = {
		Back  = { 'Psilos Mantle', 'Amemet Mantle' }		-- +12/10 RAtt
    },

--[[
	**********************
	* Spell Casting Sets *
	**********************
	

	Initially define the Reference gear sets that are primary stat based.

	Note: as log as the reference set does not contain any weapons, these
	reference sets can be referred to in weapon skill sets.
--]]

	-- Strength Reference gear set
	['rSTR'] = {
		Neck  = { 'Justice Torque', 'Spike Necklace' },		-- +5/3 STR
		Ears  = 'Beastly Earring//AXE',						-- Boosted skill if using an axe
		Body  = { 'Narasimha\'s Vest', 'Wonder Kaftan' },	-- +6/1 STR
		Hands = { 'Ogre Gloves', 'Wonder Mitts' },			-- +6/3 STR
		Rings = { 'Flame Ring', 'Kshama Ring No.8' },		-- +5/3 STR
		Back  = { 'Forager\'s Mantle', 'Amemet Mantle' },	-- +3/1 STR
		Waist = 'Warwolf Belt',								-- +5 STR
		Legs  = 'Wonder Braccae',							-- +1 STR
		Feet  = { 'Creek F Clomps', 'Wonder Clomps' },		-- +4/2 STR
	},

	-- Dexterity Reference gear set
	['rDEX'] = {
		Head   = 'Empress Hairpin',							-- +3 DEX
		Neck   = { 'Love Torque', 'Spike Necklace' },		-- +5/3 DEX
		Ears   = 'Beastly Earring//AXE',					-- Boosted skill if using an axe
		Body   = { 'Haubergeon', 'Brigandine' },			-- +5/2 DEX
		Hands  = 'Beast Gloves',							-- +3 DEX
		Rings  = 'Kshama Ring No.2',						-- +3 DEX
		Waist  = 'Warwolf Belt',							-- +5 DEX
		Legs   = 'Monster Trousers',						-- +4 DEX
		Feet   = 'Bounding Boots',							-- +3 DEX
	},

	-- Vitality Reference gear set
	['rVIT'] = {
		Body   = { 'Narasimha\'s Vest', 'Brigandine' },		-- +3/2 VIT
		Rings  = 'Kshama Ring No.4',						-- +3 VIT
		Waist  = 'Warwolf Belt',							-- +5 VIT
		Legs   = 'Wonder Braccae',							-- +2 VIT
		Feet   = 'Creek F Clomps',							-- +4 VIT
	},

	-- Agility Reference gear set
	['rAGI'] = {
		Head  = 'Empress Hairpin',							-- +3 AGI
		Ears  = { 'Genin Earring//SJNIN', 'Drone Earring' },-- +4 AGI if /NIN, +3 AGI
		Rings = 'Kshama Ring No.3',							-- +3 AGI
		Back  = 'Fed. Army Mantle',							-- +2 AGI
		Waist = 'Scouter\'s Rope',							-- +4 AGI
		Feet  = 'Bounding Boots',							-- +3 AGI
	},

	-- Intelligence Reference gear set
	['rINT'] = {
		Head  = 'Beast Helm',                               -- +5 INT
		Hands = 'Beast Gloves',                             -- +6 INT
		Rings = { 'Tamas Ring', 'Kshama Ring No.5', 'Flame Ring' },	-- +5/3/2 INT
		Feet  = 'Mannequin Pumps',							-- +1 INT
	},

	-- Mind Reference gear set
	['rMND'] = {
		Head  = 'Bst. Helm +1',								-- +8 MND
		Neck  = { 'Promise Badge', 'Justice Badge' },		-- +5/3 MND
		Ears  = 'Geist Earring',							-- +1 MND
		Body  = 'Wonder Kaftan',							-- +1 MND
		Rings = { 'Tamas Ring', 'Kshama Ring No.9' },		-- +5/3 MND
		Waist = 'Friar\'s Rope',							-- +1 MND
		Legs  = 'Wonder Braccae',							-- +2 MND
		Feet  = 'Mannequin Pumps',							-- +2 MND
	},

	-- Charisma Reference gear set. Provides accuracy with singing
	['rCHR'] = {
		Head  = 'Monster Helm',								-- +4 CHR
		Neck  = { 'Star Necklace', 'Flower Necklace' },		-- +3/3 CHR
		Ears  = 'Beastly Earring',							-- +2 CHR
		Body  = 'Gaudy harness',							-- +3 CHR
		Rings = 'Kshama Ring No.6',							-- +3 CHR
		Waist = 'Monster Belt',								-- +6 CHR
		Legs  = 'Beast Trousers',							-- +4 CHR
		Feet  = { 'Dance Shoes', 'Beast Gaiters' },			-- +3/3 CHR
	},

	-- Then Enmity based sets

	-- Enmity+ Reference gear set, for player

	['rEnmity_Plus'] = {
		Waist = 'Warwolf Belt',								-- +3 Enmity
	},
	
	['rEnmity_Minus'] = {
		Neck  = 'Fenrir\'s Torque//NIGHTTIME',				-- -3 Enmity at night
		Rings = 'Tamas Ring',								-- -5 Enmity
	},

	-- And magic or attack base sets

	-- Magic Attack Bonus Reference set
	['rMAB'] = {
		Neck   = 'Uggalepih Pendant//SPECIAL',				-- +8 MAB if MP < 51%
	},

	['rAttackPower'] = {
		Ears  = { 'Ethereal Earring', 'Coral Earring', 'Fang Earring' },	-- +5/5/4 Att
		Rings = 'Kshama Ring No.8',							-- +3 Att
		Back  = { 'Forager\'s Mantle', 'Psilos Mantle', 'Amemet Mantle' },	-- +15/12/10 Att
		Legs  = 'Tracker\'s Kecks',							-- +6 Att
		Waist = 'Warwolf Belt//IF:SWIFT BELT',				-- Remove -5 Att from Swift Belt
	},

--[[
	Spells are a bit different. Each type of spell can have it's own enhancement gear as well as 
	stat based gear. (In some cases individual spells have special entries.) These sets do not 
	include elemental gear which is	dependent on day/weather/weapon skill.

	The first stage is Precast. This is where you place any Fast Cast, cast time reduction, and
	quick cast gear
--]]

	['Precast'] = {	
		Ears = 'Loquac. Earring',							-- Enhances Fast Cast
	},

--[[
	A lot of spells have a better chance of landing if you increase your
	magic accuracy. By defining the Macc set, if the option is enabled
	on your displaybar, the defined gear will be equipped on all spells
	where magic accuracy can have an effect. Alternatively, leave this
	gearset empty and explicitly include magic accuracy gear in each of
	the individual midcast gear sets. (Doing so removes the ability to
	optionally not include magic accuracy.)
--]]
	
	['Macc'] = {
		SUBSET = {'rDark_Magic_Skill//DARK',
				  'rElemental_Magic_Skill//ELEMENTAL',
				  'rEnfeebling_Magic_Skill//ENFEEBLING',
				  'rHealing_Magic_Skill//HEALING',		-- Offensive healing only
				  'rDivine_Magic_Skill//DIVINE',
				  'rNinjutsu_Skill//NINJUTSU',
				  'rSinging_Skill//SINGING' },
		},
		Rings  = 'Tamas Ring',			-- +5 MAcc	
	},

--[[
	The second stage is Midcast. This is where you equip gear that gives 
	magic attack, enhancing bonuses, potency improvements, duration
	enhancements, recast reduction gear, etc. This implementation breaks 
	out the midcast into separate routines based on the magic type: 
	healing, divine, elemental, enhancing, enfeebling, summoning, ninjutsu,
	and song. Within each routine there's further logic breaking down
	functional paths. Based on spell (or song), the appropriate gear set
	is equipped.
	
	Every gear set will have an alternative "tanking" version if your
	main job supports tanking (BST does not.) I've included details on
	what each gear set is suppose to feature and what stats you should
	be emphasizing. Further, where appropriate, any formulas/charts that
	will help you to decide what gear to include. 
--]]

--[[
	**************************
	* Midcast: Healing Magic *
	**************************
--]]

--[[
	Healing Magic: consisting of all light-based spells, removes 
	some debuffs on players, buffs the caster, cures the health of 
	players or npcs, or causing damage to undead monsters. Healing 
	magic skill affects the	potency of cures while decreasing the 
	likelihood of the caster being interrupted.
	
	Healing spells: cures, curagas, raises, reraises, blindna, cursna,
	paralyna, poisona, silena, stona, and viruna.
--]]

	['rHealing_Magic_Skill'] = {
	},
	
--[[	
	Curing magic addresses healing players/npcs. Each time a cure 
	spell is cast, a power calculation is performed to determine 
	the base effect of the spell. After that, any bonuses will be 
	applied. What this means is that MND, VIT and healing magic 
	skill impact your power rating, but once the cap is hit, they 
	have no more influence.
	
		power = (MND*3) + VIT + (Healing Magic Skill*0.6)
	
	This chart lists all /WHM or /RDM curing spells, the power cap, 
	and the effect on HP baseline. Curaga spells are included too.
		
			Spell		cap		low		high
			-----		---		---		---
			Cure		100		 20		 30
			Cure II		170		 75		 90
			Cure III	300		160		190
			Curaga		170		 75		 90
			Curaga II	300		160		190
	
	At this point, any bonuses from day's element, weather, elemental
	staff, or gear/food with potency affects will be tacked on. Because
	cure potency is applied after the power cap is determined, it's a
	very attractive parameter to boost up. Just remember though that 
	cure potency is capped at 50%. Light's day and/or light weather 
	has a 33% chance to boost the cure's effecacy by 10% each (25% 
	if double light weather.) Casting cures on darksday or in dark 
	weather has an equal chance of a penalty.
	
	Once the "CuringMagic" set is equipped, the midcast routine will
	also check to see if you have an Apollo/Light staff for it's Cure 
	Potency.
--]]	
	
	['CuringMagic'] = {
		SUBSET = {
			[1] = 'rHealing_Magic_Skill',
			[2] = 'rMND',
		},
		Body   = { 'Narasimha\'s Vest', 'Brigandine' },		-- +3/2 VIT
		Waist  = 'Warwolf Belt',		-- +5 VIT
		Rings  = 'Kshama Ring No.4',	-- +3 VIT
		Feet   = 'Creek F Clomps',		-- +4 VIT
	},	
	
--[[
	As for the offensive use of cure spells against undead monsters, 
	most of	what was said about CuringMagic is true except cure potency. 
	This has no effect on undead monsters.
	
	After the OffensiveCuring set is equipped, the midcast routine will 
	see if a Korin Obi can be equipped to take advantage of the 100% 
	proc rate of the day's element/weather. Also, like normal curing 
	magic, an Apollo/Light staff will be check for,	but not for the 
	cure potency. Rather, for magic affinity.
--]]

	['OffensiveCuring'] = {
		SUBSET = {
			[1] = 'rHealing_Magic_Skill',
			[2] = 'rMND',
			[3] = 'rMAB',
		},
	},

--[[
	This set is used for all non-cure Healing Magic spells. Only 
	healing magic skill is of any importance here. You might want 
	to use this set as a subset for the other cure-based sets.
--]]
	['HealingMagic'] = {
		SUBSET = 'rHealing_Magic_Skill',
	},

--[[
	****************************
	* Midcast: Enhancing Magic *
	****************************
--]]
	
--[[
	Enhancing Magic: This type of magic includes a wide variety of spells 
	that enhances players as well as movement spells. It's sort of a catch 
	all category.
	
	Enhancing Spells: bar/ra elemental spells, bar/ra status spells, blink,
	aquaveil, stoneskin, phalanx, protect/ra spells, shell/ra spells, erase,
	regen, refresh, deoderize, invisible, sneak, haste, spike spells (not
	dread), escape, teleport spells, warp spells, en- spells (except 
	enlight.)
--]]
	
	['rEnhancing_Magic_Skill'] = {
	},
	
--[[
	There are two versions of barspells: elemental and status, both of which
	increase the magic evasion of a player from the element/status named.
	Only one barspell of each type can be enabled at the same time. Pairing 
	barspells that have the same element increases magic evasion further 
	(ex: barparalyze and barblizzard).
	
	The potency of an elemental barspell depends only on Enhancing Magic
	Skill as follows:
	
		Resistance = 40 + floor(Enhancing Magic Skill / 5)
		if Enhancing Magic Skill is above 300:
			Resistance = 25 + floor(Enhancing Magic Skill / 4)
	
	You can increase the resistence through gear or WHM category 1 merits.
	* There's a cap of 500 Enhancing Magic Skill, but that might be from the
	  99 era.
	
	Little is known about the potency of barstatus spells, but enhancing
	magic skill does affect the duration.
	
		duration(seconds) = Enhancing Magic Skill x 2
--]]

	['Barspell'] = {
		SUBSET = 'rEnhancing_Magic_Skill',
	},
	
--[[
	Enspells buff the player's melee weapon so that when they hit, there's
	also elemental damage being applied. The amount of damage is calculated 
	when the weapon hits.
	
		Base Damage = floor(6 * E / 100) + 3 if E <= 200
					  floor(5 * E / 100) + 5 if E > 200
			where E is your Enhancing Magic Skill
		
	When WotG comes out, Enspells II are introduced which are fairly amazing.
	I'll update this comment when that occurs.
	
	Enspells are also affected by corresponding day's element and weather,
	but not magic affinity. The appropriate obi will automatically be equipped
	if the conditions are met.
--]]

	['Enspell'] = {
		SUBSET = 'rEnhancing_Magic_Skill',
	},
	
--[[
	Spikes place an elemental buff around the player which causes damage/
	status effects to any monster that hits the player. Each type of spike 
	spell has a different formula for how much damage they do and only 
	some potentially add a status effect. All spikes all are based on INT 
	and Enhancing Magic Skill.
	
	The maximum damage is determined by INT. Enhancing Magic Skill helps
	spikes do full damage while lessening the likelihood of the spell being
	resisted.
	
	Blaze Spikes: integer(integer(((INT+2)/12) + 4) * (1 + (MAB/100)))
	Ice/Shock spikes: integeer(integer(((INT+10)/20) + 2) * (1 + (MAB/100)))
--]]
	
	['Spike'] = {
		SUBSET = {
			[1] = 'rEnhancing_Magic_Skill',
			[2] = 'rINT',
			[3] = 'rMAB',
		},
	},
	
--[[
	The rest of the the gear sets for Enhancing Magic are for specific
	spells: stoneskin, sneak, invisible, and phalanx. Include gear in
	the appropriate set that enhances the named spell accordingly.

	Stoneskin absorbs a set amount of damage before wearing off. How much
	it absorbs depends on the caster's MND and Enhancing Magic Skill.
	
		Base = floor(Enhancing Magic Skill/3 + MND)
		
		if Base < 80, then absorbed amount equals base
		if 80 <= Base < 130, then absorbed amount = floor((2 * Base) - 60)
		if 130 <= Base, then absorbed amount = floor((3 * Base) - 190)
		
		Absorbed damage is capped at 350
		
	Any equipment that enhances stoneskin grants a flat bonus to the spell's
	effect that can go past the natural cap. This equipment must be worn by
	the player who has stoneskin cast on them. (At this time only the caster
	can be the reciever.)
--]]

	['Stoneskin'] = {
		SUBSET = {
			[1] = 'rEnhancing_Magic_Skill',
			[2] = 'rMND',
		},
	},	

--[[
	Sneak's duration is variable, but the duration maxes at about 5 
	minutes. Include any gear that enhances this buff.
--]]

	['Sneak'] = {
		SUBSET = 'rEnhancing_Magic_Skill',
		Feet = 'Dream Boots +1',
	},

--[[
	Invisible's duration is variable, but the duration maxes at 
	about 5 minutes. Include any gear that enhances this buff.
--]]	

	['Invisible'] = {
		SUBSET = 'rEnhancing_Magic_Skill',
		Hands = 'Dream Mittens +1',
	},

--[[
	Phalanx gives the reciever a certain amount of damage resistance.
	The amount of resistance is calculated after every hit taken and is 
	a function of the caster's Enhancing Magic Skill at the time the 
	spell was cast.
	
		resistance = floor ((E / 10) - 2) if E <= 300
					 floor(((E - 300.5) / 28.5) + 28) if E > 300
			where E is the caster's Enhancing Magic Skill
		
		The resistance caps at 35.
--]]
	
	['Phalanx'] = {
		SUBSET = 'rEnhancing_Magic_Skill',
	},	

--[[
	This set handles the rest of the enhancing spells not covered by barspell,
	en-spells, spikes, and the individual enhancing spell gear sets. Enhancing 
	magic skill determines potency (if appropriate) and decreases the 
	likelihood of an enhancing spell being interrupted. Enhancing magic is not 
	affected by magic affinity, so elemental staves are not needed, but en- 
	spells can be affected by the day/weather effects.
--]]

	['EnhancingMagic'] = {
		SUBSET = 'rEnhancing_Magic_Skill',
	},

--[[
	****************************
	* Midcast: Elemental Magic *
	****************************
--]]

	['rElemental_Magic_Skill'] = {
	},
	
--[[
	Elemental Magic: This type of magic consists of nukes, ancient magic (a type
	of nuke), and elemental debuffs. Elemental Magic Skill determines the accuracy
	and help resist spell interuptions. It has no effect on damage except for the
	meteor skill which is out of era. All elemental spells are consider to be
	either a nuke or debuff.
	
	Elemental spells: aeros, aerogas, blizzards, blizzagas, burn, burst, drown
	fires, firagas, flare, flood, quake, rasp, sjhock, stones, stonegas, thunders,
	thundagas, tornado, waters, and watergas.
	
	Elemental magic and ancient magic is grouped together. CaLculating magic
	damage is an extensive process. (If you want to see all of the gory details,
	please refer to the Elemental Magic section in the magic.txt file found in
	the Documentation subdirectory.)
	
	The important things to remember are: the difference between the caster's 
	INT and the monster's INT (dINT) scales the	wider the gap becomes. The 
	nuke spell's tier caps the dINT that is counted, so a tier 1 caps at 100,
	tier 2 at 200, etc. Magic affinity and day/weather can boost/penalize 
	damge. Hitting a single target does more damage (even with an AoE spell)
	than two or more targets. Boosting Magic Attack Bonus (MAB) will increase
	damage. Hitting NMs with the same spell within 5 seconds again will 
	reduce your elemental damage by 20% ("nuke wall"), excluding skillchains.	
--]]

	['ElementalNuke'] = {
		SUBSET  = {
			[1] = 'rElemental_Magic_Skill',
			[2] = 'rINT',
			[3] = 'rMAB',
		},
	},	

--[[
	Elemental debuffs work in a simlar fashion to elemental nukes except they
	apply a damge over time (DOT) debuff and lessen a primary stat. How effective
	the elemental debuff is depends strictly on the caster's INT.
	
		  1 -  39 INT: 1 hp/tic and  -5 to the stat
		 40 -  69 INT: 2 hp/tic and  -7 to the stat
		 70 -  99 INT: 3 hp/tic and  -9 to the stat
		100 - 149 INT: 4 hp/tic and -11 to the stat
		>150 INT:	   5 hp/tic and -13 to the stat
	
	A target can be afflicted by as many as three different elemental debuffs
	as long as the spells' element doesn't interact with each other. So,
	rasp (earth, Dex down), Drown (water, STR down), and Frost (ice, AGI down)
	can coexist and Burn (fire, INT down), Choke (wind, VIT down), and Shock
	(lightning, MND down) can coexist. (Note that the damage done by an
	elemental debuff can wake up a player/monster that is sleeping.) Elemental
	Magic Skill, Magic Affinity, and Magic Accuracy increase the likelihood of
	the debuff not being resisted.
	
	An elemental obi and elemental staff (with //WSWAP) will be equipped if 
	available automatically.
--]]

	['ElementalDebuff'] = {
		SUBSET  = {
			[1] = 'rElemental_Magic_Skill',
			[2] = 'rINT',
			[4] = 'rMAB'
		},
	},

--[[
	**********************
	* Midcast: Summoning *
	**********************
--]]

	['rSummoning_Magic_Skill'] = {
		Neck = 'Smn. Torque',		-- +7 Summoning Magic Skill
	},
	
--[[
	Summoning: This type of magic is used when a summoner casts either an
	avatar or an elemental spirit. It is a very straightforward type of
	magic. Summoning Magic Skill mostly affect elemental spirits, decreasing
	the wait time between when the spirit is summoned and it casts a spell
	and the wait time between spells. Further, the intelligence of the AI
	increases. The spirit will cast more powerful spells and more appropriate
	spells more often. Summoning magic skill also descreases the likelihood
	of a summons being interrupted.
--]]

	['Summoning'] = {
		SUBSET = 'rSummoning_Magic_Skill',
	},

--[[
	***********************
	* Midcast: Dark Magic *
	***********************
--]]

--[[
	Dark Magic: This type of magic is used to absorb from a target, whether
	stats, mana, or HP. Further, it can weaken an enemy's attack while applying
	a DoT debuff, stun, and move a k.o.'ed player. Dark Magic Skill determines
	accuracy, potency of some spells (not absorbs), and descreases the 
	likelihood of the caster being interrupted.
	
	Dark Magic Spells: absorb accuracy, absorb AGI, absorb CHR, absorb DEX, 
	absorb INT, absorb MND, absorb STR, absorb TP, absorb VIT, aspir, bios,
	drain, stun and tractor.
--]]

	['rDark_Magic_Skill'] = {
	},
	
--[[
	There's 9 absorb spells (although some are currently out of era). If not
	resisted, they drain a specific stat from the target based on the caster's
	level:
	
		base absorbed = floor (3 + (job level) / 5)
	
	Dark magic skill has no effect on absorb spell, but do affect accuracy.
	Absorb spells resisted will have their duration cut in half or be completely
	resisted. Equipment that "Enhances" an absorb spell will increase the spells
	duration. Equipment that "Augments" an absorb spell will increase the spells
	potency.
--]]
	
	['Absorb'] = {
		SUBSET = 'rDark_Magic_Skill',
	},

--[[
	Drain steals HP from the target and absorbs it into the caster's HP pool.
	Base potency depends strictly on the caster's Dark Magic Skill:
	
		0 - 299 skill: floor((dark magic skill / 3) + 20)
		>= 300 skill: floor(dark magic skill * 0.9)
	
	The minimum potency is 50% of the maximum potency and the actual potency
	of the spell (when unresisted) will randomly fall between the minimum and
	the maximum.  All enhancements from gear, weather, and magic burst bonuses
	are applied after the base potency is determined. Drain is not affected 
	by magic attack bonus and magic crit+ hit on gear.
--]]

	['Drain'] = {
		SUBSET = 'rDark_Magic_Skill',
	},

--[[
	Aspir steals MP from the target and absorbs it into the caster's MP pool
	(assuminging the target has any MP.) Base potency depends strictly on the
	caster's Dark Magic Skill:
	
		0 - 300 skill: floor(skill / 3) + 20
		>= 300 skill: floor(skill * 0.4)

	The minimum potency is 50% of the maximum potency and the actual potency
	of the spell (when unresisted) will randomly fall between the minimum and
	the maximum. All enhancements from gear, weather, and magic burst bonuses
	are applied after the base potency is determined. Aspir is not affected 
	by magic attack bonus and magic crit+ hit on gear.	
--]]

	['Aspir'] = {
		SUBSET = 'rDark_Magic_Skill',
	},

--[[
	This last gear set, DarkMagic, covers all Dark Magic spells not covered
	by the previous three gear sets. 
--]]

	['DarkMagic'] = {
		SUBSET = 'rDark_Magic_Skill',
	},

--[[
	Currently Dread Spikes are out of era, but they're introduced in ToAU,
	so I've included them here. At the moment the code only applies a generic
	spell invocation.
--]]
	
--	['Dread'] = {
--	},

--[[
	*************************
	* Midcast: Divine Magic *
	*************************
--]]

	['rDivine_Magic_Skill'] = {
	},
	
--[[
	Divine Magic: damages or debilitates opponents with light elemental
	spells. It is especially effective against undead monsters, especially
	spells like banish whose properties are enhanced against undead.
	Divine Magic Skill determines accuracy and resuces spell interruption
	by the caster. It does not affect damage at all, except for enlight
	which sets a starting damage point.
	
	Divine Magic: banishes, banishga, enlight, flash and holy.
	
	Offensive Divine spells (banish, banishga, holy, and enlight) groups 
	spells that either just do damage or successive damage as in the case 
	of enlight.
	
	The banish spells accuracy, besides from divine magic spell, can be
	affected by magic accuracy from equipment. Damage resist rates depend
	on the difference in MND between caster and target. Banish does 50%
	more damage to undead.
	
	An elemental obi will be checked for as well as an elemental staff.
--]]

	['OffensiveDivine'] = {
		SUBSET  = {
			[1] = 'rDivine_Magic_Skill',
			[2] = 'rMND',
			[3] = 'rMAB',
		},
	},

--[[
	Enfeebling divine spell (flash) afflicts the target with accuracy 
	reduction (similar to blind) with a weakening effect over time till
	it runs out. Duration is subject to resists and partial resists
	although can last 12 seconds if not resisted. It also generates a
	significant amount of volitile and cumulative enmity.
--]]	
	
	['EnfeebleDivine'] = {
		SUBSET = 'rDivine_Magic_Skill',
	},

--[[
	Enlight is the only enhancing divine spell. It  enhances the paladin's 
	weapon with light starting at a fixed point based on their divine 
	magic skill. Each hit the value will go down 1 until 0 is hit. Multihit 
	weapons work with enlight. Enlight also provides +10 enmity. The base 
	damage starts at:
	
		Divine magic skill < 150: (divine magic skill /  30) + 10
		Divine magic skill >= 150: (divine magic skill / 15) + 5
--]]
		
	['EnhanceDivine'] = {
		SUBSET = 'rDivine_Magic_Skill',
	},

--[[
	*****************************
	* Midcast: Enfeebling Magic *
	****************************
--]]

	['rEnfeebling_Magic_Skill'] = {
		Neck = 'Enfeebling Torque',		-- +7 Enfeebling Magic Skill
	},
	
--[[
	Enfeebling Magic: this class of spells apply a debilitating status effect
	(debuff) to one or more targets. Enfeebling Magic Skill is used to determine
	the accuracy of enfeebling magic and to decrease the likelihood of a spell
	caster being interrupted when casting enfeebling magic.
	
	Enfeebling Spells: bind, blinds, blindgas, dias, diagas, dispel, gravity, 
	paralyzes, poisons, poisongas, sleeps, sleepgas, silence, and slows.
	
	There are two types of enfeebling spells, those dependent on INT (gravity,
	bind, blind, dispel, sleep, sleepga, poison, and poisonga) and those
	dependent on MND (paralyze, silence, slow, slowga, frazzlke, distract,
	dia, and diaga).
	
	After the appropriate gear set is equipped, an elemental obi might be
	equipped (for day/weather effect) and an elemental staff (for magic
	affinity.)
--]]
	
	['EnfeeblingINT'] = {
		SUBSET  = {
			[1] = 'rEnfeebling_Magic_Skill',
			[2] = 'rINT',
		},
	},

	['EnfeeblingMND'] = {
		SUBSET  = {
			[1] = 'rEnfeebling_Magic_Skill',
			[2] = 'rMND',
		},
	},

	['EnfeeblingMagic'] = {
		SUBSET = 'rEnfeebling_Magic_Skill',
	},
	
--[[
	********************
	* Midcast: Singing *
	********************
--]]

--[[
	Singing: is a general category only available to BRD (/BRD can do songs,
	but not equip instruments.) Unlike magic spells songs effectiveness is
	determined from a player's singing skill and instrument skill. (Wind and
	string instruments have different instrument skills.) A song's accuracy
	depends on CHR and the combined skill level (singing and instrument)
	multiplied by a scaling factor. Songs, once started, can not be interrupted.
	Songs either apply a buff to party members or debuff targets. Two active
	buffs can be applied to party members (assuming the bard has an instrument).
	
	Song types: carols, enfeebling, threnodies, recovery/misc, status enhancing,
	and status resistance.
--]]

	-- rSinging_Skill specifies gear that boosts Songs in general
	['rSinging_Skill'] = {	-- Covers both Singing Skill and Intrument Skill
	},
	
--[[
	EnhancementSinging contains gear that enhances party members is some specific
	manner. Included are: minne, minuet, paeon, pastoral, madigal, mambo, etude,
	operetta, ballad, march, prelude, aubade, carol, mazurka, gavotte, capriccio,
	fantasia, hymnus, and round.
--]]

	['EnhancementSinging'] = {
		SUBSET  = {
			[1] = 'rSinging_Skill',
			[2] = 'rCHR',
		},
	},

--[[
	EnfeeblingSinging contains gear that debuffs targets. Included are: requiem,
	threnody, lullaby, finale, elegy, and virelai.
--]]
	
	['EnfeeblingSinging'] = {
		SUBSET  = {
			[1] = 'rSinging_Skill',
			[2] = 'rCHR',
		},
	},

--[[
	********************
	* Midcast: Ninjusu *
	********************
--]]

	['rNinjutsu_Skill'] = {
		Ears = 'Stealth Earring',		-- +4 Ninjutsu Skill
	},
	
--[[
	Ninjutsu: this is a means for ninjas to cast magic-like abilities that
	use ninja tools instead of MP. Ninjutsu Skill affects spell interruption
	rate, potency, and magic accuracy of ninjutsu spells.
	
	There are three types of affects: buffs, debuffs, ane elemental-based
	damage spells. Buffs include: tonko, utsusemi, and monomi. Debuffs
	include: kurayami, hojo, dokumori, and jubaku. And elemental damage
	spells include: katon, suiton, raiton, doton, huton, and hyoton. 
--]]

	['NinjutsuBuff'] = {
		SUBSET = 'rNinjutsu_Skill',
	},

-- An elemental stave will be checked for after the debuff set is loaded.
	
	['NinjutsuDebuff'] = {
		SUBSET = 'rNinjutsu_Skill',
	},

--[[
	Ninjutsu Elemental spells not only damages the target but also lowers the
	target's resistance to the element that the ninjutsu's spell element is
	dominant to. (Ex, casting Hyoton deals ice damage and lowers resistance
	to fire damage.) Gear with Damage Enhancement should be included with this
	set. An elemental obi will be checked for and an elemental staff for magic
	affinity.
--]]
	
	['NinjutsuElemental'] = {
		SUBSET  = {
			[1] = 'rNinjutsu_Skill',
			[2] = 'rINT',
			[3] = 'rMAB',
		},
	},

--[[
	Blue Magic: Until the release of Treasures of Aht Urghan is close to a 
	release, there's no point in fleshing this out, especially since this job
	is being majorly altered.

	Geomancy Magic: Until the release of Seekers of Adoulin is close to a 
	reality, there's no point in fleshing this out.
--]]

--[[
	Weapon skills are driven specifically by one or more stats. In addition,
	attack power can be very advantageous. Listed below is an AttackPower
	set which is actually a subset to be included in each of the weapon
	skill gear sets. It will be used for default gear. Any additional gear
	will override slots from the AttackPower subset.
--]]

--[[
	***************
	* Blood Pacts *
	***************

	*** These two blood pact sets only need be defined if you are /SMN ***

	Specific to /SMN, Blood pacts go through a simulated process that mimics spell
	casting. The precast happens when the blood pact is invoked (either rage or ward),
	loading the 'PreBP' gear set. You want gear that has Blood Pact Ability Delay,
	Blood Pact Recast abilities, or Summoning Skill defined here.
	--]]

	['PreBP'] = {
	},

--[[
	Blood pacts are divided by type: physical, magical, summoning skill, accuracy,
	and hybrid. The 'MidBP' gear set encapsulates all those types through the
	use of groups.

	Listed below are the criteria for each BP type:
	SMN_BP_PHYS (Physical)
		Pet attack, pet accuracy, pet critical hit, and blood pact physical damage
	SMN_BP_MAG (Magical)
		Pet magic attack burst, pet magical attack, pet magical accuracy, and
		blood pact magical damage
	SMN_BP_SKILL (Skill)
		Summoning skill
	SMN_BP_ACC (Accuracy)
		Pet accuracy, pet magic accuracy
	SMN_BP_HYBRID (Hybrid)
		2x physical attacks and 1x magical, see SMN_BP_PHYS and SMN_BP_MAG for details
--]]

	['MidBP'] = {
		GROUP//SMN_BP_PHYS = {
		},
		GROUP//SMN_BP_MAG = {
		},
		GROUP//SMN_BP_SKILL = {
		},
		GROUP//SMN_BP_ACC = {
		},
		GROUP//SMN_BP_HYBRID = {
		},
	},

--[[

	****************
	* Weaponskills *
	****************

	The following weapon skill gearsets are defined by the stat(s) they emphasize. Listed are
	all of the sets that you will need to use every weapon skill that your job can do. The
	leading comment defines what weapon/weapon skill combination the set applies to.
	
	BST can use the following weapons: axe (A-), scythe (B-), dagger (C+), club(D), sword (E). 
	
	Please note that on HorizonXI you may have access to some weapon skills through your
	subjob. While not explicitly supported here, the appropriate weapon skill set will
	be loaded. If not listed below, you might have to create a custom weapon skill set to
	support	the skill. Remember, weapon skill sets are named WS_attr. If you name the set
	appropriately, that set will automatically be called when you use the weapon skill.
--]]
	
--[[
		* Strength based *
		
		Axe: Raging Axe,Smash Axe,Gale Axe,Avalanche Axe,Spinning Axe,Rampage,
			 Mistral Axe,Decimation,Onslaught
		Scythe: Slice,Spinning Scythe,Vorpal Scythe
		Club: Brainshaker,Skullbreaker,True Strike
		Sword: Flat Blade,Circle Blade,Vorpal Blade
-]]
	
	['WS_STR'] = {
		SUBSET = {
			[1] = 'rAttackPower',
			[2] = 'rSTR',
		},
	},
	
--[[
		* Strength and Dexterity based, even weighting *
		
		Sword: Fast Blade
--]]

	['WS_STRDEX'] = {
		SUBSET = 'rAttackPower',
        Head  = 'Empress Hairpin',							-- +3 DEX
        Neck  = { 'Justice Torque', 'Love Torque', 'Spike Necklace' },	-- +5 STR, +5 DEX, +3 STR/+3 DEX
        Body  = 'Narasimha\'s Vest',						-- +6 STR
        Hands = { 'Ogre Gloves', 'Wonder Mitts' },			-- +6/3 STR
        Rings = { 'Flame Ring', 'Kshama Ring No.8' },		-- +5/3 STR
        Waist = 'Warwolf Belt',								-- +5 STR/+5 DEX
		Back  = { 'Forager\'s Mantle', 'Amemet Mantle' },	-- +3 STR, +1 STR
        Legs  = { 'Monster Trousers', 'Wonder Braccae' },	-- +4 DEX, +1 STR
        Feet  = { 'Creek F Clomps', 'Bounding Boots' },		-- +4 STR, +3 DEX
    },

--[[
		* Strength and Intelligence based, even weighting *
		
		Scythe: Dark Harvest,Shadow of Death,Nightmare Scythe,Spiral Hell
		Sword: Burning Blade
--]]
	
	['WS_STRINT'] = {
		SUBSET = 'rAttackPower',
		Head   = 'Bst. Helm +1',							-- +8 INT
		Neck   = { 'Justice Torque', 'Spike necklace' },	-- +5/3 STR
		Body   = { 'Monster Jackcoat', 'Narasimha\'s vest', 'Wonder Kaftan' },	-- +6 INT, +6/1 STR
		Hands  = { 'Ogre gloves', 'Wonder Mitts' },			-- +6/3 STR
		Rings  = { 'Flame Ring', 'Tamas Ring' },			-- +5 STR, +5 INT
		Waist  = 'Warwolf Belt',							-- +5 STR
		Back   = { 'Forager\'s Mantle', 'Amemet Mantle' },	-- +3/1 STR
		Legs   = 'Wonder braccae',							-- +1 STR
		Feet   = { 'Creek F Clomps', 'Mannequin Pumps'},	-- +4 STR, +1 INT
	},

--[[
		* Strength and Intelligence based, 30%/20% respectively *
		
		Sword: Red Lotus Blade
--]]
	
	['WS_STRINT_30_20'] = {
		SUBSET = 'rAttackPower',
		Head   = 'Bst. Helm +1',							-- +8 INT
		Neck   = { 'Justice Torque', 'Spike necklace' },	-- +5/3 STR
		Body   = { 'Monster Jackcoat', 'Narasimha\'s vest', 'Wonder Kaftan' },	-- +6 INT, +6/1 STR
		Hands  = { 'Ogre gloves', 'Wonder Mitts' },			-- +6/3 STR
		Rings  = { 'Flame Ring', 'Tamas Ring' },			-- +5 STR, +5 INT
		Waist  = 'Warwolf Belt',							-- +5 STR
		Back   = { 'Forager\'s Mantle', 'Amemet Mantle' },	-- +3/1 STR
		Legs   = 'Wonder braccae',							-- +1 STR
		Feet   = { 'Creek F Clomps', 'Mannequin Pumps'},	-- +4 STR, +1 INT
	},

--[[
		* Strength and Mind based, even weighting *
		
		Scythe: Guillotine,Cross Reaper
		Club: Shining Strike,Seraph Strike,Judgement
		Sword: Shining Blade,Seraph Blade
--]]

	['WS_STRMND'] = {
		SUBSET = 'rAttackPower',
		Head  = 'Bst. Helm +1',								-- +8 MND
		Neck  = { 'Promise Badge', 'Justice badge' },		-- +5/3 MND
		Ears  = 'Geist Earring',							-- +1 MND
		Body  = { 'Narasimha\'s vest', 'Wonder Kaftan' },	-- +3/1 STR
		Hands = { 'Ogre gloves', 'Wonder Mitts' },			-- +6/3 STR
		Rings = { 'Flame Ring', 'Tamas Ring' },				-- +5 STR, +5 MND
		Waist = { 'Warwolf Belt', 'Friar\'s Rope' },		-- +5/1 STR
		Back  = { 'Forager\'s Mantle', 'Amemet Mantle' },	-- +3/1 STR
		Legs  = 'Wonder braccae',							-- +2 MND/+1 STR
		Feet  = { 'Creek F Clomps', 'Wonder Clomps' },		-- +4/2 STR
	},

--[[
		* Strength and Vitality based, even weighting *
		
		Axe: Calamity (32%/32%)
--]]
	
	['WS_STRVIT'] = {
		SUBSET = 'rAttackPower',
		Neck  = { 'Justice Torque', 'Spike necklace' },		-- +5/3 STR
		Ears  = 'Beastly Earring//AXE',						-- Boosted skill if using an axe
		Body  = { 'Narasimha\'s vest', 'Beast Jackcoat', 'Brigandine' },	-- +3 STR, +3 VIT, +2 VIT
		Hands = 'Ogre gloves',								-- +6 STR
		Rings = { 'Flame Ring', 'Kshama Ring No.8', 'Kshama Ring No.4' },	-- +5/3 STR, +3 VIT
		Waist = 'Warwolf Belt',								-- +5 STR/+5 VIT
		Back  = { 'Forager\'s Mantle', 'Amemet Mantle' },	-- +3/1 STR
		Legs  = 'Wonder braccae',							-- +2 VIT/1 STR
		Feet  = { 'Creek F clomps', 'Wonder Clomps' },		-- +4 STR/+4 VIT, +2 STR
	},

--[[
		* Dexterity based *
		
		Dagger: Wasp Sting,Viper Bite^

		^ Subjob must be one of: RDM,THF,BRD,RNG,NIN
--]]
	
	['WS_DEX'] = {
		SUBSET = {
			[1] = 'rAttackPower',
			[2] = 'rDEX',
		},
    },

--[[
		* Dexterity and Charisma based *
		
		Dagger: Eviseration
--]]

	['WS_DEXCHR'] = {
		SUBSET = 'rAttackPower',
		Head   = 'Empress Hairpin',							-- +3 DEX
		Neck   = { 'Love Torque', 'Spike Necklace' },		-- +5/3 DEX
		Ears   = 'Beastly Earring//AXE',					-- +2 CHR
		Body   = { 'Monster Jackcoat', 'Wonder Kaftan' },	-- +6 INT, +1 STR
		Hands  = 'Beast Gloves',							-- +3 DEX
		Rings  = { 'Kshama Ring No.2', 'Kshama Ring No.6' },-- +3 DEX, +3 CHR
		Waist  = 'Warwolf Belt',							-- +5 DEX
		Legs   = { 'Monster Trousers', 'Beast Trousers' },	-- +4 DEX, +4 CHR
		Feet   = 'Bounding Boots',							-- +2 DEX

		-- needs fleshing out
    },

--[[
		* Dexterity and Intelligence based *
		
		Dagger: Gust Slash,Cyclone^

		^ Subjob must be one of: RDM,THF,BRD,RNG,NIN
--]]
	
	['WS_DEXINT'] = {
		SUBSET = 'rAttackPower',
        Head   = { 'Bst. Helm +1', 'Empress Hairpin' },		-- +8 INT, +3 DEX
        Neck   = { 'Love Torque', 'Spike Necklace' },		-- +5/3 DEX
        Body   = { 'Monster Jackcoat', 'Brigandine' },		-- +6 INT
        Hands  = 'Beast Gloves',							-- +3 DEX
        Rings  = { 'Tamas Ring', 'Kshama Ring No.2' },		-- +5 INT, +3 DEX
		Legs   = 'Monster Trousers',						-- +4 DEX
        Feet   = 'Bounding Boots',							-- +3 DEX
    },

--[[
		* Charisma based *
		
		Dagger: Shadowstitch
--]]
	
	['WS_CHR'] = {
		SUBSET = {
			[1] = 'rAttackPower',
			[2] = 'rCHR',
		},
	},

--[[
		* Mind based *
		
		Dagger: Energy Steal, Energy Drain^
		
		^ Subjob must be RDM,THF,BRD,RNG, or NIN
--]]

	['WS_MND'] = {
		SUBSET = {
			[1] = 'rAttackPower',
			[2] = 'rMND',
		},
    },

--[[
		* Skill based *
		
		Club: Starlight,Moonlight

		Note: While club is the only skill-based weapon supported for BST, HorizonXI
		does support some basic weapon skills for other weapon types based on your
		subjob. So, including non-club skill pieces here isn't a bad idea, but make
		sure to use the	appropriate weapon type inline conditional.
--]]

	['WS_Skill'] = {
		SUBSET = 'rAttackPower',
		Neck   = { 'Justice Torque//SCYTHE', 'Justice Torque//GKATANA', 'Love Torque//DAGGER', 'Love Torque//POLEARM' },	-- +7 Scythe/G.Katana skill, +7 Dagger/Polearm skill
    },

--[[
		* HP based *
		
		Sword: Spirits Within
--]]

	['WS_HP'] = {
		SUBSET = 'rAttackPower',
		Ammo   = 'Happy Egg',											-- +1% HP
        Head   = { 'Monster Helm', 'Bst. Helm +1', 'Shep. Bonnet' },	-- +19/15/4 HP
		Neck   = 'Promise Badge',										-- +10 HP
        Ears   = { 'Physical Earring', 'Physical Earring', 'Ethereal Earring' }, -- Convert 25 MP to HP x2, 15 HP
        Body   = 'Wonder Kaftan',										-- +36 HP
        Hands  = { 'Monster Gloves', 'Wonder Mitts' }, 					-- +14/12 HP
        Rings  = { 'Bomb Queen Ring//CC1', 'Toreador\'s Ring', 'Toreador\'s Ring' }, -- +75/10/10 HP
        Waist  = 'Powerful Rope',										-- +20 HP
        Legs   = 'Wonder Braccae',										-- +21 HP
        Feet   = { 'Creek F Clomps', 'Wonder Clomps' },					-- +35/20 HP
    },

--[[
	Custom weaponskill sets can be used in place of the generic stats-based sets. You must name
	your custom set ['WS:skill'] where "skill" is the name of the weapon skill. If there's a blank
	in the name, substitue an underscore.

	Example: a custom set for "viper bite" would be named:	['WS:Viper_Bite'].

	Note: how you capitalize the name is up to you.
--]]

--[[
	The following are your main job (beastmaster) abilities. Unlike sub job abilities, this section
	will explicitly list all of your abilities. Please note that all abilities will be prefixed
	with an 'A:'. This is to ensure there's no conflict with any other predefined gear set (this
	is a bigger issue with subjob abilities than with main jobs.)
--]]
	
	['A_Familiar'] = {
	},

	-- CHR and Charm + gear. (Every +1 Charm adds 5% Charm duration)
	['A_Charm'] = {
		SUBSET = 'rCHR',
		Main//WSWAP  = 'Pluto\'s Staff',					-- +2 CHR
		Head  = { 'Monster Helm', 'Entrancing Ribbon' },	-- +4 CHR/+5 Charm, +2 CHR
		Body  = { 'Monster Jackcoat', 'Beast Jackcoat' },	-- +6/5 Charm
		Hands = { 'Monster Gloves', 'Beast Gloves' },		-- +4/3 Charm
		Legs  = { 'Beast Trousers', 'Monster Trousers' },	-- +6/2 Charm
		Feet  = { 'Beast Gaiters', 'Monster Gaiters' },		-- +3 CHR/+2 Charm, +3 Charm
	},

	['A_Gauge'] = {
		SUBSET = 'rCHR',
	},

	-- Reward potency, reward augment, reward enhancement, and MND gear
	['A_Reward'] = {
		SUBSET = 'rMND',
		Main//WSWAP = 'Neptune\'s Staff',				-- +5 MND
		Body = { 'Monster Jackcoat//DB:WSS', 'Beast Jackcoat//DB:BPP', 'Monster Jackcoat','Beast Jackcoat' },	-- Augments "Reward II"/remove WSS, Augments "Reward"/remove BPP, Augments "Reward II", Augments "Reward",
		Hands = 'Ogre Gloves',							-- Enhances Reward
		Feet = { 'Monster Gaiters', 'Beast Gaiters' },	-- Enhances "Reward II", Enhances "Reward"
	},

	['A_Call_Beast'] = {
		Hands = 'Monster Gloves',		-- Augments "Call Beast"
	},
	
	-- Tame success rate. Resistence depends on your INT vs target's INT
	['A_Tame'] = {
		SUBSET = 'rINT',
		Main//WSWAP = 'Aquilo\'s Staff',				-- +5 INT
		Head  = 'Bst. Helm +1',							-- +8 INT/increase "Tame" success
    },
	
	--* /THF *--
	-- if only Sneak Attack is enabled, the following will be equipped
	['A_Sneak_Attack'] = {
		SUBSET = 'rDEX',
	},

	-- If only Trick Attack is enabled, the following will be equipped
	['A_Trick_Attack'] = {
		SUBSET = 'rAGI',
	},

	-- When both Sneak Attack and Trick Attack are enabled, the following will be equipped
	['A_SATA'] = {
		Head  = 'Empress Hairpin',							-- +3 DEX/+3 AGI
		Neck  = { 'Justice Torque', 'Spike Necklace', 'Opo-opo Necklace' },	-- +5/3/3 DEX
		Ears  = { 'Genin Earring//SJNIN', 'Drone Earring' },	-- +4 AGI if sj NIN, +3 AGI
		Rings = { 'Kshama Ring No.2', 'Kshama Ring No.3' },	-- +3 DEX, +3 AGI
	},

--[[
	Pet commands can also be made into a gear set. Unlike abilities with an 'A:' prefix,
	pet commands use the 'PC:' prefix. By default the pet commands  most likely to
	have gear associated with then ()(except for blood pacts) are predefined for: BST,
	DRG and SMN.

	Note: commands like BST's SIC and READY and SMN's Blood Pact actually are identified
	by the skill they invoke. This means that the type of skill is what is processed
	and not the actual command. Defined in utilities.lua are the BSTs skill according to
	type. Blood pacts are handled separately by PreBP and MidBP, so not included here.
--]]

	['PC_Fight'] = {
	},

	['PC_Sic_Ready'] = {
		GROUP//BST_PET_ATTACK = {
		},
		GROUP//BST_PET_MATT = {
		},
		GROUP//BST_PET_MACC = {
			Head = 'Shep. Bonnet',		-- +3 Pet Macc
		},
	},

	--* /SMN *--

	['PC_Assault'] = {
	},

--[[
	If you want to create any custom gear sets, those you'd use with the /gs command, include
	the gear set definitions here. (There's no naming convention, call them what you want, but
	try to avoid any set names defined above.)
--]]

	-- MP150 equips gear to add at least 150 to max MP (for doing a raise)
	-- Current list adds +134 MP. Maybe at a higher level I'll have the gear.
	-- (Note: the MP from the Energy Earring is ignored since that's a standard
	-- gear piece when I am subbing a magic using subjob.)

	['MP150'] = {
		Head  = { 'Smn. Horn +1', 'Reraise Hairpin' },		-- +30/21 MP
		Neck  = { 'Fenrir\'s Torque//DAYTIME', 'Uggalepih Pendant', 'Star Necklace' },	-- 30 MP if daytime, +20 MP, Converts 15 HP to MP
		Ears  = { 'Loquac. Earring', 'Physical Earring', 'Geist Earring', 'Energy Earring +1', 'Energy Earring +1' },	-- +30, converts 25 HP to MP,5 MP, +4/4 MP
		Rings = { 'Ether Ring', 'Tamas Ring' },				-- Convert 40 HP to MP, +30 MP
		Back  = { 'Blue Cape', 'Fed. Army Mantle' },		-- +15 MP/Convert 15 HP to MP, +6 MP
		Waist = { 'Hierarch Belt', 'Powerful Rope' },		-- +48/20 MP
		Legs  = { 'Evk. Spats +1', 'Summoner\'s Spats' },	-- +22/20 MP
		Feet  = { 'Rostrum Pumps', 'Evk. Pigaches +1', 'Mannequin Pumps'},	-- +30/25/12 MP
	},
};

--[[
	************
	* Settings *
	************

	This is where the player specifies details about their job that has nothing to do with
	gear sets. The player defines the weapons they use, which macro book should be equipped,
	and various priorities.
--]]

profile.Sets = sets;

-- list of all jug pets available on HorizonXI. Indicate all jugs you favor to be used with ajug.
profile.FavoredJugPets = {
	['Hare Familiar']		= { ['broth'] = 'carrot broth',		['favored'] = false },
	['Sheep Familiar']    	= { ['broth'] = 'herbal broth', 	['favored'] = false },
	['Flowerpot Bill']    	= { ['broth'] = 'humus',		 	['favored'] = false },
	['Tiger Familiar']    	= { ['broth'] = 'meat broth', 		['favored'] = false },
	['Flytrap Familiar']    = { ['broth'] = 'grass. broth', 	['favored'] = false },
	['Lizard Familiar']    	= { ['broth'] = 'carrion broth', 	['favored'] = false },
	['Mayfly Familiar']    	= { ['broth'] = 'bug broth', 		['favored'] = false },
	['Eft Familiar']    	= { ['broth'] = 'mole broth', 		['favored'] = false },
	['Beetle Familiar']    	= { ['broth'] = 'tree sap', 		['favored'] = false },
	['Antlion Familiar']    = { ['broth'] = 'antica broth', 	['favored'] = false },
	['Crab Familiar']    	= { ['broth'] = 'fish broth', 		['favored'] = false },
	['Mite Familiar']    	= { ['broth'] = 'blood broth', 		['favored'] = false },
	['Keeneared Steffi']    = { ['broth'] = 'f. carrot broth', 	['favored'] = false },
	['Lullaby Melodia']    	= { ['broth'] = 's. herbal broth', 	['favored'] = false },
	['Flowerpot Ben']    	= { ['broth'] = 'rich humus', 		['favored'] = false },
	['Saber Siravard']    	= { ['broth'] = 'w. meat broth', 	['favored'] = false },
	['Funguar Familiar']    = { ['broth'] = 'seedbed soil', 	['favored'] = false },
	['Shellbuster Orob']    = { ['broth'] = 'qdv. bug broth', 	['favored'] = false },
	['Coldblood Como']    	= { ['broth'] = 'c. carrion broth', ['favored'] = false },
	['Courier Carrie']   	= { ['broth'] = 'fish oil broth', 	['favored'] = false },
	['Homunculus']    		= { ['broth'] = 'alchemist water', 	['favored'] = false },
	['Voracious Audrey']    = { ['broth'] = 'n. grass. broth', 	['favored'] = false },
	['Ambusher Allie']    	= { ['broth'] = 'l. mole broth', 	['favored'] = false },
	['Panzer Galahad']    	= { ['broth'] = 'scarlet sap', 		['favored'] = false },
	['Lifedrinker Lars']    = { ['broth'] = 'c. blood broth', 	['favored'] = false },
	['Chopsuey Chucky']    	= { ['broth'] = 'f. antica', 		['favored'] = false },
	['Amigo Sabotender']    = { ['broth'] = 'sun water', 		['favored'] = false }
};

-- The following structure stores job related settings/variables. The first section is automatically
-- populated by Luashitacast. The second section contains settings the player can modify.
profile.settings = {
	-- This first section is controlled by Luashitacast. Please do not modify any entries here
	sjb = nil;							-- Tracks subjob name
	sPetAction = nil;					-- What was the last action by your avatar
	PlayerCappedLevel = 0;				-- Indicates gear capped level. 0 defaults to current level
	bAmmo = false;						-- /BST specific. Is ammo equipped?
	sAmmo = nil;						-- /BST specific. Name of ammo equipped
	-- Trackers for the regen and refresh caps
	bCappedRefresh = false;				-- Disables resting refresh gear equip if true
	bCappedRegen = false;				-- Disables resting regen gear equip if true
	--*********************************************************************
	-- From this point forward, all entries can be modified by the player *
	--*********************************************************************
	defaultSpirit = 'Light Spirit',		-- for /911, what default spirit should be used
	defaultPetFood = nil;				-- What (if any) pet food to use when Reward processed
	-- Order of operations:
	-- After TP gearset processed, three supplimental gearsets might be also run: evasion,
	-- accuracy, and TH. postGSEngaged indicates the order to process the first two. It is a
	-- replacement for priorityEngaged. The TH gearset will always be run last.
	postGSEngaged = { [1] = 'Eva', [2] = 'Acc' };
	-- After the weaponskill gearset is loaded, three supplimental gearsets might also be run:
	-- accuracy, elemental gorget, and elemental obi. (The latter is unimplemented for now.)
	postGSWeaponSkill = { [1] = 'Acc', [2] = 'eGorget', [3] = 'eObi' };
	-- Priority settings define process of supplimental orders after gear set processing
	bPriorityRefresh = false;			-- priority setting. If true, Refresh over Regen. False inverts
	bLockAllCraftGather = true;			-- Lock all slots when crafting or gathering?
	-- Override settings are used to indicate the order sets are processed. It's recommended to leave these
	-- entries false.
	EmbedOnlyAccuracy = false;			-- Restricts accuracy to only inline conditionals if true
	EmbedOnlyEvasion = false;			-- Restricts evasion to only inline conditionals if true
	EmbedOnlyMacc = false;				-- Restricts Macc to only inline conditionals if true
	EmbedOnlyTH = false;				-- Restricts TH to only inline conditionals if true
	EmbedOnlyeGorget = false;			-- Restricts elemental gorgets to only inline conditionals if true
	EmbedOnlyeObi = false;				-- Restricts elemental obis to only inline conditionals if true
	-- Macro book/page
	bAutoMacrobook_page = true;			-- Should macro book/page be automatically assigned
	bJustMacroBook = false;				-- Should only the macro book be automatically assigned
	MacroBook = 10;						-- Which macro book should be equipped for BST
};

-- Table of custom conditionals
profile.CustomConditionals = {
	[1] = [ ['code'] = 'CC1', ['question'] = 'Is minus fire resistance an issue', ['init'] = false },
	[2] = [ ['code'] = 'CC2', ['question'] = 'Should optional gear be included', ['init'] = false },
};

--[[
	********
	* Code *
	********

	SetSubjobSet is used to pick the appropriate set for the loaded macrobook based on
	which subjob is current. 
--]]

function SetSubjobSet(chkSJ)
	-- "chkSJ" is the key for what toolbar is shown. All jobs are defined in the subs table.
	-- A value of 0 means that job is not configured. All values > 0 indicate which toolbar
	-- is to be displayed. The player must change the entries in this table to match their
	-- needs.
	local tSubs = {
		['WAR'] = 0, ['MNK'] = 0, ['WHM'] = 1, ['BLM'] = 3, ['RDM'] = 0, ['THF'] = 2,
		['PLD'] = 0, ['DRK'] = 0, ['BST'] = 0, ['BRD'] = 0, ['RNG'] = 0, ['SMN'] = 0,
		['SAM'] = 0, ['NIN'] = 4, ['DRG'] = 0, ['BLU'] = 0, ['COR'] = 0, ['PUP'] = 0,
		['DNC'] = 0, ['SCH'] = 0, ['GEO'] = 0, ['RUN'] = 0};
	local sj = 1;	-- Default toolbar

	if chkSJ == nil or chkSJ == 'NON' or
		(profile.settings.sjb ~= nil and profile.settings.sjb == chkSJ) or
		profile.settings.bAutoMacrobook_page == false then
		return;
	end
	
	-- Compare the stored subjob with the current subjob
	if profile.sjb == nil or chkSJ ~= profile.sjb then	
		if tSubs[chkSJ] > 0 then
			sj = tSubs[chkSJ];
		end
	end

	AshitaCore:GetChatManager():QueueCommand(1, '/macro set '..tostring(sj));
	profile.settings.sjb = chkSJ;
end		-- SetSubjobSet

--[[
	OnLoad is run whenever you log into your BST or change your job to BST
--]]

function profile.OnLoad()
	local player = gData.GetPlayer();

	gSettings.AllowAddSet = true;
	utilities.Initialize();

	-- Set which macro book should be displayed. Which macro set within the macro book to
	-- display depends on what your subjob is.
	if profile.settings.bAutoMacrobook_page == true then
		AshitaCore:GetChatManager():QueueCommand(1, '/macro book ' .. tostring(profile.settings.MacroBook));		-- BST macro book
		if profile.settings.bJustMacroBook == false then
			SetSubjobSet(player.SubJob);
		end
	end
	
	-- Load up the weapons bar. (This need only be done once.)
	gear.MoveToDynamicGS(profile.Sets.Start_Weapons,crossjobs.Sets.CurrentGear,false,'Start_Weapons');
	gear.EquipTheGear(crossjobs.sets.CurrentGear,false);

	-- Now define the toggles for any custom conditionals.
	for _,j in ipairs(profile.CustomConditionals) do
		utilities.SetToggle(string.upper(j['code']),j['init']);
	end

	-- Record the favored jug pets
	pets.FavoredJugPets();

	-- Make sure the saved weapons are the starting weapons
	gear.weapon = crossjobs.Sets.CurrentGear['Main'];
	gear.offhand = crossjobs.Sets.CurrentGear['Sub'];
end		-- OnLoad

--[[
	OnUnload is run when you change to another job
--]]

function profile.OnUnload()
	utilities.Unload();
end		-- OnUnload

--[[
	HandleCommand is run when you type in a command defined in LUASHITACAST. The commands 
	handled here instead of in crossjobs.HandleCommands are specific to BST or the help
	system.
--]]

function profile.HandleCommand(args)

	if args[1] == 'man' then
		help.ShowHelp();
	elseif (args[1] == 'ajug') then			-- Turns on/off whether Automated Jug Pets supported
		utilities.AdvanceToggle('AJug');
	elseif args[1] == 'petfood' then
		pets.fPetReward(args[2],true);
	else
		crossjobs.HandleCommands(args);
	end
end		-- HandleCommand

--[[
	HandlePetAction equips the appropriate gear set based on the type of action
	the pet is trying to perform. Please note that only BST pets are supported,
	not SMN avatars.
--]]

function HandlePetAction(PetAction)
	local pet = gData.GetPet();

	if PetAction == nil or PetAction.Name == nil then
		return;
	end

	-- Only gear swap if this flag is true
	if utilities.fGetToggle('GSwap') == false then
		return;
	end

	-- Finish with the generalized version of the function
	pets.HandlePetAction(PetAction);
end		-- HandlePetAction

--[[
	HandleDefault is run when some action happens. This includes both actions by the player and by
	their pet.
--]]
	
function profile.HandleDefault()
	local pet = gData.GetPet();
	local petAction = gData.GetPetAction();
	local player = gData.GetPlayer();
	local zone = gData.GetEnvironment();	
	local ew = gData.GetEquipment();
	local bSA = utilities.fBuffed('Sneak Attack');
	local bTA = utilities.fBuffed('Trick Attack');
	local bMSJ = utilities.fMagicalSubJob();
	local bWSWAP = utilities.GetToggle('WSWAP');
	local bIgnoreLocks;
	local eWeap = nil;
	local cKey;

	utilities.Reminder();		-- See if reminder should be printed
	
	-- Make sure the macro set is shown and that the display on the top of the screen is correct
	-- in case the subjob was changed.
	SetSubjobSet(player.SubJob);
	displaybar.UpdateBarStatic();

	-- No gear swapping should occure if GSwap is false or /gc has not been run
	if utilities.fGetToggle('GSwap') == false or gear.fHasGCBeenRun() == false then
		return;
	end

	-- A pet action takes priority over a player's action
	if pet ~= nil and pet.Name ~= nil and petAction ~= nil then
		HandlePetAction(petAction);
		return;
	end
	
	-- Save the name of the main weapon	
	if ew['Main'] ~= nil then
		eWeap = ew['Main'].Name;
	end;
	
	-- If a jug pet or pet food was dynamically equipped prior to this
	-- pass-through on the function, the following code will set the
	-- item back that was in the Ammo slot prior to it dynamically
	-- changing.
	if profile.bAmmo == true and locks.fIsSlotLocked('ammo') == false then
		gFunc.ForceEquip('Ammo',profile.settings.sAmmo);
		profile.sAmmo = nil;
		profile.bAmmo = false;
	end

	-- Clear out the CurrentGear in case of leftovers
	utilities.ClearSet(crossjobs.Sets.CurrentGear);

	-- If player is not resting and has swapped weapons, set the weapon
	-- back to what they had before the switch
	if player.Status ~= 'Resting' and 
	   gear.weapon ~= nil and
	   bWSWAP == true and
	   eWeap ~= gear.weapon then
		if locks.fIsSlotLocked('main') == false then
			crossjobs.Sets.CurrentGear['Main'] = gear.weapon;
		end
		if locks.fIsSlotLocked('sub') == false then
			crossjobs.Sets.CurrentGear['Sub'] = gear.offhand;
		end
	end
				
	-- Now process the player status accordingly
	if (player.Status == 'Engaged') or (pet ~= nil and pet.Status == 'Engaged') then
		profile.settings.bCappedRefresh = false;
		profile.settings.bCappedRegen = false;

		if bSA == true and bTA == true then
			gear.MoveToDynamicGS(profile.Sets.SATA,crossjobs.Sets.CurrentGear,false,'SATA');
		elseif bSA == true then					-- SA
			gear.MoveToDynamicGS(profile.Sets.SneakAttack,crossjobs.Sets.CurrentGear,false,'SA');
		elseif bTA == true then					-- TA
			gear.MoveToDynamicGS(profile.Sets.TrickAttack,crossjobs.Sets.CurrentGear,false,'TA');
		else
			gear.MoveToDynamicGS(profile.Sets.TP,crossjobs.Sets.CurrentGear,false,'TP');
			for _,j in ipairs(profile.settings.postGSEngaged) do
				j = string.lower(j);
				if j == 'eva' and utilities.fGetToggle('Eva') == true and profile.settings.EmbedOnlyEvasion == false then
					gear.MoveToDynamicGS(profile.Sets.Evasion,crossjobs.Sets.CurrentGear,false,'Evasion');
				elseif j == 'acc' and profile.settings.EmbedOnlyAccuracy == false then
					crossjobs.ProgressiveAccuracy('Acc');
				end
			end
		end
	elseif player.Status == 'Resting' then
		local bRefresh = false;
		if profile.settings.bCappedRefresh == false and player.MP >= player.MaxMP then
			profile.settings.bCappedRefresh = true;
		end
		if profile.settings.bCappedRegen == false and player.HP >= player.MaxHP then
			profile.settings.bCappedRegen = true;
		end

		-- Player kneeling. Based on priority, order regen and refresh
		if bMSJ == true and profile.settings.bPriorityRefresh == true then
			if profile.settings.bCappedRefresh == false then
				gear.MoveToDynamicGS(profile.Sets.Resting_Refresh,crossjobs.Sets.CurrentGear,false,'Resting_Refresh');
				bRefresh = true;
			elseif profile.settings.bCappedRegen == false then
				gear.MoveToDynamicGS(profile.Sets.Resting_Regen,crossjobs.Sets.CurrentGear,false,'Resting_Regen');
			end
		else
			if profile.settings.bCappedRegen == false then
				gear.MoveToDynamicGS(profile.Sets.Resting_Regen,crossjobs.Sets.CurrentGear,false,'Resting_Regen');
			elseif bMSJ == true and profile.settings.bCappedRefresh == false then
				gear.MoveToDynamicGS(profile.Sets.Resting_Refresh,crossjobs.Sets.CurrentGear),false,'Resting_Refresh';
				bRefresh = true;
			end
		end

		-- Add a dark/pluto's staff if refresh wanted and weapon swapping indicated
		if bRefresh == true and bMSJ == true and and bWSWAP == true then
			local sStave = utilities.fCheckForEleGear('staff','dark');
			if sStave ~= nil then
				gear.fSwapToStave(sStave,false,crossjobs.Sets.CurrentGear);
			end
		end
	else
		-- Assume player idling
		profile.settings.bCappedRefresh = false;
		profile.settings.bCappedRegen = false;

		gear.MoveToDynamicGS(profile.Sets.Default,crossjobs.Sets.CurrentGear,false,'Default');
	end
			
	-- In case there is a summoned pet...
	if pets.fSummonerPet() == true and bWSWAP == true then
		local sStave = gear.fCheckForElementalGearByValue('staff','Summons',pet.Name);
		if sStave ~= nil then
			gear.fSwapToStave(sStave,false,crossjobs.Sets.CurrentGear);
		end
	end

	-- Equip the composited HandleDefault set
	gear.EquipTheGear(crossjobs.Sets.CurrentGear,bIgnoreLocks);

	-- And make sure a weapon equipped.
	local tgear = gData.GetEquipment();
	if tgear.Main == nil or tgear.Main.Name == nil then
		gear.MoveToDynamicGS(profile.Sets.Start_Weapons,crossjobs.Sets.CurrentGear,true,'Start_Weapons');
	end

	-- Equip the gear
	gear.EquipTheGear(crossjobs.Sets.CurrentGear,bIgnoreLocks);

	-- Lastly, update the display, just in case
	displaybar.UpdateBarStatic();
end		-- HandleDefault

--[[
	HandleAbility is used to change the player's gear appropriately for the specified pet ability.
--]]

function profile.HandleAbility()
	local ability = gData.GetAction();
	local sj = player.SubJob;

	-- Make sure the data download is done
	if sj == nil or sj == 'NON' or ability.Name == nil then
		return;
	end

	-- Only process if /gswap is turned on
	if utilities.fGetToggle('GSwap') == false then
		return;
	end

	crossjobs.HandleAbility();
end		-- HandleAbility

--[[
	HandleItem is the place to equip gear when a special item is used. Currently only 'Holy Water' 
	is supported
--]]

function profile.HandleItem()

	-- Only process if /gswap is turned on
	if utilities.fGetToggle('GSwap') == false then
		return;
	end

	crossjobs.HandleItem();
end		-- HandleItem

--[[
	HandlePrecast is invoked when the player casts a spell. It is the first step of two where you load any
	Fast Cast, cast time reduction, and quick cast gear in anticipation of a spell
--]]

function profile.HandlePrecast()
	
	-- Only process if /gswap is turned on
	if utilities.fGetToggle('GSwap') == false then
		return;
	end

	magic.HandlePrecast();
end		-- HandlePrecast

--[[
	HandleMidcast is the second function invoked when a player casts a spell. It equips gear appropriate for 
	magic skill, duration, magic attack bonus, magic accuracy, and potency. There's an order to how the pieces 
	are loaded: INT/MND, spell specific, macc, magic skill, obi, ele swap	
--]]

function profile.HandleMidcast()

	-- Only process if /gswap is turned on
	if utilities.fGetToggle('GSwap') == false then		-- Only gear swap if this flag is true
		return;
	end

	-- Call the common HandleMidcast now
	magic.HandleMidcast();
end		-- utilities.HandleMidcast

--[[
	HandlePreshot is similar to HandlePrecast, but for ranged actions. It loads Ranged Accuracy 
	and Ranged Shot Speed Gear for a ranged attack
--]]

function profile.HandlePreshot()

	-- Only process if /gswap is turned on
	if utilities.fGetToggle('GSwap') == false then
		return;
	end

	crossjobs.HandlePreshot();
end		-- HandlePreshot

--[[
	HandleMidshot is similar to HandleMidcast, but for ranged actions. It loads Ranged Attack 
	and Damage gear for a ranged attack
--]]

function profile.HandleMidshot()

	-- Only gear swap if this flag is true
	if utilities.fGetToggle('GSwap') == false then
		return;
	end

	crossjobs.HandleMidshot();
end		-- HandleMidshot

--[[
	HandleWeaponskill loads the gear appropriately for the weapon skill you're doing
--]]

function profile.HandleWeaponskill()

	if utilities.fGetToggle('GSwap') == false then
		return;
	end

	crossjobs.HandleWeaponskill();
end		-- HandleWeaponskill

return profile;
