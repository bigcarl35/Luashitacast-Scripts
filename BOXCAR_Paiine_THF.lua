local profile = {};
gcinclude = gFunc.LoadFile('common\\gcinclude.lua');

--[[
	This file contains all the gear sets associated with the THF job.
	
	Gear Sets last updated: September 6, 2025
	Code update: April 23, 2025
	
	Role: end game
--]]

local sets = {
--[[
	The gear sets are self contained, a mixture of direct gear assignments and conditional
	assignments. Before gear swapping can occur, you must run /gc so the system can learn
	all the gear from your gear sets. Each set contains entries identified by the gear slot.
	If it's a single value, it's a direct assignment like: Body = 'Austere Robe', but there
	can be multiple	items identifying a priority order, usually ordered by level:
			Body = { 'Vermillion Cloak//PETNAME:Carbuncle',	'Austere Robe' },
	Any item that has a // appended to it contains an inline conditional. The // code is a
	test to see if the item should be equipped. Even if normal checks pass (job, level,
	accessibility, etc), if the attached conditional(s) evaluate to false, the piece of gear
	will not be equipped.
	
	Not all sets need to be defined. There is nothing wrong with leaving a set "empty", but don't
	delete any of the existing sets. All the ones listed here (except for any custom sets) are
	expected to exist by Luashitacast.

	You'll find there are two types of sets defined in this file: Gear Sets and Reference
	Sets. Both look very similar and contain gear listings, but are treated in different ways.
	Gear Sets are what Luashitacast equip based on actions the code tracks. So things like
	are you fighting, casting a spell, resting, etc. Reference sets will never be directly
	equipped by Luashitacast except as subsets found in Gear Sets. For example, ['Enmity_plus']
	is a Reference set since Luashitacast will not load it directly whereas ['TP'] is a Gear
	Set that is equipped when the player is "engaged". Now, if you are evasion tanking, you
	might what to equip enmity+ gear when you're engaged, but Luashitacast will not address
	it directly. You must include it in a gear set via the Subset commad.

	Example:

	['TP'] = {
		GROUP//NOT_TANK = {
			Head  = { 'Homam Zucchetto', 'Rogue\'s Bonnet', 'Empress Hairpin' },
			Hands = { 'Homam Manopolas', 'Dusk Gloves' },
			Waist = { 'Swift Belt', 'Mrc.Cpt. Belt' },
		},
		GROUP//TANK = {
			Subset = 'enmity_plus',
			GROUP//SJNIN = {
				Subset = 'Evasion',
				Neck  = 'Peacock Amulet',
				Ears  = { 'Ethereal Earring', 'Stealth Earring//SJNIN', 'Brutal Earring' },
				Body  = { 'Dragon Harness//DT_BREATH', 'Assassin\'s Vest', 'Scorpion Harness', 'Brigandine' },
				Hands = { 'Homam Manopolas', 'Assassin\'s Armlets' },
				Rings = { 'Kshama Ring No.4', 'Bomb Queen Ring', 'Kshama Ring No.3' },
			},
			GROUP//SJPLD = {
				Head = 'Optical Hat';
				-- More Tanky definition goes here
			},
			GROUP//NOT_SJNIN//NOT_SJPLD = {
				-- Definition for nonsub NIN or PLD goes here
			}
		}
	}

	When looking at a Gear Set definition it's important to remember that all Subsets will be
	processed first (unless found in a Group) before the rest of the definition is processed.
	This means that whether the Subset is at the top of the Gear Set or the bottom makes no
	difference. The caveat about "groups" are an exception. By it's very definition a group
	is a self-contained Gear Set inside of another Gear Set. All elements in a Group are
	processed like you're loading a Gear Set so that subsets identified in a Group will be
	processed first, it that group.

	If you're going to have multiple groups in the same gear set, it's important that the
	different group definitions that contain the same gear slots do not overlap. //TANK and
	//NOT_TANK are mutually exclusive, one or the other will be equipped, but if you have
	GROUP//TANK and GROUP//NIGHTTIME, it's possible that neither will be equipped or both will
	be equipped. Since you can't guarentee which will be processed first, it's highly doubtful
	that what you expect to happen actually	will happen. Now, if your groups contain different
	slots then this is not a problem since you'll not have overlap. Just be conscious of this
	issue when you're defining you sets.
		
	*** Note ***
	/SMN has a problem in that their pet is the level of the subjob, which is not very useful. 
	As a result, /SMN pet actions are treated "as is" without gearset swap support.	As for /DRG,
	please note that the wyvern can't be summoned.
	
	*** Note 2 ***
	No gear that supports bard songs can be worn by any job except a bard, so there's no explicit
	support given here for /BRD.
		
	Horizon changes from retail:
		- Minor changes to artifact armor
		- Major changes to Mandau
		- Treasure Hunter has been removed from Thieve's Knife and moved to the custom neck piece
		  Nanaa's Charm
		- The player's accuracy and ranged accuracy when using "perfect dodge" are increased by 999
		- Many elemental weapon skills have been changed to the hybrid type and can now stack with
		  "sneak attack"
		- Triple attack now unlocks at 30 instead of 55
		- Assassin now unlocks at 50 instead of 60
		- "Accomplice" (out of era) can now be used on an alliance, has had it's level reduced from 
		  65 to 45 and no longer shares a recast timer with "Collaborator"
		- "Collaborator" (out of era) can now be used on an alliance, has had it's level reduced
		  from 65 to 45 and it's recast time increased from 1 minute to 5 minutes. It no longer shares
		  a recast timer with "Accomplice" and it now redirects 50% of the thief's enmity to their
		  chosen target. Note that the chat log erroneously notes you stole the enmity.
		- "Bully" (our of era) has had it's level reduced from 93 to 40, it's recast been lowered from
		  3 to 1 minute. The ability has been completely reworked now giving a guarenteed status bolt
		  additional effect proc on monsters within 15 levels of the thief. The chance of proc'ing
		  a status effect on a notorious monster has been increased, but is not guarenteed. The
		  omni-directional sneak attack portion of the skill has been removed
		- "Mug" grants a critical hit rate bonus of 5% on the target for 30 seconds. Recast was lowered
		  from 15 to 10 minutes. This activates as a bewildered daze regardless of the success of the
		  mug. It wears off after 30 seconds
--]]

--[[
	The "default" gear set is what is worn when you're not fighting (neither you nor a pet)
	and you're not resting. It covers everything else: idling, traveling, in town, etc. This 
	set displays what your character looks like most of the	time. This set does not 
	distinguish the type of activities you're doing by default, so use inlines accordingly.

	Note: The inclusion of the non-group entries will not cause an issue since the slots
	are not found in either group, so order of process doesn't matter. The non-group section
	could be made into a reference set and included via 'subset', but since it's only used
	in the 'default' gear set, I saw no reason to do so.
--]]

	['Default'] = {
		Neck  = { 'Opo-opo necklace//SLEPT', 'Peacock Amulet', 'Spike Necklace' },
		Ears  = { 'Bat Earring//BLINDED', 'Coral Earring//DT_MAGICAL', 'Brutal Earring', 'Stealth Earring//SJNIN', 'Ethereal Earring',
				  'Coral Earring', 'Fang Earring', 'Energy Earring +1//MSJ', 'Energy Earring +1//MSJ' },
		Hands = { 'Homam Manopolas', 'Rogue\'s Armlets', 'Assassin\'s Armlets', 'Battle Gloves' },
		Legs  = { 'Homam Cosciales', 'Rogue\'s Culottes', 'Wonder Braccae' },
		Feet  = { 'Homam Gambieras', 'Assassin\'s Pouln.', 'Creek F Clomps', 'Wonder Clomps', 'Bounding Boots' },
		Group//NOT_TANK = {
			Head  = { 'Lilac Corsage//TOWN', 'President. Hairpin//NOT_OWN//HPP.LT.94', 'Homam Zucchetto', 'Rogue\'s Bonnet', 'Empress Hairpin' },
			Body  = { 'Ducal Aketon//TOWN-AK', 'Dragon Harness//DT_BREATH', 'Rapparee Harness', 'Assassin\'s Vest', 'Brigandine', 'Angler\'s Tunica' },
			Rings = { 'Flame Ring', 'Kshama Ring No.8', 'Kshama Ring No.2', 'Kshama Ring No.3' },
			Back  = { 'Forager\'s Mantle', 'Amemet Mantle', 'Raptor Mantle' },
			Waist = { 'Warwolf Belt', 'Swift Belt', 'Mrc.Cpt. Belt' },
		},
		Group//TANK = {
			Head  = { 'Asn. Bonnet +1', 'Assassin\'s Bonnet', 'Rogue\'s Bonnet', 'Empress Hairpin' },
			Body  = { 'Dragon Harness//DT_BREATH', 'Assassin\'s Vest', 'Brigandine', 'Wonder Kaftan' },
			Rings = { 'Kshama Ring No.4', 'Flame Ring', 'Kshama Ring No.8', 'Kshama Ring No.2', 'Kshama Ring No.3' },
			Waist = { 'Warwolf Belt', 'Life Belt'  },
		},
	},
	
--[[
	The TP set is used when you are fighting. The accuracy set will be applied in a progressively
	manner (slot by slot) as opposed to the the evasion set (if Eva is enabled of the display bar)
	which will overlap the TP set. Tanking for a THF focuses on evasion tanking. Note: whether
	Group//NOT_TANK or Group//TANK occurs first is a according to palyer preference. Since they
	are mutually exclusive, order does not matter.

	NOT_TANK priority order: Haste, Enhance Dual Wield, STR, Attack Power
	TANK priority order: Defense, Evasion, VIT, enmity plus, Shield Skill+ and to a lesser extent HP+ and haste
--]]

	['TP'] = {
		Subset = {
			[1] = 'Default',
			[2] = { 'AttackPower//NOT_TANK', 'Enmity_Plus//TANK' }
		},
		GROUP//NOT_TANK = {
			Head  = { 'Homam Zucchetto', 'Rogue\'s Bonnet', 'Empress Hairpin' },
			Hands = { 'Homam Manopolas', 'Dusk Gloves' },
			Waist = { 'Swift Belt', 'Mrc.Cpt. Belt' },
		},
		GROUP//TANK = {
			Head  = { 'Asn. Bonnet +1', 'Assassin\'s Bonnet', 'Homam Zucchetto' },
			Neck  = 'Peacock Amulet',
			Ears  = { 'Ethereal Earring', 'Stealth Earring//SJNIN', 'Brutal Earring' },
			Body  = { 'Dragon Harness//DT_BREATH', 'Narasimha\'s Vest', 'Assassin\'s Vest', 'Scorpion Harness', 'Brigandine', 'Wonder Kaftan' },
			Hands = { 'Homam Manopolas', 'Assassin\'s Armlets' },
			Rings = { 'Kshama Ring No.4', 'Bomb Queen Ring', 'Kshama Ring No.3' },
		},
    },

--[[
	The accuracy set is different than other sets since it is applied via a progressive
	utility (defined in the Progressive > accuracy structure.) The player defines what
	slots will be applied together in the associated structure and use the /acc command
	to collectively equip said accuracy gear.

	The accuracy set is where you define what gear pieces should be equipped, in a
	priority order, for each gear slot. But remember, the accuracy set is not equipped
	as a whole unless you've defined a stage in the progressive structure where the
	set is to be completely loaded.
	
	Include equipment with accuracy bonus and DEX. Remember, DEX converts to accuracy: (horizon) 
	for every 1 point of DEX you get 0.70 points of accuracy if wielding a 2H weapon, 0.65 for 
	a 1H weapon, and 0.60 for H2H. 
	
	For THF, make sure the accuracy settings maximize accuracy while minimizing loss of haste
--]]
	
	['Accuracy'] = {
		Group//NOT_TANK = {
			Head  = { 'Optical Hat', 'Homam Zucchetto', 'Empress Hairpin' },		-- +10/4 Acc, +3 DEX
			Neck  = { 'Peacock Amulet', 'Spike Necklace' },							-- +10 Acc, +3 DEX
			Body  = { 'Homam Corazza', 'Scorpion Harness', 'Narasimha\'s Vest', 'Brigandine' },	-- +15/10/4 Acc, +2 DEX
			Rings = { 'Toreador\'s Ring', 'Toreador\'s Ring', 'Woodsman Ring', 'Woodsman Ring', 'Jaeger Ring', 'Kshama Ring No.2' }, -- +7/+7/+5/+5/4/2 Acc
			Hands = { 'Homam Manopolas', 'Battle Gloves' },							-- +4/3 Acc
			Waist = { 'Life Belt', 'Tilt Belt', 'Swift Belt', 'Mrc.Cpt. Belt' },	-- +10/5/3 Acc, +1 DEX
			Legs  = 'Homam Cosciales',												-- +3 ACC
			Feet  = { 'Homam Gambieras', 'Bounding Boots' },						-- +6 Acc, +3 DEX
		},
		Group//TANK = {
			Head  = { 'Homam Zucchetto', 'Empress Hairpin' },						--  +4 Acc, +3 DEX
			Neck  = { 'Peacock Amulet', 'Spike Necklace' },					-- +10 Acc, +3 DEX
			Body  = { 'Homam Corazza', 'Scorpion Harness', 'Narasimha\'s Vest', 'Brigandine' },		-- +15/10/4 Acc, +2 DEX
			Rings = { 'Toreador\'s Ring', 'Toreador\'s Ring', 'Jaeger Ring', 'Kshama Ring No.2' },	-- +7/+7/4/2 Acc
			Hands = { 'Homam Manopolas', 'Battle Gloves' },							-- +4/3 Acc
			Waist = { 'Life Belt', 'Swift Belt', 'Mrc.Cpt. Belt' },					-- +10/3 Acc, +1 DEX
			Legs  = 'Homam Cosciales',												-- +3 ACC
			Feet  = { 'Homam Gambieras', 'Bounding Boots' },						-- +6 Acc, +3 DEX
		},
	},

--[[
	Similar to the accuracy set, the RaPeacocknged_Accuracy set will be used on ranged attacks in
	a manner definied in the Progressive > Ranged_Accuracy definition.
--]]

	['Ranged_Accuracy'] = {
		Head  = 'Optical Hat',			-- +10 RAcc
		Neck  = 'Peacock Amulet',		-- +10 RAcc
		Body  = 'Rapparee Harness',		--  +2 RAcc
		Rings = { 'Woodsman Ring', 'Woodsman Ring', 'Jaeger Ring' },	-- +5/5/4 RAcc
		Back  = 'Psilos Mantle',		--  +1 RAcc
		Feet  = 'Homam Gambieras'		--  +6 RAcc
	},

--[[
	The progressive structure is used for determining what should be equipped
	when for handling accuracy/ranged accuracy. You create stages to load
	accuracy gear from. Depending on what the player specifies,	that stage and
	any before it will be loaded.

	Progressive supports two groups: Accuracy and Ranged_Accuracy. The referred
	to sets are where, through inline conditionals, groups will be identified.
	This means that the progressive structure does not contain any grouping
	information. So, //TANK and //NOT_TANK will only be found in the referred
	to sets and not the progressive structure.
--]]

  ['Progressive'] = { 
		['Accuracy'] = { 
			[1] = { 
				['Head'] = 'Accuracy::Head',
				['Neck'] = 'Accuracy::Neck',
				['Body'] = 'Accuracy::Body'
			},
			[2] = {
				['Body']  = 'Accuracy::Body',
				['Hands'] = 'Accuracy::Hands',
				['Legs']  = 'Accuracy::Legs',
				['Feet']  = 'Accuracy::Feet'
			},
			[3] = {	
				['Rings'] = 'Accuracy::Rings'
			},
			[4] = {
				['Subset'] = {
					[1] = 'Accuracy'
				}
			}
		},
		['Ranged_Accuracy'] = {
			[1] = {
				['Head'] = 'Ranged_Accuracy::Head',
				['Neck'] = 'Ranged_Accuracy::Neck',
				['Legs'] = 'Ranged_Accuracy::Legs'
			},
			[2] = {
				['Subset'] = {
					[1] = 'Ranged_Accuracy'
				}
			}
		}				
  },
  
--[[
	If evasion wanted, equip evasion gear. Remember that AGI converts to evasion: for every
	2 points of AGI you get 1 point of evasion. Like TP and Accuracy, the evasion set has a
	Tank_Evasion variation.	
--]]
	
	['Evasion'] = {
        Head  = { 'Optical Hat', 'Empress Hairpin' },			-- +10/10 Eva
        Ears  = { 'Bat Earring//BLINDED', 'Ethereal Earring', 'Genin Earring//SJNIN', 'Drone Earring' },	-- +15 Eva if blind, +5 Eva, +4 AGI if sj NIN, +3 AGI
        Body  = { 'Scorpion Harness', 'Narasimha\'s Vest' },	-- +10/4 Eva
        Hands = 'Battle Gloves',		-- +3 Eva
        Rings = 'Kshama Ring No.3',		-- +3 AGI
        Waist = 'Scouter\'s Rope',		-- +10 Eva
        Feet  = { 'Dance Shoes', 'Bounding Boots' },			-- +6 Eva, +3 AGI
    },

--[[
	The damage taken sets are not equipped directly but rather from subsets. They're a
	way to reduce a specific types of damage. As such they're optional and up to the 
	player to decide if they should be defined and how they're used.
--]]

	['Damage_Taken_Breath'] = {
		Body = 'Dragon Harness',		-- -10% damage reduction from breath
	},
	
	['Damage_Taken_Physical'] = {
		Main = 'Earth Staff//WSWAP'		-- -20% damage reduction from physical
	},
	
	['Damage_Taken_Magical'] = {
		Ears = 'Coral Earring',			--  -1% damage reduction from magic
	},
	
--[[
	When you are resting (kneeling down), your HP 'Resting' set will be equipped. If your subjob
	uses MP and your MP is below the set threshhold (defined by gcinclude.settings.RefreshGearMP), 
	your MP 'Resting_Refresh' gear set will be equipped. Regardless of which set is equipped, 
	assuming that your subjob uses magic, you have a Dark/Pluto staff accessible, weapon swapping 
	is enabled (/wswap), and your MP is not at maximum, the Dark/Pluto staff will automatically be 
	equipped.
	
	The Damage_Taken_* sets are added as a subset to reduce damage accordingly because
	you're in a vulnerable state.
--]]
	
	['Resting_Regen'] = { 
		Subset = { 	'Damage_Taken_Breath//DT_BREATH',
					'Damage_Taken_Magical//DT_MAGICAL',
					'Damage_Taken_Physical//DT_PHYSICAL',
				},
		},
		Head = 'President. Hairpin//NOT_OWN',
	},
	
	['Resting_Refresh'] = {
		Subset = {	'Damage_Taken_Breath//DT_BREATH',
					'Damage_Taken_Magical//DT_MAGICAL',
					'Damage_Taken_Physical//DT_PHYSICAL',
				}
		},
		Main = 'Pluto\'s Staff//WSWAP//MSJ',
		Body = 'Black Cotehardie//MP.LT.25',
	},

--[[
	Start weapons are where you define what you want the first row of equipment to look 
	like when you either log in as a THF or switch your main job to THF. Any other gear 
	you mention will be overridden by the Idle or Town set, so no need to include here.
--]]

	['Start_Weapons'] = {
		Main  = 'Heart Snatcher',
		Sub   = { 'X\'s Knife//SJNIN', 'Tatami Shield' },
		Range = 'Cmb.Cst. B\'merang',
    },
	
--[[
	Preshot is the first stage of when a ranged shot is being performed. This is where 
	you place any gear that reduces the time it takes to shoot (snap shot, rapid shot, 
	quick shot, shot delay reduction, and ranged haste).
--]]
	['Preshot'] = {
		Head  = { 'Optical Hat', 'Homam Zucchetto', 'Empress Hairpin' }		-- +10/4 Acc, +3 DEX
    },
	
--[[
	Midshot is the second stage of a ranged shot. This is where you place
	Ranged Accuracy, Ranged Attack, Ranged Damage, Crit. Rate, Crit. Damage,
	Store TP, recycle, etc.
--]]

	['Midshot'] = {
		Ears  = 'Brutal Earring',							-- Store TP +1
		Back  = { 'Psilos Mantle', 'Amemet Mantle' },		-- +12/10 RAtt
    },

--[[
	*************************
	* Spell Casting Subsets *
	*************************
	
	The following sets are to be used as subsets. Once you get to individual 
	sets, include one of these or ignore them and be explicit on the gear in 
	that set.
--]]

	['INT'] = {	
		Head  = 'Rogue\'s Bonnet',										-- +5 INT
		Body  = { 'Black Cotehardie', 'Baron\'s Saio' },				-- +2/1 INT
		Rings = { 'Tamas Ring', 'Kshama Ring No.5', 'Flame Ring' },		-- +5/3/2 INT
		Waist = 'Mrc.Cpt. Belt',										-- +1 INT
		Feet  = 'Mannequin Pumps',										-- +1 INT
	},
	
	['MND'] = {
		Neck  = { 'Promise Badge', 'Justice Badge' },		-- +5/3 MND
		Ears  = 'Geist Earring',							-- +1 MND
		Body  = { 'Wonder Kaftan', 'Baron\'s Saio' },		-- +1/1 MND
		Hands = 'Baron\'s Cuffs',							-- +1 MND
		Rings = { 'Tamas Ring', 'Kshama Ring No.9' },		-- +5/3 MND
		Waist = { 'Mrc.Cpt. Belt', 'Friar\'s Rope' },		-- +1/1 MND
		Legs  = 'Wonder Braccae',							-- +2 MND
		Feet  = 'Mannequin Pumps',							-- +2 MND
	},

	['Enmity_Plus'] = {
		Head  = { 'Asn. Bonnet +1','Assassin\'s Bonnet' },	-- +3/2 Emnity
		Body  = 'Assassin\'s Vest',							-- +3 Emnity
		Hands = 'Homam Manopolas',							-- +3 Emnity
		Waist = 'Warwolf Belt',								-- +3 Emnity
		Feet  = 'Assassin\'s Pouln.',						-- +2 Enmity
	},
	
	['Enmity_Minus'] = {
		Neck  = 'Fenrir\'s Torque//NIGHTTIME',		-- -3 Emnity at night
		Rings = 'Tamas Ring',						-- -5 Emnity
	},
	
--[[
	Spells are a bit different. Each type of spell can have it's own enhancement gear as well as 
	stat based gear. (In some cases individual spells have special entries.) These sets do not 
	include elemental gear which is	dependent on day/weather/weapon skill.

	The first stage is Precast. This is where you place any Fast Cast, cast time reduction, quick 
	cast gear, and spell interruption rate
--]]

	['Precast'] = {	
		Ears = 'Loquac. Earring',
		Legs = 'Homam Cosciales',
	},

--[[
	A lot of spells have a better chance of landing if you increase your
	magic accuracy. By defining the Macc/Tank_Macc sets, if the option is 
	enabled on your displaybar, the defined gear will be equipped on all 
	spells where magic accuracy can have an effect. Alternatively, leave 
	these gearsets empty and explicitly include magic accuracy gear in 
	each of	the individual midcast gear sets. (Doing so removes the 
	ability to optionally not include magic accuracy.)
--]]
	
	['Macc'] = {
		Subset = { 'Dark_Magic_Skill//DARK',
				   'Elemental_Magic_Skill//ELEMENTAL',
				   'Enfeebling_Magic_Skill//ENFEEBLING',
				   'Healing_Magic_Skill//HEALING',		-- Offensive healing only
				   'Divine_Magic_Skill//DIVINE',
				   'Ninjutsu_Skill//NINJUTSU',
				   'CHR//SINGING' },		-- Charisma provides accuracy w/singing
		Head   = 'Homam Zucchetto', 	-- +4 Macc
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
	main job supports tanking (like THF does.) I've included details on
	what each gear set is suppose to feature and what stats you should
	be emphasizing. Further, where appropriate, any formulas/charts that
	will help you to decide what gear to include. 
--]]	

--[[
	**************************
	* Midcast: Healing Magic *
	**************************
--]]

	['Healing_Magic_Skill'] = {
	},
	
--[[
	Healing Magic: consisting of all light-based spells, removes 
	some debuffs on players, buffs the caster, cures the health of 
	players or npcs, or causing damage to undead monsters. Healing 
	magic skill affects the	potency of cures while decreasing the 
	likelihood of the caster being interrupted.
	
	Healing spells: cures, curagas, raises, reraises, blindna, cursna,
	paralyna, poisona, silena, stona, and viruna.
--]]

--[[	
	Curing magic addresses healing players/npcs. Each time a cure 
	spell is cast, a power calculation is performed to determine 
	the base effect of the spell. After that, any bonuses will be 
	applied. What this means is that MND, VIT and healing magic 
	skill impact your power rating, but once the cap is hit, they 
	have no more influence.
	
		power = (MND*3) + VIT + (Healing Magic Skill*0.6)
	
	This chart lists all RDM curing spells, the power cap, and the 
	effect on HP baseline. The curaga spells are included in case
	you subbed WHM.
	
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
		Subset = 'MND',
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
		Subset = 'CuringMagic'
	},

--[[
	This set is used for all non-cure Healing Magic spells. Only 
	healing magic skill is of any importance here. You might want 
	to use this set as a subset for the other cure-based sets.
--]]
	['HealingMagic'] = {
	},

--[[
	****************************
	* Midcast: Enhancing Magic *
	****************************
--]]

	['Enhancing_Magic_Skill'] = {
	},
	
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
		Subset = 'INT',
		Neck   = 'Uggalepih Pendant//SPECIAL',		-- +8 MAB when MP% < 51%
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
		Subset = 'MND',
	},
	
--[[
	Sneak's duration is variable, but the duration maxes at about 5 
	minutes. Include any gear that enhances this buff.
--]]

	['Sneak'] = {
		Feet = 'Dream Boots +1',
	},

--[[
	Invisible's duration is variable, but the duration maxes at 
	about 5 minutes. Include any gear that enhances this buff.
--]]	

	['Invisible'] = {
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
	},

--[[
	****************************
	* Midcast: Elemental Magic *
	****************************
--]]

	['Elemental_Magic_Skill'] = {
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
		Subset = 'INT',
		Neck   = 'Uggalepih Pendant//SPECIAL', 		-- +8 MAB when MP% < 51%
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
		Subset = 'INT',
		Neck = 'Uggalepih Pendant//SPECIAL', 		-- +8 MAB when MP% < 51%
	},

--[[
	**********************
	* Midcast: Summoning *
	**********************
--]]

	['Summoning_Skill'] = {
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
		Subset = 'Summoning_Skill',
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

	['Dark_Magic_Skill'] = {
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
	},
	
--[[
	This last gear set, DarkMagic, covers all Dark Magic spells not covered
	by the previous three gear sets. 
--]]

	['DarkMagic'] = {
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

	['Divine_Magic_Skill'] = {
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
		Subset = {
			[1] = 'Divine_Magic_Skill',
			[2] = 'MND',
		}
	},

--[[
	Enfeebling divine spell (flash) afflicts the target with accuracy 
	reduction (similar to blind) with a weakening effect over time till
	it runs out. Duration is subject to resists and partial resists
	although can last 12 seconds if not resisted. It also generates a
	significant amount of volitile and cumulative enmity.
--]]	
	
	['EnfeebleDivine'] = {
		Subset = 'Divine_Magic_Skill',
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
		Subset = 'Divine_Magic_Skill'.
	},

--[[
	*****************************
	* Midcast: Enfeebling Magic *
	****************************
--]]

	['Enfeebling_Magic_Skill'] = {
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
		Subset = {
			[1] = 'Enfeebling_Magic_Skill',
			[2] = 'INT',
		},
	},
	
	['EnfeeblingMND'] = {
		Subset = {
			[1] = 'Enfeebling_Magic_Skill',
			[2] = 'MND',
		},
	},
	

	['EnfeeblingMagic'] = {
		Subset = 'Enfeebling_Magic_Skill',
	},
	
--[[
	********************
	* Midcast: Singing *
	********************
--]]

	['CHR'] = {		-- Charisma provides accuracy w/singing
		Main//WSWAP  = { 'Pluto\'s Staff', 'Light Staff' },			-- +2/1 CHR
		Head  = 'Entrancing Ribbon',								-- +2 CHR
		Neck  = { 'Star Necklace', 'Flower Necklace' },				-- +3/3 CHR
		Ears  = 'Beastly Earring',									-- +2 CHR
		Hands = 'Assassin\'s Armlets',								-- +5 CHR
		Rings = 'Kshama Ring No.6',									-- +3 CHR
		Waist = 'Mrc.Cpt. Belt',									-- +1 CHR
		Feet  = { 'Assassin\'s Pouln.', 'Dance Shoes' }				-- +5/3 CHR
	},
	
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

--[[
	EnhancementSinging contains gear that enhances party members is some specific
	manner. Included are: minne, minuet, paeon, pastoral, madigal, mambo, etude,
	operetta, ballad, march, prelude, aubade, carol, mazurka, gavotte, capriccio,
	fantasia, hymnus, and round.
--]]

	['EnhancementSinging'] = {
		Subset = 'CHR',
	},

--[[
	EnfeeblingSinging contains gear that debuffs targets. Included are: requiem,
	threnody, lullaby, finale, elegy, and virelai.
--]]
	
	['EnfeeblingSinging'] = {
		Subset = 'EnhancementSinging',
	},

--[[
	********************
	* Midcast: Ninjusu *
	********************
--]]

	['Ninjutsu_Skill'] = {
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
		Subset = 'Ninjutsu_Skill',
	},

-- An elemental stave will be checked for after the debuff set is loaded.
	
	['NinjutsuDebuff'] = {
		Subset = 'Ninjutsu_Skill',
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
		Subset = {
			[1] = 'Ninjutsu_Skill',
			[2] = 'INT',
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
	The following weapon skill gearsets are defined by the stat they emphasize. Listed are all of the sets that
	you will need to use every weapon skill that your job can do. The leading comment defines what weapon/weapon
	skill combination the set applies to.
	
	THF can use the following weapons: Dagger (A-), Sword (D), Club (E), H2H (E), Marksmanship (C+), Archery (C-)
		
	Please note that on Horizon you may have access to some weapon skills
	through your subjob. While not explicitly supported here, the appropriate
	weapon skill set will be loaded. If not listed below, you might have to
	create a custom gear set to support the skill. Remember, weapon skill sets
	are named WS_attr. If you name the set appropriately, that set will auto-
	matically be called when you use the weapon skill.

	Most weapon skills emphasize one or more primary stats, so the following
	gear sets are broken out by which primary stat is featured. (I have
	included what weapon skills use that stat. Besides the primary stats
	though, gear with attack power should also be included. The AttackPower
	gear set is not directly equipped, but rather used as a subset. It is
	recommended that it be included in each weaponskill gear set.	
--]]

	['AttackPower'] = {
		Ears  = { 'Ethereal Earring', 'Coral Earring', 'Fang Earring' },	-- +5/5/4 Att
		Body  = 'Dragon Harness',											-- +10 Att
		Hands = 'Dusk Gloves',												-- +5 Att
		Rings = 'Kshama Ring No.8',											-- +3 Att
		Back  = { 'Forager\'s Mantle', 'Psilos Mantle', 'Amemet Mantle' },	-- +15/12/10 Att
		Waist = 'Warwolf Belt//IF:SWIFT BELT',
	},
	
--[[
		* Strength based *
		
		Sword: Flat Blade,Circle Blade,Vorpal Blade,Spirits Within,Mercy Stroke
		Club: Starlight,Skull Breaker,True Strike
		H2H: Spinning Attack
-]]
	
	['WS_STR'] = {
		Subset = 'AttackPower',
		Neck  = 'Spike Necklace',						-- +3 STR
		Body  = { 'Narasimha\'s Vest', 'Black Cotehardie', 'Rogue\'s Vest', 'Wonder Kaftan' },	-- +3/3/3/1 STR
		Hands = 'Wonder Mitts',							-- +3 STR
		Rings = { 'Flame Ring', 'Kshama Ring No.8' },	-- +5/3 STR
		Back  = { 'Forager\'s Mantle', 'Amemet Mantle' },	-- +3/1 STR
		Waist = { 'Warwolf Belt', 'Mrc.Cpt. Belt' },	-- +5/1 STR
		Legs  = 'Wonder Braccae',						-- +1 STR
		Feet  = { 'Creek F clomps', 'Wonder Clomps' },	-- +4/2 STR
	},
	
--[[
		* Strength and Agility based, ranged *
		
		Archery: Flaming Arrow^,Piercing Arrow^,Dulling Arrow^,Sidewinder^
		
		^ Subjob must be RNG
--]]

	['WS_RANGED_STRAGI'] = {
		Subset = 'AttackPower',
		Head   = 'Empress Hairpin',								-- +3 AGI
		Neck   = 'Spike Necklace',								-- +3 STR
		Ears   = { 'Genin Earring//SJNIN', 'Drone Earring' },	-- +4 AGI if sj is NIN, +3 AGI
		Body   = { 'Dragon Harness', 'Black Cotehardie', 'Assassin\'s Vest', 'Rogue\'s Vest', 'Wonder Kaftan' },	-- +6 AGI, +3 STR/+3 AGI, +4 AGI, +3/1 STR
		Hands  = 'Wonder Mitts',								-- +3 STR
		Rings  = { 'Flame Ring', 'Kshama Ring No.8', 'Kshama Ring No.3' },	-- +5/3 STR, +3 AGI
		Back   = { 'Forager\'s Mantle', 'Amemet Mantle' },		-- +3/1 STR
		Waist  = { 'Warwolf Belt', 'Mrc.Cpt. Belt' },			-- +5 STR, +1 STR/+1 AGI
		Legs   = { 'Rogue\'s Culottes', 'Wonder Braccae' },		-- +4 AGI, +1 STR
		Feet   = { 'Creek F clomps', 'Bounding Boots' },		-- +4 STR, +3 AGI
	},
		
--[[
		* Strength and Dexterity based, even weighting *
		
		Sword: Fast Blade
		H2H: Combo,Backhand Blow,Raging Fist^
		
		^ Subjob must be MNK
--]]

	['WS_STRDEX'] = {
		Subset = 'AttackPower',
		Head  = { 'Asn. Bonnet +1', 'Empress Hairpin' },					-- +6/3 DEX
		Neck  = 'Spike Necklace',											-- +3 STR
		Body  = { 'Dragon Harness', 'Narasimha\'s Vest', 'Brigandine' },	-- +6/4/2 DEX
		Hands = { 'Rogue\'s Armlets', 'Wonder Mitts' },						-- +3 DEX, +3 STR
		Rings = { 'Flame Ring', 'Kshama Ring No.8', 'Kshama Ring No.2' },	-- +5/4 STR, +3 DEX
		Back  = { 'Forager\'s Mantle', 'Amemet Mantle' },					-- +3/1 STR
		Waist = { 'Warwolf Belt', 'Mrc.Cpt. Belt' },						-- +5 STR/+5 DEX, +1 STR/+1 DEX
		Legs  = 'Wonder Braccae',											-- +1 STR
		Feet  = { 'Creek F clomps', 'Rogue\'s Poulaines', 'Bounding Boots' },	-- +4 STR, +3/3 DEX
	},
	
--[[
		* Strength and Intelligence based, even weighting *
		
		Sword: Burning Blade,Red Lotus Blade
--]]
	
	['WS_STRINT'] = {
		Subset = 'AttackPower',
		Head  = 'Rogue\'s Bonnet',					-- +5 INT
		Neck  = 'Spike Necklace',					-- +3 STR
		Body  = { 'Black Cotehardie', 'Narasimha\'s Vest', 'Rogue\'s Vest', 'Wonder Kaftan' },	-- +3 STR/+2 INT, +3/3/1 STR
		Hands = 'Wonder Mitts',						-- +3 STR
		Rings = { 'Tamas Ring', 'Flame Ring', 'Kshama Ring No.8', 'Kshama Ring No.5' },		-- +5 INT, +5/3 STR, +3 INT
		Back  = { 'Forager\'s Mantle', 'Amemet Mantle' },			-- +3/1 STR
		Waist = { 'Warwolf Belt', 'Mrc.Cpt. Belt' },				-- +5 STR/+5 DEX, +1 STR/+1 INT
		Legs  = 'Wonder Braccae',					-- +1 STR
		Feet  = { 'Creek F Clomps', 'Wonder Clomps' },				-- +4/2 STR
	},
	
--[[
		* Strength and Mind based, even weighting *
		
		Sword: Shining Blade,Seraph Blade
		Club: Shining Strike
--]]

	['WS_STRMND'] = {
		Subset = 'AttackPower',
		Neck  = { 'Promise Badge', 'Justice Badge' },		-- +5/3 MND
		Ears  = 'Geist Earring',							-- +1 MND
		Body  = { 'Narasimha\'s Vest', 'Rogue\'s Vest', 'Wonder Kaftan' },		-- +3/3/1 STR
		Hands = 'Wonder Mitts',								-- +3 STR
		Rings = { 'Tamas Ring', 'Flame Ring', 'Kshama Ring No.8', 'Kshama Ring No.9' },		-- +5 MND, +5/3 STR, +3 MND
		Back  = { 'Forager\'s Mantle', 'Amemet Mantle' },	-- +3/1 STR
		Waist = { 'Warwolf Belt', 'Mrc.Cpt. Belt' },		-- +5 STR, +1 STR/+1 MND
		Legs  = 'Wonder Braccae',							-- +1 STR
		Feet  = { 'Creek F clomps', 'Wonder Clomps' },		-- +4/2 STR
	},
	
--[[
		* Agility based, ranged *
		
		Marksmanship: Hot Shot^,Split Shot^,Sniper Shot^,Slug Shot^
		
		^ Subjob must be RNG
--]]

	['WS_RANGED_AGI'] = {
		Subset = 'AttackPower',
		Head  = 'Empress Hairpin',								-- +3 AGI
		Ears  = { 'Genin Earring//SJNIN', 'Drone Earring' },	-- +4 AGI if sj NIN, +3 AGI
		Body  = { 'Dragon Harness', 'Assassin\'s Vest', 'Black Cotehardie' },		-- +6/4/3 AGI
		Rings = 'Kshama Ring No.3',								-- +3 AGI
		Back  = 'Assassin\'s Cape',								-- +4 AGI
		Waist = 'Mrc.Cpt. Belt',								-- +1 AGI
		Legs  = 'Rogue\'s Culottes',							-- +4 AGI
		Feet  = 'Bounding Boots',								-- +3 AGI
	},
	
--[[
		* Charisma based *
		
		Dagger: Shadowstitch
--]]
	
	['WS_CHR'] = {
		Subset = {
			[1] = 'AttackPower',
			[2] = 'CHR'
		},
  },
	
--[[
		* Dexterity based *
		
		Dagger: Wasp Sting,Viper Bite,Dancing Edge
--]]
	
	['WS_DEX'] = {
		Subset = 'AttackPower',
		Head   = { 'Asn. Bonnet +1', 'Assassin\'s Bonnet', 'Empress Hairpin' },		-- +6/5/3 DEX
		Neck   = 'Spike Necklace',						-- +3 DEX
		Body   = { 'Dragon Harness', 'Brigandine' },	-- +6/2 DEX
		Hands  = 'Rogue\'s Armlets',					-- +3 DEX
		Rings  = 'Kshama Ring No.2',					-- +3 DEX
		Back   = 'Assassin\'s Cape',					-- +4 DEX
		Waist  = { 'Warwolf Belt', 'Mrc.Cpt. Belt' },	-- +5/1 DEX
		Feet   = { 'Rogue\'s Poulaines', 'Bounding Boots' },	-- +3/3 DEX
	},
	
--[[
		* Dexterity and Agility based *
		
		Dagger: Shark Bite
--]]
	
	['WS_DEXAGI'] = {
		Subset = 'AttackPower',
		Head  = { 'Asn. Bonnet +1', 'Assassin\'s Bonnet', 'Empress Hairpin' },		-- +6/5/3 DEX
		Neck  = 'Spike Necklace',								-- +3 DEX
		Ears  = { 'Genin Earring//SJNIN', 'Drone Earring' },	-- +4 AGI if sj is NIN, +3 AGI
		Body  = { 'Dragon Harness', 'Assassin\'s Vest', 'Brigandine' },	-- +6 DEX/+6 AGI, +4 AGI, +2 DEX
		Hands = 'Rogue\'s Armlets',								-- +3 DEX
		Rings = { 'Kshama Ring No.2', 'Kshama Ring No.3' },		-- +3 DEX, +3 AGI
		Back  = 'Assassin\'s Cape',								-- +4 DEX/+4 AGI
		Waist = { 'Warwolf Belt', 'Mrc.Cpt. Belt' },			-- +5 DEX, +1 DEX/+1 AGI
		Feet  = { 'Rogue\'s Poulaines', 'Bounding Boots' },		-- +3 DEX, +3 DEX/+3 AGI
	},
	
--[[
		* Dexterity and Charisma based *
		
		Dagger: Eviseration+
		
		+ Horizon modified
--]]
	
	['WS_DEXCHR'] = {
		Subset = 'AttackPower',
		Head  = { 'Asn. Bonnet +1', 'Assassin\'s Bonnet', 'Empress Hairpin' },	-- +6/5/3 DEX
		Neck  = 'Spike Necklace',												-- +3 DEX
		Ears  = 'Beastly Earring',												-- +2 CHR
		Body  = { 'Dragon Harness', 'Brigandine' },								-- +6/2 DEX
		Hands = 'Rogue\'s Armlets',												-- +3 DEX
		Rings = { 'Kshama Ring No.2', 'Kshama Ring No.6'},						-- +3 DEX, +3 CHR
		Back  = 'Assassin\'s Cape',												-- +4 DEX
		Waist = { 'Warwolf Belt', 'Mrc.Cpt. Belt' },							-- +5 DEX, +1 DEX/+1 CHR
		Feet  = { 'Rogue\'s Poulaines', 'Bounding Boots' },						-- +3/3 DEX
	},
		
--[[
		* Dexterity and Intelligence based *
		
		Dagger: Gust Slash,Cyclone
--]]
	
	['WS_DEXINT'] = {
		Subset = 'AttackPower',
		Head  = { 'Asn. Bonnet +1', 'Assassin\'s Bonnet', 'Empress Hairpin' },	-- +6/5/3 DEX
		Neck  = 'Spike Necklace',												-- +3 DEX
		Body  = { 'Dragon Harness', 'Brigandine' },								-- +6/2 DEX
		Hands = 'Rogue\'s Armlets',												-- +3 DEX
		Rings = { 'Tamas Ring', 'Kshama Ring No.2', 'Kshama Ring No.5' },		-- +5/3 DEX, +3 INT
		Back  = 'Assassin\'s Cape',												-- +4 DEX
		Waist = 'Mrc.Cpt. Belt',												-- +1 DEX/+1 INT
		Feet  = { 'Rogue\'s Poulaines', 'Bounding Boots' },						-- +3/3 DEX
	},

--[[
		* Mind based *
		
		Dagger: Energy Steal,Energy Drain
--]]

	['WS_MND'] = {
		Subset = {
			[1] = 'AttackPower',
			[2] = 'MND',
		},
	},
		
--[[
		* Vitality based *
		
		H2H: Shoulder Tackle,One Inch Punch^
		
		^ Subjob must be MNK
--]]

	['WS_VIT'] = {
		Subset = 'AttackPower',
		Body  = { 'Narasimha\'s Vest', 'Brigandine' },		-- +3/2 VIT
		Rings = 'Kshama Ring No.4',							-- +3 VIT
		Waist = { 'Warwolf Belt', 'Mrc.Cpt. Belt' },		-- +5/1 VIT
		Legs  = 'Wonder Braccae',							-- +2 VIT
		Feet  = 'Creek F Clomps',							-- +4 VIT
	},
	
--[[
		* Skill based *
		
		Club: Starlight,Moonlight
--]]

	['WS_Skill'] = {
		Subset = 'AttackPower',
	},
	
--[[
		* HP based *
		
		Sword: Spirits Within
--]]

	['WS_HP'] = {
		Subset = 'AttackPower',
		Head   = { 'Homam Zucchetto', 'Asn. Bonnet +1', 'Assassin\'s Bonnet', 'Rogue\'s Bonnet' },		-- 22/16/16/13 HP
		Neck   = 'Promise Badge',																		-- 10 HP
		Ears   = { 'Physical Earring//MP.GE.15', 'Physical Earring//MP.GT.40', 'Ethereal Earring' },	-- Convert 25 MP to HP x2, +15 HP
		Body   = 'Wonder Kaftan',																		-- +36 HP
		Hands  = { 'Homam Manopolas', 'Wonder Mitts', 'Rogue\'s Armlets' },								-- +20/12/10 HP
		Rings  = { 'Bomb Queen Ring', 'Toreador\'s Ring', 'Toreador\'s Ring' },							-- +75/10/10 HP
		Waist  = 'Powerful Rope',																		-- +20 HP
		Legs   = { 'Homam Cosciales', 'Wonder Braccae', 'Rogue\'s Culottes' },							-- +26/21/15 HP
		Feet   = { 'Creek F Clomps', 'Homam Gambieras', 'Wonder Clomps' },								-- +35/31/20 HP
  },
		
--[[
	Kite is used for kiting. Emphasis should be placed on gear that increases 
	movement speed, but you might also want gear that has evasion. The choice
	is yours.
--]]

	['Kite'] = { 
		Hands = 'Assassin\'s Armlets//IF:Dusk Gloves',					-- Make sure -movement speed gone
	},

--[[
	The following are abilities affected by gear  ** this far **
--]]

	['Perfect Dodge'] = {
	},
	
	['Steal'] = {
		Head  = 'Rogue\'s Bonnet',				-- +1 Steal
		Hands = 'Rogue\'s Armlets',				-- +1 Steal
		Legs  = 'Rogue\'s Culottes',			-- +1 Steal
		Feet  = 'Rogue\'s Poulaines',			-- +2 Steal
	},
	
	['SneakAttack'] = {
		Head  = { 'Asn. Bonnet +1', 'Assassin\'s Bonnet', 'Empress Hairpin' },	-- +6/5/3 DEX
		Neck  = 'Spike Necklace',												-- +3 DEX
		Body  = { 'Dragon Harness', 'Brigandine' },								-- +6/2 DEX
		Hands = 'Rogue\'s Armlets',												-- +3 DEX
		Rings = 'Kshama Ring No.2',												-- +3 DEX
		Back  = { 'Forager\'s Mantle', 'Assassin\'s Cape' },					-- +3 STR/+15 Att, +4 DEX (The forager's mantle seems to do more damage. Need to test this)
		Waist = { 'Warwolf Belt', 'Mrc.Cpt. Belt' },							-- +5/1 DEX
		--Legs  = 'Dragon Subligar',
		Feet  = { 'Rogue\'s Poulaines', 'Bounding Boots' },						-- +3/3 DEX
	},
	
	['Flee'] = {
		Feet = 'Rogue\'s Poulaines',		-- Increase Flee duration
	},

	['TrickAttack'] = {
		Head  = 'Empress Hairpin',												-- +3 AGI
		Ears  = { 'Genin Earring//SJNIN', 'Drone Earring' },					-- +4 AGI if sj is NIN, +3 AGI
		Body  = { 'Dragon Harness', 'Assassin\'s Vest' },						-- +6/4 AGI
		Rings = 'Kshama Ring No.3',												-- +3 AGI
		Back  = { 'Assassin\'s Cape', 'Forager\'s Mantle' },					-- +4 AGI, +3 STR/+15 Att (While the forager's cape probably would do more damage, the enmity of the assassin's cape is too useful)
		Waist = 'Mrc.Cpt. Belt',												-- +1 AGI
		Feet  = 'Bounding Boots',												-- +3 AGI
	},
	
	['SATA']  = {
		Head  = 'Empress Hairpin',												-- +3 DEX/3 AGI
		Neck  = 'Spike Necklace',												-- +3 DEX
		Ears  = { 'Genin Earring//SJNIN', 'Drone Earring' },					-- +4 AGI if sj is NIN, +3 AGI
		Body  = { 'Dragon Harness', 'Assassin\'s Vest', 'Brigandine' },			-- +6 DEX/+6 AGI, +4 AGI, +2 DEX
		Hands = 'Rogue\'s Armlets',												-- +3 DEX
		Rings = { 'Kshama Ring No.3', 'Kshama Ring No.2' },						-- +3 AGI/+3 DEX
		Back  = { 'Assassin\'s Cape', 'Forager\'s Mantle' },					-- +4 AGI/+4 DEX, +3 STR/+15 Att (While the forager's cape probably would do more damage, the enmity of the assassin's cape is too useful)
		Waist = { 'Warwolf Belt', 'Mrc.Cpt. Belt' },							-- +5 DEX. +1 DEX/+1 AGI
		Feet  = 'Bounding Boots',												-- +3 DEX/+3 AGI
	},
	
	['Mug'] = {
		Head = { 'Asn. Bonnet +1', 'Assassin\'s Bonnet' },						-- Enhances Mug
	},
	
	['Bully'] = {
	},
	
	['Hide'] = {
		Body = 'Rogue\'s Vest',				-- Increases Hide duration
	},
	
	['Accomplice'] = {
	},
	
	['Collaborator'] = {
	},
	
--[[
	While not an ability, there's times you want to toggle Treasure Hunter on
--]]
	
	['TH'] = {
		Neck  = 'Nanaa\'s Charm',			-- +1 TH
		Hands = 'Assassin\'s Armlets',		-- +1 TH
	},
	
--[[
	Some subjobs really make no sense when combined with dragoon, but all abilities across all jobs that
	have gear that can be equipped by a PLD are included here.
--]]
	--* BST *--
	-- CHR and Charm + gear. (Every +1 Charm adds 5% Charm duration)
	['Charm'] = {
		Subset = 'CHR',
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
	},--[[
	
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
	['DAGGER'] = { 'Heart Snatcher', 'X\'s Knife', 'Marauder\'s Knife' },
	['CLUB']   = { 'Warp Cudgel' },
	['STAVE'] =  { 'Fire Staff', 'Vulcan\'s Staff', 'Ice Staff', 'Aquilo\'s Staff',
				  'Wind Staff', 'Auster\'s Staff', 'Earth Staff', 'Terra\'s Staff',
				  'Thunder Staff', 'Jupiter\'s Staff', 'Water Staff', 'Neptune\'s Staff',
				  'Light Staff', 'Apollo\'s Staff', 'Dark Staff', 'Pluto\'s Staff' },
	['MARKSMANSHIP'] = { 'Thug\'s Zamburak' },
	['THROWING'] = 'Cmb.Cst. B\'merang',
};

profile.Sets = sets;
profile.sjb = nil;
profile.bAmmo = false;
profile.sAmmo = nil;

--[[
	HandlePetAction equips the appropriate gear set based on the type of action
	the pet is trying to perform. Please note that only BST pets are supported,
	not SMN avatars.
--]]

function HandlePetAction(PetAction)
	local pet = gData.GetPet();
	
	-- Only gear swap if this flag is true and the pet is a BST pet
	if gcdisplay.GetToggle('GSwap') == false or gcinclude.fSummonerPet() == true then
		return;
	end

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
	which subjob is current. 
--]]

function SetSubjobSet(chkSJ)
	-- "chkSJ" is the key for what toolbar is shown. All jobs are defined in the subs table.
	-- A value of 0 means that job is not configured. All values > 0 indicate which toolbar
	-- is to be displayed. The player must change the entries in this table to match their
	-- needs.
	local tSubs = {
		['WAR'] = 0, ['MNK'] = 0, ['WHM'] = 0, ['BLM'] = 0, ['RDM'] = 0, ['THF'] = 0,
		['PLD'] = 0, ['DRK'] = 0, ['BST'] = 2, ['BRD'] = 0, ['RNG'] = 0, ['SMN'] = 0,
		['SAM'] = 0, ['NIN'] = 1, ['DRG'] = 0, ['BLU'] = 0, ['COR'] = 0, ['PUP'] = 0,
		['DNC'] = 0, ['SCH'] = 0, ['GEO'] = 0, ['RUN'] = 0};
	local sj = 1;	-- Default toolbar

	if chkSJ == nil or chkSJ == 'NON' or 
		(profile.sjb ~= nil and profile.sjb == chkSJ) then
		return;
	end
	
	-- Compare the stored subjob with the current subjob
	if profile.sjb == nil or chkSJ ~= profile.sjb then	
		if tSubs[chkSJ] > 0 then
			sj = tSubs[chkSJ];
		end
	end

	AshitaCore:GetChatManager():QueueCommand(1, '/macro set '..tostring(sj));
	profile.sjb = chkSJ;
end		-- SetSubjobSet

--[[
	OnLoad is run whenever you log into your BST or change your job to BST
--]]

function profile.OnLoad()
	local player = gData.GetPlayer();

	gSettings.AllowAddSet = true;
	gcinclude.Initialize();
	
	-- Coded order of operation override
	gcinclude.settings.priorityEngaged = 'CEF';
	gcinclude.settings.priorityWeaponSkill = 'ADBE';
	gcdisplay.SetToggle('Tank',false);		-- Assume THF is not a tank
	gcdisplay.SetToggle('TH',true);			-- Assume THF wants TH on

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

function profile.OnUnload()
	gcinclude.Unload();
end		-- OnUnload

--[[
	HandleCommand is run when you type in a command defined in LUASHITACAST. The commands handled here instead
	of in gcinclude.HandleCommands are specific to BST or the help system, which has been tailored to BST.
--]]

function profile.HandleCommand(args)
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
	
function profile.HandleDefault()
	local player = gData.GetPlayer();
	local pet = gData.GetPet();
	local petAction = gData.GetPetAction();	
	local ew = gData.GetEquipment();
	local zone = gData.GetEnvironment();
	local bTank = gcdisplay.GetToggle('Tank');
	local bSA = gcinclude.fBuffed('Sneak Attack');
	local bTA = gcinclude.fBuffed('Trick Attack');
	local eWeap = nil;
	local cKey;

	gcinclude.StartReminder();		-- See if reminder should be printed
	
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
	gcdisplay.Update();
	
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
		if gcinclude.fIsLocked('main') == false then
			sets.CurrentGear['Main'] = gcinclude.weapon;
		end
		if gcinclude.fIsLocked('sub') == false then
			sets.CurrentGear['Sub'] = gcinclude.weapon;
		end
	end
	
	-- Start with the default set
	if bTank == true then
		gcinclude.MoveToCurrent(sets.Tank_Default,sets.CurrentGear);
	else
		gcinclude.MoveToCurrent(sets.Default,sets.CurrentGear);
	end
	
	-- Now process the player status accordingly	
	if player.Status == 'Engaged' then
		-- If sneak attack or trick attack up, make sure the appropriate gear set is
		-- equipped to maximize the damage. Note that if a weapon skill follows, the
		-- weapon skill set will take priority.
		if bSA == true or bTA == true then
			if bSA == true and bTA == true then		-- SATA
				gcinclude.MoveToCurrent(sets.SATA,sets.CurrentGear);
			elseif bSA == true then					-- SA
				gcinclude.MoveToCurrent(sets.SneakAttack,sets.CurrentGear);
			else									-- TA
				gcinclude.MoveToCurrent(sets.TrickAttack,sets.CurrentGear);
			end
		else
			if bTank == true then
				gcinclude.MoveToCurrent(sets.Tank_TP,sets.CurrentGear);
			else
				gcinclude.MoveToCurrent(sets.TP,sets.CurrentGear);
			end
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
					gcinclude.ProgressiveAccuracy('Acc');
				elseif cKey == 'F' then		-- Kiting
					if (gcdisplay.GetToggle('Kite') == true) then
						gcinclude.MoveToCurrent(sets.Kite,sets.CurrentGear);
					end
				end
			end
		
			if gcdisplay.GetToggle('TH') == true then
				gcinclude.MoveToCurrent(sets.TH,sets.CurrentGear);
			end
		end
	elseif player.Status == 'Resting' then	
		-- Player kneeling. Priority (low to high): Resting,refresh	
		if player.HP < player.MaxHP then
			gcinclude.MoveToCurrent(sets.Resting_Regen,sets.CurrentGear);
		end
		
		if gcinclude.fMagicalSubJob() == true and player.MP < player.MaxMP then	
			gcinclude.MoveToCurrent(sets.Resting_Refresh,sets.CurrentGear);
			if gcdisplay.GetToggle('WSwap') == true then
				local sStave = gcinclude.fCheckForEleGear('staff','dark');
				if sStave ~= nil then
					gcinclude.fSwapToStave(sStave,false,sets.CurrentGear);
				end
			end	
		end
	else									
		-- Assume idling. While there's no idle set, just use the 
		-- "Default" set
		if bTank == true then
			gcinclude.MoveToCurrent(sets.Tank_Default,sets.CurrentGear);
		else
			gcinclude.MoveToCurrent(sets.Default,sets.CurrentGear);
		end
	end
		
	-- In case the pet is a summoned pet...
	if pet ~= nil and gcinclude.fSummonerPet() == true then
		local sStave = gcinclude.fCheckForElementalGearByValue('staff','Summons',pet.Name);
		if sStave ~= nil then
			gcinclude.fSwapToStave(sStave,false,sets.CurrentGear);
		end
	end
	
	-- And make sure a weapon equipped. (Going into a capped area can cause no weapon to be equipped.)
	local gear = gData.GetEquipment();
	if gear.Main == nil or gear.Main.Name == nil then
		gcinclude.MoveToCurrent(sets.Start_Weapons,sets.CurrentGear,true);
	end
	
	gcinclude.EquipTheGear(sets.CurrentGear);		-- Equip the composited HandleDefault set
					
	-- Lastly, update the display, just in case
	gcdisplay.Update();
end		-- HandleDefault

--[[
	HandleAbility is used to change the player's gear appropriately.
--]]

function profile.HandleAbility()
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
		if gcdisplay.GetToggle('SS') == true then			-- flag to show the steal
			local targetIndex = gData.GetTargetIndex();
			if targetIndex ~= nil then
				local targetEntity = gData.GetEntity(targetIndex);
				local sMsg = '/s Stealing > ' .. targetEntity.Name .. ' [' .. gcinclude.fTargetId(targetIndex) .. ']';
				AshitaCore:GetChatManager():QueueCommand(-1, sMsg);
			end
		end
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
		local sStave = gcinclude.fCheckForEleGear('staff','light');
		if sStave ~= nil then
			gcinclude.fSwapToStave(sStave,false,sets.CurrentGear);
		end
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

function profile.HandleItem()
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

function profile.HandlePrecast()
	
	-- Only gear swap if this flag is true
	if gcdisplay.GetToggle('GSwap') == false then
		return;
	end

	-- Clear out the CurrentGear in case of leftovers
	gcinclude.ClearSet(sets.CurrentGear);
	
	-- Equip the precast gear set
	gcinclude.HandlePrecast();
	gcinclude.EquipTheGear(sets.CurrentGear);
end		-- HandlePrecast

--[[
	HandleMidcast is the second function invoked when a player casts a spell. It equips gear appropriate for 
	magic skill, duration, magic attack bonus, magic accuracy, and potency. There's an order to how the pieces 
	are loaded: INT/MND, spell specific, macc, magic skill, obi, ele swap	
--]]

function profile.HandleMidcast()
	local ti = gData.GetTargetIndex();
	local target = gData.GetEntity(ti);

	if gcdisplay.GetToggle('GSwap') == false then		-- Only gear swap if this flag is true	
		return;
	end

	-- Clear out the CurrentGear in case of leftovers
	gcinclude.ClearSet(sets.CurrentGear);
	
	-- Call the common HandleMidcast now
	gcinclude.HandleMidcast();

	-- Add TH gear if the target is a monster and TH enabled
	if target ~= nil and target.Type == 'Monster' and gcdisplay.GetToggle('TH') == true then
		gcinclude.MoveToCurrent(sets.TH,sets.CurrentGear);
	end

	gcinclude.EquipTheGear(sets.CurrentGear);
end		-- HandleMidcast

--[[
	HandlePreshot is similar to HandlePrecast, but for ranged actions. It loads Ranged Accuracy 
	and Ranged Shot Speed Gear for a ranged attack
--]]

function profile.HandlePreshot()
	local bTank = gcdisplay.GetToggle('Tank');

	if bTank == nil then
		bTank = false;
	end
	
	if gcdisplay.GetToggle('GSwap') == false then
		return;
	end
	
	-- Clear out the CurrentGear in case of leftovers
	gcinclude.ClearSet(sets.CurrentGear);
	
	-- Load appropriate set
	if bTank == true then
		gcinclude.MoveToCurrent(sets.Tank_Preshot,sets.CurrentGear);
	else
		gcinclude.MoveToCurrent(sets.Preshot,sets.CurrentGear);
	end	
	gcinclude.EquipTheGear(sets.CurrentGear);
end		-- HandlePreshot

--[[
	HandleMidshot is similar to HandleMidcast, but for ranged actions. It loads Ranged Attack 
	and Damage gear for a ranged attack
--]]

function profile.HandleMidshot()
	local bTank = gcdisplay.GetToggle('Tank');

	if bTank == nil then
		bTank = false;
	end
	
	-- Only gear swap if this flag is true
	if gcdisplay.GetToggle('GSwap') == false then
		return;
	end

	-- Clear out the CurrentGear in case of leftovers
	gcinclude.ClearSet(sets.CurrentGear);
	
	-- Load appropriate set
	if bTank == true then
		gcinclude.MoveToCurrent(sets.Tank_Midshot,sets.CurrentGear);
	else
		gcinclude.MoveToCurrent(sets.Midshot,sets.CurrentGear);
	end;
	gcinclude.ProgressiveAccuracy('RAcc');
	
	if gcdisplay.GetToggle('TH') == true then
		gcinclude.MoveToCurrent(sets.TH,sets.CurrentGear);
	end	
		
	-- Equip the composited Midshot set	
	gcinclude.EquipTheGear(sets.CurrentGear);
end		-- HandleMidshot

--[[
	HandleWeaponskill loads the gear appropriately for the weapon skill you're doing
--]]

function profile.HandleWeaponskill()
	local canWS = gcinclude.CheckWsBailout();
	
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
	gcinclude.fHandleWeaponskill();
	
	-- Equip the composited weaponskill set		
	gcinclude.EquipTheGear(sets.CurrentGear);
end		-- HandleWeaponskill

return profile;
