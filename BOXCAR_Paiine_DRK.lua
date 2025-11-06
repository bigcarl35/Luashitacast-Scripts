local profile = {};

local crossjobs = require('common.crossjobs');
local displaybar = require('common.displaybar');
local gear = require('common.gear');
local help = require('common.help');
local locks = require('common.locks');
local magic = require('common.magic');
local pets = require('common.pets');
local utilities = require('common.utilities');
local gcdisplay = require('common.gcdisplay');

--[[
	This file contains all the gear sets associated with the DRK job.

	Gear Sets last updated: September 26, 2025
	Code update: September 26, 2025
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
	Gear Sets are what Luashitacast equip based on actions that the code tracks. So things like
	are you fighting, casting a spell, resting, etc. Reference sets will never be directly
	equipped by Luashitacast except as subsets found in Gear Sets. For example, ['rEnmity_plus']
	is a Reference set since Luashitacast will not load it directly whereas ['TP'] is a Gear
	Set that is equipped when the player is "engaged" (fighting, weapon drawn, etc). Now, if
	you are tanking, you might want to equip enmity+ gear when you're engaged, but Luashitacast
	will not address it directly. You must include it in a gear set via the Subset command.
	(Note: the "r" prefix I included in the reference gear set is a convention I use to make
	the Reference Gear Set stand out. Whether you conform to my standard or not isstrictly up
	to you.)

	When processing a Gear Set (or Reference Gear Set), Luashitacast first processes all the
	Subsets at the current level, then the Groups, and finally the rest of the definition.
	(Levels within a gear set identify a depth in the definition.) Most gear sets only have
	one level. The exception is any that contain Groups. The level within a Group is self-
	contained. So, processing a Group is separate from the main level.

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

	A Group is treated like a mini gear set, so when a Group is processed, any Subsets within
	that group will be processed first, followed by Groups, and lastly slot definitions. So,
	in the example: first Subset 'XXX' will processed and then depending on whether TANK
	is enabled or not, the appropriate Group will be processed. Let's assume NOT_TANK is true.
	Subset 'ZZZ' will be processed next and then Group NIGHTTIME. Assuming it's night time, the
	Group doesn't have any Subsets or Groups, so the slot definition would be processed.

	If you're going to have multiple groups (like in the example) in the same gear set, it's
	important that the different group definitions do not overlap. //TANK and //NOT_TANK are
	mutually exclusive, one or the other will be true, but if you have GROUP//TANK and
	GROUP//NIGHTTIME, it's possible that neither will be equipped or both will be equipped.
	Since you can't guarentee which will be processed first, it's highly doubtful that what you
	expect to happen actually will happen. Now, if your groups contain different slots, then
	this is not a problem since you'll not have overlap. Just be conscious of this issue when
	you're defining your gear sets.
	
	Horizon changes for DRK from retail:
		- Moderate changes to artifact armor
		- Minor changes to Apocalyse
		- Significant changes to Ragnarok
		- Arcane Circle has a damage bonus against arcana for all party members as well as providing
		  protection against magic aggro. The recast of this ability has also been doubled to 10
		  minutes
		- Arcane Circle recast merits now decrease it's recast time by 20 seconds per merit instead
		  of 10
		- Desparate blows was a merit ability not available till ToAU. Now it's a job trait
		- "Souleater" has a recast of 5 minutes instead of 6
		- Souleater Recast merits now decrease it's recast by 10 seconds per merit instead of 12
		- "Last Resort"'s duration has been doubled from 30 to 60 seconds
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

	Note 2: if cursed, the parade gorget will not refesh even if MP% conditions met
--]]

	['Default'] = {
		Main   = 'Earth Staff//WSWAP',		-- -20% physical damage
		Ammo   = 'Bomb Core',
		Neck   = { 'Parade Gorget//SPECIAL//NOT_CURSED', 'Opo-opo necklace//SLEPT', 'Rep.Gold Medal//NOT_OWN',
				   'Uggalepih Pendant//NIGHTTIME', 'Fenrir\'s Torque//DAYTIME', 'Star Necklace', 'Spike Necklace' },
		Ears   = { 'Bat Earring//BLINDED', 'Coral Earring//DT_MAGICAL', 'Brutal Earring', 'Ethereal Earring',
			       'Energy Earring +1', 'Energy Earring +1' },
		Hands  = { 'Homam Manopolas', 'Abs. Gauntlets +1', 'Crimson Fng. Gnt.', 'Wonder Mitts' },
		Rings  = { 'Flame Ring', 'Kshama Ring No.8', 'Tamas Ring', 'Kshama Ring No.2' },
		Back   = { 'Forager\'s Mantle', 'Psilos Mantle', 'Amemet Mantle', 'Raptor Mantle' },
		Waist  = { 'Swift Belt', 'Powerful Rope', 'Friar\'s Rope' },
		Feet   = { 'Homam Gambieras', 'Chs. Sollerets +1', 'Bounding Boots' },
		GROUP//NOT_TANK = {
			Head   = { 'Lilac Corsage//TOWN', 'Homam Zucchetto', 'Abs. Burgeonet +1', 'Empress Hairpin' },
			Body   = { 'Ducal Aketon//TOWN-AK', 'Haubergeon', 'Chaos Cuirass', 'Brigandine' },
			Legs   = { 'Homam Cosciales', 'Chaos Flanchard', 'Tracker\'s Kecks' },
		},
		GROUP//TANK = {
			GROUP//IDLE = {
				Head   = { 'Lilac Corsage//TOWN', 'Abs. Burgeonet +1' },
				Body   = { 'Ducal Aketon//TOWN-AK', 'Chaos Cuirass', 'Brigandine' },
				Legs   = 'Chaos Flanchard',
			},
			GROUP//NOT_IDLE = {
				Head   = { 'Lilac Corsage//TOWN', 'Abs. Burgeonet +1' },
				Body   = { 'Ducal Aketon//TOWN-AK', 'Chaos Cuirass', 'Brigandine' },
				Legs   = 'Chaos Flanchard',
			},
		},
	},
	
--[[
	The TP sets are used when you are fighting.	The accuracy set will be applied in a fractional
	manner and the evasion set if /eva is specified. When tanking, Tank_TP will be equipped 
	instead of the TP set. It's a means for	the DRK to equip more defensive gear if they find
	themselves tanking. In Tank_TP, consider including shield skill+ gear.
--]]

	['TP'] = {
		SUBSET = {
			[1] = 'Default',
			[2] = { 'AttackPower//NOT_TANK', 'Enmity_Plus//TANK' }
		},
		Neck   = { 'Parade Gorget//SPECIAL//NOT_CURSED', 'Opo-opo necklace//SLEPT', 'Spike Necklace' },
		Ears   = { 'Bat Earring//BLINDED', 'Coral Earring//DT_MAGICAL', 'Brutal Earring', 'Ethereal Earring', 'Coral Earring', 'Fang Earring',
				   'Genin Earring//SJNIN', 'Pilferer\'s Earring//SJTHF', 'Drone Earring', 'Energy Earring +1', 'Energy Earring +1' },
		Hands  = { 'Homam Manopolas', 'Abs. Gauntlets +1', 'Crimson Fng. Gnt.', 'Wonder Mitts' },
		Back   = { 'Forager\'s Mantle', 'Psilos Mantle', 'Amemet Mantle', 'Raptor Mantle' },
		Feet   = { 'Homam Gambieras', 'Chs. Sollerets +1', 'Bounding Boots' },
		GROUP//NOT_TANK = {
			Head   = { 'Homam Zucchetto', 'Abs. Burgeonet +1', 'Empress Hairpin' },
			Body   = { 'Haubergeon', 'Chaos Cuirass', 'Brigandine' },
			Rings  = { 'Flame Ring', 'Kshama Ring No.8', 'Tamas Ring' },
			Waist  = { 'Swift Belt', 'Powerful Rope' },
			Legs   = { 'Homam Cosciales', 'Chaos Flanchard', 'Tracker\'s Kecks' }
		},
		GROUP//TANK = {
			Head  = 'Abs. Burgeonet +1',
			Body  = { 'Narasimha\'s Vest', 'Brigandine' },
			Rings = { 'Flame Ring', 'Bomb Queen Ring', 'Kshama Ring No.4', 'Kshama Ring No.2' },
			Waist = 'Swift Belt',
			Legs  = 'Chaos Flanchard'
		},
	},
	
--[[
	If an accuracy emphasis is desired, the following set will replace the gear, appropriately.
	Unlike TP though, accuracy is applied one slot at a time in a fractionalized manner using
	the /acc command.
	
	Include equipment with accuracy bonus and DEX. Remember, DEX converts to accuracy: (horizon) 
	for every 1 point of DEX you get 0.70 points of accuracy if wielding a 2H weapon, 0.65 for 
	a 1H weapon, and 0.60 for H2H. 
	
	Also, like the TP set, since DRK can tank, there's a Tank_Accuracy set.
--]]
		
	['Accuracy'] = {
		Ammo  = 'Orphic Egg//PJPBRD',											-- +1 Acc if BRD in party
		Head  = { 'Optical Hat', 'Homam Zucchetto', 'Empress Hairpin' },		-- +10/4 Acc, +3 DEX
		Neck  = { 'Peacock Amulet', 'Spike Necklace' },							-- +10 Acc, +3 DEX
		Ears  = 'Beastly Earring//PET',											-- +10 Pet Acc
		Body  = { 'Abs. Cuirass +1', 'Haubergeon', 'Scorpion Harness', 'Brigandine' },		-- +12/10/10 Acc, +2 DEX
		Hands = { 'Homam Manopolas', 'Abs. Gauntlets +1', 'Chaos Gauntlets' },	-- +4 Acc, +5/3 DEX
		Rings = { 'Toreador\'s Ring', 'Toreador\'s Ring', 'Woodsman Ring', 'Woodsman Ring', 'Jaeger Ring', 'Kshama Ring No.2' },	-- +7/+7/+5/+5/4/2 Acc
		Back  = { 'Abyss Cape', 'Psilos Mantle' },								-- +7/1 Acc
		Waist = { 'Life Belt', 'Tilt Belt', 'Mrc.Cpt. Belt' },					-- +10/5 Acc, +1 DEX
		Legs  = { 'Homam Cosciales', 'Chaos Flanchard', 'Ryl.Kgt. Breeches' },	-- +3 Acc, +2/2 DEX
		Feet  = { 'Homam Gambieras', 'Chs. Sollerets +1', 'Bounding Boots' },	-- +6/3 Acc, +3 DEX
  },

--[[
	Similar to the accuracy set, the Ranged_Accuracy set will be used on ranged attacks
--]]

	['Ranged_Accuracy'] = {
		Head   = 'Optical Hat',			-- +10 RAcc
		Neck   = 'Peacock Amulet',		-- +10 RAcc
		Hands  = 'Crimson Fng. Gnt.',	-- +10 RAcc
		Rings  = { 'Woodsman Ring', 'Woodsman Ring', 'Jaeger Ring' },	-- +5/5/4 RAcc
		Back   = 'Psilos Mantle',		-- +1 RAcc
		Legs   = 'Tracker\'s Kecks',	-- +4 RAcc
		Feet   = 'Homam Gambieras'		-- +6 RAcc
	},

--[[
	Progressive is a new idea for handling accuracy/ranged accuracy. You create
	stages to load accuracy gear from. Depending on what the player specifies,
	that stage and any before it will be loaded. The intent is to replace the
	Fractional Accuracy with this new system.
--]]

  ['Progressive'] = { 
		['Accuracy'] = { 
			[1] = { 							-- 32 Acc @ 75
				['Head'] = 'Accuracy::Head',
				['Neck'] = 'Accuracy::Neck',
				['Body'] = 'Accuracy::Body'
			},
			[2] = {								-- 20 Acc @ 75
				['Hands'] = 'Accuracy::Hands',
				['Back']  = 'Accuracy::Back',
				['Legs']  = 'Accuracy::Legs',
				['Feet']  = 'Accuracy::Feet'
			},
			[3] = {								-- 14 Acc @ 75
				['Rings'] = 'Accuracy::Rings',
			},
			[4] = {								-- 10 Acc @ 75
				['Subset'] = {
					[1] = 'Accuracy'
				}
			}
		},
		['Ranged_Accuracy'] = {
			[1] = {
				['Neck'] = 'Ranged_Accuracy::Neck',
				['Back'] = 'Ranged_Accuracy::Back'
			},
			[2] = { 
				['Hands'] = 'Ranged_Accuracy::Hands',
				['Legs']  = 'Ranged_Accuracy::Legs'
			},
			[3] = {
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
	
	Note: avoid coral earring (-5 eva) and fang earring (-4). There should be sufficient
	entries to not have to exclude these with entries (eg., x/IF:FANG EARRING). Also
	avoid tilt belt (-5 eva)
--]]

	['Evasion'] = {
		Main  = 'Auster\'s Staff//WSWAP',	-- +10 EVA
		Ammo  = 'Orphic Egg//PJPBRD',	-- +1 Eva
		Head  = { 'Optical Hat', 'Empress Hairpin' },	-- +10/10 Eva
		Ears  = { 'Bat Earring//BLINDED', 'Ethereal Earring', 'Reraise Earring', 'Genin Earring//SJNIN', 'Drone Earring' },		-- +15 Eva while blinded, +5/2 Eva, +4 AGI if sj NIN, +3 AGI
		Body  = 'Scorpion Harness',		-- +10 Eva
		Waist = 'Swift Belt//IF:Tilt Belt',	-- filler, Tilt Belt has -5 Eva
		Legs  = 'Chaos Flanchard',		-- +5 Eva
		Feet  = 'Bounding Boots',		-- +3 AGI
    },

--[[
	The damage taken sets are not equipped directly but rather from subsets. They're a
	way to reduce a specific types of damage. As such they're optional and up to the 
	player to decide if they should be defined and how they're used.
--]]

	['Damage_Taken_Breath'] = {
	},
	
	['Damage_Taken_Physical'] = {
		Main = 'Earth Staff//WSWAP',	-- -20% physical damage taken
	},
	
	['Damage_Taken_Magical'] = {
		Ears = 'Coral Earring',			-- -1% damage reduction from magic
	},
	
--[[
	When you are resting (kneeling down), your HP 'Resting' set will be equipped. If your MP 
	is below the set threshhold (defined by gcinclude.settings.RefreshGearMP) though, your MP 
	'Resting_Refresh' gear set will be equipped. Regardless of which set is equipped, if you
	have a Dark/Pluto staff accessible, weapon swapping has been enabled (/wswap), and your MP 
	is not at maximum, the Dark/Pluto staff will automatically be equipped.
	
	The Damage_Taken_* sets are added as a subset to reduce damage accordingly because
	you're in a vulnerable state.	
--]]
	
	['Resting_Regen'] = { 
		Subset = {
			[1] = { 
				'Damage_Taken_Breath//DT_BREATH',
				'Damage_Taken_Magical//DT_MAGICAL',
				'Damage_Taken_Physical//DT_PHYSICAL',
			}
		},	
	},
	
	['Resting_Refresh'] = {
		Subset = {
			[1] = { 
				'Damage_Taken_Breath//DT_BREATH',
				'Damage_Taken_Magical//DT_MAGICAL',
				'Damage_Taken_Physical//DT_PHYSICAL',
			}
		},	
		Main = 'Pluto\'s Staff//WSWAP',		-- +10 MP/tick
		Neck = 'Parade Gorget//SPECIAL',	-- Refresh if HP% >= 85% on visible gear
		Body = 'Plastron',					-- Refresh	
	},
	
--[[
	Start weapons are where you define what you want the first row of equipment to look 
	like when you either log in as a DRK or switch your main job to DRK. Any other gear 
	you mention will be overridden by the Idle or Town set, so no need to include here.
--]]

	['Start_Weapons'] = {
		Main = { 'Martial Sword//SJTHF', 'Y\'s Scythe' },
		Ammo = { 'Bomb Core', 'Fortune Egg' },
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
		Ears  = 'Brutal Earring',						-- Store TP +1
		Back  = { 'Psilos Mantle', 'Amemet Mantle' },	-- +12/10 RAtt
		Legs  = 'Tracker\'s Kecks',						-- +6 RAtt
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
		Main  = { 'Aquilo\'s Staff//WSWAP', 'Pluto\'s Staff//WSWAP', 'Light Staff//WSWAP' },	-- +5/2/1 INT
		Body  = 'Baron\'s Saio',		-- +1 INT
		Hands = { 'Abs. Gauntlets +1', 'Errant Cuffs' },			-- +9/5 INT
		Rings = { 'Tamas Ring', 'Kshama Ring No.5', 'Flame Ring' },	-- +5/3/2 INT
		Waist = 'Mrc.Cpt. Belt',		-- 1 INT
		Legs  = 'Chaos Flanchard',		-- +3 INT
		Feet  = 'Mannequin Pumps',		-- +1 INT
	},

	['MND'] = {
		Main  = { 'Water Staff//WSWAP', 'Pluto\'s Staff//WSWAP', 'Light Staff//WSWAP' },	-- +4/2/1 MND
		Neck  = { 'Promise Badge', 'Justice Badge' },	-- +5/3 MND
		Ears  = 'Geist Earring',						-- +1 MND
		Body  = { 'Abs. Cuirass +1', 'Wonder Kaftan', 'Baron\'s Saio' },	-- +4/1/1 MND
		Hands = 'Baron\'s Cuffs',						-- +1 MND
		Rings = { 'Tamas Ring', 'Kshama Ring No.9', 'Tranquility Ring' },	-- +5/3/2 MND
		Waist = { 'Mrc.Cpt. Belt', 'Friar\'s Rope' },			-- +1/1 MND
		Legs  = { 'Abs. Flanchard +1', 'Wonder Braccae' },		-- +5/2 MND
		Feet  = { 'Chs. Sollerets +1', 'Mannequin Pumps' },		-- +5/2 MND
	},
	
	['Enmity_Plus'] = {
		Hands = 'Homam Manopolas',	-- +3 Enmity
		Waist = 'Warwolf Belt',		-- +3 Enmity
	},
	
	['Enmity_Minus'] = {
		Ammo  = 'Hedgehog Bomb',				-- -1 Enmity
		Neck  = 'Fenrir\'s Torque//NIGHTTIME',	-- -3 enmity at night
		Rings = 'Tamas Ring',					-- -5 Enmity
	},
	
--[[
	Spells are a bit different. Each type of spell can have it's own enhancement gear as well as 
	stat based gear. (In some cases individual spells have special entries.) These sets do not 
	include elemental gear which is	dependent on day/weather/weapon skill.

	The first stage is Precast. This is where you place any Fast Cast, cast time reduction, quick 
	cast gear, and spell interruption rate
--]]

	['Precast'] = {	
		Ears = 'Loquac. Earring',	-- Enhances Fast Cast
		Legs = 'Homam Cosciales',	-- Enhances Fast Cast
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
		Subset = {
			[1] = { 'Dark_Magic_Skill//DARK', 
				   'Elemental_Magic_Skill//ELEMENTAL',
				   'Enfeebling_Magic_Skill//ENFEEBLING',
				   'Healing_Magic_Skill//HEALING',		-- Offensive healing only
				   'Divine_Magic_Skill//DIVINE',
				   'Ninjutsu_Skill//NINJUTSU',
				   'CHR//SINGING',		-- Charisma provides accuracy w/singing
				   },
		},
		Head  = 'Homam Zucchetto',	-- +4 Macc
		Rings = 'Tamas Ring',		-- +5 Macc
		Back  = 'Abyss Cape',		-- +7 Macc
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
	main job supports tanking (like DRK does.) I've included details on
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

	['Healing_Magic_Skill'] = {
	},
	
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
	
	Note: Nothing is done in a vacuum. Check MND when defining Cure magic.
	
--]]	
	
	['CuringMagic'] = {
		Subset = {
			[1] = 'Healing_Magic_Skill',
			[2] = 'MND',
		},
		Ammo   = 'Fortune Egg',				-- +1 VIT
		Head   = 'Abs. Burgeonet +1',		-- +8 VIT
		Body   = { 'Narasimha\'s Vest//EMPTY', 'Brigandine//EMPTY', 'Wonder Kaftan//EMPTY' },	-- +3/2/2 VIT
		Rings  = 'Kshama Ring No.4//EMPTY',	-- +3 VIT
		Waist  = 'Warwolf Belt',			-- +5 VIT
		Legs   = 'Wonder Braccae//EMPTY',	-- +2 VIT
		Feet   = 'Creek F Clomps//EMPTY',	-- +4 VIT
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
		Subset = {
			[1] = 'CuringMagic',
		}
	},

--[[
	This set is used for all non-cure Healing Magic spells. Only 
	healing magic skill is of any importance here. You might want 
	to use this set as a subset for the other cure-based sets.
--]]

	['HealingMagic'] = {
		Subset = {
			[1] = 'Healing_Magic_Skill',
		}
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

	['Enhancing_Magic_Skill'] = {	-- This affects potency (where applicable)
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
		Subset = {
			[1] = 'Enhancing_Magic_Skill',
		}
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
		Subset = {
			[1] = 'Enhancing_Magic_Skill',
		}
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
	Ice/Shock spikes: integer(intege,r(((INT+10)/20) + 2) * (1 + (MAB/100)))
--]]
	
	['Spike'] = {
		Subset = {
			[1] = 'INT',
		},
		Neck   = 'Uggalepih Pendant//SPECIAL',	-- +8 MAB when MP% < 51%
		Body   = 'Abs. Cuirass +1',				-- +10 MAB
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
		Subset = {
			[1] = 'MND',
		}
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
		Subset = {
			[1] = 'Enhancing_Magic_Skill',
		}
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
		Subset = {
			[1] = 'Enhancing_Magic_Skill',
		}		
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
		Subset = {
			[1] = 'INT',
		},
		Neck   = 'Uggalepih Pendant//SPECIAL',	-- +8 MAB when MP% < 51%
		Body   = 'Abs. Cuirass +1',				-- +10 MAB
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
		Subset = {
			[1] = 'INT',
		}
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
		Subset = {
			[1] = 'Summoning_Skill',
		},
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
		Head  = 'Chaos Burgeonet',		-- +5 Dark Magic Skill
		Hands = 'Crimson Fng. Gnt.',	-- +10 Dark Magic Skill
		Legs  = 'Abs. Flanchard +1'		-- +7 Dark Magic Skill
	},
	
--[[
	There's 9 absorb spells (although some are currently out of era). If not
	resisted, they drain a specific stat from the target based on the caster's
	level:
	
		base absorbed = floor (3 + (job level) / 5)
	
	Dark magic skill has no effect on absorb spell, but does affect accuracy.
	Absorb spells resisted will have their duration cut in half or be completely
	resisted. Equipment that "Enhances" an absorb spell will increase the spells
	duration. Equipment that "Augments" an absorb spell will increase the spells
	potency.
--]]
	
	['Absorb'] = {
		Legs = 'Black Cuisses',
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
		Subset = {
			[1] = 'Dark_Magic_Skill',
		}
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
		Subset = {
			[1] = 'Dark_Magic_Skill',
		}
	},

--[[
	This last gear set, DarkMagic, covers all Dark Magic spells not covered
	by the previous three gear sets. 
--]]

	['DarkMagic'] = {
		Subset = {
			[1] = 'Dark_Magic_Skill',
		}		
	},
	
--[[
	Currently Dread Spikes are out of era, but they're introduced in ToAU,
	so I've included them here. At the moment the code only applies a generic
	spell invocation.
--]]
	
--	['Dread'] = {
--	},
	
--	['Tank_Dread'] = {
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
			[2] = 'MND'
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
		Subset = {
			[1] = 'Divine_Magic_Skill'
		}
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
		Subset = {
			[1] = 'Divine_Magic_Skill'
		}
	},

--[[
	*****************************
	* Midcast: Enfeebling Magic *
	****************************
--]]
	
	['Enfeebling_Magic_Skill'] = {
		Neck   = 'Enfeebling Torque',	-- +7 Enfeebling Magic Skill
		Body   = 'Chaos Cuirass',		-- +5 Enfeebling Magic Skill
		Feet   = 'Abyss Sollerets',		-- +5 Enfeebling Magic Skill	
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
	dependent on MND (paralyze, silence, slow, slowga, frazzle, distract,
	dia, and diaga).
	
	After the appropriate gear set is equipped, an elemental obi might be
	equipped (for day/weather effect) and an elemental staff (for magic
	affinity.)
--]]
	
	['EnfeeblingINT'] = {
		Subset = { 
			[1] = 'Enfeebling_Magic_Skill',
			[2] = 'INT',
		}
	},
	
	['EnfeeblingMND'] = {
		Subset = {
			[1] = 'Enfeebling_Magic_Skill',
			[2] = 'MND',
		}
	},

	['EnfeeblingMagic'] = {
		Subset = {
			[1] = 'Enfeebling_Magic_Skill',
		}
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

	['CHR'] = {		-- Charisma provides accuracy w/singing
		Main  = 'Pluto\'s Staff//WSWAP',	-- +2 CHR
		Ears  = 'Beastly Earring',			-- +2 CHR	
		Waist = 'Mrc.Cpt. Belt',			-- +1 CHR
	},
	
--[[
	EnhancementSinging contains gear that enhances party members is some specific
	manner. Included are: minne, minuet, paeon, pastoral, madigal, mambo, etude,
	operetta, ballad, march, prelude, aubade, carol, mazurka, gavotte, capriccio,
	fantasia, hymnus, and round.
--]]

	['EnhancementSinging'] = {
		Subset = {
			[1] = 'CHR',	
		}
	},

--[[
	EnfeeblingSinging contains gear that debuffs targets. Included are: requiem,
	threnody, lullaby, finale, elegy, and virelai.
--]]
	
	['EnfeeblingSinging'] = {
		Subset = {
			[1] = 'CHR',	
		}
	},

--[[
	********************
	* Midcast: Ninjusu *
	********************
--]]

	['Ninjutsu_Skill'] = {
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
		Subset = {
			[1] = 'Ninjutsu_Skill',
		},
		Ears = 'Stealth Earring',		-- +5  Ninjutsu Skill
	},

-- An elemental stave will be checked for after the debuff set is loaded.
	
	['NinjutsuDebuff'] = {
		Subset = {
			[1] = 'Ninjutsu_Skill',
		}
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
		}
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
	
	DRK can use the following weapons: scythe (A+), great sword (A-), axe (B-), great axe (B-), sword (B-), 
	dagger (C), club(C-), marksmanship (E). 
		
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
		Ammo  = 'Bomb Core',						-- +12 Att
		Head  = 'Abs. Burgeonet +1',				-- +12 Att
		Ears  = { 'Ethereal Earring', 'Coral Earring', 'Fang Earring', 'Brutal Earring' },	-- +5/5/4 Att, store TP +1
		Body  = { 'Plastron', 'Haubergeon', 'Chaos Cuirass' },	-- +16/10/5 Att
		Rings = 'Kshama Ring No.8',					-- +3 Att
		Back  = { 'Forager\'s Mantle', 'Psilos Mantle', 'Amemet Mantle' },	-- +15/12/10 Att
		Waist = 'Powerful Rope//IF:SWIFT BELT',
		Legs  = { 'Black Cuisses', 'Tracker\'s Kecks' },	-- +14/6 Att
	},
	
--[[
		* Strength based *
		
		Scythe: Slice,Spinning Scythe,Vorpal Scythe
		Great Sword: Hard Slash,Crescent Moon
		Axe: Raging Axe,Smash Axe,Gale Axe,Avalanche Axe,Spinning Axe,Rampage,Decimation
		Great Axe: Iron Tempest,Sturmwind,Keen Edge,Raging Rush
		Sword: Flat Blade,Circle Blade,Spirits Within,Vorpal Blade
		Club: Brainshaker,Skullbreaker,True Strike		
-]]
	
	['WS_STR'] = {
		Subset = {
			[1] = 'AttackPower',
		},
		Head   = 'Chaos Burgeonet',			-- +4 STR
		Neck   = 'Spike Necklace',			-- +3 STR
		Body   = { 'Plastron', 'Haubergeon', 'Chaos Cuirass', 'Wonder Kaftan' },	-- +8/5/3/1 STR
		Hands  = 'Wonder Mitts',			-- +3 STR
		Rings  = { 'Flame Ring', 'Kshama Ring No.8' },	-- +5/3 STR
		Back   = { 'Forager\'s Mantle', 'Amemet Mantle//EMPTY' },		-- +3/1 STR
		Waist  = { 'Warwolf Belt', 'Mrc.Cpt. Belt' },	-- +5/1 STR
		Legs   = { 'Black Cuisses', 'Ryl.Kgt. Breeches', 'Wonder Braccae//EMPTY' },	-- +4/2/1 STR
		Feet   = { 'Chs. Sollerets +1', 'Creek F Clomps', 'Wonder Clomps' },	-- +5/4/2 STR
  },
	
--[[
		* Strength and Agility based, even weighting *
		
		Great Sword: Sickle Moon
--]]

	['WS_STRAGI'] = {
		Subset = {
			[1] = 'AttackPower',
		},	
		Head   = { 'Chaos Burgeonet', 'Empress Hairpin' },			-- +4 STR, +3 AGI
		Neck   = 'Spike Necklace',		-- +3 STR
		Ears   = { 'Genin Earring//SJNIN', 'Drone Earring' },		-- +4/3 AGI
		Body   = { 'Plastron', 'Chaos Cuirass', 'Wonder Kaftan' },	-- +8/3/1 STR
		Hands  = 'Wonder Mitts',			-- +3 STR
		Rings  = { 'Flame Ring', 'Kshama Ring No.8', 'Kshama Ring No.3' },	-- +5/3, +3 AGI
		Back   = { 'Forager\'s Mantle', 'Amemet Mantle//EMPTY' },	-- +3/1 STR
		Waist  = { 'Warwolf Belt', 'Mrc.Cpt. Belt' },				-- +5/1 STR
		Legs   = { 'Black Cuisses', 'Ryl.Kgt. Breeches', 'Wonder Braccae//EMPTY' },	-- +/42/1 STR
		Feet   = { 'Chs. Sollerets +1', 'Creek F Clomps', 'Bounding Boots' },	-- +5/4 STR, +3 AGI
	},
		
--[[
		* Strength and Dexterity based, even weighting *
		
		Sword: Fast Blade
--]]

	['WS_STRDEX'] = {
		Subset = {
			[1] = 'AttackPower',
		},
		Head   = { 'Chaos Burgeonet', 'Empress Hairpin' },		-- +4 STR, +3 DEX
		Neck   = 'Spike Necklace',		-- +3 STR
		Body   = { 'Haubergeon', 'Plastron', 'Chaos Cuirass', 'Brigandine', 'Wonder Kaftan' },	-- +5 STR/+5 DEX, +8/3 STR, +2 DEX, +1 STR
		Hands  = { 'Abs. Gauntlets +1', 'Chaos Gauntlets', 'Wonder Mitts' },	-- +5/3 DEX, +3 STR
		Rings  = { 'Flame Ring', 'Kshama Ring No.8', 'Kshama Ring No.2' },	-- +5, +3 DEX, +3 STR
		Back   = { 'Forager\'s Mantle', 'Amemet Mantle//EMPTY' },	-- +3/1 STR
		Waist  = { 'Warwolf Belt', 'Mrc.Cpt. Belt' },				-- +5/1 STR
		Legs   = { 'Black Cuisses', 'Ryl.Kgt. Breeches', 'Chaos Flanchard', 'Wonder Braccae' },		-- +4 STR, 2 STR/+2 DEX, +1 STR
		Feet   = { 'Chs. Sollerets +1', 'Creek F Clomps', 'Bounding Boots' },	-- +5/4 STR, +3 DEX
	},
	
--[[
		* Strength and Intelligence based, even weighting *
		
		Scythe: Dark Harvest,Shadow of Death,Nightmare Scythe,Spiral Hell, Catastrophe
		Great Sword: Frostbite,Freezebite,Spinning Slash,Ground Strike
		Sword: Burning Blade
--]]
	
	['WS_STRINT'] = {
		Subset = {
			[1] = 'AttackPower',
		},
		Head   = 'Chaos Burgeonet',		-- +4 STR 
		Neck   = 'Spike Necklace',		-- +3 STR
		Body   = { 'Plastron', 'Haubergeon', 'Chaos Cuirass', 'Wonder Kaftan', 'Baron\'s Saio' },	-- +8/5/3/3 STR, +1 INT
		Hands  = { 'Abs. Gauntlets +1', 'Wonder Mitts' },	-- +9 INT, +3 STR
		Rings  = { 'Flame Ring', 'Tamas Ring', 'Kshama Ring No.8', 'Kshama Ring No.5' },	-- +5 STR, +5 INT, +3 STR, +3 INT
		Back   = { 'Forager\'s Mantle', 'Amemet Mantle//EMPTY' },	-- +3/1 STR
		Waist  = { 'Warwolf Belt', 'Mrc.Cpt. Belt' },	-- +5 STR, +1 STR/+1 INT
		Legs   = { 'Black Cuisses', 'Ryl.Kgt. Breeches', 'Wonder Braccae//EMPTY' },	-- +4/2/1 STR
		Feet   = { 'Chs. Sollerets +1', 'Creek F Clomps', 'Wonder Clomps' },	-- +5/4/2 STR
	},
	
--[[
		* Strength and Intelligence based, 30%/20% respectively *
		
		Sword: Red Lotus Blade
--]]
	
	['WS_STRINT_30_20'] = {
		Subset = {
			[1] = 'AttackPower',
		},
		Head   = 'Chaos Burgeonet',		-- +4 STR 
        Neck   = 'Spike Necklace',		-- +3 STR 
        Body   = { 'Plastron', 'Haubergeon', 'Chaos Cuirass', 'Wonder Kaftan', 'Baron\'s Saio' },	-- +8/5/3/3 STR, +1 INT
        Hands  = { 'Abs. Gauntlets +1', 'Wonder Mitts' },	-- +9 INT, +3 STR
        Rings  = { 'Flame Ring', 'Tamas Ring', 'Kshama Ring No.8', 'Kshama Ring No.5' },		-- +5 STR, +5 INT, +3 STR, +3 INT
        Back   = { 'Forager\'s Mantle', 'Amemet Mantle//EMPTY' },	-- +3/1 STR
		Waist  = { 'Warwolf Belt', 'Mrc.Cpt. Belt' },	-- +5 STR, +1 STR/+1 INT
        Legs   = { 'Black Cuisses', 'Ryl.Kgt. Breeches', 'Wonder Braccae//EMPTY' },	-- +4/2/1 STR
        Feet   = { 'Chs. Sollerets +1', 'Creek F Clomps', 'Wonder Clomps' },		-- +5/4/2 STR
    },
	
--[[
		* Strength and Mind based, even weighting *
		
		Scythe: Guillotine,Cross Reaper
		Great Sword: Shockwave
		Sword: Shining Blade,Seraph Blade
		Club: Shining Strike,Seraph Strike,Judgement
--]]

	['WS_STRMND'] = {
		Subset = {
			[1] = 'AttackPower',
		},
		Head   = 'Chaos Burgeonet',		-- +4 STR 
        Neck   = { 'Promise Badge', 'Justice Badge' },	-- +5 MND, +3 MND
		Ears   = 'Geist Earring',		-- +1 MND
		Body   = { 'Plastron', 'Haubergeon', 'Chaos Cuirass', 'Wonder Kaftan', 'Baron\'s Saio' },	-- +8/5 STR, +3/1 STR, +1 MND
        Hands  = { 'Wonder Mitts', 'Baron\'s Cuffs//EMPTY' },		-- +3 STR, +1 MND
        Rings  = { 'Tamas Ring', 'Kshama Ring No.8', 'Kshama Ring No.9', 'Flame Ring' }, 	-- +5 MND, +3 MND, +3 STR +5 STR/-2 MND
        Back   = { 'Forager\'s Mantle', 'Amemet Mantle//EMPTY' },	-- +3/1 STR
		Waist  = { 'Warwolf Belt', 'Mrc.Cpt. Belt', 'Friar\'s Rope' },	-- +5 STR, +1 STR/+1 MND, +1 MND
        Legs   = { 'Abs. Flanchard +1', 'Black Cuisses', 'Ryl.Kgt. Breeches', 'Wonder Braccae' },	-- +5 MND, +4/2/1 STR, +2 MND
        Feet   = { 'Chs. Sollerets +1', 'Creek F Clomps', 'Wonder Clomps' },		-- +5 STR/+5 MND, +4/2 STR
    },
	
--[[
		* Strength and Vitality based, even weighting *
		
		Great Sword: Power Slash,Scourge
		Great Axe: Shield Break,Armor Break,Weapon Break,Steel Cyclone
		Sword: Shining Blade,Seraph Blade,Swift Blade,Savage Blade
--]]
	
	['WS_STRVIT'] = {
		Subset = {
			[1] = 'AttackPower',
		},
		Ammo   = 'Happy Egg',									-- +1 VIT
		Head   = { 'Abs. Burgeonet +1', 'Chaos Burgeonet' },	-- +8 VIT, +4 STR 
        Neck   = 'Spike Necklace',		-- +3 STR
		Body   = { 'Plastron', 'Haubergeon', 'Chaos Cuirass', 'Brigandine', 'Wonder Kaftan' },	-- +8/5 STR, +6 STR/+3 VIT, +2 VIT/+1 STR, +2/1 STR
        Hands  = 'Wonder Mitts',		-- +3 STR
        Rings  = { 'Flame Ring', 'Kshama Ring No.4', 'Kshama Ring No.8' },	-- +5 STR, +3 VIT, +3 STR
        Back   = { 'Forager\'s Mantle', 'Amemet Mantle//EMPTY' },							-- +3/1 STR
		Waist  = { 'Warwolf Belt', 'Mrc.Cpt. Belt' },	-- +5 STR/+5 VIT, +1 STR/+1 VIT
        Legs   = { 'Black Cuisses', 'Ryl.Kgt. Breeches', 'Wonder Braccae' },	-- +/22 STR, +2 VIT
        Feet   = { 'Creek F Clomps', 'Wonder Clomps' },	-- +4 STR/+4 VIT, +2 STR
    },
	
--[[
		* Agility based, ranged *
		
		Marksmanship: Hot Shot^,Split Shot^,Sniper Shot^,Slug Shot^
		
		^ RNG must be subjob
--]]

	['WS_RANGED_AGI'] = {
		Subset = {
			[1] = 'AttackPower',
		},
		Head   = 'Empress Hairpin',		-- +3 AGI
		Ears   = { 'Genin Earring//SJNIN', 'Drone Earring' },	-- +4/3 AGI
		Rings  = 'Kshama Ring No.3',	-- +3 AGI
		Waist  = 'Mrc.Cpt. Belt',		-- +1 AGI
		Feet   = 'Bounding Boots',		-- +3 AGI
	},
	
--[[
		* Charisma based *
		
		Dagger: Shadowstitch
--]]
	
	['WS_CHR'] = {
		Subset = {
			[1] = 'AttackPower',
			[2] = 'CHR'
		}
    },
		
--[[
		* Dexterity based *
		
		Dagger: Wasp Sting,Viper Bite^

		^ Subjob must be one of: RDM,THF,BRD,RNG,NIN
--]]
	
	['WS_DEX'] = {
		Subset = {
			[1] = 'AttackPower',
		},
        Head   = 'Empress Hairpin',	-- +3 DEX
        Neck   = 'Spike Necklace',	-- +3 DEX
		Body   = { 'Haubergeon', 'Brigandine' },					-- +5/2 DEX
		Hands  = { 'Abs. Gauntlets +1', 'Chaos Gauntlets' },		-- +5/3 DEX
        Rings  = { 'Kshama Ring No.2', 'Balance Ring' },			-- +3/2 DEX
        Waist  = { 'Warwolf Belt',  'Mrc.Cpt. Belt' },				-- +5/1 DEX
        Legs   = { 'Chaos Flanchard', 'Ryl.Kgt. Breeches' },		-- +2/2 DEX
		Feet   = 'Bounding Boots',	-- +3 DEX
    },
	
--[[
		* Dexterity and Intelligence based *
		
		Dagger: Gust Slash
--]]
	
	['WS_DEXINT'] = {
		Subset = {
			[1] = 'AttackPower',
		},
        Head   = 'Empress Hairpin',	-- +3 DEX
        Neck   = 'Spike Necklace',	-- +3 DEX
		Body   = { 'Haubergeon', 'Brigandine' },		-- +5/2 DEX
		Hands  = { 'Abs. Gauntlets +1', 'Chaos Gauntlets' },	-- +9 INT/+5 DEX, +3 DEX
        Rings  = { 'Tamas Ring', 'Kshama Ring No.2', 'Kshama Ring No.5', 'Balance Ring' },	-- +5 INT, +3 DEX, +3 INT, +2 DEX
		Legs   = { 'Chaos Flanchard', 'Ryl.Kgt. Breeches' },	-- +3 INT/+2 DEX, +2 DEX
        Waist  = { 'Warwolf Belt', 'Mrc.Cpt. Belt' },			-- +5/1 DEX
        Feet   = 'Bounding Boots',	-- +3 DEX
    },
	
--[[
		* Mind based *
		
		Dagger: Energy Steal, Energy Drain
--]]

	['WS_MND'] = {
		Subset = {
			[1] = 'AttackPower',
			[2] = 'MND'
		}
    },
	
--[[
		* Skill based *
		
		Club: Starlight,Moonlight
--]]

	['WS_Skill'] = {
		Subset = {
			[1] = 'AttackPower',
		},
		Ears   = 'Beastly Earring//AXE',	-- +5 Axe Weapon Skill
		Back   = 'Abyss Cape',				-- +7 Weapons Skill
    },
	
--[[
		* HP based *
		
		Sword: Spirits Within
--]]

	['WS_HP'] = {
		Subset = {
			[1] = 'AttackPower',
		},
		Ammo   = 'Happy Egg',								-- +1% HP
		Head   = { 'Abs. Burgeonet +1', 'Chaos Burgeonet', 'Lilac Corsage//IF:Reraise Hairpin',  'Lilac Corsage//IF:Empress Hairpin', 'Lilac Corsage//IF:Silver Hairpin +1' },	-- +30/12 HP, the rest removes -HP gear
		Neck   = 'Promise Badge',		-- +10 HP
		Ears   = { 'Physical Earring', 'Ethereal Earring' },									-- Convert 25 MP to HP, +15 HP
		Body   = 'Wonder Kaftan',							-- 36 HP
		Hands  = { 'Homam Manopolas', 'Crimson Fng. Gnt.', 'Wonder Mitts' },					-- +20/20/12 HP
		Rings  = { 'Bomb Queen Ring', 'Toreador\'s Ring', 'Toreador\'s Ring' },					-- +75/10/10 HP
		Waist  = 'Powerful Rope',							-- +20 HP
		Legs   = { 'Homam Cosciales', 'Wonder Braccae' },	-- +26/21 HP
		Feet   = { 'Creek F Clomps', 'Wonder Clomps' },		-- +35/20 HP
  },
		
--[[
	Kite is used for kiting. Emphasis should be placed on gear that increases 
	movement speed, but you might also want gear that has evasion. The choice
	is yours.
--]]

	['Kite'] = { 
		Body  = 'Ducal Aketon//TOWN-AK',				-- In case someone uses /kite in town
	},
	
--[[
	The following are abilities affected by gear
--]]

	['Blood Weapon'] = {
	},
	
	['ArcaneCircle'] = {
		Feet = 'Chs. Sollerets +1'		-- Enhances Arcane Circle
    },
	
	['LastResort'] = {
		Feet = 'Abyss Sollerets'		-- Enhances Last Resort
    },
	
	['Souleater'] = {
		Head = 'Chaos Burgeonet'		-- Enhances Souleater
    },
	
	['WeaponBash'] = {
		Hands = 'Chaos Gauntlets'		-- Enhances Weapon Bash
    },
	
--[[
	Some subjobs really make no sense when combined with paladin, but all abilities across all jobs that
	have gear that can be equipped by a DRK are included here.
--]]
	
	--* /BST *--, CHR gear.
	['Charm'] = {
		Subset = {
			[1] = 'CHR',
		}
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
		Head  = 'Empress Hairpin',							-- +3 DEX
		Neck  = { 'Spike Necklace', 'Opo-opo Necklace' },	-- +3/3 DEX
		Body  = 'Brigandine',								-- +2 DEX
		Hands = { 'Abs. Gauntlets +1', 'Chaos Gauntlets' },	-- +5/3 DEX
		Rings = { 'Kshama Ring No.2', 'Balance Ring' },		-- +3/2 DEX
		Waist = { 'Warwolf Belt', 'Mrc.Cpt. Belt' },		-- +5/1 DEX
		Legs  = { 'Chaos Flanchard', 'Ryl.Kgt. Breeches' },	-- +2/2 DEX
		Feet  = 'Bounding Boots',							-- +3 DEX
	},
	
	['Flee'] = {
	},
	
	['TrickAttack'] = {
		Head  = 'Empress Hairpin',							-- +3 AGI
		Ears  = { 'Genin Earring//SJNIN', 'Drone Earring' },-- +4 AGI if sj NIN, +3 AGI
		Rings = 'Kshama Ring No.3',							-- +3 AGI
		Waist = 'Mrc.Cpt. Belt',							-- +1 AGI
		Feet  = 'Bounding Boots',							-- +3 AGI
	},
	
	['SATA'] = {
		Head = 'Empress Hairpin',							-- +3 DEX/+3 AGI
		Neck = 'Spike Necklace',							-- +3 DEX
		Ears = { 'Genin Earring//SJNIN', 'Drone Earring' },	-- +4 AGI if sj NIN, +3 AGI
		Body = 'Brigandine',								-- +2 DEX
		Hands = 'Abs. Gauntlets +1',						-- +5 DEX
		Rings = { 'Kshama Ring No.2', 'Kshama Ring No.3', 'Balance Ring' },	-- +3 DEX, +3 AGI, +2 DEX
		Waist = { 'Warwolf Belt', 'Mrc.Cpt. Belt' },		-- +5 DEX, +1 DEX/+1 AGI
		Legs  = { 'Chaos Flanchard', 'Ryl.Kgt. Breeches' },	-- +2/2 DEX
		Feet = 'Bounding Boots',							-- +3 DEX/+3 AGI	
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
	['SCYTHE']   = { 'Y\'s Scythe','Suzaku\'s Scythe', 'Raven Scythe', 'Relic Scythe' },
	['DAGGER']   = { 'Heart Snatcher', 'Garuda\'s Dagger', 'Decurion\'s Dagger' },
	['SWORD']    = { 'Ifrit\'s Blade', 'Bee Spatha' },
	['GSWORD']   = { 'Balmung', 'Martial Sword', 'Skofnung' },
	['H2H']      = { 'Lgn. Knuckles' },
	['AXE']      = { 'Maneater', 'Tabarzin', 'Fransisca' },
	['GAXE']     = { 'Byakko\'s Axe', 'Rampager', 'Horror Voulge', 'Neckchopper' },
	['ARCHERY']  = { 'Expunger', 'Power Bow +1' },
	['THROWING'] = { 'Rogetsurin' },
	['CLUB']     = { 'Warp Cudgel' },
	['STAVE']    = { 'Fire Staff', 'Vulcan\'s Staff', 'Ice Staff', 'Aquilo\'s Staff',
					 'Wind Staff', 'Auster\'s Staff', 'Earth Staff', 'Terra\'s Staff',
					 'Thunder Staff', 'Jupiter\'s Staff', 'Water Staff', 'Neptune\'s Staff',
					 'Light Staff', 'Apollo\'s Staff', 'Dark Staff', 'Pluto\'s Staff' },	
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
	-- The second section can be modified by the player
	priorityEngaged = 'CE';				-- Priority order for "Engaged" in HandleDefault
	priorityWeaponSkill = 'ADBE';		-- Priority order for "WS" in HandleWeaponskill
	bPrioityRefresh = false;			-- priority setting. If true, Refresh over Regen. False inverts
	FavoredJugPet = nil;				-- BST only, leave nil
	DefaultPetFood = nil;				-- What pet food to automatically equip
	-- Macro book/page
	bAutoMacrobook_page = true;			-- Should macro book/page be automatically assigned
	bJustMacroBook = false;				-- Should only the macro book be automatically assigned
	MacroBook = 9;						-- Which macro book should be equipped for DRK
};

-- Table of gear to put a delay on
profile.TrackedGear = {
	[1] = { ['item'] = 'Vermillion Cloak', ['slot'] = 'Body', ['delay'] = 10 },
};

profile.Sets = sets;
profile.sjb = nil;
profile.bAmmo = false;		-- /BST specific. Is ammo equipped?
profile.sAmmo = nil;		-- /BST specific. Name of ammo equipped

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
		['WAR'] = 1, ['MNK'] = 0, ['WHM'] = 1, ['BLM'] = 0, ['RDM'] = 0, ['THF'] = 3,
		['PLD'] = 0, ['DRK'] = 0, ['BST'] = 0, ['BRD'] = 0, ['RNG'] = 0, ['SMN'] = 0,
		['SAM'] = 4, ['NIN'] = 1, ['DRG'] = 1, ['BLU'] = 0, ['COR'] = 0, ['PUP'] = 0,
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
	profile.sjb = chkSJ;
end		-- SetSubjobSet

--[[
	OnLoad is run whenever you log into your BST or change your job to BST
--]]

function profile.OnLoad()
	local player = gData.GetPlayer();

	gSettings.AllowAddSet = true;
	gcinclude.Initialize();
	gcdisplay.SetToggle('Tank',false);		-- Assume DRK is not a tank

	-- Coded order of operation override
	gcinclude.settings.priorityEngaged = 'CEF';
	gcinclude.settings.priorityWeaponSkill = 'ADBE';
	
	-- Set which macro book should be displayed. Which macro set within the macro book to
	-- display depends on what your subjob is.
	if profile.settings.bAutoMacrobook_page == true then
		AshitaCore:GetChatManager():QueueCommand(1, '/macro book ' .. tostring(profile.settings.MacroBook));		-- DRK macro book
		if profile.settings.bJustMacroBook == false then
			SetSubjobSet(player.SubJob);
		end
	end
	
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
	their pet (if they have one).
--]]

function profile.HandleDefault()
	local pet = gData.GetPet();
	local petAction = gData.GetPetAction();		
	local player = gData.GetPlayer();
	local zone = gData.GetEnvironment();
	local ew = gData.GetEquipment();
	local bTank = gcdisplay.GetToggle('Tank');
	local bSA = gcinclude.fBuffed('Sneak Attack');
	local bTA = gcinclude.fBuffed('Trick Attack');
	local eWeap = nil;
	local cKey;

	gcinclude.StartReminder();		-- See if reminder should be printed
	
	-- A /bst charmed pet action takes priority over a player's action.
	if petAction ~= nil and player.SubJob == 'BST' then
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
	displaybar.UpdateBarStatic();

	-- Only gear swap if this flag is true
	if gcdisplay.GetToggle('GSwap') == false then
		return;
	end

	-- Assuming you're /bst, when you want to reward your pet and you do not have pet food 
	-- equipped or when you want to summon a pet and a jug is not equipped, the current item 
	-- in the ammo slot is saved. The following will set it back to what you had before 
	-- either of those two items were equipped.
	if player.SubJob == 'BST' and profile.bAmmo then
		sets.CurrentGear['Ammo'] = profile.sAmmo;
		profile.sAmmo = nil;
		profile.bAmmo = false;
	end
	
	-- Clear out the CurrentGear in case of leftovers
	gcinclude.ClearSet(sets.CurrentGear);
	
	-- If player is not resting and has MP and has swapped weapons, set the weapon back to what 
	-- they had before the switch
	if player.Status ~= 'Resting' and 
			gcinclude.weapon ~= nil and
			gcdisplay.GetToggle('WSwap') == true and 
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
		end
	elseif player.Status == 'Resting' then
		-- Player kneeling. Priority (low to high): regen,refresh
		if player.HP < player.MaxHP then		
			gcinclude.MoveToCurrent(sets.Resting_Regen,sets.CurrentGear);
		end
		
		if player.MP < player.MaxMP then
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
	displaybar.UpdateBarStatic();
end		-- HandleDefault

--[[
	HandleAbility is used to change the player's gear appropriately.
--]]

function profile.HandleAbility()
	local ability = gData.GetAction();
			
	-- Only gear swap if this flag is true
	if gcdisplay.GetToggle('GSwap') == false then
		return;
	end

	-- Clear out the CurrentGear in case of leftovers
	gcinclude.ClearSet(sets.CurrentGear);
	
	-- Now process the appropriate job ability. Start with abilities associated with DRK
	if string.match(ability.Name, 'Blood Weapon') then
		gcinclude.MoveToCurrent(sets.BloodWeapon,sets.CurrentGear);
	elseif string.match(ability.Name, 'Arcane Circle') then
		gcinclude.MoveToCurrent(sets.ArcaneCircle,sets.CurrentGear);	
	elseif string.match(ability.Name, 'Last Resort') then
		gcinclude.MoveToCurrent(sets.LastResort,sets.CurrentGear);
	elseif string.match(ability.Name, 'Souleater') then
		gcinclude.MoveToCurrent(sets.Souleater,sets.CurrentGear);
	elseif string.match(ability.Name, 'WeaponBash') then
		gcinclude.MoveToCurrent(sets.WeaponBash,sets.CurrentGear);
	
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
	-- /MNK
	elseif string.match(ability.Name, 'Boost') then
		gcinclude.MoveToCurrent(sets.Boost,sets.CurrentGear);
	elseif string.match(ability.Name, 'Focus') then
		gcinclude.MoveToCurrent(sets.Focus,sets.CurrentGear);
	elseif string.match(ability.Name, 'Dodge') then
		gcinclude.MoveToCurrent(sets.Dodge,sets.CurrentGear);
	elseif string.match(ability.Name, 'Chakra') then
		gcinclude.MoveToCurrent(sets.Chakra,sets.CurrentGear);
	-- /THF
	elseif string.match(ability.Name, 'Steal') then
		gcinclude.MoveToCurrent(sets.Steal,sets.CurrentGear);
	elseif string.match(ability.Name, 'Sneak Attack') then
		gcinclude.MoveToCurrent(sets.SneakAttack,sets.CurrentGear);
	elseif string.match(ability.Name, 'Flee') then
		gcinclude.MoveToCurrent(sets.Flee,sets.CurrentGear);
	elseif string.match(ability.Name, 'Trick Attack') then
		gcinclude.MoveToCurrent(sets.TrickAttack,sets.CurrentGear);
	elseif string.match(ability.Name, 'Mug') then
		gcinclude.MoveToCurrent(sets.Mug,sets.CurrentGear);
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
	HandlePrecast is invoked when the player casts a spell. It is the first step of two where you load any
	Fast Cast, cast time reduction, and quick cast gear in anticipation of a spell
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
	
	if gcdisplay.GetToggle('GSwap') == false then
		return;
	end

	-- Clear out the CurrentGear in case of leftovers
	gcinclude.ClearSet(sets.CurrentGear);

	-- Call the common HandleMidcast now	
	gcinclude.HandleMidcast();
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
	HandleMidshot loads Ranged Attack and Damage gear for a ranged attack
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
	
	if bTank == true then
		gcinclude.MoveToCurrent(sets.Tank_Midshot,sets.CurrentGear);
	else
		gcinclude.MoveToCurrent(sets.Midshot,sets.CurrentGear);
	end;
	gcinclude.ProgressiveAccuracy('RAcc');
	
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
