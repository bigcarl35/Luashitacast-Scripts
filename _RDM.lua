local profile = {};
gcinclude = gFunc.LoadFile('common\\gcinclude.lua');

--[[
	This file contains all the gear sets associated with the RDM job.
	
	Gear Sets last updated: March 14, 2025 (complete review)
	Code update: March 9, 2025
	
	Role: End game
--]]

local sets = {
--[[
	The gear sets are self contained, a mixture of direct gear assignments and conditional
	assignments. Each set contains entries identified by the gear slot. If it's a single
	value, it's a direct assignment like: Body = 'Austere Robe', but there can be multiple
	items identified, usually ordered by level: Body = { 'Vermillion Cloak//CARBY','Austere Robe' },
	Any item that has a // appended to it contains an inline conditional. The // code is a test
	to see if the item should be equipped. The level is still checked, but if the inline coded
	test is successful, that piece of gear will be loaded. If you've done a /gc command,
	the item's suitability for the job and accessibility will also be checked.

	Not all sets need to be defined. There is nothing wrong with leaving a set "empty", but don't
	delete any of the sets. All the ones listed here (except for any custom sets) are expected to 
	exist by Luashitacast. RDM supports "tanking", so you'll find some sets with an associated 
	"Tank_" set (ex. TP and Tank_TP). Minally include a subset entry in the Tank_ set so that 
	some gear is definied until you create a tanking set.
	
	Example:
	
	['Tank_TP'] = {
		Subset = 'TP',
	}
		
	*** Note ***
	/SMN has a problem in that their pet is the level of the subjob, which is not very useful. 
	As a result, /SMN pet actions are treated "as is" without gearset swap support.	As for /DRG,
	please note that the wyvern can't be summoned.

	Also, unlike true pet jobs like SMN and BST, PLD can only have a pet through a subjob. While 
	associated pet gearsets are available, you equally can just skip them since the pet is at 
	half your level.
	
	*** Note 2 ***
	No gear that supports bard songs can be worn by any job except a bard, so there's no explicit
	support given here for /BRD.
	
	Horizon changes from retail:
		- Starts the game with an Onion Sword instead of an Onion Dagger
		- Minor changes to artifact armor
		- Significant changes to Fencing Degen
		- Major changes to Mandau
		- 30% reduction in enmity gained from casting enfeebling spells on RDM main
		- Fast Cast has had it's recast reduction halved (cast time reduction unchanged)
		- +1 to base MP recovery rate for a total of +5, gained upon level 75
		- "Convert" has had it's recast increased from 10 to 13 minutes
		- Group 1 merit Convert Recast Reduction now only reduces the recast time to 12 
		  seconds from 20 seconds
		- Some elemental magic scrolls bought from vendors have had their prices lowered
		- The cost of Refresh has increased from 40 to 50 MP and it's recast time decreased
		  from 18 to 16 seconds
		- Poison's DOT has been increased from 1 to 4/tick
--]]

--[[
	The "default" gear set is what is worn when you're not fighting (either you or your pet)
	and you're not resting. It covers everything else: idling, traveling, in town, etc. This 
	set displays what your character looks like most of the	time. This set does not 
	distinguish the type of activities you're doing by default, so use inlines accordingly.
--]]

	['Default'] = {
		Main   = 'Earth Staff//WSWAP',
		Head   = { 'Lilac Corsage//TOWN', 'President. Hairpin//NOT_OWN//HPP.LT.94', 'Duelist\'s Chapeau', 'Warlock\'s Chapeau', 'Silver Hairpin +1' },
		Neck   = { 'Rep.Gold Medal//NOT_OWN', 'Uggalepih Pendant//NIGHTTIME', 'Fenrir\'s Torque//DAYTIME', 'Star Necklace', 'Spirit Torque', 'Justice Badge' },
		Ears   = { 'Geist Earring', 'Energy Earring +1', 'Energy Earring +1' },
		Body   = { 'Ducal Aketon//TOWN-AK', 'Vermillion Cloak//MPP.LT.90', 'Wlk. Tabard +1', 'Brigandine', 'Angler\'s Tunica' },
		Hands  = { 'Duelist\'s Gloves//WSWAP', 'Wlk. Gloves +1', 'Baron\'s Cuffs', 'Battle gloves' },
		Rings  = { 'Tamas Ring', 'Ether Ring', 'Astral Ring', 'Astral Ring' },
		Back   = 'White Cape',
		Waist  = { 'Hierarch Belt', 'Powerful Rope', 'Friar\'s Rope' },
		Legs   = { 'Duelist\'s Tights', 'Warlock\'s Tights', 'Wonder Braccae', 'San. Trousers', 'Ryl.Ftm. trousers' },
		Feet   = { 'Duelist\'s Boots', 'Warlock\'s Boots', 'Mannequin Pumps', 'Bounding boots' },
	},
	
	['Tank_Default'] = {
		Subset = 'Default',
	},
	
--[[
	The TP sets are used when you are fighting.	The accuracy set will be applied in a fractional
	manner and the evasion set if /eva is specified. When tanking, Tank_TP will be equipped 
	instead of the TP set. It's a means for	the DRK to equip more defensive gear if they find
	themselves tanking. In Tank_TP, consider including shield skill+ gear.
--]]

	['TP'] = {
        Head  = { 'Duelist\'s Chapeau', 'Warlock\'s Chapeau', 'Silver Hairpin +1' },
        Neck  = { 'Spike Necklace//NOT_WSWAP',  'Rep.Gold Medal//NOT_OWN', 'Uggalepih Pendant//NIGHTTIME', 'Fenrir\'s Torque//DAYTIME', 'Star Necklace', 'Spirit Torque', 'Justice Badge' },
        Ears  = { 'Ethereal Earring//NOT_WSWAP', 'Fang Earring//NOT_WSWAP', 'Geist Earring', 'Energy Earring +1', 'Energy Earring +1' },
        Body  = { 'Narasimha\'s Vest//NOT_WSWAP', 'Brigandine//NOT_WSWAP', 'Wlk. Tabard +1', 'Brigandine', 'Ctr. Scale Mail', 'Angler\'s Tunica' },
		Back  = { 'Forager\'s Mantle', 'Psilos Mantle', 'Amemet Mantle', 'White Cape' },
        Hands = { 'Dusk Gloves//NOT_WSWAP', 'Duelist\'s Gloves//WSWAP', 'Wlk. Gloves +1', 'Baron\'s Cuffs', 'Battle gloves' },
        Rings = { 'Flame Ring//NOT_WSWAP', 'Kshama Ring No.8//NOT_WSWAP', 'Tamas Ring', 'Ether Ring', 'Astral Ring', 'Astral Ring' },
        Waist = { 'Swift Belt//NOT_WSWAP', 'Hierarch Belt', 'Powerful Rope', 'Friar\'s Rope' },
        Legs  = { 'Duelist\'s Tights', 'Warlock\'s Tights', 'Wonder Braccae', 'Ryl.Ftm. trousers' },
        Feet  = { 'Duelist\'s Boots', 'Warlock\'s Boots', 'Mannequin Pumps', 'Bounding boots' },
    },

	['Tank_TP'] = {
		Subset = 'TP',
	},
	
--[[
	If an accuracy emphasis is desired, the following set will replace the gear, appropriately.
	Unlike TP though, accuracy is applied one slot at a time in a fractionalized manner using
	the /acc command.
	
	Include equipment with accuracy bonus and DEX. Remember, DEX converts to accuracy: (horizon) 
	for every 1 point of DEX you get 0.70 points of accuracy if wielding a 2H weapon, 0.65 for 
	a 1H weapon, and 0.60 for H2H. 
--]]
	
	['Accuracy'] = {
		Head  = { 'Optical Hat', 'Empress Hairpin' },		-- +10 Acc, +3 DEX
		Neck  = { 'Peacock Amulet', 'Spike Necklace' },		-- +10 Acc, +3 DEX
		Ears  = 'Beastly Earring//Pet',						-- +10 Pet Acc
		Body  = { 'Scorpion Harness', 'Brigandine', 'Mrc.Cpt. Doublet' },	-- +10/4 Acc, +2/2/1 DEX
        Hands = { 'Wlk. Gloves +1', 'Battle Gloves' },		-- +6 DEX, +3 Acc
		Rings = { 'Toreador\'s Ring', 'Toreador\'s Ring', 'Woodsman Ring', 'Woodsman Ring', 'Jaeger Ring', 'Kshama Ring No.2', 'Balance Ring' },	-- +7/+7/+5/+5/4, +3DEX/+2 Acc, +2 DEX
		Back  = 'Psilos Mantle',							-- +1 Acc
		Waist = { 'Life Belt', 'Tilt Belt', 'Mrc.Cpt. Belt' },	-- +10/5 Acc, +1 DEX
		Legs  = 'Duelist\'s Tights',						-- +5 DEX
		Feet  = 'Bounding Boots',							-- +3 DEX
    },

	['Tank_Accuracy'] = {
		Subset = 'Accuracy',
	},

--[[
	Similar to the accuracy set, the Ranged_Accuracy set will be used on ranged attacks
--]]

	['Ranged_Accuracy'] = {
		Head   = 'Optical Hat',			-- +10 RAcc
		Neck   = 'Peacock Amulet',		-- +10 RAcc
		Hands  = 'Crimson Fng. Gnt.',	-- +10 RAcc
		Rings  = { 'Woodsman Ring', 'Woodsman Ring', 'Jaeger Ring', 'Beetle Ring +1', 'Beetle Ring +1' },	-- +5/5/4/4/4 RAcc
		Back   = 'Psilos Mantle',		-- +1 RAcc
	},

	['Tank_Ranged_Accuracy'] = {
		Subset = 'Ranged_Accuracy',
	},

--[[
	Progressive is a new idea for handling accuracy/ranged accuracy. You create
	stages to load accuracy gear from. Depending on what the player specifies,
	that stage and any before it will be loaded. The intent is to replace the
	Fractional Accuracy with this new system.
--]]

  ['Progressive'] = { 
		['Accuracy'] = { 
			[1] = { 
				['Head'] = 'Accuracy::Head',
				['Neck'] = 'Accuracy::Neck'
			},
			[2] = {
				['Body']  = 'Accuracy::Body',
				['Waist'] = 'Accuracy::Waist'
			},
			[3] = {	
				['Rings'] = 'Accuracy::Rings',
			},
			[4] = {
				['Subset'] = 'Accuracy' 
			}
		},
		['Ranged_Accuracy'] = {
			[1] = {
				['Neck']  = 'Ranged_Accuracy::Neck',
				['Hands'] = 'Ranged_Accuracy::Hands',
			},
			[2] = { 
				['Subset'] = 'Ranged_Accuracy' 
			}
		}				
  },
  
--[[
	If evasion wanted, equip evasion gear. Remember that AGI converts to evasion: for every
	2 points of AGI you get 1 point of evasion. Like TP and Accuracy, the evasion set has a
	Tank_Evasion variation.	
--]]
	
	['Evasion'] = {
		Ammo  = 'Orphic Egg//PJPBRD',							-- +1 Eva
        Head  = { 'Optical Hat', 'Empress Hairpin' },			-- +10/10 Eva
		Neck  = 'Spirit Torque',								-- +5 Eva
		Ears  = { 'Bat Earring//BLINDED', 'Ethereal Earring', 'Reraise Earring', 'Genin Earring//SJNIN', 'Drone Earring' },	-- +15 Eva while blinded, +5/2 Eva, +4/3 AGI
		Body  = { 'Scorpion Harness', 'Mrc.Cpt. Doublet' },		-- +10 Eva, +1 AGI
		Hands = 'Battle Gloves',								-- +3 Eva
		Waist = 'Swift Belt//IF:Tilt Belt',						-- filler, Tilt Belt has -5 Eva
		Legs  = 'San. Trousers',								-- +2 Eva
		Feet  = { 'Duelist\'s Boots', 'Bounding Boots' },		-- +5 Eva, +3 AGI
    },

	['Tank_Evasion'] = {
		Subset = 'Evasion',
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
		Head = 'President. Hairpin//NOT_OWN',
	},
	
	['Resting_Refresh'] = {
		Head = 'Duelist\'s Chapeau',									-- Refresh
		Main = { 'Pluto\'s Staff//WSWAP', 'Pilgrim\'s wand//WSWAP' },	-- +10/2 MP/tick while resting
		Body = { 'Wlk. Tabard +1', 'Errant Hpl.', 'Vermillion Cloak//MPP.LT.100' },	-- +5/5 MP/tick while resting, Refresh
		Waist = 'Hierarch Belt',										-- +2 MP/tick while resting
		Legs = 'Baron\'s Slops',										-- +1 MP/Tick while resting
    },
	
--[[
	Start weapons are where you define what you want the first row of equipment to look 
	like when you either log in as a RDM or switch your main job to RDM. Any other gear 
	you mention will be overridden by the Idle or Town set, so no need to include here.
--]]

	['Start_Weapons'] = {
		Main  = { 'Earth Staff//WSWAP', 'Guespiere', 'Pilgrim\'s Wand' },
		Ammo  = { 'Hedgehog Bomb', 'Fortune Egg' },
    },
	
--[[
	Preshot is the first stage of when a ranged shot is being performed. This is where 
	you place any gear that reduces the time it takes to shoot (snap shot, rapid shot, 
	quick shot, shot delay reduction, and ranged haste).
--]]

	['Preshot'] = {
    },
	
	['Tank_Preshot'] = {
		Subset = 'Preshot',
	},
	
--[[
	Midshot is the second stage of a ranged shot. This is where you place
	Ranged Accuracy, Ranged Attack, Ranged Damage, Crit. Rate, Crit. Damage,
	Store TP, recycle, etc.
	
	Note: Ranged accuracy now has it's own breakout gear set. No need to
	include here unless you want to force it.
--]]

	['Midshot'] = {
		Main  = 'Vulcan\'s Staff//WSWAP',				-- +10 RAtt
		Ears  = 'Brutal Earring',						-- Store TP +1
		Back  = { 'Psilos Mantle', 'Amemet Mantle' },	-- +12/10 RAtt
    },

	['Tank_Midshot'] = {
		Subset = 'Midshot',
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
		Main  = 'Aquilo\'s Staff//WSWAP',	-- +5 INT
		Head  = 'Warlock\'s Chapeau',		-- +3 INT
		Body  = { 'Errant Hpl.', 'Black Cotehardie', 'Baron\'s Saio' },	-- +10/2/1 INT
		Hands = { 'Errant Cuffs', 'Duelist\'s Gloves' },	-- +5/4 INT
		Rings = { 'Tamas Ring', 'Flame Ring' },				-- +5/2 INT
		Waist = { 'Penitent\'s Rope', 'Mrc.Cpt. Belt' },	-- +5/1 INT
		Legs  = 'Errant Slops',				-- +7 INT
		Feet  = { 'Rostrum Pumps', 'Warlock\'s Boots', 'Mannequin Pumps' },	-- +3/1/1 INT	
	},
	
	['Tank_INT'] = {
		Subset = 'INT',
	},
	
	['MND'] = {
		Main  = { 'Water Staff//WSWAP', 'Pluto\'s Staff//WSWAP', 'Light Staff//WSWAP' },	-- +4/2/1 MND
		Neck  = { 'Promise Badge', 'Justice Badge' },		-- +5/3 MND
		Ears  = 'Geist Earring',		-- +1 MND
		Body  = { 'Errant Hpl.', 'Wonder Kaftan', 'Baron\'s Saio' },	-- +10/1/1 MND
		Hands = { 'Wlk. Gloves +1', 'Baron\'s Cuffs' },		-- +2/1 MND
		Rings = { 'Tamas Ring', 'Kshama Ring No.9', 'Tranquility Ring' },	-- +5/3/2 MND
		Back  = 'White Cape',			-- +2 MND
		Waist = { 'Penitent\'s Rope', 'Mrc.Cpt. Belt', 'Friar\'s Rope' },	-- +5/1/1 MND
		Legs  = { 'Errant Slops', 'Warlock\'s Tights', 'Wonder Braccae' },	-- +7/3/2 MND
		Feet  = { 'Duelist\'s Boots', 'Mannequin Pumps' }, 	-- +4/2 MND	
	},
	
	['Tank_MND'] = {
		Subset = 'MND',
	},
	
	['Enmity_Plus'] = {
		Waist = 'Warwolf Belt',		-- +3 Enmity
	},
	
	['Enmity_Minus'] = {
		Ammo  = 'Hedgehog Bomb',	-- -1 Enmity
		Hands = 'Errant Cuffs',		-- -2 Enmity
		Rings = 'Tamas Ring',		-- -5 Enmity
		Waist = 'Penitent\'s Rope',	-- -3 Enmity
		Legs  = 'Errant Slops',		-- -3 Enmity
	},
		
--[[
	Spells are a bit different. Each type of spell can have it's own enhancement gear as well as 
	stat based gear. (In some cases individual spells have special entries.) These sets do not 
	include elemental gear which is	dependent on day/weather/weapon skill.

	The first stage is Precast. This is where you place any Fast Cast, cast time reduction, quick 
	cast gear, and spell interruption rate
--]]

	['Precast'] = {	
		Head = 'Warlock\'s Chapeau',	-- Enhances Fastcast
		Body = 'Duelist\'s Tabard',		-- Enhances Fastcast
		Ears = 'Loquac. Earring',		-- Enhances Fastcast
	},
	
	['Tank_Precast'] = {
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
				   'CHR//SINGING',		-- Charisma provides accuracy w/singing
				   },
		Rings  = 'Tamas Ring',			-- +5 MAcc
		Feet   = 'Nashira Crackows',	-- +2 MAcc
	},
	
	['Tank_Macc'] = {
		Subset = 'Macc',
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
	main job supports tanking (like RDM does.) I've included details on
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
		Body = 'Duelist\'s Tabard',		-- +10 Healing Magic Skill
		Legs = 'Warlock\'s Tights',		-- +10 Healing Magic Skill
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
			Cure IV		460		330		390
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
		Main   = 'Light Staff//WSWAP',
		Ammo   = 'Enmity_Minus::Ammo',			-- -1 Enmity
		Body   = 'Wlk. Tabard +1//EMPTY',		-- -12% SIR
		Hands  = 'Enmity_Minus::Hands//IFNOE:Wlk. Gloves +1',
	},
	
	['Tank_CuringMagic'] = {
		Subset = 'CuringMagic',
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
		Subset = 'CuringMagic',	
		Neck   = 'Uggalepih Pendant//SPECIAL',	-- +8 MAB if MP% < 51%
		Body   = 'Wlk. Tabard +1//EMPTY',		-- -12% SIR
		Feet   = 'Duelist\'s Boots',			-- +4 MAB
	},
	
	['Tank_OffensiveCuring'] = {
		Subset = 'CuringMagic',
	},

--[[
	This set is used for all non-cure Healing Magic spells. Only 
	healing magic skill is of any importance here. You might want 
	to use this set as a subset for the other cure-based sets.
--]]

	['HealingMagic'] = {
		Subset = 'Healing_Magic_Skill',
		Body   = 'Wlk. Tabard +1//EMPTY',		-- -12% SIR
	},
	
	['Tank_HealingMagic'] = {
		Subset = 'HealingMagic',
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

	['Enhancing_Magic_Skill'] = {
		Hands = 'Duelist\'s Gloves',	-- +15 Enhancing Magic Skill
		Legs  = 'Warlock\'s Tights',	-- +15 Enhancing Magic Skill
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
		Subset = 'Enhancing_Magic_Skill',
		Body   = 'Wlk. Tabard +1',		-- -12% SIR
	},
	
	['Tank_Barspell'] = {
		Subset = 'Barspell',
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
		Subset = 'Enhancing_Magic_Skill',
		Body   = 'Wlk. Tabard +1',		-- -12% SIR
	},
	
	['Tank_Enspell'] = {
		Subset = 'Enspell',
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
	Ice/Shock spikes: integer(integer(((INT+10)/20) + 2) * (1 + (MAB/100)))
--]]
	
	['Spike'] = {
		Subset = 'INT',
		Neck   = 'Uggalepih Pendant//SPECIAL',	-- +8 MAB if MP% < 51%
		Hands  = 'Enhancing_Magic_Skill::Hands//EMPTY',	-- Enhances Magic Skill
		Legs   = { 'Duelist\'s Tights', 'Errant Slops', 'Enhancing_Magic_Skill::Legs' },	-- Enhances Spikes, +7 INT, Enhances Magic Skill
		Feet   = 'Duelist\'s Boots',			-- +4 MAB
	},
	
	['Tank_Spike'] = {
		Subset = 'Spike',
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
		Body   = 'Wlk. Tabard +1//IFNOE:Errant Hpl.',	-- -12% Spell Interruption Rate
		Hands  = 'Enhancing_Magic_Skill::Hands',	-- Enhancing Magic Skill
		Legs   = 'Enhancing_Magic_Skill::Legs//IFNOE:Errant Slops',	-- Enhancement Magic Skill
	},
	
	['Tank_Stoneskin'] = {
		Subset = 'Stoneskin',
	},

--[[
	Sneak's duration is variable, but the duration maxes at about 5 
	minutes. Include any gear that enhances this buff.
--]]

	['Sneak'] = {
		Body = 'Wlk. Tabard +1',	-- -12% Spell Interruption Rate
		Feet = 'Dream Boots +1',	-- Enhances Sneak
	},
	
	['Tank_Sneak'] = {
		Subset = 'Sneak',	
	},

--[[
	Invisible's duration is variable, but the duration maxes at 
	about 5 minutes. Include any gear that enhances this buff.
--]]	

	['Invisible'] = {
		Body  = 'Wlk. Tabard +1',	-- -12% Spell Interruption Rate
		Hands = 'Dream Mittens +1',	-- Enhances Invisible
	},
	
	['Tank_Invisible'] = {
		Subset = 'Invisible',	
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
		Subset = 'Enhancing_Magic_Skill',
		Body   = 'Wlk. Tabard +1',		-- -12% Spell Interruption Rate
	},	

	['Tank_Phalanx'] = {
		Subset = 'Phalanx',	
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
		Subset = 'Enhancing_Magic_Skill',
		Body   = 'Wlk. Tabard +1',		-- -12% Spell Interruption Rate
		Rings  = { 'Dilation Ring//NOT_ME//SP:Refresh', 'Dilation Ring//NOT_ME//SP:Haste', 'Enmity_Minus::Rings' },
		Hands  = 'Enmity_Minus::Hands',
		Waist  = 'Enmity_Minus::Waist',
	},
	
	['Tank_EnhancingMagic'] = {
		Subset = 'EnhancingMagic',
	},

--[[
	****************************
	* Midcast: Elemental Magic *
	****************************
--]]

	['Elemental_Magic_Skill'] = {
		Main = 'Aquilo\'s Staff//WSWAP',		-- +10 Elemental Magic Skill 
		Head = 'Warlock\'s Chapeau',			-- +10 Elemental Magic Skill
		Legs = 'Duelist\'s Tights',				-- +10 Elemental Magic Skill
		Feet = 'Nashira Crackows',				-- +5 Elemental Magic Skill
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
		Neck   = 'Uggalepih Pendant//SPECIAL',	-- +8 MAB if MP% < 51%		
		Body   = 'Wlk. Tabard +1//EMPTY',		-- -12% SIR
		Feet   = 'Duelist\'s Boots',			-- +4 MAB
	},
	
	['Tank_ElementalNuke'] = {
		Subset = 'ElementalNuke',
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
		Body   = 'Wlk. Tabard +1//EMPTY',		-- -12% SIR
	},
	
	['Tank_ElementalDebuff'] = {
		Subset = 'ElementalDebuff',
	},

--[[
	**********************
	* Midcast: Summoning *
	**********************
--]]
	
	['Summoning_Skill'] = {
		Neck   = 'Smn. Torque',				-- +7 Summoning Magic Skill
		Feet   = 'Nashira Crackows',		-- +5 Summoning Magic Skill
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
		Body   = 'Wlk. Tabard +1//EMPTY',		-- -12% SIR
	},
	
	['Tank_Summoning'] = {
		Subset = 'Summoning',
	},

--[[
	***********************
	* Midcast: Dark Magic *
	***********************
--]]
	
	['Dark_Magic_Skill'] = {
		Hands = 'Crimson Fng. Gnt.',		-- +10 Dark Magic Skill
	},
	
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
		Subset = 'Enmity_Minus',
		Hands  = 'Dark_Magic_Skill::Hands',
		Body   = 'Wlk. Tabard +1',		-- -12% SIR
	},
	
	['Tank_Absorb'] = {
		Subset = 'Absorb',
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
		Subset = 'Enmity_Minus',
		Hands  = 'Dark_Magic_Skill::Hands',
		Body   = 'Wlk. Tabard +1',		-- -12% SIR
	},
	
	['Tank_Drain'] = {
		Subset = 'Tank_Drain',
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
		Subset = 'Enmity_Minus',
		Hands  = 'Dark_Magic_Skill::Hands',
		Body   = 'Wlk. Tabard +1',		-- -12% SIR
	},
	
	['Tank_Aspir'] = {
		Subset = 'Aspir',
	},

--[[
	This last gear set, DarkMagic, covers all Dark Magic spells not covered
	by the previous three gear sets. 
--]]
	
	['DarkMagic'] = {
		Subset = 'Enmity_Minus',	
		Neck   = 'Uggalepih Pendant//SPECIAL',	-- +8 MAB if MP% < 51%
		Body   = 'Wlk. Tabard +1',				-- -12% SIR
		Hands  = 'Dark_Magic_Skill::Hands',
		Feet   = 'Duelist\'s Boots',			-- +4 MAB
	},
	
	['Tank_DarkMagic'] = {
		Subset = 'DarkMagic',
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
	Divine Magic Skill determines accuracy and reduces spell interruption
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
		Subset = 'Enmity_Minus',	
		Main   = 'Water Staff//WSWAP',			-- +10 Divine Magic Skill
		Neck   = 'Uggalepih Pendant//SPECIAL',	-- +8 MAB if MP% < 51%
		Body   = 'Wlk. Tabard +1',				-- -12% SIR
		Feet   = 'Duelist\'s Boots',			-- +4 MAB
	},
	
	['Tank_OffensiveDivine'] = {
		Subset = 'OffensiveDivine',
	},

--[[
	Enfeebling divine spell (flash) afflicts the target with accuracy 
	reduction (similar to blind) with a weakening effect over time till
	it runs out. Duration is subject to resists and partial resists
	although can last 12 seconds if not resisted. It also generates a
	significant amount of volitile and cumulative enmity.
--]]	
	
	['EnfeebleDivine'] = {
		Subset = 'Enmity_Minus',	
		Main   = 'Water Staff//WSWAP',			-- +10 Divine Magic Skill	
		Body   = 'Wlk. Tabard +1',				-- -12% SIR
		Neck   = { 'Enfeebling Torque', 'Uggalepih Pendant//SPECIAL' },
		Feet   = 'Duelist\'s Boots',			-- +4 MAB		
	},
	
	['Tank_EnfeebleDivine'] = {
		Subset = 'EnfeebleDivine',
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
		Subset = 'Enmity_Minus',
		Main   = 'Water Staff//WSWAP',
		Body   = 'Wlk. Tabard +1',				-- -12% SIR
		Neck   = { 'Enfeebling Torque', 'Uggalepih Pendant//SPECIAL' },
		Feet   = 'Duelist\'s Boots',			-- +4 MAB		
	},
	
	['Tank_EnhanceDivine'] = {
		Subset = 'EnhanceDivine',
	},

--[[
	*****************************
	* Midcast: Enfeebling Magic *
	****************************
--]]
	
	['Enfeebling_Magic_Skill'] = {
		Head = 'Duelist\'s Chapeau',	-- +15 Enfeebling Magic Skill
		Neck = 'Enfeebling Torque',		--  +8 Enfeebling Magic Skill
		Body = 'Wlk. Tabard +1',		-- +15 Enfeebling Magic Skill
	},
	
--[[
	Enfeebling Magic: this class of spells apply a debilitating status effect
	(debuff) to one or more targets. Enfeebling Magic Skill is used to determine
	the accuracy of enfeebling magic and to decrease the likelihood of a spell
	caster being interrupted when casting enfeebling magic.
	
	Enfeebling Spells: bind, blinds, blindgas, dias, diagas, dispel, gravity, 
	paralyzes, poisons, poisongas, sleeps, sleepgas, silence, and slows.
	
	There are three types of enfeebling spells, those dependent on INT (gravity,
	bind, blind, dispel, sleep, sleepga, poison, and poisonga), those
	dependent on MND (paralyze, silence, slow, slowga, frazzlke, distract) and
	the rest: dia and diaga.
	
	After the appropriate gear set is equipped, an elemental obi might be
	equipped (for day/weather effect) and an elemental staff (for magic
	affinity.)
--]]
	
	['EnfeeblingINT'] = {
		Subset = 'INT',	
		Head   = 'Enfeebling_Magic_Skill::Head',
		Neck   = 'Enfeebling_Magic_Skill::Neck',
		Body   = 'Wlk. Tabard +1//IF:Baron\'s Saio',
	},
	
	['Tank_EnfeeblingINT'] = {
		Subset = 'EnfeeblingINT',
	},
	
	['EnfeeblingMND'] = {
		Subset = 'MND',
		Head   = 'Enfeebling_Magic_Skill::Head',
		Body   = 'Wlk. Tabard +1//IFNOE:Errant Hpl.',
	},
	
	['Tank_EnfeeblingMND'] = {
		Subset = 'EnfeeblingMND',
	},

	['EnfeeblingMagic'] = {
		Subset = 'Enfeebling_Magic_Skill',
	},
	
	['Tank_EnfeeblingMagic'] = {
		Subset = 'EnfeeblingMagic',
	},
	
--[[
	********************
	* Midcast: Singing *
	********************
--]]
	['Singing_Skill'] = {	-- Covers both Singing Skill and Intrument Skill
	},
	
	['CHR'] = {		-- For singing Charisma is used for accuracy
		Main  = 'Pluto\'s Staff//WSWAP',	-- +2 CHR
		Head  = 'Entrancing Ribbon',		-- +2 CHR
		Ears  = 'Beastly Earring',			-- +2 CHR
		Body  = { 'Errant Hpl.', 'Brigandine//IF:Black Cotehardie' },	-- +10 CHR, filler to avoid -3 CHR
		Neck  = { 'Star Necklace', 'Flower Necklace' },	-- +3/3 CHR		
		Rings = { 'Moon Ring', 'Moon Ring' },
		Waist = { 'Corsette', 'Mrc.Cpt. Belt' },	-- +5/1 CHR
		Legs  = 'Errant Slops',				-- +7 CHR
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
	
	['Tank_EnhancementSinging'] = {
		Subset = 'EnhancementSinging',
	},

--[[
	EnfeeblingSinging contains gear that debuffs targets. Included are: requiem,
	threnody, lullaby, finale, elegy, and virelai.
--]]
	
	['EnfeeblingSinging'] = {
		Subset = 'CHR',	
	},
	
	['Tank_EnfeeblingSinging'] = {
		Subset = 'EnfeeblingSinging',
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
	},
	
	['Tank_NinjutsuBuff'] = {
	},

-- An elemental stave will be checked for after the debuff set is loaded.
	
	['NinjutsuDebuff'] = {
	},
	
	['Tank_NinjutsuDebuff'] = {
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
		Subset = 'INT',
		Neck   = 'Uggalepih Pendant//SPECIAL',
	},
	
	['Tank_NinjutsuElemental'] = {
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
	
	RDM can use the following weapons: Dagger (B), Sword (B), Club (D), Archery (E)
		
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
		Main  = 'Vulcan\'s Staff//WSWAP',			-- +10 Att
		Ears  = { 'Ethereal Earring', 'Coral Earring', 'Fang Earring', 'Brutal Earring' },	-- +5/5/4 Att, Store TP +1
		Hands = 'Dusk Gloves',						-- +5 Att
		Rings = 'Kshama Ring No.8',					-- +3 Att
		Back  = { 'Forager\'s Mantle', 'Psilos Mantle', 'Amemet Mantle' },	-- +15/12/B10 Att
		Waist = 'Powerful Rope//IF:SWIFT BELT',
	},
	
--[[
		* Strength based *
		
		Dagger: Mercy Stroke		
		Sword: Flat Blade,Circle Blade,Vorpal Blade
		Club: Starlight,Skull Breaker,True Strike
-]]
	
	['WS_STR'] = {
		Subset = 'AttackPower',	
		Head   = 'Mrc.Cpt. Headgear',	-- +1 STR
		Neck   = 'Spike Necklace',		-- +3 STR
		Body   = { 'Narasimha\'s Vest', 'Black Cotehardie', 'Wonder Kaftan' },		-- +3/3/1 STR
		Hands  = { 'Ogre Gloves', 'Wonder Mitts' },			-- +6/3 STR
		Rings  = { 'Flame Ring', 'Sun Ring', 'Kshama Ring No.8', 'Courage Ring' },	-- +5/3/3/2 STR
		Back   = { 'Forager\'s Mantle', 'Amemet Mantle//EMPTY' },	-- +3/1 STR
		Waist  = { 'Warwolf Belt', 'Mrc.Cpt. Belt' },		-- +5/1 STR
		Legs   = 'Wonder Braccae',		--  +1 STR
		Feet   = { 'Creek F Clomps', 'Wonder Clomps' },		-- +4/2 STR
    },
	
--[[
		* Strength and Agility based, ranged *
		
		Archery: Flaming Arrow^,Piercing Arrow^,Dulling Arrow^,Sidewinder^,Blast Arrow^
		
		^ Subjob must be RNG
--]]

	['WS_RANGED_STRAGI'] = {
		Subset = 'AttackPower',	
		Head   = 'Empress Hairpin',								-- +3 AGI
		Neck   = 'Spike Necklace',								-- +3 STR
		Ears   = { 'Genin Earring//SJNIN', 'Drone Earring' },	-- +4 AGI SJ NIN, +3 AGI
		Body   = { 'Black Cotehardie', 'Wonder Kaftan', 'Mrc.Cpt. Doublet' },	-- +3 STR/+3 AGI, +1/1 STR
		Hands  = { 'Ogre Gloves', 'Wonder Mitts' },				-- +6/3 STR
		Rings  = { 'Flame Ring', 'Sun Ring', 'Kshama Ring No.8', 'Courage Ring' },	-- +5/3/3/2 STR
		Back   = { 'Forager\'s Mantle', 'Amemet Mantle//EMPTY' },		-- +3/1 STR
		Waist  = { 'Warwolf Belt', 'Mrc.Cpt. Belt' },			-- +5 STR, +1 STR/+1 AGI
		Legs   = 'Wonder Braccae',								-- +1 STR
		Feet   = { 'Creek F Clomps', 'Bounding Boots', 'Wonder Clomps' },	-- +4 STR, +3 AGI, +2 STR
    },

--[[
		* Strength and Dexterity based, even weighting *
		
		Sword: Fast Blade	
--]]

	['WS_STRDEX'] = {
		Subset = 'AttackPower',	
		Head   = { 'Empress Hairpin', 'Mrc.Cpt. Headgear' },	-- +3 DEX, +1 STR
		Neck   = 'Spike Necklace',								-- +3 STR
		Body   = { 'Black Cotehardie', 'Brigandine', 'Mrc.Cpt. Doublet' },	-- +3 STR/+2 DEX, +2 DEX, +1 STR/+1 DEX
		Hands  = { 'Wlk. Gloves +1', 'Ogre Gloves', 'Wonder Mitts' },	-- +6 DEX, +6/3 STR
		Rings  = { 'Flame Ring', 'Sun Ring', 'Kshama Ring No.8', 'Kshama Ring No.2', 'Courage Ring', 'Balance Ring' },	-- +5/3/3 STR, +3 DEX, +2 STR, +2 DEX
		Back   = { 'Forager\'s Mantle', 'Amemet Mantle//EMPTY' },		-- +3/1 STR
		Waist  = { 'Warwolf Belt', 'Mrc.Cpt. Belt' },			-- +5 STR, +1 STR/+1 DEX
		Legs   = { 'Duelist\'s Tights','Wonder Braccae' },		-- +5 DEX, +1 STR
		Feet   = { 'Creek F Clomps', 'Bounding Boots' },		-- +4 STR, +3 DEX
    },

--[[
		* Strength and Intelligence based, even weighting *
		
		Sword: Burning Blade,Red Lotus Blade
--]]
	
	['WS_STRINT'] = {
		Subset = 'AttackPower',	
		Head   = { 'Warlock\'s Chapeau', 'Mrc.Cpt. Headgear' },	-- +3 INT, +1 STR
		Neck   = 'Spike Necklace',								-- +3 STR
		Body   = { 'Errant Hpl.', 'Black Cotehardie', 'Wonder Kaftan', 'Baron\'s Saio' },	-- +10 INT, +3 STR/+2 DEX, +1 STR, +1 INT
		Hands  = { 'Errant Cuffs', 'Duelist\'s Gloves', 'Wonder Mitts' },	-- +5/4 INT, +3 STR
		Rings  = { 'Flame Ring', 'Tamas Ring', 'Sun Ring', 'Kshama Ring No.8', 'Courage Ring' },	-- +5 STR/+2 INT, +5 INT, +3/2 STR
		Back   = { 'Forager\'s Mantle', 'Amemet Mantle' },		-- +3/1 STR
		Waist  = { 'Warwolf Belt', 'Penitent\'s Rope', 'Mrc.Cpt. Belt' },	-- +5 STR, +5 INT, +1 STR/+1 INT
		Legs   = { 'Errant Slops', 'Wonder Braccae' },			-- +7 INT, +1 STR
		Feet   = { 'Creek F Clomps', 'Wonder Clomps', 'Warlock\'s Boots', 'Mannequin Pumps' },	-- +4/2 STR, +1/1 INT
	},

--[[
		* Strength and Mind based, even weighting *
		
		Sword: Shining Blade,Seraph Blade,Swift Blade,Savage Blade,
			   Knights of Round
		Club: Shining Strike
--]]

	['WS_STRMND'] = {
		Subset = 'AttackPower',	
		Head   = 'Mrc.Cpt. Headgear',					-- +1 STR
		Ears   = { 'Geist Earring' },	-- +1 MND
		Neck   = { 'Promise Badge', 'Justice Badge' },	-- +5/3 MND
		Body   = { 'Errant Hpl.', 'Wonder Kaftan', 'Baron\'s Saio' },	-- +10 MND, +1 STR/+1 MND, +1 MND
		Hands  = { 'Wonder Mitts', 'Baron\'s Cuffs//EMPTY' },	-- +2/1 MND
		Rings  = { 'Tamas Ring', 'Flame Ring', 'Sun Ring', 'Kshama Ring No.8', 'Kshama Ring No.9', 'Courage Ring', 'Tranquility Ring' },	-- +5 MND, +5/3/3 STR, +3 MND, +2 STR, +2 MND
		Back   = { 'Forager\'s Mantle', 'White Cape', 'Amemet Mantle//EMPTY' },	-- +3 STR, +2 MND, +1 STR
		Waist  = { 'Warwolf Belt', 'Penitent\'s Rope', 'Mrc.Cpt. Belt', 'Friar\'s Rope' },	-- +5 STR, +5 MND, +1 STR/+1 MND, +1 MND
		Legs   = { 'Errant Slops', 'Warlock\'s Tights', 'Wonder Braccae' },	-- +7/3 MND, +2 MND/+1 STR
		Feet   = { 'Duelist\'s Boots', 'Creek F Clomps', 'Wonder Clomps' },	-- +4 MND, +4/2 STR
    },
	
--[[
		* Charisma based *
		
		Dagger: Shadowstitch
--]]
	
	['WS_CHR'] = {
		Subset = 'AttackPower',	
		Head   = 'Entrancing Ribbon',		-- +2 CHR
		Ears   = 'Beastly Earring',			-- +2 CHR
		Body   = { 'Errant Hpl.', 'Brigandine//IF:Black Cotehardie' },	-- +10 CHR, filler to avoid -3 CHR
		Neck   = { 'Star Necklace', 'Flower Necklace' },	-- +3/3 CHR		
		Rings  = { 'Moon Ring', 'Moon Ring' },
		Waist  = { 'Corsette', 'Mrc.Cpt. Belt' },	-- +5/1 CHR
		Legs   = 'Errant Slops',				-- +7 CHR
    },
	
--[[
		* Dexterity based *
		
		Dagger: Wasp Sting,Viper Bite,Eviseration
--]]
	
	['WS_DEX'] = {
		Subset = 'AttackPower',
		Head   = 'Empress Hairpin',		-- +3 DEX
		Neck   = 'Spike Necklace',		-- +3 DEX
		Body   = { 'Black Cotehardie', 'Brigandine', 'Mrc.Cpt. Doublet' },	-- +2/2/1 DEX
		Hands  = 'Wlk. Gloves +1',		-- +6 DEX
		Rings  = { 'Kshama Ring No.2', 'Balance Ring' },	-- +3/2 DEX
		Waist  = { 'Warwolf Belt', 'Mrc.Cpt. Belt' },		-- +5/1 DEX
		Legs   = 'Duelist\'s Tights',	-- +5 DEX
		Feet   = 'Bounding Boots',		-- +3 DEX
    },

--[[
		* Dexterity and Agility based *
		
		Dagger: Eviseration
--]]
	
	['WS_DEXAGI'] = {
		Subset = 'AttackPower',	
		Head   = 'Empress Hairpin',		-- +3 DEX/+3 AGI
		Neck   = 'Spike Necklace',		-- +3 DEX
		Ears   = { 'Genin Earring//SJNIN', 'Drone Earring' },	-- +4 AGI if SJ NIN, +3 AGI
		Body   = { 'Black Cotehardie', 'Brigandine', 'Mrc.Cpt. Doublet' },	-- +3 AGI/+2 DEX, 2 DEX, +1 AGI/+1 DEX
		Hands  = 'Wlk. Gloves +1',		-- +6 DEX
		Rings  = { 'Kshama Ring No.3', 'Kshama Ring No.2', 'Balance Ring' },	-- +3 AGI, +3/2 DEX
		Waist  = { 'Warwolf Belt', 'Mrc.Cpt. Belt' },		-- +5 DEX, +1 DEX/+1 AGI
		Legs   = 'Duelist\'s Tights',	-- +5 DEX
		Feet   = 'Bounding Boots',		-- +3 DEX/+3 AGI
    },
	
--[[
		* Dexterity and Intelligence based *
		
		Dagger: Gust Slash,Cyclone
--]]
	
	['WS_DEXINT'] = {
		Subset = 'AttackPower',	
		Head   = { 'Warlock\'s Chapeau', 'Empress Hairpin' },		-- +3 INT, +3 DEX
		Neck   = 'Spike Necklace',		-- +3 DEX
		Body   = { 'Errant Hpl.', 'Black Cotehardie', 'Brigandine', 'Mrc.Cpt. Doublet' },	-- +10 INT, +2 DEX/+2 INT, +2/1 DEX
		Hands  = { 'Wlk. Gloves +1', 'Errant Cuffs', 'Duelist\'s Gloves' },		-- +6 DEX, +5/2 INT
		Rings  = { 'Tamas Ring', 'Kshama Ring No.5', 'Kshama Ring No.2', 'Balance Ring' },	-- +5/3 INT, +3/2 DEX
		Waist  = { 'Warwolf Belt', 'Penitent\'s Rope', 'Mrc.Cpt. Belt' },	-- +5 DEXm +5 INT, +1 DEX/+1 INT
		Legs   = 'Errant Slops',	-- +7 INT
		Feet   = 'Bounding Boots',	-- +3 DEX
    },
	
--[[
		* Mind based *
		
		Dagger: Energy Steal,Energy Drain
--]]

	['WS_MND'] = {
		Subset = 'AttackPower',	
		Neck  = { 'Promise Badge', 'Justice Badge' },		-- +5/3 MND
		Ears  = 'Geist Earring',		-- +1 MND
		Body  = { 'Errant Hpl.', 'Wonder Kaftan', 'Baron\'s Saio' },	-- +10/1/1 MND
		Hands = { 'Wlk. Gloves +1', 'Baron\'s Cuffs' },		-- +2/1 MND
		Rings = { 'Tamas Ring', 'Kshama Ring No.9', 'Tranquility Ring' },	-- +5/3/2 MND
		Back  = 'White Cape',			-- +2 MND
		Waist = { 'Penitent\'s Rope', 'Mrc.Cpt. Belt', 'Friar\'s Rope' },	-- +5/1/1 MND
		Legs  = { 'Errant Slops', 'Warlock\'s Tights', 'Wonder Braccae' },	-- +7/3/2 MND
		Feet  = { 'Duelist\'s Boots', 'Mannequin Pumps' }, 	-- +4/2 MND	
    },
	
--[[
		* Skill based *
		
		Club: Starlight
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
		Ammo   = 'Happy Egg',		-- +1% HP
		Head   = 'Duelist\'s Chapeau//IF:Empress Hairpin',	-- filler in case to offset -15 HP of Empress hairpin
		Neck  = 'Promise Badge',	-- 10 HP
		Ears  = { 'Physical Earring', 'Ethereal Earring' },	-- Convert 25 MP to HP, +15 HP
		Body  = 'Wonder Kaftan',	-- +36 HP
		Hands = { 'Crimson Fng. Gnt.', 'Dusk Gloves', 'Wonder Mitts' },	-- +20/20/12 HP
		Rings  = { 'Bomb Queen Ring', 'Toreador\'s Ring', 'Toreador\'s Ring', 'Courage Ring//IF:Ether Ring', 'Balance Ring//IF:Astral Ring', 'Tranquility Ring//IF:Atral Ring' },		-- +75/10/10 HP, the rest removes -HP rings
		Waist = { 'Powerful Rope', 'Warrior\'s Belt' },		-- +20/3 HP
		Legs  = 'Wonder Braccae',	-- +21 HP
		Feet  = { 'Creek F Clomps', 'Wonder Clomps' },		-- +35/20 HP
    },
	
--[[
	Kite is used for kiting. Emphasis should be placed on gear that increases 
	movement speed, but you might also want gear that has evasion. The choice
	is yours.
--]]

	['Kite'] = { 
		Body  = 'Ducal Aketon//TOWN-AK',				-- In case someone uses /kite in town
		Hands = 'Crimson Fng. Gnt.//IF:Dusk Gloves',	-- Remove the -3% movement speed	
	},

--[[
	The following are abilities affected by gear
--]]
	
	['Chainspell'] = {
	},
	
	['Convert'] = {
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
	
	--* DRK *--
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

	--* /THF *--
	['Steal'] = {
	},
	
	['SneakAttack'] = {
		Head  = 'Empress Hairpin',							-- +3 DEX
		Neck  = { 'Spike Necklace', 'Opo-opo Necklace' },	-- +3/3 DEX
		Body  = 'Brigandine',								-- +2 DEX
		Rings = { 'Kshama Ring No.2', 'Balance Ring' },		-- +3/2 DEX
		Waist = { 'Warwolf Belt', 'Mrc.Cpt. Belt' },		-- +5/1 DEX
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
		Rings = { 'Kshama Ring No.2', 'Kshama Ring No.3', 'Balance Ring' },	-- +3 DEX, +3 AGI, +2 DEX
		Waist = { 'Warwolf Belt', 'Mrc.Cpt. Belt' },		-- +5 DEX, +1 DEX/+1 AGI
		Feet = 'Bounding Boots',
	},
	
	['Mug'] = {
	},
	
	--* /BLM *--
	['ElementalSeal'] = {
	},
	
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
	
	--* DRG *--
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
	['SWORD']  =  { 'Guespiere','Sapara of Trials','Fencing Degen' },
	['STAVE']  =  { 'Fire Staff', 'Vulcan\'s Staff', 'Ice Staff', 'Aquilo\'s Staff',
				  'Wind Staff', 'Auster\'s Staff', 'Earth Staff', 'Terra\'s Staff',
				  'Thunder Staff', 'Jupiter\'s Staff', 'Water Staff', 'Neptune\'s Staff',
				  'Light Staff', 'Apollo\'s Staff', 'Dark Staff', 'Pluto\'s Staff' },
	['CLUB']   =  { 'Warp Cudgel', 'Ebony Wand +1', 'Rose Wand +1', 'Solid Wand', 
				  'Pilgrim\'s Wand' },
	['DAGGER'] =  { 'Garuda\'s Dagger', 'Decurion\'s Dagger' },
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
		['WAR'] = 0, ['MNK'] = 0, ['WHM'] = 1, ['BLM'] = 1, ['RDM'] = 0, ['THF'] = 0,
		['PLD'] = 0, ['DRK'] = 0, ['BST'] = 0, ['BRD'] = 0, ['RNG'] = 0, ['SMN'] = 0,
		['SAM'] = 0, ['NIN'] = 0, ['DRG'] = 0, ['BLU'] = 0, ['COR'] = 0, ['PUP'] = 0,
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
	
	-- Set your job macro toolbar defaults here. Which set depends on the subjob
	AshitaCore:GetChatManager():QueueCommand(1, '/macro book 3');		-- RDM
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
		gcdisplay.Update();		-- in case something has changed	
		return;
	end

	-- Assuming you're /bst, when you want to reward your pet and you do not have pet food 
	-- equipped or when you want to summon a pet and a jug is not equipped, the current item 
	-- in the ammo slot is saved. The following will set it back to what you had before 
	-- either of those two items were equipped.
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
	if gear.Main ~= nil then
		if gear.Main.Name == nil then
			gcinclude.MoveToCurrent(sets.Start_Weapons,sets.CurrentGear,true);
		end
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
	
	-- Now process the appropriate job ability. Start with abilities associated with RDM
	if string.match(ability.Name, 'Chainspell') then
		gcinclude.MoveToCurrent(sets.Chainspell,sets.CurrentGear);
	elseif string.match(ability.Name, 'Convert') then
		gcinclude.MoveToCurrent(sets.Convert,sets.CurrentGear);
		
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