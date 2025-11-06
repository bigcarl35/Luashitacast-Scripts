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
	This file contains all the gear sets associated with the RDM job.
	
	Gear Sets last updated: September 30, 2025
	Code update: October 10, 2025
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
	are you fighting, casting a spell, resting, etc. Reference Gear Sets will never be directly
	equipped by Luashitacast except as subsets found in Gear Sets. For example, ['rEnmity_plus']
	is a Reference Gear Set since Luashitacast will not load it directly whereas ['TP'] is a Gear
	Set that is equipped when the player is "engaged" (fighting, weapon drawn, etc). (Note: the
	"r" prefix I included in the reference gear set is a convention I use to make the Reference
	Gear Set stand out.)

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
	Since you can't guarantee which will be processed first, it's highly doubtful that what you
	expect to happen actually will happen. Now, if your groups contain different slots, then
	this is not a problem since you'll not have overlap. Just be conscious of this issue when
	you're defining your gear sets.
	
	Horizon changes for RDM from retail:
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
	The "default" gear set is what is worn when you're not fighting (neither you nor a pet)
	and you're not resting. It covers everything else: idling, traveling, in town, etc. This
	set displays what your character looks like most of the	time. This set does not
	distinguish the type of activities you're doing by default, so use inlines accordingly.

	RDM is one of the jobs that can tank. Further, it's possible you'll be strictly a caster
	or may be meleeing.
--]]

	['Default'] = {
		Main = 'Earth Staff//WSWAP',		-- -20% physical damage
		GROUP//TOWN = {
			-- You're in town, show your fancy duds
			Head  = 'Lilac Corsage',
			Neck  = 'Uggalepih Pendant',
			Ears  = { 'Loquac. Earring', 'Geist Earring' },
			Body  = { 'Ducal Aketon//TOWN-AK', 'Wlk. Tabard +1' },
			Hands = 'Wlk. Gloves +1',
			Rings = { 'Tamas Ring', 'Ether Ring' },
			Back  = 'Forager\'s Mantle',
			Waist = 'Hierarch Belt',
			Legs  = 'Warlock\'s Tights',
			Feet  = 'Wlk. Boots +1',
		},
		GROUP//NOT_TOWN = {
			GROUP//KITE = {
				SUBSET = 'Evasion',
			},
			GROUP//NOT_KITE = {
				GROUP//TANK = {
					-- Since tanking, this rules out just being a caster. If WSWAP is enable,
					-- then you don't care about TP which is unusual
					SUBSET = 'rEnmity_Plus',
					Ears =  { 'Bat Earring//BLINDED', 'Ethereal Earring', 'Energy Earring +1', 'Energy Earring +1' },
					Hands = 'Wlk. Gloves +1',
					Rings = 'Bomb Queen Ring',
					Waist = 'Warwolf Belt',
					Feet  = 'Wlk. Boots +1',
					GROUP//IDLE = {
						Head = { 'President. Hairpin//NOT_OWN//HPP.LT.94', 'Duelist\'s Chapeau' }
						Body = { 'Vermillion Cloak//MPP.LT.94', 'Wlk. Tabard +1' }
					},
					GROUP//NOT_IDLE = {
						Head =  'Duelist\'s Chapeau',
						Body =  'Scorpion Harness',
					},
				},
				GROUP//NOT_TANK = {
					-- Since not tanking, support needed for melee, casting, and both
					SUBSET = 'rEnmity_Minus',
					Neck   = { 'Opo-opo necklace//SLEPT', 'Rep.Gold Medal//NOT_OWN', 'Uggalepih Pendant//NIGHTTIME', 'Fenrir\'s Torque//DAYTIME' },
					Legs   = { 'Duelist\'s Tights', 'Errant Slops', 'Warlock\'s Tights', 'Wonder Braccae' },
					GROUP//WSWAP = {		-- This assumes you're a caster since weapon swapping kills TP
						Head   = 'Duelist\'s Chapeau',
						Ears   = { 'Bat Earring//BLINDED', 'Loquac. Earring', 'Geist Earring' },
						Body   = { 'Vermillion Cloak//MPP.LT.90', 'Vermillion Cloak//SMN_PET', 'Wlk. Tabard +1', 'Blue Cotehardie' },
						Hands  = 'Nashira Gages',
						Rings  = { 'Tamas Ring', 'Ether Ring' },
						Back   = 'White Cape',
						Waist  = 'Hierarch Belt',
						Feet   = { 'Duelist\'s Boots', 'Crimson Greaves', 'Mannequin Pumps' }
					},
					GROUP//NOT_WSWAP = {	-- Can still cast (w/o staff swaps), but assumes melee
						Head   = { 'President. Hairpin//NOT_OWN//HPP.LT.94', 'Duelist\'s Chapeau' },
						Ears   = { 'Bat Earring//BLINDED', 'Loquac. Earring', 'Ethereal Earring' },
						Body   = { 'Vermillion Cloak//MPP.LT.90', 'Wlk. Tabard +1' },
						Hands  = { 'Wlk. Gloves +1', 'Crimson Fng. Gnt.' },
						Rings  = { 'Flame Ring', 'Kshama Ring No.8' },
						Back   = 'Forager\'s Mantle',
						Waist  = 'Warwolf Belt',
						Feet   = { 'Wlk. Boots +1//SHIELD', 'Nashira Crackows' },
					},
				},
			},
		},
	},
	
--[[
	The TP set is used when you are fighting or at least weapons drawn. Tanking for a THF
	focuses on evasion tanking. Accuracy and Evasion (ACC and EVA) are applied separately from
	this set. If you want ACC or EVA gear pieces always equipped when fighting, including them
	here is the way to do it.

	Stat priority order:
		NOT_TANK: Haste, STR, Attack Power
		TANK: Defense, VIT, enmity plus, Shield Skill+, and to a lesser extent HP+ and haste
--]]

	['TP'] = {
		SUBSET = 'Default',
		GROUP//KITE = {
			SUBSET = 'Evasion',
		},
		GROUP//NOT_KITE = {
			GROUP//TANK = {
				SUBSET = 'rEnmity_Plus',
				Head   = { 'Optical Hat', 'Duelist\'s Chapeau', 'Warlock\'s Chapeau' },
				Neck   = { 'Opo-opo necklace//SLEPT', 'Spirit Torque' },
				Ears   = { 'Bat Earring//BLINDED', 'Ethereal Earring', 'Stealth Earring//SJNIN', 'Brutal Earring', 'Genin Earring//SJNIN', 'Drone Earring' },
				Body   = { 'Wlk. Tabard +1', 'Narasimha\'s Vest', 'Blue Cotehardie', 'Brigandine', 'Ctr. Scale Mail' },
				Hands  = { 'Wlk. Gloves +1', 'Battle Gloves' },
				Rings  = { 'Bomb Queen Ring', 'Kshama Ring No.4', 'Kshama Ring No.8' },
				Back   = { 'Forager\'s Mantle', 'Amemet Mantle' },
				Waist  = { 'Warwolf Belt', 'Swift Belt' },
				Legs   = { 'Duelist Tights', 'Warlock\'s Tights', 'Wonder Braccae' },
				Feet   = { 'Wlk. Boots +1//SHIELD', 'Crimson Greaves', 'Creek F Clomps' },
			},
			GROUP//NOT_TANK = {
				SUBSET = {
					[1] = 'rEnmity_Minus',
					[2] = 'rAttackPower',
				},
				GROUP//WSWAP = {		-- assumes to be a caster
					Head  = { 'Duelist Chapeau\'s', 'Warlock\'s Chapeau', 'Silver Hairpin +1' },
					Neck  = { 'Opo-opo necklace//SLEPT', 'Rep.Gold Medal//NOT_OWN', 'Uggalepih Pendant//NIGHTTIME', 'Fenrir\'s Torque//DAYTIME' },
					Ears  = { 'Loquac. Earring', 'Geist Earring', 'Energy Earring +1', 'Energy Earring +1' },
					Body  = { 'Vermillion Cloak//MPP.LT.94', 'Duelist\'s Tabard', 'Blue Cotehardie//MP.LT.40', 'Brigandine', 'Ctr. Scale Mail', 'Angler\'s Tunica' },
					Hands = { 'Nashira Gages', 'Baron\'s Cuffs' },
					Rings = { 'Tamas Ring', 'Ether Ring', 'Astral Ring', 'Astral Ring' },
					Back  = 'White Cape',
					Waist = { 'Hierarch Belt', 'Powerful Belt', 'Friar\'s Belt' },
					Legs  = { 'Duelist\'s Tights', 'Warlock\'s Tights', 'Baron\' Slops' },
					Feet  = { 'Wlk. Boots +1', 'Duelist\'s Boots', 'Mannequin Boots'},
				},
				GROUP//NOT_WSWAP = {	-- assumes to be melee
					Head  = { 'President. Hairpin//NOT_OWN//HPP.LT.94', 'Duelist\'s Chapeau', 'Warlock\'s Chapeau', 'Empress Hairpin' },
					Neck  = { 'Opo-opo necklace//SLEPT', 'Spike Necklace' },
					Ears  = { 'Ethereal Earring', 'Stealth Earring//SJNIN', 'Pilferer\'s Earring//SJTHF', 'Brutal Earring', 'Coral Earring', 'Fang Earring' },
					Body  = { 'Wlk. Tabard +1', 'Narasimha\'s Vest', 'Blue Cotehardie', 'Brigandine', 'Ctr. Scale Mail' },
					Hands = { 'Wlk. Gloves +1', 'Ogre Gloves', 'Wonder Mitts', 'Battle Gloves' },
					Rings = { 'Flame Ring', 'Kshama Ring No.8', 'Kshama Ring No.2' },
					Back  = { 'Forager\'s Mantle', 'Psilos Mantle', 'Amemet Mantle', 'Raptor Mantle' },
					Waist = { 'Warwolf Belt', 'Swift Belt' },
					Legs  = { 'Duelist Tights', 'Warlock\'s Tights', 'Wonder Braccae', 'Fisherman\'s Hose' },
					Feet  = { 'Wlk. Boots +1//SHIELD', 'Crimson Greaves', 'Wonder Clonps', 'Bounding Boots' },
				},
			},
		},
	},
	
--[[
	There are two special Reference Gear Sets: rAccuracy and rRanged_Accuracy. What makes
	them special though is how they're referenced. For the most part, they are accessed
	by slot in a "fractional" manner. By that I mean, a specific slot is pulled	into
	another set using an inline reference rather than using the whole as a subset. This
	comes with a restriction: these two sets cannot have any subsets in them. If you
	include a Subset in	their definition, it will be ignored. Avoid the frustration, just
	don't include a	subset in either the rAccuracy or rRanged_Accuracy.

	The rAccuracy gear set defines all the accuracy gear you might want to equip. It is used
	by the Progressive structure to load slots grouped by stages. You want to include
	equipment with accuracy bonus and DEX. Remember, DEX converts to accuracy (HorizonXI):
	for every 1 point of DEX you get 0.70 points of accuracy if wielding a 2H weapon, 0.65
	for a 1H weapon, and 0.60 for H2H. But, since you're dealing mostly with pets, DEX
	conversion isn't that important unless you're considering fighting too.

	Note: At the moment there's no difference between //TANK and //NOT_TANK rAccuracy.
	Maybe later gear will change that necessitating breaking out the groups.
--]]
	
	['rAccuracy'] = {
		Head  = { 'Optical Hat', 'Empress Hairpin' },		-- +10 Acc, +3 DEX
		Neck  = { 'Peacock Amulet', 'Spike Necklace' },		-- +10 Acc, +3 DEX
		Ears  = 'Beastly Earring//Pet',						-- +10 Pet Acc
		Body  = { 'Scorpion Harness', 'Brigandine' },		-- +10 Acc, +2 DEX
		Hands = { 'Wlk. Gloves +1', 'Battle Gloves' },		-- +6 DEX, +3 Acc
		Rings = { 'Toreador\'s Ring', 'Toreador\'s Ring', 'Woodsman Ring', 'Woodsman Ring', 'Jaeger Ring', 'Kshama Ring No.2' },	-- +7/+7/+5/+5/4/2 Acc
		Waist = { 'Life Belt', 'Tilt Belt', 'Mrc.Cpt. Belt' },	-- +10/5 Acc, +1 DEX
		Legs  = 'Duelist\'s Tights',						-- +5 DEX
		Feet  = 'Bounding Boots',							-- +3 DEX
    },

--[[
	rRanged_Accuracy is similar to the rAccuracy gear set, but for all ranged attacks. It's
	used by the Progressive structure to load slots grouped by stages. Unlike Accuracy, DEX
	does not convert into ranged accuracy.

	Note: At the moment there's no difference between //TANK and //NOT_TANK rRanged_accuracy.
	Maybe later gear will change that necessitating breaking out the groups.
--]]

	['rRanged_Accuracy'] = {
		Head   = 'Optical Hat',			-- +10 RAcc
		Neck   = 'Peacock Amulet',		-- +10 RAcc
		Hands  = 'Crimson Fng. Gnt.',	-- +10 RAcc
		Rings  = { 'Woodsman Ring', 'Woodsman Ring', 'Jaeger Ring' },	-- +5/5/4 RAcc
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
				['Neck'] = 'rAccuracy::Neck'.
			},
			[2] = {
				['Body']  = 'rAccuracy::Body',
				['Waist'] = 'rAccuracy::Waist',
			},
			[3] = {	
				['Rings'] = 'rAccuracy::Rings',
				['Hands'] = 'rAccuracy::Hands',
			},
			[4] = {
				['Subset'] = {
					[1] = 'Accuracy',
				},
			},
		},
		['Ranged_Accuracy'] = {
			[1] = {
				['Neck']  = 'rRanged_Accuracy::Neck',
				['Hands'] = 'rRanged_Accuracy::Hands',
			},
			[2] = { 
				['Subset'] = {
					[1] = 'rRanged_Accuracy',
				},
			},
		},
	},
  
--[[
	The Evasion set will be equipped if EVA is enabled. Remember that AGI converts to evasion: for every
	2 points of AGI you get 1 point of evasion. If you want to support a separate TANK evasion set, you
	should add Groups for both //TANK and //NON_TANK.
--]]
	
	['Evasion'] = {
        Head  = { 'Optical Hat', 'Empress Hairpin' },			-- +10/10 Eva
		Neck  = 'Spirit Torque',								-- +5 Eva
		Ears  = { 'Bat Earring//BLINDED', 'Ethereal Earring', 'Genin Earring//SJNIN', 'Drone Earring' },	-- +15 Eva while blinded, +5 Eva, +4/3 AGI
		Body  = 'Scorpion Harness',								-- +10 Eva
		Hands = 'Battle Gloves',								-- +3 Eva
		Waist = 'Swift Belt//IF:Tilt Belt',						-- filler, Tilt Belt has -5 Eva
		Feet  = { 'Duelist\'s Boots', 'Bounding Boots' },		-- +5 Eva, +3 AGI
    },
	
--[[
	rDamageTaken set is not equipped directly but rather from subsets since it's a reference set. It's a
	way to reduce a specific type of damage. As such it's optional and up to the player to decide where
	it should be included via a Subset. (Prior versions had three separate sets.)
--]]

	['rDamage_Taken'] = {
		GROUP//DT_PHYSICAL = {
			Main = 'Earth Staff//WSWAP',			-- -20% damage reduction from physical
		},
		GROUP//DT_BREATH = {
		},
		GROUP//DT_MAGICAL = {
			Ears = 'Coral Earring',					--  -1% damage reduction from magic
		},
	},
	
--[[
	The resting sets are equipped when you're resting (kneeling down). Resting_Refresh is used
	to get your MP back and Resting_Regen your HP. Which set gets priority over the other and
	what are the thresholds where the changeover occurs are defined in crossjobs.settings.
	Obviously if your subjob isn't magical in nature a THF does not care about refresh. This is
	handled	automatically. (When defining a threshhold you don't want to go with 100% because gear
	changed make that difficult to hit.)

	The rDamage_Taken set is added as a subset to reduce damage accordingly because you're in a
	vulnerable position.
--]]
	
	['Resting_Regen'] = {
		SUBSET = 'rDamage_Taken',
		Head   = 'President. Hairpin//NOT_OWN',		-- adds Regen if player in territory not owned by their nation
	},
	
	['Resting_Refresh'] = {
		SUBSET = 'rDamage_Taken',
		Main = { 'Pluto\'s Staff//WSWAP', 'Pilgrim\'s wand//WSWAP' },	-- +10/2 MP/tick while resting
		Head = 'Duelist\'s Chapeau',									-- Refresh
		Body = { 'Wlk. Tabard +1', 'Errant Hpl.', 'Vermillion Cloak' },	-- +5/5 MP/tick while resting, Refresh
		Waist = 'Hierarch Belt',										-- +2 MP/tick while resting
		Legs = 'Baron\'s Slops'											-- +1 MP/Tick while resting
	},


--[[
	Start weapons are where you define what you want the first row of equipment to look
	like when you either log in as a THF or switch your main job to THF. Any other gear
	you mention will be overridden by the Idle or Town set, so no need to include here.
--]]

	['Start_Weapons'] = {
		Ammo = { 'Hedgehog Bomb', 'Fortune Egg' }
		GROUP//WSWAP = {
			Main = { 'Earth Staff', 'Pilgrim\'s Wand' },
		},
		GROUP//NOT_WSWAP = {
			Main = 'Guespiere',
			Sub  = { 'Justice Sword//SJNIN', 'Genbu\'s Shield' },
		},
    },

--[[
	Preshot is the first stage of when a ranged shot is being performed. This is where
	you place any gear that reduces the time it takes to shoot: snap shot, rapid shot,
	quick shot, shot delay reduction, and ranged haste.
--]]

	['Preshot'] = {
    },
	
--[[
	Midshot is the second stage of a ranged shot. This is where you place
	Ranged Accuracy, Ranged Attack, Ranged Damage, Crit. Rate, Crit. Damage,
	Store TP, recycle, etc.
--]]

	['Midshot'] = {
		Main  = 'Vulcan\'s Staff//WSWAP',				-- +10 RAtt
		Ears  = 'Brutal Earring',						-- Store TP +1
		Back  = { 'Psilos Mantle', 'Amemet Mantle' },	-- +12/10 RAtt
    },

--[[
	*************************
	* Spell Casting Subsets *
	*************************

	Initially define the Reference gear sets commonly included when
	defining spell casting gear sets.
--]]

	-- Intelligence Reference gear set
	['rINT'] = {
		Main  = 'Aquilo\'s Staff//WSWAP',								-- +5 INT
		Head  = 'Warlock\'s Chapeau',									-- +3 INT
		Neck  = 'Philomath Stole',										-- +3 INT
		Body  = { 'Errant Hpl.', 'Black Cotehardie', 'Baron\'s Saio' },	-- +10/2/1 INT
		Hands = { 'Errant Cuffs', 'Duelist\'s Gloves' },				-- +5/4 INT
		Rings = { 'Tamas Ring', 'Kshama Ring No.5', 'Flame Ring' },		-- +5/3/2 INT
		Waist = { 'Penitent\'s Rope', 'Mrc.Cpt. Belt' },				-- +5/1 INT
		Legs  = 'Errant Slops',											-- +7 INT
		Feet  = { 'Wlk. Boots +1', 'Mannequin Pumps' },					-- +3/1 INT
	},
	
	-- Mind Reference gear set
	['rMND'] = {
		Main  = { 'Water Staff//WSWAP', 'Pluto\'s Staff//WSWAP', 'Light Staff//WSWAP' },	-- +4/2/1 MND
		Neck  = { 'Promise Badge', 'Justice Badge' },						-- +5/3 MND
		Ears  = 'Geist Earring',											-- +1 MND
		Body  = { 'Errant Hpl.', 'Wonder Kaftan', 'Baron\'s Saio' },		-- +10/1/1 MND
		Hands = { 'Wlk. Gloves +1', 'Baron\'s Cuffs' },						-- +2/1 MND
		Rings = { 'Tamas Ring', 'Kshama Ring No.9' },						-- +5/3 MND
		Back  = 'White Cape',												-- +2 MND
		Waist = { 'Penitent\'s Rope', 'Mrc.Cpt. Belt', 'Friar\'s Rope' },	-- +5/1/1 MND
		Legs  = { 'Errant Slops', 'Warlock\'s Tights', 'Wonder Braccae' },	-- +7/3/2 MND
		Feet  = { 'Duelist\'s Boots', 'Wlk. Boots +1', 'Mannequin Pumps' },	-- +4/3/2 MND
	},

	-- Charisma Reference gear set. Provides accuracy with singing
	['rCHR'] = {
		Main  = 'Pluto\'s Staff//WSWAP',	-- +2 CHR
		Head  = 'Entrancing Ribbon',		-- +2 CHR
		Ears  = 'Beastly Earring',			-- +2 CHR
		Body  = { 'Errant Hpl.', 'Brigandine//IF:Black Cotehardie' },	-- +10 CHR, filler to avoid -3 CHR
		Neck  = 'Flower Necklace',			-- +3 CHR
		Waist = 'Mrc.Cpt. Belt',			-- +1 CHR
		Legs  = 'Errant Slops',				-- +7 CHR
	},

	-- Enmity+ Reference gear set, for player
	['rEnmity_Plus'] = {
		Waist = 'Warwolf Belt',		-- +3 Enmity
	},

	-- Enmity- Reference gear set, for player
	['rEnmity_Minus'] = {
		Ammo  = 'Hedgehog Bomb',	-- -1 Enmity
		Hands = 'Errant Cuffs',		-- -2 Enmity
		Rings = 'Tamas Ring',		-- -5 Enmity
		Waist = 'Penitent\'s Rope',	-- -3 Enmity
		Legs  = 'Errant Slops',		-- -3 Enmity
	},

	-- Magic Attack Bonus Reference set
	['rMAB'] = {
		Neck   = 'Uggalepih Pendant//SPECIAL',		-- +8 MAB if MP < 51%
		Feet   = 'Duelist\'s Boots',				-- +4 MAB
	},

	-- Attack Power Reference set
	['rAttackPower'] = {
		Ears  = { 'Ethereal Earring', 'Coral Earring', 'Fang Earring', 'Brutal Earring' },	-- +5/5/4 Att, Store TP +1
		Hands = 'Dusk Gloves',						-- +5 Att
		Rings = 'Kshama Ring No.8',					-- +3 Att
		Back  = { 'Forager\'s Mantle', 'Psilos Mantle', 'Amemet Mantle' },	-- +15/12/B10 Att
		Waist = 'Powerful Rope//IF:SWIFT BELT'
	},

--[[
	Each type of spell can have it's own gear as well as stat based gear. In some
	cases individual spells have special entries. Understand though that for THF
	you're talking about spells from a magical subjob.

	The first stage is Precast. This is where you place any Fast Cast, cast time
	reduction, quick cast gear, and spell interruption rate down gear
--]]

	['Precast'] = {	
		Head = 'Warlock\'s Chapeau',	-- Enhances Fastcast
		Body = 'Duelist\'s Tabard',		-- Enhances Fastcast
		Ears = 'Loquac. Earring',		-- Enhances Fastcast
	},

--[[
	A lot of spells have a better chance of landing if you increase your
	magic accuracy. The Macc gear set will be equipped if MACC is toggled
	on.
--]]
	
	['Macc'] = {
		SUBSET = {
			[1] = {'rDark_Magic_Skill//MT:DARK',
				   'rElemental_Magic_Skill//MT:ELEMENTAL',
				   'rEnfeebling_Magic_Skill//MT:ENFEEBLING',
				   'rHealing_Magic_Skill//MT:OFFENSIVE_HEALING',
				   'rDivine_Magic_Skill//MT:DIVINE',
				   'rNinjutsu_Skill//MT:NINJUTSU',
				   'rCHR//MT:SINGING',					-- Charisma provides accuracy w/singing
				   },
			},
		Rings  = 'Tamas Ring',			-- +5 MAcc
		Feet   = 'Nashira Crackows',	-- +2 MAcc
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

	-- rHealing_Magic_Skill specifies gear that boosts Healing Magic Skill
	['rHealing_Magic_Skill'] = {
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
		SUBSET = {
			[1] = 'rHealing_Magic_Skill',
			[2] = 'rMND',
		},
		Body   = 'Wlk. Tabard +1//EMPTY',		-- -12% SIR
	},

--[[
	As for the offensive use of cure spells against undead monsters, 
	most of what was said about CuringMagic is true except cure potency.
	This has no effect on undead monsters.
	
	After the OffensiveCuring set is equipped, the midcast routine will 
	see if a Korin Obi can be equipped to take advantage of the 100% 
	proc rate of the day's element/weather. Also, like normal curing 
	magic, an Apollo/Light staff will be check for, but not for the
	cure potency. Rather, for magic affinity.
--]]

	['OffensiveCuring'] = {
		SUBSET = {
			[1] = 'rHealing_Magic_Skill',
			[2] = 'rMND',
			[3] = 'rMAB',
		},
		Body   = 'Wlk. Tabard +1//EMPTY',		-- -12% SIR
	},

--[[
	This set is used for all non-cure Healing Magic spells. Only 
	healing magic skill is of any importance here. You might want 
	to use this set as a subset for the other cure-based sets.
--]]

	['HealingMagic'] = {
		SUBSET = 'rHealing_Magic_Skill',
		Body   = 'Wlk. Tabard +1//EMPTY',		-- -12% SIR
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

	-- rEnhancing_Magic_Skill specifies gear that boosts Enhancing Magic Skill
	['rEnhancing_Magic_Skill'] = {
		Hands = 'Duelist\'s Gloves',	-- +15 Enhancing Magic Skill
		Legs  = 'Warlock\'s Tights',		-- +15 Enhancing Magic Skill
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
		Body   = 'Wlk. Tabard +1',		-- -12% SIR
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
		Body   = 'Wlk. Tabard +1',		-- -12% SIR
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
		SUBSET  = {
			[1] = 'rEnhancing_Magic_Skill',
			[2] = 'rINT',
			[3] = 'rMAB',
		},
		Legs   = { 'Duelist\'s Tights', 'Errant Slops', 'Enhancing_Magic_Skill::Legs' },	-- Enhances Spikes, +7 INT, Enhances Magic Skill
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
		SUBSET  = {
			[1] = 'rEnhancing_Magic_Skill',
			[2] = 'rMND',
		},
		Body   = 'Wlk. Tabard +1//NOT_IF:Errant Hpl.',	-- -12% Spell Interruption Rate
		Legs   = 'Enhancing_Magic_Skill::Legs//NOT_IF:Errant Slops'	-- Enhancement Magic Skill
	},

--[[
	Sneak's duration is variable, but the duration maxes at about 5 
	minutes. Include any gear that enhances this buff. Note: this set
	is also equipped when you use sneak oil.
--]]

	['Sneak'] = {
		SUBSET = 'rEnhancing_Magic_Skill',
		Body   = 'Wlk. Tabard +1',	-- -12% Spell Interruption Rate
		Feet   = 'Dream Boots +1',		-- Enhances Sneak
	},
	
--[[
	Invisible's duration is variable, but the duration maxes at 
	about 5 minutes. Include any gear that enhances this buff.
	Note: this set is also equipped when you use prism powder.
--]]	

	['Invisible'] = {
		SUBSET = 'rEnhancing_Magic_Skill',
		Body  = 'Wlk. Tabard +1',	-- -12% Spell Interruption Rate
		Hands = 'Dream Mittens +1',	-- Enhances Invisible
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
		Body   = 'Wlk. Tabard +1',		-- -12% Spell Interruption Rate
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
		Body   = 'Wlk. Tabard +1',		-- -12% Spell Interruption Rate
		Rings  = { 'Dilation Ring//SP:Refresh', 'Dilation Ring//SP:Haste', 'Enmity_Minus::Rings' },
		Hands  = 'Enmity_Minus::Hands',
		Waist  = 'Enmity_Minus::Waist',
	},

--[[
	****************************
	* Midcast: Elemental Magic *
	****************************
--]]

	-- rElemental_Magic_Skill specifies gear that boosts Elemental Magic Skill
	['rElemental_Magic_Skill'] = {
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
		SUBSET = {
			[1] = 'rEnmity_Minus',
			[2] = 'rElemental_Magic_Skill',
			[3] = 'rINT',
			[4] = 'rMAB',
		},
		Body   = 'Wlk. Tabard +1//EMPTY',		-- -12% SIR
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
		SUBSET = {
			[1] = 'rEnmity_Minus',
			[2] = 'rElemental_Magic_Skill',
			[3] = 'rINT',
		},
		Body   = 'Wlk. Tabard +1//EMPTY',		-- -12% SIR
	},

--[[
	**********************
	* Midcast: Summoning *
	**********************
--]]

	-- rSummoning_Magic_Skill specifies gear that boosts Summoning Magic Skill
	['rSummoning_Skill'] = {
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
		SUBSET = 'rSummoning_Skill',
		Body   = 'Wlk. Tabard +1//EMPTY',		-- -12% SIR
	},

--[[
	***********************
	* Midcast: Dark Magic *
	***********************
--]]
	
	-- rDark_Magic_Skill specifies gear that boosts Dark Magic Skill
	['rDark_Magic_Skill'] = {
		Hands = 'Crimson Fng. Gnt.'			-- +10 Dark Magic Skill
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
		SUBSET  = {
			[1] = 'rEnmity_Minus',
			[2] = 'rDark_Magic_Skill',
		},
		Body    = 'Wlk. Tabard +1',		-- -12% SIR
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
		SUBSET = {
			[1] = 'rEnmity_Minus',
			[2] = 'rDark_Magic_Skill'
		},
		Body   = 'Wlk. Tabard +1',		-- -12% SIR
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
		SUBSET = {
			[1] = 'rEnmity_Minus',
			[2] = 'rDark_Magic_Skill',
		},
		Body   = 'Wlk. Tabard +1'		-- -12% SIR
	},

--[[
	This last gear set, DarkMagic, covers all Dark Magic spells not covered
	by the previous three gear sets. 
--]]
	
	['DarkMagic'] = {
		Subset = {
			[1] = 'rEnmity_Minus',
			[2] = 'rDark_Magic_Skill',
			[3] = 'rMAB',
		},
		Body   = 'Wlk. Tabard +1',				-- -12% SIR
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


	-- rDivine_Magic_Skill specifies gear that boosts Divine Magic Skill
	['rDivine_Magic_Skill'] = {
		Main   = 'Water Staff//WSWAP',			-- +10 Divine Magic Skill
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
		SUBSET = {
			[1] = 'rEnmity_Minus',
			[2] = 'rDivine_Magic_Skill',
			[3] = 'rMAB',
		},
		Body   = 'Wlk. Tabard +1',				-- -12% SIR
	},
	
--[[
	Enfeebling divine spell (flash) afflicts the target with accuracy 
	reduction (similar to blind) with a weakening effect over time till
	it runs out. Duration is subject to resists and partial resists
	although can last 12 seconds if not resisted. It also generates a
	significant amount of volitile and cumulative enmity.
--]]	
	
	['EnfeebleDivine'] = {
		SUBSET = {
			[1] = 'rEnmity_Minus',
			[2] = 'rDivine_Magic_Skill',
			[3] = 'rMAB',
		},
		Main   = 'Water Staff//WSWAP',			-- +10 Divine Magic Skill	
		Body   = 'Wlk. Tabard +1',				-- -12% SIR
		Neck   = 'Enfeebling Torque',			-- Ugg. Pen. from rMab, but will be overridden by torque
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
		SUBSET = {
			[1] = 'rEnmity_Minus',
			[2] = 'rDivine_Magic_Skill',
			[3] = 'rMAB',
		},
		Main   = 'Water Staff//WSWAP',
		Body   = 'Wlk. Tabard +1',				-- -12% SIR
		Neck   = 'Enfeebling Torque',
	},

--[[
	*****************************
	* Midcast: Enfeebling Magic *
	****************************
--]]

	-- rEnfeebling_Magic_Skill specifies gear that boosts Enfeebling Magic Skill
	['rEnfeebling_Magic_Skill'] = {
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
		SUBSET = {
			[1] = 'rEnfeebling_Magic_Skill',
			[2] = 'rINT',
		},
		Body   = 'Wlk. Tabard +1//IF:Baron\'s Saio'
	},
	
	
	['EnfeeblingMND'] = {
		SUBSET = {
			[1] = 'rEnfeebling_Magic_Skill',
			[2] = 'rMND',
		},
		Body   = 'Wlk. Tabard +1//UNLESS:Errant Hpl.'
	},

	['EnfeeblingMagic'] = {
		SUBSET = 'rEnfeebling_Magic_Skill',
	},
	
--[[
	********************
	* Midcast: Singing *
	********************

--]]

	-- rSinging_Skill specifies gear that boosts Songs in general
	['rSinging_Skill'] = {	-- Covers both Singing Skill and Intrument Skill
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
		SUBSET = {
			[1] = 'rSinging_Skill',
			[2] = 'rCHR',
		},
	},
	
--[[
	EnfeeblingSinging contains gear that debuffs targets. Included are: requiem,
	threnody, lullaby, finale, elegy, and virelai.
--]]
	
	['EnfeeblingSinging'] = {
		SUBSET = {
			[1] = 'rSinging_Skill',
			[2] = 'rCHR'
		},
	},

--[[
	********************
	* Midcast: Ninjusu *
	********************
--]]

	-- rNinjutsu_Skill specifies gear that boosts Ninjutsu Skill
	['rNinjutsu_Skill'] = {
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
		SUBSET = {
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

	The following weapon skill gearsets are defined by the stat they emphasize.
	Listed are all of the sets that you will need to use every weapon skill that
	your job can do. The leading comment defines what weapon/weapon	skill
	combination the set applies to.
	
	RDM can use the following weapons: Dagger (B), Sword (B), Club (D), Archery (E)
		
	Please note that on HorizonXI you may have access to some weapon skills
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
	
--[[
		* Strength based *
		
		Dagger: Mercy Stroke		
		Sword: Flat Blade,Circle Blade,Vorpal Blade
		Club: Starlight,Skull Breaker,True Strike
-]]
	
	['WS_STR'] = {
		SUBSET = 'rAttackPower',
		Neck   = 'Spike Necklace',							-- +3 STR
		Body   = { 'Narasimha\'s Vest', 'Black Cotehardie', 'Wonder Kaftan' },		-- +3/3/1 STR
		Hands  = { 'Ogre Gloves', 'Wonder Mitts' },			-- +6/3 STR
		Rings  = { 'Flame Ring', 'Kshama Ring No.8' },		-- +5/3 STR
		Back   = { 'Forager\'s Mantle', 'Amemet Mantle//EMPTY' },	-- +3/1 STR
		Waist  = { 'Warwolf Belt', 'Mrc.Cpt. Belt' },		-- +5/1 STR
		Legs   = 'Wonder Braccae',							--  +1 STR
		Feet   = { 'Creek F Clomps', 'Wonder Clomps' },		-- +4/2 STR
    },
	
--[[
		* Strength and Agility based, ranged *
		
		Archery: Flaming Arrow^,Piercing Arrow^,Dulling Arrow^,Sidewinder^,Blast Arrow^
		
		^ Subjob must be RNG
--]]

	['WS_RANGED_STRAGI'] = {
		SUBSET = 'AttackPower',
		Head   = 'Empress Hairpin',											-- +3 AGI
		Neck   = 'Spike Necklace',											-- +3 STR
		Ears   = { 'Genin Earring//SJNIN', 'Drone Earring' },				-- +4 AGI SJ NIN, +3 AGI
		Body   = { 'Black Cotehardie', 'Wonder Kaftan' },					-- +3 STR/+3 AGI
		Hands  = { 'Ogre Gloves', 'Wonder Mitts' },							-- +6/3 STR
		Rings  = { 'Flame Ring', 'Kshama Ring No.8', 'Kshama Ring No.3' },	-- +5/3 STR, +3 AGI
		Back   = { 'Forager\'s Mantle', 'Amemet Mantle//EMPTY' },			-- +3/1 STR
		Waist  = { 'Warwolf Belt', 'Mrc.Cpt. Belt' },						-- +5 STR, +1 STR/+1 AGI
		Legs   = 'Wonder Braccae',											-- +1 STR
		Feet   = { 'Creek F Clomps', 'Wlk. Boots +1', 'Bounding Boots' }	-- +4 STR, +3/3 AGI
    },

--[[
		* Strength and Dexterity based, even weighting *
		
		Sword: Fast Blade	
--]]

	['WS_STRDEX'] = {
		SUBSET = 'rAttackPower',
		Head   = 'Empress Hairpin',											-- +3 DEX,
		Neck   = 'Spike Necklace',											-- +3 STR
		Body   = { 'Black Cotehardie', 'Brigandine' },						-- +3 STR/+2 DEX, +2 DEX
		Hands  = { 'Wlk. Gloves +1', 'Ogre Gloves', 'Wonder Mitts' },		-- +6 DEX, +6/3 STR
		Rings  = { 'Flame Ring', 'Kshama Ring No.8', 'Kshama Ring No.2' },	-- +5/3 STR, +3 DEX
		Back   = { 'Forager\'s Mantle', 'Amemet Mantle//EMPTY' },			-- +3/1 STR
		Waist  = { 'Warwolf Belt', 'Mrc.Cpt. Belt' },						-- +5 STR, +1 STR/+1 DEX
		Legs   = { 'Duelist\'s Tights','Wonder Braccae' },					-- +5 DEX, +1 STR
		Feet   = { 'Creek F Clomps', 'Bounding Boots' },					-- +4 STR, +3 DEX
    },

--[[
		* Strength and Intelligence based, even weighting *
		
		Sword: Burning Blade,Red Lotus Blade
--]]
	
	['WS_STRINT'] = {
		SUBSET = 'rAttackPower',
		Head   = 'Warlock\'s Chapeau',										-- +3 INT
		Neck   = 'Spike Necklace',											-- +3 STR
		Body   = { 'Errant Hpl.', 'Black Cotehardie', 'Wonder Kaftan', 'Baron\'s Saio' },	-- +10 INT, +3 STR/+2 DEX, +1 STR, +1 INT
		Hands  = { 'Errant Cuffs', 'Duelist\'s Gloves', 'Wonder Mitts' },	-- +5/4 INT, +3 STR
		Rings  = { 'Flame Ring', 'Tamas Ring', 'Kshama Ring No.8', 'Kshama Ring No.5' },	-- +5 STR/+2 INT, +3 STR, +3 INT
		Back   = { 'Forager\'s Mantle', 'Amemet Mantle' },					-- +3/1 STR
		Waist  = { 'Warwolf Belt', 'Penitent\'s Rope', 'Mrc.Cpt. Belt' },	-- +5 STR, +5 INT, +1 STR/+1 INT
		Legs   = { 'Errant Slops', 'Wonder Braccae' },						-- +7 INT, +1 STR
		Feet   = { 'Creek F Clomps', 'Wlk. Boots +1', 'Wonder Clomps', 'Mannequin Pumps' },	-- +4 STR, +3 INT, +2 STR, +1 INT
	},

--[[
		* Strength and Mind based, even weighting *
		
		Sword: Shining Blade,Seraph Blade,Swift Blade,Savage Blade,
			   Knights of Round
		Club: Shining Strike
--]]

	['WS_STRMND'] = {
		SUBSET = 'rAttackPower',
		Ears   = { 'Geist Earring' },								-- +1 MND
		Neck   = { 'Promise Badge', 'Justice Badge' },				-- +5/3 MND
		Body   = { 'Errant Hpl.', 'Wonder Kaftan', 'Baron\'s Saio' },			-- +10 MND, +1 STR/+1 MND, +1 MND
		Hands  = { 'Wonder Mitts', 'Baron\'s Cuffs//EMPTY' },		-- +2/1 MND
		Rings  = { 'Tamas Ring', 'Flame Ring', 'Kshama Ring No.8', 'Kshama Ring No.9' },	-- +5 MND, +5/3 STR, +3 MND
		Back   = { 'Forager\'s Mantle', 'White Cape', 'Amemet Mantle//EMPTY' },	-- +3 STR, +2 MND, +1 STR
		Waist  = { 'Warwolf Belt', 'Penitent\'s Rope', 'Mrc.Cpt. Belt', 'Friar\'s Rope' },	-- +5 STR, +5 MND, +1 STR/+1 MND, +1 MND
		Legs   = { 'Errant Slops', 'Warlock\'s Tights', 'Wonder Braccae' },		-- +7/3 MND, +2 MND/+1 STR
		Feet   = { 'Duelist\'s Boots', 'Creek F Clomps', 'Wlk. Boots +1', 'Wonder Clomps' }		-- +4 MND, +4 STR, +3 MND, +2 STR
    },
	
--[[
		* Charisma based *
		
		Dagger: Shadowstitch
--]]
	
	['WS_CHR'] = {
		SUBSET = {
			[1] = 'rAttackPower',
			[2] = 'rCHR'
		},
    },
	
--[[
		* Dexterity based *
		
		Dagger: Wasp Sting,Viper Bite,Eviseration
--]]
	
	['WS_DEX'] = {
		SUBSET = 'rAttackPower',
		Head   = 'Empress Hairpin',		-- +3 DEX
		Neck   = 'Spike Necklace',		-- +3 DEX
		Body   = { 'Black Cotehardie', 'Brigandine' },	-- +2/2 DEX
		Hands  = 'Wlk. Gloves +1',		-- +6 DEX
		Rings  = 'Kshama Ring No.2',	-- +3 DEX
		Waist  = { 'Warwolf Belt', 'Mrc.Cpt. Belt' },		-- +5/1 DEX
		Legs   = 'Duelist\'s Tights',	-- +5 DEX
		Feet   = 'Bounding Boots'		-- +3 DEX
    },

--[[
		* Dexterity and Agility based *
		
		Dagger: Eviseration
--]]
	
	['WS_DEXAGI'] = {
		SUBSET = 'rAttackPower',
		Head   = 'Empress Hairpin',		-- +3 DEX/+3 AGI
		Neck   = 'Spike Necklace',		-- +3 DEX
		Ears   = { 'Genin Earring//SJNIN', 'Drone Earring' },	-- +4 AGI if SJ NIN, +3 AGI
		Body   = { 'Black Cotehardie', 'Brigandine' },			-- +3 AGI/+2 DEX, 2 DEX
		Hands  = 'Wlk. Gloves +1',		-- +6 DEX
		Rings  = { 'Kshama Ring No.3', 'Kshama Ring No.2' },	-- +3 AGI, +3 DEX
		Waist  = { 'Warwolf Belt', 'Mrc.Cpt. Belt' },			-- +5 DEX, +1 DEX/+1 AGI
		Legs   = 'Duelist\'s Tights',	-- +5 DEX
		Feet   = 'Bounding Boots'		-- +3 DEX/+3 AGI
    },
	
--[[
		* Dexterity and Intelligence based *
		
		Dagger: Gust Slash,Cyclone
--]]
	
	['WS_DEXINT'] = {
		SUBSET = 'rAttackPower',
		Head   = { 'Warlock\'s Chapeau', 'Empress Hairpin' },		-- +3 INT, +3 DEX
		Neck   = 'Spike Necklace',		-- +3 DEX
		Body   = { 'Errant Hpl.', 'Black Cotehardie', 'Brigandine' },	-- +10 INT, +2 DEX/+2 INT
		Hands  = { 'Wlk. Gloves +1', 'Errant Cuffs', 'Duelist\'s Gloves' },		-- +6 DEX, +5/2 INT
		Rings  = { 'Tamas Ring', 'Kshama Ring No.5', 'Kshama Ring No.2' },	-- +5/3 INT, +3 DEX
		Waist  = { 'Warwolf Belt', 'Penitent\'s Rope', 'Mrc.Cpt. Belt' },	-- +5 DEXm +5 INT, +1 DEX/+1 INT
		Legs   = 'Errant Slops',	-- +7 INT
		Feet   = { 'WLK. Boots +1', 'Bounding Boots' }	-- +3 INT, +3 DEX
    },
	
--[[
		* Mind based *
		
		Dagger: Energy Steal,Energy Drain
--]]

	['WS_MND'] = {
		SUBSET = {
			[1] = 'rAttackPower',
			[2] = 'rMND'
		},
    },
	
--[[
		* Skill based *
		
		Club: Starlight
--]]

	['WS_Skill'] = {
		SUBSET = 'rAttackPower',
    },
	
--[[
		* HP based *
		
		Sword: Spirits Within
--]]

	['WS_HP'] = {
		SUBSET = 'rAttackPower',
		Ammo   = 'Happy Egg',													-- +1% HP
		Head   = 'Duelist\'s Chapeau//IF:Empress Hairpin',						-- filler in case to offset -15 HP of Empress hairpin
		Neck   = 'Promise Badge',												-- 10 HP
		Ears   = { 'Physical Earring//MP.GE.15', 'Physical Earring//MP.GT.40', 'Ethereal Earring' },	-- Convert 25 MP to HP x2, +15 HP
		Body   = 'Wonder Kaftan',												-- +36 HP
		Hands  = { 'Crimson Fng. Gnt.', 'Dusk Gloves', 'Wonder Mitts' },		-- +20/20/12 HP
		Rings  = { 'Bomb Queen Ring', 'Toreador\'s Ring', 'Toreador\'s Ring' },	-- +75/10/10 HP
		Waist  = 'Powerful Rope',												-- +20 HP
		Legs   = 'Wonder Braccae',												-- +21 HP
		Feet   = { 'Creek F Clomps', 'Wonder Clomps' },							-- +35/20 HP
    },

--[[
	Custom weaponskill sets can be used in place of the generic stats-based sets. You must name
	your custom set ['WS:skill'] where "skill" is the name of the weapon skill. If there's a blank
	in the name, substitue an underscore.

	Example: a custom set for "viper bite" would be named:	['WS:Viper_Bite'].

	Note: how you capitalize the name is up to you.
--]]

--[[
	The following are your main job (summoner) abilities. Unlike sub job abilities, this section
	will explicitly list all of your abilities. Please note that all abilities will be prefixed
	with an 'A:'. This is to ensure there's no conflict with any other predefined gear set (this
	is a bigger issue with subjob abilities than with main jobs.)
--]]
	
	['A:Chainspell'] = {
	},
	
	['A:Convert'] = {
	},
		
--[[
	All abilities associated with any subjob (up to level 37) are supported, whether they make sense to
	do or not. Instead of explicitly listing all the abilities, you define the gear sets for the
	abilities you wish to support. Like custom weaponskills there's a naming convention. For a subjob
	ability (like a main job ability) you want to prefix the ability name with a 'A:' and if the ability
	contains a space, replace with an underscore.

	Example: if you want to support SAM's Third Eye, you would name the set: A:Third_Eye

	Note: how you capitalize the name is up to you.
--]]
	--* BST *--
	-- CHR and Charm + gear. (Every +1 Charm adds 5% Charm duration)
	['A:Charm'] = {
	    SUBSET = 'rCHR',
    },
	
	--* /THF *--
	-- if only Sneak Attack is enabled, the following will be equipped
	['A:Sneak_Attack'] = {
		Head  = 'Empress Hairpin',							-- +3 DEX
		Neck  = { 'Spike Necklace', 'Opo-opo Necklace' },	-- +3/3 DEX
		Body  = 'Brigandine',								-- +2 DEX
		Rings = 'Kshama Ring No.2',							-- +3 DEX
		Waist = { 'Warwolf Belt', 'Mrc.Cpt. Belt' },		-- +5/1 DEX
		Feet  = 'Bounding Boots',							-- +3 DEX
	},

	-- If only Trick Attack is enabled, the following will be equipped
	['A:Trick_Attack'] = {
		Head  = 'Empress Hairpin',							-- +3 AGI
		Ears  = { 'Genin Earring//SJNIN', 'Drone Earring' },-- +4 AGI if sj NIN, +3 AGI
		Rings = 'Kshama Ring No.3',							-- +3 AGI
		Waist = 'Mrc.Cpt. Belt',							-- +1 AGI
		Feet  = 'Bounding Boots',							-- +3 AGI
	},

	-- When both Sneak Attack and Trick Attack are enabled, the following will be equipped
	['A:SATA'] = {
		Head = 'Empress Hairpin',							-- +3 DEX/+3 AGI
		Neck = 'Spike Necklace',							-- +3 DEX
		Ears = { 'Genin Earring//SJNIN', 'Drone Earring' },	-- +4 AGI if sj NIN, +3 AGI
		Body = 'Brigandine',								-- +2 DEX
		Rings = { 'Kshama Ring No.2', 'Kshama Ring No.3' },	-- +3 DEX, +3 AGI
		Waist = { 'Warwolf Belt', 'Mrc.Cpt. Belt' },		-- +5 DEX, +1 DEX/+1 AGI
		Feet = 'Bounding Boots',
	},

--[[
	Pet commands can also be made into a gear set. Unlike abilities with an 'A-' prefix,
	pet commands use the 'PC:' prefix. By default all the pet commands (except for
	blood pacts) are predefined for: BST, DRG and SMN.

	Note: commands like BST's SIC and READY and SMN's Blood Pact actually are identified
	by the skill they invoke. This means that the type of skill is what is processed
	and not the actual command. Defined in utilities.lua are the BST skill according to
	type. Blood pacts are handled separately by PreBP and MidBP, so not included here.
--]]

	--* /BST *--
	['PC:Reward']
	},

	['PC:Fight'] = {
	},

	-- This structure is for the Sic and Ready command, by skill type
	['PC:Sic_Ready'] = {
		GROUP//BST_PET_ATTACK = {
		},
		GROUP//BST_PET_MATT = {
		},
		GROUP//BST_PET_MACC = {
		},
	},

	--* DRG *--

	['PC:Steady_Wing'] = {
	},

	--* SMN *--

	['PC:Assault'] = {
	},

--[[
	If you want to create any custom gear sets, those you'd use with the /gs command, include
	the gear set definitions here. (There's no naming convention, call them what you want, but
	try to avoid any set names defined above.)
--]]
	
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

-- WeaponType lets the player identify what weapon the use by type. There's no way to consistently
-- identify the type of weapon name, so you need to explicitly them here. Note: You only need the
-- names of weapons whose type can be taken advantage of in the "WS:Skill" gear set.
profile.WeaponType = {
	['SWORD']  =  { 'Guespiere', 'Tutelary', 'Fencing Degen' },
	['STAVE']  =  { 'Fire Staff', 'Vulcan\'s Staff', 'Ice Staff', 'Aquilo\'s Staff',
				  'Wind Staff', 'Auster\'s Staff', 'Earth Staff', 'Terra\'s Staff',
				  'Thunder Staff', 'Jupiter\'s Staff', 'Water Staff', 'Neptune\'s Staff',
				  'Light Staff', 'Apollo\'s Staff', 'Dark Staff', 'Pluto\'s Staff' },
	['CLUB']   =  { 'Warp Cudgel', 'Ebony Wand +1', 'Rose Wand +1', 'Solid Wand', 
				  'Pilgrim\'s Wand' },
	['DAGGER'] =  { 'Garuda\'s Dagger', 'Decurion\'s Dagger' },
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
	MacroBook = 3;						-- Which macro book should be equipped for RDM
};

-- Table of gear to put a delay on
profile.TrackedGear = {
	[1] = { ['item'] = 'Vermillion Cloak', ['slot'] = 'Body', ['delay'] = 10 },
};

--[[
	********
	* Code *
	********

	SetSubjobSet is used to pick the appropriate set for the loaded macrobook based on
	which subjob is current.

	Parameter
		chkSJ		player's subjob
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
		(profile.settings.sjb ~= nil and profile.settings.sjb == chkSJ) or
		profile.settings.bAutoMacrobook_page == false then
		return;
	end

	-- Compare the stored subjob with the current subjob
	if profile.settings.sjb == nil or chkSJ ~= profile.setting.sjb then
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

	-- Set your job macro toolbar defaults here. Which set depends on the subjob
	if profile.settings.bAutoMacrobook_page == true then
		AshitaCore:GetChatManager():QueueCommand(1, '/macro book ' .. tostring(profile.settings.MacroBook));	-- RDM macro book
		if profile.settings.bJustMacroBook == false then
			SetSubjobSet(player.SubJob);
		end
	end

	-- Load up the weapons bar. (This need only be done once.)
	gear.MoveToDynamicGS(profile.Sets.Start_Weapons,crossjobs.Sets.CurrentGear,false,'Start_Weapons');
	gear.EquipTheGear(sets.CurrentGear,false);

	-- Make sure the saved weapons are the starting weapons
	gear.weapon = crossjobs.Sets.CurrentGear['Main'];
	gear.offhand = crossjobs.Sets.CurrentGear['Sub'];
end		-- OnLoad

--[[
	oad is run when you change to another job
--]]

function profile.OnUnload()
	utilities.Unload();
end		-- OnUnload

--[[
	HandleCommand is run when you type in a command defined in LUASHITACAST. The commands handled here instead
	of in gcinclude.HandleCommands are specific to BST or the help system, which has been tailored to BST.
--]]

function profile.HandleCommand(args)
	if args[1] == 'help' then
		help.ShowHelp();
	elseif args[1] == 'petfood' then			-- Supported since pet food is not job specific, but very niche
		pets.doPetFood(args[2],true);
	else
		crossjobs.HandleCommands(args);
	end
end		-- HandleCommand

--[[
	HandlePetAction equips the appropriate gear set based on the type of action
	the pet is trying to perform.

	Parameter
		PetAction		What action has your pet done
--]]

function HandlePetAction(PetAction)

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
	local ew = gData.GetEquipment();
	local bSA = utilities.fBuffed('Sneak Attack');
	local bTA = utilities.fBuffed('Trick Attack');
	local eWeap = nil;
	local bIgnoreLocks;
	local cKey;

	utilities.StartReminder();		-- See if reminder should be printed
	
	-- Make sure the macro set is shown and that the display on the top of the screen is correct
	-- in case the subjob was changed.
	SetSubjobSet(player.SubJob);
	displaybar.UpdateBarStatic();

	-- Only gear swap if this flag is true
	if utilities.fGetToggle('GSwap') == false then
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

	-- Assuming you're /bst, when you want to reward your pet and you do not have pet food
	-- equipped, the current item in the ammo slot is saved. The following will set it back
	-- to what you had before unless the slot is locked.
	if player.SubJob == 'BST' and
		profile.settings.bAmmo == true and
		utilities.fIsLocked('ammo') == false then
		gFunc.ForceEquip('Ammo',profile.settings.sAmmo);
		profile.settings.sAmmo = nil;
		profile.settings.bAmmo = false;
	end
	
	-- Clear out the CurrentGear in case of leftovers
	gcinclude.ClearSet(sets.CurrentGear);
	
	-- If player is not resting and has MP and has swapped weapons, set the weapon back to what 
	-- they had before the switch
	if player.Status ~= 'Resting' and 
		 utilities.fGetToggle('WSwap') == true and
		 gear.weapon ~= nil and
		 eWeap ~= gear.weapon then
		if locks.fIsSlotLocke('main') == false then
			crossjobs.Sets.CurrentGear['Main'] = gear.weapon;
		end
		if locks.fIsSlotLocked('sub') == false then
			crossjobs.Sets.CurrentGear['Sub'] = gear.offhand;
		end
	end
	
	-- Now process the player status accordingly
	if player.Status == 'Engaged' then
		if bSA == true or bTA == true then
			-- If sneak attack or trick attack up, make sure the appropriate gear set is
			-- equipped to maximize the damage. Note that if a weapon skill follows, the
			-- weapon skill set will take priority.
			gear.MoveToDynamicGS(profile.Sets.SATA,crossjobs.Sets.CurrentGear,false,'SATA');
		elseif bSA == true then						-- SA
			gear.MoveToDynamicGS(profile.Sets.SneakAttack,crossjobs.Sets.CurrentGear,false,'SA');
		elseif bTA == true then						-- TA
			gear.MoveToDynamicGS(profile.Sets.TrickAttack,crossjobs.Sets.CurrentGear,false,'TA');
		else	
			gear.MoveToDynamicGS(profile.Sets.TP,crossjobs.Sets.CurrentGear,false,'TP');
			profile.settings.priorityEngaged = string.upper(profile.settings.priorityEngaged);
			for i = 1,string.len(profile.settings.priorityEngaged),1 do
				cKey = string.sub(profile.settings.priorityEngaged,i,i);
				if cKey == 'C' then		-- Evasion
					if utilities.fGetToggle('Eva') == true then
						gear.MoveToDynamicGS(profile.Sets.Evasion,crossjobs.Sets.CurrentGear,false,'Evasion');
					end
				elseif cKey == 'E' then		-- Accuracy	
					crossjobs.ProgressiveAccuracy('Acc');
				end
			end				
		end
	elseif player.Status == 'Resting' then	
		-- Player kneeling. Priority (low to high): Resting,refresh
		if profile.settings.bPrioityRefresh == true then
			if player.MP < player.MaxMP then
				gear.MoveToDynamicGS(profile.Sets.Resting_Refresh,crossjobs.Sets.CurrentGear),false,'Resting_Refresh';
			elseif player.HP < player.MaxHP then
				gear.MoveToDynamicGS(profile.Sets.Resting_Regen,crossjobs.Sets.CurrentGear,false,'Resting_Regen');
			end
		else
			if player.HP < player.MaxHP then
				gear.MoveToDynamicGS(profile.Sets.Resting_Regen,crossjobs.Sets.CurrentGear,false,'Resting_Regen');
			elseif player.MP < player.MaxMP then
				gear.MoveToDynamicGS(profile.Sets.Resting_Refresh,crossjobs.Sets.CurrentGear),false,'Resting_Refresh';
			end
		end
	else
		-- Assume idling. While there's no idle set, just use the "Default" set
		gear.MoveToDynamicGS(profile.Sets.Default,crossjobs.Sets.CurrentGear,false,'Default');
	end
		
	-- In case the pet is a summoned pet...
	if pets.fSummonerPet() == true then
		local sStave = gear.fCheckForElementalGearByValue('staff','Summons',pet.Name);
		if sStave ~= nil then
			gear.fSwapToStave(sStave,false,crossjobs.Sets.CurrentGear);
		end
	end
	
	-- And make sure a weapon equipped. (Going into a capped area can cause no weapon to be equipped.)
	local tgear = gData.GetEquipment();
	if tgear.Main == nil or tgear.Main.Name == nil then
		gear.MoveToDynamicGS(profile.Sets.Start_Weapons,crossjobs.Sets.CurrentGear,true,'Start_Weapons');
	end
	
	gear.EquipTheGear(crossjobs.Sets.CurrentGear,bIgnoreLocks);
					
	-- Lastly, update the display, just in case
	displaybar.UpdateBarStatic();
end		-- HandleDefault

--[[
	HandleAbility is used to change the player's gear appropriately.
--]]

function profile.HandleAbility()
	local ability = gData.GetAction();
	local sj = player.SubJob;
			
	if utilities.fGetToggle('GSwap') == false then
		return;
	end

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
	HandlePrecast loads Fast Cast, cast time reduction, and quick cast gear in anticipation of a spell
--]]

function profile.HandlePrecast()
		
	-- Only gear swap if this flag is true
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

	if utilities.fGetToggle('GSwap') == false then
		return;
	end

	-- Call the common HandleMidcast now
	magic.HandleMidcast();
end		-- HandleMidcast

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
