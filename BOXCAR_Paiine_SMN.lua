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
	This file contains all the gear sets associated with the SMN job.
	
	Gear Sets last updated: October 24, 2025
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
	
	Horizon changes for SMN from retail:
		- Some AF have been moderately changed
		- Significant changes have been made to the Claustrum
		- Whispering Wind has erase effect added
		- Spring Water gives AoE 2 mp/tic refresh
		- Crimson Howl lasts 3 minutes
		- Ecliptic Growl and Ecliptic Howl have been swapped, level 54 and 43 now
		
		- FYI: not noted, but top level elemental blood pact rage skills do not weapon skillchain
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
		Main   = 'Earth Staff//NOT_SMN_PET',		-- -20% physical damage
		Ammo   = 'Hedgehog Bomb',
		GROUP//TOWN = {
			-- You're in town, show your fancy duds
			Head  = 'Lilac Corsage',
			Neck  = 'Uggalepih Pendant',
			Ears  = { 'Loquac. Earring', 'Geist Earring' },
			Body  = { 'Ducal Aketon//TOWN-AK', 'Yinyang Robe' },
			Hands = 'Smn. Bracers +1',
			Rings = { 'Evoker\'s Ring', 'Tamas Ring' },
			Back  = 'Blue Cape',
			Waist = 'Hierarch Belt',
			Legs  = 'Evk. Spats +1',
			Feet  = 'Evk. Pigaches +1',
		},
		GROUP//NOT_TOWN = {
			GROUP//KITE = {
				SUBSET = 'Evasion',
			},
			GROUP//NOT_KITE = {
				SUBSET = 'rEnmity_Minus',
				Neck  = { 'Rep.Gold Medal//NOT_OWN','Uggalepih Pendant//NIGHTTIME', 'Fenrir\'s Torque//DAYTIME', 'Star Necklace', 'Spirit Torque', 'Justice Badge' },
				Ears  = { 'Bat Earring//BLINDED', 'Loquac. Earring', 'Coral Earring//DT_MAGICAL', 'Bat Earring', 'Energy Earring +1', 'Energy Earring +1' },
				Rings = { 'Evoker\'s Ring', 'Tamas Ring', 'Ether Ring', 'Astral Ring', 'Astral Ring' },
				Back  = { 'Blue Cape', 'White Cape' },
				Waist = { 'Hierarch Belt', 'Powerful Rope', 'Friar\'s Rope' },
				Ammo  = { 'Hedgehog Bomb', 'Fortune Egg' },
				GROUP//SMN_PET = {			-- has a SMN pet
					Head   = { 'Smn. Horn +1//SMNPETMW', 'Austere Hat', 'Silver Hairpin +1' },
					Hands  = { 'Carbuncle Mitts//PETNAME:Carbuncle','Nashira Gages', 'Shep. Bracers' },
					Body   = { 'Yinyang Robe//MPP.LT.94', 'Summoner\'s Dblt.//SMNPETMD', 'Yinyang Robe', 'Vermillion Cloak' },
					Legs   = { 'Summoner\'s Spats//SPIRIT:EP', 'Shep. Hose' },
					Feet   = 'Evk. Pigaches +1',
				},
				GROUP//NOT_SMN_PET = {		-- has a pet, but not a SMN pet. In case of /BST or /PUP, /DRG wyvern not an issue
					Head   = 'Smn. Horn +1',
					Hands  = 'Shep. Bracers',
					Body   = { 'Yinyang Robe//MPP.LT.94', 'Vermillion Cloak//MPP.LT.94' },
					Legs   = { 'Summoner\'s Spats//SPIRIT:EP', 'Shep. Hose' },
					Feet   = 'Evk. Pigaches +1',
				},
				GROUP//NOT_PET = {			-- no pet
					Head   = { 'Smn. Horn +1', 'Austere Hat', 'Silver Hairpin +1' },
					Body   = { 'Yinyang Robe//MPP.LT.94', 'Vermillion Cloak//MPP.LT.94', 'Summoner\'s Dblt.', 'Austere Robe', 'Seer\'s Tunic', 'Angler\'s Tunica' },
					Hands  = { 'Smn. Bracers +1', 'Errant Cuffs', 'Carbuncle Mitts' },
					Legs   = { 'Evk. Spats +1', 'Summoner\'s Spats', 'Shep. Hose', 'Fisherman\'s Hose' },
					Feet   = { 'Summoner\'s Pgch.', 'Mannequin Pumps', 'Waders' },
				},
			},
		},
	},
	
--[[
	The TP set is used when you or your pet are fighting or if you have your weapons drawn. Accuracy
	and Evasion (ACC and EVA) are applied separately from this set. If you want ACC or EVA gear pieces
	always equipped when fighting, including them here although SMN are weak fighters.

	Stat priority order:
		Pet enhancements (if you have a pet), perpetuation cost (if SMN pet), Haste, accuracy, critical
		hit, etc). Adding stats for the SMN's fighting prowess is kind of pointless since SMNs are lousy
		fighters, but you might consider focusing on defense.

	The kiting set needs fleshing out

--]]

	['TP'] = {
		SUBSET = 'Default',
		GROUP//KITE = {
			SUBSET = 'Evasion',
		},
		GROUP//NOT_KITE = {
			GROUP//SMN_PET = {			-- has a SMN pet
				Head  = { 'Smn. Horn +1//SMN_PETMW', 'Shep. Bonnet//PETF' },
				Ears  = { 'Bat Earring//BLINDED//NOT_PETF', 'Beastly Earring//PETF', 'Loquac. Earring', 'Coral Earring//DT_MAGICAL', 'Bat Earring', 'Energy Earring +1', 'Energy Earring +1' },
				Body  = { 'Summoner\'s Dblt.//SMN_PETMD', 'Yinyang Robe', 'Vermillion Cloak' },
				Hands = { 'Carbuncle Mitts//PETNAME:Carbuncle', 'Nashira Gages' },
				Legs  = { 'Evk. Spats +1', 'Shep. Hose' },
				Feet  = { 'Summoner\'s Pgch.', 'Evk. Pigaches +1', 'Mannequin Pumps', 'Waders' },
			},
			GROUP//NOT_SMN_PET = {		-- has to be a charmed BST pet or PUP pet that can only do maneauvers
				Head  = 'Shep. Bonnet//PETF',
				Ears  = { 'Bat Earring//BLINDED//NOT_PETF', 'Beastly Earring//PETF', 'Loquac. Earring', 'Coral Earring//DT_MAGICAL', 'Bat Earring', 'Energy Earring +1', 'Energy Earring +1' },
				Body  = { 'Yinyang Robe', 'Vermillion Cloak' },
				Rings = { 'Tamas Ring', 'Ether Ring', 'Astral Ring', 'Astral Ring' },
				Legs  = 'Shep. Hose',
				Feet  = { 'Mannequin Pumps', 'Waders' },
			},
			GROUP//NOT_PET = {		-- Emergency, no pet! Equip defense/evasion gear
				Main  = 'Earth Staff',																-- -20% Physical damage
				Ammo  = 'Hedgehog Bomb',															-- -1 Enmity
				Head  = { 'Smn. Horn +1', 'Austere Hat', 'Shep. Bonnet' },							-- Def: 19/13/7..+4 HP
				Neck  = { 'Promise Badge', 'Justice Badge' },										-- Def: 3..+10 HP/1
				Ears  = { 'Bat Earring//BLINDED', 'Coral Earring//DT_MAGICAL', 'Ethereal Earring' },-- +15 Eva,-1% Magical attack..-5 Eva,+5 Eva
				Body  = { 'Yinyang Robe', 'Vermillion Cloak', 'Austere Robe', 'Seer\'s Tunic' },	-- Def: 43/46*/29/18
				Hands = { 'Nashira Gages', 'Smn. Bracers +1', 'Wonder Mitts' },						-- Def: 18..-4 Enmity/16/6..12 HP
				Rings = { 'Flame Ring', 'Bomb Queen Ring', 'Toreador Ring', 'Toreador Ring' },		-- Def: 3, +75 HP, Def: 1..10 HP, Def: 1..10 HP,
				Back  = 'White Cape',																-- Def: 3
				Waist = { 'Hierarch Belt', 'Powerful Rope' },										-- Def: 3, +20 HP
				Legs  = { 'Summoner\'s Spats', 'Wonder Braccae', 'Baron\'s Slops' },				-- Def: 29/12..+21 HP..+2 VIT/11
				Feet  = { 'Dance Shoes', 'Creek F Clomps', 'Mannequin Pumps', 'Waders' }			-- Def: 13..+6 Eva..-1 VIT/9..+35 HP..+4 VIT/6/2
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
--]]
		
	['rAccuracy'] = {
		GROUP//PETF = {			-- Accuracy is for pets
			Head  = 'Shep. Bonnet',					-- Pet: +5 Acc/+3Macc
			Ears  = 'Beastly Earring',				-- Pet: +10 Acc
			Hands = 'SMN. Bracers +1//SMN_PET',		-- Avatar: Enhances acc
			Legs  = 'Evk. Spats +1//SMN_PET',		-- Avatar: Enhances acc
		},
		GROUP//NOT_PETF = {		-- Accuracy is for player
			Ammo  = 'Orphic Egg//PJPBRD',								-- +1 Acc if BRD in party
			Head  = { 'Optical Hat', 'Empress Hairpin' },				-- +10 Acc, +3 DEX
			Neck  = { 'Peacock Amulet',	'Spike Necklace' },				-- +10 Acc, +3 DEX
			Body  = { 'Black Cotehardie', 'Mrc.Cpt. Doublet' },			-- +2/1 DEX
			Hands = 'Battle Gloves',									-- +3 Acc
			Rings = { 'Toreador\'s Ring', 'Toreador\'s Ring', 'Woodsman Ring', 'Woodsman Ring', 'Jaeger Ring', 'Kshama Ring No.2' },	-- +7/+7/+5/+5/4/2 Acc
			Waist = { 'Life Belt', 'Tilt Belt', 'Mrc.Cpt. Belt' },		-- +10/5 Acc, +1 DEX
		},
    },

--[[
	rRanged_Accuracy is similar to the rAccuracy gear set, but for all ranged attacks. It's
	used by the Progressive structure to load slots grouped by stages. Unlike Accuracy, DEX
	does not convert into ranged accuracy. Like rAccuracy, emphasis should be on pet ranged
	accuracy, but that's not a common attribute. Make sure you at least have ranged accuracy
	gear for the player even if all you're doing is throwing a pebble or dart.
--]]

	['rRanged_Accuracy'] = {
		Head  = 'Optical Hat',		-- +10 RAcc
		Neck  = 'Peacock Amulet',	-- +10 RAcc
		Rings = { 'Woodsman Ring', 'Woodsman Ring', 'Jaeger Ring' },	-- +5/5/4 RAcc
	},

--[[
	The Progressive structure is gear set-like, offering a way to group slot definitions
	into stages that can be applied in a progressive manner. There are four valid types
	that can be defined in the structure: Accuracy, Tank_Accuracy, Ranged_Accuracy, and
	Tank_Ranged_Accuracy. SMN does not support the TANK option, so we'll only focus on
	the two non-tank options.

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
	display	bar.
--]]

  ['Progressive'] = { 
		['Accuracy'] = { 
			[1] = { 
				['Neck']  = 'rAccuracy::Neck',
				['Hands'] = 'rAccuracy::Hands',
				['Legs']  = 'rAccuracy::Legs'
			},
			[2] = {
				['Head']  = 'rAccuracy::Head',
				['Rings'] = 'rAccuracy::Rings',
				['Ears']  = 'rAccuracy::Ears',
			},
			[3] = {	
				['Subset'] = 'rAccuracy',
			}
		},
		['Ranged_Accuracy'] = {
			[1] = {
				['Head']   = 'rRanged_Accuracy::Head',
				['Neck']   = 'rRanged_Accuracy::Neck',
			},
			[2] = { 
				['Subset'] = 'rRanged_Accuracy',
			},
		},
  },
  
--[[
	The Evasion set will be equipped if EVA is enabled. Remember that AGI converts to evasion: for every
	2 points of AGI you get 1 point of evasion. While there is some gear that will grant evasion to a pet,
	the intention of this set is for the player. I'd be more inclined to include pet evasion gear in the
	['TP'] set.
--]]
	
	['Evasion'] = {
		Main  = 'Auster\'s Staff',						-- +10 Eva
		Ammo  = 'Orphic Egg//PJPBRD',					-- +1 Eva if BRD in party
		Head  = { 'Optical Hat', 'Empress Hairpin' },	-- +10/10 Eva
		Neck  = 'Spirit Torque',						-- +5 Eva
		Ears  = { 'Bat Earring//BLINDED', 'Ethereal Earring', 'Genin Earring//SJNIN', 'Drone Earring' },	-- +15 Eva while blinded, +5 Eva, +4/3 AGI
		Body  = 'Yinyang Robe//IF:Vermillion Cloak',	-- Filler, V.Cloak has -10 eva
		Hands = 'Battle Gloves',						-- +3 Eva
		Rings = { 'Ether Ring//IF:Woodsman Ring', 'Astral Ring//IF:Woodsman Ring', 'Astral Ring//IF:Woodsman Ring' },	-- Filler, Woodsman Ring has -5 eva
		Waist = 'Swift Belt//IF:Tilt Belt', 			-- Filler, Tilt belt has -5 eva
    },

--[[
	rDamageTaken set is not equipped directly but rather from subsets since it's a reference set. It's a
	way to reduce a specific type of damage. As such it's optional and up to the player to decide where
	it should be included via a Subset. (Prior versions had three separate sets.)
--]]

	['rDamage_Taken'] = {
		GROUP//DT_PHYSICAL = {
			Main = 'Earth Staff',					-- -20% damage reduction from physical
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
	(When defining a threshhold you don't want to go with 100% because gear	change can make
	that difficult to hit.)

	The rDamage_Taken set is added as a subset to reduce damage accordingly because you're in
	a vulnerable position.
--]]
	
	['Resting_Regen'] = {
		SUBSET = 'rDamage_Taken',
		Hands  = { 'Carbuncle Cuffs//SHINING_RUBY','Shep. Bracers' },		-- +5/1 HP/tick while resting
        Waist  = 'Hierarch Belt',		-- +2 HP/tick while resting
    },
	
	['Resting_Refresh'] = {
		SUBSET = 'rDamage_Taken',
		Main   = { 'Pluto\'s Staff', 'Kukulcan\'s Staff', 'Pilgrim\'s Wand' },			-- +10/3/2 MP/tick while resting
        Body   = { 'Errant Hpl.', 'Yinyang Robe', 'Vermillion Cloak', 'Black Cotehardie//HP.LT.25', 'Seer\'s Tunic' },-- +5 MP/tick while healing, adds "refresh", adds "refresh", Adds "refresh" up to 25 HP, +1/tick while healing
		Waist  = 'Hierarch Belt',														-- +2 MP/tick while resting
		Legs   = 'Baron\'s Slops',														-- +1 MP/tick while resting
	},

	
--[[
	Start weapons are where you define what you want the first row of equipment to look 
	like when you either log in as a SMN or switch your main job to SMN. Any other gear 
	you mention will be overridden by the Default set, so no need to include here.
--]]

	['Start_Weapons'] = {
	    Main = { 'Earth Staff', 'Kukulcan\'s Staff', 'Pilgrim\'s Wand' },
		Ammo = { 'Hedgehog Bomb', 'Fortune Egg' },
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
		Main  = 'Vulcan\'s Staff',		-- +10 RAtt
		Ears  = 'Brutal Earring',		-- Store TP +1
    },

--[[
	***************
	* Blood Pacts *
	***************

	Blood pacts go through a simulated process that mimics spell casting. The precast
	happens when the blood pact is invoked (either rage or ward), loading the 'PreBP'
	gear set. You want gear that has Blood Pact Ability Delay, Blood Pact Recast
	abilities, or Summoning Skill defined here.

	Note: Blood Pact Delay has a cap of -15
--]]

	['PreBP'] = {
		SUBSET = 'Summoning_Skill',
		Head   = { 'Smn. Horn +1', 'Austere Hat' },								-- BP ability delay -3
		Body   = { 'Yinyang Robe', 'Summoner\'s Dblt.', 'Austere Robe' },		-- BP ability delay: -5/-3/-3
		Hands  = 'Smn. Bracers +1',												-- BP ability delay -2
		Legs   = 'Summoner\'s Spats',											-- BP ability delay -2
		Feet   = 'Summoner\'s Pgch.',											-- BP ability delay -2
	},

--[[
	The midcast for Blood pacts are divided by type: physical, magical, summoning
	skill, accuracy, and hybrid. The 'MidBP' gear set encapsulates all those types
	through the	use of groups.

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
		SUBSET = 'Summoning_Skill',
		GROUP//SMN_BP_PHYS = {
			Head   = 'Shep. Bonnet',			-- Pet: +5 Acc
			Ears   = 'Beastly Earring',			-- Pet: +10 Acc
			Body   = 'Summoner\'s Dblt.',		-- Avatar: +3% Crit Rate
			Hands  = 'Smn. Bracers +1',			-- Avatar: Enhances Acc
			Legs   = 'Evk. Spats +1',			-- Avatar: Enhances Acc
			Feet   = 'Summoner\'s Pgch.',		-- Avatar: Enhances Att
		},
		GROUP//SMN_BP_MAG = {
			Head   = 'Shep. Bonnet',			-- Pet: +3 Macc
		},
		GROUP//SMN_BP_SKILL = {
		},
		GROUP//SMN_BP_ACC = {
			Head  = 'Shep. Bonnet',				-- Pet: +5 Acc/+3 Macc
			Ears  = 'Beastly Earring',			-- Pet: +10 Acc
			Hands = 'Smn. Bracers +1',			-- Avatar: Enhances Acc
			Legs  = 'Evk. Spats +1',			-- Avatar: Enhances Acc
		},
		GROUP//SMN_BP_HYBRID = {
			Head  = 'Shep. Bonnet',				-- Pet: +5 Acc/+3 Macc
			Ears  = 'Beastly Earring',			-- Pet: +10 Acc
			Body  = 'Summoner\'s Dblt.',		-- Avatar: 3% Crit Rating
			Legs  = 'Evk. Spats +1',			-- Avatar: Enhances Acc
			Feet  = 'Summoner\'s Pgch.',		-- Avatar: Enhances Att
		},
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
		Main  = 'Aquilo\'s Staff//WSWAP',				-- +5 INT
		Head  = { 'Smn. Horn +1', 'Evoker\'s Horn' },	-- +4/3 INT
		Body  = { 'Errant Hpl.', 'Black Cotehardie', 'Baron\'s Saio' },	-- +10/2/1 INT
		Hands = 'Errant Cuffs',							-- +7 INT
		Rings = { 'Tamas Ring', 'Flame Ring' },			-- +5/2 INT
		Waist = { 'Penitent\'s Rope', 'Mrc.Cpt. Belt' },	-- +5/1 INT
		Legs  = 'Errant Slops',							-- +7 INT
		Feet  = { 'Rostrum Pumps', 'Mannequin Pumps' },	-- +3/1 INT
	},

	-- Mind Reference gear set
	['rMND'] = {
		Main  = { 'Water Staff//WSWAP', 'Pluto\'s Staff//WSWAP', 'Light Staff//WSWAP' },	-- +4/2/1 MND
		Neck  = { 'Promise Badge', 'Justice Badge' },	-- +5/3 MND
		Ears  = 'Geist Earring',						-- +1 MND
		Body  = { 'Errant Hpl.', 'Evoker\'s Doublet', 'Wonder Kaftan', 'Baron\'s Saio' },	-- +10/3/1/1 MND
		Hands = 'Baron\'s Cuffs',						-- +1 MND
		Rings = { 'Tamas Ring', 'Kshama Ring No.9', 'Tranquility Ring' },	-- +5/3/2 MND
		Back  = 'White Cape',							-- +2 MND
		Waist = { 'Penitent\'s Rope', 'Mrc.Cpt. Belt', 'Friar\'s Rope' },	-- +5/1/1 MND
		Legs  = { 'Errant Slops', 'Summoner\'s Spats', 'Wonder Braccae' },	-- +7/3/2 MND
		Feet  = { 'Rostrum Pumps', 'Mannequin Pumps', 'Seer\'s Pumps' }, 	-- +3/2/1 MND		
	},

	-- Charisma Reference gear set. Provides accuracy with singing
	['rCHR'] = {		-- Charisma provides accuracy w/singing
		Main  = 'Pluto\'s Staff',			-- +2 CHR
		Head  = 'Entrancing Ribbon',		-- +2 CHR
		Ears  = 'Beastly Earring',			-- +2 CHR
		Body  = { 'Errant Hpl.', 'Wonder Kaftan//IF:Black Cotehardie' },	-- +10 CHR, filler to avoid -3 CHR
		Neck  = { 'Star Necklace', 'Flower Necklace' },	-- +3/3 CHR
		Rings = 'Kshama Ring No.6',			-- +3 CHR
		Waist = { 'Corsette', 'Mrc.Cpt. Belt' },	-- +5/1 CHR
		Legs  = 'Errant Slops',				-- +7 CHR
	},

	-- Enmity+ Reference gear set, for player
	['rEnmity_Plus'] = {
	},

	-- Enmity- Reference gear set
	['rEnmity_Minus'] = {
		Ammo  = 'Hedgehog Bomb',						-- -1 Enmity
		Neck  = 'Fenrir\'s Torque//NIGHTTIME',			-- -3 Emnity at night
		Hands = { 'Nashira Gages', 'Errant Cuffs' },	-- -4/-2 Enmity
		Rings = 'Tamas Ring',							-- -5 Enmity
		Waist = 'Penitent\'s Rope',						-- -3 Enmity
		Legs  = { 'Evk. Spats +1', 'Errant Slops' },	-- -3/-3 Enmity
		Feet  = 'Evoker\'s Boots',						-- -2 Enmity
	},
	
	-- Magic Attack Bonus Reference set
	['rMAB'] = {
		Neck   = 'Uggalepih Pendant//SPECIAL',		-- +8 MAB if MP < 51%
	},

	-- Attack Power Reference set
	['rAttackPower'] = {
		Ears  = { 'Ethereal Earring', 'Coral Earring', 'Fang Earring', 'Brutal Earring' },	-- +5/5/4 Att, Store TP +1
		Rings = 'Kshama Ring No.8',					-- +3 Att
		Waist = 'Hierarch Belt//IF:SWIFT BELT',		-- Filler, Swift belt has -5 attack
	},

--[[
	Each type of spell can have it's own gear as well as stat based gear. In some
	cases individual spells have special entries.

	The first stage is Precast. This is where you place any Fast Cast, cast time
	reduction, quick cast gear, and spell interruption rate down gear
--]]

	['Precast'] = {	
		Ears = 'Loquac. Earring',	-- Enhances Fastcast
		Feet = 'Rostrum Pumps',		-- Enhances Fastcast
	},

--[[
	A lot of spells have a better chance of landing if you increase your
	magic accuracy. The Macc gear set will be equipped if MACC is toggled
	on.

	Note: Macc is only used on offensive magic casting. It is not equipped
	when providing buffs.
--]]
	
	['Macc'] = {
		Subset = { 'rDark_Magic_Skill//MT:DARK',
				   'rElemental_Magic_Skill//MT:ELEMENTAL',
				   'rEnfeebling_Magic_Skill//MT:ENFEEBLING',
				   'rHealing_Magic_Skill//MT:OFFENSIVE_HEALING',
				   'rDivine_Magic_Skill//MT:DIVINE',
				   'rNinjutsu_Skill//MT:NINJUTSU',
				   'rSinging_Skill//MT:SINGING',
				},
		Hands  = 'Nashira Gages',		-- +3 MAcc
		Rings  = 'Tamas Ring',			-- +5 MAcc
		Feet   = 'Nashira Crackows',	-- +2 MAcc	
	},
	Neck   = 'Uggalepih Pendant//SPECIAL',	-- +8 MAB if MP% < 51%
--[[
	The second stage is Midcast. This is where you equip gear that gives
	magic attack, enhancing bonuses, potency improvements, duration
	enhancements, recast reduction gear, etc. This implementation breaks
	out the midcast into separate routines for each magic type: healing,
	divine, elemental, enhancing, enfeebling, summoning, ninjutsu, and
	song. Each type is listed below in their own section. Within each
	section there's multiple gear sets providing specific functionality.

	Every gear set includes details on what it is suppose to feature and
	what stats you should be emphasizing. Further, any formulas/charts that
	will help you to decide what gear should be included.
--]]

--[[
	**************************
	* Midcast: Healing Magic *
	**************************
--]]
	
--[[
	Healing Magic: consisting of all light-based spells, can remove
	some debuffs on players, buffs the caster, cures the health of 
	players or npcs, or cause damage to undead monsters. Healing 
	magic skill affects the	potency of cures while decreasing the 
	likelihood of the caster being interrupted.
	
	Healing spells: cures, curagas, raises, reraises, blindna, cursna,
	paralyna, poisona, silena, stona, and viruna.
--]]

	-- rHealing_Magic_Skill specifies gear that boosts Healing Magic Skill
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
		SUBSET  = {
			[1] = 'rHealing_Magic_Skill',
			[2] = 'rMND'
			},
		Ammo   = 'Enmity_Minus::Ammo',
		Hands  = 'Evoker\'s Bracers',	-- +4 VIT
		Legs   = 'Shep. Hose//EMPTY',	-- +2 VIT
		Feet   = 'Creek F Clomps//IF:Seer\'s Pumps',	-- +4 VIT
	},	
	
--[[
	As for the offensive use of cure spells against undead monsters,
	most of	what was said about CuringMagic is true except cure potency.
	This has no effect on undead monsters.

	After the OffensiveCuring set is equipped, the midcast routine will
	see if a elemental Obi can be equipped to take advantage of the
	proc rate of the day's element/weather matching. Also, like normal
	curing 	magic, an Apollo/Light staff will be check for,	but not for
	the cure potency. Rather, for magic affinity.
--]]

	['OffensiveCuring'] = {
		SUBSET = {
			[1] = 'rHealing_Magic_Skill',
			[2] = 'rMND',
			[3] = 'rMAB',
		},
	},

--[[
	This last set is used for all non-cure Healing Magic spells. Only
	healing magic skill is of any importance here. You might want
	to use this set as a subset for the other cure-based sets.
--]]

	['HealingMagic'] = {
		SUBSET = 'Healing_Magic_Skill',
		},
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
	},
	
--[[
	This gear set is for all barspells: elemental and status, both of which
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
		SUBSET  = {
			[1] = 'rINT',
			[2] = 'rMAB',
	},
	
--[[
	The rest of the gear sets for Enhancing Magic are for specific spells: 
	stoneskin, sneak, invisible, and phalanx. Include gear in the 
	appropriate set that enhances the named spell accordingly.

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
	},	

--[[
	Sneak's duration is variable, but the duration maxes at about 5 
	minutes. Include any gear that enhances this buff. Note: this set
	is also equipped when you use sneak oil.
--]]

	['Sneak']  = {
		SUBSET = 'rEnhancing_Magic_Skill',
		Feet   = 'Dream Boots +1',
	},

--[[
	Invisible's duration is variable, but the duration maxes at 
	about 5 minutes. Include any gear that enhances this buff.
	Note: this set is also equipped when you use prism powder.
--]]	

	['Invisible'] = {
		SUBSET = 'rEnhancing_Magic_Skill',
		Hands  = 'Dream Mittens +1',	-- Extends duration of Invisible
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
		SUBSET  = 'rEnhancing_Magic_Skill',
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

	-- rElemental_Magic_Skill specifies gear that boosts Elemental Magic Skill
	['rElemental_Magic_Skill'] = {
		Feet = 'Nashira Crackows',		-- +5 Elemental Magic Skill
	},
	
--[[
	Elemental Magic: This type of magic consists of nukes, ancient magic (a type
	of nuke), and elemental debuffs. Elemental Magic Skill determines the accuracy
	and help resist spell interuptions. All elemental spells are consider to be
	either a nuke or debuff.
	
	Elemental spells: aeros, aerogas, blizzards, blizzagas, burn, burst, drown
	fires, firagas, flare, flood, quake, rasp, sjhock, stones, stonegas, thunders,
	thundagas, tornado, waters, and watergas.
	
	Elemental magic and ancient magic are grouped together. CaLculating magic
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
			[2] = 'rINT'
		},
	},

--[[
	**********************
	* Midcast: Summoning *
	**********************
--]]

	-- rSummoning_Magic_Skill specifies gear that boosts Summoning Magic Skill
	['rSummoning_Skill'] = {
		Head  = { 'Evoker\'s Horn',	'Austere Hat' },	-- +5/2 Summoning Skill
		Neck  = 'Smn. Torque',							--  +7 Summoning Magic Skill
		Body  = { 'Summoner\'s Dblt.', 'Austere Robe'}, -- Ensures that a V.Cloak not equipped since we want the head gear, Aus Robe: BP delay -3
		Hands = 'Smn. Bracers +1',						-- +12 Summoning Skill
		Rings = 'Evoker\'s Ring',						-- +10 Summoning Skill
		Feet  = 'Nashira Crackows',						--  +5 Summoning Magic Skill
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
		Hands  = 'Carbuncle\'s Cuffs',				-- Summoning magic casting time -1
		Feet   = 'Evoker\'s Boots'					-- Summoning magic casting time -1
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

	-- rDark_Magic_Skill specifies gear that boosts Dark Magic Skill
	['rDark_Magic_Skill'] = {
	},	
	
--[[
	There's 9 absorb spells (although some are currently out of era). If not
	resisted, they drain a specific stat from the target based on the caster's
	level:
	
		base absorbed = floor (3 + (job level) / 5)
	
	Dark magic skill has no effect on absorb spells, but do affect accuracy.
	Absorb spells resisted will have their duration cut in half or be completely
	resisted. Equipment that "Enhances" absorb spells will increase the spells
	duration. Equipment that "Augments" absorb spells will increase the spells
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
--		SUBSET = 'rDark_Magic_Skill',
--	},

--[[
	*************************
	* Midcast: Divine Magic *
	*************************
--]]

	-- rDivine_Magic_Skill specifies gear that boosts Divine Magic Skill
	['rDivine_Magic_Skill'] = {
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
	affected by magic accuracy from equipment. Damage can be enhanced
	through MAB. Damage resist rates depend	on the difference in MND 
	between caster and target. Banish does 50% more damage to undead.
	
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

	-- rEnfeebling_Magic_Skill specifies gear that boosts Enfeebling Magic Skill
	['rEnfeebling_Magic_Skill'] = {
		Neck = 'Enfeebling Torque',		--  +7 Enfeebling Magic Skill
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
		SUBSET = 'Enfeebling_Magic_Skill',
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

	-- rNinjutsu_Skill specifies gear that boosts Ninjutsu Skill
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
	****************
	* Weaponskills *
	****************

	The following weapon skill gearsets are defined by the stat(s) they emphasize.
	Listed are all of the sets that	you will need to use every weapon skill 
	that your job can do. The leading comment defines what weapon/weapon skill
	combination the set applies to.
	
	SMN can use the following weapons: staff (B), club (C+), dagger (E).
	
	Please note that on HorizonXI you may have access to some weapon skills
	through your subjob. While not explicitly supported here, the appropriate
	weapon skill set will be loaded. If not listed below, you might have to
	create a custom weapon skill set to do this. Remember, weapon skill sets
	are named WS:attr. If you name the set appropriately, that set will auto-
	matically be called when you use the weapon skill.
--]]

--[[
		* Strength based *
		
		Staff: Heavy Swing,Shell Crusher,Full Swing
		Club: Brainshaker,Skullbreaker,True Strike
-]]
	
	['WS:STR'] = {
		SUBSET = 'rAttackPower',
        Neck   = { 'Justice Torque','Spike Necklace' },		-- +5/3 STR
        Body   = { 'Black Cotehardie', 'Wonder Kaftan' },	-- 3/1 STR
        Hands  = 'Wonder Mitts',			-- +3 STR
		Rings  = { 'Flame Ring', 'Kshama Ring No.8' },	-- +5/3 STR
        Waist  = 'Mrc.Cpt. Belt',		-- +1 STR
        Legs   = 'Wonder Braccae',		-- +1 STR
        Feet   = { 'Creek F Clomps', 'Wonder Clomps' },		-- +4/2 STR
    },
	
--[[
		* Strength and Intelligence based, even weighting *
		
		Staff: Rock Crusher,Earth Crusher,Cataclysm
--]]
	
	['WS:STRINT'] = {
		SUBSET = 'rAttackPower',
        Head   = { 'Smn. Horn +1', 'Evoker\'s Horn' },		-- +4/3 INT
        Neck   = { 'Justice Torque','Spike Necklace' },		-- +5/3 STR
        Body   = { 'Black Cotehardie', 'Wonder Kaftan', 'Baron\'s Saio' },	-- +3 STR/+2 INT, +1 STR, +1 INT
        Hands  = { 'Errant Cuffs', 'Wonder Mitts' },		-- +5 INT, +3 STR
		Rings  = { 'Tamas Ring', 'Flame Ring', 'Kshama Ring No.8', 'Kshama Ring No.5' },	-- +5 STR, +5 STR/+2 INT, +3 STR, +3 INT
        Waist  = { 'Penitent\'s Rope', 'Mrc.Cpt. Belt' },	-- +5 INT, +1 INT/+1 STR
        Legs   = { 'Errant Slops', 'Wonder Braccae' },		-- +7 INT/-5 STR, +1 STR
        Feet   = { 'Rostrum Pumps', 'Wonder Clomps', 'Mannequin Pumps' },	-- +3 INT, +2 STR, +1 INT
    },

--[[
		* Strength and Mind based, even weighting *
		
		Club: Shining Strike,Seraph Strike,Judgement
		Staff: Retribution
--]]
	
	['WS:STRMND'] = {
		SUBSET = 'rAttackPower',
        Neck   = { 'Promise Badge', 'Justice Torque', 'Justice Badge' },	-- +5 MND, +5 STR, +3 MND
		Ears   = 'Geist Earring',		-- +1 MND
        Body   = { 'Black Cotehardie', 'Wonder Kaftan', 'Baron\'s Saio' },		-- +3 STR, +1/1 MND
        Hands  = { 'Wonder Mitts', 'Baron\'s Cuffs' },	-- +3 STR, +1 MND
		Rings  = { 'Tamas Ring', 'Flame Ring', 'Kshama Ring No.9', 'Kshama Ring No.8' },		-- +5 MND, +5 STR/-2 MND, +3 MND, +3 STR
        Back   = 'White Cape',			-- +2 MND
        Waist  = { 'Penitent\'s Rope', 'Mrc.Cpt. Belt', 'Friar\'s Rope' },	-- +5 MND, +1 STR/+1 MND, +1 MND
        Legs   = { 'Summoner\'s Spats', 'Errant Slops', 'Wonder Braccae' },	-- 3 MND, +7 MND/-5 STR, +2 MND
        Feet   = { 'Creek F Clomps', 'Rostrum Pumps', 'Mannequin Pumps', 'Wonder Clomps' },	-- +4 STR, +3/2 MND, +2 STR
	},

--[[
		* Strength and Mind based, 30% to 50% weighting *
		
		Club: Black Halo
--]]

	['WS:STRMND_30_50'] = {
		SUBSET = 'rAttackPower',
        Neck   = { 'Promise Badge', 'Justice Torque', 'Justice Badge' },	-- +5 MND, +5 STR, +3 MND
		Ears   = 'Geist Earring',		-- +1 MND
        Body   = { 'Black Cotehardie', 'Wonder Kaftan' },		-- +3 STR, +1 MND
        Hands  = { 'Wonder Mitts', 'Baron\'s Cuffs' },	-- +3 STR, +1 MND
		Rings  = { 'Tamas Ring', 'Flame Ring', 'Kshama Ring No.9', 'Kshama Ring No.8' },		-- +5 MND, +5 STR/-2 MND, +3 MND, +3 MND
        Back   = 'White Cape',			-- +2 MND
        Waist  = { 'Penitent\'s Rope', 'Mrc.Cpt. Belt', 'Friar\'s Rope' },	-- +5 MND, +1 STR/+1 MND, +1 MND
        Legs   = { 'Summoner\'s Spats', 'Errant Slops', 'Wonder Braccae' },	-- +3 MND, +7 MND/-5 STR, +2 MND
        Feet   = { 'Creek F Clomps', 'Rostrum Pumps', 'Mannequin Pumps', 'Wonder Clomps' },		-- +4 STR, +3/2 MND, +2 STR
    },
	
--[[
		* Dexterity based *
		
		Dagger: Wasp Sting,Viper Bite^

		^ Subjob must be one of: RDM,THF,BRD,RNG,NIN
--]]
	
	['WS:DEX'] = {
		SUBSET = 'rAttackPower',
        Head   = 'Empress Hairpin',		-- +3 DEX
        Neck   = 'Spike Necklace',		-- +3 DEX
        Body   = 'Black Cotehardie',	-- +2 DEX
        Rings  = 'Kshama Ring No.2',	-- +3 DEX
        Waist  = 'Mrc.Cpt. Belt',		-- +1 DEX
    },

--[[
		* Dexterity and Intelligence based *

		Dagger: Gust Slash,Cyclone^

		^ Subjob must be one of: RDM,THF,BRD,RNG,NIN
--]]
	
	['WS:DEXINT'] = {
		SUBSET = 'rAttackPower',
        Head   = { 'Smn. Horn +1', 'Evoker\'s Horn', 'Empress Hairpin' },	-- +4/3 INT, +3 DEX
        Neck   = { 'Spike Necklace', 'Opo-opo Necklace' },		-- +3/1 DEX
        Body   = { 'Black Cotehardie', 'Baron\'s Saio' },	-- +2 INT/+2 DEX, +1 INT
        Hands  = 'Errant Cuffs',	-- +5 INT
		Rings  = { 'Tamas Ring', 'Kshama Ring No.2', 'Kshama Ring No.5', 'Flame Ring' },	-- +5 INT, +3 DEX, +3 INT, +2 INT
        Waist  = { 'Penitent\'s Rope', 'Mrc.Cpt. Belt' },	-- +5 INT, +1 INT/+1 DEX
        Legs   = 'Errant Slops',						-- +7 INT/-5 DEX
        Feet   = { 'Rostrum Pumps', 'Mannequin Pumps' },	-- +3/1 INT
    },

--[[
		* Intellegence *

		Staff: Gate of Tartarus
--]]
	
	['WS:INT'] = {
		SUBSET = 'rAttackPower',
		Head   = { 'Smn. Horn +1', 'Evoker\'s Horn' },	-- +4/3 INT
		Body   = { 'Errant Hpl.', 'Black Cotehardie', 'Baron\'s Saio' },	-- +10/2/1 INT
		Hands  = 'Errant Cuffs',							-- +5 INT
		Rings  = { 'Tamas Ring', 'Kshama Ring No.5', 'Flame Ring' },		-- +5/3/2 INT
		Waist  = { 'Penitent\'s Rope', 'Mrc.Cpt. Belt' },	-- +5/1 INT
		Legs   = 'Errant Slops',							-- +7 INT
		Feet   = { 'Rostrum Pumps', 'Mannequin Pumps' },	-- +3/1 INT
    },
	
--[[
		* Intellegence and Mind based, even weighting *

		Staff: Spirit Taker
--]]
	
	['WS:INTMND'] = {
		SUBSET = 'rAttackPower',
		Head   = { 'Smn. Horn +1', 'Evoker\'s Horn' },		-- +4/3 INT
		Neck   = { 'Promise Badge', 'Justice Badge' },		-- +5/3 MND
		Ears   = 'Geist Earring',							-- +1 MND
		Body   = { 'Errant Hpl.', 'Evoker\'s Doublet', 'Wonder Kaftan', 'Baron\'s Saio' },	-- +10 INT/+10 MND, +3 MND, +1 INT, +1 MND
		Hands  = { 'Errant Cuffs', 'Baron\'s Cuffs' },	-- +7 INT, +3/1 MND
		Rings  = { 'Tamas Ring', 'Kshama Ring No.9', 'Kshama Ring No.5', 'Flame Ring' },		-- +5 INT/+5 MND, +3 MND, +3 INT, +2 INT
		Waist  = { 'Penitent\'s Rope', 'Mrc.Cpt. Belt', 'Friar\'s Rope' },	-- +5 INT/+5 MND, +1 INT/+1 MND, +1 MND
		Legs   = { 'Errant Slops', 'Summoner\'s Spats', 'Wonder Braccae' },	-- +7 MND/+7 INT, +3/2 MND
		Feet   = { 'Rostrum Pumps', 'Mannequin Pumps', 'Seer\'s Pumps' },	-- +3 MND/+3 INT, +2 MND/1 INT, +1 MND
    },
	
--[[
		* Charisma based *

		Dagger: Shadowstitch
--]]
	
	['WS:CHR'] = {
		SUBSET  = {
			[1] = 'rAttackPower',
			[2] = 'rCHR',
		},
    },

--[[
		* Mind based *

		Dagger: Energy Steal, Energy Drain^
		
		^ Subjob must be RDM,THF,BRD,RNG, or NIN
--]]

	['WS:MND'] = {
		SUBSET  = {
			[1] = 'rAttackPower',
			[2] = 'rMND',
		},
    },
	
--[[
		* Skill based *
		
		Club: Starlight,Moonlight
--]]
	
	['WS:Skill'] = {
		SUBSET = 'rAttackPower',
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

	['A:AstralFlow'] = {
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

	--* /BST *--
	['A:Charm'] = {				-- charm skill, CHR gear
		SUBSET = 'rCHR',
    },
	
	['A:Pet_Macc'] = {			-- Pet's Magical Accuracy
		Head = 'Shep. Bonnet',
	},

	--* /THF *--
	-- if only Sneak Attack is enabled, the following will be equipped
	['A:Sneak_Attack'] = {
		Head  = 'Empress Hairpin',							-- +3 DEX
		Neck  = { 'Spike Necklace', 'Opo-opo Necklace' },	-- +3/3 DEX
		Rings = 'Kshama Ring No.2',		-- +3 DEX
		Waist = 'Mrc.Cpt. Belt',							-- +1 DEX
	},

	-- If only Trick Attack is enabled, the following will be equipped
	['A:Trick_Attack'] = {
		Head  = 'Empress Hairpin',							-- +3 AGI
		Ears  = { 'Genin Earring//SJNIN', 'Drone Earring' },-- +4 AGI if sj NIN, +3 AGI
		Rings = 'Kshama Ring No.3',							-- +3 AGI
		Waist = 'Mrc.Cpt. Belt',							-- +1 AGI
	},

	-- When both Sneak Attack and Trick Attack are enabled, the following will be equipped
	['A:SATA'] = {
		Head  = 'Empress Hairpin',							-- +3 DEX/+3 AGI
		Neck  = 'Spike Necklace',							-- +3 DEX
		Ears  = { 'Genin Earring//SJNIN', 'Drone Earring' },	-- +4 AGI if sj NIN, +3 AGI
		Rings = { 'Kshama Ring No.2', 'Kshama Ring No.3' },	-- +3 DEX, +3 AGI
		Waist = 'Mrc.Cpt. Belt',							-- +1 DEX/+1 AGI
	},

--[[
	Pet commands can also be made into a gear set. Unlike abilities with an 'A-' prefix,
	pet commands use the 'PC:' prefix. By default the pet commands  most likely to
	have gear associated with then ()(except for blood pacts) are predefined for: BST,
	DRG and SMN.

	Note: commands like BST's SIC and READY and SMN's Blood Pact actually are identified
	by the skill they invoke. This means that the type of skill is what is processed
	and not the actual command. Defined in utilities.lua are the BSTs skill according to
	type. Blood pacts are handled separately by PreBP and MidBP, so not included here.
--]]

	--* /BST *--
	['PC:Reward'] = {
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
	['STAVE'] =  { 'Fire Staff', 'Vulcan\'s Staff', 'Ice Staff', 'Aquilo\'s Staff',
				  'Wind Staff', 'Auster\'s Staff', 'Earth Staff', 'Terra\'s Staff',
				  'Thunder Staff', 'Jupiter\'s Staff', 'Water Staff', 'Neptune\'s Staff',
				  'Light Staff', 'Apollo\'s Staff', 'Dark Staff', 'Pluto\'s Staff',
				  'Kukulcan\'s Staff' },
	['CLUB']  =  { 'Warp Cudgel', 'Solid Wand', 'Yew Wand', 'Pilgrim\'s Wand' },
	['DAGGER'] = { 'Garuda\'s Dagger' },
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
	MacroBook = 13;						-- Which macro book should be equipped for SMN
};

-- Table of gear to put a delay on
profile.TrackedGear = {
	[1] = { ['item'] = 'Vermillion Cloak', ['slot'] = 'Body', ['delay'] = 10 },
};

-- Tracked pet action
profile.sPetAction = nil;

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
	-- "chkSJ" is the key for what toolbar is shown. All jobs are defined in the tSubs table.
	-- A value of 0 means that job is not configured. All values > 0 indicate which toolbar
	-- is to be displayed. The player must change the entries in this table to match their
	-- needs.
	local tSubs = {
				['WAR'] = 0, ['MNK'] = 0, ['WHM'] = 1, ['BLM'] = 3, ['RDM'] = 2, ['THF'] = 0,
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
	if profile.settings.sjb == nil or chkSJ ~= profile.settings.sjb then
		if tSubs[chkSJ] > 0 then
			sj = tSubs[chkSJ];
		end
	end

	-- Set the macro set
	AshitaCore:GetChatManager():QueueCommand(1, '/macro set '..tostring(sj));
	profile.settings.sjb = chkSJ;
end		-- SetSubjobSet

--[[
	OnLoad is run whenever you log into your SMN or change your job to SMN
--]]

function profile.OnLoad()
	local player = gData.GetPlayer();

	-- Initialize settings
	gSettings.AllowAddSet = true;
	utilities.Initialize();
	crossjobs.settings.bWSOverride = true;
	
	-- Set which macro book should be displayed. Which macro set within the macro book to
	-- display depends on what your subjob is.
	if profile.settings.bAutoMacrobook_page == true then
		AshitaCore:GetChatManager():QueueCommand(1, '/macro book ' .. tostring(profile.settings.MacroBook));		-- SMN macro book
		if profile.settings.bJustMacroBook == false then
			SetSubjobSet(player.SubJob);
		end
	end

	-- Load up the weapons bar.
	gear.MoveToDynamicGS(profile.Sets.Start_Weapons,crossjobs.Sets.CurrentGear,true,'Start_Weapons');
	gear.EquipTheGear(crossjobs.Sets.CurrentGear,false);
	
	-- Make sure the saved weapons are the starting weapons
	gear.weapon = crossjobs.Sets.CurrentGear['Main'];
	gear.offhand = crossjobs.sets.CurrentGear['Sub';
end		-- OnLoad

--[[
	OnUnload is run when you change to another job
--]]

function profile.OnUnload()
	utilities.Unload();
end		-- OnUnload

--[[
	HandleCommand is run when you type in a command defined in LUASHITACAST. The commands handled here instead
	of in gcinclude.HandleCommands are specific to SMN or the help system.
--]]

function profile.HandleCommand(args)
	if args[1] == 'help' then
		help.ShowHelp(args);
	elseif args[1] == 'petfood' then
		pets.fPetReward(args[2],true);
	else
		crossjobs.HandleCommands(args);profile.sPetAction
	end
end		-- HandleCommand

--[[
	HandlePetAction prints the blood pact being invoked (if a SMN pet and
	indicated) and then invoked the general HandkePetAction routine found
	in gear.

	Parameter
		PetAction	What action has your pet done
--]]

function HandlePetAction(PetAction)

	if PetAction == nil or PetAction.Name == nil then
		return;
	end

	if pets.fSummonerPet() == true then
		-- Since the pet is a smn avatar, give feedback on the blood pact.
		-- If the action is a BP: rage, print out what happened in party chat
		if (profile.sPetAction == nil or profile.sPetAction ~= PetAction.Name) and
			utilities.fGetToggle('sBP') == true then
			local sMsg;
			if string.find(pets.SmnBPRageList,PetAction.Name) ~= nil then
				sMsg = '/p  [<pet>] [Blood Pact: ' .. PetAction.Name .. '] >> <t>.';
			else
				sMsg = '/echo [<pet>] [Blood Pact: ' .. PetAction.Name .. ']';
			end
			AshitaCore:GetChatManager():QueueCommand(-1, sMsg);
			profile.sPetAction = PetAction.Name;
		end
	end

	-- Only gear swap if this flag is true
	if utilities.fGetToggle('GSwap') == false then
		return;
	end

	-- Finish with the generalized version of the function
	pets.HandlePetAction(PetAction);
end		-- HandlePetAction

--[[
	HandleDefault is run when some action happens. This emphasizes pet actions
--]]
	
function profile.HandleDefault()
	local pet = gData.GetPet();
	local petAction = gData.GetPetAction();
	local player = gData.GetPlayer();
	local zone = gData.GetEnvironment();
	local ew = gData.GetEquipment();
	local bSA = utilities.fBuffed('Sneak Attack');
	local bTA = utilities.fBuffed('Trick Attack');
	local eWeap = nil;
	local bIgnoreLocks;
	local cKey,sGear;

	utilities.Reminder();		-- See if reminder should be printed

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

	profile.sPetAction = nil;

	-- Save the name of the main weapon
	if ew['Main'] ~= nil then
		eWeap = ew['Main'].Name;
	end

	-- Assuming you're /bst, when you want to reward your pet and you do not have pet food 
	-- equipped, the current item in the ammo slot is saved. The following will set it back
	-- to what you had before unless the slot is locked.
	if player.SubJob == 'BST' and 
	   profile.settings.bAmmo == true and
	   locks.fIsSlotLocked('ammo') == false then
		gFunc.ForceEquip('Ammo',profile.settings.sAmmo);
		profile.settings.sAmmo = nil;
		profile.settings.bAmmo = false;
	end

	-- Clear out the CurrentGear in case of leftovers
	utilities.ClearSet(crossjobs.Sets.CurrentGear);

	-- Now process the pet/player statuses accordingly.
	if (pet ~= nil and pet.Status == 'Engaged') or (player.Status == 'Engaged') then
		if bSA == true or bTA == true then
			-- If sneak attack or trick attack up, make sure the appropriate gear set is
			-- equipped to maximize the damage. Note that if a weapon skill follows, the
			-- weapon skill set will take priority.
			gear.MoveToDynamicGS(profile.Sets.SATA,crossjobs.Sets.CurrentGear,false,'SATA');
		elseif bSA == true then					-- SA
			gear.MoveToDynamicGS(profile.Sets.SneakAttack,crossjobs.Sets.CurrentGear,false,'SA');
		elseif bTA == true then					-- TA
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
		-- Player kneeling. Based on priority, order regen and refresh
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
		-- Assume player idling
		gear.MoveToDynamicGS(profile.Sets.Default,crossjobs.Sets.CurrentGear,false,'Default');
	end
		
	-- Make sure to equip the appropriate elemental staff if you have a smn pet out
	if pets.fSummonerPet() == true then
		local sStave = gear.fCheckForElementalGearByValue('staff','Summons',pet.Name);
		if sStave ~= nil then
			gear.fSwapToStave(sStave,false,crossjobs.Sets.CurrentGear);
		end
	end
	
	-- And make sure a weapon equipped.
	local tgear = gData.GetEquipment();
	if tgear.Main == nil or tgear.Main.Name == nil then
		gear.MoveToDynamicGS(profile.Sets.Start_Weapons,crossjobs.Sets.CurrentGear,true,'Start_Weapons');
	end
	
	-- Equip the composited HandleDefault set
	gear.EquipTheGear(crossjobs.Sets.CurrentGear,bIgnoreLocks);
	
	-- Lastly, update the display, just in case
	displaybar.UpdateBarStatic();
end		-- HandleDefault

--[[
	HandleAbility is used to change the player's gear appropriately for the specified avatar ability.
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
	magic skill, duration, magic attack bonus, magic accuracy, and potency.
--]]

function profile.HandleMidcast()

	-- Only process if /gswap is turned on
	if utilities.fGetToggle('GSwap') == false then		-- Only gear swap if this flag is true
		return;
	end

	-- Call the common HandleMidcast now
	magic.HandleMidcast();
end		-- gcinclude.HandleMidcast

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
