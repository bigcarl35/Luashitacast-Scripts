	local profile = {};

--[[
	This file contains all the gear sets associated with the SMN job.
	
	Gear Sets last updated: July 9, 2026
	Code update: July 30, 2026

	Intended Role: All Levels
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
	Gear Sets are what Luashitacast equips based on actions that the code tracks. So things like:
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
				['Group//TANK'] = {
					Subset = 'YYY',					-- //TANK level
				},
				['Group//NOT_TANK'] = {
					Subset = 'ZZZ',					-- //NOT_TANK level
					['Group//TIME:NIGHTTIME'] = {
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
		
		- FYI: not noted, but top level elemental blood pact rage skills do not close
		  weapon skillchains
--]]

--[[
	The "default" gear set is what is worn when you're not fighting (neither you nor your pet)
	and you're not resting. It covers everything else: idling, traveling, in town, etc. This
	set displays what your character looks like most of the	time. This set does not
	distinguish the type of activities you're doing by default, so use inlines accordingly.

	Note: The inclusion of the non-group entries will not cause an issue since the slots
	are not found in either group, so order of process doesn't matter. The non-group section
	could be made into a reference set and included via 'subset', but since it's only used
	in the 'default' gear set, I saw no reason to do so.

	Priority:
		Recovery (while standing), movement, defense, fashion
--]]
	
	['Default'] = {
		['Main//WSWAP'] = { 'Terra\'s Staff', 'Pilgrim\'s Wand' },	-- -20% physical damage
		Ammo   = { 'Hedgehog Bomb', 'Fortune Egg' },
		['GROUP//TOWN'] = {
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
		['GROUP//NOT_TOWN'] = {
			['GROUP//KITE'] = {
				-- I don't have movement gear, so use my evasion set
				SUBSET = 'Evasion',
			},
			['GROUP//NOT_KITE'] = {
				-- Not kiting, this is normal gear when you're outside of town. Start with the pieces that are equipped
				-- regardless of whether the player has a pet or not
				SUBSET = 'rEnmity_Minus',
				Neck  = { 'Rep.Gold Medal//NOT_OWN','Uggalepih Pendant//TIME:NIGHTTIME', 'Fenrir\'s Torque//TIME:DAYTIME', 'Star Necklace', 'Spirit Torque', 'Justice Badge' },
				Ears  = { 'Bat Earring//BLINDED', 'Loquac. Earring', 'Coral Earring//DT:MAGICAL', 'Bat Earring', 'Energy Earring +1', 'Energy Earring +1' },
				Rings = { 'Evoker\'s Ring', 'Tamas Ring', 'Ether Ring', 'Astral Ring', 'Astral Ring' },
				Back  = { 'Blue Cape', 'White Cape' },
				Waist = { 'Hierarch Belt', 'Powerful Rope', 'Friar\'s Rope' },
				['GROUP//SMN:PET'] = {
					-- has a SMN pet. Mode is ignored since that only applies when the pet is fighting. Perpetuation cost is emphasized here
					-- with a consideration for the summoner's MP and/or safety
					Head   = { 'Smn. Horn +1//SMN:PETMW', 'Austere Hat', 'Silver Hairpin +1' },
					Hands  = { 'Carbuncle Mitts//PETNAME:CARBUNCLE','Nashira Gages', 'Shep. Bracers' },
					Body   = { 'Yinyang Robe//MPP.LT.94', 'Summoner\'s Dblt.//SMN:PETMD', 'Yinyang Robe', 'Vermillion Cloak' },
					Legs   = { 'Summoner\'s Spats//SMN:SPIRIT:EP', 'Shep. Hose' },
					Feet   = 'Evk. Pigaches +1',
				},
				['GROUP//NOT_SMN:PET'] = {
					-- has a pet, but not a SMN pet. This implies /BST or /PUP
					Head   = 'Smn. Horn +1',
					Hands  = 'Shep. Bracers',
					Body   = { 'Yinyang Robe//MPP.LT.94', 'Vermillion Cloak//MPP.LT.94','Summoner\'s Dblt.' },
					Legs   = 'Shep. Hose',
					Feet   = 'Evk. Pigaches +1',
				},
				['GROUP//NOT_PET'] = {
					-- has no pet
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

	If you're working with a Summoner Pet, what stat is emphasized depends on what the Mode is set to:

	Mode:PERP
		Stat priority order:
			Perpetuation cost, Pet enhancement, Haste, accuracy, critical hit, etc. Adding stats for
			the SMN's fighting prowess is kind of pointless since SMNs are lousy fighters, but you
			might consider defensive gear or enmity down gear.

	Mode:ATTK
		Stat priority order:
			Pet attack enhancements, haste, accuracy, critical hit, etc. Perpetuation cost is more
			of an addendum, something to fill in open slots with. Like with Mode:PERP,  SMN's are
			lousy fighters, so maybe defensive gear or enmity down gear to round out the set.

	Mode:ENMM
		Stat priority order:
			With the rebase and the sharing on enmity with your avatar, an option where your pet
			doesn't generate as much enmity is a valid approach. Enmity down for your avatar,
			perpetuation cost, and then gear that improves the damage your pet can do. Round it
			out with player defensive gear if needed.

	Kiting should be about movement gear followed by defensive, and perpetuation costs. I unfortunately
	do not have much of those types of gear, so I have emphasized evasion gear. It works, but is not
	ideal.

	You might notice the lack of weapon specified. By default Luashitacast auto equips (unless you
	indicate otherwise) the appropriate elemental staff for the action. Currently there's no way
	to specify "equip this weapon if that avatar is out". Disabling the auto equips means you have to
	explicitly define each case.
--]]

	['TP'] = {
		SUBSET = 'rTP_Baseline',
		['GROUP//KITE'] = {
			SUBSET = 'Evasion',
		},
		['GROUP//NOT_KITE'] = {
			['GROUP//SMN:PET'] = {
				-- has a SMN pet
				Neck  = { 'Rep.Gold Medal//NOT_OWN','Uggalepih Pendant//TIME:NIGHTTIME', 'Fenrir\'s Torque//TIME:DAYTIME', 'Star Necklace', 'Spirit Torque', 'Justice Badge' },
				Ears  = { 'Bat Earring//BLINDED//NOT_PETF', 'Beastly Earring//PETF', 'Loquac. Earring', 'Coral Earring//DT:MAGICAL', 'Bat Earring', 'Energy Earring +1', 'Energy Earring +1' },
				Back  = { 'Astute Cape', 'Blue Cape', 'Fed. Army Mantle', 'White Cape' },
				Waist = { 'Hierarch Belt', 'Powerful Rope','Friar\'s Rope' },
				Rings = { 'Evoker\'s Ring', 'Tamas Ring', 'Ether Ring', 'Astral Ring', 'Astral Ring' },
				['GROUP//MODE:PERP'] = {
					-- Emphasis on lowering perpetuation cost
					Head  = 'Smn. Horn +1//SMN:PETMW', 									-- -3 perp if matches weather
					Body  = { 'Summoner\'s Dblt.//SMN:PETMD', 'Austere Robe' },			-- -3 perp if pet's element matches day, -1 perp
					Hands = { 'Carbuncle Mitts//PETNAME:CARBUNCLE', 'Nashira Gages' },	-- halves perp cost of carbuncle, -1 perp
					Feet  = 'Evk. Pigaches +1',											-- -1 perp
				},
				['GROUP//MODE:ATTK'] = {
					-- Emphasis on pet Attack Power
					Head  = { 'Shep. Bonnet//PETF', 'Nashira Turban', 'Austere Hat' },	-- +5 pet accuracy, +5 MAcc/-5 Enmity/+2% Haste, +2 smn magic skill/-2 BP delay
					Body  = 'Summoner\'s Dblt.',										-- +3% per crit rate/-3 BP delay/-3 perpetuation on matching days
					Hands = { 'Smn. Bracers +1', 'Nashira Gages', 'Shep. Bracers' },	-- Enhanced pet accuracy/+12 smn magic skill/
					Legs  = { 'Evk. Spats +1', 'Shep. Hose' },
					Feet  = 'Summoner\'s Pgch.',
				},
				['GROUP//MODE:ENMM'] = {
					-- Emphasis on Enmity Minus for avatar and summoner
					Head  = { 'Nashira Turban', 'Evoker\'s Horn' },						-- -5 enmity, -3 pet enmity
					Body  = 'Evoker\'s Doublet',										-- -2 pet enmity
					Hands = { 'Nashira Gages', 'Evoker\'s Bracers' },					-- -4 enmity, -2 pet enmity
					Legs  = { 'Hydra Brais', 'Evk. Spats +1' },							-- -6 enmity, -2 pet enmity
					Feet  = { 'Evk. Pigaches +1', 'Evoker\'s Boots' },					-- -4 pet enmity, -2 enmity
				},
			},
			['GROUP//NOT_SMN:PET'] = {
				-- since not a SMN pet, has to be either /BST with a charmed pet or /PUP which can only do maneauvers
				Head  = 'Shep. Bonnet//PETF',
				Ears  = { 'Bat Earring//BLINDED//NOT_PETF', 'Beastly Earring//PETF', 'Loquac. Earring', 'Coral Earring//DT:MAGICAL', 'Bat Earring', 'Energy Earring +1', 'Energy Earring +1' },
				Body  = { 'Yinyang Robe', 'Vermillion Cloak' },
				Rings = { 'Tamas Ring', 'Ether Ring', 'Astral Ring', 'Astral Ring' },
				Legs  = 'Shep. Hose',
				Feet  = { 'Mannequin Pumps', 'Waders' },
			},
			['GROUP//NOT_PET'] = {
				-- Emergency, no pet! Equip defense/evasion gear
				Main  = 'Terra\'s Staff',															-- -20% Physical damage
				Ammo  = 'Hedgehog Bomb',															-- -1 Enmity
				Head  = { 'Smn. Horn +1', 'Austere Hat', 'Shep. Bonnet' },							-- Def: 19/13/7..+4 HP
				Neck  = { 'Promise Badge', 'Justice Badge' },										-- Def: 3..+10 HP/1
				Ears  = { 'Bat Earring//BLINDED', 'Coral Earring//DT:MAGICAL', 'Ethereal Earring' },-- +15 Eva,-1% Magical attack..-5 Eva,+5 Eva
				Body  = { 'Yinyang Robe', 'Vermillion Cloak', 'Austere Robe', 'Seer\'s Tunic' },	-- Def: 43/46*/29/18
				Hands = { 'Nashira Gages', 'Smn. Bracers +1', 'Wonder Mitts' },						-- Def: 18..-4 Enmity/16/6..12 HP
				Rings = { 'Flame Ring', 'Bomb Queen Ring', 'Toreador\'s Ring', 'Toreador\'s Ring' },-- Def: 3, +75 HP, Def: 1..10 HP, Def: 1..10 HP,
				Back  = { 'Fed. Army Mantle', 'White Cape' },										-- Def: 6/3
				Waist = { 'Hierarch Belt', 'Powerful Rope' },										-- Def: 3, +20 HP
				Legs  = { 'Summoner\'s Spats', 'Wonder Braccae', 'Baron\'s Slops' },				-- Def: 29/12..+21 HP..+2 VIT/11
				Feet  = { 'Dance Shoes +1', 'Dance Shoes', 'Creek F Clomps', 'Mannequin Pumps', 'Waders' }	-- Def: 14/13..+7/6 Eva..-1 VIT/9..+35 HP..+4 VIT/6/2
			},
		},
	},

--[[
	Looking a the rTP_Baseline reference set, you may be wondering why I didn't just copy this section into the beginning of the
	TP set. I didn't do that because the results would have been different because of how a gear set is processed: first subsets
	are processed, then groups, and lastly individual slots. rTP_Baseline is just a copy of the Karma version of my TP gear set,
	reformatted to conform to Boxcar's standards. It's the groundwork I want in place so that if any "holes" exist in the TP
	definition, something appropriate will be there to bleed through.
--]]
	['rTP_Baseline'] = {
		SUBSET =  'Default',
		Head  = { 'Smn. Horn +1//SMN:PETMW', 'Shep. Bonnet//PETF' },
		Ears  = { 'Bat Earring//BLINDED//NOT_PETF', 'Loquac. Earring', 'Beastly Earring//PETF', 'Coral Earring//DT:MAGICAL', 'Bat Earring', 'Energy Earring +1', 'Energy Earring +1' },
		Body  = { 'Summoner\'s Dblt.//SMN:PETMD', 'Yinyang Robe', 'Vermillion Cloak' },
		Hands = { 'Carbuncle Mitts//PETNAME:CARBUNCLE', 'Nashira Gages//SMN:PET' },
		Legs  = 'Evk. Spats +1',
		Feet  = { 'Summoner\'s Pgch.', 'Evk. Pigaches +1', 'Mannequin Pumps', 'Waders' },
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
		['GROUP//PETF'] = {			-- Accuracy is for pets
			Head  = 'Shep. Bonnet',					-- Pet: +5 Acc/+3Macc
			Ears  = 'Beastly Earring',				-- Pet: +10 Acc
			Hands = 'SMN. Bracers +1//SMN:PET',		-- Avatar: Enhances acc
			Legs  = 'Evk. Spats +1//SMN:PET',		-- Avatar: Enhances acc
		},
		['GROUP//NOT_PETF'] = {		-- Accuracy is for player
			Ammo  = 'Orphic Egg//PJB:BRD',								-- +1 Acc if BRD in party
			Head  = { 'Optical Hat', 'Hydra Beret','Empress Hairpin' },	-- +10/3 Acc, +3 DEX
			Neck  = { 'Peacock Amulet',	'Spike Necklace' },				-- +10 Acc, +3 DEX
			Body  = 'Mrc.Cpt. Doublet',									-- +1 DEX
			Hands = 'Battle Gloves',									-- +3 Acc
			Rings = { 'Toreador\'s Ring', 'Toreador\'s Ring', 'Woodsman Ring', 'Woodsman Ring', 'Jaeger Ring', 'Kshama Ring No.2' },	-- +7/+7/+5/+5/4/2 Acc
			Waist = { 'Life Belt', 'Tilt Belt', 'Mrc.Cpt. Belt' },		-- +10/5 Acc, +1 DEX
			Legs  = 'Hydra Brais',										-- +10 Acc
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
				['Neck']  = 'rAccuracy::Necks',
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
		Ammo  = 'Orphic Egg//PJB:BRD',					-- +1 Eva if BRD in party
		Head  = { 'Optical Hat', 'Empress Hairpin' },	-- +10/10 Eva
		Neck  = 'Spirit Torque',						-- +5 Eva
		Ears  = { 'Bat Earring//BLINDED', 'Ethereal Earring', 'Genin Earring//SJ:NIN', 'Drone Earring' },	-- +15 Eva while blinded, +5 Eva, +4/3 AGI
		Body  = 'Yinyang Robe//IF:Vermillion Cloak',	-- Filler, V.Cloak has -10 eva
		Hands = 'Battle Gloves',						-- +3 Eva
		Rings = { 'Ether Ring//IF:Woodsman Ring', 'Astral Ring//IF:Woodsman Ring', 'Astral Ring//IF:Woodsman Ring' },	-- Filler, Woodsman Ring has -5 eva
		Back  = 'Boxer\'s Mantle',						-- +10 Eva
		Waist = 'Swift Belt//IF:Tilt Belt', 			-- Filler, Tilt belt has -5 eva
    },

--[[
	rDamageTaken set is not equipped directly but rather from subsets since it's a reference set. It's a
	way to reduce a specific type of damage. As such it's optional and up to the player to decide where
	it should be included via a Subset. (Prior versions had three separate sets.)
--]]

	['rDamage_Taken'] = {
		['GROUP//DT:PHYSICAL'] = {
			Main = 'Terra\'s Staff//WSWAP',				-- -20% damage reduction from physical
		},
		['GROUP//DT:BREATH'] = {
		},
		['GROUP//DT:MAGICAL'] = {
			Ears = 'Coral Earring',						--  -1% damage reduction from magic
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
	
	['Resting_Refresh'] = {
		SUBSET = 'rDamage_Taken',
		['Main//WSWAP'] = { 'Pluto\'s Staff', 'Kukulcan\'s Staff', 'Pilgrim\'s Wand' },	-- +10/3/2 MP/tick while resting
		Head   = 'Hydra Beret',															-- +2 MP/tick while healing
		Neck   = 'Checkered Scarf',														-- +1 MP/tick while healing
		Body   = { 'Errant Hpl.', 'Yinyang Robe', 'Vermillion Cloak', 'Seer\'s Tunic' },-- +5 MP/tick while healing, adds "refresh", adds "refresh", +1/tick while healing
		Waist  = 'Hierarch Belt',														-- +2 MP/tick while resting
		Legs   = { 'Hydra Brais', 'Baron\'s Slops' },									-- +1/1	 MP/tick while resting
	},

	['Resting_Regen'] = {
		SUBSET = 'rDamage_Taken',
		Neck   = 'Checkered Scarf',														-- +1 HP/tick while healing
		Hands  = { 'Carbuncle\'s Cuffs//SHINING_RUBY','Shep. Bracers' },				-- +5/1 HP/tick while resting
		Waist  = 'Hierarch Belt',														-- +2 HP/tick while resting
	},
	
--[[
	Start weapons are where you define what you want the first row of equipment to look 
	like when you either log in as a SMN or switch your main job to SMN. Any other gear 
	you mention will be overridden by the Default set, so no need to include here.
--]]

	['Start_Weapons'] = {
	    Main = { 'Terra\'s Staff', 'Kukulcan\'s Staff', 'Pilgrim\'s Wand' },
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
	*************************
	* Spell Casting Subsets *
	*************************

	Initially define the Reference gear sets that are primary stat based.

	Note: as log as the reference set does not contain any weapons, these
	reference sets can be referred to in weapon skill sets.
--]]

	-- Strength Reference gear set
	['rSTR'] = {
		Neck   = { 'Justice Torque', 'Spike Necklace' },		-- +5/3 STR
		Body   = 'Wonder Kaftan',								-- +1 STR
		Hands  = 'Wonder Mitts',								-- +3 STR
		Rings  = { 'Flame Ring', 'Kshama Ring No.8' },			-- +5/3 STR
		Legs   = 'Wonder Braccae',								-- +1 STR
		Feet   = { 'Creek F clomps', 'Wonder Clomps' },			-- +4/2 STR
	},

	-- Dexterity Reference gear set
	['rDEX'] = {
		Head   = 'Empress Hairpin',								-- +3 DEX
		Neck   = { 'Love Torque', 'Spike Necklace' },			-- +5/3 DEX
		Rings  = 'Kshama Ring No.2',							-- +3 DEX
		Body   = 'Yinyang Robe//IF:Errant Hpl.',				-- filler, voids -7 DEX
		Legs   = 'Evk. Spats +1//IF:Errant Slops',				-- filler, voids -5 DEX
		Feet   = 'Bounding Boots',								-- +3 DEX
	},

	-- Vitality Reference gear set
	['rVIT'] = {
		Body   = 'Wonder Kaftan',								-- +2 VIT
		Hands  = 'Evoker\'s Bracers',							-- +4 VIT
		Rings  = 'Kshama Ring No.4',							-- +3 VIT
		Waist  = 'Mrc.Cpt. Belt',								-- +1 VIT
		Legs   = { 'Wonder Braccae', 'Shep. Hose' },			-- +2/2 VIT
		Feet   = { 'Creek F Clomps', 'Summoner\'s Pgch.' },		-- +4/3 VIT
	},

	-- Agility Reference gear set
	['rAGI'] = {
		Head   = 'Empress Hairpin',								-- +3 AGI
		Ears   = { 'Genin Earring//SJ:NIN', 'Drone Earring' },	-- +4 AGI if sj NIN, +3 AGI
		Body   = 'Yinyang Robe//IF:Errant Hpl.',				-- filler, voids -7 AGI
		Rings  = 'Kshama Ring No.3',							-- +3 AGI
		Back   = 'Fed. Army Mantle',							-- +2 AGI
		Waist  = 'Mrc.Cpt. Belt',								-- +1 AGIhttps://tonypolecastro.com/home/
		Legs   = 'Evk. Spats +1//IF:Errant Slops',				-- filler, voids -5 AGI
	},

	-- Intelligence Reference gear set
	['rINT'] = {
		Head  = { 'Smn. Horn +1', 'Evoker\'s Horn' },			-- +4/3 INT
		Body  = { 'Errant Hpl.', 'Baron\'s Saio' },				-- +10/1 INT
		Hands = 'Errant Cuffs',									-- +7 INT
		Rings = { 'Tamas Ring', 'Flame Ring' },					-- +5/2 INT
		Waist = { 'Penitent\'s Rope', 'Mrc.Cpt. Belt' },		-- +5/1 INT
		Legs  = 'Errant Slops',									-- +7 INT
		Feet  = { 'Rostrum Pumps', 'Mannequin Pumps' },			-- +3/1 INT
	},

	-- Mind Reference gear set
	['rMND'] = {
		Neck  = { 'Promise Badge', 'Justice Badge' },						-- +5/3 MND
		Ears  = 'Geist Earring',											-- +1 MND
		Body  = { 'Errant Hpl.', 'Evoker\'s Doublet', 'Wonder Kaftan', 'Baron\'s Saio' },	-- +10/3/1/1 MND
		Hands = 'Baron\'s Cuffs',											-- +1 MND
		Rings = { 'Tamas Ring', 'Kshama Ring No.9' },						-- +5/3/2 MND
		Back  = 'White Cape',												-- +2 MND
		Waist = { 'Penitent\'s Rope', 'Mrc.Cpt. Belt', 'Friar\'s Rope' },	-- +5/1/1 MND
		Legs  = { 'Errant Slops', 'Summoner\'s Spats', 'Wonder Braccae' },	-- +7/3/2 MND
		Feet  = { 'Rostrum Pumps', 'Mannequin Pumps', 'Seer\'s Pumps' }, 	-- +3/2/1 MND		
	},

	-- Charisma Reference gear set. Provides accuracy with singing
	['rCHR'] = {
		Head  = 'Entrancing Ribbon',										-- +2 CHR
		Ears  = 'Beastly Earring',											-- +2 CHR
		Body  = 'Errant Hpl.',												-- +10 CHR
		Neck  = { 'Star Necklace', 'Flower Necklace' },						-- +3/3 CHR
		Rings = 'Kshama Ring No.6',											-- +3 CHR
		Waist = { 'Corsette', 'Mrc.Cpt. Belt' },							-- +5/1 CHR
		Legs  = 'Errant Slops',												-- +7 CHR
	},

	-- Enmity+ Reference gear set, for player
	['rEnmity_Plus'] = {
	},

	-- Enmity- Reference gear set
	['rEnmity_Minus'] = {
		Ammo  = 'Hedgehog Bomb',						-- -1 Enmity
		Neck  = 'Fenrir\'s Torque//TIME:NIGHTTIME',		-- -3 Emnity at night
		Hands = { 'Nashira Gages', 'Errant Cuffs' },	-- -4/-2 Enmity
		Rings = 'Tamas Ring',							-- -5 Enmity
		Waist = 'Penitent\'s Rope',						-- -3 Enmity
		Legs  = { 'Hydra Brais', 'Evk. Spats +1', 'Errant Slops' },	-- -6/3/-3 Enmity
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
		Ears =  'Loquac. Earring',																		-- Enhances Fastcast
		Hands = { 'Carbuncle\'s Cuffs//SMN:AVATAR', 'Carbuncle\'s Cuffs//SMN:SPIRIT:ES' },				-- Summoning magic casting time -1x2
		Feet =  { 'Evoker\'s Boots//SMN:AVATAR', 'Evoker\'s Boots//SMN:SPIRIT:ES', 'Rostrum Pumps' },	-- Summoning magic casting time -1x2, Enhances Fastcast
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
				   'rSinging_Skill//MT:SINGING' },
		Hands  = 'Nashira Gages',		-- +3 MAcc
		Rings  = 'Tamas Ring',			-- +5 MAcc
		Feet   = 'Nashira Crackows',	-- +2 MAcc	
	},

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
		Back = 'Altruistic Cape'		-- +5 Healing Magic Skill
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
	has a 33% chance to boost the cure's effacacy by 10% each (25%
	if double light weather.) Casting cures on darksday or in dark 
	weather has an equal chance of a penalty.
	
	Once the "CuringMagic" set is equipped, the midcast routine will
	also check to see if you have an Apollo/Light staff for it's Cure 
	Potency.

	Note: As a smn I have very little healing magic skill, so
	what stat to emphasis needs to be decided upon individually
	amongst healing	magic, MND and VIT. Using the above equation,
	since MND is 3x times, let's use 3 as the base score.

		1 MND == 5 healing magic skill == 3 VIT

	Consider this ratio when deciding what piece should be equipped.

	Also note that none of the pieces in this set have cure potency.
	The use of a light/apollo staff and/or elemental obi's will be
	done separately from this set. If in the future I get a piece
	with cure potency, the question will be whether I meet the cap on
	the power rating. Remember that healing magic, mnd and vit are
	only used to determine if you meet the cap of a spell. Cure potency
	is an additional affect that adds to the spell's potential amount
	cured.
--]]	
	
	['CuringMagic'] = {
		Ammo  = 'Hedgehog Bomb',							-- -1 emnity
		Head  = 'Nashira Turban',							-- -5 emnity
		Neck  = { 'Promise Badge', 'Justice Badge' },		-- +5/3 MND
		Ears  = 'Geist Earring',							-- +1 MND
		Body  = { 'Errant Hpl.', 'Evoker\'s Doublet', 'Wonder Kaftan', 'Baron\'s Saio' },	-- +10/3/1/1 MND
		Hands = { 'Evoker\'s Bracers', 'Baron\'s Cuffs' },	-- +4 VIT, +1 MND
		Rings = { 'Tamas Ring', 'Kshama Ring No.9','Kshama Ring No.4' },					-- +5/3 MND, 3 VIT
		Back  = { 'White Cape', 'Altruistic Cape' },		-- +2 MND, +5 Healing Magic Skill
		Waist = { 'Penitent\'s Rope', 'Mrc.Cpt. Belt', 'Friar\'s Rope' },	-- +5 MND, 1 MND/1 VIT, 1 MND
		Legs  = { 'Errant Slops', 'Summoner\'s Spats', 'Wonder Braccae', 'Shep. Hose' },	-- +7/3/2 MND, 2 VIT
		Feet  = { 'Rostrum Pumps', 'Mannequin Pumps', 'Creek F Clomps', 'Seer\'s Pumps' }, 	-- +3/2 MND, 4 VIT, 1 MND
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

	Note: I'm including CuringMagic as a subset since it does not contain
	any cure potency. If in the future this changes, this set should then
	be changed to be explicitly defined since cure potency has no effect
	on offensive curing.
--]]

	['OffensiveCuring'] = {
		SUBSET = {
			[1] = 'CuringMagic',
			[2] = 'rMAB',
		},
	},

--[[
	This last set is used for all non-cure Healing Magic spells. Only
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
	Skill as follows:January
	
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
	-- Note: The slot names were bracketed for consistency. Only the "body" slot
	-- name had to be bracketed.
	['rSummoning_Magic_Skill'] = {
		['Head']  = { 'Evoker\'s Horn',	'Austere Hat' },	-- +5/2 Summoning Magic Skill
		['Neck']  = 'Smn. Torque',							-- +7 Summoning Magic Skill
		['Body//IF:Vermillion Cloak'] = { 'Summoner\'s Dblt.', 'Austere Robe'},		-- Conditional removes the V.Cloak so a head will be equipped
		['Hands'] = 'Smn. Bracers +1',						-- +12 Summoning MagicSkill
		['Rings'] = 'Evoker\'s Ring',						-- +10 Summoning Magic Skill
		['Back']  = 'Astute Cape',							-- +5 Summoning Magic Skill
		['Feet']  = 'Nashira Crackows',						-- +5 Summoning Magic Skill
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
		Hands  = 'Carbuncle\'s Cuffs//SMN:PET',		-- Summoning magic casting time -1, works for both avatars and spirits
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
	
	['Dread'] = {
		SUBSET = 'rDark_Magic_Skill',
	},

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

	Weaponskills are identified by the stats that boost the damage they do.
	With the introduction of groups in a gearset, weaponskills are now handled
	by a singular gear set that is split out by the stats that need emphasizing.

	SMN has proficiencies in: staff (B), club (C+), dagger (E), but you can
	inherit proficiencies from a subjob. Each grouping within the weaponskill
	gear set is based on one or more stats. Each stat grouping has a comment
	outlining what skills use said stat. Listed is what weapon/weaponskill a
	SMN has proficiency in as well as any other combination that uses that
	stat combination. (There's no guarantee though that a SMN can wield the
	weapon that is required to generate that weaponskill. It is included here
	because it is a possibility from your sub job.)
--]]

	['Weaponskill'] = {
		SUBSET = 'rAttackPower',	-- All weaponskills gearsets have attack power as a foundation
		['GROUP//WS_CHR'] = {
			-- * Charisma based *

			-- Dagger: Shadowstitch
			SUBSET  = 'rCHR',
		},
		['GROUP//WS_DEX'] = {
			-- * Dexterity based *

			-- Dagger: Wasp Sting,Viper Bite^
			--  ^ Subjob must be one of: RDM,THF,BRD,RNG,NIN
			SUBSET  = 'rDEX',
		},
		['GROUP//WS_DEXAGI'] = {
			-- * Dexterity and Agility based, even weighting *

			-- SMN has no proficiency with this combination
		},
		['GROUP//WS_DEXCHR'] = {
			-- * Dexterity and Charisma based, even weighting *

			-- SMN has no proficiency with this combination
		},
		['GROUP//WS_DEXINT'] = {
			-- * Dexterity and Intelligence based *

			-- Dagger: Gust Slash,Cyclone^
			-- 	^ Subjob must be one of: RDM,THF,BRD,RNG,NIN
			Head   = { 'Smn. Horn +1', 'Evoker\'s Horn', 'Empress Hairpin' },	-- +4/3 INT, +3 DEX
			Neck   = { 'Spike Necklace', 'Opo-opo Necklace' },	-- +3/1 DEX
			Body   = { 'Black Cotehardie', 'Baron\'s Saio' },	-- +2 INT/+2 DEX, +1 INT
			Hands  = 'Errant Cuffs',							-- +5 INT
			Rings  = { 'Tamas Ring', 'Kshama Ring No.2', 'Kshama Ring No.5', 'Flame Ring' },	-- +5 INT, +3 DEX, +3 INT, +2 INT
			Waist  = { 'Penitent\'s Rope', 'Mrc.Cpt. Belt' },	-- +5 INT, +1 INT/+1 DEX
			Legs   = 'Errant Slops',							-- +7 INT/-5 DEX
			Feet   = { 'Rostrum Pumps', 'Mannequin Pumps' },	-- +3/1 INT
		},
		['GROUP//WS_INT'] = {
			-- * Intellegence based *

			-- Staff: Gate of Tartarus
			SUBSET  = 'rINT',
		},
		['GROUP//WS_INTMND'] = {
			-- * Intellegence and Mind based, even weighting *

			-- Staff: Spirit Taker
			Head   = { 'Smn. Horn +1', 'Evoker\'s Horn' },		-- +4/3 INT
			Neck   = { 'Promise Badge', 'Justice Badge' },		-- +5/3 MND
			Ears   = 'Geist Earring',							-- +1 MND
			Body   = { 'Errant Hpl.', 'Evoker\'s Doublet', 'Wonder Kaftan', 'Baron\'s Saio' },	-- +10 INT/+10 MND, +3 MND, +1 INT, +1 MND
			Hands  = { 'Errant Cuffs', 'Baron\'s Cuffs' },		-- +7 INT, +3/1 MND
			Rings  = { 'Tamas Ring', 'Kshama Ring No.9', 'Kshama Ring No.5', 'Flame Ring' },	-- +5 INT/+5 MND, +3 MND, +3 INT, +2 INT
			Waist  = { 'Penitent\'s Rope', 'Mrc.Cpt. Belt', 'Friar\'s Rope' },	-- +5 INT/+5 MND, +1 INT/+1 MND, +1 MND
			Legs   = { 'Errant Slops', 'Summoner\'s Spats', 'Wonder Braccae' },	-- +7 MND/+7 INT, +3/2 MND
			Feet   = { 'Rostrum Pumps', 'Mannequin Pumps', 'Seer\'s Pumps' },	-- +3 MND/+3 INT, +2 MND/1 INT, +1 MND
		},
		['GROUP//WS_MND'] = {
			-- * Mind based *

			-- Dagger: Energy Steal, Energy Drain^
			--	^ Subjob must be RDM,THF,BRD,RNG, or NIN
			SUBSET  = 'rMND',
		},
		['GROUP//WS_RANGED_AGI'] = {
			-- * Strength and Agility bases, even weighting *

			-- SMN has no proficiency with this combination

			-- Possibly from the subjob if you can equip the weapon:
			-- Marksmanship: Hot Shot, Split Shot, Sniper Shot, Slug Shot
		},
		['GROUP//WS_RANGED_STRAGI'] = {
			-- * Strength and Agility bases, even weighting *

			-- SMN has no proficiency with this combination

			-- Possibly from the subjob if you can equip the weapon:
			-- Archery: Flaming Arrow, Piercing Arrow, Dulling Arrow, Sidewinder
		},
		['GROUP//WS_STR'] = {
			-- * Strength based *

			-- Staff: Heavy Swing,Shell Crusher,Full Swing
			-- Club: Brainshaker,Skullbreaker,True Strike

			-- Possibly from the subjob if you can equip the weapon:
			-- Axe: Raging Axe, Smash Axe, Gale Axe, Avalanche Axe
			-- Great Axe: Iron Tempest, Sturmwind
			-- Great Katana: Tachi: Enpi, Tachi: Hobaku, Tachi: Goten, Tachi: Kagero
			-- Great Sword: Hard Slash
			-- Polearm: Double Thrust, Leg Sweep^
			--  ^ Subjob cannot be PLD
			-- Scythe: Slice
			-- Sword: Flat Blade
			SUBSET = 'rSTR',
		},
		['GROUP//WS_STRAGI'] = {	-- * Strength and Agility bases, even weighting *
			-- SMN has no proficiency with this combination
		},
		['GROUP//WS_STRDEX'] = {	-- * Strength and Dexterity bases, even weighting *
			-- SMN has no proficiency with this combination

			-- Possibly from the subjob if you can equip the weapon:
			-- Hand to hand: Combo, Backhand Blow^
			--  ^ Subjob cannot be NIN or THF
			-- Katana: Blade: Rin, Blade: Retsu
			-- Sword: Fast Blade
		},
		['GROUP//WS_STRINT'] = {
			-- * Strength and Intelligence based, even weighting *

			-- Staff: Rock Crusher,Earth Crusher,Cataclysm

			-- Possibly from the subjob if you can equip the weapon:
			-- Katana: Blade: Teki, Blade: To
			-- Sword: Burning Blade
			-- Great Sword: Frostbite, Freezebite
			-- Polearm: Thunder Thrust, Raiden Thrust^
			--  ^ Subjob must be PLD or DRG
			-- Scythe: Dark Harvest, Shadow of Death^,Nightmare Scythe^^
			--  ^ Subjob must be WAR or DRK
			--  ^^ Subjob cannot be BLM
			Head   = { 'Smn. Horn +1', 'Evoker\'s Horn' },		-- +4/3 INT
			Neck   = { 'Justice Torque','Spike Necklace' },		-- +5/3 STR
			Body   = { 'Black Cotehardie', 'Wonder Kaftan', 'Baron\'s Saio' },	-- +3 STR/+2 INT, +1 STR, +1 INT
			Hands  = { 'Errant Cuffs', 'Wonder Mitts' },		-- +5 INT, +3 STR
			Rings  = { 'Tamas Ring', 'Flame Ring', 'Kshama Ring No.8', 'Kshama Ring No.5' },	-- +5 STR, +5 STR/+2 INT, +3 STR, +3 INT
			Waist  = { 'Penitent\'s Rope', 'Mrc.Cpt. Belt' },	-- +5 INT, +1 INT/+1 STR
			Legs   = { 'Errant Slops', 'Wonder Braccae' },		-- +7 INT/-5 STR, +1 STR
			Feet   = { 'Rostrum Pumps', 'Wonder Clomps', 'Mannequin Pumps' },	-- +3 INT, +2 STR, +1 INT
		},
		['GROUP//WS_STRINT_30_20'] = {
			-- * Strength and Intellegence based, STR 30% to INT 20% weighting *

			-- SMN has no proficiency with this combination

			-- Possibly from the subjob if you can equip the weapon:
			-- Sword: Red Lotus Blade
		},
		['GROUP//WS_STRMND'] = {
			-- * Strength and Mind based, even weighting *

			-- Club: Shining Strike,Seraph Strike,Judgement
			-- Staff: Retribution

			-- Possibly from the subjob if you can equip the weapon:
			-- Sword: Shining Blade^
			--	^ Subjob cannot be BST
			Neck   = { 'Promise Badge', 'Justice Torque', 'Justice Badge' },	-- +5 MND, +5 STR, +3 MND
			Ears   = 'Geist Earring',											-- +1 MND
			Body   = { 'Black Cotehardie', 'Wonder Kaftan', 'Baron\'s Saio' },	-- +3 STR, +1/1 MND
			Hands  = { 'Wonder Mitts', 'Baron\'s Cuffs' },						-- +3 STR, +1 MND
			Rings  = { 'Tamas Ring', 'Flame Ring', 'Kshama Ring No.9', 'Kshama Ring No.8' },		-- +5 MND, +5 STR/-2 MND, +3 MND, +3 STR
			Back   = 'White Cape',												-- +2 MND
			Waist  = { 'Penitent\'s Rope', 'Mrc.Cpt. Belt', 'Friar\'s Rope' },	-- +5 MND, +1 STR/+1 MND, +1 MND
			Legs   = { 'Summoner\'s Spats', 'Errant Slops', 'Wonder Braccae' },	-- 3 MND, +7 MND/-5 STR, +2 MND
			Feet   = { 'Creek F Clomps', 'Rostrum Pumps', 'Mannequin Pumps', 'Wonder Clomps' },	-- +4 STR, +3/2 MND, +2 STR
		},
		['GROUP//WS_STRMND_30_50'] = {
			-- * Strength and Mind based, STR 30% to MND 50% weighting *

			-- Club: Black Halo
			Neck   = { 'Promise Badge', 'Justice Torque', 'Justice Badge' },	-- +5 MND, +5 STR, +3 MND
			Ears   = 'Geist Earring',											-- +1 MND
			Body   = { 'Black Cotehardie', 'Wonder Kaftan' },					-- +3 STR, +1 MND
			Hands  = { 'Wonder Mitts', 'Baron\'s Cuffs' },						-- +3 STR, +1 MND
			Rings  = { 'Tamas Ring', 'Flame Ring', 'Kshama Ring No.9', 'Kshama Ring No.8' },		-- +5 MND, +5 STR/-2 MND, +3 MND, +3 MND
			Back   = 'White Cape',												-- +2 MND
			Waist  = { 'Penitent\'s Rope', 'Mrc.Cpt. Belt', 'Friar\'s Rope' },	-- +5 MND, +1 STR/+1 MND, +1 MND
			Legs   = { 'Summoner\'s Spats', 'Errant Slops', 'Wonder Braccae' },	-- +3 MND, +7 MND/-5 STR, +2 MND
			Feet   = { 'Creek F Clomps', 'Rostrum Pumps', 'Mannequin Pumps', 'Wonder Clomps' },		-- +4 STR, +3/2 MND, +2 STR
		},
		['GROUP//WS_STRVIT'] = {
			-- * Strength and Vitality, even weighting *

			-- SMN has no proficiency with this combination

			-- Possibly from the subjob if you can equip the weapon:
			-- Great Sword: Shield Break, Armor Break
		},
		['WS_SKILL'] = {
			-- * Skill based *

			-- Club: Starlight,Moonlight
			Neck   = 'Love Torque//DAGGER',		-- +7 Dagger skill
		},
		['GROUP//WS_HP'] = {
			-- * HP based *

			-- SMN has no proficiency with this combination
		},
	},

--[[
	Custom weaponskill sets can be used in place of the generic stats-based sets. Place you weapon
	skill definition in the Custom_Weaponskills gearset as a group. If the weapon skill contains
	a blank in the name (ex: viper bite), change that to an underscore (ex: viper_bite)
	in the name, substitue an underscore.

	I've included a commented out example. (It's basically the strength based definition.)
--]]

	['Custom_Weaponskills'] = {
		--	['Heavy_Swing'] = {
		--		SUBSTR = {
		--			[1] = 'rAttackPower',
		--			[2] = 'rSTR',
		--		},
		--	},
	},

--[[
	The Job_Ability structure contains every ability associated with a summoner as well as all
	abilities that you can do for any subjob. While you can "flesh out" every ability, you really
	only have to worry about the abilities for subjobs you plan on using. Leaving an ability's
	group empty is fine. Not every group needs nor even has gear that can be equipped. Where
	appropriate I will add hints at what stats to emphasize.
--]]

	['Job_Ability'] = {
		-- SMN abilities
		['GROUP//JA:ASTRAL_FLOW'] = {
		},
		-- /WAR abilities
		['GROUP//JA:PROVOKE'] = {
		},
		['GROUP//JA:BERSERK'] = {
		},
		['GROUP//JA:DEFENDER'] = {
		},
		['GROUP//JA:WARCRY'] = {
		},
		-- /THF abilities

		-- Note: SNEAK_ATTACK, TRICK_ATTACK, and SATA are not included here. While they are
		-- abilities, they are handled differently in HandleDefault since the gear has to
		-- remain equipped to get the benefit.
		['GROUP//JA:STEAL'] = {
		},
		['GROUP//JA:FLEE'] = {
		},
		['GROUP//JA:MUG'] = {
		},
		-- /BLM abilities
		['GROUP//JA:ELEMENTAL_SEAL'] = {
		},

		-- /MNK abilities
		['GROUP//JA:BOOST'] = {
		},
		['GROUP//JA:FOCUS'] = {
		},
		['GROUP//JA:DODGE'] = {
		},
		['GROUP//JA:CHAKRA'] = {
		},
		-- /WHM abilities
		['GROUP//JA:DIVINE_SEAL'] = {
		},
		-- /RDM has abilities
		-- /PLD abilities
		['GROUP//JA:HOLY_CIRCLE'] = {
		},
		['GROUP//JA:SHIELD_BASH'] = {
		},
		['GROUP//JA:SENTINEL'] = {
		},
		['GROUP//JA:COVER'] = {
		},
		-- /BST abilities
		['GROUP//JA:CHARM'] = {		-- charm skill, CHR gear
			SUBSET = 'rCHR',
			['Main//WSWAP'] = { 'Apollo\'s Staff', 'Light Staff' },	-- +2/1 CHR +15/10% Charm success
		},
		['GROUP//JA:GAUGE'] = {
			-- Gauges success rate from Charm. Arguably you should equip your charm gear here
			SUBSET = 'rCHR',
			['Main//WSWAP'] = { 'Apollo\'s Staff', 'Pluto\'s Staff','Light Staff','Dark Staff' },	-- +2/1 CHR
		},
		['GROUP//JA:REWARD'] = {					-- Reward potency, reward augment, reward enhancement, MND gear
			SUBSET = 'rMND',
			['Main//WSWAP'] = { 'Neptune\'s Staff', 'Water Staff' },	-- +5/4 MND
		},
		['GROUP//JA:TAME'] = {
			-- The success rate of tame is based on the delta INT between you and your target
			SUBSET = 'rINT',
			['Main//WSWAP'] = { 'Aquilo\'s Staff', 'Ice Staff' },	-- +5/4 INT
		},
		-- /RNG abilities
		--	Very few ranged weapons (most low level gear) can be used by a smn
		['GROUP//JA:SHARPSHOT'] = {
		},
		['GROUP//JA:SCAVENGE'] = {
		},
		['GROUP//JA:CAMOUFLAGE'] = {
		},
		['GROUP//JA:BARRAGE'] = {
		},
		['GROUP//JA:SHADOWBIND'] = {
		},
		-- /NIN abilities
		['GROUP//SJ:NIN//JA:YONIN'] = {
		},
		-- /DRK abilities
		['GROUP//JA:ARCANE_CIRCLE'] = {
		},
		['GROUP//JA:LAST_RESORT'] = {
		},
		['GROUP//JA:WEAPON_BASH'] = {
		},
		['GROUP//JA:SOULEATER'] = {
		},
		-- /BRD - No abilities
		-- /SAM abilities
		['GROUP//JA:WARDING_CIRCLE'] = {
		},
		['GROUP//JA:THIRD_EYE'] = {
		},
		['GROUP//JA:HASSO'] = {
		},
		['GROUP//JA:MEDITATE'] = {
		},
		['GROUP//JA:SEIGAN'] = {
		},
		-- /DRG abilities
		['GROUP//JA:ANCIENT_CIRCLE'] = {
		},
		['GROUP//JA:JUMP'] = {
			-- Damage from jump is calculated: floor((Base Damage)*(VIT/256+1)), so adding VIT will increase damage
		},
		['GROUP//JA:HIGH_JUMP'] = {
		},
		-- /COR abilities
		-- Summoner's can't use guns, quick Draw and dice are not supported
		['GROUP//JA:RANDOM_DEAL'] = {
		},
		-- /BLU abilities
		['GROUP//JA:BURST_AFFINITY'] = {
		},
		-- /PUP abilities
		['GROUP//JA:ACTIVATE'] = {
		},
		['GROUP//JA:DEUX_EX_AUTOMATA'] = {
		},
		['GROUP//JA:REPAIR'] = {
		},
		['GROUP//JA:MAINTENANCE'] = {
		},
		-- /SCH abilities
		['GROUP//JA:LIGHT_ARTS'] = {
		},
		['GROUP//JA:DARK_ARTS'] = {
		},
		['GROUP//JA:STRATAGEMS'] = {
		},
		['GROUP//JA:SUBLIMATION'] = {
		},
		-- /DNC abilities
		['GROUP//JA:SAMBAS'] = {
		},
		['GROUP//JA:WALTZES'] = {
		},
		['GROUP//JA:STEPS'] = {
		},
		['GROUP//JA:FLOURISHES'] = {
		},
		['GROUP//JA:JIGS'] = {
		},
		-- /GEO abilities
		['GROUP//JA:FULL_CIRCLE'] = {
		},
		['GROUP//JA:LASTING_EMANATION'] = {
		},
		['GROUP//JA:ECLIPTIC_ATTRITION'] = {
		},
		-- /RUN abilities
		['GROUP//JA:RUNE_ENCHANTMENT'] = {
		},
		['GROUP//JA:WARD'] = {
		},
		['GROUP//JA:SWORDPLAY'] = {
		},
		['GROUP//JA:EFFUSION'] = {
		},
	},

--[[
	The /THF abilities Sneak Attack and Trick Attack are handled separately from other abilities since
	the gear must remain in place to affect the skill. Separate gear sets are supported for these two
	skills and a combined one when both Sneak Attack and Trick Attack are active at the same time.

--]]

	['JA_Sneak_Attack'] = {
		SUBSET = 'rDEX',
	},

	{'JA_Trick_Attack'} = {
		SUBSET = 'rAGI',
	},

	['JA_SATA'] = {
		Head  = 'Empress Hairpin',							-- +3 DEX/+3 AGI
		Neck  = 'Spike Necklace',							-- +3 DEX
		Ears  = { 'Genin Earring//SJ:NIN', 'Drone Earring' },	-- +4 AGI if sj NIN, +3 AGI
		Rings = { 'Kshama Ring No.2', 'Kshama Ring No.3' },	-- +3 DEX, +3 AGI
		Waist = 'Mrc.Cpt. Belt',							-- +1 DEX/+1 AGI
	},

	-- It's questionable if any gear provides Treasure Hunter that can be equipped by a
	-- /THF, but just in case. Unlike other abilities, TH (barring gear) is based on
	-- traits. Even so, the gear set is named like it is a job ability.
	['JA_TH'] = {
	},

--[[
	Pet commands are handled separately from abilities. While your main job is SMN it is also
	possible that your pet is from your sub job. The Pet_Command gearset identifies every pet
	command (across all jobs) that can have a pet (except for DRG since /DRG cannot summon
	a wyvern). This set is effectively used as a mid command set. (SMN also supports a pre-
	blood pact ability which uses a separate gearset: PC_Pre_BloodPact.)


	SMN Blood Pact is a special case. Like spell casting, the blood pact has a precast and a
	midcast phase. The precast happens when the blood pact is invoked (either rage or ward),
	loading the 'PC_Pre_BloodPact' gear set. You want gear that has Blood Pact Ability Delay,
	Blood Pact Recast abilities, or Summoning Skill defined here.

	Note: Blood Pact Delay has a cap of -15
--]]

	['PC_Pre_BloodPact'] = {
		SUBSET = 'rSummoning_Magic_Skill',
		Head   = { 'Smn. Horn +1', 'Evoker\'s Horn', 'Austere Hat' },			-- BP ability delay -3,
		Body   = { 'Yinyang Robe', 'Summoner\'s Dblt.', 'Austere Robe' },		-- BP ability delay: -5/-3/-3
		Hands  = 'Smn. Bracers +1',												-- BP ability delay -2
		Legs   = 'Summoner\'s Spats',											-- BP ability delay -2
		Feet   = 'Summoner\'s Pgch.',											-- BP ability delay -2
	},

	-- Now, all the different types of pet commands (including the midcast for a blood pact)
	-- are supported in the Pet_Command gear set. Details on what is expected have been included
	-- where appropriate.

	['Pet_Command'] = {
		-- SMN commands: Assault
		['GROUP//PC:ASSAULT'] = {
		},
--[[
		The midcast for Blood pacts are divided by type: physical, magical, summoning
		skill, accuracy, and hybrid. The gear set encapsulates all those types through
		the	use of groups. Summoning Magic Skill when above cap affects the accuracy and
		magic accuracy of the avatar's blood pact. Cap at level 75 is 269.
--]]

		['GROUP//PC:BP:PHYS'] = {
			-- PC:BP:PHYS (Physical)
			--	Pet attack, pet accuracy, pet critical hit, and blood pact physical damage
			SUBSET = 'rSummoning_Magic_Skill',
			Head   = 'Shep. Bonnet',							-- +5 pet accuracy
			Ears   = 'Beastly Earring',							-- +10 pet accuracy
			Body   = 'Summoner\'s Dblt.',						-- +3% pet crit rate
			Hands  = 'Smn. Bracers +1',							-- enhances pet accuracy/+12 summoning skill
			Legs   = 'Evk. Spats +1',							-- enhances pet accuracy
			Feet   = 'Summoner\'s Pgch.',						-- enhances pet attack
		},
		['GROUP//PC:BP:MAG'] = {
			-- PC:BP:MAG (Magical)
			-- 	Pet magic attack burst, pet magical attack, pet magical accuracy, and
			-- 	blood pact magical damage
			SUBSET = 'rSummoning_Magic_Skill',
			['Head']  = { 'Shep. Bonnet', 'Evoker\'s Horn', 'Austere Hat' },	-- +3 pet macc, +5/2 Summoning Skill
			['Neck']  = 'Smn. Torque',							-- +7 Summoning Magic Skill
			['Body//IF:Vermillion Cloak'] = { 'Summoner\'s Dblt.', 'Austere Robe'}, -- Conditional removes the V.Cloak so a head will be equipped
			['Hands'] = 'Smn. Bracers +1',						-- +12 Summoning Skill
			['Rings'] = 'Evoker\'s Ring',						-- +10 Summoning Skill
			['Feet']  = 'Nashira Crackows',						-- +5 Summoning Magic Skill
		},
		['GROUP//PC:BP:SKILL'] = {
			-- PC:BP:SKILL (Skill)
			--	Summoning skill
			SUBSET = 'rSummoning_Magic_Skill',					-- Not necessary to repeat here, but clearer what's happening
		},
		['GROUP//PC:BP:ACC'] = {
			-- PC:BP:ACC (Accuracy)
			--	Pet accuracy, pet magic accuracy
			SUBSET = 'rSummoning_Magic_Skill',
			['Head']  = 'Shep. Bonnet',							-- +5 pet accuracy and +3 pet macc
			['Neck']  = 'Smn. Torque',							-- +7 summoning magic skill
			['Ears']  = 'Beastly Earring',						-- +10 pet accuracy
			['Body//IF:Vermillion Cloak'] = { 'Summoner\'s Dblt.', 'Austere Robe'},		-- Conditional removes the V.Cloak so a head will be equipped
			['Hands'] = 'Smn. Bracers +1',						-- enhances pet accuracy/+12 summoning skill
			['Rings'] = 'Evoker\'s Ring',						-- +10 Summoning Skill
			['Legs']  = 'Evk. Spats +1',						-- enhances pet accuracy
			['Feet']  = 'Nashira Crackows',						-- +5 Summoning Magic Skill
		},
		['GROUP//PC:BP:HYBRID'] = {
			-- PC:BP:HYBRID (Hybrid)
			--	2x physical attacks and 1x magical, see SMN_BP_PHYS and SMN_BP_MAG for details
			SUBSET = 'rSummoning_Magic_Skill',
			Head  = 'Shep. Bonnet',								-- +5 pet accuracy/+3 pet macc
			Neck  = 'Smn. Torque',								-- +7 Summoning Magic Skill
			Ears  = 'Beastly Earring',							-- +10 pet accuracy
			Body  = 'Summoner\'s Dblt.',						-- 3% pet crit rate
			Hands = 'Smn. Bracers +1',							-- enhances pet accuracy/+12 summoning skill
			Legs  = 'Evk. Spats +1',							-- enhances pet accuracy
			Feet  = 'Summoner\'s Pgch.',						-- enhances pet attack
		},
		-- Release
		['GROUP//PC:RELEASE'] = {
		},
		-- Retreat
		['GROUP//PC:RETREAT'] = {
		},
		-- /BST commands: Fight
		['GROUP//PC:FIGHT'] = {
		},
		-- Heel
		['GROUP//PC:HEEL'] = {
		},
		-- Stay
		['GROUP//PC:STAY'] = {
		},
		-- Sic
		['GROUP//PC:SIC'] = {
		},
		-- Ready!
		['GROUP//PC:READY'] = {
		},
		-- Leave
		['GROUP//PC:LEAVE'] = {
		},
		-- /PUP commands: Deploy
		['GROUP//PC:DEPLOY'] = {
		},
		-- Deactivate
		['GROUP//PC:DEACTIVATE'] = {
		},
		-- Retrieve
		['GROUP//PC:RETRIEVE'] = {
		},
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

-- Load gVars to define most globals and the individual modules
gVars = gFunc.LoadFile('common\\gVars.lua');

-- The following structure contains settings that are controlled by the program.
--
-- ################################################
-- # DO NOT MODIFY ANY SETTINGS IN THIS STRUCTURE #
-- ################################################
profile.system_settings = {
	job = 'SMN';						-- Main job, used when NON encountered
	race = 'tarutaru';					-- Character's race
	gender = 'female';					-- Character's sex
	sjb = nil;							-- Tracks subjob name
	sPetAction = nil;					-- What was the last action by your avatar
	PlayerCappedLevel = 0;				-- Indicates gear capped level. 0 defaults to current level
	bAmmo = false;						-- /BST specific. Is ammo equipped?
	sAmmo = nil;						-- /BST specific. Name of ammo equipped
	sLastSGS = 'none';					-- Last gear set displayed
	bGCReminder = false;				-- Has GC reminder been displayed yet?
	bDefault = false;					-- Switch to default gear?
	-- Trackers for the regen and refresh caps
	bCappedRefresh = false;				-- Disables resting refresh gear equip if true
	bCappedRegen = false;				-- Disables resting regen gear equip if true
	WSTypeName = nil;					-- Name of weaponskill based on stats
};

-- The following structure stores job/process settings. All the fields shown can be modified
-- by the player, none of them are system-only fields.
--
-- ###########################################
-- # PLAYER CAN MODIFY SETTINGS IN STRUCTURE #
-- ###########################################
profile.settings = {
	defaultSpirit = 'Light Spirit',		-- for /911, what default spirit should be used
	defaultPetFood = nil;				-- What (if any) pet food to use when Reward processed
	petName = nil;						-- Leave nil for SMN. Defines pet name for DRG and PUP
	-- Order of operations:
	-- After TP gearset processed, three supplimental gearsets might be also run: evasion,
	-- accuracy, and TH. postGSEngaged indicates the order to process the first two. It is a
	-- replacement for priorityEngaged. The TH gearset will always be run last.
	postGSEngaged = { [1] = 'Eva', [2] = 'Acc' };
	-- After the weaponskill gearset is loaded, three supplimental gearsets might also be run:
	-- accuracy, elemental gorget, and elemental obi. (Note: support for closing elemental weapon
	-- skill chain is currently unsupported.)
	postGSWeaponSkill = { [1] = 'Acc', [2] = 'eGorget', [3] = 'eObi' };
	-- Priority settings define process of supplimental orders after gear set processing
	bPriorityRefresh = true;			-- Priority setting. If true, Refresh over Regen. False inverts
	bLockAllOnGS = false;				-- Lock ALL slots when a gear set is equipped. Most useful on craft and gathering sets
	bAutoStaveSwapping = true;			-- Indicates if elemental stave swapping should occur automatically
	bConfirmation = true;				-- Should confirmation be displayed? Note: some actions give feedback regardless of setting
	-- Override settings are used to indicate the order sets are processed. It's recommended to leave these
	-- entries false.
	EmbedOnly = {
		Accuracy = false,				-- Restricts accuracy to only inline conditionals if true
		Evasion = false,				-- Restricts evasion to only inline conditionals if true
		Macc = false,					-- Restricts Macc to only inline conditionals if true
		TH = false,						-- Restricts TH to only inline conditionals if true
		eGorget = false;				-- Restricts elemental gorgets to only inline conditionals if true
		eObi = false;					-- Restricts elemental obis to only inline conditionals if true
	};
	-- The AutoMacroBookPage structure is how you define which macro book and page (if wanted) will
	-- automatically be set. Identify the macro book number in the "book" sub-setting or set it to nil
	-- if you do not want Luashitacast to set it for you. The "page" sub-setting identifies which page
	-- in the macrobook should be assigned. The default is 1, so you only need identify any subjobs whose
	-- page number is greater than 1.
	AutoMacroBookPage = {
		bBook = true,					-- Should the macro book be automatically assigned?
		bPage = true,					-- Should the macro page be automatically assigned?
		-- if "bBook" is true, the following subsettings have meaning
		book = 13,						-- What macro book should be assigned, if set to nil, has same effect as bBook=false
		pages = { 						-- What page should be assingned based on subjob, 1 is default
			['RDM'] = 2, 				-- Identify which page for each subjob should be equipped
			['BLM'] = 3,
		},
	};
	-- LockStyle automatically locks style set's gear upon startup. If set to an equipment set's number, it will equip
	-- and lock that set's style. If nil, then nothing is lock styled on start up.
	LockStyle = 21;
	-- The reminder system is a "nag" system to remind the player to run /gc. Prior to /gc being run, no gear swapping
	-- will occur. /gc is required for Luashitacast to perform gear swapping. You can disable to reminder (by setting
	-- the Enabled attribute to false), but I highly recommend you leave it enabled. Nothing more frustrating then
	-- realizing no gear swapping is occurring because /gc wasn't run.
	Reminder = {
		Enabled = true;					-- Should the reminder be enabled?
		MinBasetime = 15;				-- Minimum wait before reminding player to run /gc
		MaxBasetime = 300;				-- Once reminder shown, switch to every 5 minutes
	};
	-- You can have up to two display bars. Each section definies which bar (either, both, or neither) a field should be
	-- displayed in, and should the field be visible. The two overall entries define the two bars, identified by: gVars._BAR1
	-- and gVars._BAR2. The first three entries affect the whole bar: should it be visible, and the X/Y coordinates of where
	-- the bar should start on your screen. All entries after these fields define what can be displayed on that bar. If you
	-- want to see it, it must be visible (i.e., ['visible'] = true].) You can also find a 'tag' parameter on some fields.
	-- Leave those alone. They are used by the /smg command. Some fields let you define an initial value (e.g., ['init'] = true).
	-- The initialization of these fields have been broken out into their own section: gVars._INITIALIZE. Not all fields support
	-- initialization.
	DisplayBar = {
		[gVars._BAR1] = {
			-- By default, bar 1 is visible. During testing, the prefix is set to 'X:'. This will be set to nil (meaning no
			-- prefix will be displayed) once Boxcar is ready to be deployed. You can set it to anything you want. It's just
			-- what's displayed first on the bar.
			[gVars._VISIBLE] = true,															-- should display bar 1 be visible
			[gVars._POS_X] = 325,																-- x coordinate of display bar 1
			[gVars._POS_Y] = 0,																	-- y coordinate of display bar 1
			---
			[gVars._PREFIX] = 'X:',																-- what to display prior to the fields
			[gVars._JOB] = { ['visible'] = true, ['tag'] = 'job' },								-- job/subjob display. Visible?
			[gVars._CAP] = { ['visible'] = true, ['tag'] = 'cap' },								-- gear level capped. Visible?
			--
			[gVars._GC]  = { ['visible'] = true, ['tag'] = 'gc' },								-- was gear check run. Visible?
			--
			[gVars._WSWAP]   = { ['visible'] = true,  ['tag'] = 'wswap' },						-- weapon swap. Visible?
			[gVars._KITE]    = { ['visible'] = true,  ['tag'] = 'kite' },						-- kiting. Visible?
			[gVars._TH]	     = { ['visible'] = false, ['tag'] = 'th' },							-- treasure hunter. Visible?
			[gVars._TANK]    = { ['visible'] = false, ['tag'] = 'tank' },						-- tanking. Visible?
			[gVars._IDLE]    = { ['visible'] = false, ['tag'] = 'idle' },						-- idle gear. Visible?
			[gVars._EVASION] = { ['visible'] = true,  ['tag'] = 'eva' },						-- evasion. Visible?
			[gVars._SPF]     = { ['visible'] = true,  ['tag'] = 'spf' },						-- show pull. Visible?
			[gVars._SGS]     = { ['visible'] = true,  ['tag'] = 'sgs' },						-- show gear sets. Visible?
			[gVars._GSWAP]   = { ['visible'] = true,  ['tag'] = 'gswap' },						-- gear swapping. Visible?
			-- Magic accuracy is only available to jobs/subjobs that use magic
			[gVars._MACC]    = { ['visible'] = true,  ['tag'] = 'macc' },						-- macc. Visible?
			-- sBP is only available to summoners (SMN/ or /SMN)
			[gVars._SBP]     = { ['visible'] = true,  ['tag'] = 'sbp' },						-- show blood pact. Visible?
			-- Mode is currently only available to summoners (SMN/ or /SMN). Mode let's the player define what
			-- emphasis gearing should have affecting their pet.
			[gVars._MODE]    = { ['visible'] = true,  ['tag'] = 'mode' },						-- mode. Visible?
			[gVars._DT]		 = { ['visible'] = true,  ['tag'] = 'dt' },							-- damage taken. Visible?
			[gVars._REGION]  = { ['tag'] = 'region',  ['visible'] = true },						-- region control. Visible?
			--
			[gVars._ACC]     = { ['visible'] = true,  ['tag'] = 'acc' },						-- accuracy. Visible?
			[gVars._RACC]    = { ['visible'] = true,  ['tag'] = 'racc' },						-- ranged accuracy. Visible?
			--
			[gVars._LOCKS]   = { ['visible'] = true,  ['tag'] = 'locks' },						-- locks. Visible?
			--
			[gVars._DAY]     = { ['visible'] = true,  ['tag'] = 'day' },						-- day. Visible?
			[gVars._TIME]    = { ['visible'] = true,  ['tag'] = 'time' },						-- time. Visible?
			[gVars._MOON]    = { ['visible'] = true,  ['tag'] = 'moon' },						-- moon phase and percent. Visible?
			[gVars._WEATHER] = { ['visible'] = true,  ['tag'] = 'weather' },					-- weather. Visible?
			[gVars._ZONE]    = { ['visible'] = true,  ['tag'] = 'zone' },						-- zone name. Visible?
			[gVars._CC]		 = { ['visible'] = true,  ['tag'] = 'cc' },							-- custom conditionals? Visible?
			},
		[gVars._BAR2] = {
			[gVars._VISIBLE] = false,															-- Should display bar 2 be visible
			[gVars._POS_X] = 325,																-- X coordinate of second display bar
			[gVars._POS_Y] = 40,																-- Y coordinate of second display bar
			---
			[gVars._PREFIX] = 'X2:',															-- what to display prior to the fields
			[gVars._JOB] = { ['visible'] = true, ['tag'] = 'job' },								-- job/subjob display. Visible?
			[gVars._CAP] = { ['visible'] = true, ['tag'] = 'cap' },								-- gear level capped. Visible?
			--
			[gVars._GC]  = { ['visible'] = true, ['tag'] = 'gc' },								-- was gear check run. Visible?
			--            ScanGearDelay                   Populates the gear delay structure
			[gVars._WSWAP]   = { ['visible'] = true,  ['tag'] = 'wswap' },						-- weapon swap. Visible?
			[gVars._KITE]    = { ['visible'] = true,  ['tag'] = 'kite' },						-- kiting. Visible?
			[gVars._TH]	     = { ['visible'] = false, ['tag'] = 'th' },							-- treasure hunter. Visible?
			[gVars._TANK]    = { ['visible'] = false, ['tag'] = 'tank' },						-- tanking. Visible?
			[gVars._IDLE]    = { ['visible'] = false, ['tag'] = 'idle' },						-- idle gear. Visible?
			[gVars._EVASION] = { ['visible'] = true,  ['tag'] = 'eva' },						-- evasion. Visible?
			[gVars._SPF]     = { ['visible'] = true,  ['tag'] = 'spf' },						-- show pull. Visible?
			[gVars._SGS]     = { ['visible'] = true,  ['tag'] = 'sgs' },						-- show gear sets. Visible?
			[gVars._GSWAP]   = { ['visible'] = true,  ['tag'] = 'gswap' },						-- gear swapping. Visible?
			-- Magic accuracy is only available to jobs/subjobs that use magic
			[gVars._MACC]    = { ['visible'] = true,  ['tag'] = 'macc' },						-- macc. Visible?
			-- sBP is only available to summoners (SMN/ or /SMN)
			[gVars._SBP]     = { ['visible'] = true,  ['tag'] = 'sbp' },						-- show blood pact. Visible?
			-- Mode is currently only available to summoners (SMN/ or /SMN). Mode let's the player define what
			-- emphasis gearing should have affecting their pet.
			[gVars._MODE]    = { ['visible'] = true,  ['tag'] = 'mode' },						-- mode. Visible?
			[gVars._DT]		 = { ['visible'] = true,  ['tag'] = 'dt' },							-- damage taken. Visible?
			[gVars._REGION]  = { ['tag'] = 'region',  ['visible'] = true },						-- region control. Visible?
			--
			[gVars._ACC]     = { ['visible'] = true,  ['tag'] = 'acc' },						-- accuracy. Visible?
			[gVars._RACC]    = { ['visible'] = true,  ['tag'] = 'racc' },						-- ranged accuracy. Visible?
			--
			[gVars._LOCKS]   = { ['visible'] = true,  ['tag'] = 'locks' },						-- locks. Visible?
			--
			[gVars._DAY]     = { ['visible'] = true,  ['tag'] = 'day' },						-- day. Visible?
			[gVars._TIME]    = { ['visible'] = true,  ['tag'] = 'time' },						-- time. Visible?
			[gVars._MOON]    = { ['visible'] = true,  ['tag'] = 'moon' },						-- moon phase and percent. Visible?
			[gVars._WEATHER] = { ['visible'] = true,  ['tag'] = 'weather' },					-- weather. Visible?
			[gVars._ZONE]    = { ['visible'] = true,  ['tag'] = 'zone' },						-- zone name. Visible?
			[gVars._CC]		 = { ['visible'] = true,   ['tag'] = 'cc' },						-- custom conditionals? Visible?
		},
		[gVars._INITIALIZE] = {
			-- Certain fields have initial values. Because all fields can be either in bar 1 or bar 2, the initialization
			-- values have been moved to their own breakout section. These values are shared across both display bars.
			[gVars._WSWAP] 		= true,
			[gVars._KITE] 		= false,
			[gVars._TH] 		= false,
			[gVars._TANK] 		= false,
			[gVars._IDLE] 		= true,
			[gVars._EVASION]	= false,
			[gVars._SGS] 		= false,
			[gVars._GSWAP] 		= true,
			-- Magic accuracy is only available to jobs/subjobs that use magic
			[gVars._MACC] 		= false,
			-- sBP enables display of blood pacts, it is only available to summoners (SMN/ or /SMN)
			[gVars._SBP] 		= true,
			-- Mode is currently only available to summoners (SMN/ or /SMN). Mode let's the player define what emphasis gearing should
			-- have, affecting their pet: perpetuation cost (gVars_MODE_PERPETUATION), attack (gVars_MODE_ATTACK), or enmity down
			-- (gVars_MODE_ENMITY_MINUS)
			[gVars._MODE] 		= gVars._MODE_PERPETUATION,
			-- Damage Taken is a cycle available to all jobs. Settings are: off (gVars._DT_OFF), magical (gVars._DT_MAG), breath (gVars._DT_BRE),
			-- or physical (gVars._DT_PHY)
			[gVars._DT] 		= gVars._DT_OFF,
		},
	};
	-- While not implemented yet there are multiple overlays planned for BOXCAR. The following structure
	-- is just an initial guess of what's needed. The entries are not used yet and when it's implemented
	-- they might change, but it's worthwhile to include at least a skeleton structure now.
	Overlays = {
		-- Lock Grid
		lockGrid = {
			[gVars._VISIBLE] = true,			-- Display locks on the equipment grid
			[gVars._POS_X] = 1000,				-- X coordinate of the equipment grid
			[gVars._POS_Y] = 400,				-- Y coordinate of the equipment grid
			[gVars._LOCK_STYLE] = gVars._REDX,	-- WIP. Indicates how to display the locked gear slot
			[gVars._SHOW_DELAY] = false,		-- WIP. Indicates on grid if gear slot has delayed piece
		},
		-- Day Cycle
		dayCycle = {
			[gVars._VISIBLE] = true,			-- Display day cycle list
			[gVars._POS_X] = 1000,				-- X coordinate of the day cycle list
			[gVars._POS_Y] = 400,				-- Y coordinate of the day cycle list
		},
	};
	-- Should distance be checked when performing a weapon skill?
	WScheck = true; 	 				-- set to false if you don't want to use the WSdistance safety check
	WSdistance = 4.7; 	 				-- default max distance (yalms) that a melee weapon skill can reach. 4.7 is the default for Tarutarus
	-- MPP/HPP tolerance percentages
	MPPTolerance = 98;					-- Tolerance used when checking special gear's MP% maximum
	HPPTolerance = 98;					-- Tolerance used when checking special gear's HP% maximum
	-- Default target
	DefaultSpellTarget = 't'; 			-- What is the default target to use in MaxSpell if no target specified
	DefaultSongTarget = 't';  			-- What is the default target to use in MaxSong if no target specified
	};

profile.Sets = sets;

-- Table of custom conditionals
profile.CustomConditionals = {
	[1] = { ['code'] = 'CC1', ['question'] = 'Is minus fire resistance an issue?', ['init'] = false },
	[2] = { ['code'] = 'CC2', ['question'] = 'Should optional gear be included?', ['init'] = false },
};

--[[
	****************
	* Code Section *
	****************
--]]

--[[
	SetSubjobSet sets the appropriate macro page (if wanted) from the current
	macro book.

	Parameter
		chkSJ		player's subjob
--]]

function SetSubjobSet(chkSJ)
	-- Make sure a macro page is wanted or determinable
	if chkSJ == nil or
		chkSJ == 'NON' or
		profile.settings.AutoMacroBookPage.bBook == false or
		profile.settings.AutoMacroBookPage.book == nil or
		profile.settings.AutoMacroBookPage.bPage == false or
		(profile.system_settings.sjb ~= nil and profile.system_settings.sjb == chkSJ) then
		return;
	end

	-- At this point the macro page setting is wanted
	local sj = profile.settings.AutoMacroBookPage.pages[chkSJ];

	if sj == nil then
		sj = 1;
	end

	-- Set the macro set
	AshitaCore:GetChatManager():QueueCommand(1, '/macro set '..tostring(sj));
	profile.system_settings.sjb = chkSJ;
	displaybar.UpdateBarStatic();
	crossjobs.SetVariables(false);

	-- Lastly, if indicated, lockstyle either the current gear or a specific set
	if profile.settings.LockStyle ~= nil then
		local s = '/lockstyleset ' .. tostring(profile.settings.LockStyle);
		AshitaCore:GetChatManager():QueueCommand(1, s);
		if profile.settings.bConfirmation == true then
			print(chat.message('Info: ' .. s .. ' completed'));
		end
	end
end		-- SetSubjobSet

--[[
	OnLoad is run whenever you log into your SMN or change your job to SMN
--]]

function profile.OnLoad()
	local player = utilities.SetJob();

	-- Initialize settings
	gSettings.AllowAddSet = true;
	utilities.Initialize();
	
	-- Set which macro book should be displayed. Which macro page within the macro book to
	-- display depends on what your subjob is.
	if profile.settings.AutoMacroBookPage.bBook == nil then
		profile.settings.AutoMacroBookPage.bBook = false;
	end

	if profile.settings.AutoMacroBookPage.bBook == true and
		profile.settings.AutoMacroBookPage.book == nil then
		print(chat.message('Info: AutoMacroBookPage.bBook was true, but AutoMacroBookPage.book is undefined. Turning off function'));
	elseif profile.settings.AutoMacroBookPage.bBook == true and
		profile.settings.AutoMacroBookPage.book ~= nil then
		AshitaCore:GetChatManager():QueueCommand(1, '/macro book ' .. tostring(profile.settings.AutoMacroBookPage.book));		-- SMN macro book
		SetSubjobSet(player.SubJob);
	end

	-- Load up the weapons bar.
	gear.MoveToDynamicGS(profile.Sets.Start_Weapons,crossjobs.Sets.CurrentGear,true,'Start_Weapons');
	gear.EquipTheGear(crossjobs.Sets.CurrentGear,false,false);

	-- Now define the toggles for any custom conditionals.
	for _,j in ipairs(profile.CustomConditionals) do
		utilities.SetToggle(string.upper(j['code']),j['init']);
	end

	-- Make sure the saved weapons are the starting weapons
	gVars.weapon = crossjobs.Sets.CurrentGear['Main'];
	gVars.offhand = crossjobs.sets.CurrentGear['Sub'];
end		-- OnLoad

--[[
	OnUnload is run when you change to another job
--]]

function profile.OnUnload()
	crossjobs.Unload();
end		-- OnUnload

--[[
	HandleCommand is run when you type in a command defined in LUASHITACAST.
--]]

function profile.HandleCommand(args)
	crossjobs.HandleCommands(args);
end

--[[
	HandlePetAction prints the blood pact being invoked (if a SMN pet and
	indicated) and then invoked the general HandkePetAction routine found
	in gear.

	Parameter
		PetAction	What action has your pet done
--]]

function HandlePetAction(PetAction)
	local sType;
	local sMsg,sMsg2;

	if PetAction == nil or PetAction.Name == nil then
		return;
	end

	if pets.fSummonerPet() == true then
		-- Since the pet is a smn avatar, give feedback on the blood pact.
		-- If the action is a BP: rage, print out what happened in party chat
		if table.find(pets.SmnBPRageList,PetAction.Name) ~= nil then
			sType = gVars._RAGE;
		elseif table.find(pets.SmnBPWardList,PetAction.Name) ~= nil then
			sType = gVars._WARD;
		else
			sType = gVars._UNKNOWN;
		end

		if (profile.system_settings.sPetAction == nil or profile.system_settings.sPetAction ~= PetAction.Name) and
		   utilities.fGetToggle('sBP') == true then
			if sType == gVars._Rage or sType == gVars._WARD then
				sMsg = '/p [<pet>] Blood Pact[' .. sType .. ']: ' .. PetAction.Name .. ' >> <t>.';
				AshitaCore:GetChatManager():QueueCommand(-1, sMsg);
			end
			sMsg2 = '/echo [<pet>] Blood Pact[' .. sType .. ']: ' .. PetAction.Name
			AshitaCore:GetChatManager():QueueCommand(-1, sMsg2);
			profile.system_settings.sPetAction = PetAction.Name;
		end
		--
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
	local player = utilities.SetJob();
	local zone = gData.GetEnvironment();
	local ew = gData.GetEquipment();
	local bSA = utilities.fBuffed('Sneak Attack');
	local bTA = utilities.fBuffed('Trick Attack');
	local eWeap = nil;
	local bIgnoreLocks = false;
	local bOverride = false;
	local cKey,sGear;

	utilities.Reminder();		-- See if reminder should be printed

	-- Make sure the macro set is shown and that the display on the top of the screen is correct
	-- in case the subjob was changed. Note: the macro page will only be changed if there was a
	-- subjob change.
	if profile.settings.AutoMacroBookPage.bPage == true then
		SetSubjobSet(player.SubJob);
	end

	-- No gear swapping should occure if GSwap is false or /gc has not been run
	if utilities.fGetToggle('GSwap') == false or gear.fHasGCBeenRun() == false then
		return;
	end

	-- A pet action takes priority over a player's action
	if pet ~= nil and pet.Name ~= nil and petAction ~= nil then
		HandlePetAction(petAction);
		return;
	end

	profile.system_settings.sPetAction = nil;

	-- Save the name of the main weapon
	if ew['Main'] ~= nil then
		eWeap = ew['Main'].Name;
	end

	-- Assuming you're /bst, when you want to reward your pet and you do not have pet food 
	-- equipped, the current item in the ammo slot is saved. The following will set it back
	-- to what you had before unless the slot is locked.
	if player.SubJob == 'BST' and 
	   profile.system_settings.bAmmo == true and
	   locks.fIsSlotLocked('ammo') == false then
		gFunc.ForceEquip('Ammo',profile.system_settings.sAmmo);
		profile.system_settings.sAmmo = nil;
		profile.system_settings.bAmmo = false;
	end

	-- Clear out the CurrentGear in case of leftovers
	utilities.ClearSet(crossjobs.Sets.CurrentGear);

	-- Now process the pet/player statuses accordingly.
	if (pet ~= nil and pet.Status == 'Engaged') or (player.Status == 'Engaged') then
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
				if j == 'eva' and utilities.fGetToggle('Eva') == true and profile.settings.EmbedOnly.Evasion == false then
					gear.MoveToDynamicGS(profile.Sets.Evasion,crossjobs.Sets.CurrentGear,false,'Evasion');
				elseif j == 'acc' and profile.settings.EmbedOnly.Accuracy == false then
					crossjobs.ProgressiveAccuracy('Acc');
				end
			end
		end
		-- TH (if enabled) is always loaded last
		if utilities.fGetToggle('TH') == true and profile.settings.EmbedOnly.TH == false then
			gear.MoveToDynamicGS(profile.Sets.JA_TH,crossjobs.Sets.CurrentGear,false,'TH');
		end
	elseif player.Status == 'Resting' and profile.system_settings.bDefault == false then
		local bRefresh = false;
		if profile.system_settings.bCappedRefresh == false and player.MP >= player.MaxMP then
			profile.system_settings.bCappedRefresh = true;
		end
		if profile.system_settings.bCappedRegen == false and player.HP >= player.MaxHP then
			profile.system_settings.bCappedRegen = true;
		end

		-- Player kneeling. Based on priority, order regen and refresh
		if profile.settings.bPriorityRefresh == true then
			if profile.system_settings.bCappedRefresh == false then
				gear.MoveToDynamicGS(profile.Sets.Resting_Refresh,crossjobs.Sets.CurrentGear,false,'Resting_Refresh');
				bRefresh = true;
			elseif profile.system_settings.bCappedRegen == false then
				gear.MoveToDynamicGS(profile.Sets.Resting_Regen,crossjobs.Sets.CurrentGear,false,'Resting_Regen');
			else
				profile.system_settings.bDefault = true;	-- Indicates to switch to default gear
			end
		else
			if profile.system_settings.bCappedRegen == false then
				gear.MoveToDynamicGS(profile.Sets.Resting_Regen,crossjobs.Sets.CurrentGear,false,'Resting_Regen');
			elseif profile.system_settings.bCappedRefresh == false then
				gear.MoveToDynamicGS(profile.Sets.Resting_Refresh,crossjobs.Sets.CurrentGear,false,'Resting_Refresh');
				bRefresh = true;
			else
				profile.system_settings.bDefault = true;	-- Indicates to switch to default gear
			end
		end

		-- Add a dark/pluto's staff if refresh wanted
		if bRefresh == true then
			local sStave = gear.fCheckForEleGear('staff','dark');
			if sStave ~= nil then
				gear.fSwapToStave(sStave,false,crossjobs.Sets.CurrentGear);
			end
		end
	else	-- idle or (resting and refresh/regen caps hit)
		if player.Status == 'Idle' then
			-- Once entering because 'idle' as opposed to refresh and regen being
			-- capped, flip settings because player no long resting
			profile.system_settings.bCappedRefresh = false;
			profile.system_settings.bCappedRegen = false;
			-- reset setting now that 'Resting' is over
			profile.system_settings.bDefault = false;
		end

		gear.MoveToDynamicGS(profile.Sets.Default,crossjobs.Sets.CurrentGear,false,'Default');
	end
		
	-- Make sure to equip the appropriate elemental staff if you have a smn pet out
	if pets.fSummonerPet() == true then
		local sStave = gear.fCheckForElementalGearByValue('staff','Summons',pet.Name);
		if sStave ~= nil then
			gear.fSwapToStave(sStave,false,crossjobs.Sets.CurrentGear);
		end
	end
	
	-- Equip the composited HandleDefault set
	gear.EquipTheGear(crossjobs.Sets.CurrentGear,bIgnoreLocks,bOverride);

	-- And make sure a weapon equipped.
	local tgear = gData.GetEquipment();
	if tgear.Main == nil or tgear.Main.Name == nil then
		gear.MoveToDynamicGS(profile.Sets.Start_Weapons,crossjobs.Sets.CurrentGear,true,'Start_Weapons');
	end
end		-- HandleDefault

--[[
	HandleAbility is used to change the player's gear appropriately for the specified avatar ability.
--]]

function profile.HandleAbility()
	local ability = gData.GetAction();
	local player = utilities.SetJob();
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
end		-- profile.HandleMidcast

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
