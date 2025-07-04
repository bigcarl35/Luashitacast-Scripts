local profile = {};
gcinclude = gFunc.LoadFile('common\\gcinclude.lua');

--[[
	This file contains all the gear sets associated with the SMN job.
	
	Gear Sets last updated: June 28, 2025
	Code update: April 23, 2025
	
	Role: any level
--]]

local sets = {
--[[
	The gear sets are self contained, a mixture of direct gear assignments and conditional
	assignments. Each set contains entries identified by the gear slot. If it's a single
	value, it's a direct assignment like: Body = 'Austere Robe', but there can be multiple
	items identified, usually ordered by level: Body = { 'Vermillion Cloak//CARBY','Austere Robe' },
	Any item that has a // appended to it contains an inline conditional. The // code is a test
	to see if the item should be equipped. The level is still checked, but if the inline coded
	test is successful, that piece of gear will be equipped. If you've done a /gc command,
	the item's suitability for the job and accessibility will also be checked.

	Not all sets need to be defined. There is nothing wrong with leaving a set "empty", but 
	don't delete any of the sets. All the ones listed here (except for any custom sets) are 
	expected to exist by Luashitacast. 
	
	*** Note ***
	Unlike when summoner is used as a subjob, /bst's pets are charmed at the max level of your
	BST. That means you can charm higher level mobs than you would expect with /bst. Just note 
	though that you can't have two pets, so if you have charmed a pet with /bst, you can't 
	summon your avatar and visa versa.
	
	*** Note 2 ***
	No gear that supports bard songs can be worn by any job except a bard, so there's no explicit
	support given here for /BRD.
	
	Horizon changes from retail:
		- Some AF have been moderately changed
		- Significant changes have been made to the Claustrum
		- Whispering Wind has erase effect added
		- Spring Water gives AoE 2 mp/tic refresh
		- Crimson Howl lasts 3 minutes
		- Ecliptic Growl and Ecliptic Howl have been swapped, level 54 and 43 now
		
		- Not noted, but top level elemental blood pact rage skills no longer can skillchain
--]]

--[[
	The "default" gear set is what is worn when you're not fighting (either you or your pet)
	and you're not resting. It covers everything else: idling, traveling, in town, etc. This 
	set displays what your character looks like most of the	time. This set does not 
	distinguish the type of activities you're doing by default, so use inlines accordingly.
	There's a separate default set for when you have a pet.
--]]
	
	['Default'] = {
		Main  = 'Earth Staff//NO_PET',
		Head  = { 'Lilac Corsage//TOWN', 'Smn. Horn +1', 'Austere Hat', 'Silver Hairpin +1' },
		Neck  = { 'Rep.Gold Medal//NOT_OWN','Uggalepih Pendant//NIGHTTIME', 'Fenrir\'s Torque//DAYTIME', 'Star Necklace', 'Spirit Torque', 'Justice Badge' },
		Ears  = { 'Bat Earring//BLINDED', 'Loquac. Earring', 'Coral Earring//DT_MAGICAL', 'Bat Earring', 'Energy Earring +1', 'Energy Earring +1' },
		Body  = { 'Ducal Aketon//TOWN-AK', 'Yinyang Robe//MPP.LT.94','Vermillion Cloak//MPP.LT.94', 'Summoner\'s Dblt.', 'Austere Robe', 'Seer\'s Tunic', 'Angler\'s Tunica' },
		Hands = { 'Smn. Bracers +1', 'Errant Cuffs', 'Carbuncle Mitts' },
		Rings = { 'Evoker\'s Ring', 'Tamas Ring', 'Ether Ring', 'Astral Ring', 'Astral Ring' },
		Back  = { 'Blue Cape', 'White Cape' },
        Waist = { 'Hierarch Belt', 'Powerful Rope', 'Friar\'s Rope' },
        Legs  = { 'Evk. Spats +1', 'Summoner\'s Spats', 'Shep. Hose', 'Fisherman\'s Hose' },
        Feet  = { 'Summoner\'s Pgch.', 'Mannequin Pumps', 'Waders' },
		Ammo  = { 'Hedgehog Bomb', 'Fortune Egg' },
	},

	['Default_WPet'] = {
		Subset = { 
			[1] = 'Default' 
		},
		Head   = { 'Smn. Horn +1//SMNPETMW' },		-- Avatar perpetuation cost -3 if pet's element matches weather, works w/storm spells
		Hands  = 'Carbuncle Mitts//CARBY' ,			-- Halves perpetuation cost rounded down if pet is Carbuncle, applies before all other traits or gear
		Body   = { 'Yinyang Robe//MPP.LT.94', 'Summoner\'s Dblt.//SMNPETMD', 'Vermillion Cloak//CARBY' },	-- Smn Dbl: Avatar perpetuation cost -3 if pet's element matches day's element
		Legs   = { 'Summoner\'s Spats//SPIRIT:EP', 'Shep. Hose' },		-- Smn Spats: Shortens elemental spirit's casting delay
		Feet   = 'Evk. Pigaches +1',				-- Avatar's perpetuation cost -1, enhanced evasion
	},
	
--[[
	Unlike most jobs, summoner's emphasis is fighting with your avatar. The ['TP'] set is combined
	for when either you or your pet is fighting. Make sure to use appropriate inline conditionals to 
	emphasize gear accordingly. ['Accuracy'] and ['Evasion'] also represent both pets and players.
--]]

	['TP'] = {
		Subset = {
			[1] = 'Default'
		},
        Head  = { 'Smn. Horn +1//SMNPETMW', 'Shep. Bonnet//PETF' },		-- Smn horn: Avatar perpetuation cost -3 if pet's element matches weather, works w/storm spells
		Ears  = { 'Bat Earring//BLINDED//PETNF', 'Loquac. Earring', 'Beastly Earring//PETF', 'Coral Earring//DT_MAGICAL', 'Bat Earring', 'Energy Earring +1', 'Energy Earring +1' },
        Body  = { 'Vermillion Cloak//CARBY','Summoner\'s Dblt.//SMNPETMD' },	-- Smn Dbl: Avatar perpetuation cost -3 if pet's element matches day's element
        Hands = { 'Carbuncle Mitts//CARBY' },		-- Halves perpetuation cost rounded down if pet is Carbuncle, applies before all other traits or gear effects
        Legs  = 'Evk. Spats +1',					-- Avatar: Enh Acc
        Feet  = { 'Summoner\'s Pgch.', 'Evk. Pigaches +1', 'Mannequin Pumps', 'Waders' },
	},

--[[
	If an accuracy emphasis is desired, the following set will replace the gear, appropriately.
	Unlike TP though, accuracy is applied one slot at a time in a fractionalized manner using
	the /acc command.
	
	Include equipment with accuracy bonus and DEX. Remember, DEX converts to accuracy: (horizon) 
	for every 1 point of DEX you get 0.70 points of accuracy if wielding a 2H weapon, 0.65 for 
	a 1H weapon, and 0.60 for H2H. 

    Unlike most gear sets, Accuracy and Ranged Accuracy are not invoked directly. They are 
	reference sets that are used in the Progressive Accuracy sets defined below. List 
	all your gear that is related to accuracy and ranged accuracy in these sets, so you 
	can refer to them in the Progressive set defined further down.
--]]
		
	['Accuracy'] = {
        Head  = { 'Shep. Bonnet//PETF', 'Optical Hat', 'Empress Hairpin' },	-- Pet: +5 Acc/+3Macc, +10 Acc, +3 DEX
		Neck  = { 'Peacock Amulet',	'Spike Necklace' },		-- +10 Acc, +3 DEX
		Ears  = 'Beastly Earring//PETF',	-- Pet: +10 Acc
		Body  = { 'Black Cotehardie', 'Mrc.Cpt. Doublet' },			-- +2/1 DEX
		Hands = 'Battle Gloves',			-- +3 Acc
		Rings = { 'Toreador\'s Ring', 'Toreador\'s Ring', 'Woodsman Ring', 'Woodsman Ring', 'Jaeger Ring', 'Kshama Ring No.2' },	-- +7/+7/+5/+5/4/2 Acc
		Waist = { 'Life Belt', 'Tilt Belt', 'Mrc.Cpt. Belt' },	-- +10/5 Acc, +1 DEX
		Legs  = 'Evk. Spats +1//PETF',	-- Pet: enhanced acc
    },

--[[
	Similar to the accuracy set, the Ranged_Accuracy set will be used on ranged attacks
--]]

	['Ranged_Accuracy'] = {
		Head  = 'Optical Hat',		-- +10 RAcc
		Neck  = 'Peacock Amulet',	-- +10 RAcc
		Rings = { 'Woodsman Ring', 'Woodsman Ring', 'Jaeger Ring' },	-- +5/5/4 RAcc
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
				['Neck'] = 'Accuracy::Neck',
				['Legs'] = 'Accuracy::Legs'
			},
			[2] = {
				['Rings'] = 'Accuracy::Rings',
				['Ears'] = 'Accuracy::Ears'
			},
			[3] = {	
				['Subset'] = { 
					[1] = 'Accuracy'
				}
			}
		},
		['Ranged_Accuracy'] = {
			[1] = {
				['Head'] = 'Ranged_Accuracy::Head',
				['Neck'] = 'Peacock Amulet'
			},
			[2] = { 
				['Subset'] = { 
					[1] = 'Ranged_Accuracy' 
				}
			}
		}				
  },
  
--[[
	If evasion wanted, equip evasion gear. Remember that AGI converts to evasion: for 
	every 2 points of AGI you get 1 point of evasion. Note that if you leave the body
	slot empty, but designate a piece of head gear and the previous body slot had a
	multi-slot body piece (like Vermillion Cloak), then body slot will be left empty.	
--]]
	
	['Evasion'] = {
		Main  = 'Wind Staff',		-- +10 Eva
		Ammo  = 'Orphic Egg//PJPBRD',					-- +1 Eva
		Head  = { 'Optical Hat', 'Empress Hairpin' },	-- +10/10 Eva
		Neck  = 'Spirit Torque',	-- +5 Eva
		Ears  = { 'Bat Earring//BLINDED', 'Ethereal Earring', 'Genin Earring//SJNIN', 'Drone Earring' },	-- +15 Eva while blinded, +5 Eva, +4/3 AGI
		Body  = 'Summoner\'s Dblt.//IF:Vermillion Cloak',	-- Filler, V.Cloak has -10 eva
		Waist = 'Swift Belt//IF:Tilt Belt', -- Filler, Tilt belt has -5 eva
		Hands = 'Battle Gloves',	-- +3 Eva
		Legs  = 'Shep. Hose//PET',	-- Pet: +3 Eva
    },

--[[
	The damage taken sets are not equipped directly but rather from subsets. They're a
	way to reduce a specific types of damage. As such they're optional and up to the 
	player to decide if they should be defined and how they're used.
--]]

	['Damage_Taken_Breath'] = {
	},
	
	['Damage_Taken_Physical'] = {
	},
	
	['Damage_Taken_Magical'] = {
		Ears = 'Coral Earring',		-- -1% damage reduction from magic
	},
	
--[[
	When you are resting (kneeling down), if your HP is not full, your HP 'Resting' 
	set will be equipped. If your MP is below maximum value, your MP 'Resting_Refresh' 
	gear set will be equipped.
	
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
        Waist = 'Hierarch Belt',		-- +2 HP/tick while resting
    },
	
	['Resting_Refresh'] = {
		Subset = {
			[1] = { 
				'Damage_Taken_Breath//DT_BREATH',
				'Damage_Taken_Magical//DT_MAGICAL',
				'Damage_Taken_Physical//DT_PHYSICAL',
			}
		},	
		Main  = { 'Pluto\'s Staff', 'Kukulcan\'s Staff', 'Pilgrim\'s Wand' },
        Body  = { 'Errant Hpl.', 'Yinyang Robe', 'Vermillion Cloak', 'Seer\'s Tunic' },
		Waist = 'Hierarch Belt',		-- +2 MP/tick while resting
		Legs = 'Baron\'s Slops',		-- +1 MP/tick while resting
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
	
	-- Blood pacts go through a simulated process that mimics spell casting. The precast
	-- happens when the blood pact is invoked (either rage or ward), loading the 'BP'
	-- gear set. You want gear that has Blood Pact Ability Delay, Blood Pact Recast
	-- abilities, or summoning skill defined here. The midcast happens when the actual 
	-- blood pact goes off.	
	
	-- Note: This is a simulated 'Precast' for Blood Pacts

	['BP'] = {
		Subset = { 
			[1] = 'Summoning_Skill' 
		},
        Head   = { 'Smn. Horn +1', 'Austere Hat' },		-- BP ability delay -3
        Body   = { 'Yinyang Robe', 'Summoner\'s Dblt.', 'Austere Robe' },		-- Smn Dbl: BP ability delay -3
        Hands  = 'Smn. Bracers +1',			-- BP ability delay -2
		Legs   = 'Summoner\'s Spats',		-- BP ability delay -2
		Feet   = 'Summoner\'s Pgch.',		-- BP ability delay -2
    },
	
--[[
	Blood pacts are divided by type: physical, magical, summoning skill, accuracy, 
	and hybrid. Each blood pact though is of a fixed type and can be looked up. The 
	following gear sets named SmnXXX where XXX is the type define the gear to be 
	equipped when the blood pact goes off. Look to the specific gear set type for 
	what gear stats are wanted.
	
	Note: These sets simulate the 'Midcast' for Blood Pacts. In gcinclude.lua, there's
	a definition for each of these type of blood pacts, explicitly definining what
	blood pact goes into each type. 

	-- Note: The following are simulated 'Midcasts' for the Blood Pacts
--]]
	
	-- Physical based blood pact: pet attack, pet accuracy, pet critical hit, blood pact 
	-- physical damage

	['SmnPhysical'] = {
		Subset = { 
			[1] = 'Summoning_Skill' 
		},	
        Head  = 'Shep. Bonnet',			-- Pet: +5 Acc
		Ears  = 'Beastly Earring',		-- Pet: +10 Acc
		Body  = 'Summoner\'s Dblt.',	-- Avatar: +3% Crit Rate
		Legs  = 'Evk. Spats +1',		-- Avatar: Enhances Acc
		Feet  = 'Summoner\'s Pgch.',	-- Avatar: Enhances Att
    },

	-- Magical based blood pact: pet magic attack burst, pet magical attack, pet magical
	-- accuracy, and blood Pact magical damage

	['SmnMagical'] = {
		Subset = { 
			[1] = 'Summoning_Skill' 
		},	
	    Head = 'Shep. Bonnet',			-- Pet: +3 Macc
		Body = 'Summoner\'s Dblt.',		-- Avatar: +3% Crit Rate
    },

	-- Summoning skill based blood pact. 

	['SmnSkill'] = {
		Subset = { 
			[1] = 'Summoning_Skill' 
		},
		Hands  = 'Carbuncle\'s Cuffs//BP:Shining Ruby',		-- If BP is Shining Ruby, adds regen to buff (+5 HP/tic)
    },
	
	-- Pet Accuracy based blood pact: pet accuracy, pet magic accuracy

    ['SmnAccuracy'] = {
		Subset = { 
			[1] = 'Summoning_Skill' 
		},	
        Head  = 'Shep. Bonnet',		-- Pet: +5 Acc/+3 Macc
		Ears  = 'Beastly Earring',	-- Pet: +10 Acc
		Hands = 'Smn. Bracers +1',	-- Avatar: Enhances Acc
		Legs  = 'Evk. Spats +1',	-- Avatar: Enhances Acc
    },
	
	-- Hybrid blood pact: 2x physical and 1x magical

    ['SmnHybrid'] = {
		Subset = { 
			[1] = 'Summoning_Skill' 
		},	
        Head  = 'Shep. Bonnet',			-- Pet: +5 Acc/+3 Macc
		Ears  = 'Beastly Earring',		-- Pet: +10 Acc
		Body  = 'Summoner\'s Dblt.',	-- Avatar: 3% Crit Rating	
		Legs  = 'Evk. Spats +1',		-- Avatar: Enhances Acc
		Feet  = 'Summoner\'s Pgch.',	-- Avatar: Enhances Att
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
	
	The following sets are to be used as subsets. Once you get to individual 
	sets, include one of these or ignore them and be explicit on the gear in 
	that set.
--]]

	['INT'] = {
		Main  = 'Aquilo\'s Staff//WSWAP',				-- +5 INT
		Head  = { 'Smn. Horn +1', 'Evoker\'s Horn' },	-- +4/3 INT
		Body  = { 'Errant Hpl.', 'Black Cotehardie', 'Baron\'s Saio' },	-- +10/2/1 INT
		Hands = 'Errant Cuffs',							-- +7 INT
		Rings = { 'Tamas Ring', 'Flame Ring' },			-- +5/2 INT
		Waist = { 'Penitent\'s Rope', 'Mrc.Cpt. Belt' },	-- +5/1 INT
		Legs  = 'Errant Slops',							-- +7 INT
		Feet  = { 'Rostrum Pumps', 'Mannequin Pumps' },	-- +3/1 INT
	},
	
	['MND'] = {
		Main  = { 'Water Staff//WSWAP', 'Pluto\'s Staff//WSWAP', 'Light Staff//WSWAP' },	-- +4/2/1 MND
		Neck  = { 'Promise Badge', 'Justice Badge' },		-- +5/3 MND
		Ears  = 'Geist Earring',		-- +1 MND
		Body  = { 'Errant Hpl.', 'Evoker\'s Doublet', 'Wonder Kaftan', 'Baron\'s Saio' },	-- +10/3/1/1 MND
		Hands = 'Baron\'s Cuffs',		-- +1 MND
		Rings = { 'Tamas Ring', 'Kshama Ring No.9', 'Tranquility Ring' },	-- +5/3/2 MND
		Back  = 'White Cape',			-- +2 MND
		Waist = { 'Penitent\'s Rope', 'Mrc.Cpt. Belt', 'Friar\'s Rope' },	-- +5/1/1 MND
		Legs  = { 'Errant Slops', 'Summoner\'s Spats', 'Wonder Braccae' },	-- +7/3/2 MND
		Feet  = { 'Rostrum Pumps', 'Mannequin Pumps', 'Seer\'s Pumps' }, 	-- +3/2/1 MND		
	},

	['Enmity_Plus'] = {
	},
	
	['Enmity_Minus'] = {
		Ammo  = 'Hedgehog Bomb',	-- -1 Enmity
		-- Head  = 'Evoker\'s Horn//PET',		-- -3 Pet Enmity, not sure I want this
		Neck  = 'Fenrir\'s Torque//NIGHTTIME',		-- -3 Emnity at night
		-- Body = 'Evoker\'s Doublet//PET,		-- -2 Pet Enmity, not sure I want this
		Hands = 'Errant Cuffs',					-- -2 Enmity, 'Evoker\'s Bracers//PET',	-- -2 Pet Enmity, not sure I want this
		Rings = 'Tamas Ring',					-- -5 Enmity
		Waist = 'Penitent\'s Rope',				-- -3 Enmity
		Legs  = { 'Evk. Spats +1', 'Errant Slops' },	-- -3/-3 Enmity
		-- Feet  = 'Evk. Pigaches +1',			-- -4 Pet Enmity, not sure I want this
	},
	
--[[
	Spells are a bit different. Each type of spell can have it's own enhancement gear 
	as well as stat based gear. (In some cases individual spells have special entries.) 
	These sets do not include elemental gear which is dependent on 
	day/weather/weapon skill.

	The first stage is Precast. This is where you place any Fast Cast, cast time reduction,
	and quick cast gear 
--]]

	['Precast'] = {	
		Ears = 'Loquac. Earring',	-- Enhances Fastcast
		Feet = 'Rostrum Pumps',		-- Enhances Fastcast
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
		Subset = { 
				[1] = {
				   'Dark_Magic_Skill//DARK', 
				   'Elemental_Magic_Skill//ELEMENTAL',
				   'Enfeebling_Magic_Skill//ENFEEBLING',
				   'Healing_Magic_Skill//HEALING',		-- Offensive healing only
				   'Divine_Magic_Skill//DIVINE',
				   'Ninjutsu_Skill//NINJUTSU',
				   'CHR//SINGING',		-- Charisma provides accuracy w/singing
					}
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
	main job supports tanking (SMN does not.) I've included details on
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
	Healing Magic: consisting of all light-based spells, can remove
	some debuffs on players, buffs the caster, cures the health of 
	players or npcs, or cause damage to undead monsters. Healing 
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
		Subset = { 
			[1] = 'Healing_Magic_Skill',
			[2] = 'MND' 
			},
		Ammo   = 'Enmity_Minus::Ammo',
		Hands  = 'Evoker\'s Bracers',	-- +4 VIT
		Legs   = 'Shep. Hose//EMPTY',	-- +2 VIT
		Feet   = 'Creek F Clomps//IF:Seer\'s Pumps',	-- +4 VIT
	},	
	
--[[
	This set is used for the offensive use of cure spells against undead 
	monsters, most of what was said about CuringMagic is true here too
	except ffor cure potency. This has no effect on undead monsters.
	You might also consider adding magic accuracy gear (Macc) and gear
	with magic attack bonus (MAB) here too.
	
	After the OffensiveCuring set is equipped, the midcast routine will 
	see if a Korin Obi can be equipped to take advantage of the 100% 
	proc rate of the day's element/weather. Also, like normal curing 
	magic, an Apollo/Light staff will be check for,	but not for the 
	cure potency. Rather, for magic affinity.
--]]

	['OffensiveCuring'] = {
		Subset = { 
			[1] = 'CuringMagic' 
		},
		Neck   = 'Uggalepih Pendant//SPECIAL',		-- +8 MAB if MP < 51%
	},

--[[
	This set is used for all non-cure Healing Magic spells. Only 
	healing magic skill is of any importance here. You might want 
	to use this set as a subset for the other cure-based sets.
--]]

	['HealingMagic'] = {
		Subset = { 
			[1] = 'Healing_Magic_Skill' 
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

	['Enhancing_Magic_Skill'] = {
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
		Subset = { 
			[1] = 'Enhancing_Magic_Skill' 
		},
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
			[1] = 'Enhancing_Magic_Skill' 
		},
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
		Subset = { 
			[1] = 'INT' 
		},
		Neck  = 'Uggalepih Pendant//SPECIAL',		-- +8 MAB if MP < 51%
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
		Subset = { 
			[1] = 'MND' 
		},	
	},	

--[[
	Sneak's duration is variable, but the duration maxes at about 5 
	minutes. Include any gear that enhances this buff.
--]]

	['Sneak'] = {
		Feet = 'Dream Boots +1',	-- Extends duration of Sneak
	},

--[[
	Invisible's duration is variable, but the duration maxes at 
	about 5 minutes. Include any gear that enhances this buff.
--]]	

	['Invisible'] = {
		Hands = 'Dream Mittens +1',	-- Extends duration of Invisible
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
			[1] = 'Enhancing_Magic_Skill' 
		},
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
			[1] = 'Enhancing_Magic_Skill' 
		},
	},

--[[
	****************************
	* Midcast: Elemental Magic *
	****************************
--]]

	['Elemental_Magic_Skill'] = {
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
		Subset = { 
			[1] = 'INT' 
		},
		Neck   = 'Uggalepih Pendant//SPECIAL',		-- +8 MAB if MP < 51%
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
			[1] = 'INT' 
		},
	},

--[[
	**********************
	* Midcast: Summoning *
	**********************
--]]

	['Summoning_Skill'] = {
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
		Subset = { 
			[1] = 'Summoning_Skill'
		},
		Hands = 'Carbuncle\'s Cuffs',				-- Summoning magic casting time -1
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
	
	Dark magic skill has no effect on absorb spells, but do affect accuracy.
	Absorb spells resisted will have their duration cut in half or be completely
	resisted. Equipment that "Enhances" absorb spells will increase the spells
	duration. Equipment that "Augments" absorb spells will increase the spells
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
		Subset = { 
			[1] = 'Dark_Magic_Skill'
		},
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
			[1] = 'Dark_Magic_Skill' 
		},
	},

--[[
	This last gear set, DarkMagic, covers all Dark Magic spells not covered
	by the previous three gear sets. 
--]]

	['DarkMagic'] = {
		Subset = { 
			[1] = 'Dark_Magic_Skill' 
		},	
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
		Subset = { 
			[1] = 'MND' 
		},
		Neck   = 'Uggalepih Pendant//SPECIAL',		-- +8 MAB if MP < 51%
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
		},
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
		},
	},

--[[
	*****************************
	* Midcast: Enfeebling Magic *
	****************************
--]]

	['Enfeebling_Magic_Skill'] = {
		Neck = 'Enfeebling Torque',		--  +8 Enfeebling Magic Skill
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
			[1] = 'INT' 
		},
		Neck   = 'Enfeebling_Magic_Skill::Neck',
	},

	['EnfeeblingMND'] = {
		Subset = { 
			[1] = 'MND' 
		},	
		Neck   = 'Enfeebling_Magic_Skill::Neck',
	},

	['EnfeeblingMagic'] = {
		Neck   = 'Enfeebling_Magic_Skill::Neck',
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
		Main  = 'Pluto\'s Staff',			-- +2 CHR
		Head  = 'Entrancing Ribbon',		-- +2 CHR
		Ears  = 'Beastly Earring',			-- +2 CHR
		Body  = { 'Errant Hpl.', 'Brigandine//IF:Black Cotehardie' },	-- +10 CHR, filler to avoid -3 CHR
		Neck  = { 'Star Necklace', 'Flower Necklace' },	-- +3/3 CHR
		Rings = 'Kshama Ring No.6',			-- +3 CHR
		Waist = { 'Corsette', 'Mrc.Cpt. Belt' },	-- +5/1 CHR
		Legs  = 'Errant Slops',						-- +7 CHR
	},
	
--[[
	EnhancementSinging contains gear that enhances party members is some specific
	manner. Included are: minne, minuet, paeon, pastoral, madigal, mambo, etude,
	operetta, ballad, march, prelude, aubade, carol, mazurka, gavotte, capriccio,
	fantasia, hymnus, and round.
--]]

	['EnhancementSinging'] = {
		Subset = { 
			[1] = 'CHR' 
		},	
	},

--[[
	EnfeeblingSinging contains gear that debuffs targets. Included are: requiem,
	threnody, lullaby, finale, elegy, and virelai.
--]]
	
	['EnfeeblingSinging'] = {
		Subset = { 
			[1] = 'CHR' 
		},	
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
	
-- An elemental stave will be checked for after the debuff set is loaded.
	
	['NinjutsuDebuff'] = {
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
			[1] = 'INT'
		},
		Neck   = 'Uggalepih Pendant//SPECIAL',		-- +8 MAB if MP < 51%
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
	The following weapon skill gearsets are defined by the stat they emphasize. 
	Listed are all of the sets that	you will need to use every weapon skill 
	that your job can do. The leading comment defines what weapon/weapon skill
	combination the set applies to.
	
	SMN can use the following weapons: staff (B), Club (C+), dagger (E). 
	
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

	['AttackPower'] = {
		Ears  = { 'Ethereal Earring', 'Coral Earring', 'Fang Earring', 'Brutal Earring' },	-- +5/5/4 Att, Store TP +1
		Rings = 'Kshama Ring No.8',					-- +3 Att
		Waist = 'Hierarch Belt//IF:SWIFT BELT',	-- Swift belt has -5 attack, take the MP
	},
	
--[[
		* Strength based *
		
		Staff: Heavy Swing,Shell Crusher,Full Swing
		Club: Brainshaker,Skullbreaker,True Strike
-]]
	
	['WS_STR'] = {
		Subset = { 
			[1] = 'AttackPower' 
		},
        Neck   = 'Spike Necklace',		-- +3 STR
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
	
	['WS_STRINT'] = {
		Subset = { 
			[1] = 'AttackPower' 
		},
        Head   = { 'Smn. Horn +1', 'Evoker\'s Horn' },	-- +4/3 INT
        Neck   = 'Spike Necklace',	-- +3 STR
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
	
	['WS_STRMND'] = {
		Subset = { 
			[1] = 'AttackPower' 
		},
        Neck   = { 'Promise Badge', 'Justice Badge' },	-- +5/3 MND
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

	['WS_STRMND_30_50'] = {
		Subset = {  
			[1] = 'AttackPower' 
		},
        Neck   = { 'Promise Badge', 'Justice Badge' },	-- +5/3 MND
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
	
	['WS_DEX'] = {
		Subset = { 
			[1] = 'AttackPower' 
		},
        Head   = 'Empress Hairpin',		-- +3 DEX
        Neck   = 'Spike Necklace',		-- +3 DEX
        Body   = { 'Black Cotehardie', 'Brigandine' },	-- +2/2 DEX
        Rings  = 'Kshama Ring No.2',		-- +3 DEX
        Waist  = 'Mrc.Cpt. Belt',		-- +1 DEX
    },

--[[
		* Dexterity and Intelligence based *

		Dagger: Gust Slash,Cyclone^

		^ Subjob must be one of: RDM,THF,BRD,RNG,NIN
--]]
	
	['WS_DEXINT'] = {
		Subset = { 
			[1] = 'AttackPower' 
		},
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
	
	['WS_INT'] = {
		Subset = { 
			[1] = 'AttackPower' 
		},
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
	
	['WS_INTMND'] = {
		Subset = { 
			[1] ='AttackPower' 
		},
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
	
	['WS_CHR'] = {
		Subset = {
			[1] = 'AttackPower',
			[2] = 'CHR',
		},
    },

--[[
		* Mind based *

		Dagger: Energy Steal, Energy Drain^
		
		^ Subjob must be RDM,THF,BRD,RNG, or NIN
--]]

	['WS_MND'] = {
		Subset = { 
			[1] = 'AttackPower',
			[2] = 'MND',
		},
    },
	
--[[
		* Skill based *
		
		Club: Starlight,Moonlight
--]]
	
	['WS_Skill'] = {
		Subset = { 
			[1] = 'AttackPower' 
		},
    },
	
--[[
	Kite is used for kiting. Emphasis should be placed on gear that increases 
	movement speed, but you might also want gear that has evasion. The choice
	is yours.
--]]

	['Kite'] = { 
	},

--[[
	The following are SMN abilities that can be affected by gear.
--]]

	['AstralFlow'] = {
	},

--[[
	The following skills are from all the rest of the jobs besides SMN up to level
	37 (max subjob level). They're all listed here since I can't assume what subjob
	you will use.
--]]

	--* /BST *--
	['Charm'] = {				-- charm skill, CHR gear
		Subset = { 
			[1] = 'CHR' 
		},
    },

	['Reward'] = {
	},
	
	-- With tame, remember that if your INT is higher than the target's INT, you're 
	-- less likely to be resisted
	['Tame'] = {
	},
	
	['Pet_Attack'] = {					-- Pet's strength, not accuracy
	},
	
	['Pet_Macc'] = {					-- Pet's Magical Accuracy
		Head = 'Shep. Bonnet',
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
		Rings = 'Kshama Ring No.2',		-- +3 DEX
		Waist = 'Mrc.Cpt. Belt',							-- +1 DEX
	},
	
	['Flee'] = {
	},
	
	['TrickAttack'] = {
		Head  = 'Empress Hairpin',							-- +3 AGI
		Ears  = { 'Genin Earring//SJNIN', 'Drone Earring' },-- +4 AGI if sj NIN, +3 AGI
		Rings = 'Kshama Ring No.3',							-- +3 AGI
		Waist = 'Mrc.Cpt. Belt',							-- +1 AGI
	},
	
	['SATA'] = {
		Head  = 'Empress Hairpin',							-- +3 DEX/+3 AGI
		Neck  = 'Spike Necklace',							-- +3 DEX
		Ears  = { 'Genin Earring//SJNIN', 'Drone Earring' },	-- +4 AGI if sj NIN, +3 AGI
		Rings = { 'Kshama Ring No.2', 'Kshama Ring No.3' },	-- +3 DEX, +3 AGI
		Waist = 'Mrc.Cpt. Belt',							-- +1 DEX/+1 AGI
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
	The following set is used to dynamically create a gear set to be displayed 
	once rather	than in a piecemeal manner. It is hoped that this will cut down 
	on flickering gear and possibly speed up the code. 
	
	*** This set is to be left empty by the player ***. Please do not modify it.
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
	['STAVE'] =  { 'Fire Staff', 'Vulcan\'s Staff', 'Ice Staff', 'Aquilo\'s Staff',
				  'Wind Staff', 'Auster\'s Staff', 'Earth Staff', 'Terra\'s Staff',
				  'Thunder Staff', 'Jupiter\'s Staff', 'Water Staff', 'Neptune\'s Staff',
				  'Light Staff', 'Apollo\'s Staff', 'Dark Staff', 'Pluto\'s Staff',
				  'Kukulcan\'s Staff' },
	['CLUB']  =  { 'Warp Cudgel', 'Solid Wand', 'Yew Wand', 'Pilgrim\'s Wand' },
	['DAGGER'] = { 'Garuda\'s Dagger' },
};

profile.Sets = sets;
profile.sjb = nil;			-- Tracks subjob name
profile.sPetAction = nil;	-- what was the last action by your avatar
profile.bAmmo = false;		-- /BST specific. Is ammo equipped?
profile.sAmmo = nil;		-- /BST specific. Name of ammo equipped

--[[
	HandlePetAction equips the appropriate gear set based on the type of action
	the pet is trying to perform. (This is specifically rage blood pacts.)
--]]

function HandlePetAction(Pet,PetAction)
	local bSmn = false;
	
	-- Determine name of pet
	Pet.Name = string.lower(Pet.Name);
	if gcinclude.fSummonerPet() == true then
		-- Since the pet is a smn avatar, give feedback on the blood pact
		bSmn = true;
		-- if the action is a BP: rage, print out what happened in party chat
		if (profile.sPetAction == nil or profile.sPetAction ~= PetAction.Name) and 
				gcdisplay.GetToggle('sBP') == true then
			local sMsg;
			if string.find(gcinclude.SmnBPRageList,PetAction.Name) ~= nil then
				sMsg = '/p  [<pet>] [Blood Pact: ' .. PetAction.Name .. '] >> <t>.';
			else
				sMsg = '/echo [<pet>] [Blood Pact: ' .. PetAction.Name .. ']';
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
		-- All SMN pet actions are blood pacts. Address accordingly
		if (gcinclude.SmnSkill:contains(PetAction.Name)) then		 	-- summoning skill based blood pact?
			gcinclude.MoveToCurrent(sets.SmnSkill,sets.CurrentGear);
		elseif (gcinclude.SmnMagical:contains(PetAction.Name)) then		-- magical based blood pact?
			gcinclude.MoveToCurrent(sets.SmnMagical,sets.CurrentGear);	
		elseif (gcinclude.SmnHybrid:contains(PetAction.Name)) then		-- hybrid blood pact (2x physical, 1x magical)?
			gcinclude.MoveToCurrent(sets.SmnHybrid,sets.CurrentGear);					
		else															-- physical skill blood pact
			gcinclude.MoveToCurrent(sets.SmnPhysical,sets.CurrentGear);	
		end
	else
		-- Must be a /BST charmed pet.
		if (gcinclude.BstPetAttack:contains(PetAction.Name)) then				-- Pet Attack
			gcinclude.MoveToCurrent(sets.Pet_Attack,sets.CurrentGear);		
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
	which subjob is current. 
--]]

function SetSubjobSet(chkSJ)
	-- "chkSJ" is the key for what toolbar is shown. All jobs are defined in the subs table.
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
	OnLoad is run whenever you log into your SMN or change your job to SMN
--]]

function profile.OnLoad()
	local player = gData.GetPlayer();

	-- Initialize settings
	gSettings.AllowAddSet = true;
	gcinclude.Initialize();
	gcinclude.settings.bWSOverride = true;
	
	-- Coded order of operation override
	gcinclude.settings.priorityEngaged = 'CEF';
	gcinclude.settings.priorityWeaponSkill = 'ADBE';	
	
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
end		-- OnLoad

--[[
	OnUnload is run when you change to another job
--]]

function profile.OnUnload()
	gcinclude.Unload();
end		-- OnUnload

--[[
	HandleCommand is run when you type in a command defined in LUASHITACAST. The commands handled here instead
	of in gcinclude.HandleCommands are specific to SMN or the help system.
--]]

function profile.HandleCommand(args)
	if args[1] == 'help' then
		gcdisplay.ShowHelp(args);
	elseif args[1] == 'petfood' then
		gcinclude.doPetFood(args[2],args[3]);		
	else
		gcinclude.HandleCommands(args);
	end
end		-- HandleCommand

--[[
	HandleDefault is run when some action happens. This emphasizes pet actions
--]]
	
function profile.HandleDefault()
	local pet = gData.GetPet();
	local petAction = gData.GetPetAction();
	local player = gData.GetPlayer();
	local zone = gData.GetEnvironment();
	local ew = gData.GetEquipment();
	local bSA = gcinclude.fBuffed('Sneak Attack');
	local bTA = gcinclude.fBuffed('Trick Attack');	
	local eWeap = nil;
	local cKey,sGear;

	gcinclude.StartReminder();		-- See if reminder should be printed
	
	-- A pet action takes priority over a player's action. Only SMN avatar actions supported
	if pet ~= nil then
		local sLName = string.lower(pet.Name);
		if petAction ~= nil and gcinclude.fSummonerPet() == true then
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
	gcdisplay.Update();

	-- Only gear swap if this flag is true
	if gcdisplay.GetToggle('GSwap') == false then
		return;
	end

	-- Assuming you're /bst, when you want to reward your pet and you do not have pet food 
	-- equipped, the current item in the ammo slot is saved. The following will set it back
	-- to what you had before unless the slot is locked.
	if player.SubJob == 'BST' and 
	   profile.bAmmo == true and 
	   gcinclude.fIsLocked('ammo') == false then
		sets.CurrentGear['Ammo'] = profile.sAmmo;
		profile.sAmmo = nil;
		profile.bAmmo = false;
	end
	
	-- Clear out the CurrentGear in case of leftovers
	gcinclude.ClearSet(sets.CurrentGear);
	
	-- If player is not resting and has a pet, make sure they're holding the correct 
	-- staff (assuming they own the correct staff)
	if player.Status ~= 'Resting' then 
		-- If there's a pet
		if pet ~= nil then 
			local sStave = gcinclude.fCheckForElementalGearByValue('staff','Summons',pet.Name);
			if sStave ~= nil then 
				gcinclude.fSwapToStave(sStave,true,sets.CurrentGear);
			end
		else
			-- Since no pet, assuming slot not locked, equip the default weapon
			if gcinclude.fIsLocked('main') == false then
				gcinclude.FractionalSet(sets.Start_Weapons,'Main');
			end
		end
	end

	-- Start with the default set
	gcinclude.MoveToCurrent(sets.Default,sets.CurrentGear);
	
	-- Now process the pet/player statuses accordingly.
	if (pet ~= nil and pet.Status == 'Engaged') or (player.Status == 'Engaged') then
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
			gcinclude.MoveToCurrent(sets.TP,sets.CurrentGear);
			gcinclude.settings.priorityEngaged = string.upper(gcinclude.settings.priorityEngaged);
			for i = 1,string.len(gcinclude.settings.priorityEngaged),1 do
				cKey = string.sub(gcinclude.settings.priorityEngaged,i,i);
				if cKey == 'C' then		-- Evasion
					if gcdisplay.GetToggle('Eva') == true then
						gcinclude.MoveToCurrent(sets.Evasion,sets.CurrentGear);
					end
				elseif cKey == 'E' then		-- Accuracy
					gcinclude.ProgressiveAccuracy('Acc');
					--gcinclude.FractionalAccuracy(sets.Accuracy);
				elseif cKey == 'F' then		-- Kiting
					if (gcdisplay.GetToggle('Kite') == true) then
						gcinclude.MoveToCurrent(sets.Kite,sets.CurrentGear);
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
	elseif gcinclude.fSummonerPet() then
		-- Player idling with pet
		if gcdisplay.GetToggle('Idle') == true then
			gcinclude.MoveToCurrent(sets.Default_WPet,sets.CurrentGear);
		end
	else
		-- Assume player idling without pet or /subjob's pet		
		gcinclude.MoveToCurrent(sets.Default,sets.CurrentGear);
	end
		
	-- Make sure to equip the appropriate elemental staff (if appropriate)
	-- for the current pet

	if (gcinclude.fSummonerPet() == true) then
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
	
	-- Equip the composited HandleDefault set
	gcinclude.EquipTheGear(sets.CurrentGear);
	
	-- Lastly, update the display, just in case
	gcdisplay.Update();
end		-- HandleDefault

--[[
	HandleAbility is used to change the player's gear appropriately for the specified avatar ability.
--]]

function profile.HandleAbility()
	local player = gData.GetPlayer();
	local ability = gData.GetAction();
	local eq = gData.GetEquipment();
	local sj = player.SubJob;
	
	if sj == nil or sj == 'NON' or ability.Name == nil then
		return;
	end
	
	-- gear swapping is turned off or the ability is release, assault, or retreat, no specific gear set
	-- needs to be loaded. Exit function if encountered.
	if ((gcdisplay.GetToggle('GSwap') == false) or
		(ability.Name == 'Release') or 
		(ability.Name == 'Assault') or
		(ability.Name == 'Retreat')) then 
		return;
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
	
	-- Check for abilities that are not associated with smn.
	-- /BST
	if sj == 'BST' then
		if string.match(ability.Name, 'Reward') then
			-- Pet reward. Make sure that pet food already equipped
			if profile.sAmmo == nil or string.find(string.lower(profile.sAmmo),'pet f') == nil then		-- something else equipped
				profile.bAmmo = gcinclude.doPetFood('max',nil);
			end
			gcinclude.MoveToCurrent(sets.Reward,sets.CurrentGear);
		elseif string.match(ability.Name, 'Charm') then
			gcinclude.MoveToCurrent(sets.Charm,sets.CurrentGear);
			local sStave = gcinclude.fCheckForEleGear('staff','light');
			if sStave ~= nil then
				gcinclude.fSwapToStave(sStave,false,sets.CurrentGear);
			end
		elseif string.match(ability.Name, 'Tame') then
			gcinclude.MoveToCurrent(sets.Tame,sets.CurrentGear);
		end
	-- /WAR
	elseif sj == 'WAR' then
		if string.match(ability.Name, 'Provoke') then
			gcinclude.MoveToCurrent(sets.Provoke,sets.CurrentGear);
		elseif string.match(ability.Name, 'Berserk') then
			gcinclude.MoveToCurrent(sets.Berserk,sets.CurrentGear);
		elseif string.match(ability.Name, 'Defender') then
			gcinclude.MoveToCurrent(sets.Defender,sets.CurrentGear);
		elseif string.match(ability.Name, 'Warcry') then
			gcinclude.MoveToCurrent(sets.Warcry,sets.CurrentGear);
		end
	--* /MNK *--
	elseif sj == 'MNK' then
		if string.match(ability.Name, 'Boost') then
			gcinclude.MoveToCurrent(sets.Boost,sets.CurrentGear);
		elseif string.match(ability.Name, 'Focus') then
			gcinclude.MoveToCurrent(sets.Focus,sets.CurrentGear);
		elseif string.match(ability.Name, 'Dodge') then
			gcinclude.MoveToCurrent(sets.Dodge,sets.CurrentGear);
		elseif string.match(ability.Name, 'Chakra') then
			gcinclude.MoveToCurrent(sets.Chakra,sets.CurrentGear);
		end
	-- /THF
	elseif sj == 'THF' then
		if string.match(ability.Name, 'Steal') then
			gcinclude.MoveToCurrent(sets.Steal,sets.CurrentGear);
		elseif string.match(ability.Name, 'Sneak Attack') then
			gcinclude.MoveToCurrent(sets.SneakAttack,sets.CurrentGear);
		elseif string.match(ability.Name, 'Flee') then
			gcinclude.MoveToCurrent(sets.Flee,sets.CurrentGear);
		elseif string.match(ability.Name, 'Trick Attack') then
			gcinclude.MoveToCurrent(sets.TrickAttack,sets.CurrentGear);
		elseif string.match(ability.Name, 'Mug') then
			gcinclude.MoveToCurrent(sets.Mug,sets.CurrentGear);
		end
	-- /WHM
	elseif sj == 'WHM' then
		if string.match(ability.Name, 'Divine Seal') then
			gcinclude.MoveToCurrent(sets.DivineSeal,sets.CurrentGear);	
		end
	-- /BLM
	elseif sj == 'BLM' then
		if string.match(ability.Name, 'Elemental Seal') then
			gcinclude.MoveToCurrent(sets.ElementalSeal,sets.CurrentGear);
		end
	-- /DRK
	elseif sj == 'DRK' then
		if string.match(ability.Name, 'Arcane Circle') then
			gcinclude.MoveToCurrent(sets.ArcaneCircle,sets.CurrentGear);	
		elseif string.match(ability.Name, 'Last Resort') then
			gcinclude.MoveToCurrent(sets.LastResort,sets.CurrentGear);
		elseif string.match(ability.Name, 'Weapon Bash') then
			gcinclude.MoveToCurrent(sets.WeaponBash,sets.CurrentGear);
		elseif string.match(ability.Name, 'Souleater') then
			gcinclude.MoveToCurrent(sets.Souleater,sets.CurrentGear);
		end
	-- /RNG
	elseif sj == 'RNG' then
		if string.match(ability.Name, 'Sharpshot') then
			gcinclude.MoveToCurrent(sets.Sharpshot,sets.CurrentGear);
		elseif string.match(ability.Name, 'Scavenge') then
			gcinclude.MoveToCurrent(sets.Scavenge,sets.CurrentGear);
		elseif string.match(ability.Name, 'Camouflage') then
			gcinclude.MoveToCurrent(sets.Camouflage,sets.CurrentGear);
		elseif string.match(ability.Name, 'Barrage') then
			gcinclude.MoveToCurrent(sets.Barrage,sets.CurrentGear);	
		end
	-- /SAM
	elseif sj == 'SAM' then
		if string.match(ability.Name, 'Warding Circle') then
			gcinclude.MoveToCurrent(sets.WardingCircle,sets.CurrentGear);			
		elseif string.match(ability.Name, 'Third Eye') then
			gcinclude.MoveToCurrent(sets.ThirdEye,sets.CurrentGear);
		elseif string.match(ability.Name, 'Hasso') then
			gcinclude.MoveToCurrent(sets.Hasso,sets.CurrentGear);
		elseif string.match(ability.Name, 'Meditate') then
			gcinclude.MoveToCurrent(sets.Meditate,sets.CurrentGear);
		elseif string.match(ability.Name, 'Seigan') then
			gcinclude.MoveToCurrent(sets.Seigan,sets.CurrentGear);
		end
	-- /DRG
	elseif sj == 'DRG' then
		if string.match(ability.Name, 'Ancient Circle') then
			gcinclude.MoveToCurrent(sets.AncientCircle,sets.CurrentGear);			
		elseif string.match(ability.Name, 'Jump') then
			gcinclude.MoveToCurrent(sets.Jump,sets.CurrentGear);
		elseif string.match(ability.Name, 'High Jump') then
			gcinclude.MoveToCurrent(sets.HighJump,sets.CurrentGear);
		end
	-- /PLD
	elseif sj == 'PLD' then
		if string.match(ability.Name, 'Holy Circle') then
			gcinclude.MoveToCurrent(sets.HolyCircle,sets.CurrentGear);
		elseif string.match(ability.Name, 'Shield Bash') then
			gcinclude.MoveToCurrent(sets.ShieldBash,sets.CurrentGear);
		elseif string.match(ability.Name, 'Sentinel') then
			gcinclude.MoveToCurrent(sets.Sentinel,sets.CurrentGear);	
		elseif string.match(ability.Name, 'Cover') then
			gcinclude.MoveToCurrent(sets.Cover,sets.CurrentGear);	
		end
	end
	
	-- SMN abilities
	if string.find(ability.Name, 'Astral Flow') then
		gcinclude.MoveToCurrent(sets.AstralFlow,sets.CurrentGear);
	elseif table.find(gcinclude.SmnSkill,ability.Name) ~= nil or
		   table.find(gcinclude.SmnMagical,ability.Name) ~= nil or
		   table.find(gcinclude.SmnAccuracy,ability.Name) ~= nil or
		   table.find(gcinclude.SmnHybrid,ability.Name) ~= nil or
		   string.find(gcinclude.SmnBPRageList,ability.Name) ~= nil then
		gcinclude.MoveToCurrent(sets.BP,sets.CurrentGear);
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
	
	-- Only process if /gswap is turned on
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

function profile.HandlePreshot()
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

function profile.HandleMidshot()
	-- Only gear swap if this flag is true
	if gcdisplay.GetToggle('GSwap') == false then
		return;
	end

	-- Clear out the CurrentGear in case of leftovers
	gcinclude.ClearSet(sets.CurrentGear);
	
	gcinclude.MoveToCurrent(sets.Midshot,sets.CurrentGear);
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
	-- cancelled so you do not lose your TP.
	if canWS == false then 
		gFunc.CancelAction();
		return;
	end

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
