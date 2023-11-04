local profile = {};
gcinclude = gFunc.LoadFile('common\\gcinclude.lua');

local sets = {
--[[
	The Idle sets are for when you're not in town and not doing anything like fighting (or pet fighting), casting, 
	resting, etc. "Idle" is the default setting and "_Refresh" and "_Regen" are when you want to emphasize the
	aforementioned ability.
	
	Regardless, do not include anything from the first row of equipment (Main, Sub, Ranged, Ammo). Doing so you
	will find yourself fighting luashitacast (like when equiping a dark staff when you are resting.)
	
	Most gear sets have an associated conditional set. These are for supporting items that give stat bonuses when
	a certain condition is met (eg., at nighttime or when there's a full moon.) You will find all the items supported
	in the "Conditional gear master list.txt" file which is also found in the /common/ directory. Just copy and paste
	the line item you want in the appropriate conditional area. (Please note that if you have more than one item to
	be conditionally considered, you will have to add a comma after each entry.
--]]

--[[
	The "Idle" set is what your character will wear when it is not fighting nor resting nor in town. This includes
	your pet. Whether just standing out of town or going to a different area, the "Idle" set will be equipped.
--]]

	['Idle'] = {
        Head = 'Panther Mask',
        Neck = 'Ryl.Grd. Collar',
        Ear1 = 'Coral Earring',
        Ear2 = 'Beastly Earring',
        Body = 'Narasimha\'s Vest',
        Hands = 'Thick Mufflers',
        Ring1 = 'Sun Ring',
        Ring2 = 'Sun Ring',
        Back = 'Psilos Mantle',
        Waist = 'Swift Belt',
        Legs = 'Thick Breeches',
        Feet = 'Thick Sollerets',
    },
	['Idle_Conditional'] = {
		{'BD-1','Gaudy Harness','Adds refresh if MP < 50'},
		{'RN-11','Tamas Ring','will equip if subjob can do magic'}
	},
	
	--[[
		The Idle_Regen and Idle_Refresh gear sets replace the normal Idle set when the player's HP or MP
		go below a set percentage, accordingly. For HP it is 60% and for MP it is 70%. These percentages
		are defined in gcinclude.lua. (Please note that the BST Gaudy Harness works independently from
		these settings, it is a piece of conditional gear.)
	--]]
	
	['Idle_Regen'] = {
	},
	
	['Idle_Refresh'] = {
	},
	
	--[[
		Resting emphasizes either HP regen or MP refresh. Regen is the assumed target when resting
	--]]
	
	['Resting'] = { 
	},
	['Resting_Conditional'] = {
	},
	
	['Resting_Refresh'] = {
	},
	['Resting_Refresh_Conditional'] = {
		{'BD-1','Gaudy Harness','Adds refresh if MP < 50'}
	},
	
--[[
	Start weapons are where you define what you want the first row of equipment to look like when you
	either log in as a BST or you switch your main job to BST. Any other gear you mention will be overridden
	by the Idle or Town set, so no need to include here.
--]]

	['Start_Weapons'] = {
        Main = 'Maneater',
        Sub = 'Tatami Shield',
        Ammo = 'S. Herbal Broth',
    },
	['Start_Weapons_Conditional'] = {
	},
	
--[[
	What do you want to wear around town? You can define a full set or just an item or two, it is up to you.
	(Please note that a nation's aketon is considered conditional gear, so no need to place here unless you
	want the aketon equipped regardless if it is your home nation's city or not.)
--]]
	
	['Town'] = {
        Head = 'Lilac Corsage',
    },
	
--[[
	Damage reduction gear depends on the type of damage. The most common is physical, but there's times when
	you'll want to reduce magic damage or breath damage. The three gear sets are defined below. The correct
	one will be loaded depending if DT is indicated and the TYPE selected. Please consider not including gear
	that doesn't have any damage taken property so other wanted stats can shine through
--]]

	['DT_Physical'] = {
	},
	['DT_Physical_Conditional'] = {
	},
	
	['DT_Magical'] = {
        Ear1 = 'Coral Earring',
    },
	['DT_Magical_Conditional'] = {
	},
	
	['DT_Breath'] = { 
	},
	['DT_Breath_Conditional'] = {
	},
	
	['DT_Evasion'] = {
	},
	['DT_Evasion_Conditional'] = {
	},
	
--[[
		The TP sets are used when you or your pet are fighting. The accuracy set will be used if ACC is specified
		and the evasion set if EVA is specified.
--]]

	['TP'] = {
        Head = 'Shep. Bonnet',
        Neck = 'Ryl.Grd. Collar',
        Ear1 = 'Coral Earring',
        Ear2 = 'Beastly Earring',
        Body = 'Narasimha\'s Vest',
        Hands = 'Thick Mufflers',
        Ring1 = 'Sun Ring',
        Ring2 = 'Tamas Ring',
        Back = 'Psilos Mantle',
        Waist = 'Swift Belt',
        Legs = 'Thick Breeches',
        Feet = 'Thick Sollerets',
    },
	['TP_Conditional'] = {
		{'BD-1','Gaudy Harness','Adds refresh if MP < 50'},
		{'RN-11','Tamas Ring','will equip if subjob can do magic'}
	},
	
	['TP_Accuracy'] = {
        Head = 'Optical Hat',
        Neck = 'Ryl.Grd. Collar',
        Ear1 = 'Coral Earring',
        Ear2 = 'Beastly Earring',
        Body = 'Narasimha\'s Vest',
        Hands = 'Thick Mufflers',
        Ring1 = 'Jaeger Ring',
        Ring2 = 'Toreador\'s Ring',
        Back = 'Psilos Mantle',
        Waist = 'Life Belt',
        Legs = 'Thick Breeches',
        Feet = 'Thick Sollerets',
    },
	['TP_Accuracy_Conditional'] = {
	},
	
	['TP_Evasion'] = {
        Head = 'Empress Hairpin',
        Body = 'Narasimha\'s Vest',
        Legs = 'San. Trousers',
    },
	['TP_Evasion_Conditional'] = {
	},
	
--[[
	Preshot is the first stage of when a ranged shot is being performed. This is where you place any 
	Ranged Accuracy or Ranged Attack Speed gear. 
--]]

	['Preshot'] = {
        Head = 'Optical Hat',
        Ring1 = 'Jaeger Ring',
        Back = 'Psilos Mantle',
    },
	['Preshot_Conditional'] = {
	},
	
--[[
	Midshot is the second stage of a ranged shot. This is where you place Ranged Attack or Ranged 
	Damage gear
--]]

	['Midshot'] = {
        Back = 'Psilos Mantle',
    },
	['Midshot_Conditional'] = {
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
	['Precast_Conditional'] = {
	},

--[[
	The second stage is Midcast. This is where you'll want to equip magic attack, magic attack 
	bonus, or magic enhancing gear.
--]]	

	['Midcast'] = {
	},
	['Midcast_Conditional'] = {
	},

--[[
	Further, there is a break out for each type of spell you can cast. For example, if you are
	doing a Cure, you wand mind (MND) gear, etc.
--]]

	['Cure'] = {				-- Healing Magic Skill, Cure Potency
    },
	['Cure_Conditional'] = {
	},
	
	['Dark'] = {				-- Dark Magic Skill, magical accuracy
        Ring1 = 'Tamas Ring',
    },
	['Dark_Conditional'] = {
	},
	
	['Divine'] = {				-- Divine Magic Skill
	},
	['Divine_Conditional'] = {
	},
	
	['Enfeebling'] = {	 		-- Enfeebling Magic Skill
	},
	['Enfeebling_Conditional'] = {
	},
	
	['Enhancing'] = {	 		-- Enhancing Magic Skill, enhancing magic time reduction, enhancing magic duration
	},
	['Enhancing_Conditional'] = {
	},
	
	['Elemental'] = {			-- Elemental Magic Skill, magic accuracy, elemental magic casting time reduction, elemental magic recast time reduction, magic burst bonus, magic attack bonus
        Ring1 = 'Tamas Ring',
	},
	['Elemental_Conditional'] = {
	},
	
	['Ninjitsu'] = {			-- Ninjitsu Skill, magic burst bonus, magic attack bonus
	},
	['Ninjitsu_Conditional'] = {
	},
	
	['Summoning'] = {			-- Summoning Skill, spell interruption rate, summoning magic casting time reduction
	},
	['Summoning_Conditional'] = {
	},
	
--[[
	Next is stat-based gear, (in this case intelligence or mind)
--]]

	['INT'] = {
        Ring1 = 'Tamas Ring',			-- +2~5 INT
        Ring2 = 'Windurstian Ring',		-- +1 INT
        Feet = 'Mannequin Pumps',		-- +1 INT
    },
	['INT_Conditional'] = {
	},
	
	['MND'] = {
        Head = 'Beast helm',			-- +3 MND
        Neck = 'Justice Badge',			-- +3 MND
        Body = 'Wonder Kaftan',			-- +1 MND
        Ring1 = 'Tranquility Ring',		-- +2 MND
        Ring2 = 'Tamas Ring',			-- +2~5 MND
        Waist = 'Friar\'s Rope',		-- +1 MND
        Legs = 'Wonder Braccae',		-- +2 MND
        Feet = 'Mannequin Pumps',		-- +2 MND
	},
	['MND_Conditional'] = {
	},
	
--[[
	Magic accuracy gear
--]]

	['macc'] = {
	},
	['macc_Conditional'] = {
	},
	
--[[
	And some spells are special cases, so they have individual gears sets.
--]]
	-- Phalanx: Enhancing Magic Skill
	['Phalanx'] = {
	},
	['Phalanx_Conditional'] = {
	},
	
	-- Combination of MND and enhancing skill. MND is 3x more important. There's also gear that enhances
	['Stoneskin'] = {	
	},	
	['Stoneskin_Conditional'] = {
	},
	
	-- Refresh gear, refresh duration gear
	['Refresh'] = { 
	},
	['Refresh_Conditional'] = {
	},

--[[
		BST can use the following weapons: axe (A-), scythe (B-), dagger (C+), club(D), sword (E). Any other weapon
		will have no weaponskill available. Weapon skill sets are named based on stat(s) used, regardless of weapon

		* Strength based or just skill based *
		Axe: Raging Axe,Smash Axe,Gale Axe,Avalanche Axe,Spinning Axe,Rampage,
			 Mistral Axe,Decimation
		Scythe: Slice,Spinning Scythe,Vorpal Scythe
		Club: Starlight,Brainshaker,Moonlight,Skullbreaker,True Strike
		Sword: Flat Blade,Circle Blade,Spirits Within,Vorpal Blade
		
		Aside: some weapons which the main job does not support are
		available. This is because of the subjob. I just found out 
		about this today. I'm not sure how high a skill level is supported,
		so I'll add them as I find out.
		
		Staff: Heavy Swing
-]]
	
	['WS_STR'] = {
        Head = 'Mrc.Cpt. Headgear',				-- +1 STR
        Neck = 'Spike Necklace',				-- +3 STR
		Ear2 = 'Beastly Earring',
        Body = 'Narasimha\'s Vest',				-- +3 STR
        Hands = 'Ogre Gloves',					-- +6 STR
        Ring1 = 'Sun Ring',						-- +3 STR
        Ring2 = 'Sun Ring',						-- +3 STR
        Back = 'Amemet Mantle',					-- +1 STR
        Waist = 'Barbarian\'s Belt',			-- +1 STR
        Legs = 'Wonder Braccae',				-- +1 STR
        Feet = 'Creek F Clomps',				-- +4 STR
    },
	['WS_STR_Conditional'] = {
	},
	
--[[
		* Strength and Dexterity based, even weighting *
		Sword: Fast Blade
--]]

	['WS_STRDEX'] = {
        Head = 'Empress Hairpin',				-- +3 DEX
        Neck = 'Spike Necklace',				-- +3 STR, +3 DEX
        Ear2 = 'Beastly Earring',
        Body = 'Narasimha\'s Vest',				-- +3 STR
        Hands = 'Ogre Gloves',					-- +6 STR -3 DEX
        Ring1 = 'Sun Ring',						-- +3 STR
        Ring2 = 'Sun Ring',						-- +3 STR
        Back = 'Amemet Mantle',					-- +1 STR
        Legs = 'Wonder Braccae',				-- +1 STR
        Feet = 'Creek F Clomps',				-- +4 STR
    },
	['WS_STRDEX_Conditional'] = {
	},

--[[
		* Strength and Intelligence based, even weighting *
		Scythe: Dark Harvest,Shadow of Death,Nightmare Scythe,Spiral Hell
		Sword: Burning Blade
		
		Because of subjob:
		Staff: Rock Crusher
--]]
	
	['WS_STRINT'] = {
		Head = 'Beast helm',					-- +5 INT
		Neck = 'Spike necklace',				-- +3 STR
		Ear2 = 'Beastly Earring',
		Body = 'Narasimha\'s vest',				-- +3 STR
		Hands = 'Ogre gloves',					-- +6 STR
		Ring1 = 'Sun ring',						-- +3 STR
		Ring2 = 'Sun ring',						-- +3 STR
		Waist = 'Barbarian\'s belt',			-- +1 STR
		Legs = 'Wonder braccae',				-- +1 STR
		Feet = 'Creek F clomps',				-- +4 STR
	},
	['WS_STRINT_Conditional'] = {
	},

--[[
		* Strength and Intelligence based, 30%/20% respectively *
		Sword: Red Lotus Blade
--]]
	
	['WS_STRINT_30_20'] = {
		Head = 'Beast helm',					-- +5 INT
		Neck = 'Spike necklace',				-- +3 STR
        Ear2 = 'Beastly Earring',
		Body = 'Narasimha\'s vest',				-- +3 STR
		Hands = 'Ogre gloves',					-- +6 STR
		Ring1 = 'Sun ring',						-- +3 STR
		Ring2 = 'Sun ring',						-- +3 STR
		Waist = 'Barbarian\'s belt',			-- +1 STR
		Legs = 'Wonder braccae',				-- +1 STR
		Feet = 'Creek F clomps',				-- +4 STR
	},
	['WS_STRINT_30_20_Conditional'] = {
	},

--[[
		* Strength and Mind based, even weighting *
		Scythe: Guillotine,Cross Reaper
		Club: Shining Strike,Seraph Strike,Judgement
		Sword: Shining Blade,Seraph Blade
--]]

	['WS_STRMND'] = {
		Head = 'Beast helm',					-- +3 MND
		Neck = 'Justice badge',					-- +3 MND
        Ear2 = 'Beastly Earring',
		Body = 'Narasimha\'s vest',				-- +3 STR
		Hands = 'Ogre gloves',					-- +6 STR
		Ring1 = 'Sun ring',						-- +3 STR
		Ring2 = 'Sun ring',						-- +3 STR
		Waist = 'Barbarian\'s belt',			-- +1 STR
		Legs = 'Wonder braccae',				-- +1 STR, +2 MND
		Feet = 'Creek F clomps',				-- +4 STR
	},
	['WS_STRMND_Conditional'] = {
	},

--[[
		* Strength and Vitality based, even weighting *
		Axe: Calamity (32%/32%)
--]]
	
	['WS_STRVIT'] = {
		Head = 'Mercenary Captain\'s headgear',	-- +1 STR
		Neck = 'Spike necklace',				-- +3 STR
        Ear2 = 'Beastly Earring',
		Body = 'Narasimha\'s vest',				-- +3 STR, +3 VIT
		Hands = 'Ogre gloves',					-- +6 STR
		Ring1 = 'Sun ring',						-- +3 STR
		Ring2 = 'Sun ring',						-- +3 STR
		Waist = 'Barbarian\'s belt',			-- +1 STR, +1 VIT
		Legs = 'Wonder braccae',				-- +1 STR, +2 VIT
		Feet = 'Creek F clomps',				-- +4 STR, +4 VIT
	},
	['WS_STRVIT_Conditional'] = {
	},

--[[
		* Dexterity based *
		Axe: Onslaught
		Dagger: Wasp Sting,Viper Bite^,Eviseration

		^ Subjob must be one of: RDM,THF,BRD,RNG,NIN
--]]
	
	['WS_DEX'] = {
        Head = 'Empress Hairpin',				-- +3 DEX
        Neck = 'Spike Necklace',				-- +3 DEX
        Ear2 = 'Beastly Earring',
        Body = 'Brigandine',					-- +2 DEX
        Hands = 'Beast Gloves',					-- +3 DEX
        Ring1 = 'Balance Ring',					-- +2 DEX
        Ring2 = 'Bastokan Ring',				-- +1 DEX
        Feet = 'Bounding Boots',				-- +3 DEX
    },
	['WS_DEX_Conditional'] = {
	},

--[[
		* Dexterity and Intelligence based *
		Dagger: Gust Slash,Cyclone^

		^ Subjob must be one of: RDM,THF,BRD,RNG,NIN
--]]
	
	['WS_DEXINT'] = {
        Head = 'Beast Helm',					-- +5 INT
        Neck = 'Spike Necklace',				-- +3 DEX
        Ear2 = 'Beastly Earring',
        Body = 'Brigandine',					-- +2 DEX
        Hands = 'Beast Gloves',					-- +3 DEX
        Ring1 = 'Balance Ring',					-- +2 DEX
        Feet = 'Bounding Boots',				-- +3 DEX
    },
	['WS_DEXINT_Conditional'] = {
	},

--[[
		* Intellegence and Agility based, even weighting *
		Scythe: Catastrophe
--]]
	
	['WS_INTAGI'] = {
        Head = 'Beast Helm',					-- +5 INT
        Ear1 = 'Drone Earring',					-- +3 AGI
        Ear2 = 'Beastly Earring',
        Body = 'Mrc.Cpt. Doublet',				-- +1 AGI
        Ring1 = 'Tamas Ring',					-- +2~5 INT
        Ring2 = 'Windurstian Ring',				-- +1 INT, +1 AGI
        Feet = 'Bounding Boots',				-- +3 AGI
    },
	['WS_INTAGI_Conditional'] = {
	},

--[[
		* Charisma based *
		Dagger: Shadowstitch
--]]
	
	['WS_CHR'] = {
		Head = 'Panther mask',					-- +5 CHR
		Neck = 'Flower Necklace',				-- +3 CHR
		Ear2 = 'Beastly earring',				-- +2 CHR
		Body = 'Gaudy harness',					-- +3 CHR
		Ring1 = 'Moon ring',					-- +3 CHR
		Ring2 = 'Moon ring',					-- +3 CHR
		Waist = 'Corsette',						-- +5 CHR
		Legs = 'Beast trousers',				-- +4 CHR
		Feet = 'Beast gaiters',					-- +3 CHR
	},
	['WS_CHR_Conditional'] = {
	},

--[[
		* Mind based *
		Dagger: Energy Steal, Energy Drain^
		
		^ Subjob must be RDM,THF,BRD,RNG, or NIN
--]]

	['WS_MND'] = {
        Head = 'Beast Helm',					-- +3 MND
        Neck = 'Justice Badge',					-- +3 MND
        Ear2 = 'Beastly Earring',
        Body = 'Wonder Kaftan',					-- +1 MND
        Ring1 = 'Tamas Ring',					-- +2~5 MND
        Ring2 = 'Tranquility Ring',				-- +2 MND
        Waist = 'Friar\'s Rope',				-- +1 MND
        Legs = 'Wonder Braccae',				-- +2 MND
    },
	['WS_MND_Conditional'] = {
	},
	
--[[
	Weapon skill sets don't consider accuracy in their definitions. If an accuracy emphasis is
	desired, the following set will replace the gear from the appropriate weapon skill set.
	(Please note: only include accuracy gear here so that the weapon skill set has some emphasis
	on the appropriate gear set loaded.
--]]

	['WS_Accuracy'] = {
        Head = 'Optical Hat',					-- +10 Acc
        Neck = 'Ryl.Grd. Collar',				-- +4 Acc
        Ear2 = 'Beastly Earring',
        Body = 'Narasimha\'s Vest',				-- +4 Acc
        Hands = 'Thick Mufflers',				-- +3 Acc
        Ring1 = 'Toreador\'s Ring',				-- +7 Acc
        Ring2 = 'Jaeger Ring',					-- +4 Acc
        Back = 'Psilos Mantle',					-- +1 Acc
        Waist = 'Life Belt',					-- +10 Acc
        Legs = 'Thick Breeches',				-- +2 Acc
        Feet = 'Thick Sollerets',				-- +2 Acc
    },	
	['WS_Accuracy_Conditional'] = {
	},
	
--[[
	The following sets are used with pet abilities/pet commands
--]]
	
	['Call_Beast'] = {			-- or bestial loyalty, augmented call beast gear
	},
	['Call_Beast_Conditional'] = {
	},
	
	-- Reward potency, reward augment,eward enhancement, and MND gear
	['Reward'] = {
        Head = 'Beast Helm',			-- +3 MND
        Neck = 'Justice Badge',			-- +3 MND
        Body = 'Beast Jackcoat',		-- Augments reward
        Hands = 'Ogre Gloves',			-- Enhances reward
        Ring1 = 'Tamas Ring',			-- +2~5 MND
        Ring2 = 'Tranquility Ring',		-- +2 MND
        Waist = 'Friar\'s Rope',		-- +1 MND
        Legs = 'Wonder Braccae',		-- +2 MND
        Feet = 'Beast Gaiters',			-- Enhances reward
    },
	['Reward_Conditional'] = {
	},
	
	-- Tame success rate
	['Tame'] = {
        Head = 'Beast Helm',			-- Increases tame success rate
    },
	['Tame_Conditional'] = {
	},
	
	-- CHR and Charm + gear. (Every +1 Charm adds 5% Charm duration
	['Charm'] = {
        Head = 'Beast Helm',			-- Charm+4
        Neck = 'Flower Necklace',		-- +3 CHR
        Ear2 = 'Beastly Earring',		-- +2 CHR
        Body = 'Beast Jackcoat',		-- Charm+5
        Hands = 'Beast Gloves',			-- Charm+3
        Ring1 = 'Moon Ring',			-- +3 CHR
        Ring2 = 'Moon Ring',			-- +3 CHR
        Waist = 'Corsette',				-- +5 CHR
        Legs = 'Beast Trousers',		-- Charm+6, +4 CHR
        Feet = 'Beast Gaiters',			-- Charm+2
    },
	['Charm_Conditional'] = {
	},
	
	['Charm_Evasion'] = {
	},
	['Charm_Evasion_Conditional'] = {
	},
	
	-- Applies to both Sic and Ready. Consider pet accuracy, similar abilities
	['PetReady'] = {
        Head = 'Shep. Bonnet',			-- +5 Pet Acc
        Ear2 = 'Beastly Earring',		-- +10 Pet Acc
    },
	['PetReady_Conditional'] = {
	},
	
	-- Fight command: affected by pet's STR
	['PetAttack'] = {	-- Fight command
	},
	['PetAttack_Conditional'] = {
	},

--[[
	Movement tends to be used for kiting. Emphasis should be placed on gear that increases movement speed, but you 
	might also want gear that has evasion. The choice is yours.
--]]

	-- Movement speed gear, does not include nation aketons which are found in conditional gear for home town
	['Movement'] = { 
	},
	['Movement_Conditional'] = {
	},

--[[
	Treasure Hunter gear
--]]

	['TH'] = {
	},
	['TH_Conditional'] = {
	},
	
--[[
	The following sets are added as a convenience for playing in level capped areas. The only way for them to be loaded
	is via the /gearset command, which will turn GSwap off. If you're level syncing, pick the set that's closest to the
	sync level and adjust accordingly. 
--]]

	['CAP20'] = {
        Head = 'Silver Hairpin',
        Neck = 'Rep.Bronze Medal',
        Ear1 = 'Onyx Earring',
        Ear2 = 'Onyx Earring',
        Body = 'Angler\'s Tunica',
        Hands = 'Ryl.Ftm. Gloves',
        Ring1 = 'Courage Ring',
        Ring2 = 'Balance Ring',
        Back = 'Lizard Mantle',
        Waist = 'Barbarian\'s Belt',
        Legs = 'Ryl.Ftm. Trousers',
        Feet = 'Bounding Boots',
    },
	
	['CAP25'] = {
        Head = 'Empress Hairpin',
        Neck = 'Spike Necklace',
        Ear1 = 'Beetle Earring',
        Ear2 = 'Beetle Earring',
        Body = 'Beetle Harness',
        Hands = 'Ryl.Ftm. Gloves',
        Ring1 = 'Courage Ring',
        Ring2 = 'Balance Ring',
        Back = 'Lizard Mantle',
        Waist = 'Barbarian\'s Belt',
        Legs = 'San. Trousers',
        Feet = 'Bounding Boots',
    },
	
	['CAP30'] = {
        Head = 'Shep. Bonnet',
        Neck = 'Spike Necklace',
        Ear1 = 'Beetle Earring',
        Ear2 = 'Beetle Earring',
        Body = 'Mrc.Cpt. Doublet',
        Hands = 'Wonder Mitts',
        Ring1 = 'Courage Ring',
        Ring2 = 'Tamas Ring',
        Back = 'Lizard Mantle',
        Waist = 'Barbarian\'s Belt',
        Legs = 'San. Trousers',
        Feet = 'Wonder Clomps',
    },
	
	['CAP40'] = {
        Main = 'Barbaroi Axe',
        Sub = 'Fish Scale Shield',
        Ammo = 'S. Herbal Broth',
        Head = 'Empress Hairpin',
        Neck = 'Spike Necklace',
        Ear1 = 'Beetle Earring',
        Ear2 = 'Beetle Earring',
        Body = 'Wonder Kaftan',
        Hands = 'Wonder Mitts',
        Ring1 = 'Courage Ring',
        Ring2 = 'Tamas Ring',
        Back = 'Lizard Mantle',
        Waist = 'Tilt Belt',
        Legs = 'Wonder Braccae',
        Feet = 'Bounding Boots',
    },
	
	['CAP50'] = {
        Main = 'Barbaroi Axe',
        Sub = 'Fish Scale Shield',
        Ammo = 'S. Herbal Broth',
        Head = 'Shep. Bonnet',
        Neck = 'Spike Necklace',
        Ear1 = 'Beetle Earring',
        Ear2 = 'Beetle Earring',
        Body = 'Gaudy Harness',
        Hands = 'Wonder Mitts',
        Ring1 = 'Courage Ring',
        Ring2 = 'Tamas Ring',
        Back = 'Raptor Mantle',
        Waist = 'Swift Belt',
        Legs = 'Wonder Braccae',
        Feet = 'Bounding Boots',
    },

	['CAP60'] = {
        Main = 'Darksteel Axe',
        Sub = 'Darksteel Buckler',
        Ammo = 'S. Herbal Broth',
        Head = 'Beast Helm',
        Neck = 'Ryl.Grd. Collar',
        Ear1 = 'Fang Earring',
        Ear2 = 'Fang Earring',
        Body = 'Beast Jackcoat',
        Hands = 'Beast Gloves',
        Ring1 = 'Sun Ring',
        Ring2 = 'Tamas Ring',
        Back = 'Raptor Mantle',
        Waist = 'Swift Belt',
        Legs = 'Beast Trousers',
        Feet = 'Beast Gaiters',
    },
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
profile.sjb = nil;
profile.bAmmo = false;
profile.sAmmo = nil;

--[[
	HandlePetAction equips the appropriate gear set based on the type of action
	the pet is trying to perform.
--]]

local function HandlePetAction(PetAction)
	if gcdisplay.GetToggle('GSwap') == true then		-- Only gear swap if this flag is true
		gFunc.EquipSet(sets.PetReady);

		if (gcinclude.BstPetAttack:contains(PetAction.Name)) then
			gFunc.EquipSet(sets.PetAttack);
			gcinclude.ProcessConditional(sets.PetAttack_Conditional,nil);	
		elseif (gcinclude.BstPetMagicAttack:contains(PetAction.Name)) then
			gFunc.EquipSet(sets.PetMagicAttack);
			gcinclude.ProcessConditional(sets.PetMagicAttack_Conditional,nil);			
		elseif (gcinclude.BstPetMagicAccuracy:contains(PetAction.Name)) then
			gFunc.EquipSet(sets.PetMagicAccuracy);
			gcinclude.ProcessConditional(sets.PetMagicAccuracy_Conditional,nil);			
		end
    end
end

--[[
	SetSubjobSet is used to pick the appropriate set for the loaded macrobook based on
	which subjob is current. (If no change has occurred since the last time it was called,
	nothing is checked/changed.)
--]]

local function SetSubjobSet(chkSJ)
	local sj;

	if (profile.sjb == nil or chkSJ ~= profile.sjb) then	-- Compare the stored subjob with the current subjob
		if (chkSJ == 'THF') then 
			sj = '2';										-- /THF
		elseif (chkSJ =='BLM') then
			sj = '3';										-- /BLM
		elseif (chkSJ =='NIN') then
			sj = '4';										-- /NIN
		else
			sj = '1';										-- Assume /WHM
		end
	
		AshitaCore:GetChatManager():QueueCommand(1, '/macro set '..sj);
		profile.sjb = chkSJ;
	end
end

--[[
	OnLoad is run whenever you log into your BST or change your job to BST
--]]

profile.OnLoad = function()
	local player = gData.GetPlayer();

	gSettings.AllowAddSet = true;
	gcinclude.Initialize();
	
	-- Determine if subjob uses magic and if the maximum MP is > 50.
	gcinclude.CheckMagic50(player);
	
	-- Set your job macro toolbar defaults here. Which set depends on the subjob
	AshitaCore:GetChatManager():QueueCommand(1, '/macro book 10');		-- BST
	SetSubjobSet(player.SubJob);
	
	-- Load up the weapons bar. (This need only be done once.)
	gFunc.EquipSet(sets.Start_Weapons);	
	gcinclude.ProcessConditional(sets.Start_Weapon_Conditional,nil);
end

--[[
	OnUnload is run when you change to another job
--]]

profile.OnUnload = function()
	gcinclude.Unload();
end

--[[
	ShowCommands is a helpful display of all of the commands that are available to the User.
	It does not display /lac commands.
--]]

local function ShowCommands(args)

	if #args == 1 then
		print(chat.header('Help'):append(chat.message('The following commands are available to use from within Luashitacast. These are targetting either your specific job or are available across all jobs.\n')));
		print(chat.header('Help'):append(chat.message('Commands for all jobs: ')));
		print(chat.header('Help'):append(chat.message('/gswap --Toggles whether automatic gear swaps occur or not. Default is TRUE.')));
		print(chat.header('Help'):append(chat.message('/wsdistance [#] --Toggles whether a distance check is done for non-ranged weaponskills and how far. Default TRUE at ' .. tostring(gcinclude.settings.WSdistance) .. ' yalms.')));
		print(chat.header('Help'):append(chat.message('/dt --Indicates if a damage taken set should be used')));
		print(chat.header('Help'):append(chat.message('/dt_type [P|M|B] --Determines the type of damage taken set to use. Physical is assumed.')));
		print(chat.header('Help'):append(chat.message('/kite --Equips defined movement set.')));
		print(chat.header('Help'):append(chat.message('/wswap --Toggles whether weapons will be swapped as needed. Default is FALSE to preserve TP.')));
		print(chat.header('Help'):append(chat.message('/eva --Toggles whether evasion set should be equipped or not. Default is FALSE.')));
		print(chat.header('Help'):append(chat.message('/acc --Toggle whether accuracy gear should override melee/weapon skill gear. Default is FALSE')));
		print(chat.header('Help'):append(chat.message('/gearset name [on]--Will equip the named gear set and then disable GSwap unless optional parameter set to ON')));
		print(chat.header('Help'):append(chat.message('/craftset [AL|BN|CL|CO|GS|LT|SM|WW] --Equips the specified crafting gear and turns GSwap off.')));
		print(chat.header('Help'):append(chat.message('/gatherset [HELM|DIG|CLAM] --Equips the specified gathering gear and turns GSwap off.')));
		print(chat.header('Help'):append(chat.message('/fishset --Equips the fishing set and turns off GSwap.')));
		print(chat.header('Help'):append(chat.message('/region --Toggles whether the area you\'re adventuring in is controlled by your nation or not.')));
		print(chat.header('Help'):append(chat.message('/maxspell name -- Determines the highest level spell your current jobs can cast that has the passed name')));
		print(chat.header('Help'):append(chat.message('/maxsong name [back] -- Determines the highest level song your current jobs can cast that has the passed name or next to highest')));
		print(chat.header('Help'):append(chat.message('/TH --Toggles on whether treasure hunter gear should be equipped. Default is FALSE.')));
		print(chat.header('Help'):append(chat.message('/help [command] --Display this listing or specific details on the specified command.')));
		print(chat.header('Help'):append(chat.message(' ')));
		print(chat.header('Help'):append(chat.message('Command(s) specific for BST:')));
		print(chat.header('Help'):append(chat.message('/petfood [name] --Equips the specified pet food or determines best food and equips it.')));
		print(chat.header('Help'):append(chat.message('/ajug -- Tiggles whether the automated jug pet system is enabled. Default is TRUE'))); 
		print(chat.header('Help'):append(chat.message(' ')));
		print(chat.header('Help'):append(chat.message('Some /lac commands of note:')));
		print(chat.header('Help'):append(chat.message('/lac disable --Disables all gear slots so that no automatic gear changes can occur.')));
		print(chat.header('Help'):append(chat.message('/lac enable --Enables all gear slots so automatic gear changes can occur.')));
		print(chat.header('Help'):append(chat.message('/lac load --Loads the Luashitacast BST definitions')));
		print(chat.header('Help'):append(chat.message('/lac unload --Unloads the Luashitacast BST definitions')));
		print(chat.header('Help'):append(chat.message('/lac reload --Unloads and reloads the Luashitacast BST definition')));
		print(chat.header('Help'):append(chat.message('/lac addset \"name\" --Saves the current equipped gear into Luashitacast\'s BST definition file. Don\'t include the \"\'s.')));
		print(chat.header('Help'):append(chat.message('/lac list --Lists all the defined gear sets from your BST definition.')));
		print(chat.header('Help'):append(chat.message(' ')));
		print(chat.header('Help'):append(chat.message('Please note that if you use style lock, you will not see the gear changing, but it is changing')))
	else
		local cmd = string.lower(args[2]);
		
		if cmd == 'gswap' then
			print(chat.header('Help'):append(chat.message('/gswap --This toggles whether automatic gear swapping is enabled or not. Default is TRUE')));
		elseif cmd == 'wsdistance' then
			print(chat.header('Help'):append(chat.message('wsdistance # --Non-ranged weapon skills require you to be no more than a certain distance from your target. By default, the maximum distance a tarutaru can hit a target is ' .. tostring(gcinclude.settings.WSdistance) ..' yalms. If your character\'s race is larger, you might want to change the distance to a larger number. This check prevents TP lost when you\'re too far away.')));
		elseif cmd == 'dt' then
			print(chat.header('Help'):append(chat.message('/dt --This toggles whether the damage taken gear set takes priority over the current gear set. Default is FALSE')));
		elseif cmd == 'dt_type' then
			print(chat.header('Help'):append(chat.message('/dt_type indicates the type of damage taken gear to use. P for physical, M for magical, and B for breath. Default is Physical')));
		elseif cmd == 'kite' then
			print(chat.header('Help'):append(chat.message('/kite --This toggles whether movement gear should be equipped. Default is FALSE')));
		elseif cmd == 'wswap' then
			print(chat.header('Help'):append(chat.message('/wswap --Toggles whether weapon swapping is permissible. Weapon swapping causes the loss of tp, but there are advantages too. Default is FALSE')));
		elseif cmd == 'gearset' then
			print(chat.header('Help'):append(chat.message('/gearset name [on]--This forcibly loads the indicated gear set and turns off GSwap unless optional parameter is on. Then GSwap remains enabled.')));
		elseif cmd == 'acc' then
			print(chat.header('Help'):append(chat.message('/acc --This toggles whether accuracy gear takes priority over normal melee gear. Casting and ranged accuracy are handled automatically. If Acc is true, then the accuracy set will be loaded over the TP set and the appropriate weaponskill set. Default is FALSE.')));
		elseif cmd == 'eva' then
			print(chat.header('Help'):append(chat.message('/eva --This toggles whether evasion gear takes priority over normal melee gear. If Eva is true, then the evasion set will be loaded over the TP set and the appropriate weaponskill set. Default is FALSE')));
		elseif cmd == 'craftset' then
			print(chat.header('Help'):append(chat.message('/craftset [AL,BN,CL,CO,GS,LT,SM,WW] equips specified crafting gear and turns GSwap off.')));
			print(chat.header('Help'):append(chat.message('AL - Alchemy, BN - Bonecraft, CL - Clothcraft, CO - Cooking, GS - Goldsmithing, LT - Leathercraft, SM - Smithing, and WW - Woodworking')));			
		elseif cmd == 'gatherset' then
			print(chat.header('Help'):append(chat.message('/gatherset [HELM,DIG,CLAM] --This equips specified gathering gear and turns GSwap off.')));
			print(chat.header('Help'):append(chat.message('HELM - harvest,excavation,logging,mining, DIG - digging, and CLAM - clamming')));
		elseif cmd == 'region' then
			print(chat.header('Help'):append(chat.message('/region --This indicates if the current area where you\'re playing is controlled by your nation. Default is TRUE')));
		elseif cmd == 'fishset' then
			print(chat.header('Help'):append(chat.message('/fishset --This command loads up your fishing gear and turns off GSwap.')));
		elseif cmd == 'maxspell' then
			print(chat.header('Help'):append(chat.message('/maxspell name --This determines the highest level spell that matches the name you indicated that your current job can cast.')));
		elseif cmd == 'maxsong' then
			print(chat.header('Help'):append(chat.message('/maxsong name [back] --This determines the highest level song that matches the name you indicated to cast or one of the max if asked for.')));			
		elseif cmd == 'th' then
			print(chat.header('Help'):append(chat.message('/TH --Toggles whether TH gear should be equipped or not. Default is FALSE.')));
		elseif cmd == 'help' then
			print(chat.header('Help'):append(chat.message('/help [[all]|command] --This command displays help for all Luashitacast commands or the specified command.')));
		elseif cmd == 'petfood' then
			print(chat.header('Help'):append(chat.message('/petfood [alpha|beta|gamma|delta|epsilon|zeta] --This command either equips the specified pet food in the ammo slot or determines what is the best pet food that can be equipped.')));
		elseif cmd == 'ajug' then
			print(chat.header('Help'):append(chat.message('/ajug --Toggles whether the automated jug pet system is enabled. This loads a jug pet if the ammo slot doesn\'t have a jug pet in it. Default is TRUE')));
		elseif cmd == 'lac' then
			print(chat.header('Help'):append(chat.message('/lac action ... --This command is native to Luashitacast and requires an action (ex: load, unload, list, etc.) and possibly further arguments. Further details are beyond what this help section can cover.')));
		else
			print(chat.header('Help'):append(chat.message('The command you specified either does not exist or is not supported for BST.')));
		end
	end
end
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
end

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
end

--[[
	HandleCommand is run when you type in a command defined in LUASHITACAST. The commands handled here instead
	of in gcinclude.HandleCommands are specific to BST or the help system, which has been tailored to BST.
--]]

profile.HandleCommand = function(args)
	if args[1] == 'help' then
		ShowCommands(args);
	elseif (args[1] == 'ajug') then			-- Turns on/off whether Automated Jug Pets supported
		gcdisplay.AdvanceToggle('AJug');
	elseif args[1] == 'petfood' then
		gcinclude.doPetFood(args[2],args[3]);
	else
		gcinclude.HandleCommands(args);
	end
end

--[[
	HandleDefault is run when some action happens. This includes both actions by the player and by
	their pet.
--]]
	
profile.HandleDefault = function()
	local pet = gData.GetPet();
	local petAction = gData.GetPetAction();
		
	if gcdisplay.GetToggle('GSwap') == true then		-- Only gear swap if this flag is true

		-- When you want to reward your pet and you do not have pet food equipped or when you 
		-- want to summon a pet and a jug is not equipped, the current item in the ammo slot 
		-- is saved. The following will set it back to what you had before either of those two 
		-- items were equipped.
		if profile.bAmmo then
			gFunc.ForceEquip('Ammo',profile.sAmmo);
			profile.sAmmo = nil;
			profile.bAmmo = false;
		end
		
		-- A pet action takes priority over a player's action.
		if (petAction ~= nil) then
			HandlePetAction(petAction);
			return;
		end
	
		local player = gData.GetPlayer();
		local ew = gData.GetEquipment();
		local eWeap = nil;

		if ew['Main'] ~= nil then
			eWeap = ew['Main'].Name;
		end;
		
		SetSubjobSet(player.SubJob);			-- Make sure the correct set is shown in case the subjob was changed.
	
		-- If player is not resting and has MP and has swapped weapons, set the weapon back to what 
		-- they had before the switch
		if player.Status ~= 'Resting' and gcdisplay.GetToggle('WSwap') == true then
			if gcinclude.weapon ~= nil and eWeap ~= gcinclude.weapon then
				gFunc.ForceEquip('Main', gcinclude.weapon);	
				gFunc.ForceEquip('Sub', gcinclude.offhand);	
			end
		end
		
		-- Now process the player status accordingly
		if player.Status == 'Engaged' then		-- Player is fighting. Priority (low to high): TP,evasion,accuracy
			gFunc.EquipSet(sets.TP);
			gcinclude.ProcessConditional(sets.TP_Conditional,nil);
			if gcdisplay.GetToggle('Eva') == true then
				gFunc.EquipSet(sets.TP_Evasion);
				gcinclude.ProcessConditional(sets.TP_Evasion_Conditional,nil);
			end
			if gcdisplay.GetToggle('Acc') == true then 
				gFunc.EquipSet(sets.TP_Accuracy);
				gcinclude.ProcessConditional(sets.TP_Accuracy_Conditional,nil);
			end
		elseif player.Status == 'Resting' then	-- Player kneeling. Priority (low to high): Resting,refresh
			gFunc.EquipSet(sets.Resting);
			gcinclude.ProcessConditional(sets.Resting_Conditional,nil);
			if (gcinclude.settings.bMagic and player.MPP < gcinclude.settings.RefreshGearMPP) then
				gFunc.EquipSet(sets.Resting_Refresh);
				gcinclude.ProcessConditional(sets.Resting_Refresh_Conditional,nil);
				-- Weapon swap to a higher MP refresh while healing weapon if appropriate.
				if gcdisplay.GetToggle('WSwap') == true and gcinclude.settings.bEleStaves == true then
					gcinclude.SwapToStave('dark',false);
				end
			end
		else									-- Assume idling. Priority (low to high): Idle,refresh
			gFunc.EquipSet(sets.Idle);
			gcinclude.ProcessConditional(sets.Idle_Conditional,nil);
			if player.HPP < gcinclude.settings.RegenGearHPP then		-- if the player's HP is below the threshold setting, equip the idle regen gear
				gFunc.EquipSet(sets.Idle_Regen);
			end
			if profile.bMagic == true then				-- Magic subjob
				if player.MPP < gcinclude.settings.RefreshGearMPP then		-- if the player's MP is below the threshold setting, equip the idle refresh gear
					gFunc.EquipSet(sets.Idle_Refresh);
				end
			end
		end
	
		-- If player has indicated kiting, load movement gear set
		if (gcdisplay.GetToggle('Kite') == true) then
			gFunc.EquipSet(sets.Movement);
			gcinclude.ProcessConditional(sets.Movement_Conditional,nil);
		end
		
		gcinclude.CheckDefault ();
		
		-- Add TH gear if indicated
		if (gcdisplay.GetToggle('TH') == true) then
			gFunc.EquipSet(sets.TH);
			gcinclude.ProcessConditional(sets.TH_Conditional,nil);
		end
			
		-- Lastly, equip the appriopriate Damage Taken gear if desired
		if (gcdisplay.GetToggle('DT') == true) then
			gFunc.EquipSet('DT_' .. gcdisplay.GetCycle('DT_Type'));
			if gcdisplay.GetCycle('DT_Type') == gcinclude.PHY then
				gcinclude.ProcessConditional(sets.DT_Physical_Conditional,nil);
			elseif gcdisplay.GetCycle('DT_Type') == gcinclude.MAG then
				gcinclude.ProcessConditional(sets.DT_Magical_Conditional,nil);
			else
				gcinclude.ProcessConditional(sets.DT_Breath_Conditional,nil);
			end
		end
	end
end

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
end

--[[
	HandleAbility is used to change the player's gear appropriately for the specified pet ability.
--]]

profile.HandleAbility = function()
	local ability = gData.GetAction();
	local eq = gData.GetEquipment();
	
	if eq.Ammo ~= nil then
		profile.sAmmo = eq.Ammo.Name;
	else
		profile.sAmmo = nil;
	end
	
	if gcdisplay.GetToggle('GSwap') == true then		-- Only gear swap if this flag is true
		if string.match(ability.Name, 'Call Beast') or string.match(ability.Name, 'Bestial Loyalty') then
			-- First make sure player wants the automated jug pet funtionality
			if gcdisplay.GetToggle('AJug') == true then
				-- Ok, now see if a jug pet already equipped
				local bJugFound = profile.bAmmoIsJug(profile.sAmmo);
				if bJugFound ~= nil and bJugFound == false then
					profile.bAmmo = profile.findMaxEquipableJugPet();
				end
			end
			gFunc.EquipSet(sets.Call_Beast);
			gcinclude.ProcessConditional(sets.Call_Beast_Conditional,nil);
		elseif string.match(ability.Name, 'Reward') then
			-- See if pet food already equipped
			if string.find(string.lower(profile.sAmmo),'pet f') == nil then		-- something else equipped
				if gcinclude.findMaxEquipablePetFood() == false then
					print(chat.header('HandleAbility'):append(chat.message('Error: Reward failed, no equipable pet food found')));
					return;
				else
					profile.bAmmo = true;
				end
			end
			gFunc.EquipSet(sets.Reward);
			gcinclude.ProcessConditional(sets.Reward_Conditional,nil);
		elseif string.match(ability.Name, 'Ready') or string.match(ability.Name, 'Sic') then
			gFunc.EquipSet(sets.PetReady);
			gcinclude.ProcessConditional(sets.PetReady_Conditional,nil);
		elseif string.match(ability.Name, 'Tame') then
			gFunc.EquipSet(sets.Tame);
			gcinclude.ProcessConditional(sets.Tame_Conditional,nil);
		elseif string.match(ability.Name, 'Charm') then
			gFunc.EquipSet(sets.Charm);
			gcinclude.ProcessConditional(sets.Charm_Conditional,nil);
		
			-- If evasion is wanted, override the appropriate gear
			if gcdisplay.GetToggle('eva') == true then
				gFunc.EquipSet(sets.Charm_Evasion);
				gcinclude.ProcessConditional(sets.Charm_Evasion_Conditional,nil);
			end
			
			-- If weapon swapping is allowed, equip a light/apollo staff (if you have one)
			if gcdisplay.GetToggle('WSwap') == true and gcinclude.settings.bEleStaves == true then
				gcinclude.SwapToStave('light',false);
			end
		end
		
		gcinclude.CheckCancels();
	end
end

--[[
	HandleItem is the place to equip gear when a special item is used. Currently only 'Holy Water' 
	is supported
--]]

profile.HandleItem = function()
	local item = gData.GetAction();

	if gcdisplay.GetToggle('GSwap') == true then		-- Only gear swap if this flag is true
		if string.match(item.Name, 'Holy Water') then gFunc.EquipSet(gcinclude.sets.Holy_Water) end
	end
end

--[[
	HandlePrecast loads Fast Cast, cast time reduction, and quick cast gear in anticipation of a spell
--]]

profile.HandlePrecast = function()
    local spell = gData.GetAction();
	local obi;
	local mSet;
	
	-- Special case if casting an utsusemi spell
	gcinclude.DoShadows(spell);
	
	-- Now, normal process
	if gcdisplay.GetToggle('GSwap') == true then		-- Only gear swap if this flag is true
		gFunc.EquipSet(sets.Precast);
		gcinclude.ProcessConditional(sets.Precast_Conditional,nil);
		
		-- See if an elemental obi should be equipped
		if gcinclude.settings.bEleObis == true then
			obi = gcinclude.CheckEleSpells(spell.Name,gcinclude.MagicEleAcc,gcinclude.OBI,nil);
			if obi ~= nil then
				gFunc.ForceEquip('Waist',obi);
			end
		end
		gcinclude.CheckCancels();
	end
end

--[[
	HandleMidcast loads the appropriate gear for magic skill, duration, mab, macc, and potency
	
	There's an order to how the pieces are loaded: INT/MND, spell specific, macc, magic skill, obi, ele swap
	
--]]

profile.HandleMidcast = function()
	local player = gData.GetPlayer();
	local spell = gData.GetAction();
	local obi;
	local sSet;

	if gcdisplay.GetToggle('GSwap') == true then		-- Only gear swap if this flag is true	
		-- First load the midcast set
		gFunc.EquipSet(sets.Midcast);
		gcinclude.ProcessConditional(sets.Midcast_Conditional,nil);
		
		-- Then, see if INT/MND gear should be loaded
		sSet = gcinclude.WhichStat(spell.Name);
		if sSet ~= nil then
			if sSet == 'MND' then
				gFunc.EquipSet(sets.MND);
				gcinclude.ProcessConditional(sets.MND_Conditional,nil);
			elseif sSet == 'INT' then
				gfunc.EquipSet(sets.INT);
				gcinclude.ProcessConditional(sets.INT_Conditional,nil);
			end
		end
		
		-- Then check spell specific gear
		if string.match(spell.Name, 'Phalanx') then
			gFunc.EquipSet(sets.Phalanx);
			gcinclude.ProcessConditional(sets.Phalanx_Conditional,nil);
		elseif string.match(spell.Name, 'Stoneskin') then
			gFunc.EquipSet(sets.Stoneskin);
			gcinclude.ProcessConditional(sets.Stoneskin_Conditional,nil);
			-- Stoneskin is heavily affected by MND, but it's an enhancing spell, so
			-- MND gear wasn't loaded above. Additionally, MND is more potent than
			-- Enhancing skill. Load that here.
			gFunc.EquipSet(sets.MND);
			gcinclude.ProcessConditional(sets.MND_Conditional,nil);
		elseif string.contains(spell.Name, 'Refresh') then
			gFunc.EquipSet(sets.Refresh);
			gcinclude.ProcessConditional(sets.Refresh_Conditional,nil);
		end

		-- Then magical accuracy
		if gcdisplay.GetToggle('acc') == true then
			gFunc.EquipSet(sets.macc);
			gcinclude.ProcessConditional(sets.macc_Conditional,nil);
		end
		
		-- Then the appropriate magic skill
		mSet = gcinclude.WhichMagicSkill(spell.Name);

		if mSet ~= nil then
			gFunc.EquipSet(mSet);
			if mSet == 'Cure' then
				gcinclude.ProcessConditional(sets.Cure_Conditional,nil);
			elseif mSet == 'Dark' then
				gcinclude.ProcessConditional(sets.Dark_Conditional,nil);
			elseif mSet == 'Divine' then
				gcinclude.ProcessConditional(sets.Divine_Conditional,nil);
			elseif mSet == 'Enfeebling' then
				gcinclude.ProcessConditional(sets.Enfeebling_Conditional,nil);
			elseif mSet == 'Enhancing' then
				gcinclude.ProcessConditional(sets.Enhancing_Conditional,nil);
			elseif mSet == 'Elemental' then
				gcinclude.ProcessConditional(sets.Elemental_Conditional,nil);
			elseif mSet == 'Ninjitsu' then
				gcinclude.ProcessConditional(sets.Ninjitsu_Conditional,nil);
			elseif mSet == 'Summoning' then
				gcinclude.ProcessConditional(sets.Summoning_Conditional,nil);
			end				
		end

--[[		
		Then, regardless of type of spell, see if an obi would help. No need to check and see if the 
		player has the obi or not, if they do, it equips. If not, nothing happens.
		
		Note: This seems like a repeat of the obi check in the precast, but in this case it's checking
		for the spell damage type rather than the spell accuracy.
--]]
		if gcinclude.settings.bEleObis == true then
			obi = gcinclude.CheckEleSpells(spell.Name,gcinclude.MagicEleDmg,gcinclude.OBI);
			if obi ~= nil then
				gFunc.ForceEquip('Waist',obi);
			end
		end
		
		stat = nil;
		-- Lastly, how about an elemental stave (use the MagicEleDmg in gcinclude) or summons
		if gcdisplay.GetToggle('WSwap') == true and gcinclude.settings.bEleStaves == true then
			if mSet == 'Summoning' then
				stat = gcinclude.CheckSummons(spell.Name);
			else
				stat = gcinclude.CheckEleSpells(spell.Name,gcinclude.MagicEleDmg,gcinclude.ELEMENT);
			end
		end
		
		if stat ~= nil then
			gcinclude.SwapToStave(stat,false);
		end
	end
end

--[[
	HandlePreshot loads Ranged Accuracy and Ranged Shot Speed Gear for a ranged attack
--]]

profile.HandlePreshot = function()
	if gcdisplay.GetToggle('GSwap') == true then		-- Only gear swap if this flag is true
		gFunc.EquipSet(sets.Preshot);
		gcinclude.ProcessConditional(sets.Preshot_Conditional,nil);
	end
end

--[[
	HandleMidshot loads Ranged Attack and Damage gear for a ranged attack
--]]

profile.HandleMidshot = function()
	if gcdisplay.GetToggle('GSwap') == true then		-- Only gear swap if this flag is true
		gFunc.EquipSet(sets.Midshot);
		gcinclude.ProcessConditional(sets.Midshot_Conditional,nil);
	end
end

--[[
	HandleWeaponskill loads the gear appropriately for the weapon skill you're doing
--]]

profile.HandleWeaponskill = function()
	if gcdisplay.GetToggle('GSwap') == true then		-- Only gear swap if this flag is true
		local ws = gData.GetAction();
		local canWS = gcinclude.CheckWsBailout();
 
		-- If conditions would cause the weaponskill to fail, the action will be
		-- cancelled so you do not lose your tp.
		if (canWS == false) then 
			gFunc.CancelAction();
			return;
		else
			local sWS = gcinclude.WsStat(ws.Name,'STR');	-- Equip appropriate gear for weapon skill
			gFunc.EquipSet(sWS);
			
			if sWS == 'WS_CHR' then
				gcinclude.ProcessConditional(sets.WS_CHR_Conditional,nil);
			elseif sWS == 'WS_DEX' then
				gcinclude.ProcessConditional(sets.WS_DEX_Conditional,nil);
			elseif sWS == 'WS_DEXINT' then
				gcinclude.ProcessConditional(sets.WS_DEXINT_Conditional,nil);
			elseif sWS == 'WS_INTAGI' then
				gcinclude.ProcessConditional(sets.WS_INTAGI_Conditional,nil);
			elseif sWS == 'WS_STR' then
				gcinclude.ProcessConditional(sets.WS_STR_Conditional,nil);
			elseif sWS == 'WS_MND' then
				gcinclude.ProcessConditional(sets.WS_MND_Conditional,nil);
			elseif sWS == 'WS_STRDEX' then
				gcinclude.ProcessConditional(sets.WS_STRDEX_Conditional,nil);
			elseif sWS == 'WS_STRMND' then
				gcinclude.ProcessConditional(sets.WS_STRMND_Conditional,nil);
			elseif sWS == 'WS_STRINT' then
				gcinclude.ProcessConditional(sets.WS_STRINT_Conditional,nil);
			elseif sWS == 'WS_STRINT_30_20' then
				gcinclude.ProcessConditional(sets.WS_STRINT_30_20_Conditional,nil);
			elseif sWS == 'WS_STRVIT' then
				gcinclude.ProcessConditional(sets.WS_STRVIT_Conditional,nil);
			end
			
			-- See if an elemental gorget makes sense to equip
			if gcinclude.settings.bEleGorgets == true then
				local sGorget = gcinclude.CheckEleGorget(ws.Name);
				if sGorget ~= nil then
					gFunc.ForceEquip('Neck',sGorget);
				end
			end
		end
	end
end

return profile;