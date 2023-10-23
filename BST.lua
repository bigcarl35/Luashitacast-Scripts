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
	in the "Conditional gear master list.txt" file which is found in the /common/ directory. Just copy and paste
	the line item you want in the appropriate conditional area. (Please note that if you have more than one item to
	be conditionally considered, you will have to add a comma after each entry.)
--]]

--[[
	The "Idle" set is what your character will wear when it is not fighting nor resting nor in town. This includes
	your pet. Whether just standing out of town or going to a different area, the "Idle" set will be equipped.
--]]

	['Idle'] = {
        Head = 'Panther mask',
        Neck = 'Ryl.Grd. Collar',
        Ear1 = 'Coral Earring',
        Ear2 = 'Beastly Earring',
        Body = 'Narasimha\'s Vest',
        Hands = 'Thick Mufflers',
        Ring1 = 'Sun Ring',
        Ring2 = 'Sun Ring',
        Back = 'Psilos Mantle',
        Waist = 'Life Belt',
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
        Head = 'Optical Hat',
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
	},
	['Preshot_Conditional'] = {
	},
	
--[[
	Midshot is the second stage of a ranged shot. This is where you place Ranged Attack or Ranged 
	Damage gear
--]]

	['Midshot'] = {
	},
	['Midshot_Conditional'] = {
	},

--[[
	Spells are a bit different. Each type of spell can have it's own enhancement gear as well as 
	stat based gear. (In some cases individual spells have special entries.) These sets do not 
	include elemental gear which is	dependent on day/weather/weapon skill.

	The first stage is Precast. This is where you place any Fast Cast, cast time reduction, and 
	quick cast gear 
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

	['Cure'] = {				-- Healing Magic Skill
    },
	['Cure_Conditional'] = {
	},
	
	['Dark'] = {				-- Dark Magic Skill
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
	
	['Enhancing'] = {	 		-- Enhancing Magic Skill
	},
	['Enhancing_Conditional'] = {
	},
	
	['Elemental'] = {			-- Elemental Magic Skill
	},
	['Elemental_Conditional'] = {
	},
	
	['Ninjitsu'] = {			-- Ninjitsu Skill
	},
	['Ninjitsu_Conditional'] = {
	},
	
	['Summoning'] = {			-- Summoning Skill, any boost to pet or summoning ability
	},
	['Summoning_Conditional'] = {
	},
	
--[[
	Next is stat-based gear, (in this case intelligence or mind)
--]]

	['INT'] = {
		Ring2 = 'Tamas Ring',			-- +2~5 INT
	},
	['INT_Conditional'] = {
	},
	
	['MND'] = {
        Head = 'Beast helm',
        Neck = 'Justice Badge',			-- +3 MND
        Body = 'Wonder Kaftan',			-- +1 MND
        Ring1 = 'Tranquility Ring',		-- +2 MND
        Ring2 = 'Tamas Ring',			-- +2~5 MND
        Waist = 'Friar\'s Rope',		-- +1 MND
        Legs = 'Wonder Braccae',		-- +2 MND
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
	['Phalanx'] = {
	},
	['Phalanx_Conditional'] = {
	},
	
	['Stoneskin'] = {	-- Combination of MND and enhancing skill. MND is 3x more important. There's also gear that enhances
	},	
	['Stoneskin_Conditional'] = {
	},
	
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
		Head = 'Mercenary Captain\'s headgear',		-- +1 STR
		Neck = 'Spike necklace',					-- +3 STR
		Body = 'Narasimha\'s vest',					-- +3 STR
		Hands = 'Ogre gloves',						-- +6 STR
		Ring1 = 'Sun ring',							-- +3 STR
		Ring2 = 'Sun ring',							-- +3 STR
		Waist = 'Barbarian\'s belt',				-- +1 STR
		Legs = 'Wonder braccae',					-- +1 STR
		Feet = 'Creek F clomps',					-- +4 STR
	},
	['WS_STR_Conditional'] = {
	},
	
--[[
		* Strength and Dexterity based, even weighting *
		Sword: Fast Blade
--]]

	['WS_STRDEX'] = {
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
	
	['WS_B'] = {
		Head = 'Beast helm',					-- +3 MND
		Neck = 'Justice badge',					-- +3 MND
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
		Head = 'Mercenary Captain\'s headgear',	-- +1 DEX
		Neck = 'Spike necklace',				-- +3 DEX
		Body = 'Brigandine armor',				-- +2 DEX
		Hands = 'Beast gloves',					-- +3 DEX
		Ring1 = 'Balance ring',					-- +2 DEX
	},
	['WS_DEX_Conditional'] = {
	},

--[[
		* Dexterity and Intelligence based *
		Dagger: Gust Slash,Cyclone^

		^ Subjob must be one of: RDM,THF,BRD,RNG,NIN
--]]
	
	['WS_DEXINT'] = {
		Head = 'Beast helm',					-- +5 INT
		Neck = 'Spike necklace',				-- +3 DEX
		Body = 'Brigandine armor',				-- +2 DEX
		Hands = 'Beast gloves',					-- +3 DEX
		Ring1 = 'Balance ring',					-- +2 DEX
	},
	['WS_DEXINT_Conditional'] = {
	},

--[[
		* Intellegence and Agility based, even weighting *
		Scythe: Catastrophe
--]]
	
	['WS_INTAGI'] = {
		Head = 'Beast helm',					-- +5 INT
		Ear1 = 'Drone earring',					-- +3 AGI
		Body = 'Mercenary Captain\'s doublet',	-- +1 AGI
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
	},
	['WS_MND_Conditional'] = {
	},
	
--[[
	Weapon skill sets don't consider accuracy in their definitions. If an accuracy emphasis is
	desired, the following set will replace the gear from the appropriate weapon skill set.
	(Please note: only include accuracy gear here so that the weapon skill set has some emphasis
	on the appropriate gear set loaded.
--]]

	['WS_Accuracy'] = {	-- Gear with weaponskill accuracy
	},	
	['WS_Accuracy_Conditional'] = {
	},
	
--[[
	The following sets are used with pet abilities/pet commands
--]]
	
	['Call_Beast'] = {			-- or bestial loyalty
	},
	['Call_Beast_Conditional'] = {
	},
	
	['Reward'] = {
        Ammo = 'Pet Food Zeta',
        Head = 'Beast Helm',
        Neck = 'Justice Badge',
        Body = 'Beast Jackcoat',
        Hands = 'Ogre Gloves',
        Ring1 = 'Tamas Ring',
        Ring2 = 'Tranquility Ring',
        Waist = 'Friar\'s Rope',
        Legs = 'Wonder Braccae',
        Feet = 'Beast Gaiters',
    },
	['Reward_Conditional'] = {
	},
	
	['Tame'] = {
        Head = 'Beast Helm',
    },
	['Tame_Conditional'] = {
	},
	
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
	
	['PetReady'] = {	-- Applies to both Sic and Ready. Consider pet accuracy, similar abilities
	},
	['PetReady_Conditional'] = {
	},
	
	['PetAttack'] = {	-- Fight command
	},
	['PetAttack_Conditional'] = {
	},

--[[
	Movement tends to be used for kiting. Emphasis should be placed on gear that increases movement speed, but you 
	might also want gear that has evasion. The choice is yours.
--]]

	['Movement'] = { 
	},
	['Movement_Conditional'] = {
	},
	
--[[
	The following sets are added as a convenience for playing in level capped areas. The only way for them to be loaded
	is via the /gearset command, which will turn GSwap off. If you're level syncing, pick the set that's closest to the
	sync level and adjust accordingly. 
--]]

	['CAP20'] = {
        Main = 'Freesword\'s Staff',
        Ammo = 'Fortune Egg',
        Head = 'Garrison Sallet',
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
        Feet = 'Lizard Ledelsens',
    },
	
	['CAP25'] = {
        Main = 'Freesword\'s Staff',
        Ammo = 'S. Herbal Broth',
        Head = 'Garrison Sallet',
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
        Feet = 'Lizard Ledelsens',
    },
	
	['CAP30'] = {
        Main = 'Frostreaper',
        Ammo = 'S. Herbal Broth',
        Head = 'Mrc.Cpt. Headgear',
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
        Head = 'Mrc.Cpt. Headgear',
        Neck = 'Spike Necklace',
        Ear1 = 'Beetle Earring',
        Ear2 = 'Beetle Earring',
        Body = 'Wonder Kaftan',
        Hands = 'Wonder Mitts',
        Ring1 = 'Courage Ring',
        Ring2 = 'Tamas Ring',
        Back = 'Lizard Mantle',
        Waist = 'Barbarian\'s Belt',
        Legs = 'Wonder Braccae',
        Feet = 'Wonder Clomps',
    },
	
	['CAP50'] = {
        Main = 'Barbaroi Axe',
        Sub = 'Fish Scale Shield',
        Ammo = 'S. Herbal Broth',
        Head = 'Mrc.Cpt. Headgear',
        Neck = 'Spike Necklace',
        Ear1 = 'Beetle Earring',
        Ear2 = 'Beetle Earring',
        Body = 'Gaudy Harness',
        Hands = 'Wonder Mitts',
        Ring1 = 'Courage Ring',
        Ring2 = 'Tamas Ring',
        Back = 'Lizard Mantle',
        Waist = 'Life Belt',
        Legs = 'Wonder Braccae',
        Feet = 'Wonder Clomps',
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
        Waist = 'Life Belt',
        Legs = 'Beast Trousers',
        Feet = 'Beast Gaiters',
    },
};

profile.Sets = sets;
profile.sjb = nil;
profile.bReward = false;
profile.sAmmo;

--[[
	HandlePetAction equips the appropriate gear set based on the type of action
	the pet is trying to perform.
--]]

local function HandlePetAction(PetAction)
	if gcdisplay.GetToggle('GSwap') == true then		-- Only gear swap if this flag is true
		gFunc.EquipSet(sets.PetReady);

		if (gcinclude.BstPetAttack:contains(PetAction.Name)) then
			gFunc.EquipSet(sets.PetAttack);
			gcinclude.ProcessConditional(sets.PetAttack_Conditional);	
		elseif (gcinclude.BstPetMagicAttack:contains(PetAction.Name)) then
			gFunc.EquipSet(sets.PetMagicAttack);
			gcinclude.ProcessConditional(sets.PetMagicAttack_Conditional);			
		elseif (gcinclude.BstPetMagicAccuracy:contains(PetAction.Name)) then
			gFunc.EquipSet(sets.PetMagicAccuracy);
			gcinclude.ProcessConditional(sets.PetMagicAccuracy_Conditional);			
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
	if gcdisplay.GetToggle('GSwap') == true then		-- Only gear swap if this flag is true
		gFunc.EquipSet(sets.Start_Weapons);	
		gcinclude.ProcessConditional(sets.Start_Weapon_Conditional);
	end
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
		print(chat.header('Help'):append(chat.message('/gearset name --Will equip the named gear set and then disable GSwap.')));
		print(chat.header('Help'):append(chat.message('/craft_type [AL|BN|CL|CO|GS|LT|SM|WW] --Sets the type of crafting set to load')));
		print(chat.header('Help'):append(chat.message('/craft [type] --Equips the selected crafting set or the passed type and turns GSwap off.')));
		print(chat.header('Help'):append(chat.message('/gather_type [HELM|DIG|CLAM] --Sets the type of gathering gear set to load')));
		print(chat.header('Help'):append(chat.message('/gather [type] --Equips the selected gathering set or passed type and turns GSwap off.')));
		print(chat.header('Help'):append(chat.message('/fishset --Equips the fishing set and turns off GSwap.')));
		print(chat.header('Help'):append(chat.message('/region --Toggles whether the area you\'re adventuring in is controlled by your nation or not.')));
		print(chat.header('Help'):append(chat.message('/maxspell name -- Determines the highest level spell your current jobs can cast that has the passed name')));
		print(chat.header('Help'):append(chat.message('/maxsong name [back] -- Determines the highest level song your current jobs can cast that has the passed name or next to highest')));
		print(chat.header('Help'):append(chat.message('/help [command] --Display this listing or specific details on the specified command.')));
		print(chat.header('Help'):append(chat.message(' ')));
		print(chat.header('Help'):append(chat.message('Command(s) specific for BST:')));
		print(chat.header('Help'):append(chat.message('/petfood [name] --Equips the specified pet food or determines best food and equips it.')));
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
			print(chat.header('Help'):append(chat.message('/gearset name --This forcibly loads the indicated gear set and turns off GSwap.')));
		elseif cmd == 'acc' then
			print(chat.header('Help'):append(chat.message('/acc --This toggles whether accuracy gear takes priority over normal melee gear. Casting and ranged accuracy are handled automatically. If Acc is true, then the accuracy set will be loaded over the TP set and the appropriate weaponskill set. Default is FALSE.')));
		elseif cmd == 'eva' then
			print(chat.header('Help'):append(chat.message('/eva --This toggles whether evasion gear takes priority over normal melee gear. If Eva is true, then the evasion set will be loaded over the TP set and the appropriate weaponskill set. Default is FALSE')));
		elseif cmd == 'craft_type' then
			print(chat.header('Help'):append(chat.message('/craft_type [AL|BN|CL|CO|GS|LT|SM|WW] --This command sets the type of crafting gear that will be loaded when /craft specified. Valid types are: AL (Alchemy), BN (Bonecraft), CL (Clothcraft), CO (Cooking), GS (Gold Smithing), LT (Leatherwork), SM (Smithing), and WW (Woodworking). No type specified just increments the type to the next available setting')));
		elseif cmd == 'craft' then
			print(chat.header('Help'):append(chat.message('/craft [type] equips the crafting gear specied in the craft_type or the passed type and turns GSwap off.')));
		elseif cmd == 'gather_type' then
			print(chat.header('Help'):append(chat.message('/gather_type [HELM|DIG|CLAM] --This command sets the type of gathering gear that will be loaded when /gather specified. HELM includes harvesting, excavation, logging, and mining, DIG is digging and CLAM is clamming. No type specified just increments the type to the next available setting')));
		elseif cmd == 'gather' then
			print(chat.header('Help'):append(chat.message('/gather [type] --This equips gather_type\'s setting or the specified gathering gear and turns GSwap off.')));
		elseif cmd == 'region' then
			print(chat.header('Help'):append(chat.message('/region --This indicates if the current area where you\'re playing is controlled by your nation. Default is TRUE')));
		elseif cmd == 'fishset' then
			print(chat.header('Help'):append(chat.message('/fishset --This command loads up your fishing gear and turns off GSwap.')));
		elseif cmd == 'maxspell' then
			print(chat.header('Help'):append(chat.message('maxspell name --This determines the highest level spell that matches the name you indicated that your current job can cast.')));
		elseif cmd == 'maxsong' then
			print(chat.header('Help'):append(chat.message('maxsong name [back] --This determines the highest level song that matches the name you indicated to cast or one of the max if asked for.')));			
		elseif cmd == 'help' then
			print(chat.header('Help'):append(chat.message('/help [[all]|command] --This command displays help for all Luashitacast commands or the specified command.')));
		elseif cmd == 'petfood' then
			print(chat.header('Help'):append(chat.message('/petfood [alpha|beta|gamma|delta|epsilon|zeta] --This command either equips the specified pet food in the ammo slot or determines what is the best pet food that can be equipped.')));
		elseif cmd == 'lac' then
			print(chat.header('Help'):append(chat.message('/lac action ... --This command is native to Luashitacast and requires an action (ex: load, unload, list, etc.) and possibly further arguments. Further details are beyond what this help section can cover.')));
		else
			print(chat.header('Help'):append(chat.message('The command you specified either does not exist or is not supported for BST.')));
		end
	end
end

--[[
	HandleCommand is run when you type in a command defined in LUASHITACAST. The commands handled here instead
	of in gcinclude.HandleCommands are specific to BST or the help system, which has been tailored to BST.
--]]

profile.HandleCommand = function(args)
	if args[1] == 'help' then
		ShowCommands(args);
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

		-- When you want to reward your pet the current item in the ammo slot is saved.
		-- The following will set it back to what you had before the pet food was equipped.
		if profile.bReward then
			gFunc.ForceEquip('Ammo',profile.sAmmo);
			profile.sAmmo = nil;
			profile.bReward = false;
		end
		
		-- A pet action takes priority over a player's action. Sorry.
		if (petAction ~= nil) then
			HandlePetAction(petAction);
			return;
		end
	
		local player = gData.GetPlayer();
		local eWeap = gData.GetEquipSlot('Main');
		
		SetSubjobSet(player.SubJob);			-- Make sure the correct set is shown in case the subjob was changed.
	
		-- If player is not resting and has MP and has swapped weapons, set the weapon back to what 
		-- they had before the switch
		if player.Status ~= 'Resting' and gcdisplay.GetToggle('WSwap') == true then
			if eWeap ~= gcinclude.weapon then
				gFunc.ForceEquip('Main', gcinclude.weapon);	
				gFunc.ForceEquip('Sub', gcinclude.offhand);	
			end
		end
		
		-- Now process the player status accordingly
		if player.Status == 'Engaged' then		-- Player is fighting. Priority (low to high): TP,evasion,accuracy
			gFunc.EquipSet(sets.TP);
			gcinclude.ProcessConditional(sets.TP_Conditional);
			if gcdisplay.GetToggle('Eva') == true then
				gFunc.EquipSet(sets.TP_Evasion);
				gcinclude.ProcessConditional(sets.TP_Evasion_Conditional);
			end
			if gcdisplay.GetToggle('Acc') == true then 
				gFunc.EquipSet(sets.TP_Accuracy);
				gcinclude.ProcessConditional(sets.TP_Accuracy_Conditional);
			end
		elseif player.Status == 'Resting' then	-- Player kneeling. Priority (low to high): Resting,refresh
			gFunc.EquipSet(sets.Resting);
			gcinclude.ProcessConditional(sets.Resting_Conditional);
			if (gcinclude.settings.bMagic and player.MPP < gcinclude.settings.RefreshGearMPP) then
				gFunc.EquipSet(sets.Resting_Refresh);
				gcinclude.ProcessConditional(sets.Resting_Refresh_Conditional);
				-- Weapon swap to a higher MP refresh while healing weapon if appropriate.
				gcinclude.SwapToStave('dark',false);
			end
		else									-- Assume idling. Priority (low to high): Idle,refresh
			gFunc.EquipSet(sets.Idle);
			gcinclude.ProcessConditional(sets.Idle_Conditional);
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
			gcinclude.ProcessConditional(sets.Movement_Conditional);
		end
		
		gcinclude.CheckDefault ();
		-- Lastly, equip the appriopriate Damage Taken gear if desired
		if (gcdisplay.GetToggle('DT') == true) then
			gFunc.EquipSet('DT_' .. gcdisplay.GetCycle('DT_Type'));
			if gcdisplay.GetCycle('DT_Type') == gcinclude.PHY then
				gcinclude.ProcessConditional(sets.DT_Physical_Conditional);
			elseif gcdisplay.GetCycle('DT_Type') == gcinclude.MAG then
				gcinclude.ProcessConditional(sets.DT_Magical_Conditional);
			else
				gcinclude.ProcessConditional(sets.DT_Breath_Conditional);
			end
		end
	end
end

--[[
	HandleAbility is used to change the player's gear appropriately for the specified pet ability.
--]]

profile.HandleAbility = function()
	local ability = gData.GetAction();

	if gcdisplay.GetToggle('GSwap') == true then		-- Only gear swap if this flag is true
		if string.match(ability.Name, 'Call Beast') or string.match(ability.Name, 'Bestial Loyalty') then
			gFunc.EquipSet(sets.Call_Beast);
			gcinclude.ProcessConditional(sets.Call_Beast_Conditional);
		elseif string.match(ability.Name, 'Reward') then
			-- Save what's in the ammo slot before changing to the pet food
			profile.bReward = true;
			profile.sAmmo = gData.GetEquipSlot('Ammo');
			gFunc.EquipSet(sets.Reward);
			gcinclude.ProcessConditional(sets.Reward_Conditional);
		elseif string.match(ability.Name, 'Ready') or string.match(ability.Name, 'Sic') then
			gFunc.EquipSet(sets.PetReady);
			gcinclude.ProcessConditional(sets.PetReady_Conditional);
		elseif string.match(ability.Name, 'Tame') then
			gFunc.EquipSet(sets.Tame);
			gcinclude.ProcessConditional(sets.Tame_Conditional);
		elseif string.match(ability.Name, 'Charm') then
			gFunc.EquipSet(sets.Charm);
			gcinclude.ProcessConditional(sets.Charm_Conditional);
		
			-- If evasion is wanted, override the appropriate gear
			if gcdisplay.GetToggle('eva') == true then
				gFunc.EquipSet(sets.Charm_Evasion);
				gcinclude.ProcessConditional(sets.Charm_Evasion_Conditional);
			end
			
			-- If weapon swapping is allowed, equip a light/apollo staff (if you have one)
			if gcdisplay.GetToggle('WSwap') == true then
				gcinclude.SwapToStave('light',false);
			end
		end
		
		gcinclude.CheckCancels();
	end
end

--[[
	HandleItem is the place for gear a special item is used. Currently only 'Holy Water' is supported
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
		gcinclude.ProcessConditional(sets.Precast_Conditional);
		
		-- See if an elemental obi should be equipped
		obi = gcinclude.CheckEleSpells(spell.Name,gcinclude.MagicEleAcc,gcinclude.OBI,nil);
		if obi ~= nil then
			gFunc.ForceEquip('Waist',obi);
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
		gcinclude.ProcessConditional(sets.Midcast_Conditional);
		
		-- Then, see if INT/MND gear should be loaded
		sSet = gcinclude.WhichStat(spell.Name);
		if sSet ~= nil then
			if sSet == 'MND' then
				gFunc.EquipSet(sets.MND);
				gcinclude.ProcessConditional(sets.MND_Conditional);
			elseif sSet == 'INT' then
				gfunc.EquipSet(sets.INT);
				gcinclude.ProcessConditional(sets.INT_Conditional);
			end
		end
		
		-- Then check spell specific gear
		if string.match(spell.Name, 'Phalanx') then
			gFunc.EquipSet(sets.Phalanx);
			gcinclude.ProcessConditional(sets.Phalanx_Conditional);
		elseif string.match(spell.Name, 'Stoneskin') then
			gFunc.EquipSet(sets.Stoneskin);
			gcinclude.ProcessConditional(sets.Stoneskin_Conditional);
		elseif string.contains(spell.Name, 'Refresh') then
			gFunc.EquipSet(sets.Refresh);
			gcinclude.ProcessConditional(sets.Refresh_Conditional);
		end

		-- Then magical accuracy
		if gcdisplay.GetToggle('acc') == true then
			gFunc.EquipSet(sets.macc);
			gcinclude.ProcessConditional(sets.macc_Conditional);
		end
		
		-- Then the appropriate magic skill
		mSet = gcinclude.WhichMagicSkill(spell.Name);

		if mSet ~= nil then
			gFunc.EquipSet(mSet);
			if mSet == 'Cure' then
				gcinclude.ProcessConditional(sets.Cure_Conditional);
			elseif mSet == 'Dark' then
				gcinclude.ProcessConditional(sets.Dark_Conditional);
			elseif mSet == 'Divine' then
				gcinclude.ProcessConditional(sets.Divine_Conditional);
			elseif mSet == 'Enfeebling' then
				gcinclude.ProcessConditional(sets.Enfeebling_Conditional);
			elseif mSet == 'Enhancing' then
				gcinclude.ProcessConditional(sets.Enhancing_Conditional);
			elseif mSet == 'Elemental' then
				gcinclude.ProcessConditional(sets.Elemental_Conditional);
			elseif mSet == 'Ninjitsu' then
				gcinclude.ProcessConditional(sets.Ninjitsu_Conditional);
			elseif mSet == 'Summoning' then
				gcinclude.ProcessConditional(sets.Summoning_Conditional);
			end				
		end

--[[		
		Then, regardless of type of spell, see if an obi would help. No need to check and see if the 
		player has the obi or not, if they do, it equips. If not, nothing happens.
		
		Note: This seems like a repeat of the obi check in the precast, but in this case it's checking
		for the spell damage type rather than the spell accuracy.
--]]

		obi = gcinclude.CheckEleSpells(spell.Name,gcinclude.MagicEleDmg,gcinclude.OBI);
		if obi ~= nil then
			gFunc.ForceEquip('Waist',obi);
		end

		stat = nil;
		-- Lastly, how about an elemental stave (use the MagicEleDmg in gcinclude) or summons
		if gcdisplay.GetToggle('WSwap') == true then
			if mSet == 'Summoning' then
				stat = gcinclude.CheckSummons(spell.Name);
			else
				stat = gcinclude.CheckEleSpells(spell.Name,gcinclude.MagicEleDmg,gcinclude.ELEMENT);
			end
		end
		
		if stat ~= nil then
			gcinclude.SwapToStave(stat,false,mSet);
		end
	end
end

--[[
	HandlePreshot loads Ranged Accuracy and Ranged Shot Speed Gear for a ranged attack
--]]

profile.HandlePreshot = function()
	if gcdisplay.GetToggle('GSwap') == true then		-- Only gear swap if this flag is true
		gFunc.EquipSet(sets.Preshot);
		gcinclude.ProcessConditional(sets.Preshot_Conditional);
	end
end

--[[
	HandleMidshot loads Ranged Attack and Damage gear for a ranged attack
--]]

profile.HandleMidshot = function()
	if gcdisplay.GetToggle('GSwap') == true then		-- Only gear swap if this flag is true
		gFunc.EquipSet(sets.Midshot);
		gcinclude.ProcessConditional(sets.Midshot_Conditional);
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
				gcinclude.ProcessConditional(sets.WS_CHR_Conditional);
			elseif sWS == 'WS_DEX' then
				gcinclude.ProcessConditional(sets.WS_DEX_Conditional);
			elseif sWS == 'WS_DEXINT' then
				gcinclude.ProcessConditional(sets.WS_DEXINT_Conditional);
			elseif sWS == 'WS_INTAGI' then
				gcinclude.ProcessConditional(sets.WS_INTAGI_Conditional);
			elseif sWS == 'WS_STR' then
				gcinclude.ProcessConditional(sets.WS_STR_Conditional);
			elseif sWS == 'WS_MND' then
				gcinclude.ProcessConditional(sets.WS_MND_Conditional);
			elseif sWS == 'WS_STRDEX' then
				gcinclude.ProcessConditional(sets.WS_STRDEX_Conditional);
			elseif sWS == 'WS_STRMND' then
				gcinclude.ProcessConditional(sets.WS_STRMND_Conditional);
			elseif sWS == 'WS_STRINT' then
				gcinclude.ProcessConditional(sets.WS_STRINT_Conditional);
			elseif sWS == 'WS_STRINT_30_20' then
				gcinclude.ProcessConditional(sets.WS_STRINT_30_20_Conditional);
			elseif sWS == 'WS_STRVIT' then
				gcinclude.ProcessConditional(sets.WS_STRVIT_Conditional);
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
