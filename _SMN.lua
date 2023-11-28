local profile = {};
gcinclude = gFunc.LoadFile('common\\gcinclude.lua');

--[[
	This file contains all the gear sets associated with the SMN job. While it isn't outside of the realm of 
	possibility that the subjob might be able to use gear sets too, that is not the emphasis of this program. 
	It is tailored to handle all the aspects of SMN. If you desire a gear set change to strengthen an ability 
	from your subjob that is not supported by this program, you probably will have to make a custom gear set 
	and use the /gearset command to use it.
--]]

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
        Ammo = 'Fortune Egg',
        Head = 'Austere Hat',
        Neck = 'Smn. Torque',
        Ear1 = 'Black Earring',
        Ear2 = 'Bat Earring',
        Body = 'Austere Robe',
        Hands = 'Carbuncle Mitts',
        Ring1 = 'Evoker\'s Ring',
		Ring2 = 'Tamas Ring',
        Back = 'Blue Cape',
		Waist = 'Hierarch Belt',
        Legs = 'Evoker\'s Spats',
        Feet = 'Mannequin Pumps',
    },
	['Idle_Conditional'] = {
	},
	
	--[[
		The Idle_Regen and Idle_Refresh gear sets replace the normal Idle set when the player's HP or MP
		go below a set percentage, accordingly. For HP it is 60% and for MP it is 70%. These percentages
		are defined in gcinclude.lua. 
	--]]
	
	['Idle_Regen'] = {
	},
	
	['Idle_Refresh'] = {
	},
	
	-- This is equipped when the pet is idle
	['Pet_Idle'] = {
        Ammo = 'Fortune Egg',
		Head = 'Austere Hat',
        Neck = 'Smn. Torque',
        Ear1 = 'Black Earring',
        Ear2 = 'Bat Earring',
        Body = 'Austere Robe',
        Hands = 'Carbuncle Mitts',
        Ring1 = 'Evoker\'s Ring',
		Ring2 = 'Tamas Ring',
        Back = 'Blue Cape',
        Waist = 'Hierarch Belt',
        Legs = 'Evoker\'s Spats',
        Feet = 'Mannequin Pumps',
    },
	['Pet_Idle_Conditional'] = {
	},	
	
	--[[
		Resting emphasizes either HP regen or MP refresh. Regen is the assumed target when resting
	--]]
	
	['Resting'] = {
        Ammo = 'Fortune Egg',
        Neck = 'Smn. Torque',
        Ear1 = 'Black Earring',
        Ear2 = 'Bat Earring',
        Body = 'Austere Robe',
        Hands = 'Carbuncle Mitts',
        Ring1 = 'Evoker\'s Ring',
		Ring2 = 'Tamas Ring',
        Back = 'Blue Cape',
        Waist = 'Hierarch Belt',
        Legs = 'Evoker\'s Spats',
        Feet = 'Mannequin Pumps',
    },
	['Resting_Conditional'] = {
	},
	
	['Resting_Refresh'] = {
		Waist = 'Hierarch Belt',
	},
	['Resting_Refresh_Conditional'] = {
	},

	-- Spell interuption rate
	['SIR'] = {
	},
	['SIR_Conditional'] = {
	},
	
	-- Blood pact: Blood pact delay, blood pact recast
	['BP'] = {
        Head = 'Austere Hat',
        Neck = 'Smn. Torque',
        Body = 'Austere Robe',
        Hands = 'Carbuncle Mitts',
        Ring1 = 'Evoker\'s Ring',
    },
	['BP_Conditional'] = {
	},
	
	-- Physical blood pact: pet atk, pet acc, pet crit, pet DA, BP physical dmg+
	['SmnPhysical'] = {
    },
	['SmnPhysical_Conditional'] = {
	},
	
	-- Magical blood pact: pet MAB, pet M.acc, BP magical dmg+
	['SmnMagical'] = {
    },
	['SmnMagical_Conditional'] = {
	},
	
	['SmnSkill'] = {
        Head = 'Austere Hat',		-- +2 Summoning Skill
        Neck = 'Smn. Torque',		-- +7 Summoning Skill
        Hands = 'Carbuncle Mitts',
        Ring1 = 'Evoker\'s Ring',	-- +10 Summoning Skill
    },
	['SmnSkill_Conditional'] = {
	},
	
    ['SmnAccuracy'] = {
        Head = 'Shep. Bonnet',		-- +5 Pet Accuracy, +3 Pet Magic Accuracy
		Ear2 = 'Beastly Earring',	-- +10 Pet Accuracy
    },
	['SmnAccuracy_Conditional'] = {
	},
	
    ['SmnHybrid'] = {				--special set, 2x physical, 1x elemental
    },
	['SmnHybrid_Conditional'] = {
	},	
	
--[[
	Start weapons are where you define what you want the first row of equipment to look like when you
	either log in as a BST or you switch your main job to BST. Any other gear you mention will be overridden
	by the Idle or Town set, so no need to include here.
--]]

	['Start_Weapons'] = {
	    Main = 'Light Staff',
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
		Unlike most other jobs, Summoner's emphasis is fighting with your avatar. So, the TP sets and
		the associated subsets (for accuracy, magical accuracy, and evasion) are directed at the avatar, 
		not the player. If you truly wish to fight too and need special gearsets, make a special set 
		with the appropriate gear for the player and use /gearset.

--]]

	['TP'] = {
        Head = 'Shep. Bonnet',
        Neck = 'Smn. Torque',
        Ear1 = 'Black Earring',
        Ear2 = 'Beastly Earring',
        Body = 'Austere Robe',
        Hands = 'Carbuncle Mitts',
        Ring1 = 'Evoker\'s Ring',
        Ring2 = 'Tamas Ring',
        Back = 'Blue Cape',
        Waist = 'Hierarch Belt',
        Legs = 'Evoker\'s Spats',
        Feet = 'Mannequin Pumps',
    },
	['TP_Conditional'] = {
	},
	
	['TP_Accuracy'] = {
        Head = 'Shep. Bonnet',
        Ear2 = 'Beastly Earring',
        Ring2 = 'Tamas Ring',
    },
	['TP_Accuracy_Conditional'] = {
	},
	
	['TP_Evasion'] = {
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

	-- Healing Magic Skill
	['Cure'] = {
    },
	['Cure_Conditional'] = {
	},
	
	-- Dark Magic Skill
	['Dark'] = {
	},
	['Dark_Conditional'] = {
	},
	
	-- Divine Magic Skill
	['Divine'] = {
	},
	['Divine_Conditional'] = {
	},
	
	-- Enfeebling Magic Skill
	['Enfeebling'] = {
	},
	['Enfeebling_Conditional'] = {
	},
	
	-- Enhancing Magic Skill
	['Enhancing'] = {
	},
	['Enhancing_Conditional'] = {
	},
	
	-- Elemental Magic Skill
	['Elemental'] = {
	},
	['Elemental_Conditional'] = {
	},
	
	-- Ninjitsu Skill
	['Ninjitsu'] = {
	},
	['Ninjitsu_Conditional'] = {
	},
	
	-- Summoning Skill, any boost to pet or summoning ability
	['Summoning'] = {
        Head = 'Austere Hat',
        Neck = 'Smn. Torque',
        Ear2 = 'Beastly Earring',
        Body = 'Austere Robe',
        Hands = 'Carbuncle Mitts',
        Ring1 = 'Evoker\'s Ring',
        Legs = 'Evoker\'s Spats',
    },
	['Summoning_Conditional'] = {
	},
	
--[[
	Next is stat-based gear, (in this case intelligence or mind)
--]]

	['INT'] = {
        Hands = 'Seer\'s Mitts',		-- +1 INT
        Ring1 = 'Windurstian Ring',		-- +1 INT
        Ring2 = 'Tamas Ring',			-- +2~5 INT
        Waist = 'Mrc.Cpt. Belt',		-- +1 INT
        Legs = 'Seer\'s Slacks',		-- +1 INT
        Feet = 'Mannequin Pumps',		-- +1 INT
    },
	['INT_Conditional'] = {
	},
	
	['MND'] = {
        Neck = 'Justice Badge',			-- +3 MND
        Body = 'Wonder Kaftan',			-- +1 MND
        Hands = 'Seer\'s Mitts',		-- +1 MND
        Ring1 = 'Tranquility Ring',		-- +2 MND
        Ring2 = 'Tamas Ring',			-- +2~5 MND
        Back = 'White Cape',			-- +2 MND
        Waist = 'Mrc.Cpt. Belt',		-- +1 MND
        Legs = 'Wonder Braccae',		-- +2 MND
        Feet = 'Mannequin Pumps',		-- +2 MND
    },
	['MND_Conditional'] = {
	},
	
--[[
	Magic accuracy gear, for your avatar although you can include player magical accuracy gear too if
	you want. Just make sure to emphasize pet magical accuracy.
--]]

	['macc'] = {
		Head = 'Shep. Bonnet',
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
	
	-- Combination of MND and enhancing skill. MND is 3x more important. There's also gear that enhances
	['Stoneskin'] = {
	},	
	['Stoneskin_Conditional'] = {
	},
	
	['Refresh'] = { 
	},
	['Refresh_Conditional'] = {
	},


--[[
		SMN can use the following weapons: staff (B), Club (C+), dagger (E). Any other weapon will have 
		no weaponskill available. Weapon skill sets are named based on stat(s) used, regardless of weapon

		* Strength based or just skill based *
		Staff: Heavy Swing,Shell Crusher,Full Swing
		Club: Starlight,Brainshaker,Moonlight,Skullbreaker,True Strike

-]]
	
	['WS_STR'] = {
        Head = 'Mrc.Cpt. Headgear',
        Neck = 'Spike Necklace',
        Ear1 = 'Fang Earring',
        Ear2 = 'Fang Earring',
        Body = 'Wonder Kaftan',
        Hands = 'Wonder Mitts',
        Ring1 = 'Sun Ring',
        Ring2 = 'Sun Ring',
        Waist = 'Mrc.Cpt. Belt',
        Legs = 'Wonder Braccae',
        Feet = 'Creek F Clomps',
    },
	['WS_STR_Conditional'] = {
	},
	
--[[
		* Strength and Intelligence based, even weighting *
		Staff: Rock Crusher,Earth Crusher,Cataclysm
		
--]]
	
	['WS_STRINT'] = {
        Head = 'Mrc.Cpt. Headgear',
        Neck = 'Spike Necklace',
        Ear1 = 'Fang Earring',
        Ear2 = 'Fang Earring',
        Body = 'Wonder Kaftan',
        Hands = 'Wonder Mitts',
        Ring1 = 'Sun Ring',
        Ring2 = 'Tamas Ring',
        Waist = 'Mrc.Cpt. Belt',
        Legs = 'Seer\'s Slacks',
        Feet = 'Creek F Clomps',
    },
	['WS_STRINT_Conditional'] = {
	},

--[[
		* Strength and Mind based, even weighting *
		Club: Shining Strike,Seraph Strike,Judgement
		Staff: Starburst,Sunburst,Retribution
--]]
	
	['WS_STRMND'] = {
        Head = 'Mrc.Cpt. Headgear',
        Neck = 'Justice Badge',
        Body = 'Wonder Kaftan',
        Hands = 'Wonder Mitts',
        Ring1 = 'Sun Ring',
        Ring2 = 'Tamas Ring',
        Back = 'White Cape',
        Waist = 'Mrc.Cpt. Belt',
        Legs = 'Wonder Braccae',
        Feet = 'Creek F Clomps',
    },
	['WS_STRMND_Conditional'] = {
	},

--[[
		* Strength and Mind based, 30% to 50% weighting *
		Club: Black Halo
--]]

	['WS_STRMND_30_50'] = {
        Head = 'Mrc.Cpt. Headgear',
        Neck = 'Justice Badge',
        Body = 'Wonder Kaftan',
        Hands = 'Wonder Mitts',
        Ring1 = 'Sun Ring',
        Ring2 = 'Tamas Ring',
        Back = 'White Cape',
        Waist = 'Mrc.Cpt. Belt',
        Legs = 'Wonder Braccae',
        Feet = 'Creek F Clomps',
    },
	['WS_STRMND_30_50_Conditional'] = {
	},
	
--[[
		* Dexterity based *
		Dagger: Wasp Sting,Viper Bite^

		^ Subjob must be one of: RDM,THF,BRD,RNG,NIN
--]]
	
	['WS_DEX'] = {
        Head = 'Empress Hairpin',
        Neck = 'Spike Necklace',
        Body = 'Mrc.Cpt. Doublet',
        Ring1 = 'Balance Ring',
        Ring2 = 'Bastokan Ring',
        Waist = 'Mrc.Cpt. Belt',
    },
	['WS_DEX_Conditional'] = {
	},

--[[
		* Dexterity and Intelligence based *
		Dagger: Gust Slash,Cyclone^

		^ Subjob must be one of: RDM,THF,BRD,RNG,NIN
--]]
	
	['WS_DEXINT'] = {
        Head = 'Empress Hairpin',
        Neck = 'Spike Necklace',
        Body = 'Mrc.Cpt. Doublet',
        Hands = 'Seer\'s Mitts',
        Ring1 = 'Windurstian Ring',
        Ring2 = 'Tamas Ring',
        Waist = 'Mrc.Cpt. Belt',
        Legs = 'Seer\'s Slacks',
        Feet = 'Mannequin Pumps',
    },
	['WS_DEXINT_Conditional'] = {
	},

--[[
		* Intellegence *
		Staff: Gate of Tartarus
--]]
	
	['WS_INT'] = {
        Hands = 'Seer\'s Mitts',
        Ring1 = 'Windurstian Ring',
        Ring2 = 'Tamas Ring',
        Waist = 'Mrc.Cpt. Belt',
        Legs = 'Seer\'s Slacks',
        Feet = 'Mannequin Pumps',
    },
	['WS_INT_Conditional'] = {
	},
	
--[[
		* Intellegence and Mind based, even weighting *
		Staff: Spirit Taker
--]]
	
	['WS_INTMND'] = {
        Neck = 'Justice Badge',
        Body = 'Wonder Kaftan',
        Hands = 'Seer\'s Mitts',
        Ring1 = 'Windurstian Ring',
        Ring2 = 'Tamas Ring',
        Waist = 'Mrc.Cpt. Belt',
        Legs = 'Wonder Braccae',
        Feet = 'Mannequin Pumps',
    },
	['WS_INTMND_Conditional'] = {
	},
	
--[[
		* Charisma based *
		Dagger: Shadowstitch
--]]
	
	['WS_CHR'] = {
        Head = 'Entrancing Ribbon',
        Neck = 'Flower Necklace',
        Ring1 = 'Moon Ring',
        Ring2 = 'Moon Ring',
        Waist = 'Corsette',
    },
	['WS_CHR_Conditional'] = {
	},

--[[
		* Mind based *
		Dagger: Energy Steal, Energy Drain^
		
		^ Subjob must be RDM,THF,BRD,RNG, or NIN
--]]

	['WS_MND'] = {
        Neck = 'Justice Badge',
        Body = 'Wonder Kaftan',
        Hands = 'Seer\'s Mitts',
        Ring1 = 'Tranquility Ring',
        Ring2 = 'Tamas Ring',
        Back = 'White Cape',
        Waist = 'Mrc.Cpt. Belt',
        Legs = 'Wonder Braccae',
        Feet = 'Mannequin Pumps',
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
        Head = 'Optical Hat',
        Ring1 = 'Toreador\'s Ring',
        Ring2 = 'Jaeger Ring',
        Waist = 'Life Belt',
        Legs = 'Evoker\'s Spats',
    },	
	['WS_Accuracy_Conditional'] = {
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
        Ammo = 'Fortune Egg',
        Head = 'Silver Hairpin',
        Neck = 'Rep.Bronze Medal',
        Ear1 = 'Onyx Earring',
        Ear2 = 'Onyx Earring',
        Body = 'Angler\'s Tunica',
        Hands = 'Carbuncle Mitts',
        Ring1 = 'Astral Ring',
        Ring2 = 'Astral Ring',
        Waist = 'Friar\'s Rope',
        Legs = 'Freesword\'s Slops',
        Feet = 'Waders',
    },
	
	['CAP25'] = {
        Ammo = 'Fortune Egg',
        Head = 'Shep. Bonnet',
        Neck = 'Rep.Bronze Medal',
        Ear1 = 'Onyx Earring',
        Ear2 = 'Onyx Earring',
        Body = 'Angler\'s Tunica',
        Hands = 'Carbuncle Mitts',
        Ring1 = 'Astral Ring',
        Ring2 = 'Astral Ring',
        Waist = 'Friar\'s Rope',
        Legs = 'Freesword\'s Slops',
        Feet = 'Waders',
    },
	
	['CAP30'] = {
        Ammo = 'Fortune Egg',
        Head = 'Shep. Bonnet',
        Neck = 'Rep.Bronze Medal',
        Ear1 = 'Onyx Earring',
        Ear2 = 'Onyx Earring',
        Body = 'Seer\'s Tunic',
        Hands = 'Carbuncle Mitts',
        Ring1 = 'Astral Ring',
        Ring2 = 'Tamas Ring',
        Waist = 'Friar\'s Rope',
        Legs = 'Seer\'s Slacks',
        Feet = 'Seer\'s Pumps',
    },
	
	['CAP40'] = {
        Ammo = 'Fortune Egg',
        Head = 'Shep. Bonnet',
        Neck = 'Spirit Torque',
        Ear1 = 'Black Earring',
        Ear2 = 'Onyx Earring',
        Body = 'Seer\'s Tunic',
        Hands = 'Carbuncle Mitts',
        Ring1 = 'Astral Ring',
        Ring2 = 'Tamas Ring',
        Back = 'White Cape',
        Waist = 'Friar\'s Rope',
        Legs = 'Seer\'s Slacks',
        Feet = 'Mannequin Pumps',
    },
	
	['CAP50'] = {
        Ammo = 'Fortune Egg',
        Head = 'Austere Hat',
        Neck = 'Spirit Torque',
        Ear1 = 'Black Earring',
        Ear2 = 'Onyx Earring',
        Body = 'Austere Robe',
        Hands = 'Carbuncle Mitts',
        Ring1 = 'Astral Ring',
        Ring2 = 'Tamas Ring',
        Back = 'White Cape',
        Waist = 'Powerful Rope',
        Legs = 'Seer\'s Slacks',
        Feet = 'Mannequin Pumps',
    },

	['CAP60'] = {
        Ammo = 'Fortune Egg',
        Head = 'Austere Hat',
        Neck = 'Spirit Torque',
        Ear1 = 'Black Earring',
        Ear2 = 'Bat Earring',
        Body = 'Austere Robe',
        Hands = 'Carbuncle Mitts',
        Ring1 = 'Astral Ring',
        Ring2 = 'Tamas Ring',
        Back = 'White Cape',
        Waist = 'Powerful Rope',
        Legs = 'Evoker\'s Spats',
        Feet = 'Mannequin Pumps',
    },
};

profile.Sets = sets;
profile.sjb = nil;
profile.bAmmo = false;
profile.sAmmo = nil;
profile.sPetAction = nil;



--[[
	HandlePetAction equips the appropriate gear set based on the type of action
	the pet is trying to perform.
--]]

local function HandlePetAction(Pet,PetAction)

	if gcdisplay.GetToggle('GSwap') ~= false then		-- Only gear swap if this flag is true
	
		if (gcinclude.SmnSkill:contains(PetAction.Name)) then		
			gFunc.EquipSet(sets.SmnSkill);
			gcinclude.ProcessConditional(sets.SmnSkill_Conditional,nil);
		elseif (gcinclude.SmnMagical:contains(PetAction.Name)) then	
			gFunc.EquipSet(sets.SmnMagical);
			gcinclude.ProcessConditional(sets.SmnMagical_Conditional,nil);
			if gcdisplay.GetToggle('Acc') == true then
				gFunc.EquipSet(sets.macc);
				gcinclude.ProcessConditional(sets.macc_Conditional,nil);
			end			
		elseif (gcinclude.SmnHybrid:contains(PetAction.Name)) then		
			gFunc.EquipSet(sets.SmnHybrid);
			gcinclude.ProcessConditional(sets.SmnHybrid_Conditional,nil);
			if gcdisplay.GetToggle('Acc') == true then
				gFunc.EquipSet(sets.macc);
				gcinclude.ProcessConditional(sets.macc_Conditional,nil);
				gFunc.EquipSet(sets.SmnAccuracy);
				gcinclude.ProcessConditional(sets.SmnAccuracy_Conditional,nil);
			end				
		else	
			gFunc.EquipSet(sets.SmnPhysical);
			gcinclude.ProcessConditional(sets.SmnPhysical_Conditional,nil);
			if gcdisplay.GetToggle('Acc') == true then
				gFunc.EquipSet(sets.SmnAccuracy);
				gcinclude.ProcessConditional(sets.SmnAccuracy_Conditional,nil);
			end
		end
	end

	-- if the action just done is a BP: rage, print out what happened in party chat
	if (profile.sPetAction == nil or profile.sPetAction ~= PetAction.Name) and gcdisplay.GetToggle('sBP') == true then
		local sMsg;
		if string.find(gcinclude.SmnBPRageList,PetAction.Name) ~= nil then
			sMsg = '/p [' .. Pet.Name .. ']: Blood Pact - ' .. PetAction.Name .. ' --> <t>.';
		else
			sMsg = '/echo [' .. Pet.Name .. ']: Blood Pact - ' .. PetAction.Name;
		end
		AshitaCore:GetChatManager():QueueCommand(-1, sMsg);
		profile.sPetAction = PetAction.Name;
	end
end

--[[
	SetSubjobSet is used to pick the appropriate set for the loaded macrobook based on
	which subjob is current. (If no change has occurred since the last time it was called,
	nothing is checked/changed.)
--]]

local function SetSubjobSet(chkSJ)
	local subs = {['WAR'] = 0, ['MNK'] = 0, ['WHM'] = 1, ['BLM'] = 3, ['RDM'] = 2, ['THF'] = 0,
				 ['PLD'] = 0, ['DRK'] = 0, ['BST'] = nil, ['BRD'] = 0, ['RNG'] = 0, ['SMN'] = 0,
				 ['SAM'] = 0, ['NIN'] = 0, ['DRG'] = 0, ['BLU'] = 0, ['COR'] = 0, ['PUP'] = 0,
				 ['DNC'] = 0, ['SCH'] = 0, ['GEO'] = 0, ['RUN'] = 0};
	local sj = nil;

	if (profile.sjb == nil or (chkSJ ~= nil and chkSJ ~= 'NON' and chkSJ ~= profile.sjb)) then	-- Compare the stored subjob with the current subjob
		if subs[chkSJ] ~= nil and subs[chkSJ] > 0 then
			sj = subs[chkSJ];
		else
			sj = 1;					-- Default set
		end

		AshitaCore:GetChatManager():QueueCommand(1, '/macro set '..tostring(sj));
		if chkSJ ~= nil and chkSJ ~= 'NON' then
			profile.sjb = chkSJ;
		end
	end
end

--[[
	OnLoad is run whenever you log into your BST or change your job to BST
--]]

profile.OnLoad = function()
	local player = gData.GetPlayer();

	gSettings.AllowAddSet = true;
	gcinclude.Initialize();
	gcinclude.settings.RegenGearHPP = 50;
    gcinclude.settings.RefreshGearMPP = 60;
	gcinclude.settings.bSummoner = true;
	
	-- Determine if subjob uses magic and if the maximum MP is > 50.
	gcinclude.CheckMagic50(player);
	
	-- Set your job macro toolbar defaults here. Which set depends on the subjob
	AshitaCore:GetChatManager():QueueCommand(1, '/macro book 13');		-- SMN
	SetSubjobSet(player.SubJob);
	
	-- Load up the weapons bar. (This need only be done once.)
	if gcdisplay.GetToggle('GSwap') == true then		-- Only gear swap if this flag is true
		gFunc.EquipSet(sets.Start_Weapons);	
		gcinclude.ProcessConditional(sets.Start_Weapon_Conditional,nil);
	end
end

--[[
	OnUnload is run when you change to another job
--]]

profile.OnUnload = function()
	gcinclude.Unload();
end

--[[
	HandleCommand is run when you type in a command defined in LUASHITACAST. The commands handled here instead
	of in gcinclude.HandleCommands are specific to BST or the help system, which has been tailored to BST.
--]]

profile.HandleCommand = function(args)
	if args[1] == 'help' then
		gcdisplay.ShowHelp(args);
	elseif args[1] == 'petfood' then
		gcinclude.doPetFood(args[2],args[3]);		
	else
		gcinclude.HandleCommands(args);
	end
end

--[[
	HandleDefault is run when some action happens. This emphasizes pet actions
--]]
	
profile.HandleDefault = function()
	local pet = gData.GetPet();
	local petAction = gData.GetPetAction();
	local player = gData.GetPlayer();
	local ew = gData.GetEquipment();
	local eWeap = nil;
	
	if gcinclude.settings.bMagicCheck == false then
		gcinclude.CheckMagic50(player);
	end
	
	-- A pet action takes priority over a player's action.
	if (petAction ~= nil) then
		HandlePetAction(pet,petAction);
		return;
	end
	
	profile.sPetAction = nil;
	
	if ew['Main'] ~= nil then
		eWeap = ew['Main'].Name;	
	end
		
	SetSubjobSet(player.SubJob);			-- Make sure the correct set is shown in case the subjob was changed.
	
	if gcdisplay.GetToggle('GSwap') == false then		-- Only gear swap if this flag is true
		return;
	end
	
	-- If player is not resting and has a pet, make sure they're holding the
	-- correct staff (assuming they own the correct staff)
	if player.Status ~= 'Resting' and pet ~= nil then
		local pName = string.lower(pet.Name);
		local pEle = gcinclude.SummonStaves[pName];
	
		if eWeap ~= nil and (eWeap ~= gcinclude.elemental_staves[pEle][1] or eWeap ~= gcinclude.elemental_staves[pEle][3]) then
			gcinclude.SwapToStave(pEle,true);
		end
	end
		
	-- Now process the player status accordingly. (Note that player.Status == 'Engaged' is no longer supported since
	-- the emphasis is on how the pet fights, not the summoner.
	if (pet ~= nil and pet.Status == 'Engaged') then
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
	elseif player.Status == 'Resting' then	-- Player kneeling. Priority (low to high): resting, refresh
		if player.HPP < gcinclude.settings.RegenGearHPP then
			gFunc.EquipSet(sets.Resting);
			gcinclude.ProcessConditional(sets.Resting_Conditional,nil);
		end
		if player.MPP < gcinclude.settings.RefreshGearMPP then
			gFunc.EquipSet(sets.Resting_Refresh);
			gcinclude.ProcessConditional(sets.Resting_Refresh_Conditional,nil);
		end
		
		-- Weapon swap to a higher MP refresh while healing weapon if appropriate.
		if player.MP < player.MaxMP then
			gcinclude.SwapToStave('dark',false);
		end
	else									-- Assume idling. Priority (low to high): Idle,refresh. (Could be player fighting, but that's ignored.)
		gFunc.EquipSet(sets.Idle);
		gcinclude.ProcessConditional(sets.Idle_Conditional,nil);
		if player.HPP < gcinclude.settings.RegenGearHPP then		-- if the player's HP is below the threshold setting, equip the idle regen gear
			gFunc.EquipSet(sets.Idle_Regen);
		end
		if player.MPP < gcinclude.settings.RefreshGearMPP then		-- if the player's MP is below the threshold setting, equip the idle refresh gear
			gFunc.EquipSet(sets.Idle_Refresh);
		end
			
		if (pet ~= nil and pet.Status == 'Idle') then
			gFunc.EquipSet(sets.Pet_Idle);
			gcinclude.ProcessConditional(sets.Pet_Idle_Conditional,nil);
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

--[[
	HandleAbility is used to change the player's gear appropriately for the specified avatar ability.
--]]

profile.HandleAbility = function()
	local ability = gData.GetAction();
	
	if (ability.Name == 'Release') or 
		(ability.Name == 'Assault') or
		(ability.Name == 'Retreat') then 
		return 
	end
	
	-- it has to be a Blood Pact
	if gcdisplay.GetToggle('GSwap') == true then		-- Only gear swap if this flag is true
		gFunc.EquipSet(sets.BP);
		gcinclude.ProcessConditional(sets.BP_Conditional,nil);
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
		if string.match(item.Name, 'Holy Water') then 
			gFunc.EquipSet(gcinclude.sets.Holy_Water);
		end			
	end
end

--[[
	HandlePrecast loads Fast Cast, cast time reduction, and quick cast gear in anticipation of a spell
--]]

profile.HandlePrecast = function()
    local spell = gData.GetAction();
	local obi;
	local mSet;
	
	-- Normal process
	if gcdisplay.GetToggle('GSwap') == true then		-- Only gear swap if this flag is true
		gFunc.EquipSet(sets.Precast);
		gcinclude.ProcessConditional(sets.Precast_Conditional,nil);
		
		-- See if an elemental obi should be equipped
		if gcinclude.settings.bEleObis == false then
			gcinclude.CheckForObisGorgets();
		end	
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

	if gcdisplay.GetToggle('GSwap') == false then		-- Only gear swap if this flag is true	
		return;
	end
		
	-- First load the midcast set
	gFunc.EquipSet(sets.Midcast);
	gcinclude.ProcessConditional(sets.Midcast_Conditional,nil);
		
	-- Then the Spell Interruption set
	gFunc.EquipSet(sets.SIR);
	gcinclude.ProcessConditional(sets.SIR_Conditional,nil);
		
	-- Next, see if INT/MND gear should be loaded
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

	if gcinclude.settings.bEleObis == false then
		gcinclude.CheckForObisGorgets();
	end	
	if gcinclude.settings.bEleObis == true then
		obi = gcinclude.CheckEleSpells(spell.Name,gcinclude.MagicEleDmg,gcinclude.OBI);
		if obi ~= nil then
			gFunc.ForceEquip('Waist',obi);
		end
	end
		
	stat = nil;
	-- Lastly, how about an elemental stave (use the MagicEleDmg in gcinclude) or summons
	if gcinclude.settings.bStave == false then
		gcinclude.CheckForStaves();
	end
	if gcinclude.settings.bEleStaves == true then
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

	if (gcdisplay.GetToggle('TH') == true) then gFunc.EquipSet(sets.TH) end
end

--[[
	HandleWeaponskill loads the gear appropriately for the weapon skill you're doing
--]]

profile.HandleWeaponskill = function()

	if gcdisplay.GetToggle('GSwap') == false then		-- Only gear swap if this flag is true
		return;
	end
	
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
		elseif sWS == 'WS_STR' then
			gcinclude.ProcessConditional(sets.WS_STR_Conditional,nil);
		elseif sWS == 'WS_MND' then
			gcinclude.ProcessConditional(sets.WS_MND_Conditional,nil);
		elseif sWS == 'WS_INT' then
			gcinclude.ProcessConditional(sets.WS_INT_Conditional,nil);				
		elseif sWS == 'WS_INTMND' then
			gcinclude.ProcessConditional(sets.WS_INTMND_Conditional,nil);
		elseif sWS == 'WS_STRMND' then
			gcinclude.ProcessConditional(sets.WS_STRMND_Conditional,nil);
		elseif sWS == 'WS_STRMND_30_50' then
			gcinclude.ProcessConditional(sets.WS_STRMND_30_50_Conditional,nil);
		elseif sWS == 'WS_STRINT' then
			gcinclude.ProcessConditional(sets.WS_STRINT_Conditional,nil);
		end
			
		-- See if an elemental gorget makes sense to equip
		if gcinclude.settings.bEleGorgets == false then
			gcinclude.CheckForObisGorgets();
		end
		if gcinclude.settings.bEleGorgets == true then
			local sGorget = gcinclude.CheckEleGorget(ws.Name);
			if sGorget ~= nil then
				gFunc.ForceEquip('Neck',sGorget);
			end
		end
	end
end

return profile;