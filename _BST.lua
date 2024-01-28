local profile = {};
gcinclude = gFunc.LoadFile('common\\gcinclude.lua');

--[[
	This file contains all the gear sets associated with the BST job. While it isn't outside of the realm of 
	possibility that the subjob might be able to use gear sets too, that is not the emphasis of this program. 
	It is tailored to handle all the aspects of BST. It also supports magic use which is obviously a possibility
	from the subjob. If you desire a gear set change to strengthen an ability from your subjob that is not 
	supported by this program, you probably will have to make a custom gear set and use the /gearset command to
	use it.
--]]

local sets = {
--[[
	The gear sets are usually defined by a pair of sets: the "main" one and an associated "conditional" set. The
	"main" is loaded when appropriate and the conditional is processed to see if any of the entries should be
	equipped too. ("Conditional" entries consist of gear that need to meet certain conditions before they will be
	equipped.) "main" sets contain your standard gear slot='gear piece' combinations and the "conditional" entries
	of the piece of gear, a description of the condition, the slot the piece equips into, the minimum level the 
	player must be to equip the piece, what jobs can equip the piece, and the conditional code with potentially
	associated information needed to determine if you can wear the piece. (All conditional gear's definitions
	can be found in "Conditional gear master list.txt found in ../common as well as some user-defined conditionals
	known to work. Just copy the entry from that file into the appropriate "conditional" set.
		
	It is recommended that "main" sets not include any gear found in the top line of your equipment grid (main hand,
	off hand, ranged weapon, ammo). Doing so will mean that TP will be reset to 0 whenever gear is changed which can
	be very frustrating. Further, any time you do change a weapon, it will convert back to what was defined in a set.
	Believe me, it's no fun	fighting Luashitacast!

	Also, not all sets need to be defined. There is nothing wrong with leaving a set "empty", but don't delete any
	of the sets. All the ones listed here (except for any custom sets) are expected to exist by Luashitacast.
		
	*** Note ***
	/SMN has a problem in that their pet is the level of the subjob, which is not very useful. As a
	result, /SMN pet actions are not supported in this implementation. As for /DRG, the wyvern can't be summoned.
	Just an FYI.
--]]

	--[[
		The Idle_Regen and Idle_Refresh gear sets are used to restore a player's HP or MP that goes 
		below a set percentage (defined in gcinclude.lua, but can be overriden in profile.OnLoad
		function).
	--]]
	
	['Idle_Regen'] = {
	},
	['Idle_Regen_Conditional'] = {
	},
	
	['Idle_Refresh'] = {
	},
	['Idle_Refresh_Conditional'] = {
		{'Gaudy Harness','Adds refresh if MP < 50','Body',50,'BST','MP<50'},
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
	},
	['Resting_Regen_Conditional'] = {
	},

	['Resting_Refresh'] = {
	},
	['Resting_Refresh_Conditional'] = {
		{'Gaudy Harness','Adds refresh if MP < 50','Body',50,'BST','MP<50'},
	},

	['Resting_Refresh_Weapon_Sub51'] = {
	},
	['Resting_Refresh_Weapon_Sub51_Conditional'] = {
	},
	
	-- If you have any Spell Interruption Rate down gear, put them into the "SIR" gear set.
	-- This gear set is equipped in the HandleMidcast function that all spells go through.
	-- Only BST gear that has this attribute is a Woodsville Axe. All other gear has to be 
	-- equippable for any job.
	['SIR'] = {
	},
	['SIR_Conditional'] = {
	},
	
--[[
	Start weapons are where you define what you want the first row of equipment to look like when you
	either log in as a SMN or you switch your main job to SMN. Any other gear you mention will be overridden
	by the Idle or Town set, so no need to include here.
--]]

	['Start_Weapons'] = {
        Main = 'Maneater',
        Sub = 'Tatami Shield',
        Ammo = 'S. Herbal Broth',
    },
	['Start_Weapons_Conditional'] = {
		{'Tabarzin','Equip if /nin or /dnc','Sub',71,'WAR/DRK/BST/RUN','SJIS','NIN/DNC'},
	},

--[[
	What do you want to wear around town? You can define a full set or just an item or two, it is up to you.
	(Please note that a nation's aketon is considered conditional gear, so no need to place here unless you
	want the aketon equipped regardless if it is your home nation's city or not. Due to some of the 
	complexities of lua, the Town_Conditional set is found in the gcinclude.lua file.)
--]]
	
	['Town'] = {
        Head = 'Lilac Corsage',
    },
	
--[[
	Damage reduction gear depends on the type of damage. The most common is physical, but there's times when
	you'll want to reduce magic damage or breath damage. The three gear sets are defined below. The correct
	one will be equipped depending on how DT is set. Please consider not including gear that doesn't have 
	any damage taken property so other wanted stats can shine through.
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
	
--[[
		The TP sets are used when you or your pet are fighting: "TP" for you and "TP_Pet" for you and your pet (or just your pet). 
		The accuracy set will be used if ACC is specified and the evasion set if EVA is specified.
--]]

	['TP'] = {
        Head = 'Panther Mask',
        Neck = 'Peacock Amulet',
        Ear1 = 'Ethereal Earring',
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
	['TP_Conditional'] = {
		{'Gaudy Harness','Adds refresh if MP < 50','Body',50,'BST','MP<50'},
	},
	
	['TP_Pet'] = {
        Head = 'Shep. Bonnet',
		Legs = 'Shep. Hose',
    },
	['TP_Pet_Conditional'] = {
	},
	
	['TP_Tank'] = {
	},
	['TP_Tank_Conditional'] = {
	},
	
--[[
	If an accuracy emphasis is desired, the following set will replace the gear appropriately.
	(Please note that Pet_Accuracy is applied after Accuracy if you have a pet.)
--]]

	['Accuracy'] = {
        Head = 'Optical Hat',					-- +10 Acc
        Neck = 'Peacock Amulet',				-- +10 Acc
        Body = 'Narasimha\'s Vest',				-- +4 Acc
        Hands = 'Thick Mufflers',				-- +3 Acc
        Ring1 = 'Toreador\'s Ring',				-- +7 Acc
        Ring2 = 'Jaeger Ring',					-- +4 Acc
        Back = 'Psilos Mantle',					-- +1 Acc
        Waist = 'Life Belt',					-- +10 Acc
        Legs = 'Thick Breeches',				-- +2 Acc
        Feet = 'Thick Sollerets',				-- +2 Acc
    },	
	['Accuracy_Conditional'] = {
	},

	['Pet_Accuracy'] = {
	    Head = 'Shep. Bonnet',					-- Pet Accuracy +5
		Ear2 = 'Beastly Earring',				-- Pet Accuracy +10
    },
	['Pet_Accuracy_Conditional'] = {
	},

--[[
		If evasion wanted, equip evasion gear
--]]

	['Evasion'] = {
        Head = 'Optical Hat',					-- +10 eva
		Ear1 = 'Ethereal Earring',				-- +5 eva
        Body = 'Narasimha\'s Vest',				-- +4 eva
		Hands = 'Battle Gloves',				-- +3 eva
        Legs = 'San. Trousers',					-- +2 eva
		Feet = 'Bounding Boots',				-- Default gear is Thick Sollerets which have -2 eva
    },
	['Evasion_Conditional'] = {
	},

--[[
	Magic accuracy gear
--]]

	['macc'] = {
	},
	['macc_Conditional'] = {
		{'Tamas Ring','will equip if subjob can do magic','RING',30,'ALL','SJ:MAGIC'},
	},
	
--[[
	Magic Attack Bonus (MAB) is used for more than just spells, so it is broken out
--]]

	['MAB'] = {
	},
	['MAB_Conditional'] = {
		{'Uggalepih Pendant','MAB +8% if MPP <= 50%','Neck',70,'ALL','MP.LE.50P'},
	},
	
--[[
	Preshot is the first stage of when a ranged shot is being performed. This is where you place any 
	Ranged Accuracy or Ranged Attack Speed gear. 
--]]

	['Preshot'] = {
        Head = 'Optical Hat',			-- Ranged Accuracy +10
		Neck = 'Peacock Amulet',		-- Ranged Accuracy +10
        Ring1 = 'Jaeger Ring',			-- Ranged Accuracy +4
        Back = 'Psilos Mantle',			-- Ranged Accuracy +1
    },
	['Preshot_Conditional'] = {
	},
	
--[[
	Midshot is the second stage of a ranged shot. This is where you place Ranged Attack or Ranged 
	Damage gear
--]]

	['Midshot'] = {
        Back = 'Psilos Mantle',			-- Ranged Attack +12
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
	The second stage is Midcast. This is where you'll want to equip magic attack, or magic enhancing 
	gear. (Magic Attack Bonus also happens here, but is broken out into it's own gear set. See MAB.)
--]]	

	['Midcast'] = {
	},
	['Midcast_Conditional'] = {
		{'Uggalepih Pendant','MAB +8% if MPP <= 50%','Neck',70,'ALL','MP.LE.50P'}, -- doesn't need to be user-defined
	},

--[[
	Further, there is a break out for each type of spell. I've included a comment on the type of attributes
	the piece of gear should have. While the spell might have other attributes than those listed, the ones I have
	listed have gear that a BST or anyone can wear.
--]]

	-- Healing: Healing Magic Skill. Currently only a Healing Earring affects healing spells from a sub job. No other gear
    -- gives bonuses to Healing magic from a sub job. Also, gear with MND bonuses will boost cure spell's potency, but MND 
	-- gear is automatically equipped prior to the Healing set being equipped in the HandleMidcast function. There's no need 
	-- to include MND gear here. As to items that add cure potency directly there are a few pieces for "all jobs". So, 
	-- include healing magic skill items and cure potency items here.
	['Healing'] = {
    },
	['Healing_Conditional'] = {
	},
	
	-- Dark: Dark Magic Skill. Currently only gear equippable by any job gives is applicable here. There's no gear that's 
	-- specific for BST that gives any dark magic skill.	
	['Dark'] = {
    },
	['Dark_Conditional'] = {
	},
	
	-- Divine: Divine Magic Skill.
	['Divine'] = {
	},
	['Divine_Conditional'] = {
	},

	-- Enfeebling: Enfeebling Magic Skill. Currently only gear equippable by any job gives is applicable here. There's no gear 
	-- that's specific for BST that gives any enfeebling magic skill.
	['Enfeebling'] = {
	},
	['Enfeebling_Conditional'] = {
	},

	-- Enhancing: There is no gear that a BST can wear to enhance any magic spell. Leave the Enhancing gear sets empty.
	['Enhancing'] = {
	},
	['Enhancing_Conditional'] = {
	},
	
	-- Elemental: Elemental Magic Skill. Currently only gear equippable by any job gives is applicable here. There's no gear
	-- that's specific for BST that gives any elemental magic skill. Note: don't include elemental staves or elemental 
	-- obis/gorgets here, that is done automatically in the HandlePrecast/HandleMidcast functions (if /wswap is enabled).
	['Elemental'] = {
	},
	['Elemental_Conditional'] = {
	},
	
	-- Ninjitsu: There is no gear that a BST can wear to add Ninjitsu skill. Leave the following two
	-- gear sets empty.	
	['Ninjitsu'] = {
	},
	['Ninjitsu_Conditional'] = {
	},
	
	-- Summoning: Summoning Magic Skill and Avatar Perpetuation Cost. Currently only gear equippable by any job gives
	-- is applicable here. There's no gear that's specific for BST that gives any summoning skill. Note: currently on 
	-- HorizonXI summoning skills are ignored. Any gear piece that only gives summoning skill will be commented out	
	['Summoning'] = {
	},
	['Summoning_Conditional'] = {
	},
	
--[[
	Next is stat-based gear, (in this case intelligence or mind)
--]]

	['INT'] = {
        Head = 'Beast helm',			-- +5 INT
        Ring1 = 'Tamas Ring',			-- +2~5 INT
        Ring2 = 'Windurstian Ring',		-- +1 INT
        Feet = 'Mannequin Pumps',		-- +1 INT
    },
	['INT_Conditional'] = {
	},
	
	['MND'] = {
        Neck = 'Justice Badge',			-- +3 MND
        Body = 'Wonder Kaftan',			-- +1 MND
        Ring1 = 'Tamas Ring',			-- +2~5 MND
        Ring2 = 'Tranquility Ring',		-- +2 MND
        Waist = 'Friar\'s Rope',		-- +1 MND
        Legs = 'Wonder Braccae',		-- +2 MND
        Feet = 'Mannequin Pumps',		-- +2 MND
	},
	['MND_Conditional'] = {
	},
	
--[[
	And some spells are special cases, so they have individual gears sets.
--]]
	-- Stoneskin: Stoneskin Enhancement, Mind, and Enhancing Magic Skill. Mind is 3x more important than enhancing
	-- Magic Skill. The only piece of gear a BST can wear to enhance stoneskin is a Stone Gorget. There's no gear
	-- that a SMN (or any job) can wear to enhance magic. Note: This gear set has no effect on Titan's Earthen
	-- ward blood pact.
	['Stoneskin'] = {	
	    Neck = 'Justice Badge',			-- +3 MND
        Body = 'Wonder Kaftan',			-- +1 MND
        Ring1 = 'Tamas Ring',			-- +2~5 MND
        Ring2 = 'Tranquility Ring',		-- +2 MND
        Waist = 'Friar\'s Rope',		-- +1 MND
        Legs = 'Wonder Braccae',		-- +2 MND
        Feet = 'Mannequin Pumps',		-- +2 MND
	},	
	['Stoneskin_Conditional'] = {
	},
	
	-- Drain: Drain Enhancement, Dark Magic Skill, Dark Magic Accuracy. Currently no gear equippable by a
	-- BST enhances Drain. Drain is part of Dark Magic, so Potency which is based on dark magic skill will
	-- already be loaded in HandleMidcast function and need not be repeated here. No current gear supports
	-- dark magic accuracy for any job. Magic attack bonus and magic critical hit have no effect on potency.
	-- Leave this gear set empty.
	['Drain'] = {
    },
	['Drain_Conditional'] = {
	},

	-- Aspir: Aspir Enhancement, Dark Magic Skill, Dark Magic Accuracy. Currently no gear equippable by a
	-- BST enhances Aspir. Aspir is part of Dark Magic, so potency which is based on dark magic skill will
	-- already be loaded in HandleMidcast function and need not be repeated here. No current gear supports
	-- dark magic accuracy for any job. Magic attack bonus and magic critical hit have no effect on potency.
	-- Leave this gear set empty.
	['Aspir'] = {
    },
	['Aspir_Conditional'] = {
	},

	-- Sneak: Enhances Sneak and Enhances Stealth. Currently on Dream Boots +1 enhances sneak and is equippable
	-- by any job. (Attained through the Starlight Celebration.) No gear for any job supports Enhances Stealth
	-- yet.
	['Sneak'] = {
		Feet = 'Dream Boots +1',
	},
	['Sneak_Conditional'] = {
	},

	-- Invisible: Enhances Invisible Effect. Currently only Dream Mittens +1 enhances invisible and is equippable
	-- by any job. (Attained through the Starlight Celebration.)	
	['Invisible'] = {
		Hands = 'Dream Mittens +1',
	},
	['Invisible_Conditional'] = {
	},
	
	-- Note: Phalanx does have gear that supports the spell, but it is out of era
	
--[[
	The following weapon skill gearsets are defined by the stat they emphasize. Listed are all of the sets that
	you will need to use every weapon skill that your job can do. The leading comment defines what weapon/weapon
	skill combination the set applies to.
	
	BST can use the following weapons: axe (A-), scythe (B-), dagger (C+), club(D), sword (E). Any other weapon
	will have no weaponskill available. Weapon skill sets are named based on stat(s) used, regardless of weapon
--]]

--[[
		* Strength based or just skill based *
		
		Axe: Raging Axe,Smash Axe,Gale Axe,Avalanche Axe,Spinning Axe,Rampage,
			 Mistral Axe,Decimation
		Scythe: Slice,Spinning Scythe,Vorpal Scythe
		Club: Brainshaker,Skullbreaker,True Strike
		Sword: Flat Blade,Circle Blade,Vorpal Blade
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
		Head = 'Mrc.Cpt. Headgear',				-- +1 STR
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
		Head = 'Mrc.Cpt. Headgear',				-- +1 STR
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
		* Skill based *
		
		Club: Starlight,Moonlight
--]]

	['WS_Skill'] = {
    },
	['WS_Skill_Conditional'] = {
	},

--[[
		* HP based *
		
		Sword: Spirits Within
--]]

	['WS_HP'] = {
        Head = 'Beast Helm',				-- +15 HP
        Ear1 = 'Ethereal Earring',			-- +15 HP
        Ear2 = 'Physical Earring',			-- +25 HP
        Body = 'Wonder Kaftan',				-- +36 HP
        Hands = 'Wonder Mitts',				-- +12 HP
        Ring1 = 'Toreador\'s Ring',			-- +10 HP
        Waist = 'Powerful Rope',			-- +20 HP
        Legs = 'Wonder Braccae',			-- +21 HP
        Feet = 'Creek F Clomps',			-- +35 HP
    },
	['WS_HP_Conditional'] = {
	},
	
--[[
	The following sets are used with pet abilities/pet commands
--]]
	
	['Call_Beast'] = {			-- or bestial loyalty, augmented call beast gear
	},
	['Call_Beast_Conditional'] = {
	},
	
	-- Reward potency, reward augment, reward enhancement, and MND gear
	['Reward'] = {
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
	
	['Pet_Attack'] = {					-- Pet's strength, not accuracy
	},
	['Pet_Attack_Conditional'] = {
	},

	['Pet_Macc'] = {					-- Pet's Magical Accuracy
	},
	['Pet_Macc_Conditional'] = {
	},
	
	['Pet_Matt'] = {					-- Pet's Magical Attack
	},
	['Pet_Matt_Conditional'] = {
	},

	--* DRK *--
	['WeaponBash'] = {
	},
	['WeaponBash_Conditional'] = {
	},
	
	--* DRG *--
	['Jumps'] = {		-- Jump and High Jump, Super is too high a level
	},
	['Jumps_Conditional'] = {
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
								*** Custom Sets Go below this comment ***
								
	The following "CAP" sets are added as a convenience for playing in level capped areas. The only way for them to be 
	loaded is via the /gearset command, which will turn GSwap off. If you're level syncing, pick the set that's closest 
	to the sync level and adjust accordingly.
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
        Hands = 'Battle Gloves',
        Ring1 = 'Courage Ring',
        Ring2 = 'Tamas Ring',
        Back = 'Lizard Mantle',
        Waist = 'Barbarian\'s Belt',
        Legs = 'Shep. Hose',
        Feet = 'Wonder Clomps',
    },
	
	['CAP40'] = {
        Main = 'Barbaroi Axe',
        Sub = 'War Pick',
        Ammo = 'Meat Broth',
        Head = 'Shep. Bonnet',
        Neck = 'Peacock Amulet',
        Ear1 = 'Beetle Earring',
        Ear2 = 'Beetle Earring',
        Body = 'Wonder Kaftan',
        Hands = 'Battle Gloves',
        Ring1 = 'Tamas Ring',
        Ring2 = 'Courage Ring',
        Back = 'Ram Mantle',
        Waist = 'Tilt Belt',
        Legs = 'Shep. Hose',
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
        Legs = 'Shep. Hose',
        Feet = 'Bounding Boots',
    },

	['CAP60'] = {
        Main = 'Darksteel Axe',
        Sub = 'Darksteel Buckler',
        Ammo = 'S. Herbal Broth',
        Head = 'Beast Helm',
        Neck = 'Peacock Amulet',
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
	
--[[
	The following set is used to dynamically create a gear set to be displayed once rather
	than in a piecemeal manner. It is hoped that this will cut down on flickering gear and
	possibly speed up the code. *** This set is to be left empty by the player ***. Please
	do not modify it.
--]]	
	['CurrentGear'] = { },	
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
profile.sjb = nil;			-- Tracks subjob name
profile.bAmmo = false;		-- BST specific. Is ammo equipped?
profile.sAmmo = nil;		-- BST specific. Name of ammo equipped

--[[
	HandlePetAction equips the appropriate gear set based on the type of action
	the pet is trying to perform.
--]]

local function HandlePetAction(PetAction)
	local pet = gData.GetPet();
	
	-- Only gear swap if this flag is true and the pet is a summoned pet
	if gcdisplay.GetToggle('GSwap') == false or string.find(gcinclude.MagicSkill['Summoning'],pet.Name) ~= nil then
		return;
	end

	-- Only BST pet attacks have associated gear sets because /smn pets are at most half the
	-- level of your BST level
	if (gcinclude.BstPetAttack:contains(PetAction.Name)) then				-- Pet Attack
		gcinclude.MoveToCurrent(sets.Pet_Attack,sets.CurrentGear);
		gcinclude.ProcessConditional(sets.Pet_Attack_Conditional,nil,sets.CurrentGear);
		-- If /acc enabled equip pet accuracy gear
		if gcdisplay.GetToggle('acc') == true then
			gcinclude.MoveToCurrent(sets.Pet_Accuracy,sets.CurrentGear);
			gcinclude.ProcessConditional(sets.Pet_Accuracy_Conditional,nil,sets.CurrentGear);			
		end
	elseif (gcinclude.BstPetMagicAttack:contains(PetAction.Name)) then		-- Pet Magical Attack
		gcinclude.MoveToCurrent(sets.Pet_Matt,sets.CurrentGear);
		gcinclude.ProcessConditional(sets.Pet_Matt_Conditional,nil,sets.CurrentGear);		
	elseif (gcinclude.BstPetMagicAccuracy:contains(PetAction.Name)) then	-- Pet Magical Accuracy Attack
		gcinclude.MoveToCurrent(sets.Pet_Macc,sets.CurrentGear);
		gcinclude.ProcessConditional(sets.Pet_Macc_Conditional,nil,sets.CurrentGear);		
    end
	gcinclude.EquipTheGear(sets.CurrentGear);
end

--[[
	SetSubjobSet is used to pick the appropriate set for the loaded macrobook based on
	which subjob is current. (If no change has occurred since the last time it was called,
	nothing is checked/changed.)
--]]

local function SetSubjobSet(chkSJ)
	-- "subs" is the key for what toolbar is shown. Each job listed in the array is a possible subjob.
	-- If the associated value for a subjob is greater than zero, that indicates what macro set from the
	-- macro book should be displayed. (The value associated with BST, your current main job, is nil
	-- since you can't be a bst/bst.) A value of 0 means the subjob is not configured. All other values
	-- imply that the subjob is expected and shows what macro set to show.
	local subs = {['WAR'] = 0, ['MNK'] = 0, ['WHM'] = 1, ['BLM'] = 3, ['RDM'] = 0, ['THF'] = 2,
				 ['PLD'] = 0, ['DRK'] = 0, ['BST'] = nil, ['BRD'] = 0, ['RNG'] = 0, ['SMN'] = 0,
				 ['SAM'] = 0, ['NIN'] = 4, ['DRG'] = 0, ['BLU'] = 0, ['COR'] = 0, ['PUP'] = 0,
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
	
	-- Coded order of operation override
	gcinclude.settings.priorityEngaged = 'BCEFGH';
	gcinclude.settings.priorityMidCast = 'ABCDEFGH';
	gcinclude.settings.priorityWeaponSkill = 'ABDE';	
	
	-- Determine if subjob uses magic and if the maximum MP is > 50.
	gcinclude.CheckMagic50(player);
	
	-- Set which macro book should be displayed. Which macro set within the macro book to
	-- display depends on what your subjob is.
	AshitaCore:GetChatManager():QueueCommand(1, '/macro book 10');		-- BST macro book
	SetSubjobSet(player.SubJob);
	
	-- Load up the weapons bar. (This need only be done once.)
	gcinclude.MoveToCurrent(sets.Start_Weapons,sets.CurrentGear);
	gcinclude.ProcessConditional(sets.Start_Weapons_Conditional,nil,sets.CurrentGear);
	gcinclude.EquipTheGear(sets.CurrentGear);
	
	-- Make sure the saved weapons are the starting weapons
	gcinclude.weapon = sets.CurrentGear['Main'];
	if sets.CurrentGear['Sub'] == nil then
		gcinclude.offhand = nil;
	else
		gcinclude.offhand = sets.CurrentGear['Sub'];
	end
end

--[[
	OnUnload is run when you change to another job
--]]

profile.OnUnload = function()
	gcinclude.Unload();
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
	of in gcinclude.HandleCommands are specific to PLD or the help system.
--]]

profile.HandleCommand = function(args)
	if args[1] == 'help' then
		gcdisplay.ShowHelp();
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
	local player = gData.GetPlayer();
	local zone = gData.GetEnvironment();	
	local ew = gData.GetEquipment();
	local eWeap = nil;
	local cKey;
	
	-- Make sure that the global magic settings for the player are known. The secoond clause in
	-- the if statement takes care of a bizarre case. Turns out if you change the player.MainJob
	-- from a job where there is not a luashitacast script, it initially remembers the old main
	-- job. by including the second call, a subsequent invocation occurs getting it right.
	if gcinclude.settings.bMagicCheck == false  or gcinclude.settings.sMJ ~= player.MainJob then
		gcinclude.CheckMagic50(player);
	end
	
	-- Only pet actions from BST are supported.
	if (petAction ~= nil and player.MainJob == 'BST') then
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
		return;
	end
	
	-- When you want to reward your pet and you do not have pet food equipped or when you 
	-- want to summon a pet and a jug is not equipped, the current item in the ammo slot 
	-- is saved. The following will set it back to what you had before either of those two 
	-- items were equipped.
	if profile.bAmmo then
		gFunc.ForceEquip('Ammo',profile.sAmmo);
		profile.sAmmo = nil;
		profile.bAmmo = false;
	end

	-- Clear out the CurrentGear in case of leftovers
	gcinclude.ClearSet(sets.CurrentGear);
	
	-- If player is not resting and has swapped weapons, set the weapon back to what 
	-- they had before the switch

	if player.Status ~= 'Resting' and 
		gcdisplay.GetToggle('WSwap') == true and 	
		gcinclude.weapon ~= nil and 
		eWeap ~= gcinclude.weapon then
		sets.CurrentGear['Main'] = gcinclude.weapon;
		sets.CurrentGear['Sub'] = gcinclude.offhand;
	end

	-- The default set is the TP gear set. Load it up
	gcinclude.MoveToCurrent(sets.TP,sets.CurrentGear);
	gcinclude.ProcessConditional(sets.TP_Conditional,nil,sets.CurrentGear);	

	if gcdisplay.GetToggle('Tank') == true then
		gcinclude.MoveToCurrent(sets.TP_Tank,sets.CurrentGear);
		gcinclude.ProcessConditional(sets.TP_Tank_Conditional,nil,sets.CurrentGear);	
	end
	
	-- Now process the player status accordingly
	if (player ~= nil and player.Status == 'Engaged') or (pet ~= nil and pet.Status == 'Engaged') then
		gcinclude.settings.priorityEngaged = string.upper(gcinclude.settings.priorityEngaged);
		for i = 1,string.len(gcinclude.settings.priorityEngaged),1 do
			cKey = string.sub(gcinclude.settings.priorityEngaged,i,i);
			if cKey == 'B' then		-- Pet (if out) is fighting
				if pet ~= nil and pet.Status == 'Engaged' then
					gcinclude.MoveToCurrent(sets.TP_Pet,sets.CurrentGear);
					gcinclude.ProcessConditional(sets.TP_Pet_Conditional,nil,sets.CurrentGear);
				end
			elseif cKey == 'C' then		-- Evasion			
				if gcdisplay.GetToggle('Eva') == true then
					gcinclude.MoveToCurrent(sets.Evasion,sets.CurrentGear);
					gcinclude.ProcessConditional(sets.Evasion_Conditional,nil,sets.CurrentGear);
				end
			elseif cKey == 'E' then		-- Accuracy	
				if gcdisplay.GetToggle('Acc') == true then 
					gcinclude.MoveToCurrent(sets.Accuracy,sets.CurrentGear);
					gcinclude.ProcessConditional(sets.Accuracy_Conditional,nil,sets.CurrentGear);
					if pet ~= nil and pet.Status == 'Engaged' then
						gcinclude.MoveToCurrent(sets.Pet_Accuracy,sets.CurrentGear);
						gcinclude.ProcessConditional(sets.Pet_Accuracy_Conditional,nil,sets.CurrentGear);						
					end
				end
			elseif cKey == 'F' then		-- Kiting
				if (gcdisplay.GetToggle('Kite') == true) then
					gcinclude.MoveToCurrent(sets.Movement,sets.CurrentGear);
					gcinclude.ProcessConditional(sets.Movement_Conditional,nil,sets.CurrentGear);
				end				
			elseif cKey == 'G' then		-- common buffs/debuffs
				gcinclude.CheckCommonDebuffs();
			elseif cKey == 'H' then		-- Damage Taken gear
				if (gcdisplay.GetCycle('DT') ~= gcinclude.OFF) then
					if gcdisplay.GetCycle('DT') == 'Physical' then
						gcinclude.MoveToCurrent(sets.DT_Physical,sets.CurrentGear);
						gcinclude.ProcessConditional(sets.DT_Physical_Conditional,nil,sets.CurrentGear);						
					elseif gcdisplay.GetCycle('DT') == 'Magical' then
						gcinclude.MoveToCurrent(sets.DT_Magical,sets.CurrentGear);
						gcinclude.ProcessConditional(sets.DT_Magical_Conditional,nil,sets.CurrentGear);
					elseif gcdisplay.GetCycle('DT') == 'Breath' then
						gcinclude.MoveToCurrent(sets.DT_Breath,sets.CurrentGear);
						gcinclude.ProcessConditional(sets.DT_Breath_Conditional,nil,sets.CurrentGear);						
					end
				end
			end
		end
	elseif player.Status == 'Resting' then
		-- Player kneeling. Priority (low to high): regen,refresh
		gcinclude.MoveToCurrent(sets.Resting_Regen,sets.CurrentGear);
		gcinclude.ProcessConditional(sets.Resting_Regen_Conditional,nil,sets.CurrentGear);
		if gcinclude.settings.bMagic == true and player.MP < player.MaxMP then		
			gcinclude.MoveToCurrent(sets.Resting_Refresh,sets.CurrentGear);
			gcinclude.ProcessConditional(sets.Resting_Refresh_Conditional,nil,sets.CurrentGear);
		end
		
		-- Weapon swap to a weapon that refreshes MP if player's subjob uses magic, weapon swapping
		-- is enabled (/wswap) and their MP is not at maximum
		if gcdisplay.GetToggle('WSwap') == true and gcinclude.settings.bSJ == true and player.MP < player.MaxMP then
			if gcinclude.settings.bStave == false then
				gcinclude.CheckForStaves();
			end
			if player.MainJobLevel < 51 then
				gcinclude.MoveToCurrent(sets.Resting_Refresh_Weapon_Sub51,sets.CurrentGear);
				gcinclude.ProcessConditional(sets.Resting_Refresh_Weapon_Sub51_Conditional,nil,sets.CurrentGear);
			else
				gcinclude.SwapToStave('dark',false,sets.CurrentGear);
			end
		end
		-- Check for common debuffs
		gcinclude.CheckCommonDebuffs();
	else
		-- Assume idling. There's no idle set, just idle conditions. Please note if there
		-- is a pet, it is assumed to be a BST pet and not a /SMN pet. If it is a /SMN
		-- pet, no special sets are loaded
		
		-- See if in a town
		if zone.Area ~= nil and gcinclude.Towns:contains(zone.Area) then
			gcinclude.MoveToCurrent(sets.Town,sets.CurrentGear);
			gcinclude.ProcessConditional(gcinclude.sets.Town_Conditional,nil,sets.CurrentGear);
		end
		-- if the player's HP is below the threshold setting, equip the idle regen gear
		if player.HPP < gcinclude.settings.RegenGearHPP then
			gcinclude.MoveToCurrent(sets.Idle_Regen,sets.CurrentGear);
			gcinclude.ProcessConditional(sets.Idle_Regen_Conditional,nil,sets.CurrentGear);
		end
		-- if the player's MP is below the threshold setting, equip the idle refresh gear				
		if gcinclude.settings.bSJ == true and player.MPP < gcinclude.settings.RefreshGearMPP then		-- if the player's MP is below the threshold setting, equip the idle refresh gear
			gcinclude.MoveToCurrent(sets.Idle_Refresh,sets.CurrentGear);
			gcinclude.ProcessConditional(sets.Idle_Refresh_Conditional,nil,sets.CurrentGear);
		end
		-- Check for common debuffs
		gcinclude.CheckCommonDebuffs();
	end
	
	gcinclude.EquipTheGear(sets.CurrentGear);		-- Equip the composited HandleDefault set
	
	-- Lastly, update the display, just in case
	gcdisplay.Update();
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
	
	-- Store the name of the ammo. This is used when the ammo slot is automatically
	-- populated so that the original ammo can be re-equipped.
	if eq.Ammo ~= nil then
		profile.sAmmo = eq.Ammo.Name;
	else
		profile.sAmmo = nil;
	end

	-- Only gear swap if this flag is true
	if gcdisplay.GetToggle('GSwap') == False then
		return;
	end

	-- Clear out the CurrentGear in case of leftovers
	gcinclude.ClearSet(sets.CurrentGear);
	
	-- Now process the appropriate job ability. 
	-- Start with abilities associated with BST
	if string.match(ability.Name, 'Call Beast') or string.match(ability.Name, 'Bestial Loyalty') then
		-- First make sure player wants the automated jug pet funtionality
		if gcdisplay.GetToggle('AJug') == true then
			-- Ok, now see if a jug pet already equipped
			local bJugFound = profile.bAmmoIsJug(profile.sAmmo);
			if bJugFound == nil or (bJugFound ~= nil and bJugFound == false) then
				profile.bAmmo = profile.findMaxEquipableJugPet();
			end
		end
		gcinclude.MoveToCurrent(sets.Call_Beast,sets.CurrentGear);
		gcinclude.ProcessConditional(sets.Call_Beast_Conditional,nil,sets.CurrentGear);
	elseif string.match(ability.Name, 'Reward') then
		-- Pet reward. Make sure that pet food already equipped
		if profile.sAmmo == nil or string.find(string.lower(profile.sAmmo),'pet f') == nil then		-- something else equipped
			profile.bAmmo = gcinclude.doPetFood('max',nil);
		end
		gcinclude.MoveToCurrent(sets.Reward,sets.CurrentGear);
		gcinclude.ProcessConditional(sets.Reward_Conditional,nil,sets.CurrentGear);		
	elseif string.match(ability.Name, 'Ready') or string.match(ability.Name, 'Sic') then
		-- Pet "Ready!" or "Sic!" command invoked. Check to see if /acc enabled
		if gcdisplay.GetToggle('acc') == true then
			gcinclude.MoveToCurrent(sets.Pet_Accuracy,sets.CurrentGear);
			gcinclude.ProcessConditional(sets.Pet_Accuracy_Conditional,nil,sets.CurrentGear);			
		end
	elseif string.match(ability.Name, 'Tame') then
		-- Trying to tame a beast. (Someone's charm failed.)
		gcinclude.MoveToCurrent(sets.Tame,sets.CurrentGear);
		gcinclude.ProcessConditional(sets.Tame_Conditional,nil,sets.CurrentGear);		
	elseif string.match(ability.Name, 'Charm') then
		-- Trying to charm a beast. 
		gcinclude.MoveToCurrent(sets.Charm,sets.CurrentGear);
		gcinclude.ProcessConditional(sets.Charm_Conditional,nil,sets.CurrentGear);
		
		-- If weapon swapping is allowed, equip a light/apollo staff (if you have one)
	
		if gcdisplay.GetToggle('WSwap') == true then
			if gcinclude.settings.bStave == false then
				gcinclude.CheckForStaves();
			end	
			if gcinclude.settings.bStave == true then
				gcinclude.SwapToStave('light',false,sets.CurrentGear);
			end
		end
	elseif string.match(ability.Name, 'Weapon Bash') then		-- assumes /drk
		gcinclude.MoveToCurrent(sets.WeaponBash,sets.CurrentGear);
		gcinclude.ProcessConditional(sets.WeaponBash_Conditional,nil,sets.CurrentGear);		
	elseif string.find(ability.Name, 'Jump') then		-- assumes /drk
		gcinclude.MoveToCurrent(sets.Jumps,sets.CurrentGear);
		gcinclude.ProcessConditional(sets.Jumps_Conditional,nil,sets.CurrentGear);
	else
--[[
		Abilities associated with subjobs go here. The following subjobs have
		no ability entries because of lack of gear or just doesn't make sense: 
		DRK,SMN,PLD(out of era),WAR,MNK,WHM,BLM,RDM,BRD,RNG,SAM,DRG,THF
		
		Note: for /THF, sneak attack gets no bonus from DEX and trick attack gets
		no bonus from AGI
--]]
	end
	gcinclude.EquipTheGear(sets.CurrentGear);		-- Equip the composited HandleAbility set
end

--[[
	HandleItem is the place to equip gear when a special item is used. Currently only 'Holy Water' 
	is supported
--]]

profile.HandleItem = function()
	local item = gData.GetAction();

	-- Clear out the CurrentGear in case of leftovers
	gcinclude.ClearSet(sets.CurrentGear);
	
	if gcdisplay.GetToggle('GSwap') == true then		-- Only gear swap if this flag is true
		if string.match(item.Name, 'Holy Water') then 
			gcinclude.MoveToCurrent(gcinclude.sets.Holy_Water,sets.CurrentGear);
			gcinclude.ProcessConditional(gcinclude.sets.Holy_Water_Conditional,nil,sets.CurrentGear);
			gcinclude.EquipTheGear(sets.CurrentGear);	-- if more items are added, move this to addess all
		end
	end
end

--[[
	HandlePrecast is invoked when the player casts a spell. It is the first step of two where you load any
	Fast Cast, cast time reduction, and quick cast gear in anticipation of a spell
--]]

profile.HandlePrecast = function()
    local spell = gData.GetAction();
	local obi;
	local mSet;
	
	-- Only process if /gswap is turned on
	if gcdisplay.GetToggle('GSwap') == false then
		return;
	end
	
	-- Equip the precast gear set
	gFunc.EquipSet(sets.Precast);
		
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
end

--[[
	HandleMidcast is the second function invoked when a player casts a spell. It equips gear appropriate for 
	magic skill, duration, magic attack bonus, magic accuracy, and potency. There's an order to how the pieces 
	are loaded: INT/MND, spell specific, macc, magic skill, obi, ele swap	
--]]

profile.HandleMidcast = function()
	local player = gData.GetPlayer();
	local spell = gData.GetAction();
	local obi;
	local sSet;
	local cKey;

	if gcdisplay.GetToggle('GSwap') == false then		-- Only gear swap if this flag is true	
		return;
	end

	-- Clear out the CurrentGear in case of leftovers
	gcinclude.ClearSet(sets.CurrentGear);
	
	gcinclude.settings.priorityMidCast = string.upper(gcinclude.settings.priorityMidCast);
	for i = 1,string.len(gcinclude.settings.priorityMidCast),1 do
		cKey = string.sub(gcinclude.settings.priorityMidCast,i,i);
	
		if cKey == 'A' then				-- midcast gear	
			gcinclude.MoveToCurrent(sets.Midcast,sets.CurrentGear);
			gcinclude.ProcessConditional(sets.Midcast_Conditional,nil,sets.CurrentGear);
		elseif cKey == 'B' then			-- Spell Interruption Rate gear
			gcinclude.MoveToCurrent(sets.SIR,sets.CurrentGear);
			gcinclude.ProcessConditional(sets.SIR_Conditional,nil,sets.CurrentGear);			
		elseif cKey == 'C' then			-- INT/MND gear?
			sSet = gcinclude.WhichStat(spell.Name);
			if sSet ~= nil then
				if sSet == 'MND' then
					gcinclude.MoveToCurrent(sets.MND,sets.CurrentGear);
					gcinclude.ProcessConditional(sets.MND_Conditional,nil,sets.CurrentGear);
				elseif sSet == 'INT' then
					gcinclude.MoveToCurrent(sets.INT,sets.CurrentGear);
					gcinclude.ProcessConditional(sets.INT_Conditional,nil,sets.CurrentGear);
				end
			end
		elseif cKey == 'D' then			-- Magic Skill Type
			mSet = gcinclude.WhichMagicSkill(spell.Name);
			if mSet ~= nil then
				if mSet == 'Healing' then
					gcinclude.MoveToCurrent(sets.Healing,sets.CurrentGear);
					gcinclude.ProcessConditional(sets.Healing_Conditional,nil,sets.CurrentGear);					
				elseif mSet == 'Dark' then
					gcinclude.MoveToCurrent(sets.Dark,sets.CurrentGear);
					gcinclude.ProcessConditional(sets.Dark_Conditional,nil,sets.CurrentGear);
				elseif mSet == 'Divine' then
					gcinclude.MoveToCurrent(sets.Divine,sets.CurrentGear);
					gcinclude.ProcessConditional(sets.Divine_Conditional,nil,sets.CurrentGear);					
				elseif mSet == 'Enfeebling' then
					gcinclude.MoveToCurrent(sets.Enfeebling,sets.CurrentGear);
					gcinclude.ProcessConditional(sets.Enfeebling_Conditional,nil,sets.CurrentGear);
				elseif mSet == 'Enhancing' then
					gcinclude.MoveToCurrent(sets.Enhancing,sets.CurrentGear);
					gcinclude.ProcessConditional(sets.Enhancing_Conditional,nil,sets.CurrentGear);					
				elseif mSet == 'Elemental' then
					gcinclude.MoveToCurrent(sets.Elemental,sets.CurrentGear);				
					gcinclude.ProcessConditional(sets.Elemental_Conditional,nil,sets.CurrentGear);
				elseif mSet == 'Ninjitsu' then
					gcinclude.MoveToCurrent(sets.Ninjitsu,sets.CurrentGear);
					gcinclude.ProcessConditional(sets.Ninjitsu_Conditional,nil,sets.CurrentGear);					
				elseif mSet == 'Summoning' then
					gcinclude.MoveToCurrent(sets.Summoning,sets.CurrentGear);
					gcinclude.ProcessConditional(sets.Summoning_Conditional,nil,sets.CurrentGear);					
				end
				-- See if Magic Attack Bonus useful. Note: Spells like bio/dia initial damage is
				-- affected by MAB, but not the tic. Not worth trying to support here. Also, Ninjitsu
				-- is affected by Ninjitsu Magic Attack Bonus and not just Magic Bonus.
				if string.find('Healing,Enfeebling,Enhancing,Ninjitsu,Summoning',mSet) == nil then
					gcinclude.MoveToCurrent(sets.MAB,sets.CurrentGear);
					gcinclude.ProcessConditional(sets.MAB_Conditional,nil,sets.CurrentGear);
				end				
			end
		elseif cKey == 'E' then			--Magical accuracy
			if gcdisplay.GetToggle('acc') == true then
				gcinclude.MoveToCurrent(sets.Macc,sets.CurrentGear);
				gcinclude.ProcessConditional(sets.macc_Conditional,nil,sets.CurrentGear);
			end
		elseif cKey == 'F' then			-- Spell specific gear
		if string.match(spell.Name, 'Stoneskin') then
				gcinclude.MoveToCurrent(sets.Stoneskin,sets.CurrentGear);
				gcinclude.ProcessConditional(sets.Stoneskin_Conditional,nil,sets.CurrentGear);				
			elseif string.match(spell.Name, 'Drain') then
				gcinclude.MoveToCurrent(sets.Drain,sets.CurrentGear);
				gcinclude.ProcessConditional(sets.Drain_Conditional,nil,sets.CurrentGear);				
			elseif string.match(spell.Name, 'Aspir') then
				gcinclude.MoveToCurrent(sets.Aspir,sets.CurrentGear);
				gcinclude.ProcessConditional(sets.Aspir_Conditional,nil,sets.CurrentGear);
			elseif string.match(spell.Name, 'Sneak') then
				gcinclude.MoveToCurrent(sets.Sneak,sets.CurrentGear);
				gcinclude.ProcessConditional(sets.Sneak_Conditional,nil,sets.CurrentGear);
			elseif string.match(spell.Name, 'Invisible') then
				gcinclude.MoveToCurrent(sets.Invisible,sets.CurrentGear);
				gcinclude.ProcessConditional(sets.Invisible_Conditional,nil,sets.CurrentGear);
			end
		elseif cKey == 'G' then				-- Elemental Obi	
			if gcinclude.settings.bEleObis == false then
				gcinclude.CheckForObisGorgets();
			end	
			if gcinclude.settings.bEleObis == true then
				obi = gcinclude.CheckEleSpells(spell.Name,gcinclude.MagicEleDmg,gcinclude.OBI);
				if obi ~= nil then
					sets.CurrentGear['Waist'] = obi;
				end
			end
		elseif cKey == 'H' then				-- Elemental Stave	
			if gcinclude.settings.bStave == false then
				gcinclude.CheckForStaves();
			end
			if gcdisplay.GetToggle('WSwap') == true and gcinclude.settings.bEleStaves == true then
				if mSet == 'Summoning' then
					stat = gcinclude.CheckSummons(spell.Name);
				else
					stat = gcinclude.CheckEleSpells(spell.Name,gcinclude.MagicEleDmg,gcinclude.ELEMENT);
				end
			end
		
			if stat ~= nil then
				gcinclude.SwapToStave(stat,false,sets.CurrentGear);
			end
			stat = nil;	
		end
	end
	gcinclude.EquipTheGear(sets.CurrentGear);		-- Equip the composited midcast set
end

--[[
	HandlePreshot is similar to HandlePrecast, but for ranged actions. It loads Ranged Accuracy 
	and Ranged Shot Speed Gear for a ranged attack
--]]

profile.HandlePreshot = function()
	if gcdisplay.GetToggle('GSwap') == true then		-- Only gear swap if this flag is true
		-- Clear out the CurrentGear in case of leftovers
		gcinclude.ClearSet(sets.CurrentGear);	
		
		gcinclude.MoveToCurrent(sets.Preshot,sets.CurrentGear);
		gcinclude.ProcessConditional(sets.Preshot_Conditional,nil,sets.CurrentGear);
		gcinclude.EquipTheGear(sets.CurrentGear);
	end
end

--[[
	HandleMidshot is similar to HandleMidcast, but for ranged actions. It loads Ranged Attack 
	and Damage gear for a ranged attack
--]]

profile.HandleMidshot = function()
	-- Only gear swap if this flag is true
	if gcdisplay.GetToggle('GSwap') == false then
		return;
	end

	-- Clear out the CurrentGear in case of leftovers
	gcinclude.ClearSet(sets.CurrentGear);
	
	gcinclude.MoveToCurrent(sets.Midshot,sets.CurrentGear);
	gcinclude.ProcessConditional(sets.Midshot_Conditional,nil,sets.CurrentGear);

	-- Equip the composited Midshot set
	gcinclude.EquipTheGear(sets.CurrentGear);
end

--[[
	HandleWeaponskill loads the gear appropriately for the weapon skill you're doing
--]]

profile.HandleWeaponskill = function()
	local ws = gData.GetAction();
	local canWS = gcinclude.CheckWsBailout();
	local cKey;
	
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
	
 	gcinclude.settings.priorityWeaponSkill = string.upper(gcinclude.settings.priorityWeaponSkill);
	for i = 1,string.len(gcinclude.settings.priorityWeaponSkill),1 do
		cKey = string.sub(gcinclude.settings.priorityWeaponSkill,i,i);
		if cKey == 'A' then			-- weaponskill set
			local sWS = gcinclude.WsStat(ws.Name,'STR');
			
			if sWS == 'WS_CHR' then
				gcinclude.MoveToCurrent(sets.WS_CHR,sets.CurrentGear);
				gcinclude.ProcessConditional(sets.WS_CHR_Conditional,nil,sets.CurrentGear);
			elseif sWS == 'WS_DEX' then
				gcinclude.MoveToCurrent(sets.WS_DEX,sets.CurrentGear);
				gcinclude.ProcessConditional(sets.WS_DEX_Conditional,nil,sets.CurrentGear);
			elseif sWS == 'WS_DEXINT' then
				gcinclude.MoveToCurrent(sets.WS_DEXINT,sets.CurrentGear);
				gcinclude.ProcessConditional(sets.WS_DEXINT_Conditional,nil,sets.CurrentGear);
			elseif sWS == 'WS_STR' then
				gcinclude.MoveToCurrent(sets.WS_STR,sets.CurrentGear);
				gcinclude.ProcessConditional(sets.WS_STR_Conditional,nil,sets.CurrentGear);
			elseif sWS == 'WS_MND' then
				gcinclude.MoveToCurrent(sets.WS_MND,sets.CurrentGear);
				gcinclude.ProcessConditional(sets.WS_MND_Conditional,nil,sets.CurrentGear);
			elseif sWS == 'WS_STRDEX' then
				gcinclude.MoveToCurrent(sets.WS_STRDEX,sets.CurrentGear);
				gcinclude.ProcessConditional(sets.WS_STRDEX_Conditional,nil,sets.CurrentGear);
			elseif sWS == 'WS_STRMND' then
				gcinclude.MoveToCurrent(sets.WS_STRMND,sets.CurrentGear);
				gcinclude.ProcessConditional(sets.WS_STRMND_Conditional,nil,sets.CurrentGear);
			elseif sWS == 'WS_STRINT' then
				gcinclude.MoveToCurrent(sets.WS_STRINT,sets.CurrentGear);
				gcinclude.ProcessConditional(sets.WS_STRINT_Conditional,nil,sets.CurrentGear);
			elseif sWS == 'WS_STRINT_30_20' then
				gcinclude.MoveToCurrent(sets.WS_STRINT_30_20,sets.CurrentGear);
				gcinclude.ProcessConditional(sets.WS_STRINT_30_20_Conditional,nil,sets.CurrentGear);
			elseif sWS == 'WS_STRVIT' then
				gcinclude.MoveToCurrent(sets.WS_STRVIT,sets.CurrentGear);
				gcinclude.ProcessConditional(sets.WS_STRVIT_Conditional,nil,sets.CurrentGear);
			elseif sWS == 'WS_Skill' then
				gcinclude.MoveToCurrent(sets.WS_Skill,sets.CurrentGear);
				gcinclude.ProcessConditional(sets.WS_Skill_Conditional,nil,sets.CurrentGear);
			elseif sWS == 'WS_HP' then
				gcinclude.MoveToCurrent(sets.WS_HP,sets.CurrentGear);			
				gcinclude.ProcessConditional(sets.WS_HP_Conditional,nil,sets.CurrentGear);				
			end
		elseif cKey == 'B' then		-- elemental gorget	
			if gcinclude.settings.bEleGorgets == false then
				gcinclude.CheckForObisGorgets();
			end			
			if gcinclude.settings.bEleGorgets == true then
				local sGorget = gcinclude.CheckEleGorget(ws.Name);
				if sGorget ~= nil then
					sets.CurrentGear['Neck'] = sGorget;
				end
			end
		elseif cKey == 'D' then		-- accuracy	
			if gcdisplay.GetToggle('acc') == true then
				gcinclude.MoveToCurrent(sets.Accuracy,sets.CurrentGear);
				gcinclude.ProcessConditional(sets.Accuracy_Conditional,nil,sets.CurrentGear);
			end
		elseif cKey == 'E' then		-- elemental obi
--[[
			If the weaponskill is elemental and is closing a skillchain, then if the
			conditions for equipping an elemental obi are advantageous, it should be
			equipped now. Unfortunately I have no idea how to detect the closing of
			a skillchain and the automatic equipping of an elemental obi could 
			adversely affect the damage, so this section is not implemented. If I can
			ever figure out how to detect closing a skillchain, I will readdress this.
															- CCF, 1/12/2024
--]]	
		end
	end
	
	-- Special case(s) for specific weapon skills go here
	ws.Name = string.lower(ws.Name);
	if string.find('red lotus blade,sanguine blade',ws.Name) ~= nil then
		gcinclude.MoveToCurrent(sets.MAB,sets.CurrentGear);
		gcinclude.ProcessConditional(sets.MAB_Conditional,nil,sets.CurrentGear);	
	end

	-- Equip the composited weaponskill set		
	gcinclude.EquipTheGear(sets.CurrentGear);
end

return profile;