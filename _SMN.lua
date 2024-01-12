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
	The gear sets are usually defined by a pair of sets: the "main" one and an associated "conditional" set. The
	"main" is loaded when appropriate and the conditional is processed to see if any of the entries should be
	equipped too. ("Conditional" entries consist of gear that need to meet certain conditions before they will be
	equipped.) "main" sets contain your standard gear slot='gear piece' combinations and the "conditional" entries
	consist of a gear ID (defined in "Conditional gear master list.txt" found in the ./common directory), the name 
	of the piece of gear, and a description of the condition that must be true. Entries in the "conditional" set 
	should Just be copied from the master list file.
	
	It is recommended that "main" sets not include any gear found in the top line of your equipment grid (main hand,
	off hand, ranged weapon, ammo). Doing so will mean that TP will be reset to 0 whenever gear is changed which can
	be very frustrating. Further, any time you do change a weapon, it will convert back to what was defined in a set.
	Believe me, it's no fun	fighting Luashitacast!

	Also, not all sets need to be defined. There is nothing wrong with leaving a set "empty", but don't delete any
	of the sets. All the ones listed here (except for any custom sets) are expected to exist by Luashitacast.
	
	*** Note ***
	
	If you use a piece of gear in one of of your common gear sets (e.g., Idle, TP, etc) that restricts you from using 
	another specific armor slot, if in a subsequent gear set you specify gear for the slot that was restricted, it
	is recommended that you also replace the other piece of gear too. (In some cases gear will keep swapping as 
	Luashitacast fights the FFXI client. In other cases, the other slot will become unequipped. This is just a normal
	behavior of the equipment grid, but it can be disconcerting.

	Example: body='Vermillion cloak', head is restricted. Next set equips 'austere hat'. You should also replace the body
	piece. head='austere hat', body='austere robe'.
			
	*** Aside ***
	If the piece of gear that restricts an additional slot is from a "conditional" set where /gswap is turned off,
	you don't have to worry about it. When /gswap is turned on that conditional piece will be removed.
	
	*** Note 2 ***
	Unlike when summoner is used as a subjob, /bst's pets are charmed at the max level of your BST or the level
	of your SMN, whichever is lower. That means you can charm higher level mobs than you would expect with /bst.
	Just note though that you can't have two pets, so if you have charmed a pet with /bst, you can't summon your
	avatar and visa versa.
--]]

--[[
	The "Idle" set is what your character will wear when it is not fighting nor resting nor in town. Whether 
	just standing out of town or going to a different area, the "Idle" set will be equipped.
	
	If you have an avatar out though, the "Idle_With_Pet" set will be equipped since you will need avatar
	perpetuation costs down gear and things like that. If that pet is Carbuncle, then you want
	Idle_With_Carbuncle.
	
--]]

	['Idle'] = {
		--Neck = 'Fenrir\'s Torque',				-- Daytime: 30MP, Nighttime: -3 enmity
        Ear1 = 'Black Earring',					-- +4 MP
        Ear2 = 'Bat Earring',					-- +5 MP, blinded: evasion +15
        Body = 'Vermillion Cloak',				-- No head allowed, adds "refresh" effect
        Hands = 'Carbuncle Mitts',				-- +14 MP, 1/2 perpetuation cost on carbuncle
        Ring1 = 'Evoker\'s Ring',				-- summoning magic skill +10, avatar perpetuation cost -1
		Ring2 = 'Tamas Ring',					-- magic accuracy +5, enmity -5
        Back = 'Blue Cape',
		Waist = 'Hierarch Belt',				-- +2 MP while healing, +2 hp while healing
        Legs = 'Evoker\'s Spats',				-- +15 MP, avatar: enhances accuracy, enmity -2
        Feet = 'Mannequin Pumps',				-- +12 MP
	},
	['Idle_Conditional'] = {
	},
	
	['Idle_With_Pet'] = {
        Head = 'Austere Hat',					-- ability delay -2, summoning skill +2
		Body = 'Austere Robe',					-- avatar perpetuation cost -1, ability delay -3
        Hands = 'Carbuncle Mitts',				-- +14 MP, 1/2 perpetuation cost on carbuncle
        Ring1 = 'Evoker\'s Ring',				-- summoning magic skill +10, avatar perpetuation cost -1
    },
	['Idle_With_Pet_Conditional'] = {			-- Conjurer's ring seems the only possibility
	},
	
	['Idle_With_Carbuncle'] = {
		Body = 'Vermillion Cloak',				-- No head allowed, adds "refresh" effect
		Hands = 'Carbuncle Mitts',				-- +14 MP, 1/2 perpetuation cost on carbuncle
	},
	['Idle_With_Carbuncle_Conditional'] = {
	},	
	
	--[[
		The Idle_Regen and Idle_Refresh gear sets replace the normal Idle set when the player's HP or MP
		go below a set percentage (defined in gcinclude.lua, but can be overriden in profile.OnLoad function).
	--]]
	
	['Idle_Regen'] = {
	},
	['Idle_Regen_Conditional'] = {
	},
	
	['Idle_Refresh'] = {
        Body = 'Vermillion Cloak',				-- adds "refresh" effect
	},
	['Idle_Refresh_Conditional'] = {
	},
	
	--[[
		When you are resting (kneeling down), your HP 'Resting' set will be equipped. If your subjob
		uses MP and your MP is below the set threshhold (defined by gcinclude.settings.RefreshGearMP), 
		your MP 'Resting_Refresh' gear set will be equipped. Regardless of which set is equipped, 
		assuming that your subjob uses magic, you have a Dark/Pluto staff accessible, weapon swapping 
		is enabled (/wswap), and your MP is not at maximum, the Dark/Pluto staff will automatically be 
		equipped.
	--]]
	
	['Resting'] = {
        Waist = 'Hierarch Belt',		-- +2 HP while healing
    },
	
	['Resting_Refresh'] = {
        Body = 'Vermillion Cloak',		-- adds "refresh" effect
		Waist = 'Hierarch Belt',		-- +2 MP while healing
	},
	['Resting_Refresh_Conditional'] = {
	},

	-- If you have any Spell Interruption Rate down gear, put them into the "SIR" gear set.
	-- This gear set is equipped in the HandleMidcast function that all spells go through.
	['SIR'] = {
	},
	
	-- Blood pacts go through a simulated process that mimics spell casting. The precast
	-- happens when the blood page is invoked (either rage or ward), loading the 'BP'
	-- gear set. You want gear that has Blood Pact Ability Delay, Blood Pact Recast, and
	-- avatar perpetuation cost abilities defined here. (Once summoning skill is working
	-- on HorizonXI, that is also a desirable attribute.) The midcast happens when the
	-- actual blood pact goes off.
	['BP'] = {
        Head = 'Austere Hat',				-- ability delay -2, summoning skill +2
        --Neck = 'Smn. Torque',				-- summoning skill +7
        Body = 'Austere Robe',				-- avatar perpetuation cost -1, ability delay -3
        Hands = 'Carbuncle Mitts',			-- halfs perpetuation cost of carbuncle
        Ring1 = 'Evoker\'s Ring',			-- summoning skill +10, avatar perpetuation cost -1
    },
	['BP_Conditional'] = {
	},
	
	--[[
		Rage blood pacts are devided by type: physical, magical,summoning skill,accuracy, 
		and hybrid. (Ward blood pacts do not have this type of distiction. Each blood pact 
		though is of a fixed type and can be looked up. The following gear sets named
		SmnXXX where XXX is the type define the gear to be equipped when the blood pact
		goes off. Look to the specific gear set type for what gear stats are wanted.
	--]]
	
	-- Physical rage blood pact: pet attack, pet accuracy, pet critical hit, blood pact 
	-- physical damage
	['SmnPhysical'] = {
	    Head = 'Shep. Bonnet',				-- pet accuracy +5
		Ear2 = 'Beastly Earring',			-- pet accuracy +10
    },

	-- Magical rage blood pact: pet magic attack burst, pet magical attack, pet magical
	-- accuracy, and blood Pact magical damage
	['SmnMagical'] = {
	    Head = 'Shep. Bonnet',				-- pet magical accuracy +3
    },

	-- Summoning skill rage blood pact. Currently HorizonXI ignores summoning skill. As a 
	-- result the gear in this set is commented out
	['SmnSkill'] = {
        -- Head = 'Austere Hat',		-- +2 Summoning Skill
        -- Neck = 'Smn. Torque',		-- +7 Summoning Skill
        -- Ring1 = 'Evoker\'s Ring',	-- +10 Summoning Skill
    },
	
	-- Accuracy blood pact: pet accuracy, Pet magic accuracy
    ['SmnAccuracy'] = {
        Head = 'Shep. Bonnet',		-- +5 Pet Accuracy, +3 Pet Magic Accuracy
		Ear2 = 'Beastly Earring',	-- +10 Pet Accuracy
    },
	
	-- Hybrid blood pact: 2x physical and 1x magical
    ['SmnHybrid'] = {
		 Head = 'Shep. Bonnet',				-- pet accuracy +5, pet magical accuracy +3
		Ear2 = 'Beastly Earring',			-- pet accuracy +10
    },
	
--[[
	Start weapons are where you define what you want the first row of equipment to look like when you
	either log in as a SMN or you switch your main job to SMN. Any other gear you mention will be overridden
	by the Idle or Town set, so no need to include here.
--]]

	['Start_Weapons'] = {
	    Main = 'Light Staff',
		Ammo = 'Fortune Egg',
 	},

--[[
	What do you want to wear around town? You can define a full set or just an item or two, it is up to you.
	(Please note that a nation's aketon is considered conditional gear, so no need to place here unless you
	want the aketon equipped regardless if it is your home nation's city or not. Due to some of the 
	complexities of lua, the Town_Conditional set is found in the gcinclude.lua file.)
--]]
	
	['Town'] = {
        Head = 'Lilac Corsage',
		Body = 'Austere Robe',
    },
	
--[[
	Damage reduction gear depends on the type of damage. The most common is physical, but there's times when
	you'll want to reduce magic damage or breath damage. The three gear sets are defined below. The correct
	one will be equipped depending on whether DT is enabled and which set is equipped depends on the DT_TYPE 
	selected. Please consider not including gear that doesn't have any damage taken property so other wanted 
	stats can shine through.
--]]

	['DT_Physical'] = {
	},
	
	['DT_Magical'] = {
        Ear1 = 'Coral Earring',							-- magic damage taken -1
    },
	['DT_Magical_Conditional'] = {
	},
	
	['DT_Breath'] = { 
	},
	
--[[
		Unlike most jobs, Summoner's emphasis is fighting with your avatar. So, the TP sets and
		the associated subsets (for accuracy, magical accuracy, and evasion) are directed at the avatar, 
		not the player. If you insist on fighting, modify the TP set to be a hybrid of stats for your
		avatar and stats that will help you. (Beyond skilling up a weapon I doubt you'll ever fight
		on a summoner.) Please note that accuracy gear should go into the Accuracy set, magic accuracy/
		attack 	gear goes in the Macc set, magical attack gear goes in the Matt set, and that evasion
		gear should go into the Evasion set. If you want to make a TP set just for you (no pet), I 
		suggest you make a custom gear set and use /gearset.
--]]

	['TP'] = {
        Head = 'Shep. Bonnet',
        Ear1 = 'Black Earring',				-- +4 MP
        Ear2 = 'Beastly Earring',			-- +5 MP, blinded: +15 evasion
        Body = 'Austere Robe',				-- avatar perpetuation cost -1, ability delay -3 
        Hands = 'Carbuncle Mitts',			-- +14 MP, 1/2 perpetuation cost for carbuncle
		Ring1 = 'Evoker\'s Ring',			-- +25 MP, +10 summoning magic skill,-1 avatar perpetuation cost
        Ring2 = 'Tamas Ring',				-- +5 magic accuracy, -5 enmity
        Back = 'Blue Cape',					-- +15 MP, convert 15 HP to MP
        Waist = 'Hierarch Belt',			-- +48 MP, +2 HP while healing, +2 MP while healing
        Legs = 'Evoker\'s Spats',			-- +15 MP, avatar: enhances accuracy, -2 enmity 
        Feet = 'Mannequin Pumps',			-- +12 MP
    },
	['TP_Conditional'] = {
		{'NK-*','Uggalepih Pendant','Equip at night','Neck',70,'ALL','TIME','Nighttime'},
		{'NK-*','Fenrir\'s Torque','Equip during day','Neck',70,'ALL','TIME','Daytime'},	
	},
	
	['TP_With_Carbuncle'] = {
		Body = 'Vermillion Cloak',				-- No head allowed, adds "refresh" effect
		Hands = 'Carbuncle Mitts',				-- +14 MP, 1/2 perpetuation cost on carbuncle
	},
	['TP_With_Carbuncle_Conditional'] = {
		{'NK-*','Uggalepih Pendant','Equip at night','Neck',70,'ALL','TIME','Nighttime'},
		{'NK-*','Fenrir\'s Torque','Equip during day','Neck',70,'ALL','TIME','Daytime'},
	},	
	
--[[
	If an accuracy emphasis is desired, the following set will replace the gear appropriately.
--]]
	
	['Accuracy'] = {
        Head = 'Shep. Bonnet',				-- Pet Accuracy +5
		Ear2 = 'Beastly Earring',			-- Pet Accuracy +10
		Ring1 = 'Toreador\'s Ring',			-- Accuracy +7
		Ring2 = 'Jaeger ring',				-- Accuract +4
		Waist = 'Life Belt',				-- Accuracy +10
		Legs = 'Evoker\'s Spats',			-- Enhances avatar accuracy
    },
	['Accuracy_Conditional'] = {
	},
	
	['Evasion'] = {							-- This is player evasion
		Head = 'Optical Hat',				-- Evasion +10
		Neck = 'Spirit Torque',				-- Evasion +5
    },
	['Evasion_Conditional'] = {
	},
	
	['Macc'] = {							-- This is player magical accuracy/attack, pet macc/matt goes in SmnAccuracy
		Ring1 = 'Tamas Ring',				-- Magical accuracy +5
    },
	['Macc_Conditional'] = {
	},

--[[
	Enmity sets are used to boost/reduce enmity, accordingly
--]]

	['Enmity_Plus'] = {
	},

	['Enmity_Minus'] = {
        Ring1 = 'Tamas Ring',				-- Enmity -5
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

--[[
	The second stage is Midcast. This is where you'll want to equip magic attack, magic attack 
	bonus, or magic enhancing gear.
--]]	

	['Midcast'] = {
	},
	['Midcast_Conditional'] = {
	},

--[[
	Further, there is a break out for each type of spell. I've included a comment on the type of attributes
	the piece of gear should have. While the spell might have other attributes than those listed, the ones I have
	listed have gear that a SMN or anyone can wear.
--]]

	-- Healing: Healing Magic Skill, cure potency. A few pieces that affect healing magic skill for either a SMN
	-- or "any jobs" exist. Also, gear with MND bonuses will boost cure spell's potency, but MND  gear is 
	-- automatically equipped prior to the Healing set being equipped in the HandleMidcast function. There's no 
	-- need to include MND gear here. As to items that add cure potency directly there are a few pieces both for
	-- smn and "all jobs". So, include healing magic skill items and cure potency items here.
	['Healing'] = {
    },

	-- Dark: Dark Magic Skill. Currently only gear equippable by any job gives is applicable here. There's no gear that's 
	-- specific for SMN that gives any dark magic skill.
	['Dark'] = {
	},
	['Dark_Conditional'] = {
	},
	
	-- Divine: Divine Magic Skill.
	['Divine'] = {
	},
	
	-- Enfeebling: Enfeebling Magic Skill.
	['Enfeebling'] = {
	},
	['Enfeebling_Conditional'] = {
	},
	
	-- Enhancing: There is no gear that a SMN can wear to enhance any magic spell. Leave the Enhancing gear sets empty.
	['Enhancing'] = {
	},
	
	-- Elemental: Elemental Magic Skill. Note: don't include elemental staves or elemental obis/gorgets here,
	-- that is done automatically in the HandlePrecast/HandleMidcast functions.
	['Elemental'] = {
	},
	['Elemental_Conditional'] = {
	},
	
	-- Ninjitsu: There is no gear that a SMN can wear to add Ninjitsu skill. Leave the following two
	-- gear sets empty.
	['Ninjitsu'] = {
	},

	-- Summoning: Summoning Magic Skill, Avatar Perpetuation Cost, Blood Pact Ability Delay. Please 
	-- note that currently on HorizonXI summoning skill is ignored. Any gear piece that only gives 
	-- summoning skill will be commented out
	['Summoning'] = {
		Head = 'Austere hat',			-- Summoning magic skill +2, blood pact ability delay -2
        -- Neck = 'Smn. Torque',		-- Summoning magic skill +7
		Body = 'Austere Robe',			-- Avatar perpetuation cost -1, blood pact ability delay -3
        Ring1 = 'Evoker\'s Ring',		-- Summoning magic skill +10, avatar perpetuation cost -1
    },
	
--[[
	Next is stat-based gear, (in this case intelligence or mind)
--]]

	['INT'] = {
        Hands = 'Seer\'s Mitts',		-- +1 INT
        Ring1 = 'Tamas Ring',			-- +2~5 INT
        Ring2 = 'Windurstian Ring',		-- +1 INT
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
        Ring1 = 'Tamas Ring',			-- +2~5 MND
        Ring2 = 'Tranquility Ring',		-- +2 MND
        Back = 'White Cape',			-- +2 MND
        Waist = 'Mrc.Cpt. Belt',		-- +1 MND
        Legs = 'Wonder Braccae',		-- +2 MND
        Feet = 'Mannequin Pumps',		-- +2 MND
    },
	['MND_Conditional'] = {
	},
	
--[[
	And some spells are special cases, so they have individual gears sets.
--]]
	
	-- Stoneskin: Stoneskin Enhancement, Mind, and Enhancing Magic Skill. Mind is 3x more important than enhancing
	-- Magic Skill. The only piece of gear a SMN can wear to enhance stoneskin is a Stone Gorget. There's no gear
	-- that a SMN (or any job) can wear to enhance magic. Note: This gear set has no effect on Titan's Stoneskin
	-- blood pact.
	['Stoneskin'] = {
	    Neck = 'Justice Badge',			-- +3 MND
        Body = 'Wonder Kaftan',			-- +1 MND
        Hands = 'Seer\'s Mitts',		-- +1 MND
        Ring1 = 'Tamas Ring',			-- +2~5 MND
        Ring2 = 'Tranquility Ring',		-- +2 MND
        Back = 'White Cape',			-- +2 MND
        Waist = 'Mrc.Cpt. Belt',		-- +1 MND
        Legs = 'Wonder Braccae',		-- +2 MND
        Feet = 'Mannequin Pumps',		-- +2 MND
	},	

	-- Drain: Drain Enhancement, Dark Magic Skill, Dark Magic Accuracy. Currently no gear equippable by a
	-- SMN enhances Drain. Drain is part of Dark Magic, so Potency which is based on dark magic skill will
	-- already be loaded in HandleMidcast function and need not be repeated here. No current gear supports
	-- dark magic accuracy for any job. Magic attack bonus and magic critical hit have no effect on potency.
	-- Leave the two Drain gear sets empty.
	['Drain'] = {
    },

	-- Aspir: Aspir Enhancement, Dark Magic Skill, Dark Magic Accuracy. Currently no gear equippable by a
	-- SMN enhances Aspir. Aspir is part of Dark Magic, so potency which is based on dark magic skill will
	-- already be loaded in HandleMidcast function and need not be repeated here. No current gear supports
	-- dark magic accuracy for any job. Magic attack bonus and magic critical hit have no effect on potency.
	-- Leave the two Aspir gear sets empty.
	['Aspir'] = {
    },
	
	-- Sneak: Enhances Sneak and Enhances Stealth. Currently only Dream Boots +1 enhances sneak and is equippable
	-- by any job. (Attained through the Starlight Celebration.) No gear for any job supports Enhances Stealth
	-- yet.	
	['Sneak'] = {
		Feet = 'Dream Boots +1',			-- enhances sneak
	},
	
	-- Invisible: Enhances Invisible Effect. Currently only Dream Mittens +1 enhances invisible and is equippable
	-- by any job. (Attained through the Starlight Celebration.)
	['Invisible'] = {
		Hands = 'Dream Mittens +1',			-- enhances invisibility
	},
	
	-- Note: Phalanx does have gear that supports the spell, but it is out of era
	
--[[
	The following weapon skill gearsets are defined by the stat they emphasize. Listed are all of the sets that
	you will need to use every weapon skill that your job can do. The leading comment defines what weapon/weapon
	skill combination the set applies to.
	
	SMN can use the following weapons: staff (B), Club (C+), dagger (E). Any other weapon will have 
	no weaponskill available. Weapon skill sets are named based on stat(s) used, regardless of weapon
--]]

--[[
		* Strength based or just skill based *
		
		Staff: Heavy Swing,Shell Crusher,Full Swing
		Club: Starlight,Brainshaker,Moonlight,Skullbreaker,True Strike
-]]
	
	['WS_STR'] = {
        Head = 'Mrc.Cpt. Headgear',			-- +1 STR
        Neck = 'Spike Necklace',			-- +3 STR
        Body = 'Wonder Kaftan',				-- +1 STR
        Hands = 'Wonder Mitts',				-- +3 STR
        Ring1 = 'Sun Ring',					-- +3 STR
        Ring2 = 'Sun Ring',					-- +3 STR
        Waist = 'Mrc.Cpt. Belt',			-- +1 STR
        Legs = 'Wonder Braccae',			-- +1 STR
        Feet = 'Creek F Clomps',			-- +4 STR
    },
	['WS_STR_Conditional'] = {
	},
	
--[[
		* Strength and Intelligence based, even weighting *
		
		Staff: Rock Crusher,Earth Crusher,Cataclysm
--]]
	
	['WS_STRINT'] = {
        Head = 'Mrc.Cpt. Headgear',			-- +1 STR
        Neck = 'Spike Necklace',			-- +3 STR
        Body = 'Wonder Kaftan',				-- +1 STR
        Hands = 'Wonder Mitts',				-- +3 STR
        Ring1 = 'Tamas Ring',				-- +2~5 INT
        Ring2 = 'Sun Ring',					-- +3 STR
        Waist = 'Mrc.Cpt. Belt',			-- +1 STR, +1 INT
        Legs = 'Seer\'s Slacks',			-- +1 INT
        Feet = 'Wonder Clomps',				-- +2 STR
    },
	['WS_STRINT_Conditional'] = {
	},

--[[
		* Strength and Mind based, even weighting *
		
		Club: Shining Strike,Seraph Strike,Judgement
		Staff: Starburst,Sunburst,Retribution
--]]
	
	['WS_STRMND'] = {
        Head = 'Mrc.Cpt. Headgear',			-- +1 STR
        Neck = 'Justice Badge',				-- +3 MND
        Body = 'Wonder Kaftan',				-- +1 STR, +1 MND
        Hands = 'Wonder Mitts',				-- +3 STR
        Ring1 = 'Tamas Ring',				-- +2~5 MND
        Ring2 = 'Sun Ring',					-- +3 STR
        Back = 'White Cape',				-- +2 MND
        Waist = 'Mrc.Cpt. Belt',			-- +1 STR, +1 MND
        Legs = 'Wonder Braccae',			-- +1 STR, +2 MND
        Feet = 'Creek F Clomps',			-- +4 STR
    },
	['WS_STRMND_Conditional'] = {
	},

--[[
		* Strength and Mind based, 30% to 50% weighting *
		
		Club: Black Halo
--]]

	['WS_STRMND_30_50'] = {
        Head = 'Mrc.Cpt. Headgear',			-- +1 STR
        Neck = 'Justice Badge',				-- +3 MND
        Body = 'Wonder Kaftan',				-- +1 STR, +1 MND
        Hands = 'Wonder Mitts',				-- +3 STR
        Ring1 = 'Tamas Ring',				-- +2~5 MND
        Ring2 = 'Sun Ring',					-- +3 STR
        Back = 'White Cape',				-- +2 MND
        Waist = 'Mrc.Cpt. Belt',			-- +1 STR, +1 MND
        Legs = 'Wonder Braccae',			-- +1 STR, +2 MND
        Feet = 'Creek F Clomps',			-- +4 STR
    },
	['WS_STRMND_30_50_Conditional'] = {
	},
	
--[[
		* Dexterity based *
		
		Dagger: Wasp Sting,Viper Bite^

		^ Subjob must be one of: RDM,THF,BRD,RNG,NIN
--]]
	
	['WS_DEX'] = {
        Head = 'Empress Hairpin',			-- +3 DEX
        Neck = 'Spike Necklace',			-- +3 DEX
        Body = 'Mrc.Cpt. Doublet',			-- +1 DEX
        Ring1 = 'Balance Ring',				-- +2 DEX
        Ring2 = 'Bastokan Ring',			-- +1 DEX
        Waist = 'Mrc.Cpt. Belt',			-- +1 DEX
    },
	['WS_DEX_Conditional'] = {
	},

--[[
		* Dexterity and Intelligence based *

		Dagger: Gust Slash,Cyclone^

		^ Subjob must be one of: RDM,THF,BRD,RNG,NIN
--]]
	
	['WS_DEXINT'] = {
        Head = 'Empress Hairpin',			-- +3 DEX
        Neck = 'Spike Necklace',			-- +3 DEX
        Body = 'Mrc.Cpt. Doublet',			-- +1 DEX
        Hands = 'Seer\'s Mitts',			-- +1 INT
        Ring1 = 'Tamas Ring',				-- +2~5 INT
        Ring2 = 'Windurstian Ring',			-- +1 INT
        Waist = 'Mrc.Cpt. Belt',			-- +1 DEX, +1 INT
        Legs = 'Seer\'s Slacks',			-- +1 DEX
        Feet = 'Mannequin Pumps',			-- +1 INT
    },
	['WS_DEXINT_Conditional'] = {
	},

--[[
		* Intellegence *

		Staff: Gate of Tartarus
--]]
	
	['WS_INT'] = {
        Hands = 'Seer\'s Mitts',			-- +1 INT
        Ring1 = 'Tamas Ring',				-- +2~5 INT
        Ring2 = 'Windurstian Ring',			-- +1 INT
        Waist = 'Mrc.Cpt. Belt',			-- +1 INT
        Legs = 'Seer\'s Slacks',			-- +1 INT
        Feet = 'Mannequin Pumps',			-- +1 INT
    },
	['WS_INT_Conditional'] = {
	},
	
--[[
		* Intellegence and Mind based, even weighting *

		Staff: Spirit Taker
--]]
	
	['WS_INTMND'] = {
        Neck = 'Justice Badge',				-- +3 MND
        Body = 'Wonder Kaftan',				-- +1 MND
        Hands = 'Seer\'s Mitts',			-- +1 INT, +1 MND
        Ring1 = 'Windurstian Ring',			-- +1 INT
        Ring2 = 'Tamas Ring',				-- +2~5 INT, +2~5 MND
        Waist = 'Mrc.Cpt. Belt',			-- +1 INT, +1 MND
        Legs = 'Wonder Braccae',			-- +2 MND
        Feet = 'Mannequin Pumps',			-- +1 INT, +2 MND
    },
	['WS_INTMND_Conditional'] = {
	},
	
--[[
		* Charisma based *

		Dagger: Shadowstitch
--]]
	
	['WS_CHR'] = {
        Head = 'Entrancing Ribbon',			-- +2 CHR
        Neck = 'Flower Necklace',			-- +3 CHR
        Ring1 = 'Moon Ring',				-- +3 CHR
        Ring2 = 'Moon Ring',				-- +3 CHR
        Waist = 'Corsette',					-- +5 CHR
    },
	['WS_CHR_Conditional'] = {
	},

--[[
		* Mind based *

		Dagger: Energy Steal, Energy Drain^
		
		^ Subjob must be RDM,THF,BRD,RNG, or NIN
--]]

	['WS_MND'] = {
        Neck = 'Justice Badge',				-- +3 MND
        Body = 'Wonder Kaftan',				-- +1 MND
        Hands = 'Seer\'s Mitts',			-- +1 MND
        Ring1 = 'Tamas Ring',				-- +2~5 MND
        Ring2 = 'Tranquility Ring',			-- +2 MND
        Back = 'White Cape',				-- +2 MND
        Waist = 'Mrc.Cpt. Belt',			-- +1 MND
        Legs = 'Wonder Braccae',			-- +2 MND
        Feet = 'Mannequin Pumps',			-- +2 MND
    },
	['WS_MND_Conditional'] = {
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
	 Might seem strange to include BST gear sets, but a SMN/BST can charm a monster
	(not sure why you'd want to) and the level of the pet will be the level of the
	charmed monster.
--]]
	-- BST ability, CHR gear.
	['Charm'] = {
        Neck = 'Flower Necklace',		-- +3 CHR
        Ear2 = 'Beastly Earring',		-- +2 CHR
        Ring1 = 'Moon Ring',			-- +3 CHR
        Ring2 = 'Moon Ring',			-- +3 CHR
        Waist = 'Corsette',				-- +5 CHR
    },
	['Charm_Conditional'] = {
	},
		
	['Pet_Attack'] = {					-- Pet's strength, not accuracy
	},

	['Pet_Macc'] = {					-- Pet's Magical Accuracy
	},
	
	['Pet_Matt'] = {					-- Pet's Magical Attack
	},
	
--[[
								*** Custom Sets Go below this comment ***
								
	The following "CAP" sets are added as a convenience for playing in level capped areas. The only way for them to be 
	loaded is via the /gearset command, which will turn GSwap off. If you're level syncing, pick the set that's closest 
	to the sync level and adjust accordingly.
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
		Main = 'Kukulcan\'s Staff',
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
		Main = 'Kukulcan\'s Staff',	
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
		Main = 'Dark Staff',
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
profile.sjb = nil;			-- Tracks subjob name
profile.sPetAction = nil;	-- what was the last action by your avatar
profile.bAmmo = false;		-- /BST specific. Is ammo equipped?
profile.sAmmo = nil;		-- /BST specific. Name of ammo equipped
--[[
	HandlePetAction equips the appropriate gear set based on the type of action
	the pet is trying to perform. (This is specifically rage blood pacts.)
	
	Note: This version of HandlePetAction has two parameters instead of the one
	that is more commonly found in the other job files. This is because SMN/BST
	can possibly have either type of pet.
--]]

local function HandlePetAction(Pet,PetAction)
	
	-- Only gear swap if this flag is true
	if gcdisplay.GetToggle('GSwap') == false then
		return;
	end
	
	-- Check to see if the pet is an smn's avatar or a /bst pet
	Pet.Name = string.lower(Pet.Name);
	if string.find(gcinclude.MagicSkill['Summoning'],Pet.Name) ~= nil then
		-- All SMN pet actions are blood pacts. Address accordingly
		if (gcinclude.SmnSkill:contains(PetAction.Name)) then			-- summoning skill based blood pact?
			gFunc.EquipSet(sets.SmnSkill);
		elseif (gcinclude.SmnMagical:contains(PetAction.Name)) then		-- magical based blood pact?
			gFunc.EquipSet(sets.SmnMagical);
				-- If /acc flagged, load accuracy set (for magical accuracy)
			if gcdisplay.GetToggle('Acc') == true then
				gFunc.EquipSet(sets.SmnAccuracy);
			end
		elseif (gcinclude.SmnHybrid:contains(PetAction.Name)) then		-- hybrid blood pact (2x physical, 1x magical)?
			gFunc.EquipSet(sets.SmnHybrid);
			-- lastly check on accuracy
			if gcdisplay.GetToggle('Acc') == true then
				gFunc.EquipSet(sets.SmnAccuracy);
			end				
		else																-- physical	blood pact
			gFunc.EquipSet(sets.SmnPhysical);
			-- if /acc flagged, load accuracy set (for physical accuracy)
			if gcdisplay.GetToggle('Acc') == true then
				gFunc.EquipSet(sets.SmnAccuracy);
			end
		end

		-- if the action just done is a BP: rage, print out what happened in party chat
		if (profile.sPetAction == nil or profile.sPetAction ~= PetAction.Name) and gcdisplay.GetToggle('sBP') == true then
			local sMsg;
			if string.find(gcinclude.SmnBPRageList,PetAction.Name) ~= nil then
				sMsg = '/p  Blood Pact [' .. Pet.Name .. ']:  ' .. PetAction.Name .. ' >> <t>.';
			else
				sMsg = '/echo [' .. Pet.Name .. ']: Blood Pact - ' .. PetAction.Name;
			end
			AshitaCore:GetChatManager():QueueCommand(-1, sMsg);
			profile.sPetAction = PetAction.Name;
		end
	else
		-- Must be a /BST charmed pet.
		if (gcinclude.BstPetAttack:contains(PetAction.Name)) then				-- Pet Attack
			gFunc.EquipSet(sets.Pet_Attack);
			-- If /acc enabled equip pet accuracy gear
			if gcdisplay.GetToggle('acc') == true then
				gFunc.EquipSet(sets.Pet_Accuracy);
			end
		elseif (gcinclude.BstPetMagicAttack:contains(PetAction.Name)) then		-- Pet Magical Attack
			gFunc.EquipSet(sets.Pet_Matt);		
		elseif (gcinclude.BstPetMagicAccuracy:contains(PetAction.Name)) then	-- Pet Magical Accuracy Attack
			gFunc.EquipSet(sets.Pet_Macc);		
		end
	end
end		-- HandlePetAction

--[[
	SetSubjobSet is used to pick the appropriate set for the loaded macrobook based on
	which subjob is current. (If no change has occurred since the last time it was called,
	nothing is checked/changed.)
--]]

local function SetSubjobSet(chkSJ)
	-- "subs" is the key for what toolbar is shown. Each job listed in the array is a possible subjob.
	-- If the associated value for a subjob is greater than zero, that indicates what macro set from the
	-- macro book should be displayed. (The value associated with SMN, your current main job, is nil
	-- since you can't be a smn/smn.) A value of 0 means the subjob is not configured. All other values
	-- imply that the subjob is expected and shows what macro set to show.
	local subs = {['WAR'] = 0, ['MNK'] = 0, ['WHM'] = 1, ['BLM'] = 3, ['RDM'] = 2, ['THF'] = 0,
				 ['PLD'] = 0, ['DRK'] = 0, ['BST'] = 0, ['BRD'] = 0, ['RNG'] = 0, ['SMN'] = nil,
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
	OnLoad is run whenever you log into your SMN or change your job to SMN
--]]

profile.OnLoad = function()
	local player = gData.GetPlayer();

	-- Initialize settings
	gSettings.AllowAddSet = true;
	gcinclude.Initialize();
	gcinclude.settings.RegenGearHPP = 50;
    gcinclude.settings.RefreshGearMPP = 60;
	gcinclude.settings.bSummoner = true;
	
	-- Coded order of operation override
	gcinclude.settings.priorityEngaged = 'BCDEFGH';
	gcinclude.settings.priorityMidCast = 'ABCDEFGH';
	gcinclude.settings.priorityWeaponSkill = 'ABCD';	
	
	-- Determine if subjob uses magic and if the maximum MP is > 50.
	gcinclude.CheckMagic50(player);
	
	-- Set which macro book should be displayed. Which macro set within the macro book to
	-- display depends on what your subjob is.
	AshitaCore:GetChatManager():QueueCommand(1, '/macro book 13');		-- SMN macro book
	SetSubjobSet(player.SubJob);
	
	-- Load up the weapons bar. (This need only be done once.)
	if gcdisplay.GetToggle('GSwap') == true then
		gFunc.ForceEquipSet(sets.Start_Weapons);	
	end
end

--[[
	OnUnload is run when you change to another job
--]]

profile.OnUnload = function()
	gcinclude.Unload();
end

--[[
	HandleCommand is run when you type in a command defined in LUASHITACAST. The commands handled here are
	either specific to SMN or something that was localized earlier in the implementation and never moved
	to the gcinclude.lua file. If the command is not explicitly handled, the version of trhis function In
	gcinclude.lua is invoked.
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
	local zone = gData.GetEnvironment();
	local ew = gData.GetEquipment();
	local eWeap = nil;
	local cKey;
	
	-- Make sure that the global magic settings for the player are known.
	if gcinclude.settings.bMagicCheck == false then
		gcinclude.CheckMagic50(player);
	end
	
	-- A pet action takes priority over a player's action.
	if (petAction ~= nil) then
		HandlePetAction(pet,petAction);
		return;
	end
	
	profile.sPetAction = nil;
	
	-- Save the name of the main weapon
	if ew['Main'] ~= nil then
		eWeap = ew['Main'].Name;	
	end
	
	-- Make sure the macro set is shown and that the display on the top of the screen is correct
	-- in case the subjob was changed.
	SetSubjobSet(player.SubJob);
	
	-- Only gear swap if this flag is true
	if gcdisplay.GetToggle('GSwap') == false then
		return;
	end

	-- If player is not resting and has a pet, make sure they're holding the correct staff 
	-- (assuming they own the correct staff)
	if player.Status ~= 'Resting' and pet ~= nil then
		local pName = string.lower(pet.Name);
		local pEle = gcinclude.SummonStaves[pName];

		if eWeap ~= nil and (eWeap ~= gcinclude.elemental_staves[pEle][1] or eWeap ~= gcinclude.elemental_staves[pEle][3]) then
			gcinclude.SwapToStave(pEle,true);
		end
	end
		
	-- Now process the pet/player statuses accordingly.
	if pet ~= nil and pet.Status == 'Engaged' or player ~= nil and player.status == 'Engaged' then
		gcinclude.settings.priorityEngaged = string.upper(gcinclude.settings.priorityEngaged);
		for i = 1,string.len(gcinclude.settings.priorityEngaged),1 do
			cKey = string.sub(gcinclude.settings.priorityEngaged,i,i);
			if cKey == 'B' then			-- Pet fighting
				if gcinclude.isPetNamed('Carbuncle',pet) == true then
					gFunc.EquipSet(sets.TP_With_Carbuncle);
					gcinclude.ProcessConditional(sets.TP_With_Carbuncle_Conditional,nil);
				else
					gFunc.EquipSet(sets.TP);
					gcinclude.ProcessConditional(sets.TP_Conditional,nil);
				end
			elseif cKey == 'C' then		-- Evasion
				if gcdisplay.GetToggle('Eva') == true then
					gFunc.EquipSet(sets.Evasion);
					gcinclude.ProcessConditional(sets.Evasion_Conditional,nil);
				end
			elseif cKey == 'D' then		-- Enmity
				local sEmn = gcdisplay.GetCycle('Enmity');
				if sEmn == 'Minus' then
					gFunc.EquipSet(sets.Enmity_Minus);				
				elseif sEmn == 'Plus' then
					gFunc.EquipSet(sets.Enmity_Plus);	
				end
			elseif cKey == 'E' then		-- Accuracy
				if gcdisplay.GetToggle('Acc') == true then 
					gFunc.EquipSet(sets.Accuracy);				-- Assume hybrid of pet and avatar
					gcinclude.ProcessConditional(sets.Accuracy_Conditional,nil);
				end
			elseif cKey == 'F' then		-- Kiting
				if (gcdisplay.GetToggle('Kite') == true) then
					gFunc.EquipSet(sets.Movement);
					gcinclude.ProcessConditional(sets.Movement_Conditional,nil);
				end
			elseif cKey == 'G' then		-- common buffs/debuffs
				gcinclude.CheckCommonDebuffs();
			elseif cKey == 'H' then		-- Damage Taken gear
				if (gcdisplay.GetCycle('DT') ~= gcinclude.OFF) then
					gFunc.EquipSet('DT_' .. gcdisplay.GetCycle('DT'));
					if gcdisplay.GetCycle('DT') == gcinclude.MAG then
						gcinclude.ProcessConditional(sets.DT_Magical_Conditional,nil);
					end
				end
			end
		end
	elseif player.Status == 'Resting' then	
		-- Player kneeling. Priority (low to high): resting, refresh
		gFunc.EquipSet(sets.Resting);
		if player.MPP < gcinclude.settings.RefreshGearMPP then
			gFunc.EquipSet(sets.Resting_Refresh);
			gcinclude.ProcessConditional(sets.Resting_Refresh_Conditional,nil);
		end
		-- Weapon swap to a weapon that refreshes MP if their MP is not at maximum
		if player.MP < player.MaxMP then
			gcinclude.SwapToStave('dark',false);
		end
		-- Check for common debuffs
		gcinclude.CheckCommonDebuffs();
	else
		-- See if in a town
		if zone.Area ~= nil and gcinclude.Towns:contains(zone.Area) then
			gFunc.EquipSet(sets.Town);
			gcinclude.ProcessConditional(gcinclude.sets.Town_Conditional,nil);
		else
			-- Assume idling. Priority (low to high): Idle,refresh.
			-- Check to see if there's an avatar

			if pet ~= nil then	
				pet.Name = string.lower(pet.Name);		
				if string.find(gcinclude.MagicSkill['Summoning'],pet.Name) ~= nil then
					if gcinclude.isPetNamed('Carbuncle',pet) then
						gFunc.EquipSet(sets.Idle_With_Carbuncle);
						gcinclude.ProcessConditional(sets.Idle_With_Carbuncle_Conditional,nil);					
					else
						gFunc.EquipSet(sets.Idle_With_Pet);
						gcinclude.ProcessConditional(sets.Idle_With_Pet_Conditional,nil);
					end
				end
			else			
				gFunc.EquipSet(sets.Idle);
				gcinclude.ProcessConditional(sets.Idle_Conditional,nil);
			end
			-- if the player's HP is below the threshold setting, equip the idle regen gear
			if player.HPP < gcinclude.settings.RegenGearHPP then		
				gFunc.EquipSet(sets.Idle_Regen);
				gcinclude.ProcessConditional(sets.Idle_Regen_Conditional,nil);
			end
			-- if the player's MP is below the threshold setting, equip the idle refresh gear
			if player.MPP < gcinclude.settings.RefreshGearMPP then
				gFunc.EquipSet(sets.Idle_Refresh);
				gcinclude.ProcessConditional(sets.Idle_Refresh_Conditional,nil);
			end
			-- Check for common debuffs
			gcinclude.CheckCommonDebuffs();
		end
	end
	
	-- Lastly, update the display, just in case
	gcdisplay.Update();
end

--[[
	HandleAbility is used to change the player's gear appropriately for the specified avatar ability.
--]]

profile.HandleAbility = function()
	local ability = gData.GetAction();
	local eq = gData.GetEquipment();
		
	-- gear swapping is turned off or the ability is release, assault, or retreat, no specific gear set
	-- needs to be loaded. Exit function if encountered.
	if ((gcdisplay.GetToggle('GSwap') == false) or
		(ability.Name == 'Release') or 
		(ability.Name == 'Assault') or
		(ability.Name == 'Retreat')) then 
		return 
	end

	-- Store the name of the ammo. This is used when the ammo slot is automatically
	-- populated so that the original ammo can be re-equipped.
	if eq.Ammo ~= nil then
		profile.sAmmo = eq.Ammo.Name;
	else
		profile.sAmmo = nil;
	end
	
	-- Check for abilities first that are not associated with smn.
	-- Start with BST
	if string.match(ability.Name, 'Reward') then
		-- Pet reward. Make sure that pet food already equipped
		if profile.sAmmo == nil or string.find(string.lower(profile.sAmmo),'pet f') == nil then		-- something else equipped
			profile.bAmmo = gcinclude.doPetFood('max',nil);
		end
		gFunc.EquipSet(sets.Reward);
	elseif string.match(ability.Name, 'Charm') then
		-- Trying to charm a beast. 
		gFunc.EquipSet(sets.Charm);
		gcinclude.ProcessConditional(sets.Charm_Conditional,nil);	
		gcinclude.SwapToStave('light',false);
	else
		-- Since we got here, the action has to be a SMN Blood Pact
		gFunc.EquipSet(sets.BP);
		gcinclude.ProcessConditional(sets.BP_Conditional,nil);
--[[
	Abilities associated with subjobs go here. The following subjobs have
	no ability entries because of lack of gear or just doesn't make sense: 
	DRK,SMN,PLD(out of era),WAR,MNK,WHM,BLM,RDM,BRD,RNG,SAM,DRG,THF
		
	Note: for /THF, sneak attack gets no bonus from DEX and trick attack gets
	no bonus from AGI
--]]

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

	gcinclude.settings.priorityMidCast = string.upper(gcinclude.settings.priorityMidCast);
	for i = 1,string.len(gcinclude.settings.priorityMidCast),1 do
		cKey = string.sub(gcinclude.settings.priorityMidCast,i,i);

		if cKey == 'A' then				-- midcast gear
			gFunc.EquipSet(sets.Midcast);
			gcinclude.ProcessConditional(sets.Midcast_Conditional,nil);
		elseif cKey == 'B' then			-- Spell Interruption Rate gear
			gFunc.EquipSet(sets.SIR);
		elseif cKey == 'C' then			-- INT/MND gear?
			sSet = gcinclude.WhichStat(spell.Name);
			if sSet ~= nil then
				if sSet == 'MND' then
					gFunc.EquipSet(sets.MND);
					gcinclude.ProcessConditional(sets.MND_Conditional,nil);
				elseif sSet == 'INT' then
					gFunc.EquipSet(sets.INT);
					gcinclude.ProcessConditional(sets.INT_Conditional,nil);
				end
			end
		elseif cKey == 'D' then			-- Magic Skill Type
			mSet = gcinclude.WhichMagicSkill(spell.Name);
			if mSet ~= nil then
				gFunc.EquipSet(mSet);
				if mSet == 'Dark' then
					gcinclude.ProcessConditional(sets.Dark_Conditional,nil);
				elseif mSet == 'Enfeebling' then
					gcinclude.ProcessConditional(sets.Enfeebling_Conditional,nil);
				elseif mSet == 'Elemental' then
					gcinclude.ProcessConditional(sets.Elemental_Conditional,nil);
				end
			end
		elseif cKey == 'E' then			--Magical accuracy
			if gcdisplay.GetToggle('acc') == true then
				gFunc.EquipSet(sets.Macc);
				gcinclude.ProcessConditional(sets.Macc_Conditional,nil);
			end
		elseif cKey == 'F' then			-- Spell specific gear
			if string.match(spell.Name, 'Stoneskin') then
				gFunc.EquipSet(sets.Stoneskin);			
			elseif string.match(spell.Name, 'Drain') then
				gFunc.EquipSet(sets.Drain);
			elseif string.match(spell.Name, 'Aspir') then
				gFunc.EquipSet(sets.Aspir);			
			elseif string.match(spell.Name, 'Sneak') then
				gFunc.EquipSet(sets.Sneak);
			elseif string.match(spell.Name, 'Invisible') then
				gFunc.EquipSet(sets.Invisible);
			end
		elseif cKey == 'G' then				-- Elemental Obi
			if gcinclude.settings.bEleObis == false then
				gcinclude.CheckForObisGorgets();
			end	
			if gcinclude.settings.bEleObis == true then
				obi = gcinclude.CheckEleSpells(spell.Name,gcinclude.MagicEleDmg,gcinclude.OBI);
				if obi ~= nil then
					gFunc.ForceEquip('Waist',obi);
				end
			end
		elseif cKey == 'H' then				-- Elemental Stave
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
			stat = nil;
		end		
	end	
end

--[[
	HandlePreshot is similar to HandlePrecast, but for ranged actions. It loads Ranged Accuracy 
	and Ranged Shot Speed Gear for a ranged attack
--]]

profile.HandlePreshot = function()
	if gcdisplay.GetToggle('GSwap') == true then		-- Only gear swap if this flag is true
		gFunc.EquipSet(sets.Preshot);
		gcinclude.ProcessConditional(sets.Preshot_Conditional,nil);
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
	
	gFunc.EquipSet(sets.Midshot);
	gcinclude.ProcessConditional(sets.Midshot_Conditional,nil);
	
	-- if enmity wanted, load that
	local sEmn = gcdisplay.GetCycle('Enmity');
	if sEmn == 'Minus' then
		gFunc.EquipSet(sets.Enmity_Minus);				
	elseif sEmn == 'Plus' then
		gFunc.EquipSet(sets.Enmity_Plus);		
	end
end

--[[
	HandleWeaponskill loads the gear appropriately for the weapon skill you're doing
--]]

profile.HandleWeaponskill = function()
	local ws = gData.GetAction();
	local canWS = gcinclude.CheckWsBailout();
	local cKey;

	-- If conditions would cause the weaponskill to fail, the action will be
	-- cancelled so you do not lose your TP.
	if (canWS == false) then 
		gFunc.CancelAction();
		return;
	end

	if gcdisplay.GetToggle('GSwap') == false then
		return;
	end
	
	gcinclude.settings.priorityWeaponSkill = string.upper(gcinclude.settings.priorityWeaponSkill);
	for i = 1,string.len(gcinclude.settings.priorityWeaponSkill),1 do
		cKey = string.sub(gcinclude.settings.priorityWeaponSkill,i,i);
		if cKey == 'A' then			-- weaponskill set
			-- Equip appropriate gear for weapon skill
			local sWS = gcinclude.WsStat(ws.Name,'STR');
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
		elseif cKey == 'B' then		-- elemental gorget	
			if gcinclude.settings.bEleGorgets == false then
				gcinclude.CheckForObisGorgets();
			end
			if gcinclude.settings.bEleGorgets == true then
				local sGorget = gcinclude.CheckEleGorget(ws.Name);
				if sGorget ~= nil then
					gFunc.ForceEquip('Neck',sGorget);
				end
			end
		elseif cKey == 'C' then		-- enmity
			local sEmn = gcdisplay.GetCycle('Enmity');
			if sEmn == 'Minus' then
				gFunc.EquipSet(sets.Enmity_Minus);				
			elseif sEmn == 'Plus' then
				gFunc.EquipSet(sets.Enmity_Plus);	
			end
		elseif cKey == 'D' then		-- accuracy
			print('accuracy');
			if gcdisplay.GetToggle('acc') == true then
				gFunc.EquipSet(sets.Accuracy);
				gcinclude.ProcessConditional(sets.Accuracy_Conditional,nil);
			end
		elseif cKey == 'E' then		-- elemental obi
--[[
			If the weaponskill is elemental and is closing a skillchain, then if the
			conditions for equipping an elemental obi are advantageous, it should be
			equipped now. Unfortunately I have no idea how to detect the closing of
			a skillchain and the automatic equipment of an elemental obi could 
			adversely affect the damage, so this section is not implemented. If I can
			ever figure out how to detect closing a skillchain, I will readdress this.
															- CCF, 1/12/2024
--]]		
		end
	end
end

return profile;