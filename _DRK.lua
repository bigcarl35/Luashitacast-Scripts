local profile = {};
gcinclude = gFunc.LoadFile('common\\gcinclude.lua');

--[[
	This file contains all the gear sets associated with the DRK job. While it isn't outside of the realm of 
	possibility that the subjob might be able to use gear sets too, that is not the emphasis of this program. 
	It is tailored to handle all the aspects of DRK. If you desire a gear set change to strengthen an ability
	from your subjob that is not supported by this program, you probably will have to make a custom gear set 
	and use the /gearset command to use it.
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
	Unlike when summoner is used as a subjob, /bst's pets are charmed at the max level of your BST or the level
	of your DRK, whichever is lower. That means you can charm higher level mobs than you would expect with /bst.
	Just note though that you can't have two pets, so if you have charmed a pet with /bst, you can't summon your
	avatar and visa versa.

	*** Note 2 ***
	No gear that supports bard songs can be worn by any job except a bard, no there's no support given here for
	/BRD.	
--]]

--[[
	The TP sets are used when you are fighting.	The accuracy set will be used if ACC is specified 
	and the evasion set if EVA is specified.
--]]

	['TP'] = {
        Head  = { 'chaos Burgeonet','Empress Hairpin' },
        Neck  = { 'Parade Gorget', 'Peacock Amulet' },
        Ear2  = { 'Genin Earring//SJNIN', 'Drone Earring', 'Physical Earring' },
        Body  = { 'Chaos Cuirass', 'Brigandine' },
        Hands = { 'Chaos Gauntlets', 'Wonder Mitts' },
        Rings = { 'Tamas Ring','Jaeger Ring' },
        Back  = 'Raptor Mantle',
        Waist = 'Swift Belt',
        Legs  = { 'Chaos Flanchard', 'Ryl.Sqr. Breeches' },
        Feet  = { 'Chaos Sollerets', 'Bounding Boots' },
    },
	['TP_Conditional'] = {
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
		Neck = 'Peacock Amulet',
        Rings = { 'Toreador Ring', 'Jaeger Ring', 'Balance Ring', 'Bastokan Ring' },
		Waist = 'Tilt Belt',
		Feet = { 'Chaos Sollerets', 'Bounding Boots' },
    },
	['Accuracy_Conditional'] = {
	},
	
	['Pet_Accuracy'] = {
    },
	['Pet_Accuracy_Conditional'] = {
	},
	
--[[
	If evasion wanted, equip evasion gear
--]]

	['Evasion'] = {
        Head = 'Empress Hairpin',
		Ears  = { 'Genin Earring//SJNIN', 'Drone Earring', 'Windurstian Earring' },		
        Legs = 'San. Trousers',
    },
	['Evasion_Conditional'] = {
	},

	['Pet_Evasion'] = {
    },
	['Pet_Evasion_Conditional'] = {
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
	},
	['Idle_Refresh_Conditional'] = {
	},

--[[
	When you are resting (kneeling down), your HP 'Resting' set will be equipped. If your MP 
	is below the set threshhold (defined by gcinclude.settings.RefreshGearMP) though, your MP 
	'Resting_Refresh' gear set will be equipped. Regardless of which set is equipped, if you
	have a Dark/Pluto staff accessible, weapon swapping has been enabled (/wswap), and your MP 
	is not at maximum, the Dark/Pluto staff will automatically be equipped.
--]]
	
	['Resting_Regen'] = { 
	},
	['Resting_Regen_Conditional'] = {
	},
	
	['Resting_Refresh'] = {
	},
	['Resting_Refresh_Conditional'] = {
	},
	
	-- If you have any Spell Interruption Rate down gear, put them into the "SIR" gear set.
	-- This gear set is equipped in the HandleMidcast function that all spells go through. 
	-- The only DRK gear that has this attribute is a Woodsville Axe. All other gear has to 
	-- be equippable for any job.
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
        Main = { 'Zweihander', 'Glorious Sword' },
        Ammo = 'Fortune Egg',
    },
	['Start_Weapons_Conditional'] = {
	},
	
--[[
	Specify what you want to wear around town.
--]]
	
	['Town'] = {
		Head = 'Lilac Corsage',
    },
	
	['Town_Conditional'] = {
		{'Federation Aketon','Movement gain in home nation city','Body',1,'ALL','AKETON','Windy'},	
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
    },
	['DT_Magical_Conditional'] = {
	},
	
	['DT_Breath'] = { 
	},
	['DT_Breath_Conditional'] = {
	},
		
--[[
	Magic accuracy gear
--]]

	['Macc'] = {
        Ring1 = 'Tamas Ring',
    },
	['Macc_Conditional'] = {
	},

--[[
	Magic Attack Bonus (MAB) is used for more than just spells, so it is broken out
--]]

	['MAB'] = {
	},
	['MAB_Conditional'] = {
	},
	
--[[
	Preshot is the first stage of when a ranged shot is being performed. This is where you place any 
	Ranged Accuracy or Ranged Attack Speed gear. 
--]]

	['Preshot'] = {
		Neck  = 'Peacock Amulet',
		Rings = { 'Jaeger Ring', 'Beetle Ring +1', 'Beetle Ring +1' },	
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

	The first stage is Precast. This is where you place any Fast Cast, cast time reduction, quick 
	cast gear, and spell interruption rate
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
	},

--[[
	Further, there is a break out for each type of spell. I've included a comment on the type of attributes
	the piece of gear should have. While the spell might have other attributes than those listed, the ones I have
	listed have gear that a DRK or anyone can wear.
--]]

	-- Healing: Healing Magic Skill. Currently only a Healing Earring affects healing spells from a sub job. No other gear
    -- gives bonuses to Healing magic from a sub job. Also, gear with MND bonuses will boost cure spell's potency, but MND 
	-- gear is automatically equipped prior to the Healing set being equipped in the HandleMidcast function. There's no need 
	-- to include MND gear here.
	['Healing'] = {
    },
	['Healing_Conditional'] = {
	},
	
	-- Dark: Dark Magic Skill.	
	['Dark'] = {
    },
	['Dark_Conditional'] = {
	},

	-- Divine: Divine Magic Skill.	
	['Divine'] = {
	},
	['Divine_Conditional'] = {
	},
	
	-- Enfeebling Magic Skill.	Currently only gear equippable by any job gives is applicable here. There's no gear that's 
	-- specific for DRK that gives any dark magic skill.	
	['Enfeebling'] = {
	},
	['Enfeebling_Conditional'] = {
	},
	
	-- Enhancing: There is no gear that a DRK can wear to enhance any magic spell. Leave the Enhancing gear sets empty.
	['Enhancing'] = {
	},
	['Enhancing_Conditional'] = {
	},
	
	-- Elemental: Elemental Magic Skill. Note: don't include elemental staves or elemental obis/gorgets here, 
	-- that is done automatically in the HandlePrecast/HandleMidcast functions (if /wswap is enabled).
	['Elemental'] = {
	},
	['Elemental_Conditional'] = {
	},

	-- Ninjitsu: There is no gear that a DRK can wear to add Ninjitsu skill. Leave the following two
	-- gear sets empty.	
	['Ninjitsu'] = {			-- Ninjitsu Skill, magic burst bonus, magic attack bonus
	},
	['Ninjitsu_Conditional'] = {
	},
	
	-- Summoning: Summoning Magic Skill and Avatar Perpetuation Cost. Currently only gear equippable by any job gives
	-- is applicable here. There's no gear that's specific for DRK that gives any summoning skill. Note: currently on 
	-- HorizonXI summoning skills are ignored. Any gear piece that only gives summoning skill will be commented out	
	['Summoning'] = {
		Hands = 'Carbuncle Mitts',
	},
	['Summoning_Conditional'] = {
	},
	
--[[
	Next is stat-based gear, (in this case intelligence or mind)
--]]

	['INT'] = {
        Rings = { 'Tamas Ring', 'Windurstian Ring' },
        Waist = 'Mrc.Cpt. Belt',
        Feet = 'Mannequin Pumps',
    },
	['INT_Conditional'] = {
	},
	
	['MND'] = {
        Neck = 'Justice Badge',
        Body = 'Wonder Kaftan',
        Rings = { 'Tamas Ring', 'Tranquility Ring', 'San d\'Orian Ring' },
        Waist = 'Mrc.Cpt. Belt',
        Legs = 'Wonder Braccae',
        Feet = { 'Chaos Sollerets', 'Mannequin Pumps' },
    },
	['MND_Conditional'] = {
	},

--[[
	And some spells are special cases, so they have individual gears sets.
--]]
	-- Stoneskin: Stoneskin Enhancement, Mind, and Enhancing Magic Skill. Mind is 3x more important than enhancing
	-- Magic Skill. The only piece of gear a DRK can wear to enhance stoneskin is a Stone Gorget. There's no gear
	-- that a DRK (or any job) can wear to enhance magic. Note: This gear set has no effect on Titan's Stoneskin
	-- blood pact.
	['Stoneskin'] = {	
	},	
	['Stoneskin_Conditional'] = {
	},
	
	-- Drain: Drain Enhancement, Dark Magic Skill, Dark Magic Accuracy. Currently no gear supports Drain enhancement.
	-- Drain is part of Dark Magic, so Potency which is based on dark magic skill will already be loaded in HandleMidcast 
	-- function and need not be repeated here. No current gear supports dark magic accuracy for any job. Magic attack 
	-- bonus and magic critical hit have no effect on potency. Leave the two Drain gear sets empty.
	['Drain'] = {
    },
	['Drain_Conditional'] = {
	},
	
	-- Aspir: Aspir Enhancement, Dark Magic Skill, Dark Magic Accuracy. Currently no gear equippable by a
	-- DRK enhances Aspir. Aspir is part of Dark Magic, so potency which is based on dark magic skill will
	-- already be loaded in HandleMidcast function and need not be repeated here. No current gear supports
	-- dark magic accuracy for any job. Magic attack bonus and magic critical hit have no effect on potency.
	-- Leave the two Aspir gear sets empty.
	['Aspir'] = {
    },
	['Aspir_Conditional'] = {
	},
	
	-- Dread Spikes: Dread Spikes Potency, Dread Spikes Absorption, and Hit Points (HP). No gear currently 
	-- supports for any job dread spikes potency and dread spikes absorption. What you want to do is maximize 
	-- your HP when you cast dread spikes.
    ['DreadSpikes'] = {
    },
	['Dreadspikes_Conditional'] = {
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
	
	DRK can use the following weapons: scythe (A+), great sword (A-), axe (B-), great axe (B-), sword (B-), dagger (C), club(C-).
	Any other weapon will have no weaponskill available. Weapon skill sets are named based on stat(s) used, regardless of weapon
--]]

--[[
		* Strength based or just skill based *
		
		Scythe: Slice,Spinning Scythe,Vorpal Scythe
		Great Sword: Hard Slash,Crescent Moon
		Axe: Raging Axe,Smash Axe,Gale Axe,Avalanche Axe,Spinning Axe,Rampage,Decimation
		Great Axe: Iron Tempest,Sturmwind,Keen Edge,Raging Rush
		Sword: Flat Blade,Circle Blade,Spirits Within,Vorpal Blade
		Club: Starlight,Brainshaker,Moonlight,Skullbreaker,True Strike		
-]]
	
	['WS_STR'] = {
        Neck = 'Spike Necklace',
        Body = 'Wonder Kaftan',
        Hands = 'Wonder Mitts',
        Rings = { 'San d\'Orian Ring', 'Courage Ring' },
        Waist = 'Mrc.Cpt. Belt',
        Legs = 'Wonder Braccae',
        Feet = 'Wonder Clomps',
    },
	['WS_STR_Conditional'] = {
	},

--[[
		* Strength and Agility based, even weighting *
		
		Great Sword: Sickle Moon
--]]

	['WS_STRAGI'] = {
        Head = 'Empress Hairpin',
        Neck = 'Spike Necklace',
        Ears = 'Drone Earring',
        Body = 'Wonder Kaftan',
        Hands = 'Wonder Mitts',
        Rings = {'Courage Ring', 'San d\'Orian Ring' },
        Waist = 'Mrc.Cpt. Belt',
        Legs = 'Wonder Braccae',
        Feet = 'Bounding Boots',
    },
	['WS_STRAGI_Conditional'] = {
	},
	
--[[
		* Strength and Dexterity based, even weighting *
		
		Sword: Fast Blade
--]]

	['WS_STRDEX'] = {
        Head = 'Empress Hairpin',
        Neck = 'Spike Necklace',
        Ears = { 'Genin Earring//SJNIN', 'Drone Earring' },
        Body = 'Wonder Kaftan',
        Hands = 'Wonder Mitts',
        Rings = { 'Courage Ring', 'Balance Ring' },
        Waist = 'Mrc.Cpt. Belt',
        Legs = 'Wonder Braccae',
        Feet = 'Bounding Boots',
    },
	['WS_STRDEX_Conditional'] = {
	},

--[[
		* Strength and Intelligence based, even weighting *
		
		Scythe: Dark Harvest,Shadow of Death,Nightmare Scythe,Spiral Hell, Catastrophe
		Great Sword: Frostbite,Freezebite,Spinning Slash,Ground Strike
		Sword: Burning Blade
--]]
	
	['WS_STRINT'] = {
        Neck  = 'Spike Necklace',
        Ears  = { 'Genin Earring//SJNIN','Drone Earring' },
        Body  = 'Wonder Kaftan',
        Hands = 'Wonder Mitts',
        Rings = { 'Tamas Ring', 'Courage Ring' },
        Waist = 'Mrc.Cpt. Belt',
        Legs  = 'Wonder Braccae',
        Feet  = 'Wonder Clomps',
    },
	['WS_STRINT_Conditional'] = {
	},

--[[
		* Strength and Intelligence based, 30%/20% respectively *
		
		Sword: Red Lotus Blade
--]]
	
	['WS_STRINT_30_20'] = {
        Neck = 'Spike Necklace',
        Ears = 'Drone Earring',
        Body = 'Wonder Kaftan',
        Hands = 'Wonder Mitts',
        Rings = { 'Tamas Ring', 'Courage Ring' },
        Waist = 'Mrc.Cpt. Belt',
        Legs = 'Wonder Braccae',
        Feet = 'Wonder Clomps',
    },
	['WS_STRINT_30_20_Conditional'] = {
	},

--[[
		* Strength and Mind based, even weighting *
		
		Scythe: Guillotine,Cross Reaper
		Great Sword: Shockwave
		Sword: Shining Blade,Seraph Blade
		Club: Shining Strike,Seraph Strike,Judgement
--]]

	['WS_STRMND'] = {
        Neck = 'Justice Badge',
        Body = 'Wonder Kaftan',
        Hands = 'Wonder Mitts',
        Rings = { 'Tamas Ring', 'Courage Ring' },
        Waist = 'Mrc.Cpt. Belt',
        Legs = 'Wonder Braccae',
        Feet = 'Wonder Clomps',
    },
	['WS_STRMND_Conditional'] = {
	},

--[[
		* Strength and Vitality based, even weighting *
		
		Great Sword: Power Slash,Scourge
		Great Axe: Shield Break,Armor Break,Weapon Break,Steel Cyclone
		Sword: Shining Blade,Seraph Blade,Swift Blade,Savage Blade
--]]
	
	['WS_STRVIT'] = {
        Neck = 'Spike Necklace',
        Body = 'Wonder Kaftan',
        Hands = 'Wonder Mitts',
        Rings = { 'Courage Ring', 'Bastokan Ring' },
        Waist = 'Mrc.Cpt. Belt',
        Legs = 'Wonder Braccae',
        Feet = 'Wonder Clomps',
    },
	['WS_STRVIT_Conditional'] = {
	},

--[[
		* Charisma based *
		
		Dagger: Shadowstitch
--]]
	
	['WS_CHR'] = {
        Neck = 'Flower Necklace',
        Waist = 'Mrc.Cpt. Belt',
    },
	['WS_CHR_Conditional'] = {
	},
	
--[[
		* Dexterity based *
		
		Dagger: Wasp Sting,Viper Bite^

		^ Subjob must be one of: RDM,THF,BRD,RNG,NIN
--]]
	
	['WS_DEX'] = {
        Head = 'Empress Hairpin',
        Neck = 'Spike Necklace',
        Rings = 'Balance Ring',
        Waist = 'Mrc.Cpt. Belt',
        Feet = 'Bounding Boots',
    },
	['WS_DEX_Conditional'] = {
	},

--[[
		* Dexterity and Intelligence based *
		
		Dagger: Gust Slash
--]]
	
	['WS_DEXINT'] = {
        Head = 'Empress Hairpin',
        Neck = 'Spike Necklace',
        Rings = 'Balance Ring',
        Waist = 'Mrc.Cpt. Belt',
        Feet = 'Bounding Boots',
    },
	['WS_DEXINT_Conditional'] = {
	},

--[[
		* Mind based *
		
		Dagger: Energy Steal, Energy Drain
--]]

	['WS_MND'] = {
        Neck = 'Justice Badge',
        Body = 'Wonder Kaftan',
        Rings = { 'Tamas Ring', 'Tranquility Ring' },
        Waist = 'Mrc.Cpt. Belt',
        Legs = 'Wonder Braccae',
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
        Ears = 'Physical Earring',
        Body = { 'Brigandine', 'Wonder Kaftan' },
        Hands = 'Wonder Mitts',
        Rings = 'Toreador\'s Ring',
        Waist = 'Powerful Rope',
        Legs = 'Wonder Braccae',
        Feet = 'Creek F Clomps',
    },
	['WS_HP_Conditional'] = {
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
	The following are abilities affected by gear
--]]

	['ArcaneCircle'] = {
		Feet = 'Chaos Sollerets',
    },
	['ArcaneCircle_Conditional'] = {
	},
	
	['LastResort'] = {
    },
	['LastResort_Conditional'] = {
	},
	
	['Souleater'] = {
    },
	['Souleater_Conditional'] = {
	},
	
	['WeaponBash'] = {
    },
	['WeaponBash_Conditional'] = {
	},
	
--[[
	Some subjobs really make no sense when combined with paladin, but all abilities across all jobs that
	have gear that can be equipped by a DRK are included here.
	
	The following sub jobs have no skills with equippable gear by a DRK: WAR,THF,BLM,MNK,WHM,RDM,RNG,NIN,
	SMN,BRD,SAM
--]]
	
	--* BST *--, CHR gear.
	['Charm'] = {
        Neck = 'Flower Necklace',
        Rings = { 'Moon Ring', 'Moon Ring' },
        Waist = 'Corsette',
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
	
	--* DRG *--
	['Jumps'] = {		-- Jump and High Jump, Super is too high a level
	},
	['Jumps_Conditional'] = {
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


	
	['CAP40'] = {
    },
	
};

profile.Sets = sets;
profile.sjb = nil;
profile.bAmmo = false;		-- /BST specific. Is ammo equipped?
profile.sAmmo = nil;		-- /BST specific. Name of ammo equipped

--[[
	HandlePetAction equips the appropriate gear set based on the type of action
	the pet is trying to perform.
--]]

local function HandlePetAction(PetAction)
	local pet = gData.GetPet();
	
	-- Only gear swap if this flag is true
	if gcdisplay.GetToggle('GSwap') == false or string.find(gcinclude.SummonSkill,pet.Name) ~= nil then
		return;
	end

	-- Only /BST pet attacks have associated gear sets because /smn pets are at most half the
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
	-- macro book should be displayed. (The value associated with DRK, your current main job, is nil
	-- since you can't be a smn/smn.) A value of 0 means the subjob is not configured. All other values
	-- imply that the subjob is expected and shows what macro set to show.
	local subs = {['WAR'] = 1, ['MNK'] = 0, ['WHM'] = 1, ['BLM'] = 0, ['RDM'] = 0, ['THF'] = 1,
				 ['PLD'] = 0, ['DRK'] = nil, ['BST'] = 0, ['BRD'] = 0, ['RNG'] = 0, ['SMN'] = 0,
				 ['SAM'] = 1, ['NIN'] = 1, ['DRG'] = 1, ['BLU'] = 0, ['COR'] = 0, ['PUP'] = 0,
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

	-- Coded order of operation override
	gcinclude.settings.priorityEngaged = 'CEFGH';
	gcinclude.settings.priorityMidCast = 'ABCDEFGH';
	gcinclude.settings.priorityWeaponSkill = 'ABDE';
	
	-- Determine if subjob uses magic and if the maximum MP is > 50.
	gcinclude.CheckMagic50(player);
	
	-- Set which macro book should be displayed. Which macro set within the macro book to
	-- display depends on what your subjob is.
	AshitaCore:GetChatManager():QueueCommand(1, '/macro book 9');		-- DRK
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
	HandleCommand is run when you type in a command defined in LUASHITACAST. The commands handled here instead
	of in gcinclude.HandleCommands are specific to BST or the help system, which has been tailored to BST.
--]]

profile.HandleCommand = function(args)
	if args[1] == 'help' then
		gcdisplay.ShowHelp();
	elseif args[1] == 'petfood' then			-- Supported since pet food is not job specific, but very niche
		gcinclude.doPetFood(args[2],args[3]);
	else
		gcinclude.HandleCommands(args);
	end
end
	
--[[
	HandleDefault is run when some action happens. This includes both actions by the player and by
	their pet (if they have one).
--]]

profile.HandleDefault = function()
	local pet = gData.GetPet();
	local petAction = gData.GetPetAction();		
	local player = gData.GetPlayer();
	local zone = gData.GetEnvironment();
	local ew = gData.GetEquipment();
	local eWeap = nil;
	local cKey;
	local myLevel = AshitaCore:GetMemoryManager():GetPlayer():GetMainJobLevel();
	
	-- Note the player's current level
	if (myLevel ~= gcinclude.settings.iCurrentLevel) then
        gcinclude.settings.iCurrentLevel = myLevel;
    end
		
	-- Make sure that the global magic settings for the player are known. The second clause in
	-- the if statement takes care of a bizarre case. Turns out if you change the player.MainJob
	-- from a job where there is not a luashitacast script, it initially remembers the old main
	-- job. by including the second call, a subsequent invocation occurs getting it right.
	if gcinclude.settings.bMagicCheck == false or gcinclude.settings.sMJ ~= player.MainJob then
		gcinclude.CheckMagic50(player);
	end

	-- A /bst charmed pet action takes priority over a player's action.
	if petAction ~= nil and player.SubJob == 'BST' then
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

	-- Clear out the CurrentGear in case of leftovers
	gcinclude.ClearSet(sets.CurrentGear);
	
	-- If player is not resting and has MP and has swapped weapons, set the weapon back to what 
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
	gcdisplay.SetLocksAction(gcinclude.LocksNumeric,player.Status);		
	if player ~= nil and player.Status == 'Engaged' then
		gcinclude.settings.priorityEngaged = string.upper(gcinclude.settings.priorityEngaged);
		for i = 1,string.len(gcinclude.settings.priorityEngaged),1 do
			cKey = string.sub(gcinclude.settings.priorityEngaged,i,i);
			if cKey == 'C' then		-- Evasion			
				if gcdisplay.GetToggle('Eva') == true then
					if pet ~= nil then
						gcinclude.MoveToCurrent(sets.Pet_Evasion,sets.CurrentGear);
						gcinclude.ProcessConditional(sets.Pet_Evasion_Conditional,nil,sets.CurrentGear);
					end				
					gcinclude.MoveToCurrent(sets.Evasion,sets.CurrentGear);
					gcinclude.ProcessConditional(sets.Evasion_Conditional,nil,sets.CurrentGear);
				end
			elseif cKey == 'E' then		-- Accuracy	
				if gcdisplay.GetToggle('Acc') == true then 
					if pet ~= nil and pet.Status == 'Engaged' then
						gcinclude.MoveToCurrent(sets.Pet_Accuracy,sets.CurrentGear);
						gcinclude.ProcessConditional(sets.Pet_Accuracy_Conditional,nil,sets.CurrentGear);
					end				
					gcinclude.MoveToCurrent(sets.Accuracy,sets.CurrentGear);
					gcinclude.ProcessConditional(sets.Accuracy_Conditional,nil,sets.CurrentGear);
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
		if player.HP < player.MaxHP then		
			gcinclude.MoveToCurrent(sets.Resting_Regen,sets.CurrentGear);
			gcinclude.ProcessConditional(sets.Resting_Regen_Conditional,nil,sets.CurrentGear);
		end
		
		if player.MP < player.MaxMP then
			gcinclude.MoveToCurrent(sets.Resting_Refresh,sets.CurrentGear);
			gcinclude.ProcessConditional(sets.Resting_Refresh_Conditional,nil,sets.CurrentGear);
			gcinclude.SwapToStave('dark',false,sets.CurrentGear);
		end	
		
		-- Check for common debuffs
		gcinclude.CheckCommonDebuffs();			
	else
		-- Assume idling. There's no idle set, just idle conditions.

		-- See if in a town
		if (zone.Area ~= nil and table.find(gcinclude.Towns,zone.Area)) then
			gcinclude.MoveToCurrent(sets.Town,sets.CurrentGear);
			gcinclude.ProcessConditional(sets.Town_Conditional,nil,sets.CurrentGear);
		end
		
		-- if the player's HP is below the threshold setting, equip the idle regen gear
		if player.HPP < gcinclude.settings.RegenGearHPP then
			gcinclude.MoveToCurrent(sets.Idle_Regen,sets.CurrentGear);
			gcinclude.ProcessConditional(sets.Idle_Regen_Conditional,nil,sets.CurrentGear);
		end
		-- if the player's MP is below the threshold setting, equip the idle refresh gear
		if player.MPP < gcinclude.settings.RefreshGearMPP then
			gcinclude.MoveToCurrent(sets.Idle_Refresh,sets.CurrentGear);
			gcinclude.ProcessConditional(sets.Idle_Refresh_Conditional,nil,sets.CurrentGear);
		end
		-- Check for common debuffs
		gcinclude.CheckCommonDebuffs();			
	end
	
	-- Make sure to equip the appropriate elemental staff for the current pet (/smn only)
	if (pet ~= nil and player.SubJob == 'SMN' and gcdisplay.GetToggle('WSwap') == true) then
		local pName = string.lower(pet.Name);
		if string.find(gcinclude.SummonSkill,pName) ~= nil then
			local pEle = gcinclude.SummonStaves[pet.Name];
			gcinclude.SwapToStave(pEle,false,sets.CurrentGear);
		end
	end
	
	-- And make sure a weapon equipped. (Going into a capped area can cause no weapon to be equipped.)
	local gear = gData.GetEquipment();
	if gear.Main == nil then
		gcinclude.MoveToCurrent(sets.Start_Weapons,sets.CurrentGear,true);
		gcinclude.ProcessConditional(sets.Start_Weapons_Conditional,nil,sets.CurrentGear);
	end
	
	gcinclude.EquipTheGear(sets.CurrentGear);		-- Equip the composited HandleDefault set
	
	-- Lastly, update the display, just in case
	gcdisplay.Update();
end

--[[
	HandleAbility is used to change the player's gear appropriately.
--]]

profile.HandleAbility = function()
	local ability = gData.GetAction();
			
	-- Only gear swap if this flag is true
	if gcdisplay.GetToggle('GSwap') == false then
		return;
	end

	-- Clear out the CurrentGear in case of leftovers
	gcinclude.ClearSet(sets.CurrentGear);
	
	-- Now process the appropriate job ability. Start with abilities associated with DRK
	if string.match(ability.Name, 'Arcane Circle') then
		gcinclude.MoveToCurrent(sets.ArcaneCircle,sets.CurrentGear);
		gcinclude.ProcessConditional(sets.ArcaneCircle_Conditional,nil,sets.CurrentGear);	
	elseif string.match(ability.Name, 'Last Resort') then
		gcinclude.MoveToCurrent(sets.LastResort,sets.CurrentGear);
		gcinclude.ProcessConditional(sets.LastResort_Conditional,nil,sets.CurrentGear);
	elseif string.match(ability.Name, 'Souleater') then
		gcinclude.MoveToCurrent(sets.Souleater,sets.CurrentGear);
		gcinclude.ProcessConditional(sets.Souleater_Conditional,nil,sets.CurrentGear);	
	elseif string.match(ability.Name, 'WeaponBash') then
		gcinclude.MoveToCurrent(sets.WeaponBash,sets.CurrentGear);
		gcinclude.ProcessConditional(sets.WeaponBash_Conditional,nil,sets.CurrentGear);
	
	-- And now the subjob abilities	
	elseif string.match(ability.Name, 'Charm') then						-- assumes /bst	
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
	elseif string.contains(ability.Name, 'Jump') then			-- assumes /drg
		gcinclude.MoveToCurrent(sets.Jumps,sets.CurrentGear);
		gcinclude.ProcessConditional(sets.Jumps_Conditional,nil,sets.CurrentGear);
		
	else
	
--[[
		Abilities associated with subjobs go here. The following subjobs have
		no ability entries because of lack of gear or just doesn't make sense: 
		SMN,WAR,MNK,WHM,BLM,RDM,BRD,RNG,SAM,THF
		
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
			bShow = true;
		elseif string.match(item.Name, 'Silent Oil') then
			gcinclude.MoveToCurrent(sets.Sneak,sets.CurrentGear);
			gcinclude.ProcessConditional(sets.Sneak_Conditional,nil,sets.CurrentGear);
			bShow = true;
		elseif string.match(item.Name, 'Prism Powder') then
			gcinclude.MoveToCurrent(sets.Invisible,sets.CurrentGear);
			gcinclude.ProcessConditional(sets.Invisible_Conditional,nil,sets.CurrentGear);
			bShow = true;
		end
		
		if bShow == true then
			gcinclude.EquipTheGear(sets.CurrentGear);
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
		
	-- Only gear swap if this flag is true
	if gcdisplay.GetToggle('GSwap') == false then
		return;
	end
	
	-- Equip the precast gear set
	gcinclude.MoveToCurrent(sets.Precast,sets.CurrentGear);
	gcinclude.ProcessConditional(sets.Precast_Conditional,nil,sets.CurrentGear);
		
	-- See if an elemental obi should be equipped
	if gcinclude.settings.bEleObis == false then
		gcinclude.CheckForObisGorgets();
	end		
	if gcinclude.settings.bEleObis == true then
		obi = gcinclude.CheckEleSpells(spell.Name,gcinclude.MagicEleAcc,gcinclude.OBI,nil);
		if obi ~= nil then
			sets.CurrentGear['Waist'] = obi;
		end
	end
	gcinclude.EquipTheGear(sets.CurrentGear);
end

--[[
	HandleMidcast is the second function invoked when a player casts a spell. It equips gear appropriate for 
	magic skill, duration, magic attack bonus, magic accuracy, and potency. There's an order to how the pieces 
	are loaded: INT/MND, spell specific, macc, magic skill, obi, ele swap	
--]]

profile.HandleMidcast = function()

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
	HandleMidshot loads Ranged Attack and Damage gear for a ranged attack
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
	gcinclude.ClearSet(gProfile.Sets.CurrentGear);

	-- Call the common weaponskill handler
	gcinclude.HandleWeaponskill();
	
	-- Equip the composited weaponskill set		
	gcinclude.EquipTheGear(sets.CurrentGear);
end		-- HandleWeaponskill

return profile;