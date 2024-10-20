local profile = {};
gcinclude = gFunc.LoadFile('common\\gcinclude.lua');

--[[
	This file contains all the gear sets associated with the DRK job.

	Gear Sets last updated: September 15, 2024	
	Code update: September 28, 2024	
--]]

local sets = {
--[[
	The gear sets are self contained, a mixture of direct gear assignments and conditional
	assignments. Each set contains entries identified by the gear slot. If it's a single
	value, it's a direct assignment like: Body = 'Austere Robe', but there can be multiple
	items identified, usually ordered by level: Body = { 'Vermillion Cloak//CARBY','Austere Robe' },
	Any item that has a // appended to it contains an inline conditional. The // code is a test
	to see if the item should be equipped. The level is still checked, but if the inline coded
	test is successful, that piece of gear will be loaded. Currently nothing checks to see
	if that item can be equipped by the job it's associated with let alone whether the player
	even has it accessible. Those are all planned for the future. In the mean time the onus is
	on the player to create the correct definitions.
	
	Not all sets need to be defined. There is nothing wrong with leaving a set "empty", but don't
	delete any of the sets. All the ones listed here (except for any custom sets) are expected to 
	exist by Luashitacast.
		
	*** Note ***
	/SMN has a problem in that their pet is the level of the subjob, which is not very useful. 
	As a result, /SMN pet actions are treated "as is" without gearset swap support.	As for /DRG,
	please note that the wyvern can't be summoned.

	Also, unlike true pet jobs like SMN and BST, PLD can only have a pet through a subjob. While 
	associated pet gearsets are available, you equally can just skip them since the pet is at 
	half your level.
	
	*** Note 2 ***
	No gear that supports bard songs can be worn by any job except a bard, so there's no explicit
	support given here for /BRD.

	
	Horizon changes from retail:
		- Moderate changes to artifact armor
		- Minor changes to Apocalyse
		- Significant changes to Ragnarok
		- Arcane Circle has a damage bonus against arcana for all party members as well as providing
		  protection against magic aggro. The recast of this ability has also been doubled to 10
		  minutes
		- Arcane Circle recast merits now decrease it's recast time by 20 seconds per merit instead
		  of 10
		- Desparate blows was a merit ability not available till ToAU. Now it's a job trait
		- "Souleater" has a recast of 5 minutes instead of 6
		- Souleater Recast merits now decrease it's recast by 10 seconds per merit instead of 12
		- "Last Resort"'s duration has been doubled from 30 to 60 seconds
--]]
	
--[[
	The "default" gear set is what is worn when you're not fighting (either you or your pet)
	and you're not resting. It covers everything else: idling, traveling, in town, etc. This 
	set displays what your character looks like most of the	time. This set does not 
	distinguish the type of activities you're doing by default, so use inlines accordingly.
--]]

	['Default'] = {
		Subset = {'TP_Tank//TANK', 'TP' },
		Head   = { 'Lilac Corsage//TOWN', 'Abs. Burgeonet +1', 'Empress Hairpin' },
		Body   = { 'Ducal Aketon//TOWN-AK', 'Plastron', 'Chaos Cuirass', 'Scorpion Harness', 'Brigandine', 'Beetle Harness' },
	},
	
--[[
	The TP sets are used when you are fighting.	The accuracy set will be applied in a fractional
	manner and the evasion set if /eva is specified. When tanking, Tank_TP will be equipped 
	instead of the TP set. It's a means for	the DRK to equip more defensive gear if they find
	themselves tanking. In Tank_TP, consider including shield skill+ gear.
--]]

	['TP'] = {
        Head  = { 'Abs. Burgeonet +1', 'Empress Hairpin' },
        Neck  = { 'Parade Gorget//SPECIAL', 'Opo-opo necklace//SLEPT', 'Peacock Amulet', 'Spike Necklace' },
        Ears  = { 'Bat Earring//BLINDED', 'Coral Earring//DT_MAGICAL', 'Brutal Earring', 'Coral Earring', 'Fang Earring', 'Genin Earring//SJNIN', 'Pilferer\'s Earring//SJTHF', 'Drone Earring', 'Energy Earring +1', 'Energy Earring +1' },
        Body  = { 'Plastron', 'Chaos Cuirass', 'Scorpion Harness', 'Brigandine', 'Beetle Harness' },        
		Hands = { 'Abs. Gauntlets +1', 'Thick Mufflers', 'Wonder Mitts' },
        Rings = { 'Flame Ring', 'Sun Ring', 'Sun Ring', 'Courage Ring', 'Tamas Ring', 'Kshama Ring No.2', 'Balance Ring' },
        Back  = { 'Forager\'s Mantle', 'Psilos Mantle', 'Amemet Mantle', 'Raptor Mantle', 'Ram Mantle' },
        Waist = { 'Swift Belt', 'Powerful Rope', 'Friar\'s Rope' },
        Legs  = { 'Thick Breeches', 'Ryl.Sqr. Breeches', 'San. Trousers' },
        Feet  = { 'Chs. Sollerets +1', 'Thick Sollerets', 'Bounding Boots' },
    },

	['Tank_TP'] = {
		Subset = 'TP',
		Legs  = 'Chaos Flanchard',
	},
	
--[[
	If an accuracy emphasis is desired, the following set will replace the gear, appropriately.
	Unlike TP though, accuracy is applied one slot at a time in a fractionalized manner using
	the /acc command.
	
	Include equipment with accuracy bonus and DEX. Remember, DEX converts to accuracy: (horizon) 
	for every 1 point of DEX you get 0.70 points of accuracy if wielding a 2H weapon, 0.65 for 
	a 1H weapon, and 0.60 for H2H. 
	
	Also, like the TP set, since DRK can tank, there's a Tank_Accuracy set.
--]]
		
	['Accuracy'] = {
		Head  = 'Optical Hat',
		Neck  = 'Peacock Amulet',
        Body  = { 'Abyss Cuirass', 'Scorpion Harness' },
		Rings = { 'Toreador\'s Ring', 'Woodsman Ring', 'Jaeger Ring', 'Kshama Ring No.2', 'Balance Ring' },
		Hands = { 'Abs. Gauntlets +1', 'Thick Mufflers' }, -- DEX from +1 gauntlets > 3 acc
		Waist = { 'Life Belt', 'Tilt Belt', 'Swift Belt' },
		Legs  = 'Thick Breeches',
		Feet  = { 'Chaos Sollerets +1', 'Bounding Boots' }, -- Needed to override the default Thick Sollerets
    },

	['Tank_Accuracy'] = {
		Head  = 'Optical Hat',
		Neck  = 'Peacock Amulet',
        Body  = 'Scorpion Harness',
		Rings = { 'Toreador\'s Ring', 'Woodsman Ring', 'Jaeger Ring', 'Kshama Ring No.2', 'Balance Ring' },
		Hands = 'Thick Mufflers',
		Waist = { 'Life Belt', 'Tilt Belt', 'Swift Belt' },
		Legs  = 'Thick Breeches',
		Feet  = { 'Chaos Sollerets +1', 'Bounding Boots' }, -- Needed to override the default Thick Sollerets	
	},
	
--[[
	If evasion wanted, equip evasion gear. Remember that AGI converts to evasion: for every
	2 points of AGI you get 1 point of evasion. Like TP and Accuracy, the evasion set has a
	Tank_Evasion variation.	
--]]

	['Evasion'] = {
        Head = 'Empress Hairpin',
		Body = 'Scorpion Harness',
		Ears  = { 'Genin Earring//SJNIN', 'Drone Earring', 'Windurstian Earring' },		
        Legs = { 'Chaos Flanchard', 'San. Trousers' },
    },

	['Tank_Evasion'] = {
        Head = 'Empress Hairpin',
		Body = 'Scorpion Harness',
		Ears  = { 'Genin Earring//SJNIN', 'Drone Earring', 'Windurstian Earring' },		
        Legs = { 'Chaos Flanchard', 'San. Trousers' },	
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
	
	['Resting_Refresh'] = {
		Neck = 'Parade Gorget//SPECIAL',
		Body = 'Plastron',		
	},
	
	-- If you have any Spell Interruption Rate down gear, put them into the "SIR" 
	-- gear set. This gear set is equipped in the gcinclude.HandleMidcast function 
	-- that all spells go through.
	['SIR'] = {
	},
	
--[[
	Start weapons are where you define what you want the first row of equipment to look 
	like when you either log in as a DRK or switch your main job to DRK. Any other gear 
	you mention will be overridden by the Idle or Town set, so no need to include here.
--]]

	['Start_Weapons'] = {
        Main = 'Balmung',
        Ammo = { 'Hedgehog Bomb', 'Fortune Egg' },
    },

--[[
	Magic accuracy gear for the player and/or if you have a pet
--]]

	['Macc'] = {
        Rings = 'Tamas Ring',
    },

--[[
	Magic Attack Bonus (MAB) is used for more than just spells, so it is broken out.
	MAB only affects damage dealing spells and elemental weapon skills
--]]

	['MAB'] = {
		Body = 'Abyss Cuirass',
	},
	
--[[
	Preshot is the first stage of when a ranged shot is being performed. This is where 
	you place any gear that reduces the time it takes to shoot (snap shot, rapid shot, 
	quick shot, and haste).
--]]

	['Preshot'] = {	
    },
	
--[[
	Midshot is the second stage of a ranged shot. This is where you place Ranged 
	Accuracy, Ranged Attack, Ranged Damage, recycle, etc.
--]]

	['Midshot'] = {
		Neck  = 'Peacock Amulet',
		Hands = 'Crimson Fng. Gnt.',
		Rings = { 'Woodsman Ring', 'Jaeger Ring', 'Beetle Ring +1', 'Beetle Ring +1' },	
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

--[[
	The second stage is Midcast. This is where you'll want to equip magic attack, or magic
	enhancing gear. (Magic Attack Bonus also happens here, but is broken out into it's own 
	gear set. See MAB.) Please note: if you want the recast reduction from fast cast, you
	must include the fast cast gear here too.
--]]	

	['Midcast'] = {
	},

--[[
	Further, there is a break out for each type of spell. I've included a comment on the type 
	of attributes the piece of gear should have.
--]]

	-- Healing: Healing Magic Skill, cure potency. Healing magic skill helps players regain
	-- hit points, remove negative status effects, deal damage to undead, and help players
	-- recover from being K.O.'ed. You should also consider adding cure potency gear to this
	-- set (excluding light staff which will be addressed later on in the gcinclude.midacast
	-- procedure.)
	['Healing'] = {
    },
	
	-- Dark: Dark Magic Skill. Dark magic skill determines accuracy, potency for some dark
	-- magic, the spell interruption rate of dark magic. Dark magic skill does not affect
	-- the potency of absorb spells, but does affect the accuracy and duration.	
	['Dark'] = {
		Head  = 'Chaos Burgeonet',
		Hands = 'Crimson Fng. Gnt.',
		Legs  = 'Abyss Flanchard',
    },

	-- Divine: Divine Magic Skill. Divine Magic is the smallest category of spells and 
	-- focuses on damaging and debilitating enemies with light elemental white magic
	-- spells. Divine Magic Skill increases magical accuracy and decreases spell 
	-- interruption rate. It does not increase the damage done by a divine spells.	
	['Divine'] = {
	},
	
	-- Enfeebling: Enfeebling Magic Skill. Enfeebling magic is a general category of
	-- spells that apply negative status effects to one or more enemy targets. Enfeebling
	-- magic skill determines the accuracy and spell interruption rate of Enfeebling
	-- magic. 	
	['Enfeebling'] = {
		Body = 'Chaos Cuirass',
		Feet = 'Abyss Sollerets',
	},
	
	-- Enhancing: Enhancing Magic Skill. Enhancing magic governs all magic that
	-- enhances the user and sometimes their party. While only some of the enhancing
	-- magic is affected by the caster's skill (e.g., enspells and stoneskin), 
	-- enhancing magic skill also affects the skill interruption rate for the
	-- enhancing skill being cast. The duration is affected by the level of the
	-- recipient of the enhancing skill. (The more levels they are under the base
	-- level of the spell, the more duration will be subtracted from the buff.
	['Enhancing'] = {
	},

	-- Elemental: Elemental Magic Skill. Elemental magic focuses on the destructive
	-- nature of the elementals. Elemental Magic Skill lowers the resistance rate
	-- while decreasing the likelihood off an elemental spell being interrupted. It
	-- does not affect the direct damage. Please note that including elemental staves
	-- in this gear set will be overriden later in the midcast process, so need to
	-- include it here, assuming /wswap is enabled.
	['Elemental'] = {
	},

	-- Ninjutsu: Ninjutsu Magic Skill, magic burst bonus, magic attack bonus. While not
	-- an actual magic skill per se, ninjutsu demonstrates expertise with the specialized 
	-- ninja tools to enfeeble or damage an opponent or buff the caster. The higher your 
	-- ninjutsu skill, the more effective your spells will be. Ninjutsu is affected by 
	-- Magical Accuracy, INT and MAB accordingly.	
	['Ninjutsu'] = {
	},
	
	-- Summoning: Summoning Magic Skill, Avatar Perpetuation Cost, Blood Pact Ability Delay.
	-- Summoning magic skill reduces the chance that a summons will be interrupted and 
	-- influences a summoner's elemental spirits. It decreases the wait time between when
	-- an elemental spirit is summoned and when it uses a spell and before casting another.
	-- Further, it increases the intelligence of the elemental spirit's AI. The spirit will
	-- tend to cast more powerful and relevant spells more often. If you are over the skill
	-- cap, it will increase the duration of a blood pact ward and for a blood pact rage,
	-- increase the accuracy and magic accuracy based on how far over cap the player is.
	['Summoning'] = {
	},
	
--[[
	Next is stat-based gear, (in this case intelligence or mind)
--]]

	-- INT is used to determine the effectiveness of elemental magic, black magic
	-- enfeebling spells, black magic enhancing skills, ninjutsu and some blue
	-- magic spells. INT reduces the damage taken from black magic on ninjutsu spells.
	-- INT also determines the additional effect from bloody bolts, earth arrows,
	-- water arrows and wind arrows. There's also indications that INT affects the
	-- success of THF's lock picking skill, reducing the chance of spawning a mimic
	-- or the chance of failure. INT is associated with the element ice
	['INT'] = {
        Rings = { 'Tamas Ring', 'Windurstian Ring' },
		Hands = 'Abs. Gauntlets +1',
        Waist = 'Mrc.Cpt. Belt',
		Legs  = 'Chaos Flanchard',
        Feet = 'Mannequin Pumps',
    },

	-- Tank_INT is a special version of the INT set that composits INT gear with
	-- tanking gear. If /tank enabled, this set will be equipped instead of the
	-- INT set
	['Tank_INT'] = {
	},

	-- MND is used to determine the effectiveness of healing magic spells, 
	-- white magic enhancing spells and white magic enfeebling spells, by
	-- increasing the damage and accuracy. MND increases resistance to white
	-- magic spells as well as reducing the base damage taken from them. MND 
	-- is associated with the element water
	['MND'] = {
        Neck = { 'Promise Badge', 'Justice Badge' },
        Body = { 'Abyss Cuirass', 'Wonder Kaftan' },
        Rings = { 'Tamas Ring', 'Tranquility Ring' },
        Waist = 'Mrc.Cpt. Belt',
        Legs = { 'Abyss Flanchard', 'Wonder Braccae' },
        Feet = { 'Chaos Sollerets +1', 'Mannequin Pumps' },
    },

	-- Tank_MND is a special version of the MND set that composits MND gear with
	-- tanking gear. If /tank enabled, this set will be equipped instead of the
	-- MND set
	['Tank_MND'] = {
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
	
	-- Drain: Drain Enhancement, Dark Magic Skill, Dark Magic Accuracy. Base 
	-- potency of the spell depends exclusively on dark magic skill. 
	--
	-- 0-300 skill: magic potency is floor(skill/3 + 20)
	-- 300+ skill: base potency is approximately floor(skill * 0.9)
	['Drain'] = {
    },
	
	-- Aspir: Aspir Enhancement, Dark Magic Skill, Dark Magic Accuracy. Base 
	-- potency of the spell depends exclusively on dark magic skill. 
	--
	-- 0-300 skill: magic potency is floor(skill.3 + 20)
	-- 300+ skill: base potency is floor(Skill * 0.4)
	['Aspir'] = {
    },
	
	-- Dread Spikes: Dread Spikes Potency, Dread Spikes Absorption, and Hit Points (HP). This is currently
	-- out of era.
    -- ['DreadSpikes'] = {
    -- },
	
	-- Sneak: Enhances Sneak and Enhances Stealth. 
	['Sneak'] = {
		Feet = 'Dream Boots +1',
	},
	
	-- Invisible: Enhances Invisible Effect.
	['Invisible'] = {
		Hands = 'Dream Mittens +1',
	},
	
	-- Phalanx: Enhancing Magic Skill. The amount of magical resistence
	-- this spell gives strictly is dependent on the amout of enhancing
	-- magic skill the player has. It is calculated by the following
	-- formula: 
	-- (Enhancing Magic Skill/10) - 2 if (enhancing magic skill <= 300)
	-- or floor((enhancing magic skill-300.5)/28.5+28 if (enhancing magic
	-- skill > 300). Damage reduction caps at 35 (500 enhancing magic skill).
	-- It stacks with other defense or damage reduction buffs.
	['Phalanx'] = {
	},
	
--[[
	The following weapon skill gearsets are defined by the stat they emphasize. Listed are all of the sets that
	you will need to use every weapon skill that your job can do. The leading comment defines what weapon/weapon
	skill combination the set applies to.
	
	DRK can use the following weapons: scythe (A+), great sword (A-), axe (B-), great axe (B-), sword (B-), dagger (C), club(C-),
	marksmanship (E). 
		
	Please note that on Horizon you may have access to some weapon skills
	through your subjob. While not explicitly supported here, the appropriate
	weapon skill set will be loaded. If not listed below, you might have to
	create a custom gear set to support the skill. Remember, weapon skill sets
	are named WS_attr. If you name the set appropriately, that set will auto-
	matically be called when you use the weapon skill.
--]]

--[[
		* Strength based *
		
		Scythe: Slice,Spinning Scythe,Vorpal Scythe
		Great Sword: Hard Slash,Crescent Moon
		Axe: Raging Axe,Smash Axe,Gale Axe,Avalanche Axe,Spinning Axe,Rampage,Decimation
		Great Axe: Iron Tempest,Sturmwind,Keen Edge,Raging Rush
		Sword: Flat Blade,Circle Blade,Spirits Within,Vorpal Blade
		Club: Starlight,Brainshaker,Moonlight,Skullbreaker,True Strike		
-]]
	
	['WS_STR'] = {
		Head  = 'Chaos Burgeonet',
        Neck  = 'Spike Necklace',
		Ears  = 'Beastly Earring//AXE',		-- Boosted skill if using an axe
		Body  = { 'Plastron', 'Chaos Cuirass', 'Wonder Kaftan' },
        Hands = { 'Wonder Mitts', 'Battle Gloves//ACCURACY', 'Ryl.Ftm. Gloves' },
        Rings = { 'Flame Ring', 'Sun Ring', 'Sun Ring', 'Courage Ring' },
        Waist = { 'Life Belt//ACCURACY', 'Mrc.Cpt. Belt' },
		Back  = { 'Forager\'s Mantle', 'Amemet Mantle' },
        Legs  = { 'Thick Breeches' ,'Wonder Braccae' },
        Feet  = { 'Chaos Sollerets +1', 'Creek F Clomps', 'Wonder Clomps' },
    },

--[[
		* Strength and Agility based, even weighting *
		
		Great Sword: Sickle Moon
--]]

	['WS_STRAGI'] = {
        Head  = { 'Chaos Burgeonet', 'Empress Hairpin' },
        Neck  = 'Spike Necklace',
        Ears  = { 'Genin Earring//SJNIN', 'Drone Earring', 'Beastly Earring//AXE' },
        Body  = { 'Plastron','Chaos Cuirass', 'Wonder Kaftan' },
        Hands = { 'Wonder Mitts', 'Battle Gloves//ACCURACY', 'Ryl.Ftm. Gloves' },
        Rings = { 'Flame Ring', 'Sun Ring', 'Sun Ring', 'Courage Ring' },
        Waist = { 'Life Belt//ACCURACY', 'Mrc.Cpt. Belt' },
		Back  = { 'Forager\'s Mantle', 'Amemet Mantle' },
        Legs  = { 'Thick Breeches', 'Ryl.Sqr. Breeches', 'Wonder Braccae' },
        Feet  = { 'Chaos Sollerets +1', 'Creek F Clomps', 'Wonder Clomps', 'Bounding Boots' },
    },
	
--[[
		* Strength and Dexterity based, even weighting *
		
		Sword: Fast Blade
--]]

	['WS_STRDEX'] = {
        Head  = { 'Chaos Burgeonet', 'Empress Hairpin' },
        Neck  = 'Spike Necklace',
        Ears  = { 'Genin Earring//SJNIN', 'Drone Earring', 'Beastly Earring//AXE' },
        Body  = { 'Plastron', 'Chaos Cuirass', 'Brigandine', 'Wonder Kaftan' },
        Hands = { 'Wonder Mitts', 'Battle Gloves//ACCURACY', 'Ryl.Ftm. Gloves' },
        Rings = { 'Flame Ring', 'Sun Ring', 'Sun Ring', 'Courage Ring', 'Kshama Ring No.2', 'Balance Ring' },
        Waist = { 'Life Belt//ACCURACY', 'Mrc.Cpt. Belt' },
		Back  = { 'Forager\'s Mantle', 'Amemet Mantle' },
        Legs  = { 'Thick Breeches', 'Ryl.Sqr. Breeches', 'Wonder Braccae' },
        Feet  = { 'Chaos Sollerets +1', 'Creek F Clomps', 'Wonder Clomps', 'Bounding Boots' },
    },

--[[
		* Strength and Intelligence based, even weighting *
		
		Scythe: Dark Harvest,Shadow of Death,Nightmare Scythe,Spiral Hell, Catastrophe
		Great Sword: Frostbite,Freezebite,Spinning Slash,Ground Strike
		Sword: Burning Blade
--]]
	
	['WS_STRINT'] = {
		Head  = 'Chaos Burgeonet', 
        Neck  = 'Spike Necklace',
		Ears  = 'Beastly Earring//AXE',		-- Boosted skill if using an axe
        Body  = { 'Plastron', 'Chaos Cuirass', 'Wonder Kaftan' },
        Hands = { 'Wonder Mitts', 'Battle Gloves//ACCURACY', 'Ryl.Ftm. Gloves' },
        Rings = { 'Flame Ring', 'Tamas Ring', 'Sun Ring', 'Sun Ring', 'Courage Ring' },
        Waist = { 'Life Belt//ACCURACY', 'Mrc.Cpt. Belt' },
		Back  = { 'Forager\'s Mantle', 'Amemet Mantle' },
        Legs  = { 'Thick Breeches', 'Wonder Braccae' },
        Feet  = { 'Chaos Sollerets +1', 'Creek F Clomps', 'Wonder Clomps' },
    },

--[[
		* Strength and Intelligence based, 30%/20% respectively *
		
		Sword: Red Lotus Blade
--]]
	
	['WS_STRINT_30_20'] = {
		Head  = 'Chaos Burgeonet',
        Neck  = 'Spike Necklace',
		Ears  = 'Beastly Earring//AXE',		-- Boosted skill if using an axe
        Body  = { 'Plastron', 'Chaos Cuirass', 'Wonder Kaftan' },
        Hands = { 'Wonder Mitts', 'Battle Gloves//ACCURACY', 'Ryl.Ftm. Gloves' },
        Rings = { 'Flame Ring', 'Tamas Ring', 'Sun Ring', 'Sun Ring', 'Courage Ring' },
        Waist = { 'Life Belt//ACCURACY', 'Mrc.Cpt. Belt' },
		Back  = { 'Forager\'s Mantle', 'Amemet Mantle' },
        Legs  = { 'Thick Breeches', 'Wonder Braccae' },
        Feet  = { 'Chaos Sollerets +1', 'Creek F Clomps', 'Wonder Clomps' },
    },

--[[
		* Strength and Mind based, even weighting *
		
		Scythe: Guillotine,Cross Reaper
		Great Sword: Shockwave
		Sword: Shining Blade,Seraph Blade
		Club: Shining Strike,Seraph Strike,Judgement
--]]

	['WS_STRMND'] = {
		Head  = 'Chaos Sollerets',
        Neck  = { 'Promise Badge','Justice Badge' },
		Ears  = 'Beastly Earring//AXE',		-- Boosted skill if using an axe
		Body  = { 'Plastron', 'Abyss Cuirass', 'Wonder Kaftan' },
        Hands = { 'Wonder Mitts', 'Battle Gloves//ACCURACY' },
        Rings = { 'Flame Ring', 'Tamas Ring', 'Sun Ring', 'Sun Ring', 'Courage Ring', 'Tranquility Ring' },
        Waist = { 'Life Belt//ACCURACY', 'Mrc.Cpt. Belt' },
		Back  = { 'Forager\'s Mantle', 'Amemet Mantle' },
        Legs  = 'Wonder Braccae',
        Feet  = { 'Chaos Sollerets +1', 'Creek F Clomps', 'Wonder Clomps' },
    },

--[[
		* Strength and Vitality based, even weighting *
		
		Great Sword: Power Slash,Scourge
		Great Axe: Shield Break,Armor Break,Weapon Break,Steel Cyclone
		Sword: Shining Blade,Seraph Blade,Swift Blade,Savage Blade
--]]
	
	['WS_STRVIT'] = {
		Head  = 'Chaos Sollerets',
        Neck  = 'Spike Necklace',
		Ears  = 'Beastly Earring//AXE',		-- Boosted skill if using an axe
		Body  = { 'Plastron', 'Wonder Kaftan' },
        Hands = { 'Wonder Mitts', 'Battle Gloves//ACCURACY' },
        Rings = { 'Flame Ring', 'Sun Ring', 'Sun Ring', 'Courage Ring' },
        Waist = { 'Life Belt//ACCURACY', 'Mrc.Cpt. Belt' },
		Back  = { 'Forager\'s Mantle', 'Amemet Mantle' },
        Legs  = { 'Wonder Braccae', 'San. Trousers' },
        Feet  = { 'Chaos Sollerets +1', 'Creek F Clomps', 'Wonder Clomps' },
    },

--[[
		* Agility based *
		
		Marksmanship: Hot Shot^,Split Shot^,Sniper Shot^,Slug Shot^
		
		^ RNG must be subjob
--]]

	['WS_AGI'] = {
		Head  = 'Empress Hairpin',
		Ears  = { 'Genin Earring//SJNIN', 'Drone Earring' },
		Waist = 'Mrc.Cpt. Belt',
		Legs  = 'Ryl.Sqr. Breeches',
		Feet  = 'Bounding Boots',
	},

--[[
		* Charisma based *
		
		Dagger: Shadowstitch
--]]
	
	['WS_CHR'] = {
        Neck  = { 'Star Necklace', 'Flower Necklace' },
		Ears  = 'Beastly Earring//AXE',		-- Boosted skill if using an axe
		Body  = 'Chaos Cuirass',
		Rings = { 'Moon Ring', 'Moon Ring' },
        Waist = { 'Corsette', 'Life Belt//ACCURACY', 'Mrc.Cpt. Belt' },
    },
	
--[[
		* Dexterity based *
		
		Dagger: Wasp Sting,Viper Bite^

		^ Subjob must be one of: RDM,THF,BRD,RNG,NIN
--]]
	
	['WS_DEX'] = {
        Head  = 'Empress Hairpin',
        Neck  = 'Spike Necklace',
		Ears  = 'Beastly Earring//AXE',		-- Boosted skill if using an axe
		Body  = 'Brigandine',
		Hands = { 'Abs. Gauntlets +1', 'Battle Gloves//ACCURACY', 'Ryl.Ftm. Gloves' },
        Rings = { 'Kshama Ring No.2', 'Balance Ring' },
        Waist = { 'Life Belt//ACCURACY', 'Mrc.Cpt. Belt' },
        Feet  = 'Bounding Boots',
    },

--[[
		* Dexterity and Intelligence based *
		
		Dagger: Gust Slash
--]]
	
	['WS_DEXINT'] = {
        Head  = 'Empress Hairpin',
        Neck  = 'Spike Necklace',
		Ears  = 'Beastly Earring//AXE',		-- Boosted skill if using an axe
		Body  = 'Brigandine',
		Hands = { 'Abs. Gauntlets +1', 'Battle Gloves//ACCURACY', 'Ryl.Ftm. Gloves' },
        Rings = { 'Kshama Ring No.2', 'Balance Ring' },
		Legs  = 'Chaos Flanchard',
        Waist = { 'Life Belt//ACCURACY', 'Mrc.Cpt. Belt' },
        Feet  = 'Bounding Boots',
    },

--[[
		* Mind based *
		
		Dagger: Energy Steal, Energy Drain
--]]

	['WS_MND'] = {
        Neck = { 'Promise Badge', 'Justice Badge' },
		Ears  = 'Beastly Earring//AXE',		-- Boosted skill if using an axe
        Body = { 'Abyss Cuirass', 'Wonder Kaftan' },
        Rings = { 'Tamas Ring', 'Tranquility Ring' },
        Waist = 'Mrc.Cpt. Belt',
        Legs = 'Wonder Braccae',
		Feet = { 'Chaos Sollerets +1', 'Mannequin Pumps' },
    },

--[[
		* Skill based *
		
		Club: Starlight,Moonlight
--]]

	['WS_Skill'] = {
		Ears  = 'Beastly Earring//AXE',		-- Boosted skill if using an axe
    },

--[[
		* HP based *
		
		Sword: Spirits Within
--]]

	['WS_HP'] = {
		Head  = 'Chaos Burgeonet',
        Ears  = 'Physical Earring',
        Body  = { 'Chaos Cuirass', 'Brigandine', 'Wonder Kaftan' },
        Hands = 'Crimson Fng. Gnt.',
        Rings = 'Toreador\'s Ring',
        Waist = 'Powerful Rope',
        Legs  = 'Wonder Braccae',
        Feet  = { 'Creek F Clomps', 'Chaos Sollerets' },
    },
	
--[[
	Kite is used for kiting. Emphasis should be placed on gear that increases 
	movement speed, but you might also want gear that has evasion. The choice
	is yours.
--]]

	['Kite'] = { 
	},
	
--[[
	The following are abilities affected by gear
--]]

	['Blood Weapon'] = {
	},
	
	['ArcaneCircle'] = {
		Feet = 'Chaos Sollerets +1',
    },
	
	['LastResort'] = {
		Feet = 'Abyss Sollerets',
    },
	
	['Souleater'] = {
		Head = 'Chaos Burgeonet',
    },
	
	['WeaponBash'] = {
		Hands = 'Chaos Gauntlets',
    },
	
--[[
	Some subjobs really make no sense when combined with paladin, but all abilities across all jobs that
	have gear that can be equipped by a DRK are included here.
--]]
	
	--* /BST *--, CHR gear.
	['Charm'] = {
        Neck = 'Flower Necklace',
        Rings = { 'Moon Ring', 'Moon Ring' },
        Waist = 'Corsette',
    },	

	['Reward'] = {
	},
	
	['Tame'] = {						-- Remember that higher if your INT is higher than the target's INT, you're less likely to be resisted
	},
	
	['Pet_Attack'] = {					-- Pet's strength, not accuracy
	},
	
	['Pet_Macc'] = {					-- Pet's Magical Accuracy
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
	},
	
	['Flee'] = {
	},
	
	['TrickAttack'] = {
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
	The following set is used to dynamically create a gear set to be displayed once rather
	than in a piecemeal manner. It is hoped that this will cut down on flickering gear and
	possibly speed up the code. *** This set is to be left empty by the player ***. Please
	do not modify it.
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
	['SCYTHE'] = { 'Tredecim Scythe','Suzaku\'s Scythe', 'Raven Scythe' },
	['DAGGER'] = { 'Heart Snatcher', 'Anubis\'s Knife', 'Garuda\'s Dagger', 'Bone Knife' },
	['SWORD']  = { 'Ifrit\'s Blade', 'Bee Spatha' },
	['GSWORD'] = { 'Balmung','Skofnung', 'Zweihander' },
	['H2H']    = { 'Lgn. Knuckles' },
	['AXE']    = { 'Maneater', 'Tabarzin', 'Fransisca', 'Darksteel Axe' },
	['GAXE']   = { 'Byakko\'s Axe', 'Axe of Trials', 'Rampager', 'Horror Voulge', 'Neckchopper' },
	['CLUB']   = { 'Warp Cudgel' },
	['STAVE']  = { 'Fire Staff', 'Vulcan\'s Staff', 'Ice Staff', 'Aquilo\'s Staff',
				  'Wind Staff', 'Auster\'s Staff', 'Earth Staff', 'Terra\'s Staff',
				  'Thunder Staff', 'Jupiter\'s Staff', 'Water Staff', 'Neptune\'s Staff',
				  'Light Staff', 'Apollo\'s Staff', 'Dark Staff', 'Pluto\'s Staff' },	
};

-- Accuracy Sets are predefined /acc commands. You identify them by a name and
-- a comma delimited list of slots. It's just a convenient shortcut mechanism.
profile.AccuracySet = {
	['base'] = 'Rings,Body',
	['full'] = 'Rings,Head,Body,Waist,Hands',
};

profile.Sets = sets;
profile.sjb = nil;
profile.bAmmo = false;		-- /BST specific. Is ammo equipped?
profile.sAmmo = nil;		-- /BST specific. Name of ammo equipped

--[[
	HandlePetAction equips the appropriate gear set based on the type of action
	the pet is trying to perform. Please note that only BST pets are supported,
	not SMN avatars.
--]]

local function HandlePetAction(PetAction)
	local pet = gData.GetPet();
	
	-- Only gear swap if this flag is true and the pet is a BST pet
	if gcdisplay.GetToggle('GSwap') == false or table.find(gcinclude.tSummonSkill,pet.Name) ~= nil then
		return;
	end

	if (gcinclude.BstPetAttack:contains(PetAction.Name)) then				-- Pet Attack
		gcinclude.MoveToCurrent(sets.Pet_Attack,sets.CurrentGear);
	elseif (gcinclude.BstPetMagicAttack:contains(PetAction.Name)) then		-- Pet Magical Attack
		gcinclude.MoveToCurrent(sets.Pet_Matt,sets.CurrentGear);
	elseif (gcinclude.BstPetMagicAccuracy:contains(PetAction.Name)) then	-- Pet Magical Accuracy Attack
		gcinclude.MoveToCurrent(sets.Pet_Macc,sets.CurrentGear);
    end
	gcinclude.EquipTheGear(sets.CurrentGear);
end		-- HandlePetAction

--[[
	SetSubjobSet is used to pick the appropriate set for the loaded macrobook based on
	which subjob is current. 
--]]

local function SetSubjobSet(chkSJ)
	-- "chkSJ" is the key for what toolbar is shown. All jobs are defined in the subs table.
	-- A value of 0 means that job is not configured. All values > 0 indicate which toolbar
	-- is to be displayed. The player must change the entries in this table to match their
	-- needs.
	local tSubs = {
		['WAR'] = 1, ['MNK'] = 0, ['WHM'] = 1, ['BLM'] = 0, ['RDM'] = 0, ['THF'] = 3,
		['PLD'] = 0, ['DRK'] = 0, ['BST'] = 0, ['BRD'] = 0, ['RNG'] = 0, ['SMN'] = 0,
		['SAM'] = 4, ['NIN'] = 1, ['DRG'] = 1, ['BLU'] = 0, ['COR'] = 0, ['PUP'] = 0,
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
	OnLoad is run whenever you log into your BST or change your job to BST
--]]

profile.OnLoad = function()
	local player = gData.GetPlayer();

	gSettings.AllowAddSet = true;
	gcinclude.Initialize();
	gcdisplay.SetToggle('Tank',false);		-- Assume DRK is not a tank

	-- Coded order of operation override
	gcinclude.settings.priorityEngaged = 'CEF';
	gcinclude.settings.priorityWeaponSkill = 'ADBE';
	
	-- Set which macro book should be displayed. Which macro set within the macro book to
	-- display depends on what your subjob is.
	AshitaCore:GetChatManager():QueueCommand(1, '/macro book 9');		-- DRK
	SetSubjobSet(player.SubJob);
	
	-- Load up the weapons bar. (This need only be done once.)
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

profile.OnUnload = function()
	gcinclude.Unload();
end		-- OnUnload

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
end		-- HandleCommand
	
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
	local bTank = gcdisplay.GetToggle('Tank');
	local eWeap = nil;
	local cKey;

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
		gcdisplay.Update();		-- in case something has changed	
		return;
	end

	-- Assuming you're /bst, when you want to reward your pet and you do not have pet food 
	-- equipped or when you want to summon a pet and a jug is not equipped, the current item 
	-- in the ammo slot is saved. The following will set it back to what you had before 
	-- either of those two items were equipped.
	if player.SubJob == 'BST' and profile.bAmmo then
		sets.CurrentGear['Ammo'] = profile.sAmmo;
		profile.sAmmo = nil;
		profile.bAmmo = false;
	end
	
	-- Clear out the CurrentGear in case of leftovers
	gcinclude.ClearSet(sets.CurrentGear);
	
	-- If player is not resting and has MP and has swapped weapons, set the weapon back to what 
	-- they had before the switch
	if player.Status ~= 'Resting' and 
			gcinclude.weapon ~= nil and
			gcdisplay.GetToggle('WSwap') == true and 
			eWeap ~= gcinclude.weapon then
		if gcinclude.fIsLocked('main') == false then
			sets.CurrentGear['Main'] = gcinclude.weapon;
		end
		if gcinclude.fIsLocked('sub') == false then
			sets.CurrentGear['Sub'] = gcinclude.weapon;
		end
	end
		
	-- Start with the default set
	gcinclude.MoveToCurrent(sets.Default,sets.CurrentGear);
		
	-- Now process the player status accordingly		
	if player ~= nil and player.Status == 'Engaged' then
		gcinclude.settings.priorityEngaged = string.upper(gcinclude.settings.priorityEngaged);
		for i = 1,string.len(gcinclude.settings.priorityEngaged),1 do
			cKey = string.sub(gcinclude.settings.priorityEngaged,i,i);
			if cKey == 'C' then		-- Evasion			
				if gcdisplay.GetToggle('Eva') == true then	
					if bTank == true then
						gcinclude.MoveToCurrent(sets.Tank_Evasion,sets.CurrentGear);
					else			
						gcinclude.MoveToCurrent(sets.Evasion,sets.CurrentGear);
					end
				end
			elseif cKey == 'E' then		-- Accuracy	
				gcinclude.FractionalAccuracy(sets.Accuracy,sets.Tank_Accuracy);
			elseif cKey == 'F' then		-- Kiting
				if (gcdisplay.GetToggle('Kite') == true) then
					gcinclude.MoveToCurrent(sets.Kite,sets.CurrentGear);
				end
			end				
		end
	elseif player.Status == 'Resting' then
		-- Player kneeling. Priority (low to high): regen,refresh
		if player.HP < player.MaxHP then		
			gcinclude.MoveToCurrent(sets.Resting_Regen,sets.CurrentGear);
		end
		
		if player.MP < player.MaxMP then
			gcinclude.MoveToCurrent(sets.Resting_Refresh,sets.CurrentGear);
			if gcdisplay.GetToggle('WSwap') == true then
				local sStave = gcinclude.fCheckForEleGear('staff','dark');
				if sStave ~= nil then
					gcinclude.fSwapToStave(sStave,false,sets.CurrentGear);
				end
			end	
		end			
	else
		-- Assume idling. While there's no idle set, just use the 
		-- "Default" set
		gcinclude.MoveToCurrent(sets.Default,sets.CurrentGear);
	end
				
	-- In case the pet is a summoned pet...
	if pet ~= nil and table.find(gcinclude.tSummonSkill,pet.Name) ~= nil and gcdisplay.GetToggle('WSwap') == true then
		local sStave = gcinclude.fCheckForElementalGearByValue('staff','Summons',pet.Name);
		if sStave ~= nil then
			gcinclude.fSwapToStave(sStave,false,sets.CurrentGear);
		end
	end
	
	-- And make sure a weapon equipped. (Going into a capped area can cause no weapon to be equipped.)
	local gear = gData.GetEquipment();
	if gear.Main == nil then
		gcinclude.MoveToCurrent(sets.Start_Weapons,sets.CurrentGear,true);
	end
	
	gcinclude.EquipTheGear(sets.CurrentGear);		-- Equip the composited HandleDefault set
	
	-- Lastly, update the display, just in case
	gcdisplay.Update();
end		-- HandleDefault

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
	if string.match(ability.Name, 'Blood Weapon') then
		gcinclude.MoveToCurrent(sets.BloodWeapon,sets.CurrentGear);
	elseif string.match(ability.Name, 'Arcane Circle') then
		gcinclude.MoveToCurrent(sets.ArcaneCircle,sets.CurrentGear);	
	elseif string.match(ability.Name, 'Last Resort') then
		gcinclude.MoveToCurrent(sets.LastResort,sets.CurrentGear);
	elseif string.match(ability.Name, 'Souleater') then
		gcinclude.MoveToCurrent(sets.Souleater,sets.CurrentGear);
	elseif string.match(ability.Name, 'WeaponBash') then
		gcinclude.MoveToCurrent(sets.WeaponBash,sets.CurrentGear);
	
	-- And now the subjob abilities	
	-- /BST
	elseif string.match(ability.Name, 'Charm') then	
		gcinclude.MoveToCurrent(sets.Charm,sets.CurrentGear);
		local sStave = gcinclude.fCheckForEleGear('staff','light');
		if sStave ~= nil then
			gcinclude.fSwapToStave(sStave,false,sets.CurrentGear);
		end
	elseif string.match(ability.Name, 'Reward') then
		-- Pet reward. Make sure that pet food already equipped
		if profile.sAmmo == nil or string.find(string.lower(profile.sAmmo),'pet f') == nil then		-- something else equipped
			profile.bAmmo = gcinclude.doPetFood('max',nil);
		end	
		gcinclude.MoveToCurrent(sets.Reward,sets.CurrentGear);
	elseif string.match(ability.Name, 'Tame') then
		gcinclude.MoveToCurrent(sets.Tame,sets.CurrentGear);
	-- /WAR
	elseif string.match(ability.Name, 'Provoke') then
		gcinclude.MoveToCurrent(sets.Provoke,sets.CurrentGear);
	elseif string.match(ability.Name, 'Berserk') then
		gcinclude.MoveToCurrent(sets.Berserk,sets.CurrentGear);
	elseif string.match(ability.Name, 'Defender') then
		gcinclude.MoveToCurrent(sets.Defender,sets.CurrentGear);
	elseif string.match(ability.Name, 'Warcry') then
		gcinclude.MoveToCurrent(sets.Warcry,sets.CurrentGear);
	-- /MNK
	elseif string.match(ability.Name, 'Boost') then
		gcinclude.MoveToCurrent(sets.Boost,sets.CurrentGear);
	elseif string.match(ability.Name, 'Focus') then
		gcinclude.MoveToCurrent(sets.Focus,sets.CurrentGear);
	elseif string.match(ability.Name, 'Dodge') then
		gcinclude.MoveToCurrent(sets.Dodge,sets.CurrentGear);
	elseif string.match(ability.Name, 'Chakra') then
		gcinclude.MoveToCurrent(sets.Chakra,sets.CurrentGear);
	-- /THF
	elseif string.match(ability.Name, 'Steal') then
		gcinclude.MoveToCurrent(sets.Steal,sets.CurrentGear);
	elseif string.match(ability.Name, 'Sneak Attack') then
		gcinclude.MoveToCurrent(sets.SneakAttack,sets.CurrentGear);
	elseif string.match(ability.Name, 'Flee') then
		gcinclude.MoveToCurrent(sets.Flee,sets.CurrentGear);
	elseif string.match(ability.Name, 'Trick Attack') then
		gcinclude.MoveToCurrent(sets.TrickAttack,sets.CurrentGear);
	elseif string.match(ability.Name, 'Mug') then
		gcinclude.MoveToCurrent(sets.Mug,sets.CurrentGear);
	-- /WHM
	elseif string.match(ability.Name, 'Divine Seal') then
		gcinclude.MoveToCurrent(sets.DivineSeal,sets.CurrentGear);
	-- /BLM
	elseif string.match(ability.Name, 'Elemental Seal') then
		gcinclude.MoveToCurrent(sets.ElementalSeal,sets.CurrentGear);
	-- /RNG
	elseif string.match(ability.Name, 'Sharpshot') then
		gcinclude.MoveToCurrent(sets.Sharpshot,sets.CurrentGear);
	elseif string.match(ability.Name, 'Scavenge') then
		gcinclude.MoveToCurrent(sets.Scavenge,sets.CurrentGear);
	elseif string.match(ability.Name, 'Camouflage') then
		gcinclude.MoveToCurrent(sets.Camouflage,sets.CurrentGear);
	elseif string.match(ability.Name, 'Barrage') then
		gcinclude.MoveToCurrent(sets.Barrage,sets.CurrentGear);	
	-- /SAM
	elseif string.match(ability.Name, 'Warding Circle') then
		gcinclude.MoveToCurrent(sets.WardingCircle,sets.CurrentGear);
	elseif string.match(ability.Name, 'Third Eye') then
		gcinclude.MoveToCurrent(sets.ThirdEye,sets.CurrentGear);
	elseif string.match(ability.Name, 'Hasso') then
		gcinclude.MoveToCurrent(sets.Hasso,sets.CurrentGear);
	elseif string.match(ability.Name, 'Meditate') then
		gcinclude.MoveToCurrent(sets.Meditate,sets.CurrentGear);
	elseif string.match(ability.Name, 'Seigan') then
		gcinclude.MoveToCurrent(sets.Seigan,sets.CurrentGear);
	-- /PLD
	elseif string.match(ability.Name, 'Holy Circle') then
		gcinclude.MoveToCurrent(sets.HolyCircle,sets.CurrentGear);
	elseif string.match(ability.Name, 'Shield Bash') then
		gcinclude.MoveToCurrent(sets.ShieldBash,sets.CurrentGear);
	elseif string.match(ability.Name, 'Sentinel') then
		gcinclude.MoveToCurrent(sets.Sentinel,sets.CurrentGear);	
	elseif string.match(ability.Name, 'Cover') then
		gcinclude.MoveToCurrent(sets.Cover,sets.CurrentGear);
	-- /DRG
	elseif string.match(ability.Name, 'Ancient Circle') then
		gcinclude.MoveToCurrent(sets.AncientCircle,sets.CurrentGear);	
	elseif string.match(ability.Name, 'Jump') then
		gcinclude.MoveToCurrent(sets.Jump,sets.CurrentGear);
	elseif string.match(ability.Name, 'High Jump') then
		gcinclude.MoveToCurrent(sets.HighJump,sets.CurrentGear);
	end
	gcinclude.EquipTheGear(sets.CurrentGear);		-- Equip the composited HandleAbility set
end		-- HandleAbility
	
--[[
	HandleItem is the place to equip gear when a special item is used. Currently only 'Holy Water' 
	is supported
--]]

profile.HandleItem = function()
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

profile.HandlePrecast = function()
    local spell = gData.GetAction();
	local obi;
	local mSet;
		
	-- Only gear swap if this flag is true
	if gcdisplay.GetToggle('GSwap') == false then
		return;
	end

	-- Clear out the CurrentGear in case of leftovers
	gcinclude.ClearSet(sets.CurrentGear);
	
	-- Equip the precast gear set
	gcinclude.MoveToCurrent(sets.Precast,sets.CurrentGear);
	gcinclude.EquipTheGear(sets.CurrentGear);
end		-- HandlePrecast

--[[
	HandleMidcast is the second function invoked when a player casts a spell. It equips gear appropriate for 
	magic skill, duration, magic attack bonus, magic accuracy, and potency. There's an order to how the pieces 
	are loaded: INT/MND, spell specific, macc, magic skill, obi, ele swap	
--]]

profile.HandleMidcast = function()
	local bTank = gcdisplay.GetToggle('Tank');
	
	if gcdisplay.GetToggle('GSwap') == false then		-- Only gear swap if this flag is true	
		return;
	end

	-- Clear out the CurrentGear in case of leftovers
	gcinclude.ClearSet(sets.CurrentGear);

	if bTank then
		gcinclude.settings.priorityMidCast = 'ACBDEGHF';
	else
		gcinclude.settings.priorityMidCast = 'ABCDEGHF';	
	end
	
	-- Call the common HandleMidcast now
	gcinclude.HandleMidcast(bTank);
	
	gcinclude.EquipTheGear(sets.CurrentGear);		-- Equip the composited midcast set
end		-- HandleMidcast

--[[
	HandlePreshot is similar to HandlePrecast, but for ranged actions. It loads Ranged Accuracy 
	and Ranged Shot Speed Gear for a ranged attack
--]]

profile.HandlePreshot = function()
	if gcdisplay.GetToggle('GSwap') == false then
		return;
	end
	
	-- Clear out the CurrentGear in case of leftovers
	gcinclude.ClearSet(sets.CurrentGear);
		
	gcinclude.MoveToCurrent(sets.Preshot,sets.CurrentGear);
	gcinclude.EquipTheGear(sets.CurrentGear);
end		-- HandlePreshot

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
	
	-- Equip the composited Midshot set	
	gcinclude.EquipTheGear(sets.CurrentGear);
end		-- HandleMidshot

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

	-- Call the common weaponskill handler
	gcinclude.fHandleWeaponskill();
	
	-- Equip the composited weaponskill set		
	gcinclude.EquipTheGear(sets.CurrentGear);
end		-- HandleWeaponskill

return profile;