local profile = {};
gcinclude = gFunc.LoadFile('common\\gcinclude.lua');

--[[
	This file contains all the gear sets associated with the NIN job.
	
	Gear Sets last updated: October 21, 2024
	Code update: October 21, 2024
--]]

local sets = {
--[[
	The gear sets are self contained, a mixture of direct gear assignments and conditional
	assignments. Each set contains entries identified by the gear slot. If it's a single
	value, it's a direct assignment like: Body = 'Austere Robe', but there can be multiple
	items identified, usually ordered by level: Body = { 'Vermillion Cloak//CARBY','Austere Robe' },
	Any item that has a // appended to it contains an inline conditional. The // code is a test
	to see if the item should be equipped. The level is still checked, but if the inline coded
	test is successful, that piece of gear will be loaded. If you've done a /gc command,
	the item's suitability for the job and accessibility will also be checked.
		
	Not all sets need to be defined. There is nothing wrong with leaving a set "empty", but don't
	delete any of the sets. All the ones listed here (except for any custom sets) are expected to 
	exist by Luashitacast. NIN supports "tanking", so you'll find some sets with an associated 
	"Tank_" set (ex. TP and Tank_TP). Minally include a subset entry in the Tank_ set so that 
	some gear is definied until you create a tanking set.
	
	Example:
	
	['Tank_TP'] = {
		Subset = 'TP',
	}
		
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
		- Ichi elemental wheel spells can be learned at level 12, but only usable from level 15 
		  onwards.
		- Ninjutsu Magic Bursts receive and extra +50 Magic Accuracy, +35 Magic Attack and reduced
		  enmity generation, except on notorious monsters.
		- Resistance down on all elemental ninjutsu is -30 regardless of the level of the ninjutsu.
		  The resistance down lasts 15 seconds, but can be extended to 25 seconds through merits.
		- Elemental wheel spells receive a total of +25 magic accuracy.
		- All ninjutsu have +15 magic accuracy with a further +10 for all elemental wheel spells
		  (+25 magic accuracy total.) This effect increases to +50 magic accuracy on magic bursts
		  on all non-notorius monsters.
		- Hojo: Ni and Kurayami: Ni both provide +40 cumulative enmity (CE) and +450 volitile 
		  enmity (VE). Essentially CE is halved and VE is doubled.
--]]

--[[
	The "default" gear set is what is worn when you're not fighting (either you or your pet)
	and you're not resting. It covers everything else: idling, traveling, in town, etc. This 
	set displays what your character looks like most of the	time. This set does not 
	distinguish the type of activities you're doing by default, so use inlines accordingly.
--]]
	
	['Default'] = {
		Subset = { 'Tank_TP//TANK','TP' },
		Head   = { 'Lilac Corsage//TOWN', 'Empress Hairpin' },
		Body   = { 'Ducal Aketon//TOWN-AK', 'Wonder Kaftan', 'Beetle Harness' },
	},
	
--[[
	The TP sets are used when you are fighting.	The accuracy set will be applied in a fractional
	manner and the evasion set if /eva is specified. When tanking, Tank_TP will be equipped 
	instead of the TP set. It's a means for	the NIN to equip more defensive gear if they find
	themselves tanking.
--]]
	
	['TP'] = {
		Head  = 'Empress Hairpin',
		Neck  = 'Peacock Amulet',
		Ears  = { 'Pilferer\'s Earring//SJTHF', 'Physical Earring', 'Energy Earring +1//MSJ', 'Energy Earring +1//MSJ' },
		Body  = { 'Wonder Kaftan', 'Beetle Harness' },
		Hands = { 'Wonder Mitts', 'Ryl.Ftm. Gloves' },
		Rings = { 'Tamas Ring//MSJ', 'Courage Ring', 'Balance Ring' },
		Back  = 'Ram Mantle',
		Waist = { 'Warrior\'s Belt', 'Friar\'s Rope//MSJ' },
		Legs  = { 'Ryl.Sqr. Breeches', 'Wonder Braccae', 'Ryl.Ftm. Trousers', 'Fisherman\'s Hose' },
		Feet  = { 'Mannequin Pumps//MSJ', 'Bounding Boots' },
    },
	
	['Tank_TP'] = {
		Head  = 'Empress Hairpin',
		Neck  = 'Peacock Amulet',
		Ears  = { 'Pilferer\'s Earring//SJTHF', 'Physical Earring', 'Energy Earring +1//MSJ', 'Energy Earring +1//MSJ' },
		Body  = { 'Wonder Kaftan', 'Beetle Harness' },
		Hands = { 'Wonder Mitts', 'Ryl.Ftm. Gloves' },
		Rings = { 'Tamas Ring//MSJ', 'Courage Ring', 'Balance Ring' },
		Back  = 'Ram Mantle',
		Waist = { 'Warrior\'s Belt', 'Friar\'s Rope//MSJ' },
		Legs  = { 'Ryl.Sqr. Breeches', 'Wonder Braccae', 'Ryl.Ftm. Trousers', 'Fisherman\'s Hose' },
		Feet  = { 'Mannequin Pumps//MSJ', 'Bounding Boots' },	
	},
	
--[[
	If an accuracy emphasis is desired, the following set will replace the gear, appropriately.
	Unlike TP though, accuracy is applied one slot at a time in a fractionalized manner using
	the /acc command.
	
	Include equipment with accuracy bonus and DEX. Remember, DEX converts to accuracy: (horizon) 
	for every 1 point of DEX you get 0.70 points of accuracy if wielding a 2H weapon, 0.65 for 
	a 1H weapon, and 0.60 for H2H. 
	
	Also, like the TP set, since NIN can tank, there's a Tank_Accuracy set.
--]]
	
	['Accuracy'] = {
		Head  = 'Empress Hairpin',
		Neck  = { 'Peacock Amulet', 'Spike Necklace' },
		Rings = { 'Woodsman Ring', 'Jaeger Ring', 'Balance Ring' },
		Waist = 'Tilt Belt',
		Legs  = 'Ryl.Sqr. Breeches',
		Feet  = 'Bounding Boots',
    },
	
	['Tank_Accuracy'] = {
		Head  = 'Empress Hairpin',
		Neck  = { 'Peacock Amulet', 'Spike Necklace' },
		Rings = { 'Woodsman Ring', 'Jaeger Ring', 'Balance Ring' },
		Waist = 'Tilt Belt',
		Legs  = 'Ryl.Sqr. Breeches',
		Feet  = 'Bounding Boots',
	},

--[[
	Similar to accuracy except will be used on ranged attacks
--]]

	['Ranged_Accuracy'] = {
	},

	['Tank_Ranged_Accuracy'] = {
	},
	
--[[
	If evasion wanted, equip evasion gear. Remember that AGI converts to evasion: for every
	2 points of AGI you get 1 point of evasion. Like TP and Accuracy, the evasion set has a
	Tank_Evasion variation.	
--]]
	
	['Evasion'] = {
		Head  = 'Empress Hairpin',
		Neck  = 'Spirit Torque',
		Ears  = 'Drone Earring',
		Rings = 'Balance Ring',
		Legs  = 'San. Trousers',
		Feet  = 'Bounding Boots',
    },

	['Tank_Evasion'] = {
		Head  = 'Empress Hairpin',
		Neck  = 'Spirit Torque',
		Ears  = 'Drone Earring',
		Rings = 'Balance Ring',
		Legs  = 'San. Trousers',
		Feet  = 'Bounding Boots',	
	},
	
--[[
	When you are resting (kneeling down), your HP 'Resting' set will be equipped. If your MP 
	is below the set threshhold (defined by gcinclude.settings.RefreshGearMP) though, your MP 
	'Resting_Refresh' gear set will be equipped. Regardless of which set is equipped, if you
	have a Dark/Pluto staff accessible, you've indicated that weapon swapping is permissible,
	and your MP is not at maximum, the Dark/Pluto staff will automatically be equipped.
--]]
	
	['Resting_Regen'] = { 
	},
	
	['Resting_Refresh'] = {
	},

	-- If your subjob can use magic, then place any Spell Interruption Rate down 
	-- gear into the "SIR" gear set. This set is equipped in the gcinclude.HandleMidcast
	-- function that all spells go through.
	['SIR'] = {
	},
	
--[[
	Start weapons are where you define what you want the first row of equipment to look 
	like when you either log in as a NIN or switch your main job to NIN. Any other gear 
	you mention will be overridden by the Idle or Town set, so no need to include here.
--]]

	['Start_Weapons'] = {
		Main = 'Zushio',
		Sub  = 'Anju',
		Ammo = 'Happy Egg',
    },
		
--[[
	Magic accuracy gear for the player and/or if you have a pet
--]]

	['Macc'] = {
		Ring1 = 'Tamas Ring',
    },

--[[
	Magic Attack Bonus (MAB) is used for more than just spells, so it is broken out.
	MAB only affects damage dealing spells and elemental weapon skills
--]]

	['MAB'] = {
	},
	
--[[
	Preshot is the first stage of when a ranged shot is being performed. This is where 
	you place any gear that reduces the time it takes to shoot (snap shot, rapid shot, 
	quick shot, and haste).
--]]

	['Preshot'] = {
    },
	
--[[
	Midshot is the second stage of a ranged shot. This is where you place 
	Ranged Attack, Ranged Damage, recycle, etc.
--]]

	['Midshot'] = {
		Neck = 'Peacock Amulet',
		Rings = { 'Woodsman Ring', 'Jaeger Ring', 'Beetle Ring +1', 'Beetle Ring +1' },
    },

--[[
	Spells are a bit different. Each type of spell can have it's own enhancement gear as well as 
	stat based gear. (In some cases individual spells have special entries.) These sets do not 
	include elemental gear which is	dependent on day/weather/weapon skill.

	The first stage is Precast. This is where you place any Fast Cast, cast time reduction, quick 
	cast gear, and spell interruption rate.
	
	Since NIN uses Ninjutsu, you may question the use of the Pre- and Mid-cast sets. While it is
	true that Ninjutsu does not expend MP, all the other factors hold true. Populate the sets 
	accordingly.
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
	Next is stat-based gear for spells: intelligence (INT) and mind (MND). Tank_INT and Tank_MND are subsets of the 
	simalarly names gear sets. The intent is to boost the appropriate attribute-based skill/spell without compromising
	your tanking gear.
--]]

	-- INT is used to determine the effectiveness of elemental magic, black magic
	-- enfeebling spells, black magic enhancing skills, ninjutsu and some blue
	-- magic spells. INT reduces the damage taken from black magic on ninjutsu spells.
	-- INT also determines the additional effect from bloody bolts, earth arrows,
	-- water arrows and wind arrows. There's also indications that INT affects the
	-- success of THF's lock picking skill, reducing the chance of spawning a mimic
	-- or the chance of failure. INT is associated with the element ice
	['INT'] = {
		Rings = 'Tamas Ring',
    },

	-- Tank_INT is a special version of the INT set that composits INT gear with
	-- tanking gear. If /tank enabled, this set will be equipped instead of the
	-- INT set	
	['Tank_INT'] = {
		Rings = 'Tamas Ring',	
	},

	-- MND is used to determine the effectiveness of healing magic spells, 
	-- white magic enhancing spells and white magic enfeebling spells, by
	-- increasing the damage and accuracy. MND increases resistance to white
	-- magic spells as well as reducing the base damage taken from them. MND 
	-- is associated with the element water
	['MND'] = {
		Neck  = 'Justice Badge',
		Rings = { 'Tamas Ring',	'Tranquility Ring' },
		Waist = 'Friar\'s Rope',
		Feet  = 'Mannequin Pumps',
    },

	-- Tank_MND is a special version of the MND set that composits MND gear with
	-- tanking gear. If /tank enabled, this set will be equipped instead of the
	-- MND set	
	['Tank_MND'] = {
		Neck  = 'Justice Badge',
		Rings = { 'Tamas Ring',	'Tranquility Ring' },
		Waist = 'Friar\'s Rope',
		Feet  = 'Mannequin Pumps',	
	},
	
--[[
	Some spells are special cases, so they require tailored gears sets.
--]]

	-- Stoneskin: Stoneskin Enhancement, Mind, and Enhancing Magic Skill. Mind is 3x more important than enhancing
	-- Magic Skill. The only pieces of gear a NIN can wear to enhance stoneskin is a Stone Gorget and Stone Mufflers. 
	-- There's no gear that a NIN (or any job) can wear to enhance magic. Note: This gear set has no effect on 
	-- Titan's Stoneskin blood pact.
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
	
	NIN can use the following weapons: Katana (A-), Dagger (C+), Sword (C), Great Katana (C-), Club (E), H2H (E),
	Throwing (A-), Marksmanship (C), Archery (E). 
		
	Please note that on Horizon you may have access to some weapon skills
	through your subjob. While not explicitly supported here, the appropriate
	weapon skill set will be loaded. If not listed below, you might have to
	create a custom gear set to support the skill. Remember, weapon skill sets
	are named WS_attr. If you name the set appropriately, that set will auto-
	matically be called when you use the weapon skill.
--]]

--[[	
		* Strength based *

		Sword: Flat Blade,Circle Blade,Vorpal Blade	
		Great Katana: Tachi: Enpi,Tachi: Hobaku,Tachi: Goten,Tachi: Kagero,
					  Tachi: Jinpu,Tachi: Yukikaze
-]]
	
	['WS_STR'] = {
		Neck  = 'Spike Necklace',
		Body  = 'Wonder Kaftan',
		Hands = 'Wonder Mitts',
		Rings = 'Courage Ring',
		Legs  = 'Wonder Braccae',
		Feet  = 'Wonder Clomps',
    },

--[[
		* Strength and Dexterity based, even weighting *
		
		Katana: Blade: Rin,Blade: Retsu,Blade: Jin,Blade: Ten,Blade: Ku
		Sword: Fast Blade
		H2H: Combo,Backhand Blow

--]]

	['WS_STRDEX'] = {
		Head  = 'Empress Hairpin',
		Neck  = 'Spike Necklace',
		Body  = 'Wonder Kaftan',
		Hands = 'Wonder Mitts',
		Rings = { 'Courage Ring', 'Balance Ring' },
		Legs  = { 'Ryl.Sqr. Breeches', 'Wonder Braccae' },
		Feet  = 'Bounding Boots',	
    },

--[[
		* Strength and Intelligence based, even weighting *
		
		Katana: Blade: Teki,Blade: To,Blade: Chi,Blade: Ei
		Sword: Burning Blade
--]]
	
	['WS_STRINT'] = {
		Neck  = 'Spike Necklace',
		Body  = 'Wonder Kaftan',
		Hands = 'Wonder Mitts',
		Rings = { 'Tamas Ring', 'Courage Ring' },
		Legs  = 'Wonder Braccae',
		Feet  = 'Wonder Clomps',	
    },
--[[
		* Strength and Intelligence based, 30%/20% respectively *
		
		Sword: Red Lotus Blade
--]]
	
	['WS_STRINT_30_20'] = {
		Neck  = 'Spike Necklace',
		Body  = 'Wonder Kaftan',
		Hands = 'Wonder Mitts',
		Rings = { 'Tamas Ring', 'Courage Ring' },
		Legs  = 'Wonder Braccae',
		Feet  = 'Wonder Clomps',
    },

--[[
		* Strength and Mind based, even weighting *
		
		Sword: Shining Blade,Seraph Blade,Swift Blade
		Great Katana: Tachi: Koki		
		Club: Shining Strike
--]]

	['WS_STRMND'] = {
		Body  = 'Wonder Kaftan',
		Hands = 'Wonder Mitts',
		Rings = { 'Tamas Ring', 'Courage Ring', 'Tranquility Ring' },
		Legs  = 'Wonder Braccae',
		Feet  = { 'Mannequin Pumps', 'Wonder Clomps' },
    },

--[[
		* Agility based, ranged *
		
		Marksmanship: Hot Shot^,Split Shot^,Sniper Shot^,Slug Shot^
		
		^ Ranger must be subjob
--]]
	
	['WS_RANGED_AGI'] = {
		Head  = 'Empress Hairpin',
		Ears  = 'Drone Earring',
		Legs  = 'Ryl.Sqr. Breeches',
		Feet  = 'Bounding Boots',
    },
	
--[[
		* Charisma based *
		
		Dagger: Shadowstitch
--]]
	
	['WS_CHR'] = {
		Neck  = 'Flower Necklace',
		Waist = 'Corsette',
    },
	
--[[
		* Dexterity based *
		
		Katana: Blade: Metsu
		Dagger: Wasp Sting,Viper Bite,Evisceration
--]]
	
	['WS_DEX'] = {
		Head  = 'Empress Hairpin',
		Neck  = 'Spike Necklace',
		Rings = 'Balance Ring',
		Legs  = 'Ryl.Sqr. Breeches',
		Feet  = 'Bounding Boots',
    },

--[[
		* Dexterity and Intelligence based *
		
		Dagger: Gust Slash,Cyclone
--]]
	
	['WS_DEXINT'] = {
		Head  = 'Empress Hairpin',
		Neck  = 'Spike Necklace',
		Rings = { 'Tamas Ring', 'Balance Ring' },
		Legs  = 'Ryl.Sqr. Breeches',
		Feet  = 'Bounding Boots',	
    },
	
--[[
		* Mind based *
		
		Dagger: Energy Steal,Energy Drain,
--]]
	
	['WS_MND'] = {
		Neck  = 'Justice Badge',
		Body  = 'Wonder Kaftan',
		Rings = { 'Tamas Ring', 'Tranquility Ring' },
		Waist = 'Friar\'s Rope',
		Legs  = 'Wonder Braccae',
		Feet  = 'Mannequin Pumps',
    },

--[[
		* Vitality based *

		H2H: Shoulder Tackle
--]]

	['WS_VIT'] = {
		Body  = 'Wonder Kaftan',
		Waist = 'Warrior\'s Belt',
		Legs  = 'Wonder Braccae',
    },
	
--[[
		* Skill based *
		
		Club: Starlight,Moonlight
--]]

	['WS_Skill'] = {
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

	['Mikage'] = {
	},
	
	['Yonin'] = {
	},
	
--[[
	Some subjobs really make no sense when combined with paladin, but all abilities across all jobs that
	have gear that can be equipped by a NIN are included here.
--]]
	--* BST *--
	['Charm'] = {		-- CHR and Charm + gear. (Every +1 Charm adds 5% Charm duration)
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

	--* /DRK *--
	['ArcaneCircle'] = {
	},
	
	['LastResort'] = {
	},
	
	['WeaponBash'] = {
	},

	['Souleater'] = {
	},
	
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
	['KATANA']  = { 'Anju', 'Zushio' },
	['CLUB']    = { 'Warp Cudgel' },
	['H2H']     = { 'Avengers' },
	['ARCHERY'] = { 'Power Bow +1' },
};

-- Accuracy Sets are predefined /acc commands. You identify them by a name and
-- a comma delimited list of slots. It's just a convenient shortcut mechanism.
profile.AccuracySet = {
	['base'] = 'Rings,Body',
};

profile.Sets = sets;
profile.sjb = nil;
profile.bAmmo = false;
profile.sAmmo = nil;

--[[
	HandlePetAction equips the appropriate gear set based on the type of action
	the pet is trying to perform. Please note that only BST pets are supported,
	not SMN avatars.
--]]

function HandlePetAction(PetAction)
	local pet = gData.GetPet();
	
	-- Only gear swap if this flag is true and the pet is a BST pet
	if gcdisplay.GetToggle('GSwap') == false or gcinclude.fSummonerPet() == true then
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

function SetSubjobSet(chkSJ)
	-- "chkSJ" is the key for what toolbar is shown. All jobs are defined in the subs table.
	-- A value of 0 means that job is not configured. All values > 0 indicate which toolbar
	-- is to be displayed. The player must change the entries in this table to match their
	-- needs.
	local tSubs = {
		['WAR'] = 1, ['MNK'] = 0, ['WHM'] = 0, ['BLM'] = 0, ['RDM'] = 0, ['THF'] = 1,
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
	OnLoad is run whenever you log into your NIN or change your job to NIN
--]]

function profile.OnLoad()
	local player = gData.GetPlayer();

	gSettings.AllowAddSet = true;
	gcinclude.Initialize();
	gcdisplay.SetToggle('Tank',true);		-- Assume NIN is a tank

	-- Coded order of operation override
	gcinclude.settings.priorityEngaged = 'CEF';
	gcinclude.settings.priorityWeaponSkill = 'ADBE';
	
	-- Set your job macro toolbar defaults here. Which set depends on the subjob
	AshitaCore:GetChatManager():QueueCommand(1, '/macro book 6');		-- NIN
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

function profile.OnUnload()
	gcinclude.Unload();
end		-- OnUnload

--[[
	HandleCommand is run when you type in a command defined in LUASHITACAST. The commands handled here instead
	of in gcinclude.HandleCommands are specific to NIN or the help system.
--]]

function profile.HandleCommand(args)
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
	their pet.
--]]
	
function profile.HandleDefault()
	local pet = gData.GetPet();
	local petAction = gData.GetPetAction();
	local player = gData.GetPlayer();
	local zone = gData.GetEnvironment();
	local ew = gData.GetEquipment();
	local eWeap = nil;
	local cKey;
	local bTank = gcdisplay.GetToggle('Tank');

	gcinclude.StartReminder();		-- See if reminder should be printed
	
	-- Only pet actions from BST are supported.
	if (petAction ~= nil and player.SubJob == 'BST') then
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
	
	-- Assuming you're /bst, when you want to reward your pet and you do not have pet food equipped 
	-- or when you want to summon a pet and a jug is not equipped, the current item in the ammo slot 
	-- is saved. The following will set it back to what you had before either of those two items 
	-- were equipped.
	if player.SubJob == 'BST' and profile.bAmmo then
		gFunc.ForceEquip('Ammo',profile.sAmmo);
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
		if bTank == true then
			gcinclude.MoveToCurrent(sets.Tank_TP,sets.CurrentGear);
		else
			gcinclude.MoveToCurrent(sets.TP,sets.CurrentGear);
		end
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
				if bTank == true then
					gcinclude.FractionalAccuracy(sets.Tank_Accuracy);
				else
					gcinclude.FractionalAccuracy(sets.Accuracy);
				end
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
		
		if gcinclude.fMagicalSubJob() == true and player.MP < player.MaxMP then
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
	if pet ~= nil and gcinclude.fSummonerPet() == true then
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

function profile.HandleAbility()
	local ability = gData.GetAction();
			
	if gcdisplay.GetToggle('GSwap') == false then
		return;
	end

	-- Clear out the CurrentGear in case of leftovers
	gcinclude.ClearSet(sets.CurrentGear);
	
	if string.match(ability.Name, 'Mikage') then
		gcinclude.MoveToCurrent(sets.Mikage,sets.CurrentGear);
	elseif string.match(ability.Name, 'Yonin') then
		gcinclude.MoveToCurrent(sets.Yonin,sets.CurrentGear);		
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
	-- /DRG
	elseif string.match(ability.Name, 'Ancient Circle') then
		gcinclude.MoveToCurrent(sets.AncientCircle,sets.CurrentGear);	
	elseif string.match(ability.Name, 'Jump') then
		gcinclude.MoveToCurrent(sets.Jump,sets.CurrentGear);
	elseif string.match(ability.Name, 'High Jump') then
		gcinclude.MoveToCurrent(sets.HighJump,sets.CurrentGear);
	-- /DRK
	elseif string.match(ability.Name, 'Arcane Circle') then
		gcinclude.MoveToCurrent(sets.ArcaneCircle,sets.CurrentGear);
	elseif string.match(ability.Name, 'Last Resort') then
		gcinclude.MoveToCurrent(sets.LastResort,sets.CurrentGear);
	elseif string.match(ability.Name, 'Weapon Bash') then
		gcinclude.MoveToCurrent(sets.WeaponBash,sets.CurrentGear);
	elseif string.match(ability.Name, 'Souleater') then
		gcinclude.MoveToCurrent(sets.Souleater,sets.CurrentGear);	
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

function profile.HandlePreshot()
	if gcdisplay.GetToggle('GSwap') == true then		-- Only gear swap if this flag is true
		-- Clear out the CurrentGear in case of leftovers
		gcinclude.ClearSet(sets.CurrentGear);	
		
		gcinclude.MoveToCurrent(sets.Preshot,sets.CurrentGear);
		gcinclude.EquipTheGear(sets.CurrentGear);
	end
end		-- HandlePreshot

--[[
	HandleMidshot is similar to HandleMidcast, but for ranged actions. It loads Ranged Attack 
	and Damage gear for a ranged attack
--]]

function profile.HandleMidshot()
	local b = gcdisplay.GetToggle('Tank');
	
	-- Only gear swap if this flag is true
	if gcdisplay.GetToggle('GSwap') == false then
		return;
	end

	-- Clear out the CurrentGear in case of leftovers
	gcinclude.ClearSet(sets.CurrentGear);
	
	gcinclude.MoveToCurrent(sets.Midshot,sets.CurrentGear);
	
	if b ~= nil and b == true then
		gcinclude.FractionalAccuracy(gProfile.Sets.Tank_Ranged_Accuracy);
	else
		gcinclude.FractionalAccuracy(gProfile.Sets.Ranged_Accuracy);
	end	
	
	-- Equip the composited Midshot set
	gcinclude.EquipTheGear(sets.CurrentGear);	
end		-- HandleMidshot

--[[
	HandleWeaponskill loads the gear appropriately for the weapon skill you're doing
--]]

function profile.HandleWeaponskill()
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