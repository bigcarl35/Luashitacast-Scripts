local profile = {};
gcinclude = gFunc.LoadFile('common\\gcinclude.lua');

--[[
	This file contains all the gear sets associated with the SMN job.
	
	Gear Sets last updated: July 78, 2024
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
	test is successful, that piece of gear will be equipped. Currently nothing checks to see
	if that item can be equipped by the job it's associated with let alone whether the player
	even has it accessible. Those are all planned for the future. In the mean time the onus is
	on the player to create the correct definitions.

	Not all sets need to be defined. There is nothing wrong with leaving a set "empty", but don't
	delete any of the sets. All the ones listed here (except for any custom sets) are expected to 
	exist by Luashitacast.
	
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
	Unlike most jobs, summoner's emphasis is fighting with your avatar. The ['TP'] set is combined
	for both you and your pet fighting. Make sure to use appropriate inline conditionals to 
	emphasize gear accordingly. ['Accuracy'] and ['Evasion'] also represent both pets and players.
	['Macc'] is for the player since pet Macc is applied during a blood pact and ['TP'] is still 
	combined though and emphasizes the pet since summoner's are bad fighters.
--]]

	['TP'] = {
        Head  = { 'Summoner\'s Horn//SMNPETMW', 'Shep. Bonnet//PETF', 'Austere Hat', 'Silver Hairpin' },
		Neck  = { 'Rep.Gold Medal//NOT_OWN','Uggalepih Pendant//NIGHTTIME', 'Fenrir\'s Torque//DAYTIME', 'Star Necklace', 'Spirit Torque', 'Justice Badge' },
		Ears  = { 'Bat Earring//BLIND', 'Loquac. Earring', 'Coral Earring//DT_MAGICAL', 'Bat Earring', 'Energy Earring +1', 'Energy Earring +1', 'Reraise Earring' },
        Body  = { 'Vermillion Cloak//CARBY','Summoner\'s Dblt.//SMNPETMD', 'Austere Robe', 'Seer\'s Tunic', 'Angler\'s Tunica' }, 
        Hands = { 'Carbuncle Mitts//CARBY', 'Smn. Bracers +1', 'Errant Cuffs', 'Carbuncle Mitts' },
		Rings = { 'Evoker\'s Ring', 'Tamas Ring', 'Ether Ring', 'Astral Ring', 'Astral Ring' },
        Back  = { 'Blue Cape', 'White Cape' },
        Waist = { 'Hierarch Belt', 'Powerful Rope', 'Friar\'s Rope' },
        Legs  = { 'Evoker\'s Spats//PETF', 'Summoner\'s Spats', 'Evoker\'s Spats', 'Shep. Hose', 'Fisherman\'s Hose' }, 
        Feet  = { 'Rostrum Pumps//NO_SMNPET', 'Summoner\'s Pgch.', 'Mannequin Pumps', 'Seer\'s Pumps', 'Waders'},
		Ammo  = { 'Hedgehog Bomb', 'Fortune Egg' },
	},

--[[
	If an accuracy emphasis is desired, the following set will replace the gear, appropriately.
	Unlike TP though, accuracy is applied one slot at a time in a fractionalized manner using
	the /acc command.
	
	Include equipment with accuracy bonus and DEX. Remember, DEX converts to accuracy: (horizon) 
	for every 1 point of DEX you get 0.70 points of accuracy if wielding a 2H weapon, 0.65 for 
	a 1H weapon, and 0.60 for H2H. 
--]]
	
	['Accuracy'] = {
        Head  = { 'Shep. Bonnet//PETF', 'Optical Hat' },
		Ears  = 'Beastly Earring//PETF',
		Neck  = 'Peacock Amulet',
		Body  = 'Austere Robe',			-- Needed to offset the "possible" Vermillion Cloak		
		Hands = { 'Carbuncle Mitts//CARBY', 'Smn. Bracers +1//SMNPET', 'Battle Gloves' },
		Rings = { 'Toreador\'s Ring', 'Woodsman Ring', 'Jaeger Ring', 'Balance Ring' },
		Waist = { 'Life Belt', 'Tilt Belt' },
		Legs  = 'Evoker\'s Spats//PETF',
    },
	
--[[
	If evasion wanted, equip evasion gear. Remember that AGI converts to evasion: for every
	2 points of AGI you get 1 point of evasion
--]]
	
	['Evasion'] = {
		Head  = { 'Optical Hat', 'Empress Hairpin' },
		Ears  = { 'Bat Earring//BLIND', 'Ethereal Earring', 'Genin Earring//SJNIN', 'Drone Earring' },
		Neck  = 'Spirit Torque',
		Body  = 'Austere Robe',			-- Needed to offset the "possible" Vermillion Cloak	
		Hands = 'Battle Gloves',
		Legs  = { 'Evoker\'s Spats', 'Shep. Hose//PETF'},
    },

--[[
	The "default" gear set is what is worn when you're not fighting (either you or your pet)
	and you're not resting. It covers everything else: idling, traveling, in town, etc. The
	"default" set replaces the "travel" set which replaced the "idle" set. I just think the
	new name makes more sense. This set displays what your character looks like most of the
	time. It also includes the new //town gear (there use to be a separate town set. That
	has been removed.) This set does not distinguish the type of activities you're doing by
	default, so use inlines accordingly. 
--]]
	
	['Default'] = {
		Subset = 'TP',
		Head   = { 'Lilac Corsage//TOWN', 'Summoner\'s Horn//SMNPETMW', 'Shep. Bonnet//PETF', 'Austere Hat', 'Silver Hairpin' },
		Body   = { 'Ducal Aketon//TOWN-AK', 'Vermillion Cloak//MPP.LT.90', 'Vermillion Cloak//CARBY','Summoner\'s Dblt.', 'Austere Robe', 'Seer\'s Tunic', 'Angler\'s Tunica' },
	},

	['Default_WPet'] = {
	},
	
--[[
	When you are resting (kneeling down), if your HP is not full, your HP 'Resting' 
	set will be equipped. If your MP is below maximum value, your MP 'Resting_Refresh' 
	gear set will be equipped. 
--]]
	
	['Resting_Regen'] = {
        Waist = 'Hierarch Belt',
    },
	
	['Resting_Refresh'] = {
		Main  = { 'Dark Staff', 'Kukulcan\'s Staff', 'Pilgrim\'s Wand' },
        Body  = { 'Errant Hpl.', 'Vermillion Cloak', 'Seer\'s Tunic' },
		Waist = 'Hierarch Belt',
	},
	
	-- If you have any Spell Interruption Rate down gear, put them into the "SIR" 
	-- gear set. This gear set is equipped in the gcinclude.HandleMidcast function 
	-- that all spells go through.
	['SIR'] = {
	},
	
	-- Blood pacts go through a simulated process that mimics spell casting. The precast
	-- happens when the blood pact is invoked (either rage or ward), loading the 'BP'
	-- gear set. You want gear that has Blood Pact Ability Delay or Blood Pact Recast
	-- abilities defined here. The midcast happens when the actual blood pact goes off.
	['BP'] = {
        Head  = { 'Summoner\'s Horn +1', 'Austere Hat' },
        Neck = 'Smn. Torque',
        Body  = { 'Summoner\'s Dblt.//SMNPETMD', 'Austere Robe' },
        Hands = { 'Carbuncle Mitts//CARBY', 'Smn. Bracers +1', 'Carbuncle Mitts' },
        Rings = 'Evoker\'s Ring',
		Legs  = 'Summoner\'s Spats',
		Feet = 'Summoner\'s Pgch.',
    },
	
--[[
	Rage blood pacts are devided by type: physical, magical, summoning skill, accuracy, 
	and hybrid. (Ward blood pacts do not have this type of distinction.) Each blood pact 
	though is of a fixed type and can be looked up. The following gear sets named
	SmnXXX where XXX is the type define the gear to be equipped when the blood pact
	goes off. Look to the specific gear set type for what gear stats are wanted.
--]]
	
	-- Physical rage blood pact: pet attack, pet accuracy, pet critical hit, blood pact 
	-- physical damage
	['SmnPhysical'] = {
        Head  = 'Shep. Bonnet',
		Ears  = 'Beastly Earring',
		Body  = { 'Summoner\'s Dblt.', 'Austere Robe' }, -- Needed to offset the "possible" Vermillion Cloak	
		Legs  = 'Evoker\'s Spats',		--  for pet accuracy
		Feet  = 'Summoner\'s Pgch.',
    },

	-- Magical rage blood pact: pet magic attack burst, pet magical attack, pet magical
	-- accuracy, and blood Pact magical damage
	['SmnMagical'] = {
	    Head = 'Shep. Bonnet',
		Body = 'Summoner\'s Dblt.',
    },

	-- Summoning skill rage blood pact. 
	['SmnSkill'] = {
        Head  = { 'Evoker\'s Horn', 'Austere Hat' },
        Neck  = 'Smn. Torque',
		Hands = 'Smn. Bracers +1',
        Rings = 'Evoker\'s Ring',
		Feet  = 'Nashira Crackows',
    },
	
	-- Accuracy blood pact: pet accuracy, pet magic accuracy
    ['SmnAccuracy'] = {
        Head  = 'Shep. Bonnet',
		Ears  = 'Beastly Earring',
		Body  = 'Austere Robe',			-- Needed to offset the "possible" Vermillion Cloak	
		Hands = 'Smn. Bracers +1',
		Legs  = 'Evoker\'s Spats',
    },
	
	-- Hybrid blood pact: 2x physical and 1x magical
    ['SmnHybrid'] = {
        Head  = 'Shep. Bonnet',
		Ears  = 'Beastly Earring',
		Body  = { 'Summoner\'s Dblt.', 'Austere Robe' },	
		Legs  = 'Evoker\'s Spats',
		Feet  = 'Summoner\'s Pgch.',
    },
	
--[[
	Start weapons are where you define what you want the first row of equipment to look 
	like when you either log in as a SMN or switch your main job to SMN. Any other gear 
	you mention will be overridden by the Default set, so no need to include here.
--]]

	['Start_Weapons'] = {
	    Main = { 'Earth Staff', 'Kukulcan\'s Staff', 'Solid Wand', 'Yew Wand' },
		Ammo = { 'Hedgehog Bomb', 'Fortune Egg' },
 	},
	
--[[
	Magic accuracy gear. This is player magical accuracy/attack. Pet macc/matt goes in 
	SmnAccuracy
--]]
	
	['Macc'] = {
		Rings = 'Tamas Ring',
		Feet  = 'Nashira Crackows',
    },
	
--[[
	Magic Attack Bonus (MAB) is used for more than just spells, so it is broken out.
	MAB only affects damage dealing spells and elemental weapon skills
--]]

	['MAB'] = {
		Neck = 'Uggalepih Pendant//SPECIAL',
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
		Head  = 'Optical Hat',
		Neck  = 'Peacock Amulet',
		Body  = 'Austere Robe',		-- To bypass the conditions of a v.cloak
		Rings = { 'Woodsman Ring', 'Jaeger Ring', 'Beetle Ring +1', 'Beetle Ring +1' },
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
		Ears = 'Loquac. Earring',
		Feet = 'Rostrum Pumps',
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
	-- include it here.
	['Elemental'] = {
		Feet = 'Nashira Crackows',
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
		Head  = 'Austere Hat',
        Neck  = 'Smn. Torque',
		Body  = { 'Summoner\'s Dblt.', 'Austere Robe' },	-- offset for v.cloak
		Hands = { 'Smn. Bracers +1', 'Carbuncle Mitts' },
        Rings = 'Evoker\'s Ring',
		Legs  = 'Evoker\'s Spats',
		Feet  = 'Nashira Crackows',
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
		Head  = 'Summoner\'s Horn +1',
        Hands = { 'Errant Cuffs', 'Seer\'s Mitts' },
        Rings = 'Tamas Ring',
		Body  = 'Errant Hpl.',
        Waist = 'Mrc.Cpt. Belt',
        Legs  = { 'Errant Slops', 'Seer\'s Slacks' },
        Feet  = { 'Rostrum Pumps', 'Mannequin Pumps' },
    },

	-- MND is used to determine the effectiveness of healing magic spells, 
	-- white magic enhancing spells and white magic enfeebling spells, by
	-- increasing the damage and accuracy. MND increases resistance to white
	-- magic spells as well as reducing the base damage taken from them. MND 
	-- is associated with the element water
	['MND'] = {
        Neck  = { 'Promise Badge', 'Justice Badge' },
        Body  = { 'Errant Hpl.', 'Wonder Kaftan' },
        Hands = 'Seer\'s Mitts',
		Rings = { 'Tamas Ring', 'Kshama Ring No.9', 'Tranquility Ring' },
        Back  = 'White Cape',
        Waist = { 'Mrc.Cpt. Belt', 'Friar\'s Rope' },
        Legs  = { 'Errant Slops', 'Summoner\'s Spats', 'Wonder Braccae' },
        Feet  = { 'Rostrum Pumps', 'Mannequin Pumps', 'Seer\'s Pumps' },
    },
	
--[[
	And some spells are special cases, so they have individual gears sets.
--]]
	
	-- Stoneskin: Stoneskin Enhancement, Mind, and Enhancing Magic Skill. 
	-- Stoneskin heavily depends on MND and to a lesser degree, the player's
	-- Enhancing Magic skill level. 
	-- 
	-- The effect is calculated as follows:
	--   BASE = Enhancing Magic Skill/3 + MND. If the BASE is < 80, then
	--   the amount of protection is the BASE. If between 80 and 130, the
	--   amount is (2 * BASE) - 60. If greater than 130, it's (3 * BASE) - 190.
	--   Cap is 350 although wearing gear that "enhances" stoneskin can raise
	--   the cap to 380.
	--
	-- Note: Titan's Earthen Ward does not use the summoner's stats. The amount
	-- of the stoneskin is (avatar's level * 2) + 50 HP. Thus, earthen ward is
	-- treated as a normal blood pact ward and not as part of stoneskin
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
		Feet = 'Dream Boots +1',				-- enhances sneak
	},
	
	-- Invisible: Enhances Invisible Effect.
	['Invisible'] = {
		Hands = 'Dream Mittens +1',				-- enhances invisibility
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
	The following weapon skill gearsets are defined by the stat they emphasize. 
	Listed are all of the sets that	you will need to use every weapon skill 
	that your job can do. The leading comment defines what weapon/weapon skill
	combination the set applies to.
	
	SMN can use the following weapons: staff (B), Club (C+), dagger (E). 
	
	Please note that on Horizon you may have access to some weapon skills
	through your subjob. While not explicitly supported here, the appropriate
	weapon skill set will be loaded. If not listed below, you might have to
	create a custom gear set to support the skill. Remember, weapon skill sets
	are named WS_attr. If you name the set appropriately, that set will auto-
	matically be called when you use the weapon skill.
--]]

--[[
		* Strength based *
		
		Staff: Heavy Swing,Shell Crusher,Full Swing
		Club: Brainshaker,Skullbreaker,True Strike
-]]
	
	['WS_STR'] = {
        Head  = 'Mrc.Cpt. Headgear',
        Neck  = 'Spike Necklace',
        Body  = 'Wonder Kaftan',
        Hands = 'Wonder Mitts',
		Rings = { 'Flame Ring', 'Sun Ring', 'Sun Ring', 'Courage Ring' },
        Waist = 'Mrc.Cpt. Belt',
        Legs  = 'Wonder Braccae',
        Feet  = { 'Creek F Clomps', 'Wonder Clumps' },
    },
	
--[[
		* Strength and Intelligence based, even weighting *
		
		Staff: Rock Crusher,Earth Crusher,Cataclysm
--]]
	
	['WS_STRINT'] = {
        Head  = { 'Summoner\'s Horn +1', 'Mrc.Cpt. Headgear' },
        Neck  = 'Spike Necklace',
        Body  = { 'Errant Hpl.', 'Wonder Kaftan' },
        Hands = 'Wonder Mitts',
		Rings = { 'Tamas Ring', 'Flame Ring', 'Sun Ring', 'Courage Ring' },
        Waist = 'Mrc.Cpt. Belt',
        Legs  = { 'Errant Slops', 'Seer\'s Slacks' },
        Feet  = { 'Creek F Clomps', 'Wonder Clumps' },
    },

--[[
		* Strength and Mind based, even weighting *
		
		Club: Shining Strike,Seraph Strike,Judgement
		Staff: Retribution
--]]
	
	['WS_STRMND'] = {
        Head  = 'Mrc.Cpt. Headgear',
        Neck  = { 'Promise Badge', 'Justice Badge' },
        Body  = { 'Errant Hpl.', 'Wonder Kaftan' },
        Hands = 'Wonder Mitts',
		Rings = { 'Tamas Ring', 'Flame Ring', 'Sun Ring', 'Kshama Ring No.9', 'Courage Ring', 'Tranquility Ring' },
        Back  = 'White Cape',
        Waist = 'Mrc.Cpt. Belt',
        Legs  = { 'Errant Slops', 'Summoner\'s Spats', 'Wonder Braccae' },
        Feet  = { 'Creek F Clomps', 'Wonder Clumps' },
    },

--[[
		* Strength and Mind based, 30% to 50% weighting *
		
		Club: Black Halo
--]]

	['WS_STRMND_30_50'] = {
        Head  = 'Mrc.Cpt. Headgear',
        Neck  = { 'Promise Badge', 'Justice Badge' },
        Body  = { 'Errant Hpl.', 'Wonder Kaftan' },
        Hands = 'Wonder Mitts',
		Rings = { 'Tamas Ring', 'Flame Ring', 'Kshama Ring No.9', 'Sun Ring', 'Sun Ring', 'Courage Ring', 'Tranquility Ring' },
        Back  = 'White Cape',
        Waist = 'Mrc.Cpt. Belt',
        Legs  = { 'Errant Slops', 'Summoner\'s Spats', 'Wonder Braccae' },
        Feet  = { 'Creek F Clomps', 'Wonder Clumps' },
    },
	
--[[
		* Dexterity based *
		
		Dagger: Wasp Sting,Viper Bite^

		^ Subjob must be one of: RDM,THF,BRD,RNG,NIN
--]]
	
	['WS_DEX'] = {
        Head  = 'Empress Hairpin',
        Neck  = 'Spike Necklace',
        Body  = 'Mrc.Cpt. Doublet',
        Rings = { 'Kshama Ring No.2', 'Balance Ring' },
        Waist = 'Mrc.Cpt. Belt',
    },

--[[
		* Dexterity and Intelligence based *

		Dagger: Gust Slash,Cyclone^

		^ Subjob must be one of: RDM,THF,BRD,RNG,NIN
--]]
	
	['WS_DEXINT'] = {
        Head  = 'Empress Hairpin',
        Neck  = 'Spike Necklace',
        Body  = { 'Errant Hpl.', 'Mrc.Cpt. Doublet' },
        Hands = 'Seer\'s Mitts',
		Rings  = { 'Tamas Ring', 'Kshama Ring No.2', 'Balance Ring' },
        Waist = 'Mrc.Cpt. Belt',
        Legs  = { 'Errant Slops', 'Seer\'s Slacks' },
        Feet  = { 'Rostrum Pumps', 'Mannequin Pumps' },
    },

--[[
		* Intellegence *

		Staff: Gate of Tartarus
--]]
	
	['WS_INT'] = {
		Head  = 'Summoner\'s Horn +1',
        Hands = 'Seer\'s Mitts',
        Rings = 'Tamas Ring',
		Body  = 'Errant Hpl.',
        Waist = 'Mrc.Cpt. Belt',
        Legs  = { 'Errant Slops', 'Seer\'s Slacks' },
        Feet  = { 'Rostrum Pumps', 'Mannequin Pumps' },
    },
	
--[[
		* Intellegence and Mind based, even weighting *

		Staff: Spirit Taker
--]]
	
	['WS_INTMND'] = {
		Head  = 'Summoner\'s Horn +1',
        Neck  = { 'Promise Badge', 'Justice Badge' },
        Body  = { 'Errant Hpl.', 'Wonder Kaftan' },
        Hands = 'Seer\'s Mitts',
		Rings = { 'Tamas Ring', 'Kshama Ring No.9', 'Tranquility Ring' },
        Waist = 'Mrc.Cpt. Belt',
        Legs  = { 'Errant Slops', 'Summoner\'s Spats', 'Wonder Braccae' },
        Feet  = { 'Rostrum Pumps', 'Mannequin Pumps' },
    },
	
--[[
		* Charisma based *

		Dagger: Shadowstitch
--]]
	
	['WS_CHR'] = {
        Head  = 'Entrancing Ribbon',
        Neck  = 'Flower Necklace',
		Ears  = 'Beastly Earring',
		Body  = 'Errant Hpl.',
        Rings = { 'Moon Ring', 'Moon Ring' },
        Waist = { 'Corsette', 'Mrc.Cpt. Belt' },
		Legs  = 'Errant Slops',
    },

--[[
		* Mind based *

		Dagger: Energy Steal, Energy Drain^
		
		^ Subjob must be RDM,THF,BRD,RNG, or NIN
--]]

	['WS_MND'] = {
        Neck  = { 'Promise Badge', 'Justice Badge' },
        Body  = { 'Errant Hpl.', 'Wonder Kaftan' },
        Hands = 'Seer\'s Mitts',
		Rings = { 'Tamas Ring', 'Kshama Ring No.9', 'Tranquility Ring' },
        Back  = 'White Cape',
        Waist = { 'Mrc.Cpt. Belt', 'Friar\'s Rope' },
        Legs  = { 'Errant Slops', 'Summoner\'s Spats', 'Wonder Braccae' },
        Feet  = { 'Rostrum Pumps', 'Mannequin Pumps' },
    },
	
--[[
		* Skill based *
		
		Club: Starlight,Moonlight
--]]
	
	['WS_Skill'] = {
    },
	
--[[
	Movement tends to be used for kiting. Emphasis should be placed on gear 
	that increases movement speed, but you might also want gear that has evasion. 
	The choice is yours.
--]]

	['Movement'] = { 
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
	    Head  = 'Entrancing Ribbon',
        Neck  = 'Flower Necklace',
		Ears  = 'Beastly Earring',
		Body  = 'Errant Hpl.',
        Rings = { 'Moon Ring', 'Moon Ring' },
        Waist = { 'Corsette', 'Mrc.Cpt. Belt' },
		Legs  = 'Errant Slops',
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

-- Accuracy Sets are predefined for /acc commands. You identify them by a 
-- name and a comma delimited list of slots. It's just a convenient shortcut 
-- mechanism.
profile.AccuracySet = {
	['base'] = 'Rings,Body',
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

local function HandlePetAction(Pet,PetAction)
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
		if (gcinclude.SmnSkill:contains(PetAction.Name)) then			-- summoning skill based blood pact?
			gcinclude.fMoveToCurrent(sets.SmnSkill,sets.CurrentGear);		
		elseif (gcinclude.SmnMagical:contains(PetAction.Name)) then		-- magical based blood pact?
			gcinclude.fMoveToCurrent(sets.SmnMagical,sets.CurrentGear);	
			-- If /acc flagged, load accuracy set (for magical accuracy)
			if gcdisplay.GetToggle('Acc') == true then
				gcinclude.fMoveToCurrent(sets.SmnAccuracy,sets.CurrentGear);			
			end
		elseif (gcinclude.SmnHybrid:contains(PetAction.Name)) then		-- hybrid blood pact (2x physical, 1x magical)?
			gcinclude.fMoveToCurrent(sets.SmnHybrid,sets.CurrentGear);		
			-- If /acc flagged, load accuracy set (for both physical and magical accuracy)
			if gcdisplay.GetToggle('Acc') == true then
				gcinclude.fMoveToCurrent(sets.SmnAccuracy,sets.CurrentGear);
			end				
		else																-- physical	blood pact
			gcinclude.fMoveToCurrent(sets.SmnPhysical,sets.CurrentGear);			
			-- if /acc flagged, load accuracy set (for physical accuracy)
			if gcdisplay.GetToggle('Acc') == true then
				gcinclude.fMoveToCurrent(sets.SmnAccuracy,sets.CurrentGear);
			end
		end
	else
		-- Must be a /BST charmed pet. Since the accuracy sets are based on
		-- a SMN's avatar, if you want accuracy gear here, you must use the
		-- //ACCURACY inline in the specified Pet_xxx sets, accordingly.
		if (gcinclude.BstPetAttack:contains(PetAction.Name)) then				-- Pet Attack
			gcinclude.fMoveToCurrent(sets.Pet_Attack,sets.CurrentGear);		
		elseif (gcinclude.BstPetMagicAttack:contains(PetAction.Name)) then		-- Pet Magical Attack
			gcinclude.fMoveToCurrent(sets.Pet_Matt,sets.CurrentGear);		
		elseif (gcinclude.BstPetMagicAccuracy:contains(PetAction.Name)) then	-- Pet Magical Accuracy Attack
			gcinclude.fMoveToCurrent(sets.Pet_Macc,sets.CurrentGear);		
		end
	end
	gcinclude.fEquipTheGear(sets.CurrentGear);
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

profile.OnLoad = function()
	local player = gData.GetPlayer();

	-- Initialize settings
	gSettings.AllowAddSet = true;
	gcinclude.Initialize();
	gcinclude.settings.bWSOverride = true;
	
	-- Coded order of operation override
	gcinclude.settings.priorityEngaged = 'CEF';
	gcinclude.settings.priorityMidCast = 'ABCDEGHF';
	gcinclude.settings.priorityWeaponSkill = 'ADBE';	
	
	-- Set which macro book should be displayed. Which macro set within the macro book to
	-- display depends on what your subjob is.
	AshitaCore:GetChatManager():QueueCommand(1, '/macro book 13');		-- SMN macro book
	SetSubjobSet(player.SubJob);
	
	-- Load up the weapons bar.
	gcinclude.fMoveToCurrent(sets.Start_Weapons,sets.CurrentGear);
	gcinclude.fEquipTheGear(sets.CurrentGear);
	
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
	of in gcinclude.HandleCommands are specific to SMN or the help system.
--]]

profile.HandleCommand = function(args)
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
	
profile.HandleDefault = function()
	local pet = gData.GetPet();
	local petAction = gData.GetPetAction();
	local player = gData.GetPlayer();
	local zone = gData.GetEnvironment();
	local ew = gData.GetEquipment();
	local eWeap = nil;
	local cKey,sGear;

	
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
	
	-- Only gear swap if this flag is true
	if gcdisplay.GetToggle('GSwap') == false then
		gcdisplay.Update();
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
				gcinclude.fFractionalSet(sets.Start_Weapons,'Main');
			end
		end
	end

	-- Start with the default set
	gcinclude.fMoveToCurrent(sets.Default,sets.CurrentGear);
	
	-- Now process the pet/player statuses accordingly.
	if (pet ~= nil and pet.Status == 'Engaged') or (player ~= nil and player.Status == 'Engaged') then
		gcinclude.settings.priorityEngaged = string.upper(gcinclude.settings.priorityEngaged);
		for i = 1,string.len(gcinclude.settings.priorityEngaged),1 do
			cKey = string.sub(gcinclude.settings.priorityEngaged,i,i);
			if cKey == 'C' then		-- Evasion
				if gcdisplay.GetToggle('Eva') == true then
					gcinclude.fMoveToCurrent(sets.Evasion,sets.CurrentGear);
				end
			elseif cKey == 'E' then		-- Accuracy
				gcinclude.fFractionalAccuracy(sets.Accuracy,nil);
			elseif cKey == 'F' then		-- Kiting
				if (gcdisplay.GetToggle('Kite') == true) then
					gcinclude.fMoveToCurrent(sets.Movement,sets.CurrentGear);
				end
			end
		end
	elseif player.Status == 'Resting' then	
		-- Player kneeling. Priority (low to high): regen, refresh
		
		if player.HP < player.MaxHP then		
			gcinclude.fMoveToCurrent(sets.Resting_Regen,sets.CurrentGear);
		end

		if player.MP < player.MaxMP then
			gcinclude.fMoveToCurrent(sets.Resting_Refresh,sets.CurrentGear);
		end

	elseif gcinclude.fSummonerPet() then
		-- Player idling with pet
		gcinclude.fMoveToCurrent(sets.Default_WPet,sets.CurrentGear);
	else
		-- Assume player idling without pet or /subjob's pet
		gcinclude.fMoveToCurrent(sets.Default,sets.CurrentGear);
	end
		
	-- Make sure to equip the appropriate elemental staff (if appropriate)
	-- for the current pet

	if (pet ~= nil and table.find(gcinclude.tSummonSkill,pet.Name) ~= nil) then
		local sStave = gcinclude.fCheckForElementalGearByValue('staff','Summons',pet.Name);
		if sStave ~= nil then
			gcinclude.fSwapToStave(sStave,false,sets.CurrentGear);
		end
	end
	
	-- And make sure a weapon equipped. (Going into a capped area can cause no weapon to be equipped.)
	local gear = gData.GetEquipment();
	if gear.Main == nil then
		gcinclude.fMoveToCurrent(sets.Start_Weapons,sets.CurrentGear,true);
	end
	
	-- Equip the composited HandleDefault set
	gcinclude.fEquipTheGear(sets.CurrentGear);
	
	-- Lastly, update the display, just in case
	gcdisplay.Update();
end		-- HandleDefault

--[[
	HandleAbility is used to change the player's gear appropriately for the specified avatar ability.
--]]

profile.HandleAbility = function()
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
			gcinclude.fMoveToCurrent(sets.Reward,sets.CurrentGear);
		elseif string.match(ability.Name, 'Charm') then
			gcinclude.fMoveToCurrent(sets.Charm,sets.CurrentGear);
			local sStave = gcinclude.fCheckForEleGear('staff','light');
			if sStave ~= nil then
				gcinclude.fSwapToStave(sStave,false,sets.CurrentGear);
			end
		elseif string.match(ability.Name, 'Tame') then
			gcinclude.fMoveToCurrent(sets.Tame,sets.CurrentGear);
		end
	-- /WAR
	elseif sj == 'WAR' then
		if string.match(ability.Name, 'Provoke') then
			gcinclude.fMoveToCurrent(sets.Provoke,sets.CurrentGear);
		elseif string.match(ability.Name, 'Berserk') then
			gcinclude.fMoveToCurrent(sets.Berserk,sets.CurrentGear);
		elseif string.match(ability.Name, 'Defender') then
			gcinclude.fMoveToCurrent(sets.Defender,sets.CurrentGear);
		elseif string.match(ability.Name, 'Warcry') then
			gcinclude.fMoveToCurrent(sets.Warcry,sets.CurrentGear);
		end
	--* /MNK *--
	elseif sj == 'MNK' then
		if string.match(ability.Name, 'Boost') then
			gcinclude.fMoveToCurrent(sets.Boost,sets.CurrentGear);
		elseif string.match(ability.Name, 'Focus') then
			gcinclude.fMoveToCurrent(sets.Focus,sets.CurrentGear);
		elseif string.match(ability.Name, 'Dodge') then
			gcinclude.fMoveToCurrent(sets.Dodge,sets.CurrentGear);
		elseif string.match(ability.Name, 'Chakra') then
			gcinclude.fMoveToCurrent(sets.Chakra,sets.CurrentGear);
		end
	-- /THF
	elseif sj == 'THF' then
		if string.match(ability.Name, 'Steal') then
			gcinclude.fMoveToCurrent(sets.Steal,sets.CurrentGear);
		elseif string.match(ability.Name, 'Sneak Attack') then
			gcinclude.fMoveToCurrent(sets.SneakAttack,sets.CurrentGear);
		elseif string.match(ability.Name, 'Flee') then
			gcinclude.fMoveToCurrent(sets.Flee,sets.CurrentGear);
		elseif string.match(ability.Name, 'Trick Attack') then
			gcinclude.fMoveToCurrent(sets.TrickAttack,sets.CurrentGear);
		elseif string.match(ability.Name, 'Mug') then
			gcinclude.fMoveToCurrent(sets.Mug,sets.CurrentGear);
		end
	-- /WHM
	elseif sj == 'WHM' then
		if string.match(ability.Name, 'Divine Seal') then
			gcinclude.fMoveToCurrent(sets.DivineSeal,sets.CurrentGear);	
		end
	-- /BLM
	elseif sj == 'BLM' then
		if string.match(ability.Name, 'Elemental Seal') then
			gcinclude.fMoveToCurrent(sets.ElementalSeal,sets.CurrentGear);
		end
	-- /DRK
	elseif sj == 'DRK' then
		if string.match(ability.Name, 'Arcane Circle') then
			gcinclude.fMoveToCurrent(sets.ArcaneCircle,sets.CurrentGear);	
		elseif string.match(ability.Name, 'Last Resort') then
			gcinclude.fMoveToCurrent(sets.LastResort,sets.CurrentGear);
		elseif string.match(ability.Name, 'Weapon Bash') then
			gcinclude.fMoveToCurrent(sets.WeaponBash,sets.CurrentGear);
		elseif string.match(ability.Name, 'Souleater') then
			gcinclude.fMoveToCurrent(sets.Souleater,sets.CurrentGear);
		end
	-- /RNG
	elseif sj == 'RNG' then
		if string.match(ability.Name, 'Sharpshot') then
			gcinclude.fMoveToCurrent(sets.Sharpshot,sets.CurrentGear);
		elseif string.match(ability.Name, 'Scavenge') then
			gcinclude.fMoveToCurrent(sets.Scavenge,sets.CurrentGear);
		elseif string.match(ability.Name, 'Camouflage') then
			gcinclude.fMoveToCurrent(sets.Camouflage,sets.CurrentGear);
		elseif string.match(ability.Name, 'Barrage') then
			gcinclude.fMoveToCurrent(sets.Barrage,sets.CurrentGear);	
		end
	-- /SAM
	elseif sj == 'SAM' then
		if string.match(ability.Name, 'Warding Circle') then
			gcinclude.fMoveToCurrent(sets.WardingCircle,sets.CurrentGear);			
		elseif string.match(ability.Name, 'Third Eye') then
			gcinclude.fMoveToCurrent(sets.ThirdEye,sets.CurrentGear);
		elseif string.match(ability.Name, 'Hasso') then
			gcinclude.fMoveToCurrent(sets.Hasso,sets.CurrentGear);
		elseif string.match(ability.Name, 'Meditate') then
			gcinclude.fMoveToCurrent(sets.Meditate,sets.CurrentGear);
		elseif string.match(ability.Name, 'Seigan') then
			gcinclude.fMoveToCurrent(sets.Seigan,sets.CurrentGear);
		end
	-- /DRG
	elseif sj == 'DRG' then
		if string.match(ability.Name, 'Ancient Circle') then
			gcinclude.fMoveToCurrent(sets.AncientCircle,sets.CurrentGear);			
		elseif string.match(ability.Name, 'Jump') then
			gcinclude.fMoveToCurrent(sets.Jump,sets.CurrentGear);
		elseif string.match(ability.Name, 'High Jump') then
			gcinclude.fMoveToCurrent(sets.HighJump,sets.CurrentGear);
		end
	-- /PLD
	elseif sj == 'PLD' then
		if string.match(ability.Name, 'Holy Circle') then
			gcinclude.fMoveToCurrent(sets.HolyCircle,sets.CurrentGear);
		elseif string.match(ability.Name, 'Shield Bash') then
			gcinclude.fMoveToCurrent(sets.ShieldBash,sets.CurrentGear);
		elseif string.match(ability.Name, 'Sentinel') then
			gcinclude.fMoveToCurrent(sets.Sentinel,sets.CurrentGear);	
		elseif string.match(ability.Name, 'Cover') then
			gcinclude.fMoveToCurrent(sets.Cover,sets.CurrentGear);	
		end
	end
	
	-- SMN abilities
	if string.find(ability.Name, 'Astral Flow') then
		gcinclude.fMoveToCurrent(sets.AstralFlow,sets.CurrentGear);
	elseif table.find(gcinclude.SmnSkill,ability.Name) ~= nil or
		   table.find(gcinclude.SmnMagical,ability.Name) ~= nil or
		   table.find(gcinclude.SmnAccuracy,ability.Name) ~= nil or
		   table.find(gcinclude.SmnHybrid,ability.Name) ~= nil or
		   string.find(gcinclude.SmnBPRageList,ability.Name) ~= nil then
		gcinclude.fMoveToCurrent(sets.BP,sets.CurrentGear);
	end
	gcinclude.fEquipTheGear(sets.CurrentGear);		-- Equip the composited HandleAbility set
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
		gcinclude.fMoveToCurrent(gcinclude.sets.Holy_Water,sets.CurrentGear);
		bShow = true;
	elseif string.match(item.Name, 'Silent Oil') then
		gcinclude.fMoveToCurrent(sets.Sneak,sets.CurrentGear);
		bShow = true;
	elseif string.match(item.Name, 'Prism Powder') then
		gcinclude.fMoveToCurrent(sets.Invisible,sets.CurrentGear);
		bShow = true;
	end
		
	if bShow == true then
		gcinclude.fEquipTheGear(sets.CurrentGear);
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
	
	-- Only process if /gswap is turned on
	if gcdisplay.GetToggle('GSwap') == false then
		return;
	end

	-- Clear out the CurrentGear in case of leftovers
	gcinclude.ClearSet(sets.CurrentGear);
	
	-- Equip the precast gear set
	gcinclude.fMoveToCurrent(sets.Precast,sets.CurrentGear);	
		
	-- See if an elemental obi should be equipped
	obi = gcinclude.fCheckForElementalGearByValue('obi','MEacc',spell.Name);
	if obi ~= nil then
		sets.CurrentGear['Waist'] = obi;
	end	
	gcinclude.fEquipTheGear(sets.CurrentGear);
end		-- HandlePrecast

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
	
	gcinclude.fEquipTheGear(sets.CurrentGear);		-- Equip the composited midcast set
end		-- gcinclude.HandleMidcast

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
		
	gcinclude.fMoveToCurrent(sets.Preshot,sets.CurrentGear);
	gcinclude.fEquipTheGear(sets.CurrentGear);
end		-- HandlePreshot

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
	
	gcinclude.fMoveToCurrent(sets.Midshot,sets.CurrentGear);
	
	-- Equip the composited Midshot set
	gcinclude.fEquipTheGear(sets.CurrentGear);
end		-- HandleMidshot

--[[
	HandleWeaponskill loads the gear appropriately for the weapon skill you're doing
--]]

profile.HandleWeaponskill = function()
	local ws = gData.GetAction();
	local canWS = gcinclude.CheckWsBailout();
	local cKey;

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
	gcinclude.HandleWeaponskill(false);
	
	-- Equip the composited weaponskill set
	gcinclude.fEquipTheGear(sets.CurrentGear);
end		-- HandleWeaponskill

return profile;