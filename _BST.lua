local profile = {};
gcinclude = gFunc.LoadFile('common\\gcinclude.lua');

--[[
	This file contains all the gear sets associated with the BST job.
	
	Gear Sets last updated: September 15, 2024
	Code update: September , 2024
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
	
	*** Note 2 ***
	No gear that supports bard songs can be worn by any job except a bard, so there's no explicit
	support given here for /BRD.
	
	Horizon changes from retail:
		- Moderate changes to AF gear
		- Significant changes to Gutter
		- Night flowers that need to be examined in "Save My Son" can now be examined as early as
		  16:00 and as late as 6:00
		- Job ability Ready available (this is out of era). Also, changes are gained every 45 
		  seconds as opposed to 60 seconds found in retail. Max charges are still 3.
		- Every physical and magic damage dealing TP moves are no longer imbued with
		  skillchain/magic burst properties
		- Pets will persist through player zoning
		- All HQ jug pets now cap at level 75
		- Pet food has been buffed to provide increased HP/stats to pets
--]]

--[[
	The TP sets are used when you and/or your pet are fighting,	The accuracy set will be applied
	in a fractional manner. The evasion set if equipped if /eva is specified.
--]]

	['TP'] = {
        Head  = { 'Shep. Bonnet//PET', 'Panther Mask', 'Monster Helm', 'Beast Helm +1', 'Shep. Bonnet', 'Empress Hairpin', 'Silver Hairpin//MSJ' },
        Neck  = { 'Opo-opo necklace//SLEPT', 'Peacock Amulet', 'Spike Necklace' },
		Ears  = { 'Bat Earring//BLIND', 'Ethereal Earring', 'Coral Earring//DT_MAGICAL', 'Beastly Earring', 'Brutal Earring', 'Genin Earring//SJNIN', 'Bat Earring//MSJ', 'Fang Earring', 'Energy Earring +1//MSJ', 'Energy Earring +1//MSJ', 'Reraise Earring' },
        Body  = { 'Gaudy Harness//MSJ//MP.LT.50', 'Narasimha\'s Vest', 'Scorpion Harness', 'Gaudy Harness', 'Wonder Kaftan', 'Mrc.Cpt. Doublet', 'Beetle Harness', 'Angler\'s Tunica' },
        Hands = { 'Thick Mufflers', 'Beast Gloves', 'Wonder Mitts', 'Battle Gloves', 'Ryl.Ftm. Gloves' },
        Rings = { 'Forager\'s Mantle', 'Flame Ring', 'Tamas Ring//MSJ', 'Sun Ring', 'Sun Ring', 'Courage Ring', 'Kshama Ring No.2', 'Balance Ring' },
        Back  = { 'Psilos Mantle', 'Raptor Mantle', 'Ram Mantle' },
        Waist = { 'Swift Belt', 'Tilt Belt', 'Warrior\'s Belt' },
        Legs  = { 'Thick Breeches', 'Monster Trousers', 'Beast Trousers', 'Shep. Hose', 'San. Trousers', 'Ryl.Ftm. Trousers' },
        Feet  = { 'Thick Sollerets', 'Monster Gaiters', 'Beast Gaiters', 'Wonder Clomps', 'Bounding Boots' },
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
		Ammo  = 'Orphic Egg//PJPBRD',
        Head  = { 'Bst. Helm +1//PETFNPF', 'Optical Hat', 'Shep. Bonnet//PETF' } ,
        Neck  = 'Peacock Amulet',
        Body  = { 'Scorpion Harness', 'Narasimha\'s Vest', 'Beast Jackcoat' },
        Hands = { 'Thick Mufflers', 'Battle Gloves' },
		Ears  = { 'Beastly Earring//PETF', 'Pilferer\'s Earring//SJTHF' },
		Rings = { 'Toreador\'s Ring', 'Woodsman Ring', 'Jaeger Ring', 'Kshama Ring No.2', 'Balance Ring' },
        Back  = 'Psilos Mantle',		
        Waist = { 'Life Belt', 'Monster Belt', 'Tilt Belt', 'Swift Belt' },
        Legs  = { 'Thick Breeches', 'Monster Trousers' },
        Feet  = 'Thick Sollerets',
    },	

--[[
	If evasion wanted, equip evasion gear. Remember that AGI converts to evasion: for every
	2 points of AGI you get 1 point of evasion
--]]

	['Evasion'] = {
		Ammo  = 'Orphic Egg//PJPBRD',
        Head  = { 'Optical Hat', 'Empress Hairpin' },
		Ears  = { 'Bat Earring//BLIND', 'Ethereal Earring', 'Reraise Earring' },
        Body  = { 'Scorpion Harness', 'Narasimha\'s Vest' },
		Hands = 'Battle Gloves',
        Legs  = { 'Shep. Hose//PETFNPF', 'San. Trousers' },
		Feet  = 'Bounding Boots',	-- default gear is thick sollerets which are -2 eva
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
		Head   = { 'Lilac Corsage//TOWN', 'Shep. Bonnet//PET', 'Panther Mask', 'Monster Helm', 'Beast Helm +1', 'Shep. Bonnet', 'Empress Hairpin', 'Silver Hairpin//MSJ' },
		Body   = { 'Ducal Aketon//TOWN-AK', 'Gaudy Harness//MSJ//MP.LT.50', 'Narasimha\'s Vest', 'Scorpion Harness', 'Gaudy Harness', 'Wonder Kaftan', 'Mrc.Cpt. Doublet', 'Beetle Harness', 'Angler\'s Tunica' },
	},
	
--[[
	When you are resting (kneeling down), your HP 'Resting' set will be equipped. 
	If your subjob uses MP and your MP is below the set threshhold (defined by 
	gcinclude.settings.RefreshGearMP), your MP 'Resting_Refresh' gear set will 
	be equipped. Regardless of which set is equipped, assuming that your subjob 
	uses magic, you have a Dark/Pluto staff accessible, weapon swapping is 
	enabled (/wswap), and your MP is not at maximum, the Dark/Pluto staff will 
	automatically be equipped.
--]]
	
	['Resting_Regen'] = { 
		Legs = 'Monster trousers',
	},

	['Resting_Refresh'] = {
		Body = 'Gaudy Harness//MSJ//MP.LT.50',	
	},
	
	-- If your subjob can use magic, then place any Spell Interruption Rate down 
	-- gear into the "SIR" gear set. This set is equipped in the gcinclude.HandleMidcast
	-- function that all spells go through.
	['SIR'] = {
	},
	
--[[
	Start weapons are where you define what you want the first row of equipment to look 
	like when you either log in as a BST or switch your main job to BST. Any other gear 
	you mention will be overridden by the Idle or Town set, so no need to include here.
--]]

	['Start_Weapons'] = {
        Main = { 'Maneater', 'Barbaroi Axe', 'Ryl.Arc. Sword' },	-- Sword added for low level option
        Sub  = { 'Tabarzin//SJNIN', 'Tatami Shield', 'War Pick//SJNIN' },
        Ammo = { 'Hedgehog Bomb//MSJ', 'S. Herbal Broth' },
    },
	
--[[
	Magic accuracy gear for either/or/both player and pet
--]]

	['Macc'] = {
		Rings = 'Tamas Ring//MSJ',
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
		Rings = { 'Woodsman Ring', 'Jaeger Ring', 'Beetle Ring +1', 'Beetle Ring +1' },	
        Back = 'Psilos Mantle',
    },

--[[
	Spells are a bit different. Each type of spell can have it's own enhancement gear as well as 
	stat based gear. (In some cases individual spells have special entries.) These sets do not 
	include elemental gear which is	dependent on day/weather/weapon skill.

	The first stage is Precast. This is where you place any Fast Cast, cast time reduction, and
	quick cast gear
--]]

	['Precast'] = {	
		Ears = { 'Loquac. Earring' },	
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
	-- include it here, assuming /wswap is enabled
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
        Head  = 'Beast helm +1',
		Body  = 'Monster Jackcoat',
        Rings = 'Tamas Ring',
        Feet  = 'Mannequin Pumps',
    },

	-- MND is used to determine the effectiveness of healing magic spells, 
	-- white magic enhancing spells and white magic enfeebling spells, by
	-- increasing the damage and accuracy. MND increases resistance to white
	-- magic spells as well as reducing the base damage taken from them. MND 
	-- is associated with the element water	
	['MND'] = {
        Head  = 'Beast helm +1',
		Neck  = { 'Promise Badge', 'Justice Badge' },
        Body  = 'Wonder Kaftan',
        Rings = { 'Tamas Ring', 'Kshama Ring No.9', 'Tranquility Ring' },
        Waist = 'Friar\'s Rope',
        Legs  = 'Wonder Braccae',
        Feet  = 'Mannequin Pumps',
	},
	
--[[
	And some spells are special cases, so they have individual gears sets.
--]]
	-- Stoneskin: Stoneskin Enhancement, Mind, and Enhancing Magic Skill. Mind is 3x more important than enhancing
	-- Magic Skill. The only piece of gear a BST can wear to enhance stoneskin is a Stone Gorget. There's no gear
	-- that a SMN (or any job) can wear to enhance magic. Note: This gear set has no effect on Titan's Earthen
	-- ward blood pact.
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
	
	BST can use the following weapons: axe (A-), scythe (B-), dagger (C+), club(D), sword (E). 
	
	Please note that on Horizon you may have access to some weapon skills
	through your subjob. While not explicitly supported here, the appropriate
	weapon skill set will be loaded. If not listed below, you might have to
	create a custom gear set to support the skill. Remember, weapon skill sets
	are named WS_attr. If you name the set appropriately, that set will auto-
	matically be called when you use the weapon skill.
--]]

--[[
		* Strength based *
		
		Axe: Raging Axe,Smash Axe,Gale Axe,Avalanche Axe,Spinning Axe,Rampage,
			 Mistral Axe,Decimation,Onslaught
		Scythe: Slice,Spinning Scythe,Vorpal Scythe
		Club: Brainshaker,Skullbreaker,True Strike
		Sword: Flat Blade,Circle Blade,Vorpal Blade
-]]
	
	['WS_STR'] = {
        Head  = 'Mrc.Cpt. Headgear',
        Neck  = 'Spike Necklace',
		Ears  = 'Beastly Earring//AXE',		-- Boosted skill if using an axe
        Body  = 'Narasimha\'s Vest',
        Hands = { 'Ogre Gloves', 'Wonder Mitts' },
        Rings = { 'Flame Ring', 'Sun Ring', 'Sun Ring', 'Courage Ring' },
        Back  = { 'Forager\'s Mantle', 'Amemet Mantle' },
		Waist = 'Warwolf Belt',
        Legs  = 'Wonder Braccae',
        Feet  = { 'Creek F Clomps', 'Wonder Clomps' },
	},
	
--[[
		* Strength and Dexterity based, even weighting *
		
		Sword: Fast Blade
--]]

	['WS_STRDEX'] = {
        Head  = 'Empress Hairpin',
        Neck  = 'Spike Necklace',
		Ears  = 'Beastly Earring//AXE',		-- Boosted skill if using an axe
        Body  = 'Narasimha\'s Vest',
        Hands = 'Wonder Mitts',
        Rings = { 'Flame Ring', 'Sun Ring', 'Sun Ring', 'Courage Ring' },
        Waist = 'Warwolf Belt',
		Back  = { 'Forager\'s Mantle', 'Amemet Mantle' },
        Legs  = { 'Ryl.Sqr. Breeches', 'Wonder Braccae' },
        Feet  = { 'Creek F Clomps', 'Bounding Boots' },
    },

--[[
		* Strength and Intelligence based, even weighting *
		
		Scythe: Dark Harvest,Shadow of Death,Nightmare Scythe,Spiral Hell
		Sword: Burning Blade
--]]
	
	['WS_STRINT'] = {
		Head  = { 'Bst. Helm +1', 'Mrc.Cpt. Headgear' },
		Neck  = 'Spike necklace',
		Ears  = 'Beastly Earring//AXE',		-- Boosted skill if using an axe
		Body  = { 'Monster Jackcoat', 'Narasimha\'s vest', 'Wonder Kaftan' },
		Hands = { 'Ogre gloves', 'Wonder Mitts' },
		Rings = { 'Flame Ring', 'Sun Ring', 'Sun Ring', 'Courage Ring' },
		Waist = 'Warwolf Belt',
		Back  = { 'Forager\'s Mantle', 'Amemet Mantle' },
		Legs  = 'Wonder braccae',
		Feet  = 'Wonder Clomps',
	},

--[[
		* Strength and Intelligence based, 30%/20% respectively *
		
		Sword: Red Lotus Blade
--]]
	
	['WS_STRINT_30_20'] = {
		Head  = { 'Bst. Helm +1', 'Mrc.Cpt. Headgear' },
		Neck  = 'Spike necklace',
		Ears  = 'Beastly Earring//AXE',		-- Boosted skill if using an axe
		Body  = { 'Monster Jackcoat', 'Narasimha\'s vest', 'Wonder Kaftan' },
		Hands = { 'Ogre gloves', 'Wonder Mitts' },
		Rings = { 'Flame Ring', 'Sun Ring', 'Sun Ring', 'Courage Ring' },
		Waist = 'Warwolf Belt',
		Back  = { 'Forager\'s Mantle', 'Amemet Mantle' },
		Legs  = 'Wonder braccae',
		Feet  = 'Wonder Clomps',
	},

--[[
		* Strength and Mind based, even weighting *
		
		Scythe: Guillotine,Cross Reaper
		Club: Shining Strike,Seraph Strike,Judgement
		Sword: Shining Blade,Seraph Blade
--]]

	['WS_STRMND'] = {
		Head  = { 'Bst. Helm +1', 'Mrc.Cpt. Headgear' },
		Neck  = { 'Promise Badge', 'Justice badge' },
		Ears  = 'Beastly Earring//AXE',		-- Boosted skill if using an axe
		Body  = { 'Narasimha\'s vest', 'Wonder Kaftan' },
		Hands = 'Ogre gloves',
		Rings = { 'Flame Ring', 'Tamas Ring', 'Sun ring', 'Sun ring', 'Kshama Ring No.9', 'Courage Ring' },
		Waist = { 'Warwolf Belt', 'Friar\'s Rope' },
		Back  = { 'Forager\'s Mantle', 'Amemet Mantle' },
		Legs  = 'Wonder braccae',
		Feet  = { 'Creek F Clomps', 'Wonder Clomps' },
	},

--[[
		* Strength and Vitality based, even weighting *
		
		Axe: Calamity (32%/32%)
--]]
	
	['WS_STRVIT'] = {
		Head  = 'Mrc.Cpt. Headgear',	
		Neck  = 'Spike necklace',
		Ears  = 'Beastly Earring//AXE',		-- Boosted skill if using an axe
		Body  = { 'Narasimha\'s vest', 'Beast Jackcoat', 'Wonder Kaftan' },
		Hands = 'Ogre gloves',
		Ring1 = { 'Flame Ring', 'Sun ring', 'Sun ring', 'Courage Ring' },
		Waist = { 'Warwolf Belt', 'Warrior\'s belt' },
		Back  = { 'Forager\'s Mantle', 'Amemet Mantle' },
		Legs  = { 'Wonder braccae', 'Ryl.Ftm. Trousers' },
		Feet  = { 'Creek F clomps', 'Wonder Clomps' },
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
        Body  = { 'Brigandine', 'Mrc.Cot. Doublet' },
        Hands = 'Beast Gloves',
        Rings = { 'Kshama Ring No.2', 'Balance Ring' },
		Waist = 'Warwolf Belt',
		Legs  = { 'Monster Trousers', 'Ryl.Sqr. Breeches' },
        Feet  = 'Bounding Boots',
    },

--[[
		* Dexterity and Charisma based *
		
		Dagger: Eviseration
--]]

	['WS_DEXCHR'] = {
    },

--[[
		* Dexterity and Intelligence based *
		
		Dagger: Gust Slash,Cyclone^

		^ Subjob must be one of: RDM,THF,BRD,RNG,NIN
--]]
	
	['WS_DEXINT'] = {
        Head  = { 'Beast Helm +1', 'Empress Hairpin' },
        Neck  = 'Spike Necklace',
		Ears  = 'Beastly Earring//AXE',		-- Boosted skill if using an axe
        Body  = { 'Monster Jackcoat', 'Brigandine', 'Mrc.Cpt. Doublet' },
        Hands = 'Beast Gloves',
        Rings = { 'Tamas Ring', 'Kshama Ring No.2', 'Balance Ring' },
		Waist = 'Warwolf Belt',
		Legs  = { 'Monster Trousers', 'Ryl.Sqr. Breeches' },
        Feet  = 'Bounding Boots',
    },

--[[
		* Charisma based *
		
		Dagger: Shadowstitch
--]]
	
	['WS_CHR'] = {
		Head = { 'Panther mask', 'Monster Helm', 'Entrancing Ribbon' },
		Neck = { 'Star Necklace', 'Flower Necklace' },
		Ears = 'Beastly earring',
		Body = 'Gaudy harness',
		Rings = { 'Moon ring', 'Moon ring' },
		Waist = { 'Monster Belt', 'Corsette' },
		Legs = 'Beast Trousers',
		Feet = 'Beast Gaiters',
	},

--[[
		* Mind based *
		
		Dagger: Energy Steal, Energy Drain^
		
		^ Subjob must be RDM,THF,BRD,RNG, or NIN
--]]

	['WS_MND'] = {
		Head  = 'Bst. Helm +1/ACCESSIBLE',
        Neck  = { 'Promise Badge', 'Justice Badge' },
		Ears  = 'Beastly Earring//AXE',		-- Boosted skill if using an axe
        Body  = 'Wonder Kaftan',
        Rings = { 'Tamas Ring', 'Kshama Ring No.9', 'Tranquility Ring' },
        Waist = 'Friar\'s Rope',
        Legs  = 'Wonder Braccae',
		Feet  = 'Mannequin Pumps',
    },

--[[
		* Skill based *
		
		Club: Starlight,Moonlight
--]]

	['WS_Skill'] = {
		Ears = 'Beastly Earring//AXE',
    },

--[[
		* HP based *
		
		Sword: Spirits Within
--]]

	['WS_HP'] = {
		Ammo  = 'Happy Egg',
        Head  = { 'Monster Helm', 'Bst. Helm +1', 'Shep. Bonnet' },
		Neck  = 'Promise Badge',
        Ears  = { 'Physical Earring', 'Ethereal Earring' },
        Body  = 'Wonder Kaftan',
        Hands = 'Wonder Mitts', 
        Rings = 'Toreador\'s Ring',	
        Waist = { 'Powerful Rope', 'Warrior\'s Belt' },
        Legs  = 'Wonder Braccae',
        Feet  = { 'Creek F Clomps', 'Wonder Clomps' },
    },

--[[
	Movement tends to be used for kiting. Emphasis should be placed on gear that increases movement speed, but you 
	might also want gear that has evasion. The choice is yours.
--]]

	-- Movement speed gear, does not include nation aketons which are found in conditional gear for home town
	['Movement'] = { 
	},
	
--[[
	The following sets are used with pet abilities/pet commands
--]]
	
	['Familiar'] = {
	},
	
	['CallBeast'] = {			-- or bestial loyalty, augmented call beast gear
		Hands = 'Monster Gloves',
	},
	
	['Gauge'] = {
	},
	
	-- Reward potency, reward augment, reward enhancement, and MND gear
	['Reward'] = {
		Head  = 'Bst. Helm +1',
        Neck = { 'Promise Badge', 'Justice Badge' },
        Body = { 'Monster Jackcoat//DB:WSS', 'Beast Jackcoat//DB:BPP', 'Monster Jackcoat','Beast Jackcoat' },
        Hands = 'Ogre Gloves',
        Rings = { 'Tamas Ring',	'Kshama Ring No.9', 'Tranquility Ring' },
        Waist = 'Friar\'s Rope',
        Legs = 'Wonder Braccae',
        Feet = { 'Monster Gaiters', 'Beast Gaiters', 'Mannequin Pumps' },
	},
	
	-- Tame success rate. Resistence depends on your INT vs target's INT
	['Tame'] = {
        Head = 'Beast Helm +1',
		Body = 'Monster Jackcoat',
		Rings = 'Tamas Ring',
		Feet = 'Mannequin Pumps',
    },
	
	-- CHR and Charm + gear. (Every +1 Charm adds 5% Charm duration)
	['Charm'] = {
        Head = { 'Monster Helm', 'Entrancing Ribbon' },
        Neck = 'Flower Necklace',
        Ears = 'Beastly Earring',
        Body = { 'Monster Jackcoat', 'Beast Jackcoat' },
        Hands = 'Beast Gloves',
        Rings = { 'Moon Ring', 'Moon Ring' },
        Waist = { 'Monster Belt', 'Corsette' },
        Legs = 'Beast Trousers',
        Feet = { 'Monster Gaiters', 'Beast Gaiters' },
    },
	
	['Pet_Attack'] = {					-- Pet's strength, not accuracy
	},

	['Pet_Macc'] = {					-- Pet's Magical Accuracy
		Head = { 'Beast Helm +1', 'Shep. Bonnet' },
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
-- list of all jug pets available on HorizonXI.
-- what,name,min level,max level,duration,have,favored
profile.JPI = T {['JUG'] = 1, ['MIN'] = 2, ['MAX'] = 3, ['DUR'] = 4, 
				 ['HAVE'] = 5, ['FAVE'] = 6};
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

-- There's no way to consistently identify the type of weapon you're currently
-- using by just looking at the name. (Ex: Maneater is an axe. The name does
-- not give that away.) The following table lists weapons by type that you're
-- likely to use. Add the weapon names accordingly. You only need the names of
-- the weapons if you want to conditionally equip an item with a weapon skill
-- attribute.
profile.WeaponType = {
	['AXE']    = { 'Maneater', 'Tabarzin', 'Barbaroi Axe', 'War Pick' },
	['SWORD']  = { 'Ifrit\'s Blade' },
	['DAGGER'] = { 'Garuda\'s Dagger' },
	['SCYTHE'] = { 'Suzaku\'s Scythe' },
	['CLUB']   = { 'Warp Cudgel' },
	['STAVE'] =  { 'Fire Staff', 'Vulcan\'s Staff', 'Ice Staff', 'Aquilo\'s Staff',
				  'Wind Staff', 'Auster\'s Staff', 'Earth Staff', 'Terra\'s Staff',
				  'Thunder Staff', 'Jupiter\'s Staff', 'Water Staff', 'Neptune\'s Staff',
				  'Light Staff', 'Apollo\'s Staff', 'Dark Staff', 'Pluto\'s Staff' },	
};

-- Accuracy Sets are predefined /acc commands. You identify them by a name and
-- a comma delimited list of slots. It's just a convenient shortcut mechanism.
profile.AccuracySet = {
	['base'] = 'Rings,Body',
};

profile.Sets = sets;
profile.sjb = nil;			-- Tracks subjob name
profile.bAmmo = false;		-- BST specific. Is ammo equipped?
profile.sAmmo = nil;		-- BST specific. Name of ammo equipped

--[[
	HandlePetAction equips the appropriate gear set based on the type of action
	the pet is trying to perform. Please note that only BST pets are supported,
	not SMN avatars.
--]]

local function HandlePetAction(PetAction)
	local pet = gData.GetPet();
	
	-- Only gear swap if this flag is true and the pet is a summoned pet
	if gcdisplay.GetToggle('GSwap') == false or 
	   table.find(gcinclude.tSummonSkill,pet.Name) ~= nil then
		return;
	end

	-- Only BST pet attacks have associated gear sets because /smn pets are at most half the
	-- level of your BST level
	if (gcinclude.BstPetAttack:contains(PetAction.Name)) then				-- Pet Attack
		gcinclude.fMoveToCurrent(sets.Pet_Attack,sets.CurrentGear);
	elseif (gcinclude.BstPetMagicAttack:contains(PetAction.Name)) then		-- Pet Magical Attack
		gcinclude.fMoveToCurrent(sets.Pet_Matt,sets.CurrentGear);		
	elseif (gcinclude.BstPetMagicAccuracy:contains(PetAction.Name)) then	-- Pet Magical Accuracy Attack
		gcinclude.fMoveToCurrent(sets.Pet_Macc,sets.CurrentGear);		
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
		['WAR'] = 0, ['MNK'] = 0, ['WHM'] = 1, ['BLM'] = 3, ['RDM'] = 0, ['THF'] = 2,
		['PLD'] = 0, ['DRK'] = 0, ['BST'] = 0, ['BRD'] = 0, ['RNG'] = 0, ['SMN'] = 0,
		['SAM'] = 0, ['NIN'] = 4, ['DRG'] = 0, ['BLU'] = 0, ['COR'] = 0, ['PUP'] = 0,
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
	
	-- Coded order of operation override
	gcinclude.settings.priorityEngaged = 'CEF';
	gcinclude.settings.priorityMidCast = 'ABCDEGHF';
	gcinclude.settings.priorityWeaponSkill = 'ADBE';	
	
	-- Set which macro book should be displayed. Which macro set within the macro book to
	-- display depends on what your subjob is.
	AshitaCore:GetChatManager():QueueCommand(1, '/macro book 10');	-- BST macro book
	SetSubjobSet(player.SubJob);
	
	-- Load up the weapons bar. (This need only be done once.)
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
	FindJugPets traverses the master list of jugs and determines if any of the accessible jugs
	make sense to equip based on the level range of the pet and the player's level. All that
	meet that criteria will have their appropriate indicators turned on. Returned is whether 
	any jug pets that match that criteria were found.
--]]

profile.FindJugPets = function()
	local inventory = AshitaCore:GetMemoryManager():GetInventory();
	local resources = AshitaCore:GetResourceManager();
	local player = gData.GetPlayer();
	local iCount = 0;
	
	-- Clear the table ownership settings
	for k,v in pairs(profile.JugPets) do
		v[profile.JPI['HAVE']] = false;
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
						if (tpf[profile.JPI['MIN']] <= player.MainJobSync) and 
							(tpf[profile.JPI['MAX']] >= player.MainJobSync) then
							-- finally, this one is possible to be selected
							profile.JugPets[kk][profile.JPI['HAVE']] = true;
							iCount = iCount + 1;
						end
					end
				end
			end
		end
	end
	
	-- assuming any were found, return true or false
	return (iCount > 0);
end		-- FindJugPets

--[[
	EquipMaxEquipableJugPet determines what is the best jug pet to load and equips it. The success is returned.
	The way this function works is by searching for available jug pets and determining which are level
	appropriate. Of those check if any are favored. If so, equip that one, else equip first one on list.
--]]

profile.EquipMaxEquipableJugPet = function(cs)
	local sBroth = nil;
	
	if cs == nil then
		cs = sets.CurrentGear;
	end

	-- find any equipable jug pets
	if profile.FindJugPets() == true then
		-- then cycle through the list and find the favored one. If none favored, the first jug found will be used.
		for k,v in pairs(profile.JugPets) do
			if v[profile.JPI['HAVE']] == true then		-- One of the found pets
				sBroth = k;
				if v[profile.JPI['FAVE']] == true then	-- Favored
					break;
				end
			end
		end
		if sBroth ~= nil then
			cs['Ammo'] = sBroth;
			return true;
		else
			print(chat.header('EquipMaxEquipableJugPet'):append(chat.message('Error: Found jug, but none equipped!')));
			return false;
		end
	else
		print(chat.header('EquipMaxEquipableJugPet'):append(chat.message('Error: No jug pets found to equip')));
		return false;
	end
end		-- EquipMaxEquipableJugPet

--[[
	HandleCommand is run when you type in a command defined in LUASHITACAST. The commands 
	handled here instead of in gcinclude.HandleCommands are specific to BST or the help 
	system.
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
end		-- HandleCommand

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
	
	-- A pet action takes priority over a player's action as long as it is a BST pet action.
	-- /SMN pet's actions are not supported.
	if pet ~= nil then
		local sLName = string.lower(pet.Name);
		if petAction ~= nil and (table.find(gcinclude.tSummonSkill,sLName) == nil) then
			HandlePetAction(petAction);
			return;
		end
	end
	
	profile.sPetAction = nil;
	
	-- Save the name of the main weapon	
	if ew['Main'] ~= nil then
		eWeap = ew['Main'].Name;
	end;
	
	-- Make sure the macro set is shown and that the display on the top of the screen is 
	-- correct in case the subjob was changed.	
	SetSubjobSet(player.SubJob);
	
	-- Only gear swap if this flag is true
	if gcdisplay.GetToggle('GSwap') == false then
		gcdisplay.Update();		-- in case something has changed
		return;
	end

	-- Clear out the CurrentGear in case of leftovers
	gcinclude.ClearSet(sets.CurrentGear);
	
	-- If a jug pet or pet food was dynamically equipped prior to this
	-- pass-through on the function, the following code will set the
	-- item back that was in the Ammo slot prior to it dynamically
	-- changing.
	if profile.bAmmo == true then
		sets.CurrentGear['Ammo'] = profile.sAmmo;
		profile.sAmmo = nil;
		profile.bAmmo = false;
	end
	
	-- If player is not resting and has swapped weapons, set the weapon
	-- back to what they had before the switch
	if player.Status ~= 'Resting' and 
			gcinclude.weapon ~= nil and 
			gcdisplay.GetToggle('WSwap') == true and 
			eWeap ~= gcinclude.weapon then
		if gcinclude.Locks[1][2] == false then
			sets.CurrentGear['Main'] = gcinclude.weapon;
		end
		if gcinclude.Locks[2][2] == false then
			sets.CurrentGear['Sub'] = gcinclude.weapon;
		end
	end

	-- Start with the default set
	gcinclude.fMoveToCurrent(sets.Default,sets.CurrentGear);
				
	-- Now process the player status accordingly
	if (player ~= nil and player.Status == 'Engaged') or (pet ~= nil and pet.Status == 'Engaged') then
		gcinclude.settings.priorityEngaged = string.upper(gcinclude.settings.priorityEngaged);
		for i = 1,string.len(gcinclude.settings.priorityEngaged),1 do
			cKey = string.sub(gcinclude.settings.priorityEngaged,i,i);
			if cKey == 'C' then		-- Evasion			
				if gcdisplay.GetToggle('Eva') == true then
					gcinclude.fMoveToCurrent(sets.Evasion,sets.CurrentGear);
				end
			elseif cKey == 'E' then		-- Accuracy	
				gcinclude.ffFractionalAccuracy(sets.Accuracy,nil);
			elseif cKey == 'F' then		-- Kiting
				if (gcdisplay.GetToggle('Kite') == true) then
					gcinclude.fMoveToCurrent(sets.Movement,sets.CurrentGear);
				end				
			end
		end
	elseif player.Status == 'Resting' then
		-- Player kneeling. Priority (low to high): regen,refresh
		if player.HP < player.MaxHP then
			gcinclude.fMoveToCurrent(sets.Resting_Regen,sets.CurrentGear);
		end

		if gcinclude.MagicalJob('S') == true and player.MP < player.MaxMP then
			gcinclude.fMoveToCurrent(sets.Resting_Refresh,sets.CurrentGear);
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
		gcinclude.fMoveToCurrent(sets.Default,sets.CurrentGear);
	end
			
	-- In case the pet is a summoned pet...
	if pet ~= nil and gcdisplay.GetToggle('WSwap') == true then
		local sStave = gcinclude.fCheckForElementalGearByValue('staff','Summons',pet.Name);
		if sStave ~= nil then
			gcinclude.fSwapToStave(sStave,false,sets.CurrentGear);
		end
	end
	
	-- And make sure a weapon equipped. (Going into a capped area can cause no 
	-- weapon to be equipped.)
	local gear = gData.GetEquipment();
	if gear.Main == nil then
		gcinclude.fMoveToCurrent(sets.Start_Weapons,sets.CurrentGear,true);
	end
	
	gcinclude.fEquipTheGear(sets.CurrentGear);		-- Equip the composited HandleDefault set
	
	-- Lastly, update the display, just in case
	gcdisplay.Update();
end		-- HandleDefault

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
end		-- bAmmoIsJug

--[[
	HandleAbility is used to change the player's gear appropriately for the specified pet ability.
--]]

profile.HandleAbility = function()
	local ability = gData.GetAction();
	local eq = gData.GetEquipment();
	
	-- Only gear swap if this flag is true
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

	-- Clear out the CurrentGear in case of leftovers
	gcinclude.ClearSet(sets.CurrentGear);
	
	-- Now process the appropriate job ability. Start with abilities associated with BST
	if string.match(ability.Name, 'Call Beast') or string.match(ability.Name, 'Bestial Loyalty') then
		-- First make sure player wants the automated jug pet funtionality
		if gcdisplay.GetToggle('AJug') == true then
			-- Ok, now see if a jug pet already equipped
			local bJugFound = profile.bAmmoIsJug(profile.sAmmo);
			if bJugFound == nil or (bJugFound ~= nil and bJugFound == false) then
				profile.bAmmo = profile.EquipMaxEquipableJugPet(sets.CurrentGear);
			end
		end
		gcinclude.fMoveToCurrent(sets.CallBeast,sets.CurrentGear);
	elseif string.match(ability.Name, 'Familiar') then
		gcinclude.fMoveToCurrent(sets.Familiar,sets.CurrentGear);
	elseif string.match(ability.Name, 'Gauge') then
		gcinclude.fMoveToCurrent(sets.Gauge,sets.CurrentGear);
	elseif string.match(ability.Name, 'Reward') then
		-- Pet reward. Make sure that pet food already equipped
		if profile.sAmmo == nil or string.find(string.lower(profile.sAmmo),'pet f') == nil then		-- something else equipped
			profile.bAmmo = gcinclude.doPetFood('max',nil);
		end
		gcinclude.fMoveToCurrent(sets.Reward,sets.CurrentGear);
	elseif string.match(ability.Name, 'Tame') then
		-- Trying to tame a beast. (Someone's charm failed.)
		gcinclude.fMoveToCurrent(sets.Tame,sets.CurrentGear);		
	elseif string.match(ability.Name, 'Charm') then
		-- Trying to charm a beast. 
		gcinclude.fMoveToCurrent(sets.Charm,sets.CurrentGear);
		local sStave = gcinclude.CheckForEleGear('staff','light');
		if sStave ~= nil then
			gcinclude.fSwapToStave(sStave,false,sets.CurrentGear);
		end
	-- And now the subjob abilities
	-- /WAR
	elseif string.match(ability.Name, 'Provoke') then
		gcinclude.fMoveToCurrent(sets.Provoke,sets.CurrentGear);
	elseif string.match(ability.Name, 'Berserk') then
		gcinclude.fMoveToCurrent(sets.Berserk,sets.CurrentGear);
	elseif string.match(ability.Name, 'Defender') then
		gcinclude.fMoveToCurrent(sets.Defender,sets.CurrentGear);
	elseif string.match(ability.Name, 'Warcry') then
		gcinclude.fMoveToCurrent(sets.Warcry,sets.CurrentGear);
	--* /MNK *--
	elseif string.match(ability.Name, 'Boost') then
		gcinclude.fMoveToCurrent(sets.Boost,sets.CurrentGear);
	elseif string.match(ability.Name, 'Focus') then
		gcinclude.fMoveToCurrent(sets.Focus,sets.CurrentGear);
	elseif string.match(ability.Name, 'Dodge') then
		gcinclude.fMoveToCurrent(sets.Dodge,sets.CurrentGear);
	elseif string.match(ability.Name, 'Chakra') then
		gcinclude.fMoveToCurrent(sets.Chakra,sets.CurrentGear);
	-- /THF
	elseif string.match(ability.Name, 'Steal') then
		gcinclude.fMoveToCurrent(sets.Steal,sets.CurrentGear);
	elseif string.match(ability.Name, 'Sneak Attack') then
		gcinclude.fMoveToCurrent(sets.SneakAttack,sets.CurrentGear);
	elseif string.match(ability.Name, 'Flee') then
		gcinclude.fMoveToCurrent(sets.Flee,sets.CurrentGear);
	elseif string.match(ability.Name, 'Trick Attack') then
		gcinclude.fMoveToCurrent(sets.TrickAttack,sets.CurrentGear);
	elseif string.match(ability.Name, 'Mug') then
		gcinclude.fMoveToCurrent(sets.Mug,sets.CurrentGear);
	-- /WHM
	elseif string.match(ability.Name, 'Divine Seal') then
		gcinclude.fMoveToCurrent(sets.DivineSeal,sets.CurrentGear);
	-- /BLM
	elseif string.match(ability.Name, 'Elemental Seal') then
		gcinclude.fMoveToCurrent(sets.ElementalSeal,sets.CurrentGear);
	-- /DRK
	elseif string.match(ability.Name, 'Arcane Circle') then
		gcinclude.fMoveToCurrent(sets.ArcaneCircle,sets.CurrentGear);
	elseif string.match(ability.Name, 'Last Resort') then
		gcinclude.fMoveToCurrent(sets.LastResort,sets.CurrentGear);
	elseif string.match(ability.Name, 'Weapon Bash') then
		gcinclude.fMoveToCurrent(sets.WeaponBash,sets.CurrentGear);
	elseif string.match(ability.Name, 'Souleater') then
		gcinclude.fMoveToCurrent(sets.Souleater,sets.CurrentGear);	
	-- /RNG
	elseif string.match(ability.Name, 'Sharpshot') then
		gcinclude.fMoveToCurrent(sets.Sharpshot,sets.CurrentGear);
	elseif string.match(ability.Name, 'Scavenge') then
		gcinclude.fMoveToCurrent(sets.Scavenge,sets.CurrentGear);
	elseif string.match(ability.Name, 'Camouflage') then
		gcinclude.fMoveToCurrent(sets.Camouflage,sets.CurrentGear);
	elseif string.match(ability.Name, 'Barrage') then
		gcinclude.fMoveToCurrent(sets.Barrage,sets.CurrentGear);	
	-- /SAM
	elseif string.match(ability.Name, 'Warding Circle') then
		gcinclude.fMoveToCurrent(sets.WardingCircle,sets.CurrentGear);
	elseif string.match(ability.Name, 'Third Eye') then
		gcinclude.fMoveToCurrent(sets.Third_Eye,sets.CurrentGear);
	elseif string.match(ability.Name, 'Hasso') then
		gcinclude.fMoveToCurrent(sets.Hasso,sets.CurrentGear);
	elseif string.match(ability.Name, 'Meditate') then
		gcinclude.fMoveToCurrent(sets.Meditate,sets.CurrentGear);
	elseif string.match(ability.Name, 'Seigan') then
		gcinclude.fMoveToCurrent(sets.Seigan,sets.CurrentGear);
	-- /DRG
	elseif string.match(ability.Name, 'Ancient Circle') then
		gcinclude.fMoveToCurrent(sets.AncientCircle,sets.CurrentGear);
	elseif string.match(ability.Name, 'Jump') then
		gcinclude.fMoveToCurrent(sets.Jump,sets.CurrentGear);
	elseif string.match(ability.Name, 'High Jump') then
		gcinclude.fMoveToCurrent(sets.HighJump,sets.CurrentGear);		
	-- /PLD
	elseif string.match(ability.Name, 'Holy Circle') then
		gcinclude.fMoveToCurrent(sets.HolyCircle,sets.CurrentGear);
	elseif string.match(ability.Name, 'Shield Bash') then
		gcinclude.fMoveToCurrent(sets.ShieldBash,sets.CurrentGear);
	elseif string.match(ability.Name, 'Sentinel') then
		gcinclude.fMoveToCurrent(sets.Sentinel,sets.CurrentGear);	
	elseif string.match(ability.Name, 'Cover') then
		gcinclude.fMoveToCurrent(sets.Cover,sets.CurrentGear);
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
	obi = gcinclude.CheckEleSpells(spell.Name,gcinclude.MagicEleAcc,gcinclude.OBI,nil);
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
	gcinclude.HandleWeaponskill(false);
	
	-- Equip the composited weaponskill set		
	gcinclude.fEquipTheGear(sets.CurrentGear);
end		-- HandleWeaponskill

return profile;