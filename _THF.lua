local profile = {};
gcinclude = gFunc.LoadFile('common\\gcinclude.lua');

--[[
	This file contains all the gear sets associated with the THF job.
	
	Gear Sets last updated: October 21, 2024
	Code update: October 28, 2024
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
	exist by Luashitacast. THF supports evasion "tanking", so you'll find some sets 
	with an associated "Tank_" set (ex. TP and Tank_TP). Minally include a subset entry in 
	the Tank_ set so that some gear is definied until you create a tanking set.
	
	Example:
	
	['Tank_TP'] = {
		Subset = 'TP',
	}
		
	*** Note ***
	/SMN has a problem in that their pet is the level of the subjob, which is not very useful. 
	As a result, /SMN pet actions are treated "as is" without gearset swap support.	As for /DRG,
	please note that the wyvern can't be summoned.
	
	*** Note 2 ***
	No gear that supports bard songs can be worn by any job except a bard, so there's no explicit
	support given here for /BRD.
	
		
	Horizon changes from retail:
		- Minor changes to artifact armor
		- Major changes to Mandau
		- Treasure Hunter has been removed from Thieve's Knife and moved to the custom neck piece
		  Nanaa's Charm
		- The player's accuracy and ranged accuracy when using "perfect dodge" are increased by 999
		- Many elemental weapon skills have been changed to the hybrid type and can now stack with
		  "sneak attack"
		- Triple attack now unlocks at 30 instead of 55
		- Assassin now unlocks at 50 instead of 60
		- "Accomplice" (out of era) can now be used on an alliance, has had it's level reduced from 
		  65 to 45 and no longer shares a recast timer with "Collaborator"
		- "Collaborator" (out of era) can now be used on an alliance, has had it's level reduced
		  from 65 to 45 and it's recast time increased from 1 minute to 5 minutes. It no longer shares
		  a recast timer with "Accomplice" and it now redirects 50% of the thief's enmity to their
		  chosen target. Note that the chat log erroneously notes you stole the enmity.
		- "Bully" (our of era) has had it's level reduced from 93 to 40, it's recast been lowered from
		  3 to 1 minute. The ability has been completely reworked now giving a guarenteed status bolt
		  additional effect proc on monsters within 15 levels of the thief. The chance of proc'ing
		  a status effect on a notorious monster has been increased, but is not guarenteed. The
		  omni-directional sneak attack portion of the skill has been removed
		- "Mug" grants a critical hit rate bonus of 5% on the target for 30 seconds. Recast was lowered
		  from 15 to 10 minutes. This activates as a bewildered daze regardless of the success of the
		  mug. It wears off after 30 seconds
--]]

--[[
	The "default" gear set is what is worn when you're not fighting (either you or your pet)
	and you're not resting. It covers everything else: idling, traveling, in town, etc. This 
	set displays what your character looks like most of the	time. This set does not 
	distinguish the type of activities you're doing by default, so use inlines accordingly.
--]]
		
	['Default'] = {
		Head  = { 'Lilac Corsage//TOWN', 'Panther Mask', 'Rogue\'s Bonnet', 'Empress Hairpin' },
		Neck  = { 'Opo-opo necklace//SLEPT', 'Peacock Amulet', 'Spike Necklace' },
		Ears  = { 'Bat Earring//BLINDED', 'Coral Earring//DT_MAGICAL', 'Brutal Earring', 'Coral Earring', 'Fang Earring', 'Reraise Earring', 'Physical Earring', 'Energy Earring +1//MSJ', 'Energy Earring +1//MSJ' },
		Body  = { 'Ducal Aketon//TOWN-AK', 'Rapparee Harness', 'Brigandine', 'Mrc.Cpt. Doublet', 'Beetle Harness', 'Angler\'s Tunica' },
	    Hands = { 'Rogue\'s Armlets', 'Battle Gloves' },
        Rings = { 'Kshama Ring No.2', 'Balance Ring', 'Courage Ring' },
        Back  = { 'Forager\'s Mantle', 'Amemet Mantle', 'Raptor Mantle', 'Ram Mantle' },
        Waist = { 'Swift Belt', 'Mrc.Cpt. Belt' },
        Legs  = { 'Bravo\'s Subligar', 'Wonder Braccae', 'San. Trousers', 'Ryl.Ftm. Trousers' },
        Feet  = { 'Assassin\'s Pouln.', 'Creek F Clomps', 'Bounding Boots' },
	},
	
--[[
	The TP sets are used when you are fighting.	The accuracy set will be applied in a fractional
	manner and the evasion set if /eva is specified. When evasion tanking, Tank_TP will be equipped 
	instead of the TP set. It's a means for	the THF to equip more defensive gear if they find
	themselves tanking. In Tank_TP, consider including shield skill+ gear.
--]]

	['TP'] = {
        Head  = { 'Panther Mask', 'Rogue\'s Bonnet', 'Empress Hairpin' },
        Neck  = { 'Opo-opo necklace//SLEPT', 'Peacock Amulet', 'Spike Necklace' },
        Ears  = { 'Bat Earring//BLINDED', 'Coral Earring//DT_MAGICAL', 'Brutal Earring', 'Coral Earring', 'Fang Earring', 'Reraise Earring', 'Physical Earring', 'Energy Earring +1//MSJ', 'Energy Earring +1//MSJ' },
        Body  = { 'Rapparee Harness', 'Brigandine', 'Mrc.Cpt. Doublet', 'Beetle Harness', 'Angler\'s Tunica' },
        Hands = { 'Rogue\'s Armlets', 'Battle Gloves' },
        Rings = { 'Kshama Ring No.2', 'Balance Ring', 'Courage Ring' },
        Back  = { 'Forager\'s Mantle', 'Amemet Mantle', 'Raptor Mantle', 'Ram Mantle' },
        Waist = { 'Swift Belt', 'Mrc.Cpt. Belt' },
        Legs  = { 'Bravo\'s Subligar', 'Wonder Braccae', 'San. Trousers', 'Ryl.Ftm. Trousers' },
        Feet  = { 'Assassin\'s Pouln.', 'Creek F Clomps', 'Bounding Boots' },
    },

	['Tank_TP'] = {
		Subset = 'TP',
	},
	
--[[
	If an accuracy emphasis is desired, the following set will replace the gear, appropriately.
	Unlike TP though, accuracy is applied one slot at a time in a fractionalized manner using
	the /acc command.
	
	Include equipment with accuracy bonus and DEX. Remember, DEX converts to accuracy: (horizon) 
	for every 1 point of DEX you get 0.70 points of accuracy if wielding a 2H weapon, 0.65 for 
	a 1H weapon, and 0.60 for H2H. 
	
	Also, like the TP set, since THF can evasion tank, there's a Tank_Accuracy set.
--]]
	
	['Accuracy'] = {
        Head  = { 'Optical Hat', 'Empress Hairpin' },
        Neck  = { 'Peacock Amulet', 'Spike Necklace' },
        Body  = { 'Scorpion Harness', 'Brigandine', 'Mrc.Cpt. Doublet' },
        Hands = 'Battle Gloves',
        Rings = { 'Toreador\'s Ring', 'Woodsman Ring', 'Woodsman Ring', 'Jaeger Ring', 'Kshama Ring No.2', 'Balance Ring' },
        Waist = { 'Life Belt', 'Tilt Belt', 'Mrc.Cpt. Belt' },
	    Feet  = 'Bounding Boots',
    },

	['Tank_Accuracy'] = {
        Head  = 'Empress Hairpin',
        Neck  = { 'Peacock Amulet', 'Spike Necklace' },
        Body  = { 'Brigandine', 'Mrc.Cpt. Doublet' },
        Hands = 'Battle Gloves',
        Rings = { 'Woodsman Ring', 'Jaeger Ring', 'Kshama Ring No.2', 'Balance Ring' },
        Waist = { 'Life Belt', 'Tilt Belt', 'Mrc.Cpt. Belt' },
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
        Head  = { 'Optical Hat', 'Empress Hairpin' },
        Ears  = { 'Reraise Earring', 'Genin Earring//SJNIN', 'Drone Earring' },
        Body  = { 'Scorpion Harness', 'Mrc.Cpt. Doublet' },
        Hands = 'Battle Gloves',
        Feet  = 'Bounding Boots',
    },

	['Tank_Evasion'] = {
        Head  = 'Empress Hairpin',
        Ears  = { 'Reraise Earring', 'Genin Earring//SJNIN', 'Drone Earring' },
        Body  = 'Mrc.Cpt. Doublet',
        Hands = 'Battle Gloves',
        Waist = 'Mrc.Cpt. Belt',
        Legs  = 'San. Trousers',
        Feet  = 'Bounding Boots',	
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
	
	['Resting_Refresh'] = {
		Main = 'Pluto\'s Staff//WSWAP//MSJ',	
	},
	
	-- If your subjob can use magic, then place any Spell Interruption Rate down 
	-- gear into the "SIR" gear set. This set is equipped in the gcinclude.HandleMidcast
	-- function that all spells go through.
	['SIR'] = {
	},
	
--[[
	Start weapons are where you define what you want the first row of equipment to look 
	like when you either log in as a THF or switch your main job to THF. Any other gear 
	you mention will be overridden by the Idle or Town set, so no need to include here.
--]]

	['Start_Weapons'] = {
		Main  = { 'X\'s knife','Thief\'s knife' },
		Sub   = { 'Corsair\'s Knife//SJNIN', 'Tatami Shield' },
		Range = 'Thug\'s Zamburak',
		Ammo  = 'Acid Bolt',
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
		Neck  = 'Peacock Amulet',
		Body  = 'Rapparee Harness',
		Rings = { 'Woodsman Ring', 'Woodsman Ring', 'Jaeger Ring', 'Beetle Ring +1', 'Beetle Ring +1' },
		Back  = 'Psilos Mantle',
		Legs  = 'Bravo\'s Subligar',
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
	The second stage is Midcast. This is where you equip gear that gives 
	magic attack, enhancing bonuses, potency improvements, duration
	enhancements, recast reduction gear, etc. This implementation breaks 
	out the midcast into separate routines based on the magic type: 
	healing, divine, elemental, enhancing, enfeebling, summoning, ninjutsu,
	and song. Within each routine there's further logic breaking down
	functional paths. Based on spell (or song), the appropriate gear set
	is equipped.
	
	Every gear set will have an alternative "tanking" version if your
	main job supports tanking (like THF does.) I've included details on
	what each gear set is suppose to feature and what stats you should
	be emphasizing. Further, where appropriate, any formulas/charts that
	will help you to decide what gear to include. 
--]]	

	['Midcast'] = {
	},

--[[
	Healing Magic: consisting of all light-based spells, removes 
	some debuffs on players, buffs the caster, cures the health of 
	players or npcs, or causing damage to undead monsters. Healing 
	magic skill affects the	potency of cures while decreasing the 
	likelihood of the caster being interrupted.
	
	Healing spells: cures, curagas, raises, reraises, blindna, cursna,
	paralyna, poisona, silena, stona, and viruna.
--]]

--[[
	These two sets are used for all non-cure Healing Magic spells. 
	Only healing magic skill is of any importance here. You might 
	want to use these sets as subsets for subsequent cure-based sets.
--]]
	['HealingMagic'] = {
	},
	
	['Tank_HealingMagic'] = {
	},
	
--[[	
	Curing magic addresses healing players/npcs. Each time a cure 
	spell is cast, a power calculation is performed to determine 
	the base effect of the spell. After that, any bonuses will be 
	applied. What this means is that MND, VIT and healing magic 
	skill impact your power rating, but once the cap is hit, they 
	have no more influence.
	
		power = (MND*3) + VIT + (Healing Magic Skill*0.6)
	
	This chart lists all RDM curing spells, the power cap, and the 
	effect on HP baseline. The curaga spells are included in case
	you subbed WHM.
	
			Spell		cap		low		high
			-----		---		---		---
			Cure		100		 20		 30
			Cure II		170		 75		 90
			Cure III	300		160		190
			Cure IV		460		330		390
			Curaga		170		 75		 90
			Curaga II	300		160		190
	
	At this point, any bonuses from day's element, weather, elemental
	staff, or gear/food with potency affects will be tacked on. Because
	cure potency is applied after the power cap is determined, it's a
	very attractive parameter to boost up. Just remember though that 
	cure potency is capped at 50%. Light's day and/or light weather 
	has a 33% chance to boost the cure's effecacy by 10% each (25% 
	if double light weather.) Casting cures on darksday or in dark 
	weather has an equal chance of a penalty.
	
	Once the "CuringMagic" set is equipped, the midcast routine will
	also check to see if you have an Apollo/Light staff for it's Cure 
	Potency.
--]]	
	
	['CuringMagic'] = {
		Neck  = 'Promise Badge',
		Ears  = 'Geist Earring',
		Body  = 'Wonder Kaftan',
		Rings = 'Tamas Ring',
		Back  = 'White Cape',
		Legs  = 'Wonder Braccae',
		Feet  = 'Mannequin Pumps',
	},
	
	['Tank_CuringMagic'] = {
	},

--[[
	As for the offensive use of cure spells against undead monsters, 
	most of	what was said about CuringMagic is true except cure potency. 
	This has no effect on undead monsters.
	
	After the OffensiveCuring set is equipped, the midcast routine will 
	see if a Korin Obi can be equipped to take advantage of the 100% 
	proc rate of the day's element/weather. Also, like normal curing 
	magic, an Apollo/Light staff will be check for,	but not for the 
	cure potency. Rather, for magic affinity.
--]]

	['OffensiveCuring'] = {
	},
	
	['Tank_OffensiveCuring'] = {
	},

--[[
	Enhancing Magic: This type of magic includes a wide variety of spells 
	that enhances players as well as movement spells. It's sort of a catch 
	all category.
	
	Enhancing Spells: bar/ra elemental spells, bar/ra status spells, blink,
	aquaveil, stoneskin, phalanx, protect/ra spells, shell/ra spells, erase,
	regen, refresh, deoderize, invisible, sneak, haste, spike spells (not
	dread), escape, teleport spells, warp spells, en- spells (except 
	enlight.)
--]]

--[[
	These two sets are the generic equipment sets used to cover spells not 
	defined in subsequent gear sets. Enhancing magic skill determines potency 
	(if appropriate) and decreases the likelihood of an enhancing spell 
	being interrupted. Enhancing magic is not affected by magic affinity, 
	so elemental staves are not needed, but en- spells can be affected by 
	the day/weather effects.
--]]

	['EnhancingMagic'] = {
	},
	
	['Tank_EnhancingMagic'] = {
	},
	
--[[
	There are two versions of barspells: elemental and status, both of which
	increase the magic evasion of a player from the element/status named.
	Only one barspell of each type can be enabled at the same time. Pairing 
	barspells that have the same element increases magic evasion further 
	(ex: barparalyze and barblizzard).
	
	The potency of an elemental barspell depends only on Enhancing Magic
	Skill as follows:
	
		Resistance = 40 + floor(Enhancing Magic Skill / 5)
		if Enhancing Magic Skill is above 300:
			Resistance = 25 + floor(Enhancing Magic Skill / 4)
	
	You can increase the resistence through gear or WHM category 1 merits.
	* There's a cap of 500 Enhancing Magic Skill, but that might be from the
	  99 era.
	
	Little is known about the potency of barstatus spells, but enhancing
	magic skill does affect the duration.
	
		duration(seconds) = Enhancing Magic Skill x 2
--]]

	['Barspell'] = {
	},
	
	['Tank_Barspell'] = {
	},
	
--[[
	Enspells buff the player's melee weapon so that when they hit, there's
	also elemental damage being applied. The amount of damage is calculated 
	when the weapon hits.
	
		Base Damage = floor(6 * E / 100) + 3 if E <= 200
					  floor(5 * E / 100) + 5 if E > 200
			where E is your Enhancing Magic Skill
		
	When WotG comes out, Enspells II are introduced which are fairly amazing.
	I'll update this comment when that occurs.
	
	Enspells are also affected by corresponding day's element and weather,
	but not magic affinity. The appropriate obi will automatically be equipped
	if the conditions are met.
--]]

	['Enspell'] = {
	},
	
	['Tank_Enspell'] = {
	},

--[[
	Spikes, place an elemental buff around the player which causes damage/
	status effect to any monster that hits the player. Each type of spike 
	spell has a different formula for how much damage they do and only 
	some potentially add a status effect. All spikes all are based on INT 
	and Enhancing Magic Skill.
--]]
	
	['Spike'] = {
	},
	
	['Tank_Spike'] = {
	},

--[[
	The rest of the the gear sets for Enhancing Magic are for specific
	spells: stoneskin, sneak, invisible, and phalanx. Include gear in
	the appropriate set that enhances the named spell accordingly.

	Stoneskin absorbs a set amount of damage before wearing off. How much
	it absorbs depends on the caster's MND and Enhancing Magic Skill.
	
		Base = floor(Enhancing Magic Skill/3 + MND)
		
		if Base < 80, then absorbed amount equals base
		if 80 <= Base < 130, then absorbed amount = floor((2 * Base) - 60)
		if 130 <= Base, then absorbed amount = floor((3 * Base) - 190)
		
		Absorbed damage is capped at 350
		
	Any equipment that enhances stoneskin grants a flat bonus to the spell's
	effect that can go past the natural cap. This equipment must be worn by
	the player who has stoneskin cast on them. (At this time only the caster
	can be the reciever.)
--]]

	['Stoneskin'] = {
	},
	
	['Tank_Stoneskin'] = {
	},

--[[
	Sneak's duration is variable, but the duration maxes at about 5 
	minutes. Include any gear that enhances this buff.
--]]

	['Sneak'] = {
		Feet = 'Dream Boots +1',
	},
	
	['Tank_Sneak'] = {
	},

--[[
	Invisible's duration is variable, but the duration maxes at 
	about 5 minutes. Include any gear that enhances this buff.
--]]	

	['Invisible'] = {
		Hands = 'Dream Mittens +1',
	},
	
	['Tank_Invisible'] = {
	},

--[[
	Phalanx gives the reciever a certain amount of damage resistance.
	The amount of resistance is calculated after every hit taken and is 
	a function of the caster's Enhancing Magic Skill at the time the 
	spell was cast.
	
		resistance = floor ((E / 10) - 2) if E <= 300
					 floor(((E - 300.5) / 28.5) + 28) if E > 300
			where E is the caster's Enhancing Magic Skill
		
		The resistance caps at 35.
--]]
	
	['Phalanx'] = {
	},	

	['Tank_Phalanx'] = {
	},

--[[
	Elemental Magic: This type of magic consists of nukes, ancient magic (a type
	of nuke), and elemental debuffs. Elemental Magic Skill determines the accuracy
	and help resist spell interuptions. It has no effect on damage except for the
	meteor skill which is out of era. All elemental spells are consider to be
	either a nuke or debuff.
	
	Elemental spells: aeros, aerogas, blizzards, blizzagas, burn, burst, drown
	fires, firagas, flare, flood, quake, rasp, sjhock, stones, stonegas, thunders,
	thundagas, tornado, waters, and watergas.
	
	Elemental magic and ancient magic is grouped together. CaLculating magic
	damage is an extensive process. (If you want to see all of the gory details,
	please refer to the Elemental Magic section in the magic.txt file found in
	the Documentation subdirectory.)
	
	The important things to remember are: the difference between the caster's 
	INT and the monster's INT (dINT) scales the	wider the gap becomes. The 
	nuke spell's tier caps the dINT that is counted, so a tier 1 caps at 100,
	tier 2 at 200, etc. Magic affinity and day/weather can boost/penalize 
	damge. Hitting a single target does more damage (even with an AoE spell)
	than two or more targets. Boosting Magic Attack Bonus (MAB) will increase
	damage. Hitting NMs with the same spell within 5 seconds again will 
	reduce your elemental damage by 20% ("nuke wall"), excluding skillchains.	
--]]
	['ElementalNuke'] = {
		Rings = 'Tamas Earring',
		Feet  = 'Mannequin Pumps',
	},
	
	['Tank_ElementalNuke'] = {
	},

--[[
	Elemental debuffs work in a simlar fashion to elemental nukes except they
	apply a damge over time (DOT) debuff and lessen a primary stat. How effective
	the elemental debuff is depends strictly on the caster's INT.
	
		  1 -  39 INT: 1 hp/tic and  -5 to the stat
		 40 -  69 INT: 2 hp/tic and  -7 to the stat
		 70 -  99 INT: 3 hp/tic and  -9 to the stat
		100 - 149 INT: 4 hp/tic and -11 to the stat
		>150 INT:	   5 hp/tic and -13 to the stat
	
	A target can be afflicted by as many as three different elemental debuffs
	as long as the spells' element doesn't interact with each other. So,
	rasp (earth, Dex down), Drown (water, STR down), and Frost (ice, AGI down)
	can coexist and Burn (fire, INT down), Choke (wind, VIT down), and Shock
	(lightning, MND down) can coexist. (Note that the damage done by an
	elemental debuff can wake up a player/monster that is sleeping.) Elemental
	Magic Skill, Magic Affinity, and Magic Accuracy increase the likelihood of
	the debuff not being resisted.
	
	An elemental obi and elemental staff (with //WSWAP) will be equipped if 
	available automatically.
--]]

	['ElementalDebuff'] = {
	},
	
	['Tank_ElementalDebuff'] = {
	},
	
--[[
	Summoning: This type of magic is used when a summoner casts either an
	avatar or an elemental spirit. It is a very straightforward type of
	magic. Summoning Magic Skill mostly affect elemental spirits, decreasing
	the wait time between when the spirit is summoned and it casts a spell
	and the wait time between spells. Further, the intelligence of the AI
	increases. The spirit will cast more powerful spells and more appropriate
	spells more often. Summoning magic skill also descreases the likelihood
	of a summons being interrupted.
--]]

	['Summoning'] = {
	},
	
	['Tank_Summoning'] = {
	},
	
--[[
	Dark Magic: This type of magic is used to absorb from a target, whether
	stats, mana, or HP. Further, it can weaken an enemy's attack while applying
	a DoT debuff, stun, and move a k.o.'ed player. Dark Magic Skill determines
	accuracy, potency of some spells (not absorbs), and descreases the 
	likelihood of the caster being interrupted.
	
	Dark Magic Spells: absorb accuracy, absorb AGI, absorb CHR, absorb DEX, 
	absorb INT, absorb MND, absorb STR, absorb TP, absorb VIT, aspir, bios,
	drain, stun and tractor.
--]]
	
	['DarkMagic'] = {
	},
	
	['Tank_DarkMagic'] = {
	},

--[[
	There's 9 absorb spells (although some are currently out of era). If not
	resisted, they drain a specific stat from the target based on the caster's
	level:
	
		base absorbed = floor (3 + (job level) / 5)
	
	Dark magic skill has no effect on absorb spell, but do affect accuracy.
	Absorb spells resisted will have their duration cut in half or be completely
	resisted. Equipment that "Enhances" an absorb spell will increase the spells
	duration. Equipment that "Augments" an absorb spell will increase the spells
	potency.
--]]
	
	['Absorb'] = {
	},
	
	['Tank_Absorb'] = {
	},

--[[
	Drain steals HP from the target and absorbs it into the caster's HP pool.
	Base potency depends strictly on the caster's Dark Magic Skill:
	
		0 - 299 skill: floor((dark magic skill / 3) + 20)
		>= 300 skill: floor(dark magic skill * 0.9)
	
	The minimum potency is 50% of the maximum potency and the actual potency
	of the spell (when unresisted) will randomly fall between the minimum and
	the maximum.  All enhancements from gear, weather, and magic burst bonuses
	are applied after the base potency is determined. Drain is not affected 
	by magic attack bonus and magic crit+ hit on gear.
--]]

	['Drain'] = {
	},
	
	['Tank_Drain'] = {
	},

--[[
	Aspir steals MP from the target and absorbs it into the caster's MP pool
	(assuminging the target has any MP.) Base potency depends strictly on the
	caster's Dark Magic Skill:
	
		0 - 300 skill: floor(skill / 3) + 20
		>= 300 skill: floor(skill * 0.4)

	The minimum potency is 50% of the maximum potency and the actual potency
	of the spell (when unresisted) will randomly fall between the minimum and
	the maximum. All enhancements from gear, weather, and magic burst bonuses
	are applied after the base potency is determined. Aspir is not affected 
	by magic attack bonus and magic crit+ hit on gear.	
--]]

	['Aspir'] = {
	},
	
	['Tank_Aspir'] = {
	},

--[[
	Currently Dread Spikes are out of era, but they're introduced in ToAU,
	so I've included them here. At the moment the code only applies a generic
	spell invocation.
--]]
	
--	['Dread'] = {
--	},
	
--	['Tank_Dread'] = {
--	},

--[[
	Divine Magic: damages or debilitates opponents with light elemental
	spells. It is especially effective against undead monsters, especially
	spells like banish whose properties are enhanced against undead.
	Divine Magic Skill determines accuracy and resuces spell interruption
	by the caster. It does not affect damage at all, except for enlight
	which sets a starting damage point.
	
	Divine Magic: banishes, banishga, enlight, flash and holy.
	
	Offensive Divine spells (banish, banishga, holy, and enlight) groups 
	spells that either just do damage or successive damage as in the case 
	of enlight.
	
	The banish spells accuracy, besides from divine magic spell, can be
	affected by magic accuracy from equipment. Damage resist rates depend
	on the difference in MND between caster and target. Banish does 50%
	more damage to undead.
	
	An elemental obi will be checked for as well as an elemental staff.
--]]

	['OffensiveDivine'] = {
	},
	
	['Tank_OffensiveDivine'] = {
	},

--[[
	Enfeebling divine spell (flash) afflicts the target with accuracy 
	reduction (similar to blind) with a weakening effect over time till
	it runs out. Duration is subject to resists and partial resists
	although can last 12 seconds if not resisted. It also generates a
	significant amount of volitile and cumulative enmity.
--]]	
	
	['EnfeebleDivine'] = {
	},
	
	['Tank_EnfeebleDivine'] = {
	},

--[[
	Enlight is the only enhancing divine spell. It  enhances the paladin's 
	weapon with light starting at a fixed point based on their divine 
	magic skill. Each hit the value will go down 1 until 0 is hit. Multihit 
	weapons work with enlight. Enlight also provides +10 enmity. The base 
	damage starts at:
	
		Divine magic skill < 150: (divine magic skill /  30) + 10
		Divine magic skill >= 150: (divine magic skill / 15) + 5
--]]
		
	['EnhanceDivine'] = {
	},
	
	['Tank_EnhanceDivine'] = {
	},
	
--[[
	Enfeebling Magic: TBD
--]]
	
	['EnfeeblingINT'] = {
	},
	
	['Tank_EnfeeblingINT'] = {
	},
	
	['EnfeeblingMND'] = {
	    Neck  = { 'Promise Badge', 'Justice Badge' },
        Rings = { 'Tamas Ring', 'Tranquility Ring' },
        Waist = 'Friar\'s Rope',
	},
	
	['Tank_EnfeeblingMND'] = {
	},
	
--[[
	Singing: TBD
--]]

--[[
	Ninjutsu:
--]]
	['NinjutsuBuff'] = {
	},
	
	['Tank_NinjutsuBuff'] = {
	},
	
	['NinjutsuDebuff'] = {
	},
	
	['Tank_NinjutsuDebuff'] = {
	},
	
	['NinjutsuElemental'] = {
	},
	
	['Tank_NinjutsuElemental'] = {
	},

--[[
	Blue Magic: Until the release of Treasures of Aht Urghan is close to a 
	release, there's no point in fleshing this out, especially since this job
	is being majorly altered.
--]]

--[[
	Geomancy Magic: Until the release of Seekers of Adoulin is close to a 
	reality, there's no point in fleshing this out.
--]]

-- old midcast sets included during the transition	

	['Healing'] = {
    },
	
	['Dark'] = {
    },

	['Divine'] = {
	},

	['Enfeebling'] = {
	},

	['Enhancing'] = {
	},

	['Elemental'] = {
	},

	['Ninjutsu'] = {
	},

-- stat based gear sets are no longer supported beyond as a subset. They
-- need to be integrated into the appropriate sets.
	['INT'] = {
		Head  = 'Rogue\'s Bonnet',
        Rings = 'Tamas Ring',
        Waist = 'Mrc.Cpt. Belt',
        Feet  = 'Mannequin Pumps',	
    },

	['Tank_INT'] = {
		Subset = 'INT',
	},

	['MND'] = {
        Neck = { 'Promise Badge', 'Justice Badge' },
        Body = 'Wonder Kaftan',
        Rings = { 'Tamas Ring', 'Kshama Ring No.9', 'Tranquility Ring' },
        Waist = 'Mrc.Cpt. Belt',
        Legs = 'Wonder Braccae',
        Feet = 'Mannequin Pumps',
	},
	
	['Tank_MND'] = {
		Subset = 'MND',
	},
	
--[[
	The following weapon skill gearsets are defined by the stat they emphasize. Listed are all of the sets that
	you will need to use every weapon skill that your job can do. The leading comment defines what weapon/weapon
	skill combination the set applies to.
	
	THF can use the following weapons: Dagger (A-), Sword (D), Club (E), H2H (E), Marksmanship (C+), Archery (C-)
		
	Please note that on Horizon you may have access to some weapon skills
	through your subjob. While not explicitly supported here, the appropriate
	weapon skill set will be loaded. If not listed below, you might have to
	create a custom gear set to support the skill. Remember, weapon skill sets
	are named WS_attr. If you name the set appropriately, that set will auto-
	matically be called when you use the weapon skill.
--]]

--[[
		* Strength based *
		
		Sword: Flat Blade,Circle Blade,Vorpal Blade,Spirits Within,Mercy Stroke
		Club: Starlight,Skull Breaker,True Strike
		H2H: Spinning Attack
-]]
	
	['WS_STR'] = {
        Head  = 'Mrc.Cpt. Headgear',
        Neck  = 'Spike Necklace',
        Body  = { 'Narasimha\'s Vest', 'Wonder Kaftan' },
        Hands = 'Wonder Mitts',
        Rings = { 'Flame Ring', 'Sun Ring', 'Sun Ring', 'Courage Ring' },
        Back  = { 'Forager\'s Mantle', 'Amemet Mantle' },
		Waist = { 'Warwolf Belt', 'Mrc.Cpt. Belt' },
        Legs  = 'Wonder Braccae',
        Feet  = { 'Creek F clomps', 'Wonder Clomps' },
    },

--[[
		* Strength and Agility based, ranged *
		
		Archery: Flaming Arrow^,Piercing Arrow^,Dulling Arrow^,Sidewinder^
		
		^ Subjob must be RNG
--]]

	['WS_RANGED_STRAGI'] = {
        Head = 'Empress Hairpin',
        Neck = 'Spike Necklace',
        Ears = { 'Genin Earring//SJNIN', 'Drone Earring' },
        Body = { 'Assassin\'s Vest', 'Wonder Kaftan' },
        Hands = 'Wonder Mitts',
        Rings = { 'Flame Ring', 'Sun Ring', 'Sun Ring', 'Courage Ring' },
        Back  = { 'Forager\'s Mantle', 'Amemet Mantle' },
		Waist = { 'Warwolf Belt', 'Mrc.Cpt. Belt' },
        Legs = 'Wonder Braccae',
        Feet = { 'Creek F clomps', 'Bounding Boots' },
    },
	
--[[
		* Strength and Dexterity based, even weighting *
		
		Sword: Fast Blade
		H2H: Combo,Backhand Blow,Raging Fist^
		
		^ Subjob must be MNK
--]]

	['WS_STRDEX'] = {
        Head  = { 'Assassin\'s Bonnet', 'Empress Hairpin' },
        Neck  = 'Spike Necklace',
        Body  = 'Brigandine',
        Hands = { 'Rogue Armlets', 'Wonder Mitts' },
        Rings = { 'Flame Ring', 'Kshama Ring No.2', 'Sun Ring', 'Sun Ring', 'Balance Ring', 'Courage Ring' },
        Back  = { 'Forager\'s Mantle', 'Amemet Mantle' },
		Waist = { 'Warwolf Belt', 'Mrc.Cpt. Belt' },
        Legs  = { 'Wonder Braccae' },
        Feet  = { 'Creek F clomps', 'Bounding Boots' },
    },

--[[
		* Strength and Intelligence based, even weighting *
		
		Sword: Burning Blade,Red Lotus Blade
--]]
	
	['WS_STRINT'] = {
        Head  = 'Rogue\'s Bonnet',
        Neck  = 'Spike Necklace',
        Body  = { 'Narasimha\'s Vest', 'Wonder Kaftan' },
        Hands = 'Wonder Mitts',
        Rings = { 'Tamas Ring', 'Flame Ring', 'Sun Ring', 'Sun Ring', 'Courage Ring' },
        Back  = { 'Forager\'s Mantle', 'Amemet Mantle' },
		Waist = { 'Warwolf Belt', 'Mrc.Cpt. Belt' },
        Legs  = 'Wonder Braccae',
        Feet  = 'Wonder Clomps',
    },

--[[
		* Strength and Mind based, even weighting *
		
		Sword: Shining Blade,Seraph Blade
		Club: Shining Strike
--]]

	['WS_STRMND'] = {
        Head  = 'Mrc.Cpt. Headgear',
        Neck  = { 'Promise Badge', 'Justice Badge' },
		Ears  = 'Geist Earring',
        Body  = { 'Narasimha\'s Vest', 'Wonder Kaftan' },
        Hands = 'Wonder Mitts',
        Rings = { 'Tamas Ring', 'Flame Ring', 'Kshama Ring No.9', 'Sun Ring', 'Sun Ring', 'Courage Ring', 'Tranquility Ring' },
        Back  = { 'Forager\'s Mantle', 'Amemet Mantle' },
		Waist = { 'Warwolf Belt', 'Mrc.Cpt. Belt' },
        Legs  = 'Wonder Braccae',
        Feet  = { 'Creek F clomps', 'Wonder Clomps' },
    },

--[[
		* Agility based, ranged *
		
		Marksmanship: Hot Shot^,Split Shot^,Sniper Shot^,Slug Shot^
		
		^ Subjob must be RNG
--]]

	['WS_RANGED_AGI'] = {
        Head  = 'Empress Hairpin',
        Ears  = { 'Genin Earring//SJNIN', 'Drone Earring' },
        Body  = { 'Assassin\'s Vest', 'Mrc.Cpt. Doublet' },
        Back  = 'Assassin\'s Cape',
		Waist = 'Mrc.Cpt. Belt',
		Legs  = 'Rogue\'s Culottes',
        Feet  = 'Bounding Boots',
    },
	
--[[
		* Charisma based *
		
		Dagger: Shadowstitch
--]]
	
	['WS_CHR'] = {
        Head  = { 'Panther Mask', 'Entrancing Ribbon' },
        Neck  = 'Flower Necklace',
		Ears  = 'Beastly Earrings',
        Waist = 'Corsette',
		Feet  = 'Assassin\'s Poulaines',
    },

--[[
		* Dexterity based *
		
		Dagger: Wasp Sting,Viper Bite,Dancing Edge,Eviseration
--]]
	
	['WS_DEX'] = {
        Head  = { 'Assassin\'s Bonnet', 'Empress Hairpin' },
        Neck  = 'Spike Necklace',
        Body  = 'Brigandine',
		Hands = 'Rogue Armlets',
        Rings = { 'Kshama Ring No.2', 'Balance Ring' },
        Back  = 'Assassin\'s Cape',
		Waist = { 'Warwolf Belt', 'Mrc.Cpt. Belt' },
        Feet  = 'Bounding Boots',
    },

--[[
		* Dexterity and Agility based *
		
		Dagger: Shark Bite
--]]
	
	['WS_DEXAGI'] = {
        Head  = { 'Assassin\'s Bonnet', 'Empress Hairpin' },
        Neck  = 'Spike Necklace',
        Ears  = { 'Genin Earring//SJNIN', 'Drone Earring' },
        Body  = { 'Assassin\'s Vest', 'Brigandine' },
		Hands = 'Rogue Armlets',
        Rings = { 'Kshama Ring No.2', 'Balance Ring' },
        Back  = 'Assassin\'s Cape',
		Waist = { 'Warwolf Belt', 'Mrc.Cpt. Belt' },
        Feet  = 'Bounding Boots',
    },
	
--[[
		* Dexterity and Charisma based *
		
		Dagger: Eviseration+
		
		+ Horizon modified
--]]
	
	['WS_DEXCHR'] = {
        Head  = { 'Assassin\'s Bonnet', 'Empress Hairpin' },
        Neck  = 'Spike Necklace',
		Ears  = 'Beastly Earring',
        Body  = { 'Brigandine', 'Mrc.Cpt. Doublet' },
		Hands = 'Rogue Armlets',
        Rings = { 'Kshama Ring No.2', 'Balance Ring' },
        Back  = 'Assassin\'s Cape',
		Waist = 'Corsette',
        Feet  = 'Bounding Boots',
    },
	
--[[
		* Dexterity and Intelligence based *
		
		Dagger: Gust Slash,Cyclone
--]]
	
	['WS_DEXINT'] = {
        Head  = { 'Assassin\'s Bonnet', 'Rogue\'s Bonnet', 'Empress Hairpin' },
        Neck  = 'Spike Necklace',
        Body  = 'Brigandine',
		Hands = 'Rogue Armlets',
        Rings = { 'Tamas Ring', 'Kshama Ring No.2', 'Balance Ring' },
        Back  = 'Assassin\'s Cape',
		Waist = 'Mrc.Cpt. Belt',
        Feet  = 'Bounding Boots',
    },

--[[
		* Mind based *
		
		Dagger: Energy Steal,Energy Drain
--]]

	['WS_MND'] = {
        Neck  = { 'Promise Badge', 'Justice Badge' },
        Body  = 'Brigandine',
        Rings = { 'Tamas Ring', 'Kshama Ring No.9', 'Tranquility Ring' },
        Waist = 'Mrc.Cpt. Belt',
        Legs  = 'Wonder Braccae',
        Feet  = 'Mannequin Pumps',
    },
	
--[[
		* Vitality based *
		
		H2H: Shoulder Tackle,One Inch Punch^
		
		^ Subjob must be MNK
--]]

	['WS_VIT'] = {
        Body  = { 'Narasimha\'s Vest', 'Brigandine' },
        Waist = { 'Warwolf Belt', 'Warrior\'s Belt' },
        Legs  = { 'Wonder Braccae', 'San. Trousers', 'Ryl.Ftm. Trousers' },
    },

	['WS_Skill'] = {
    },

--[[
		* HP based *
		
		Sword: Spirits Within
--]]

	['WS_HP'] = {
		Head  = { 'Assassin\'s Bonnet', 'Rogue\'s Bonnet' },
        Neck  = 'Promise Badge',
		Ears  = { 'Physical Earring', 'Ethereal Earring' },
        Body  = { 'Assassin\'s Vest', 'Rogue\'s Vest', 'Wonder Kaftan' },
        Hands = { 'Wonder Mitts', 'Rogue\'s Armlets' },
		Rings = 'Toreador\'s Ring',
        Waist = 'Powerful Rope',
        Legs  = 'Wonder Braccae',
		Feet  = { 'Creek F Clomps', 'Wonder Clomps', 'Creek F Clomps' }, 
    },
	
--[[
	Kite is used for kiting. Emphasis should be placed on gear that increases 
	movement speed, but you might also want gear that has evasion. The choice
	is yours.
--]]

	['Kite'] = { 
	},

--[[
	The following are abilities affected by gear  ** this far **
--]]
	['Perfect Dodge'] = {
	},
	
	['Steal'] = {
		Head  = 'Rogue\'s Bonnet',
		Hands = 'Rogue\'s Armlets',
		Legs  = 'Rogue\'s Culottes',
		Feet  = 'Rogue\'s Poulaines',
	},
	
	['SneakAttack'] = {
        Head  = { 'Assassin\'s Bonnet', 'Empress Hairpin' },
        Neck  = 'Spike Necklace',
        Body  = { 'Brigandine', 'Mrc.Cpt. Doublet' },
		Hands = 'Rogue\'s Armlets',
        Rings = { 'Kshama Ring No.2', 'Balance Ring' },
        Back  = 'Assassin\'s Cape',
		Waist = 'Mrc.Cpt. Belt',
        Feet  = 'Bounding Boots',	
	},
	
	['Flee'] = {
		Feet = 'Rogue\'s Poulaines',
	},

	['TrickAttack'] = {
        Head = 'Empress Hairpin',
        Ears = { 'Genin Earring//SJNIN', 'Drone Earring' },
        Body = { 'Assassin\'s Vest', 'Mrc.Cpt. Doublet' },
        Back  = 'Assassin\'s Cape',
		Waist = 'Mrc.Cpt. Belt',
        Feet = 'Bounding Boots',	
	},
	
	['Mug'] = {
		Head = 'Assassin\'s Bonnet', 
	},
	
	['Bully'] = {
	},
	
	['Hide'] = {
	},
	
	['Accomplice'] = {
	},
	
	['Collaborator'] = {
	},
	
--[[
	While not an ability, there's times you want to toggle Treasure Hunter on
--]]
	
	['TH'] = {
		Neck = 'Nanaa\'s Charm',
		Hands = 'Assassin\'s Armlets',
	},
	
--[[
	Some subjobs really make no sense when combined with dragoon, but all abilities across all jobs that
	have gear that can be equipped by a PLD are included here.
--]]
	--* BST *--
	-- CHR and Charm + gear. (Every +1 Charm adds 5% Charm duration)
	['Charm'] = {
	    Head = 'Entrancing Ribbon',
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
	
	--* /DRK *--
	['ArcaneCircle'] = {
	},
	
	['Last_Resort'] = {
	},
	
	['WeaponBash'] = {
	},
	
	['Souleater'] = {
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
	['DAGGER'] = { 'Thief\'s Knife', 'Corsair\'s Knife', 'Hornetneedle', 'Bone Knife', 'Marauder\'s Knife' },
	['CLUB']   = { 'Warp Cudgel' },
	['STAVE'] =  { 'Fire Staff', 'Vulcan\'s Staff', 'Ice Staff', 'Aquilo\'s Staff',
				  'Wind Staff', 'Auster\'s Staff', 'Earth Staff', 'Terra\'s Staff',
				  'Thunder Staff', 'Jupiter\'s Staff', 'Water Staff', 'Neptune\'s Staff',
				  'Light Staff', 'Apollo\'s Staff', 'Dark Staff', 'Pluto\'s Staff' },	
};

-- Accuracy Sets are predefined /acc commands. You identify them by a name and
-- a comma delimited list of slots. It's just a convenient shortcut mechanism.
profile.AccuracySet = {
	['b0'] = 'Rings,Hands',
	['b1'] = 'Rings,Hands,Body',
	['b2'] = 'Rings,Hands,Body,Waist',
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
		['WAR'] = 0, ['MNK'] = 0, ['WHM'] = 0, ['BLM'] = 0, ['RDM'] = 0, ['THF'] = 0,
		['PLD'] = 0, ['DRK'] = 0, ['BST'] = 2, ['BRD'] = 0, ['RNG'] = 0, ['SMN'] = 0,
		['SAM'] = 0, ['NIN'] = 1, ['DRG'] = 0, ['BLU'] = 0, ['COR'] = 0, ['PUP'] = 0,
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

function profile.OnLoad()
	local player = gData.GetPlayer();

	gSettings.AllowAddSet = true;
	gcinclude.Initialize();
	
	-- Coded order of operation override
	gcinclude.settings.priorityEngaged = 'CEF';
	gcinclude.settings.priorityWeaponSkill = 'ADBE';
	gcdisplay.SetToggle('Tank',false);		-- Assume THF is not a tank
	
	-- Set your job macro toolbar defaults here. Which set depends on the subjob
	AshitaCore:GetChatManager():QueueCommand(1, '/macro book 11');		-- THF
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
	of in gcinclude.HandleCommands are specific to BST or the help system, which has been tailored to BST.
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
	local player = gData.GetPlayer();
	local pet = gData.GetPet();
	local petAction = gData.GetPetAction();	
	local ew = gData.GetEquipment();
	local zone = gData.GetEnvironment();
	local bTank = gcdisplay.GetToggle('Tank');
	local eWeap = nil;
	local cKey;

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
			gcdisplay.GetToggle('WSwap') == true and
			gcinclude.weapon ~= nil and 
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
	if player.Status == 'Engaged' then
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
		
		if gcdisplay.GetToggle('TH') == true then
			gcinclude.MoveToCurrent(sets.TH,sets.CurrentGear);
		end			
	elseif player.Status == 'Resting' then	
		-- Player kneeling. Priority (low to high): Resting,refresh	
		if player.HP < player.MaxHP then
			gcinclude.MoveToCurrent(sets.Resting_Regen,sets.CurrentGear);
		end
		
		if gcinclude.fMagicalSubJob() == true and player.MP < player.MaxMP then	
			gcinclude.MoveToCurrent(sets.Resting_Refresh,sets.CurrentGear);
			if gcdisplay.GetToggle('WSwap') == true then
				local sStave = gcinclude.CheckForEleGear('staff','dark');
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
	
	-- Now process the appropriate job ability. Start with abilities associated with THF
	if string.match(ability.Name, 'Perfect Dodge') then
		gcinclude.MoveToCurrent(sets.PerfectDodge,sets.CurrentGear);
	elseif string.match(ability.Name, 'Steal') then
		gcinclude.MoveToCurrent(sets.Steal,sets.CurrentGear);
	elseif string.match(ability.Name,'Sneak Attack') then
		gcinclude.MoveToCurrent(sets.SneakAttack,sets.CurrentGear);
	elseif string.match(ability.Name, 'Flee') then
		gcinclude.MoveToCurrent(sets.Flee,sets.CurrentGear);
	elseif string.match(ability.Name,'Trick Attack') then
		gcinclude.MoveToCurrent(sets.TrickAttack,sets.CurrentGear);
	elseif string.match(ability.Name,'Mug') then
		gcinclude.MoveToCurrent(sets.Mug,sets.CurrentGear);
	elseif string.match(ability.Name,'Bully') then
		gcinclude.MoveToCurrent(sets.Bully,sets.CurrentGear);
	elseif string.match(ability.Name,'Hide') then
		gcinclude.MoveToCurrent(sets.Hide,sets.CurrentGear);
	elseif string.match(ability.Name,'Accomplice') then
		gcinclude.MoveToCurrent(sets.Accomplice,sets.CurrentGear);
	elseif string.match(ability.Name,'Collaborator') then
		gcinclude.MoveToCurrent(sets.Collaborator,sets.CurrentGear);		
		
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
	--* /MNK *--
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
	HandlePrecast loads Fast Cast, cast time reduction, and quick cast gear in anticipation of a spell
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

function profile.HandleMidcast()
	local bTank = gcdisplay.GetToggle('Tank');
	
	if gcdisplay.GetToggle('GSwap') == false then		-- Only gear swap if this flag is true	
		return;
	end

	-- Clear out the CurrentGear in case of leftovers
	gcinclude.ClearSet(sets.CurrentGear);
	
	-- Call the common HandleMidcast now
	-- Note: uncomment the line after this comment block and comment out the 
	-- next one if you want the old midcast routine to be called. If you
	-- want the new midcast, the first line should be commented out and the
	-- second line enabled.
	--gcinclude.HandleMidcast();
	gcinclude.fHandleMidcast();

	if bTank then
		gcinclude.settings.priorityMidCast = 'ACBDEGHF';
	else
		gcinclude.settings.priorityMidCast = 'ABCDEGHF';	
	end	
	
	gcinclude.EquipTheGear(sets.CurrentGear);		-- Equip the composited midcast set
end		-- HandleMidcast

--[[
	HandlePreshot is similar to HandlePrecast, but for ranged actions. It loads Ranged Accuracy 
	and Ranged Shot Speed Gear for a ranged attack
--]]

function profile.HandlePreshot()
	if gcdisplay.GetToggle('GSwap') == false then
		return;
	end
	
	-- Clear out the CurrentGear in case of leftovers
	gcinclude.ClearSet(sets.CurrentGear);
		
	gcinclude.MoveToCurrent(sets.Preshot,sets.CurrentGear);
	gcinclude.EquipTheGear(sets.CurrentGear);
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
				
	if gcdisplay.GetToggle('TH') == true then
		gcinclude.MoveToCurrent(sets.TH,sets.CurrentGear);
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