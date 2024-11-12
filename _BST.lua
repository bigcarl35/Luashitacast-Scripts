local profile = {};
gcinclude = gFunc.LoadFile('common\\gcinclude.lua');

--[[
	This file contains all the gear sets associated with the BST job.
	
	Gear Sets last updated: November 12, 2024
	Code update: November 12, 2024
--]]

local sets = {
--[[
	The gear sets are self contained, a mixture of direct gear assignments and conditional
	assignments. Each set contains entries identified by the gear slot. If it's a single
	value, it's a direct assignment like: Body = 'Austere Robe', but there can be multiple
	items identified, usually ordered by level: Body = { 'Vermillion Cloak//CARBY','Austere Robe' },
	Any item that has a // appended to it contains an inline conditional. The // code is a test
	to see if the item should be equipped. The level is still checked, but if the inline code
	test is successful, that piece of gear will be loaded. If you've done a /gc command,
	the item's suitability for the job and accessibility will also be checked.
		
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
	The "default" gear set is what is worn when you're not fighting (either you or your pet)
	and you're not resting. It covers everything else: idling, traveling, in town, etc. This 
	set displays what your character looks like most of the	time. This set does not 
	distinguish the type of activities you're doing by default, so use inlines accordingly.
--]]

	['Default'] = {
		Head   = { 'Lilac Corsage//TOWN', 'Panther Mask', 'Empress Hairpin' },
		Neck   = { 'Opo-opo necklace//SLEPT', 'Peacock Amulet', 'Spike Necklace' },
		Body   = { 'Ducal Aketon//TOWN-AK', 'Gaudy Harness//MSJ//MP.LT.50', 'Narasimha\'s Vest', 'Ducal Aketon' },
		Hands  = { 'Thick Mufflers', 'Battle Gloves' },
        Rings  = { 'Flame Ring', 'Tamas Ring//MSJ', 'Sun Ring' },
        Back   = { 'Forager\'s Mantle', 'Ram Mantle' },
        Waist  = { 'Swift Belt', 'Warrior\'s Belt' },
        Legs   = { 'Thick Breeches', 'Ryl.Ftm. Trousers' },
        Feet   = { 'Thick Sollerets', 'Bounding Boots' },
	},

	['Default_WPet'] = {
		Subset = 'Default',
	},
	
--[[
	The TP set is used when you and/or your pet are fighting. The accuracy set will be applied
	in a fractional manner. The evasion set if equipped if /eva is specified.
--]]

	['TP'] = {
        Head   = { 'Shep. Bonnet//PET', 'Panther Mask', 'Monster Helm', 'Beast Helm +1', 'Shep. Bonnet', 'Empress Hairpin', 'Silver Hairpin +1//MSJ' },
        Neck   = { 'Opo-opo necklace//SLEPT', 'Peacock Amulet', 'Spike Necklace' },
		Ears   = { 'Bat Earring//BLINDED', 'Ethereal Earring', 'Coral Earring//DT_MAGICAL', 'Beastly Earring', 'Brutal Earring', 'Genin Earring//SJNIN', 'Bat Earring//MSJ', 'Fang Earring', 'Energy Earring +1//MSJ', 'Energy Earring +1//MSJ', 'Reraise Earring' },
        Body   = { 'Gaudy Harness//MSJ//MP.LT.50', 'Narasimha\'s Vest', 'Scorpion Harness', 'Gaudy Harness', 'Wonder Kaftan', 'Mrc.Cpt. Doublet', 'Beetle Harness', 'Angler\'s Tunica' },
        Hands  = { 'Thick Mufflers', 'Beast Gloves', 'Wonder Mitts', 'Battle Gloves', 'Ryl.Ftm. Gloves' },
        Rings  = { 'Flame Ring', 'Tamas Ring//MSJ', 'Sun Ring', 'Sun Ring', 'Courage Ring', 'Kshama Ring No.2', 'Balance Ring' },
        Back   = { 'Forager\'s Mantle', 'Psilos Mantle', 'Raptor Mantle', 'Ram Mantle' },
        Waist  = { 'Swift Belt', 'Tilt Belt', 'Warrior\'s Belt' },
        Legs   = { 'Thick Breeches', 'Monster Trousers', 'Beast Trousers', 'Shep. Hose', 'San. Trousers', 'Ryl.Ftm. Trousers' },
        Feet   = { 'Thick Sollerets', 'Monster Gaiters', 'Beast Gaiters', 'Wonder Clomps', 'Bounding Boots' },
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
	Similar to accuracy except will be used on ranged attacks
--]]

	['Ranged_Accuracy'] = {
	},
	
--[[
	If evasion wanted, equip evasion gear. Remember that AGI converts to evasion: for every
	2 points of AGI you get 1 point of evasion
--]]

	['Evasion'] = {
		Ammo  = 'Orphic Egg//PJPBRD',
        Head  = { 'Optical Hat', 'Empress Hairpin' },
		Ears  = { 'Bat Earring//BLINDED', 'Ethereal Earring', 'Reraise Earring' },
        Body  = { 'Scorpion Harness', 'Narasimha\'s Vest' },
		Hands = 'Battle Gloves',
        Legs  = { 'Shep. Hose//PETFNPF', 'San. Trousers' },
		Feet  = 'Bounding Boots',	-- default gear is thick sollerets which are -2 eva
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
		Main = 'Pluto\'s Staff//WSWAP//MSJ',
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
	Midshot is the second stage of a ranged shot. This is where you place
	Ranged Attack, Ranged Damage, recycle, etc.
--]]

	['Midshot'] = {
		Head  = 'Optical Hat',
		Neck  = 'Peacock Amulet',
		Rings = { 'Woodsman Ring', 'Woodsman Ring', 'Jaeger Ring', 'Beetle Ring +1', 'Beetle Ring +1' },	
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
	The second stage is Midcast. This is where you equip gear that gives 
	magic attack, enhancing bonuses, potency improvements, duration
	enhancements, recast reduction gear, etc. This implementation breaks 
	out the midcast into separate routines based on the magic type: 
	healing, divine, elemental, enhancing, enfeebling, summoning, ninjutsu,
	and song. Within each routine there's further logic breaking down
	functional paths. Based on spell (or song), the appropriate gear set
	is equipped.
	
	Every gear set will have an alternative "tanking" version if your
	main job supports tanking (BST does not.) I've included details on
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

--[[	
	Curing magic addresses healing players/npcs. Each time a cure 
	spell is cast, a power calculation is performed to determine 
	the base effect of the spell. After that, any bonuses will be 
	applied. What this means is that MND, VIT and healing magic 
	skill impact your power rating, but once the cap is hit, they 
	have no more influence.
	
		power = (MND*3) + VIT + (Healing Magic Skill*0.6)
	
	This chart lists all /WHM or /RDM curing spells, the power cap, 
	and the effect on HP baseline. Curaga spells are included too.
		
			Spell		cap		low		high
			-----		---		---		---
			Cure		100		 20		 30
			Cure II		170		 75		 90
			Cure III	300		160		190
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
	
--[[
	Spikes, place an elemental buff around the player which causes damage/
	status effect to any monster that hits the player. Each type of spike 
	spell has a different formula for how much damage they do and only 
	some potentially add a status effect. All spikes all are based on INT 
	and Enhancing Magic Skill.
--]]
	
	['Spike'] = {
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

--[[
	Sneak's duration is variable, but the duration maxes at about 5 
	minutes. Include any gear that enhances this buff.
--]]

	['Sneak'] = {
		Feet = 'Dream Boots +1',
	},

--[[
	Invisible's duration is variable, but the duration maxes at 
	about 5 minutes. Include any gear that enhances this buff.
--]]	

	['Invisible'] = {
		Hands = 'Dream Mittens +1',
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

--[[
	Enfeebling divine spell (flash) afflicts the target with accuracy 
	reduction (similar to blind) with a weakening effect over time till
	it runs out. Duration is subject to resists and partial resists
	although can last 12 seconds if not resisted. It also generates a
	significant amount of volitile and cumulative enmity.
--]]	
	
	['EnfeebleDivine'] = {
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

--[[
	Enfeebling Magic: TBD
--]]
	
	['EnfeeblingINT'] = {
	},

	['EnfeeblingMND'] = {
	    Neck  = { 'Promise Badge', 'Justice Badge' },
        Rings = { 'Tamas Ring', 'Tranquility Ring' },
        Waist = 'Friar\'s Rope',
	},

--[[
	Singing: TBD
--]]

--[[
	Ninjutsu:
--]]
	['NinjutsuBuff'] = {
	},
	
	['NinjutsuDebuff'] = {
	},
	
	['NinjutsuElemental'] = {
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
        Head  = 'Beast helm +1',
		Body  = 'Monster Jackcoat',
        Rings = 'Tamas Ring',
        Feet  = 'Mannequin Pumps',  
	},

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
	Kite is used for kiting. Emphasis should be placed on gear that increases 
	movement speed, but you might also want gear that has evasion. The choice
	is yours.
--]]

	-- Kite speed gear, does not include nation aketons which are found in conditional gear for home town
	['Kite'] = { 
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
profile.JugPets = T {
	['carrot broth'] = { 
		['name'] = 'Hare Familiar', ['min'] = 23, ['max'] = 35, 
		['have'] = false, ['fav'] = false
	},
	['herbal broth'] = { 
		['name'] = 'Sheep Familiar', ['min'] = 23, ['max'] = 35,
		['have'] = false, ['fav'] = false
	},
	['humus'] = { 
		['name'] = 'Flowerpot Bill', ['min'] = 28, ['max'] = 40,
		['have'] = false, ['fav'] = false
	},
	['meat broth'] = { 
		['name'] = 'Tiger Familiar', ['min'] = 28, ['max'] = 40,
		['have'] = false, ['fav'] = false
	},
	['grasshopper broth'] = { 
		['name'] = 'Flytrap Familiar', ['min'] = 28, ['max'] = 40,
		['have'] = false, ['fav'] = false
	},
	['carrion broth'] = { 
		['name'] = 'Lizard Familiar', ['min'] = 33, ['max'] = 45,
		['have'] = false, ['fav'] = false
	},
	['bug broth'] = { 
		['name'] = 'Mayfly Familiar', ['min'] = 33, ['max'] = 45,
		['have'] = false, ['fav'] = false
	},
	['mole broth'] = { 
		['name'] = 'Eft Familiar', ['min'] = 33, ['max'] = 45,
		['have'] = false, ['fav'] = false
	},
	['tree sap'] = { 
		['name'] = 'Beetle Familiar', ['min'] = 38, ['max'] = 45,
		['have'] = false, ['fav'] = false
	},
	['antica broth'] = { 
		['name'] = 'Antlion Familiar', ['min'] = 38, ['max'] = 50,
		['have'] = false, ['fav'] = false
	},
	['fish broth'] = { 
		['name'] = 'Crab Familiar', ['min'] = 23, ['max'] = 55,
		['have'] = false, ['fav'] = false
	},
	['blood bath'] = { 
		['name'] = 'Mite Familiar', ['min'] = 43, ['max'] = 55,
		['have'] = false, ['fav'] = false
	},
	['f. carrot broth'] = { 
		['name'] = 'Keeneared Steffi', ['min'] = 43, ['max'] = 75,
		['have'] = false, ['fav'] = false
	},
	['s. herbal broth'] = { 
		['name'] = 'Lullaby Melodia', ['min'] = 43, ['max'] = 75,
		['have'] = false, ['fav'] = true
	},
	['rich humus'] = { 
		['name'] = 'Flowerpot Ben', ['min'] = 51, ['max'] = 75,
		['have'] = false, ['fav'] = false
	},
	['w. meat broth'] = { 
		['name'] = 'Saber Siravarde', ['min'] = 51, ['max'] = 75,
		['have'] = false, ['fav'] = false
	},
	['seedbed soil'] = { 
		['name'] = 'Funguar Familiar', ['min'] = 33, ['max'] = 65,
		['have'] = false, ['fav'] = false
	},
	['qdv. bug broth'] = { 
		['name'] = 'Shellbuster Orob', ['min'] = 53, ['max'] = 75,
		['have'] = false, ['fav'] = false
	},
	['c. carrion broth'] = { 
		['name'] = 'Coldblood Como', ['min'] = 53, ['max'] = 75,
		['have'] = false, ['fav'] = false
	},
	['fish oil broth'] = { 
		['name'] = 'Courier Carrie', ['min'] = 23, ['max'] = 75,
		['have'] = false, ['fav'] = false
	},
	['alchemist water'] = { 
		['name'] = 'Homunculus', ['min'] = 23, ['max'] = 75,
		['have'] = false, ['fav'] = false
	},
	['n. grasshopper broth'] = { 
		['name'] = 'Voracious Audrey', ['min'] = 53, ['max'] = 75,
		['have'] = false, ['fav'] = false
	},
	['l. mole broth'] = { 
		['name'] = 'Ambusher Allie', ['min'] = 58, ['max'] = 75,
		['have'] = false, ['fav'] = false
	},
	['scarlet sap'] = { 
		['name'] = 'Panzer Galahad', ['min'] = 63, ['max'] = 75,
		['have'] = false, ['fav'] = false
	},
	['c. blood broth'] = { 
		['name'] = 'Lifedrinker Lars', ['min'] = 63, ['max'] = 75,
		['have'] = false, ['fav'] = false
	},
	['f. antica broth'] = { 
		['name'] = 'Chopsuey Chucky', ['min'] = 63, ['max'] = 75,
		['have'] = false, ['fav'] = false
	},
	['sun water'] = { 
		['name'] = 'Amigo Sabotender', ['min'] = 75, ['max'] = 75,
		['have'] = false, ['fav'] = false
	}
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

function HandlePetAction(PetAction)
	local pet = gData.GetPet();
	
	-- Only gear swap if this flag is true or the pet is a summoned pet
	if gcdisplay.GetToggle('GSwap') == false or gcinclude.fSummonerPet() == true then
		return;
	end

	-- Only BST pet attacks have associated gear sets because /smn pets are at most half the
	-- level of your BST level
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

function profile.OnLoad()
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
	fFindJugPets traverses the master list of jugs and determines if any of the accessible jugs
	make sense to equip based on the level range of the pet and the player's level. All that
	meet that criteria will have their appropriate indicators turned on. Returned is whether 
	any jug pets that match that criteria were found.
--]]

function fFindJugPets()
	local inventory = AshitaCore:GetMemoryManager():GetInventory();
	local resources = AshitaCore:GetResourceManager();
	local player = gData.GetPlayer();
	local tStorage = EQUIPABLE_NONHOLIDAY;
	local iCount = 0;
	
	-- Clear the table ownership settings
	for k,v in pairs(profile.JugPets) do
		v['have'] = false;
	end
	
	-- Now walk the equipable (in the field) storage areas
	for k,v in ipairs(tStorage) do
		containerID = v['id'];
		-- then loop through the selected container looking for a jug pet's broth
		for j = 1,inventory:GetContainerCountMax(containerID),1 do
			local itemEntry = inventory:GetContainerItem(containerID, j);
			if (itemEntry.Id ~= 0 and itemEntry.Id ~= 65535) then
                local item = resources:GetItemById(itemEntry.Id);
				if item ~= nil then
					-- then check the master list of jug pets
					for kk,tpf in pairs(profile.JugPets) do
						if kk == string.lower(item.Name[1]) then
							if (tpf['min'] <= player.MainJobSync) and 
								(tpf['max'] >= player.MainJobSync) then
								-- finally, this one is possible to be selected
								profile.JugPets[kk]['have'] = true;
								iCount = iCount + 1;
							end
						end
					end
				end
			end
		end
	end
	
	-- assuming any were found, return true or false
	return (iCount > 0);
end		-- fFindJugPets

--[[
	fEquipMaxEquipableJugPet determines what is the best jug pet to load and equips it. The success is returned.
	The way this function works is by searching for available jug pets and determining which are level
	appropriate. Of those check if any are favored. If so, equip that one, else equip first one on list.
--]]

function fEquipMaxEquipableJugPet(cs)
	local sBroth = nil;
	
	if cs == nil then
		cs = sets.CurrentGear;
	end

	-- find any equipable jug pets
	if fFindJugPets() == true then
		-- then cycle through the list and find the favored one. If none favored, the first jug found will be used.
		for k,v in pairs(profile.JugPets) do
			if v['have'] == true then		-- One of the found pets
				sBroth = k;
				if v['fave'] == true then	-- Favored
					break;
				end
			end
		end
		if sBroth ~= nil then
			cs['Ammo'] = sBroth;
			return true;
		else
			print(chat.header('fEquipMaxEquipableJugPet'):append(chat.message('Error: Found jug, but none equipped!')));
			return false;
		end
	else
		print(chat.header('fEquipMaxEquipableJugPet'):append(chat.message('Error: No jug pets found to equip')));
		return false;
	end
end		-- fEquipMaxEquipableJugPet

--[[
	HandleCommand is run when you type in a command defined in LUASHITACAST. The commands 
	handled here instead of in gcinclude.HandleCommands are specific to BST or the help 
	system.
--]]

function profile.HandleCommand(args)
	if args[1] == 'help' then
		gcdisplay.ShowHelp();
	elseif (args[1] == 'ajug') then			-- Turns on/off whether Automated Jug Pets supported
		gcdisplay.AdvanceToggle('AJug');
	elseif args[1] == 'petfood' then
		gcinclude.fPetReward(args[2],true);
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

	gcinclude.StartReminder();		-- See if reminder should be printed
	
	-- A pet action takes priority over a player's action as long as it is a BST pet action.
	-- /SMN pet's actions are not supported.
	if pet ~= nil then
		local sLName = string.lower(pet.Name);
		if petAction ~= nil and gcinclude.fSummonerPet() == false then
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
	if (player ~= nil and player.Status == 'Engaged') or (pet ~= nil and pet.Status == 'Engaged') then
		gcinclude.MoveToCurrent(sets.TP,sets.CurrentGear);
		gcinclude.settings.priorityEngaged = string.upper(gcinclude.settings.priorityEngaged);
		for i = 1,string.len(gcinclude.settings.priorityEngaged),1 do
			cKey = string.sub(gcinclude.settings.priorityEngaged,i,i);
			if cKey == 'C' then		-- Evasion			
				if gcdisplay.GetToggle('Eva') == true then
					gcinclude.MoveToCurrent(sets.Evasion,sets.CurrentGear);
				end
			elseif cKey == 'E' then		-- Accuracy	
				gcinclude.FractionalAccuracy(sets.Accuracy);
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
			local sStave = gcinclude.fCheckForEleGear('staff','dark');
			if sStave ~= nil then
				gcinclude.fSwapToStave(sStave,false,sets.CurrentGear);
			end
		end
	elseif pet ~= nil then
		-- Player idling with pet
		gcinclude.MoveToCurrent(sets.Default_WPet,sets.CurrentGear);
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
	
	-- And make sure a weapon equipped. (Going into a capped area can cause no 
	-- weapon to be equipped.)
	local gear = gData.GetEquipment();
	if gear.Main == nil then
		gcinclude.MoveToCurrent(sets.Start_Weapons,sets.CurrentGear,true);
	end
	
	gcinclude.EquipTheGear(sets.CurrentGear);		-- Equip the composited HandleDefault set
	
	-- Lastly, update the display, just in case
	gcdisplay.Update();
end		-- HandleDefault

--[[
	fAmmoIsJug determines if the item in the Ammo slot is a jug pet or not.
--]]

function fAmmoIsJug(sAmmo)
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
end		-- fAmmoIsJug

--[[
	HandleAbility is used to change the player's gear appropriately for the specified pet ability.
--]]

function profile.HandleAbility()
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
			local bJugFound = fAmmoIsJug(profile.sAmmo);
			if bJugFound == nil or (bJugFound ~= nil and bJugFound == false) then
				profile.bAmmo = fEquipMaxEquipableJugPet(sets.CurrentGear);
			end
		end
		gcinclude.MoveToCurrent(sets.CallBeast,sets.CurrentGear);
	elseif string.match(ability.Name, 'Familiar') then
		gcinclude.MoveToCurrent(sets.Familiar,sets.CurrentGear);
	elseif string.match(ability.Name, 'Gauge') then
		gcinclude.MoveToCurrent(sets.Gauge,sets.CurrentGear);
	elseif string.match(ability.Name, 'Reward') then
		gcinclude.MoveToCurrent(sets.Reward,sets.CurrentGear);
		-- Pet reward. Make sure that pet food already equipped
		if profile.sAmmo == nil or string.find(string.lower(profile.sAmmo),'pet f') == nil then		-- something else equipped
			if gcdisplay.GetCycle('DB') == 'Norm' then
				profile.bAmmo = gcinclude.fPetReward(nil,true);
			else
				profile.bAmmo = gcinclude.fPetReward(nil,false);
			end
		end
	elseif string.match(ability.Name, 'Tame') then
		-- Trying to tame a beast. (Someone's charm failed.)
		gcinclude.MoveToCurrent(sets.Tame,sets.CurrentGear);		
	elseif string.match(ability.Name, 'Charm') then
		-- Trying to charm a beast. 
		gcinclude.MoveToCurrent(sets.Charm,sets.CurrentGear);
		local sStave = gcinclude.fCheckForEleGear('staff','light');
		if sStave ~= nil then
			gcinclude.fSwapToStave(sStave,false,sets.CurrentGear);
		end
	-- And now the subjob abilities
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
	-- /DRK
	elseif string.match(ability.Name, 'Arcane Circle') then
		gcinclude.MoveToCurrent(sets.ArcaneCircle,sets.CurrentGear);
	elseif string.match(ability.Name, 'Last Resort') then
		gcinclude.MoveToCurrent(sets.LastResort,sets.CurrentGear);
	elseif string.match(ability.Name, 'Weapon Bash') then
		gcinclude.MoveToCurrent(sets.WeaponBash,sets.CurrentGear);
	elseif string.match(ability.Name, 'Souleater') then
		gcinclude.MoveToCurrent(sets.Souleater,sets.CurrentGear);	
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
		gcinclude.MoveToCurrent(sets.Third_Eye,sets.CurrentGear);
	elseif string.match(ability.Name, 'Hasso') then
		gcinclude.MoveToCurrent(sets.Hasso,sets.CurrentGear);
	elseif string.match(ability.Name, 'Meditate') then
		gcinclude.MoveToCurrent(sets.Meditate,sets.CurrentGear);
	elseif string.match(ability.Name, 'Seigan') then
		gcinclude.MoveToCurrent(sets.Seigan,sets.CurrentGear);
	-- /DRG
	elseif string.match(ability.Name, 'Ancient Circle') then
		gcinclude.MoveToCurrent(sets.AncientCircle,sets.CurrentGear);
	elseif string.match(ability.Name, 'Jump') then
		gcinclude.MoveToCurrent(sets.Jump,sets.CurrentGear);
	elseif string.match(ability.Name, 'High Jump') then
		gcinclude.MoveToCurrent(sets.HighJump,sets.CurrentGear);		
	-- /PLD
	elseif string.match(ability.Name, 'Holy Circle') then
		gcinclude.MoveToCurrent(sets.HolyCircle,sets.CurrentGear);
	elseif string.match(ability.Name, 'Shield Bash') then
		gcinclude.MoveToCurrent(sets.ShieldBash,sets.CurrentGear);
	elseif string.match(ability.Name, 'Sentinel') then
		gcinclude.MoveToCurrent(sets.Sentinel,sets.CurrentGear);	
	elseif string.match(ability.Name, 'Cover') then
		gcinclude.MoveToCurrent(sets.Cover,sets.CurrentGear);
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
	
	-- Only process if /gswap is turned on
	if gcdisplay.GetToggle('GSwap') == false then
		return;
	end

	-- Clear out the CurrentGear in case of leftovers
	gcinclude.ClearSet(sets.CurrentGear);
	
	-- Equip the precast gear set
	gcinclude.HandlePrecast();
	gcinclude.EquipTheGear(sets.CurrentGear);	
end		-- HandlePrecast

--[[
	HandleMidcast is the second function invoked when a player casts a spell. It equips gear appropriate for 
	magic skill, duration, magic attack bonus, magic accuracy, and potency. There's an order to how the pieces 
	are loaded: INT/MND, spell specific, macc, magic skill, obi, ele swap	
--]]

function profile.HandleMidcast()

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
	
	gcinclude.EquipTheGear(sets.CurrentGear);		-- Equip the composited midcast set
end		-- gcinclude.HandleMidcast

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
	-- Only gear swap if this flag is true
	if gcdisplay.GetToggle('GSwap') == false then
		return;
	end

	-- Clear out the CurrentGear in case of leftovers
	gcinclude.ClearSet(sets.CurrentGear);
	
	gcinclude.MoveToCurrent(sets.Midshot,sets.CurrentGear);
	gcinclude.FractionalAccuracy(gProfile.Sets.Ranged_Accuracy);
	
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