local profile = {};
gcinclude = gFunc.LoadFile('common\\gcinclude.lua');

--[[
	This file contains all the gear sets associated with the WHM job. While it isn't outside of the realm of 
	possibility that the subjob might be able to use gear sets too, that is not the emphasis of this program. 
	It is tailored to handle all the aspects of WHM. If you desire a gear set change to strengthen an ability
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
	of your PLD, whichever is lower. That means you can charm higher level mobs than you would expect with /bst.
	Just note though that you can't have two pets, so if you have charmed a pet with /bst, you can't summon your
	avatar and visa versa.
			
	*** Note 3 ***
	I have restored all of the _conditional gear sets because the User-Definied conditionals are not limited to
	what gear is available. (A lot of these sets were originally removed since there was no conditional gear
	in era for them.)
	
--]]

--[[
	The "Idle" set is what your character will wear when it is not fighting nor resting nor in town. Whether just 
	standing out of town or going to a different area, the "Idle" set will be equipped. If you've subbed /SMN,
	it is strongly recommended that you use gear that has avatar perpetuation cost down attributes on it here. If
	you've subbed /BST, gear swaps occur during the appropriate ability.
--]]

	['Idle'] = {
    },
	['Idle_Conditional'] = {
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
		When you are resting (kneeling down), your HP 'Resting' set will be equipped. If your subjob
		uses MP and your MP is below the set threshhold (defined by gcinclude.settings.RefreshGearMP), 
		your MP 'Resting_Refresh' gear set will be equipped. Regardless of which set is equipped, 
		assuming that your subjob uses magic, you have a Dark/Pluto staff accessible, weapon swapping 
		is enabled (/wswap), and your MP is not at maximum, the Dark/Pluto staff will automatically be 
		equipped.
	--]]
	
	['Resting'] = { 
	},
	['Resting_Conditional'] = {
	},
	
	['Resting_Refresh'] = {
	},
	['Resting_Refresh_Conditional'] = {
	},

	-- If you have any Spell Interruption Rate down gear, put them into the "SIR" gear set.
	-- This gear set is equipped in the HandleMidcast function that all spells go through.
	-- Currently only gear equippable by any job is applicable here. There's no gear that's 
	-- specific for WHM that gives spell interruption rate down.
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
    },
	['Start_Weapons_Conditional'] = {
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
	one will be equipped depending on whether DT is enabled and which set is equipped depends on the DT_TYPE 
	selected. Please consider not including gear that doesn't have any damage taken property so other wanted 
	stats can shine through.
--]]

	['DT_Physical'] = {
	},
	['DT_Physical_Conditional'] = {
	}
	
	['DT_Magical'] = {
    },
	['DT_Magical_Conditional'] = {
	},
	
	['DT_Breath'] = { 
	},
	['DT_Breath_Conditional'] = {
	},
	
--[[
		The TP sets are used when you are fighting. The accuracy set will be used if /acc is specified
		and the evasion set if /eva is specified. Please note that if you have a subjob that can use a
		pet, none of the abilities are supported here. Only main jobs that have pets (SMN,BST) support
		pet actions.
--]]

	['TP'] = {
    },
	['TP_Conditional'] = {
	},

	['TP_Pet'] = {
    },
	['TP_Pet_Conditional'] = {
	},
	
--[[
	If an accuracy emphasis is desired, the following set will replace the gear appropriately.
	(Please note that Pet_Accuracy is applied after Accuracy if you have a pet.)
--]]
	
	['Accuracy'] = {
    },
	['Accuracy_Conditional'] = {
	},
	
	['Pet_Accuracy'] = {
    },
	['Pet_Accuracy_Conditional'] = {
	},
	
--[[
	Haste gear
--]]

	['Haste'] = {
	},
	['Haste_Conditional'] = {
	},
	
--[[
	If evasion wanted, equip evasion gear
--]]
	
	['Evasion'] = {
    },
	['Evasion_Conditional'] = {
	},

--[[
	Magic accuracy gear
--]]

	['Macc'] = {
    },
	['Macc_Conditional'] = {
	},

--[[
	Enmity sets are used to boost/reduce enmity, accordingly
--]]

	['Enmity_Plus'] = {
	},
	['Enmity_Plus_Conditional'] = {
	},

	['Enmity_Minus'] = {
	},
	['Enmity_Minus_Conditional'] = {
	},

--[[
	Preshot is the first stage of when a ranged shot is being performed. This is where you place any 
	Ranged Accuracy or Ranged Attack Speed gear. 
--]]

	['Preshot'] = {
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
	listed have gear that a WHM or anyone can wear.
--]]

	-- Healing: Healing Magic Skill. Currently only a Healing Earring affects healing spells from a sub job. No other gear
    -- gives bonuses to Healing magic from a sub job. Also, gear with MND bonuses will boost cure spell's potency, but MND 
	-- gear is automatically equipped prior to the Healing set being equipped in the HandleMidcast function. There's no need 
	-- to include MND gear here.
	['Healing'] = {
    },
	['Healing_Conditional'] = {
	},
	
	-- Dark: Dark Magic Skill. Currently only gear equippable by any job gives is applicable here. There's no gear that's 
	-- specific for WHM that gives any dark magic skill.	
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
	-- specific for WHM that gives any enfeebling magic skill.	
	['Enfeebling'] = {
	},
	['Enfeebling_Conditional'] = {
	},
	
	-- Enhancing: There is no gear that a WHM can wear to enhance any magic spell. Leave the Enhancing gear sets empty.
	['Enhancing'] = {
	},
	['Enhancing_Conditional'] = {
	},
	
	-- Elemental: Elemental Magic Skill. Currently only gear equippable by any job gives is applicable here. There's no gear
	-- that's specific for WHM that gives any elemental magic skill. Note: don't include elemental staves or elemental 
	-- obis/gorgets here, that is done automatically in the HandlePrecast/HandleMidcast functions (if /wswap is enabled).
	['Elemental'] = {
	},
	['Elemental_Conditional'] = {
	},

	-- Ninjitsu: There is no gear that a WHM can wear to add Ninjitsu skill. Leave the following two
	-- gear sets empty.	
	['Ninjitsu'] = {			-- Ninjitsu Skill, magic burst bonus, magic attack bonus
	},
	['Ninjitsu_Conditional'] = {
	},
	
	-- Summoning: Summoning Magic Skill and Avatar Perpetuation Cost. Currently only gear equippable by any job gives
	-- is applicable here. There's no gear that's specific for WHM that gives any summoning skill. Note: currently on 
	-- HorizonXI summoning skills are ignored. Any gear piece that only gives summoning skill will be commented out	
	['Summoning'] = {
	},
	['Summoning_Conditional'] = {
	},
	
--[[
	Next is stat-based gear, (in this case intelligence or mind)
--]]

	['INT'] = {
    },
	['INT_Conditional'] = {
	},
	
	['MND'] = {
    },
	['MND_Conditional'] = {
	},

--[[
	And some spells are special cases, so they have individual gears sets.
--]]
	
	-- Stoneskin: Stoneskin Enhancement, Mind, and Enhancing Magic Skill. Mind is 3x more important than enhancing
	-- Magic Skill. The only piece of gear a WHM can wear to enhance stoneskin is a Stone Gorget. There's no gear
	-- that a WHM (or any job) can wear to enhance magic. Note: This gear set has no effect on Titan's Stoneskin
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
	-- WHM enhances Aspir. Aspir is part of Dark Magic, so potency which is based on dark magic skill will
	-- already be loaded in HandleMidcast function and need not be repeated here. No current gear supports
	-- dark magic accuracy for any job. Magic attack bonus and magic critical hit have no effect on potency.
	-- Leave the two Aspir gear sets empty.
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
	The following are abilities affected by gear
--]]
	
--[[
	The following weapon skill gearsets are defined by the stat they emphasize. Listed are all of the sets that
	you will need to use every weapon skill that your job can do. The leading comment defines what weapon/weapon
	skill combination the set applies to.
	
	WHM can use the following weapons: Dagger (A-), Sword (D), Club (E), H2H (E), Marksmanship (C+), Archery (C-)
	Any other weapon will have no weaponskill available. Weapon skill sets are named based on stat(s) used, 
	regardless of weapon
--]]

--[[
		* Strength based or just skill based *
		
		Sword: Flat Blade,Circle Blade,Vorpal Blade,Spirits Within,Mercy Stroke
		Club: Starlight,Skull Breaker,True Strike
		H2H: Spinning Attack
-]]
	
	['WS_STR'] = {
    },
	['WS_STR_Conditional'] = {
	},

--[[
		* Strength and Agility based *
		
		Archery: Flaming Arrow^,Piercing Arrow^,Dulling Arrow^,Sidewinder^
		
		^ Subjob must be RNG
--]]

	['WS_STRAGI'] = {
    },
	['WS_STRAGI_Conditional'] = {
	},
	
--[[
		* Strength and Dexterity based, even weighting *
		
		Sword: Fast Blade
		H2H: Combo,Backhand Blow,Raging Fist^
		
		^ Subjob must be MNK
--]]

	['WS_STRDEX'] = {
    },
	['WS_STRDEX_Conditional'] = {
	},

--[[
		* Strength and Intelligence based, even weighting *
		
		Sword: Burning Blade,Red Lotus Blade
--]]
	
	['WS_STRINT'] = {
    },
	['WS_STRINT_Conditional'] = {
	},

--[[
		* Strength and Mind based, even weighting *
		
		Sword: Shining Blade,Seraph Blade
		Club: Shining Strike
--]]

	['WS_STRMND'] = {
    },
	['WS_STRMND_Conditional'] = {
	},


--[[
		* Agility based *
		
		Marksmanship: Hot Shot^,Split Shot^,Sniper Shot^,Slug Shot^
		
		^ Subjob must be RNG
--]]

	['WS_AGI'] = {
    },
	['WS_AGI_Conditional'] = {
	},
	
--[[
		* Charisma based *
		
		Dagger: Shadowstitch
--]]
	
	['WS_CHR'] = {
    },
	['WS_CHR_Conditional'] = {
	},

--[[
		* Dexterity based *
		
		Dagger: Wasp Sting,Viper Bite,Eviseration
--]]
	
	['WS_DEX'] = {
    },
	['WS_DEX_Conditional'] = {
	},

--[[
		* Dexterity and Agility based *
		
		Dagger: Shark Bite
--]]
	
	['WS_DEXAGI'] = {
    },
	['WS_DEXAGI_Conditional'] = {
	},
	
--[[
		* Dexterity and Charisma based *
		
		Dagger: Dancing Edge
--]]
	
	['WS_DEXCHR'] = {
    },
	['WS_DEXCHR_Conditional'] = {
	},
	
--[[
		* Dexterity and Intelligence based *
		
		Dagger: Gust Slash,Cyclone
--]]
	
	['WS_DEXINT'] = {
    },
	['WS_DEXINT_Conditional'] = {
	},

--[[
		* Mind based *
		
		Dagger: Energy Steal,Energy Drain
--]]

	['WS_MND'] = {
    },
	['WS_MND_Conditional'] = {
	},
	
--[[
		* Vitality based *
		
		H2H: Shoulder Tackle,One Inch Punch^
		
		^ Subjob must be MNK
--]]

	['WS_VIT'] = {
    },
	['WS_VIT_Conditional'] = {
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
	
	['DivineSeal'] = {
	},
	['DivineSeal_Conditional'] = {
	},
	
	['Devotion'] = {
	},
	['Devotion_Conditional'] = {
	},
		
--[[
	Some subjobs really make no sense when combined with dragoon, but all abilities across all jobs that
	have gear that can be equipped by a PLD are included here.
	
	The following sub jobs have no skills with equippable gear by a WHM: WAR,DRG,BLM,MNK,WHM,RDM,RNG,NIN,
	SMN,BRD,SAM,PLD
--]]
	--* BST *--
	-- CHR and Charm + gear. (Every +1 Charm adds 5% Charm duration)
	['Charm'] = {
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
								*** Custom Sets Go below this comment ***
								
	The following "CAP" sets are added as a convenience for playing in level capped areas. The only way for them to be 
	loaded is via the /gearset command, which will turn GSwap off. If you're level syncing, pick the set that's closest 
	to the sync level and adjust accordingly.
--]]

	['CAP20'] = {
    },
	
	['CAP25'] = {
    },
	
	['CAP30'] = {
    },
	
	['CAP40'] = {
    },
	
	['CAP50'] = {
    },

	['CAP60'] = {
    },
	
--[[
	The following set is used to dynamically create a gear set to be displayed once rather
	than in a piecemeal manner. It is hoped that this will cut down on flickering gear and
	possibly speed up the code. *** This set is to be left empty by the player ***. Please
	do not modify it.
--]]	
	['CurrentGear'] = { },		
};

profile.Sets = sets;
profile.sjb = nil;
profile.bAmmo = false;
profile.sAmmo = nil;

local function HandlePetAction(PetAction)
	-- Only gear swap if this flag is true
	if gcdisplay.GetToggle('GSwap') == false then
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
	local subs = {['WAR'] = 0, ['MNK'] = 0, ['WHM'] = 0, ['BLM'] = 0, ['RDM'] = 0, ['THF'] = nil,
				 ['PLD'] = 0, ['DRK'] = 0, ['BST'] = 0, ['BRD'] = 0, ['RNG'] = 0, ['SMN'] = 0,
				 ['SAM'] = 0, ['NIN'] = 1, ['DRG'] = 0, ['BLU'] = 0, ['COR'] = 0, ['PUP'] = 0,
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
	gcinclude.settings.priorityEngaged = 'ABCDIEFGH';
	gcinclude.settings.priorityMidCast = 'ABCDIEFGH';
	gcinclude.settings.priorityWeaponSkill = 'ABCDE';
	
	-- Determine if subjob uses magic and if the maximum MP is > 50.
	gcinclude.CheckMagic50(player);
	
	-- Set your job macro toolbar defaults here. Which set depends on the subjob
	AshitaCore:GetChatManager():QueueCommand(1, '/macro book 15');		-- WHM
	SetSubjobSet(player.SubJob);
	
	-- Load up the weapons bar. (This need only be done once.)
	gcinclude.MoveToCurrent(sets.Start_Weapons,sets.CurrentGear);
	gcinclude.ProcessConditional(sets.Start_Weapons_Conditional,nil,sets.CurrentGear);	
	gcinclude.EquipTheGear(sets.CurrentGear);
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
	elseif args[1] == 'petfood' then			-- Supported since pet food is not job specific, but very niche
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
	local player = gData.GetPlayer();
	local pet = gData.GetPet();
	local petAction = gData.GetPetAction();	
	local ew = gData.GetEquipment();
	local zone = gData.GetEnvironment();	
	local eWeap = nil;
	local cKey;

	-- Make sure that the global magic settings for the player are known.	
	if gcinclude.settings.bMagicCheck == false or gcinclude.settings.sMJ ~= player.MainJob then
		gcinclude.CheckMagic50(player);
	end

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
		return;
	end
	
	-- If player is not resting and has MP and has swapped weapons, set the weapon back to what 
	-- they had before the switch
	if player.Status ~= 'Resting' and gcdisplay.GetToggle('WSwap') == true then
		if gcinclude.weapon ~= nil and eWeap ~= gcinclude.weapon then
			gFunc.ForceEquip('Main', gcinclude.weapon);	
			gFunc.ForceEquip('Sub', gcinclude.offhand);	
		end
	end
		
	-- Now process the player status accordingly
	if player.Status == 'Engaged' then
		gcinclude.settings.priorityEngaged = string.upper(gcinclude.settings.priorityEngaged);
		for i = 1,string.len(gcinclude.settings.priorityEngaged),1 do
			cKey = string.sub(gcinclude.settings.priorityEngaged,i,i);
			if cKey == 'A' then			-- Player is fighting. Equip the TP gear set
				gcinclude.MoveToCurrent(sets.TP,sets.CurrentGear);
				gcinclude.ProcessConditional(sets.TP_Conditional,nil,sets.CurrentGear);
			elseif cKey == 'B' then		-- Pet (if out) is fighting
				if pet ~= nil and pet.Status == 'Engaged' then
					gcinclude.MoveToCurrent(sets.TP_Pet,sets.CurrentGear);
					gcinclude.ProcessConditional(sets.TP_Pet_Conditional,nil,sets.CurrentGear);
				end		
			elseif cKey == 'C' then		-- Evasion			
				if gcdisplay.GetToggle('Eva') == true then
					gcinclude.MoveToCurrent(sets.Evasion,sets.CurrentGear);
					gcinclude.ProcessConditional(sets.Evasion_Conditional,nil,sets.CurrentGear);
				end
			elseif cKey == 'D' then		-- Enmity		
				local sEmn = gcdisplay.GetCycle('Enmity');
				if sEmn == 'Minus' then
					gcinclude.MoveToCurrent(sets.Enmity_Minus,sets.CurrentGear);
					gcinclude.ProcessConditional(sets.Enmity_Minus_Conditional,nil,sets.CurrentGear);				
				elseif sEmn == 'Plus' then
					gcinclude.MoveToCurrent(sets.Enmity_Plus,sets.CurrentGear);
					gcinclude.ProcessConditional(sets.Enmity_Plus_Conditional,nil,sets.CurrentGear);
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
			elseif cKey == 'I' then				-- Haste
				if gcdisplay.GetToggle('Haste') == true then 
					gcinclude.MoveToCurrent(sets.Haste,sets.CurrentGear);
					gcinclude.ProcessConditional(sets.Haste_Conditional,nil,sets.CurrentGear);
				end			
			end				
		end
	elseif player.Status == 'Resting' then	
		-- Player kneeling. Priority (low to high): Resting,refresh
		gcinclude.MoveToCurrent(sets.Resting,sets.CurrentGear);
		gcinclude.ProcessConditional(sets.Resting_Conditional,nil,sets.CurrentGear);
		if gcinclude.settings.bMagic == true and player.MPP < gcinclude.settings.RefreshGearMPP then
			gcinclude.MoveToCurrent(sets.Resting_Refresh,sets.CurrentGear);
			gcinclude.ProcessConditional(sets.Resting_Refresh_Conditional,nil,sets.CurrentGear);
		end
		
		-- Weapon swap to a weapon that refreshes MP if player's subjob uses magic, weapon swapping
		-- is enabled (/wswap) and their MP is not at maximum
		if gcdisplay.GetToggle('WSwap') == true and player.MP < player.MaxMP then
			if gcinclude.settings.bStave == false then
				gcinclude.CheckForStaves();
			end
			if gcinclude.settings.bStave == true then
				gcinclude.SwapToStave('dark',false,sets.CurrentGear);
			end
		end
		-- Check for common debuffs
		gcinclude.CheckCommonDebuffs();
	else									
		-- Assume idling. Priority (low to high): Idle,refresh
		gcinclude.MoveToCurrent(sets.Idle,sets.CurrentGear);
		gcinclude.ProcessConditional(sets.Idle_Conditional,nil);
		-- See if in a town		
		if not (zone.Area ~= nil and gcinclude.Towns:contains(zone.Area)) then		
			gcinclude.MoveToCurrent(sets.Town,sets.CurrentGear);
			gcinclude.ProcessConditional(gcinclude.sets.Town_Conditional,nil,sets.CurrentGear);
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
		
	gcinclude.EquipTheGear(sets.CurrentGear);		-- Equip the composited HandleDefault set
					
	-- Lastly, update the display, just in case
	gcdisplay.Update();
end

--[[
	HandleAbility is used to change the player's gear appropriately.
--]]

profile.HandleAbility = function()
	local ability = gData.GetAction();
			
	if gcdisplay.GetToggle('GSwap') == false then
		return;
	end
	
	-- Now process the appropriate job ability. Start with abilities associated with WHM
	if string.match(ability.Name, 'Divine Seal') then
		gcinclude.MoveToCurrent(sets.DivineSeal,sets.CurrentGear);
		gcinclude.ProcessConditional(sets.DivineSeal_Conditional,nil,sets.CurrentGear);
	elseif string.contains(ability.Name,'Devotion') then
		gcinclude.MoveToCurrent(sets.Devotion,sets.CurrentGear);
		gcinclude.ProcessConditional(sets.Devotion_Conditional,nil,sets.CurrentGear);
		
	-- And now the subjob abilities
	elseif string.contains(ability.Name, 'Charm') then			-- assumes /bst	
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

	if gcdisplay.GetToggle('GSwap') == true then		-- Only gear swap if this flag is true
		if string.match(item.Name, 'Holy Water') then 
			gcinclude.MoveToCurrent(gcinclude.sets.Holy_Water,sets.CurrentGear);
			gcinclude.ProcessConditional(gcinclude.sets.Holy_Water_Conditional,nil,sets.CurrentGear);
			gcinclude.EquipTheGear(sets.CurrentGear);	-- if more items are added, move this to addess all
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
		elseif cKey == 'I' then				-- Haste
			if gcdisplay.GetToggle('Haste') == true then 
				gcinclude.MoveToCurrent(sets.Haste,sets.CurrentGear);
				gcinclude.ProcessConditional(sets.Haste_Conditional,nil,sets.CurrentGear);
			end			
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
	
	gcinclude.MoveToCurrent(sets.Midshot,sets.CurrentGear);
	gcinclude.ProcessConditional(sets.Midshot_Conditional,nil,sets.CurrentGear);

	-- if enmity wanted, load that
	local sEmn = gcdisplay.GetCycle('Enmity');
	if sEmn == 'Minus' then
		gcinclude.MoveToCurrent(sets.Enmity_Minus,sets.CurrentGear);
		gcinclude.ProcessConditional(sets.Enmity_Minus_Conditional,nil,sets.CurrentGear);
	elseif sEmn == 'Plus' then
		gcinclude.MoveToCurrent(sets.Enmity_Plus,sets.CurrentGear);
		gcinclude.ProcessConditional(sets.Enmity_Plus_Conditional,nil,sets.CurrentGear);
	end
	gcinclude.EquipTheGear(sets.CurrentGear);		-- Equip the composited Midshot set	
end

--[[
	HandleWeaponskill loads the gear appropriately for the weapon skill you're doing
--]]

profile.HandleWeaponskill = function()	
		local ws = gData.GetAction();
		local canWS = gcinclude.CheckWsBailout();
 
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

 	gcinclude.settings.priorityWeaponSkill = string.upper(gcinclude.settings.priorityWeaponSkill);
	for i = 1,string.len(gcinclude.settings.priorityWeaponSkill),1 do
		cKey = string.sub(gcinclude.settings.priorityWeaponSkill,i,i);
		if cKey == 'A' then			-- weaponskill set
			local sWS = gcinclude.WsStat(ws.Name,'STR');

			if sWS == 'WS_AGI' then
				gcinclude.MoveToCurrent(sets.WS_AGI,sets.CurrentGear);
				gcinclude.ProcessConditional(sets.WS_AGI_Conditional,nil,sets.CurrentGear);			
			elseif sWS == 'WS_CHR' then
				gcinclude.MoveToCurrent(sets.WS_CHR,sets.CurrentGear);
				gcinclude.ProcessConditional(sets.WS_CHR_Conditional,nil,sets.CurrentGear);
			elseif sWS == 'WS_DEX' then
				gcinclude.MoveToCurrent(sets.WS_DEX,sets.CurrentGear);
				gcinclude.ProcessConditional(sets.WS_DEX_Conditional,nil,sets.CurrentGear);
			elseif sWS == 'WS_DEXAGI' then
				gcinclude.MoveToCurrent(sets.WS_DEXAGI,sets.CurrentGear);
				gcinclude.ProcessConditional(sets.WS_DEXAGI_Conditional,nil,sets.CurrentGear);
			elseif sWS == 'WS_DEXCHR' then
				gcinclude.MoveToCurrent(sets.WS_DEXCHR,sets.CurrentGear);
				gcinclude.ProcessConditional(sets.WS_DEXCHR_Conditional,nil,sets.CurrentGear);
			elseif sWS == 'WS_DEXINT' then
				gcinclude.MoveToCurrent(sets.WS_DEXINT,sets.CurrentGear);
				gcinclude.ProcessConditional(sets.WS_DEXINT_Conditional,nil,sets.CurrentGear);
			elseif sWS == 'WS_STR' then
				gcinclude.MoveToCurrent(sets.WS_STR,sets.CurrentGear);
				gcinclude.ProcessConditional(sets.WS_STR_Conditional,nil,sets.CurrentGear);
			elseif sWS == 'WS_STRAGI' then
				gcinclude.MoveToCurrent(sets.WS_STRAGI,sets.CurrentGear);
				gcinclude.ProcessConditional(sets.WS_STRAGI_Conditional,nil,sets.CurrentGear);
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
			elseif sWS == 'WS_VIT' then
				gcinclude.MoveToCurrent(sets.WS_VIT,sets.CurrentGear);
				gcinclude.ProcessConditional(sets.WS_VIT_Conditional,nil,sets.CurrentGear);			
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
		elseif cKey == 'C' then		-- enmity	
			local sEmn = gcdisplay.GetCycle('Enmity');
			if sEmn == 'Minus' then
				gcinclude.MoveToCurrent(sets.Enmity_Minus,sets.CurrentGear);
				gcinclude.ProcessConditional(sets.Enmity_Minus_Conditional,nil,sets.CurrentGear);
			elseif sEmn == 'Plus' then
				gcinclude.MoveToCurrent(sets.Enmity_Plus,sets.CurrentGear);
				gcinclude.ProcessConditional(sets.Enmity_Plus_Conditional,nil,sets.CurrentGear);
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
			a skillchain and the automatic equipment of an elemental obi could 
			adversely affect the damage, so this section is not implemented. If I can
			ever figure out how to detect closing a skillchain, I will readdress this.
															- CCF, 1/12/2024
--]]	
		end				
	end
	gcinclude.EquipTheGear(sets.CurrentGear);		-- Equip the composited weaponskill set	
end

return profile;