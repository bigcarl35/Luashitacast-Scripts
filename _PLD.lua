local profile = {};
gcinclude = gFunc.LoadFile('common\\gcinclude.lua');

--[[
	This file contains all the gear sets associated with the PLD job. While it isn't outside of the realm of 
	possibility that the subjob might be able to use gear sets too, that is not the emphasis of this program. 
	It is tailored to handle all the aspects of PLD. If you desire a gear set change to strengthen an ability
	from your subjob that is not supported by this program, you probably will have to make a custom gear set 
	and use the /gearset command to use it.
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
		
	It is recommended that the gear sets not include any gear found in the top line of your 
	equipment grid (main hand, off hand, ranged weapon, ammo). Doing so will mean that TP will be 
	reset to 0 whenever gear is changed which can be very frustrating. Further, any time you do 
	change a weapon, it will convert back to what was defined in a set.	Believe me, it's no fun	
	fighting Luashitacast!

	Also, not all sets need to be defined. There is nothing wrong with leaving a set "empty", but don't delete any
	of the sets. All the ones listed here (except for any custom sets) are expected to exist by Luashitacast.
		
	*** Note ***
	Unlike when summoner is used as a subjob, /bst's pets are charmed at the max level of your BST or the level
	of your PLD, whichever is lower. That means you can charm higher level mobs than you would expect with /bst.
	Just note though that you can't have two pets, so if you have charmed a pet with /bst, you can't summon your
	avatar and visa versa.
	
	Also, unlike true pet jobs like SMN and BST, PLD can only have a pet through a subjob. While associated pet
	gearsets are available, you equally can just skip them since the pet is at half your level.

	*** Note 2 ***
	No gear that supports bard songs can be worn by any job except a bard, no there's no support given here for
	/BRD.
--]]

--[[
	The TP sets are used when you are fighting.	The accuracy set will be used if ACC is specified 
	and the evasion set if EVA is specified. TP_Tank is equipped if indicated. It's a means for
	the PLS to equip more defensive gear if they find themselves tanking.
--]]
	
	['TP'] = {
		Head  = 'Mandra. Masque',
		Body  = 'Choc. Jack Coat',
		Hands = 'Fsh. Gloves',
		Rings = { 'San d\'Orian Ring', 'Bastokan Ring' },
		Legs  = 'Fisherman\'s Hose',
		Feet  = 'Waders',
    },
	
	['TP_Tank'] = {
	},
	
--[[
	If an accuracy emphasis is desired, the following set will replace the gear appropriately.
	Remember that DEX converts to accuracy: for every 1 point of DEX you get 0.75 points
	of accuracy. Tank_Accuracy is a subset of Accuracy. It lets you specify what accuracy
	gear to equip that doesn't compromise your tanking set as much as a full-blown accuracy
	set would.
--]]
	
	['Accuracy'] = {
    },
	
	['Tank_Accuracy'] = {
	},
	
--[[
	If evasion wanted, equip evasion gear. Tank_Evasion is a subset of Evasion. It lets you 
	specify what evasion gear to equip that doesn't compromise your tanking set as much as 
	a full-blown evasion set would.
--]]
	
	['Evasion'] = {
    },

	['Tank_Evasion'] = {
	},
	
--[[
	The Idle_Regen and Idle_Refresh gear sets are used to restore a player's HP or MP that goes 
	below a set percentage (defined in gcinclude.lua, but can be overriden in profile.OnLoad
	function).
--]]
	
	['Idle_Regen'] = {
	},
	
	['Idle_Refresh'] = {
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

	-- If you have any Spell Interruption Rate down gear, put them into the "SIR" gear set.
	-- This gear set is equipped in the HandleMidcast function that all spells go through.
	['SIR'] = {
	},
	
--[[
	Start weapons are where you define what you want the first row of equipment to look like when you
	either log in as a PLD or you switch your main job to PLD. 
--]]

	['Start_Weapons'] = {
    },
	
--[[
	Specify what you want to wear around town.
--]]
	
	['Town'] = {
		Head = 'Lilac Corsage',
		Body = 'Ducal Aketon//AK:OMNI',	
    },

--[[
	The "Travel" gear set is what is worn when you're not fighting (either
	you or your pet), you're not resting. It's a good place to put gear 
	that increases your movement speed. (Not to be confused with the 
	['Movement'] gear set which is used when you're kiting.) This is also 
	where you put gear that is adventageous if you have a pet present 
	(i.e., lower perpetuation cost, etc.)
--]]
		
	['Travel'] = {
	},
	
--[[
	Damage reduction gear depends on the type of damage. The most common is physical, but there's times when
	you'll want to reduce magic damage or breath damage. The three gear sets are defined below. The correct
	one will be equipped depending on how DT is set. Please consider not including gear that doesn't have 
	any damage taken property so other wanted stats can shine through.
--]]

	['DT_Physical'] = {
	},
	
	['DT_Magical'] = {
    },
	
	['DT_Breath'] = { 
	},
	
--[[
	Magic accuracy gear
--]]

	['Macc'] = {
    },

--[[
	Magic Attack Bonus (MAB) is used for more than just spells, so it is broken out
--]]

	['MAB'] = {
	},
	
--[[
	Preshot is the first stage of when a ranged shot is being performed. This is where you place any 
	Ranged Accuracy or Ranged Attack Speed gear. 
--]]

	['Preshot'] = {
    },
	
--[[
	Midshot is the second stage of a ranged shot. This is where you place Ranged Attack or Ranged 
	Damage gear
--]]

	['Midshot'] = {
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
	The second stage is Midcast. This is where you'll want to equip magic attack, or magic enhancing 
	gear. (Magic Attack Bonus also happens here, but is broken out into it's own gear set. See MAB.)
--]]	

	['Midcast'] = {
	},

--[[
	Further, there is a break out for each type of spell. I've included a comment on the type of attributes
	the piece of gear should have. While the spell might have other attributes than those listed, the ones I have
	listed have gear that a PLD or anyone can wear.
--]]

	-- Healing: Healing Magic Skill, cure potency. Currently only a Healing Earring affects healing spells from 
	-- a sub job. No other gear gives bonuses to Healing magic from a sub job. Also, gear with MND bonuses will 
	-- boost cure spell's potency, but MND gear is automatically equipped prior to the Healing set being equipped 
	-- in the HandleMidcast function. There's no need to include MND gear here. As to items that add cure potency 
	-- directly there are a few pieces both for pld and "all jobs". So, include healing magic skill items and 
	--cure potency items here.
	['Healing'] = {
    },
	
	-- Dark: Dark Magic Skill. Currently only gear equippable by any job gives is applicable here. There's no gear that's 
	-- specific for PLD that gives any dark magic skill.	
	['Dark'] = {
    },
	
	-- Divine: Divine Magic Skill.
	['Divine'] = {
	},
	
	-- Enfeebling: Enfeebling Magic Skill.
	['Enfeebling'] = {
	},
	
	-- Enhancing: Enhancing Magic Skill. There is no gear that a PLD can wear to enhance any magic spell. 
	-- Leave the Enhancing gear sets empty.
	['Enhancing'] = {
	},
	
	-- Elemental: Elemental Magic Skill. Note: don't include elemental staves or elemental obis/gorgets here, 
	-- that is done automatically in the HandlePrecast/HandleMidcast functions (if /wswap is enabled).
	['Elemental'] = {
	},

	-- Ninjitsu: There is no gear that a PLD can wear to add Ninjitsu skill. Leave the following two
	-- gear sets empty.	
	['Ninjitsu'] = {
	},
	
	-- Summoning: Summoning Magic Skill and Avatar Perpetuation Cost. Currently only gear equippable by any job gives
	-- is applicable here. There's no gear that's specific for PLD that gives any summoning skill. Note: currently on 
	-- HorizonXI summoning skills are ignored. Any gear piece that only gives summoning skill will be commented out		
	['Summoning'] = {
	},
	
--[[
	Next is stat-based gear for spells: intelligence (INT) and mind (MND). Tank_INT and Tank_MND are subsets of the 
	simalarly names gear sets. The intent is to boost the appropriate attribute-based skill/spell without compromising
	your tanking gear.
--]]

	['INT'] = {
    },
	
	['Tank_INT'] = {
	},
	
	['MND'] = {
    },
	
	['Tank_MND'] = {
	},
	
--[[
	Some spells are special cases, so they require tailored gears sets.
--]]

	-- Stoneskin: Stoneskin Enhancement, Mind, and Enhancing Magic Skill. Mind is 3x more important than enhancing
	-- Magic Skill. The only pieces of gear a PLD can wear to enhance stoneskin is a Stone Gorget and Stone Mufflers. 
	-- There's no gear that a PLD (or any job) can wear to enhance magic. Note: This gear set has no effect on 
	-- Titan's Stoneskin blood pact.
	['Stoneskin'] = {
	},	
	
	-- Drain: Drain Enhancement, Dark Magic Skill, Dark Magic Accuracy. Currently no gear supports Drain enhancement.
	-- Drain is part of Dark Magic, so Potency which is based on dark magic skill will already be loaded in HandleMidcast 
	-- function and need not be repeated here. No current gear supports dark magic accuracy for any job. Magic attack 
	-- bonus and magic critical hit have no effect on potency. Leave the two Drain gear sets empty.
	['Drain'] = {
    },
	
	-- Aspir: Aspir Enhancement, Dark Magic Skill, Dark Magic Accuracy. Currently no gear equippable by a
	-- PLD enhances Aspir. Aspir is part of Dark Magic, so potency which is based on dark magic skill will
	-- already be loaded in HandleMidcast function and need not be repeated here. No current gear supports
	-- dark magic accuracy for any job. Magic attack bonus and magic critical hit have no effect on potency.
	-- Leave the two Aspir gear sets empty.
	['Aspir'] = {
    },
	
	-- Sneak: Enhances Sneak and Enhances Stealth. Currently on Dream Boots +1 enhances sneak and is equippable
	-- by any job. (Attained through the Starlight Celebration.) No gear for any job supports Enhances Stealth
	-- yet.
	['Sneak'] = {
		Feet = 'Dream Boots +1',
	},
	
	-- Invisible: Enhances Invisible Effect. Currently only Dream Mittens +1 enhances invisible and is equippable
	-- by any job. (Attained through the Starlight Celebration.)	
	['Invisible'] = {
		Hands = 'Dream Mittens +1',
	},
	
	-- Note: Phalanx does have gear that supports the spell, but it is out of era

--[[
	The following weapon skill gearsets are defined by the stat they emphasize. Listed are all of the sets that
	you will need to use every weapon skill that your job can do. The leading comment defines what weapon/weapon
	skill combination the set applies to.
	
	PLD can use the following weapons: Sword (A+), Club (A-), Staff (A-), Great Sword (B), Dagger (C-), Polearm (E).
	Any other weapon will have no weaponskill available. Weapon skill sets are named based on stat(s) used, regardless 
	of weapon
--]]

--[[	
		* Strength based or just skill based *

		Sword: Flat Blade,Circle Blade,Spirits Within,Vorpal Blade		
		Club: Starlight,Brainshaker,Moonlight,Skullbreaker,True Strike
		Staff: Heavy Swing,Shell Crusher,Full Swing
		Great Sword: Hard Slash,Crescent Moon
		Polearm: Double Thrust,Leg Sweep
-]]
	
	['WS_STR'] = {
    },

--[[
		* Strength and Agility based, even weighting *
		
		Great Sword: Sickle Moon
		Polearm: Vorpal Thrust
--]]

	['WS_STRAGI'] = {
    },
	
--[[
		* Strength and Dexterity based, even weighting *
		
		Sword: Fast Blade
		Polearm: Penta Thrust
--]]

	['WS_STRDEX'] = {
    },

--[[
		* Strength and Intelligence based, even weighting *
		
		Sword: Burning Blade
		Staff: Rock Crusher,Earth Crusher,Cataclysm
		Great Sword: Frostbite,Freezebite,Spinning Slash,Ground Strike
		Polearm: Thunder Thrust,Raiden Thrust
--]]
	
	['WS_STRINT'] = {
    },
--[[
		* Strength and Intelligence based, 30%/20% respectively *
		
		Sword: Red Lotus Blade
--]]
	
	['WS_STRINT_30_20'] = {
    },

--[[
		* Strength and Mind based, even weighting *
		
		Sword: Shining Blade,Seraph Blade,Swift Blade,Savage Blade,Knights of Round
		Club: Shining Strike,Seraph Strike,Judgement,Black Halo,Randgrith
		Staff: Starburst,Sunburst,Retribution
		Great Sword: Shockwave,
--]]

	['WS_STRMND'] = {
    },

--[[
		* Strength and Vitality based, even weighting *
		
		Great Sword: Power Slash,Scourge
--]]
	
	['WS_STRVIT'] = {
    },

--[[
		* Charisma based *
		
		Dagger: Shadowstitch
--]]
	
	['WS_CHR'] = {
    },
	
--[[
		* Dexterity based *
		
		Dagger: Wasp Sting
--]]
	
	['WS_DEX'] = {
    },

--[[
		* Dexterity and Charisma based *
		
		Dagger: Dancing Edge
--]]
	
	['WS_DEXCHR'] = {
    },
	
--[[
		* Dexterity and Intelligence based *
		
		Dagger: Gust Slash
--]]
	
	['WS_DEXINT'] = {
    },

--[[
		* Intelligence based *
		
		Staff: Gate of Tartarus
--]]
	
	['WS_INT'] = {
    },
	
--[[
		* Intelligence and Mind based *
		
		Staff: Spirit Taker
--]]
	
	['WS_DEXMND'] = {
    },
	
--[[
		* Mind based *

		Dagger: Energy Steal
--]]

	['WS_MND'] = {
    },

--[[
		* Skill based *
		
		Club: Starlight,Moonlight
--]]

	['WS_Skill'] = {
    },
	
--[[
		* HP based *
		
		Sword: Spirits Within
--]]

	['WS_HP'] = {
    },
	
--[[
	Movement tends to be used for kiting. Emphasis should be placed on gear that increases movement speed, but you 
	might also want gear that has evasion. The choice is yours.
--]]

	['Movement'] = { 
	},
	
--[[
	The following are abilities affected by gear. Please note that currently there's no gear
	that affects Chivalry.
--]]

	['HolyCircle'] = {
    },
	
	['ShieldBash'] = {
    },
	
	['Sentinel'] = {
    },

	['Cover'] = {
    },

	['Rampart'] = {
    },
		
--[[
	Some subjobs really make no sense when combined with paladin, but all abilities across all jobs that
	have gear that can be equipped by a PLD are included here.
	
	The following sub jobs have no skills with equippable gear by a PLD: WAR,THF,BLM,MNK,WHM,RDM,RNG,NIN,
	SMN,BRD,SAM
--]]
	--* BST *--
	['Charm'] = {		-- CHR and Charm + gear. (Every +1 Charm adds 5% Charm duration)
    },
	
	['Pet_Attack'] = {					-- Pet's strength, not accuracy
	},
	
	['Pet_Macc'] = {					-- Pet's Magical Accuracy
	},
	
	['Pet_Matt'] = {					-- Pet's Magical Attack
	},
	
	--* /DRK *--
	['WeaponBash'] = {
	},
	
	--* /DRG *--
	['Jumps'] = {		-- Jump and High Jump, Super is too high a level
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

profile.Sets = sets;
profile.sjb = nil;
profile.bAmmo = false;
profile.sAmmo = nil;

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
	elseif (gcinclude.BstPetMagicAttack:contains(PetAction.Name)) then		-- Pet Magical Attack
		gcinclude.MoveToCurrent(sets.Pet_Matt,sets.CurrentGear);
	elseif (gcinclude.BstPetMagicAccuracy:contains(PetAction.Name)) then	-- Pet Magical Accuracy Attack
		gcinclude.MoveToCurrent(sets.Pet_Macc,sets.CurrentGear);
    end
	gcinclude.EquipTheGear(sets.CurrentGear);
end

--[[
	SetSubjobSet is used to pick the appropriate set for the loaded macrobook based on
	which subjob is current. (If no change has occurred since the last time it was called,
	nothing is checked/changed.)
--]]

local function SetSubjobSet(chkSJ)
	local subs = {['WAR'] = 1, ['MNK'] = 0, ['WHM'] = 1, ['BLM'] = 1, ['RDM'] = 1, ['THF'] = 1,
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
	OnLoad is run whenever you log into your PLD or change your job to PLD
--]]

profile.OnLoad = function()
	local player = gData.GetPlayer();

	gSettings.AllowAddSet = true;
	gcinclude.Initialize();
	gcinclude.settings.RegenGearHPP = 50;
    gcinclude.settings.RefreshGearMPP = 60;
	gcdisplay.SetToggle('Tank',true);		-- Assume PLD is a tank

	-- Coded order of operation override
	gcinclude.settings.priorityEngaged = 'CEFGH';
	gcinclude.settings.priorityMidCast = 'ABCDEFGH';
	gcinclude.settings.priorityWeaponSkill = 'ABDE';
	
	-- Set your job macro toolbar defaults here. Which set depends on the subjob
	AshitaCore:GetChatManager():QueueCommand(1, '/macro book 9');		-- PLD
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
end

--[[
	OnUnload is run when you change to another job
--]]

profile.OnUnload = function()
	gcinclude.Unload();
end

--[[
	HandleCommand is run when you type in a command defined in LUASHITACAST. The commands handled here instead
	of in gcinclude.HandleCommands are specific to PLD or the help system.
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
	their pet.
--]]
	
profile.HandleDefault = function()
	local pet = gData.GetPet();
	local petAction = gData.GetPetAction();
	local player = gData.GetPlayer();
	local zone = gData.GetEnvironment();
	local ew = gData.GetEquipment();
	local eWeap = nil;
	local cKey,sGear;
	local bTank = gcdisplay.GetToggle('Tank');
	
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
	
	-- Clear out the CurrentGear in case of leftovers
	gcinclude.ClearSet(sets.CurrentGear);
	
	-- If player is not resting and has MP and has swapped weapons, set the weapon back to what 
	-- they had before the switch
	if player.Status ~= 'Resting' and gcdisplay.GetToggle('WSwap') == true then
		if gcinclude.weapon ~= nil and eWeap ~= gcinclude.weapon then
			sets.CurrentGear['Main'] = gcinclude.weapon;
			sets.CurrentGear['Sub'] = gcinclude.offhand;	
		end
	end

	-- The default set is the TP gear set, but if a tanking set is indicated, That
	-- should take priority.
	gcinclude.MoveToCurrent(sets.TP,sets.CurrentGear);
	
	if bTank == true then
		gcinclude.MoveToCurrent(sets.TP_Tank,sets.CurrentGear);	
	end
		
	-- Now process the player status accordingly
	gcdisplay.SetLocksAction(gcinclude.LocksNumeric,player.Status);		
	if player ~= nil and player.Status == 'Engaged' then
		gcinclude.settings.priorityEngaged = string.upper(gcinclude.settings.priorityEngaged);
		for i = 1,string.len(gcinclude.settings.priorityEngaged),1 do
			cKey = string.sub(gcinclude.settings.priorityEngaged,i,i);
			if cKey == 'C' then		-- Evasion	
				-- The player should have priority over any pets
				if gcdisplay.GetToggle('Eva') == true then
					if bTank == true then
						gcinclude.MoveToCurrent(sets.Tank_Evasion,sets.CurrentGear);
					else
						gcinclude.MoveToCurrent(sets.Evasion,sets.CurrentGear);
					end
				end
			elseif cKey == 'E' then		-- Accuracy	
				-- The player should have priority over any pets
				if gcdisplay.GetToggle('Acc') == true then 
					if bTank == true then
						gcinclude.MoveToCurrent(sets.Tank_Accuracy,sets.CurrentGear);
					else
						gcinclude.MoveToCurrent(sets.Accuracy,sets.CurrentGear);
					end
				end
			elseif cKey == 'F' then		-- Kiting
				if (gcdisplay.GetToggle('Kite') == true) then
					gcinclude.MoveToCurrent(sets.Movement,sets.CurrentGear);
				end	
			elseif cKey == 'G' then		-- common buffs/debuffs
				gcinclude.CheckCommonDebuffs(sets.CurrentGear);	
			elseif cKey == 'H' then		-- Damage Taken gear
				if (gcdisplay.GetCycle('DT') ~= gcinclude.OFF) then
					if gcdisplay.GetCycle('DT') == 'Physical' then
						gcinclude.MoveToCurrent(sets.DT_Physical,sets.CurrentGear);
					elseif gcdisplay.GetCycle('DT') == 'Magical' then
						gcinclude.MoveToCurrent(sets.DT_Magical,sets.CurrentGear);
					elseif gcdisplay.GetCycle('DT') == 'Breath' then
						gcinclude.MoveToCurrent(sets.DT_Breath,sets.CurrentGear);
					end
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
			gcinclude.SwapToStave('dark',false,sets.CurrentGear);			
		end

		-- Check for common debuffs
		gcinclude.CheckCommonDebuffs(sets.CurrentGear);
	else
		-- Assume idling. Priority (low to high): regen,refresh

		-- See if in a town
		if (zone.Area ~= nil and table.find(gcinclude.Towns,zone.Area)) then
			gcinclude.MoveToCurrent(sets.Town,sets.CurrentGear);
		else
			if gcdisplay.GetToggle('Idle') == true then
				gcinclude.MoveToCurrent(sets.Travel,sets.CurrentGear);
			
				-- if the player's HP is below the threshold setting, equip the idle regen gear
				if player.HPP < gcinclude.settings.RegenGearHPP then
					gcinclude.MoveToCurrent(sets.Idle_Regen,sets.CurrentGear);
				end
			
				-- if the player's MP is below the threshold setting, equip the idle refresh gear
				if player.MPP < gcinclude.settings.RefreshGearMPP then
					gcinclude.MoveToCurrent(sets.Idle_Refresh,sets.CurrentGear);
				end		
			
				-- Check for common debuffs
				gcinclude.CheckCommonDebuffs(sets.CurrentGear);
			end
		end
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

	-- Clear out the CurrentGear in case of leftovers
	gcinclude.ClearSet(sets.CurrentGear);
	
	-- Now process the appropriate job ability. Start with abilities associated with PLD
	if string.match(ability.Name, 'Holy Circle') then
		gcinclude.MoveToCurrent(sets.HolyCircle,sets.CurrentGear);
	elseif string.match(ability.Name, 'Shield Bash') then
		gcinclude.MoveToCurrent(sets.ShieldBash,sets.CurrentGear);
	elseif string.contains(ability.Name, 'Sentinel') then
		gcinclude.MoveToCurrent(sets.Sentinel,sets.CurrentGear);	
	elseif string.contains(ability.Name, 'Cover') then
		gcinclude.MoveToCurrent(sets.Cover,sets.CurrentGear);	
	elseif string.contains(ability.Name, 'Rampart') then
		gcinclude.MoveToCurrent(sets.Rampart,sets.CurrentGear);	

	-- And now the subjob abilities
	elseif string.contains(ability.Name, 'Charm') then			-- assumes /bst	
		gcinclude.MoveToCurrent(sets.Charm,sets.CurrentGear);
		gcinclude.SwapToStave('light',false,sets.CurrentGear);
	elseif string.match(ability.Name, 'Weapon Bash') then		-- assumes /drk
		gcinclude.MoveToCurrent(sets.WeaponBash,sets.CurrentGear);
	elseif string.contains(ability.Name, 'Jump') then			-- assumes /drg
		gcinclude.MoveToCurrent(sets.Jumps,sets.CurrentGear);		
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
		
	-- See if an elemental obi should be equipped
	obi = gcinclude.CheckEleSpells(spell.Name,gcinclude.MagicEleAcc,gcinclude.OBI,nil);
	if obi ~= nil then
		sets.CurrentGear['Waist'] = obi;
	end
	gcinclude.EquipTheGear(sets.CurrentGear);
end

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
	
	-- Call the common HandleMidcast now
	gcinclude.HandleMidcast(bTank);
	
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
	local bTank = gcdisplay.GetToggle('Tank');
	
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
	gcinclude.HandleWeaponskill(bTank);
	
	-- Equip the composited weaponskill set		
	gcinclude.EquipTheGear(sets.CurrentGear);
end		-- HandleWeaponskill

return profile;