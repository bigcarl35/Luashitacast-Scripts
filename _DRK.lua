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
	The Idle sets are for when you're not in town and not doing anything like fighting (or pet fighting), casting, 
	resting, etc. "Idle" is the default setting and "_Refresh" and "_Regen" are when you want to emphasize the
	aforementioned ability.
	
	Regardless, do not include anything from the first row of equipment (Main, Sub, Ranged, Ammo). Doing so you
	will find yourself fighting luashitacast (like when equiping a dark staff when you are resting.)
	
	Most gear sets have an associated conditional set. These are for supporting items that give stat bonuses when
	a certain condition is met (eg., at nighttime or when there's a full moon.) You will find all the items supported
	in the "Conditional gear master list.txt" file which is also found in the /common/ directory. Just copy and paste
	the line item you want in the appropriate conditional area. (Please note that if you have more than one item to
	be conditionally considered, you will have to add a comma after each entry.
--]]

	['Idle'] = {
        Head = 'Empress Hairpin',
        Neck = 'Spike Necklace',
        Ear1 = 'Beetle Earring',
        Ear2 = 'Beetle Earring',
        Body = 'Wonder Kaftan',
        Hands = 'Wonder Mitts',
        Ring1 = 'Tamas Ring',
        Ring2 = 'Balance Ring',
        Waist = 'Mrc.Cpt. Belt',
        Legs = 'Wonder Braccae',
        Feet = 'Bounding Boots',
    },
	['Idle_Conditional'] = {
	},
	
	--[[
		The Idle_Regen and Idle_Refresh gear sets replace the normal Idle set when the player's HP or MP
		go below a set percentage, accordingly. For HP it is 60% and for MP it is 70%. These percentages
		are defined in gcinclude.lua.
	--]]
	
	['Idle_Regen'] = {
	},
	
	['Idle_Refresh'] = {
	},
	
	--[[
		Resting emphasizes either HP regen or MP refresh. Regen is the assumed target when resting
	--]]
	
	['Resting'] = { 
	},
	['Resting_Conditional'] = {
	},
	
	['Resting_Refresh'] = {
	},
	['Resting_Refresh_Conditional'] = {
	},
	
--[[
	Start weapons are where you define what you want the first row of equipment to look like when you
	either log in as a BST or you switch your main job to BST. Any other gear you mention will be overridden
	by the Idle or Town set, so no need to include here.
--]]

	['Start_Weapons'] = {
        Main = 'Windurstian Sword',
        Ammo = 'Happy Egg',
    },
	['Start_Weapons_Conditional'] = {
	},
	
--[[
	What do you want to wear around town? You can define a full set or just an item or two, it is up to you.
	(Please note that a nation's aketon is considered conditional gear, so no need to place here unless you
	want the aketon equipped regardless if it is your home nation's city or not.)
--]]
	
	['Town'] = {
        Head = 'Lilac Corsage',
    },
	
--[[
	Damage reduction gear depends on the type of damage. The most common is physical, but there's times when
	you'll want to reduce magic damage or breath damage. The three gear sets are defined below. The correct
	one will be loaded depending if DT is indicated and the TYPE selected. Please consider not including gear
	that doesn't have any damage taken property so other wanted stats can shine through
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
	
	['DT_Evasion'] = {
	},
	['DT_Evasion_Conditional'] = {
	},
	
--[[
		The TP sets are used when you are fighting. The accuracy set will be used if ACC is specified
		and the evasion set if EVA is specified.
--]]

	['TP'] = {
        Head = 'Empress Hairpin',
        Neck = 'Spike Necklace',
        Ear1 = 'Beetle Earring',
        Ear2 = 'Beetle Earring',
        Body = 'Wonder Kaftan',
        Hands = 'Wonder Mitts',
        Ring1 = 'Courage Ring',
        Ring2 = 'Balance Ring',
        Waist = 'Mrc.Cpt. Belt',
        Legs = 'Wonder Braccae',
        Feet = 'Bounding Boots',
    },
	['TP_Conditional'] = {
	},
	
	['TP_Accuracy'] = {
        Ring1 = 'Jaeger Ring',
        Ring2 = 'Balance Ring',
        Waist = 'Mrc.Cpt. Belt',
        Feet = 'Bounding Boots',
    },
	['TP_Accuracy_Conditional'] = {
	},
	
	['TP_Evasion'] = {
        Head = 'Empress Hairpin',
        Ring2 = 'Balance Ring',
        Waist = 'Mrc.Cpt. Belt',
        Legs = 'San. Trousers',
        Feet = 'Bounding Boots',
    },
	['TP_Evasion_Conditional'] = {
	},
	
--[[
	The following are abilities affected by gear
--]]

	['ArcaneCircle'] = {
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
	Further, there is a break out for each type of spell you can cast. For example, if you are
	doing a Cure, you wand mind (MND) gear, etc.
--]]

	['Cure'] = {				-- Healing Magic Skill, Cure Potency
    },
	['Cure_Conditional'] = {
	},
	
	['Dark'] = {
        Ring1 = 'Tamas Ring',
    },
	['Dark_Conditional'] = {
	},
	
	['Divine'] = {				-- Divine Magic Skill
	},
	['Divine_Conditional'] = {
	},
	
	['Enfeebling'] = {	 		-- Enfeebling Magic Skill
	},
	['Enfeebling_Conditional'] = {
	},
	
	['Enhancing'] = {	 		-- Enhancing Magic Skill, enhancing magic time reduction, enhancing magic duration
	},
	['Enhancing_Conditional'] = {
	},
	
	['Elemental'] = {			-- Elemental Magic Skill, magic accuracy, elemental magic casting time reduction, elemental magic recast time reduction, magic burst bonus, magic attack bonus
	},
	['Elemental_Conditional'] = {
	},
	
	['Ninjitsu'] = {			-- Ninjitsu Skill, magic burst bonus, magic attack bonus
	},
	['Ninjitsu_Conditional'] = {
	},
	
	['Summoning'] = {			-- Summoning Skill, spell interruption rate, summoning magic casting time reduction
	},
	['Summoning_Conditional'] = {
	},

	-- Spell interuption rate
	['SIR'] = {
	},
	['SIR_Conditional'] = {
	},
	
--[[
	Next is stat-based gear, (in this case intelligence or mind)
--]]

	['INT'] = {
        Ring1 = 'Tamas Ring',
        Ring2 = 'Windurstian Ring',
        Waist = 'Mrc.Cpt. Belt',
        Feet = 'Mannequin Pumps',
    },
	['INT_Conditional'] = {
	},
	
	['MND'] = {
        Neck = 'Justice Badge',
        Body = 'Wonder Kaftan',
        Ring1 = 'Tamas Ring',
        Ring2 = 'Tranquility Ring',
        Waist = 'Mrc.Cpt. Belt',
        Legs = 'Wonder Braccae',
        Feet = 'Mannequin Pumps',
    },
	['MND_Conditional'] = {
	},
	
--[[
	Magic accuracy gear
--]]

	['macc'] = {
        Ring1 = 'Tamas Ring',
    },
	['macc_Conditional'] = {
	},
	
--[[
	And some spells are special cases, so they have individual gears sets.
--]]
	-- Phalanx: Enhancing Magic Skill
	['Phalanx'] = {
	},
	['Phalanx_Conditional'] = {
	},
	
	-- Combination of MND and enhancing skill. MND is 3x more important. There's also gear that enhances
	['Stoneskin'] = {	
	},	
	['Stoneskin_Conditional'] = {
	},
	
	-- Refresh gear, refresh duration gear
	['Refresh'] = { 
	},
	['Refresh_Conditional'] = {
	},
	
	-- Potency of dread spikes depends on how much HP you have during cast
    ['DreadSpikes'] = {
    },
	['DreadSpikes_Conditional'] = {
	},
	
	-- Potency of drain depends on your dark magic skill, but a drain set is needed to include drain enhancing gear
	['Drain'] = {
    },
	['Drain_Conditional'] = {
	},
	
--[[
		DRK can use the following weapons: scythe (A+), great sword (A-), axe (B-), great axe (B-), sword (B-), dagger (C), club(C-).
		Any other weapon will have no weaponskill available. Weapon skill sets are named based on stat(s) used, regardless of weapon

		* Strength based or just skill based *
		Scythe: Slice,Spinning Scythe,Vorpal Scythe
		Great Sword: Hard Slash,Crescent Moon
		Axe: Raging Axe,Smash Axe,Gale Axe,Avalanche Axe,Spinning Axe^,Rampage,Decimation^
		Great Axe: Iron Tempest,Sturmwind^^,Keen Edge,Raging Rush
		Sword: Flat Blade,Circle Blade,Spirits Within,Vorpal Blade
		Club: Starlight,Brainshaker,Moonlight,Skullbreaker,True Strike
		
		^ Main or Sub must be WAR, DRK, or BST
		^^ Main or Sub must be WAR, DRK or RUN
-]]
	
	['WS_STR'] = {
        Neck = 'Spike Necklace',
        Body = 'Wonder Kaftan',
        Hands = 'Wonder Mitts',
        Ring1 = 'Courage Ring',
        Ring2 = 'San d\'Orian Ring',
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
        Ear2 = 'Drone Earring',
        Body = 'Wonder Kaftan',
        Hands = 'Wonder Mitts',
        Ring1 = 'Courage Ring',
        Ring2 = 'San d\'Orian Ring',
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
        Ear2 = 'Drone Earring',
        Body = 'Wonder Kaftan',
        Hands = 'Wonder Mitts',
        Ring1 = 'Courage Ring',
        Ring2 = 'Balance Ring',
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
        Neck = 'Spike Necklace',
        Ear2 = 'Drone Earring',
        Body = 'Wonder Kaftan',
        Hands = 'Wonder Mitts',
        Ring1 = 'Courage Ring',
        Ring2 = 'Tamas Ring',
        Waist = 'Mrc.Cpt. Belt',
        Legs = 'Wonder Braccae',
        Feet = 'Wonder Clomps',
    },
	['WS_STRINT_Conditional'] = {
	},

--[[
		* Strength and Intelligence based, 30%/20% respectively *
		Sword: Red Lotus Blade
--]]
	
	['WS_STRINT_30_20'] = {
        Neck = 'Spike Necklace',
        Ear2 = 'Drone Earring',
        Body = 'Wonder Kaftan',
        Hands = 'Wonder Mitts',
        Ring1 = 'Courage Ring',
        Ring2 = 'Tamas Ring',
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
        Ring1 = 'Courage Ring',
        Ring2 = 'Tamas Ring',
        Waist = 'Mrc.Cpt. Belt',
        Legs = 'Wonder Braccae',
        Feet = 'Wonder Clomps',
    },
	['WS_STRMND_Conditional'] = {
	},

--[[
		* Strength and Vitality based, even weighting *
		Great Sword: Power Slash,Scourge
		Great Axe: Shield Break,Armor Break,Weapon Break,Steel Cyclone^^
		Sword: Shining Blade,Seraph Blade,Swift Blade,Savage Blade
		
		^^ WAR, DRK or RUN only
--]]
	
	['WS_STRVIT'] = {
        Neck = 'Spike Necklace',
        Body = 'Wonder Kaftan',
        Hands = 'Wonder Mitts',
        Ring1 = 'Courage Ring',
        Ring2 = 'Bastokan Ring',
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
        Ring1 = 'Balance Ring',
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
        Ring1 = 'Balance Ring',
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
        Ring1 = 'Tranquility Ring',
        Ring2 = 'Tamas Ring',
        Waist = 'Mrc.Cpt. Belt',
        Legs = 'Wonder Braccae',
    },
	['WS_MND_Conditional'] = {
	},
	
--[[
	Weapon skill sets don't consider accuracy in their definitions. If an accuracy emphasis is
	desired, the following set will replace the gear from the appropriate weapon skill set.
	(Please note: only include accuracy gear here so that the weapon skill set has some emphasis
	on the appropriate gear set loaded.
--]]

	['WS_Accuracy'] = {
        Ring1 = 'Jaeger Ring',
    },	
	['WS_Accuracy_Conditional'] = {
	},
	
--[[
	While it's possible that the player's subjob will be pet-based (/BST,/SMN,/DRG), minimal gear set
	support is provided.
--]]
	
	-- CHR and Charm + gear. (Every +1 Charm adds 5% Charm duration)
	['Charm'] = {
    },
	['Charm_Conditional'] = {
	},
	
	['Charm_Evasion'] = {
	},
	['Charm_Evasion_Conditional'] = {
	},
	
--[[
	Movement tends to be used for kiting. Emphasis should be placed on gear that increases movement speed, but you 
	might also want gear that has evasion. The choice is yours.
--]]

	-- Movement speed gear, does not include nation aketons which are found in conditional gear for home town
	['Movement'] = { 
	},
	['Movement_Conditional'] = {
	},

--[[
	Treasure Hunter gear
--]]

	['TH'] = {
	},
	['TH_Conditional'] = {
	},
	
--[[
	The following sets are added as a convenience for playing in level capped areas. The only way for them to be loaded
	is via the /gearset command, which will turn GSwap off. If you're level syncing, pick the set that's closest to the
	sync level and adjust accordingly. 
--]]

	['CAP20'] = {
        Head = 'Silver Hairpin',
        Neck = 'Spike Necklace',
        Ear1 = 'Beetle Earring',
        Ear2 = 'Beetle Earring',
        Body = 'Angler\'s Tunica',
        Hands = 'Wonder Mitts',
        Ring1 = 'Courage Ring',
        Ring2 = 'Balance Ring',
        Waist = 'Barbarian\'s Belt',
        Legs = 'Ryl.Ftm. Trousers',
        Feet = 'Bounding Boots',
    },
	
	['CAP25'] = {
        Head = 'Empress Hairpin',
        Neck = 'Spike Necklace',
        Ear1 = 'Beetle Earring',
        Ear2 = 'Beetle Earring',
        Body = 'Beetle Harness',
        Hands = 'Ryl.Ftm. Gloves',
        Ring1 = 'Courage Ring',
        Ring2 = 'Balance Ring',
        Waist = 'Barbarian\'s Belt',
        Legs = 'San. Trousers',
        Feet = 'Bounding Boots',
    },
	
	['CAP30'] = {
        Head = 'Empress Hairpin',
        Neck = 'Spike Necklace',
        Ear1 = 'Beetle Earring',
        Ear2 = 'Beetle Earring',
        Body = 'Beetle Harness',
        Hands = 'Wonder Mitts',
        Ring1 = 'Tamas Ring',
        Ring2 = 'Balance Ring',
        Waist = 'Mrc.Cpt. Belt',
        Legs = 'San. Trousers',
        Feet = 'Bounding Boots',
    },
	
	['CAP40'] = {
    },
	
	['CAP50'] = {
    },

	['CAP60'] = {
    },
};

profile.Sets = sets;
profile.sjb = nil;
profile.bAmmo = false;
profile.sAmmo = nil;

--[[
	SetSubjobSet is used to pick the appropriate set for the loaded macrobook based on
	which subjob is current. (If no change has occurred since the last time it was called,
	nothing is checked/changed.)
--]]

local function SetSubjobSet(chkSJ)
	local subs = {['WAR'] = 2, ['MNK'] = 0, ['WHM'] = 2, ['BLM'] = 3, ['RDM'] = 2, ['THF'] = 3,
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
	
	-- Determine if subjob uses magic and if the maximum MP is > 50.
	gcinclude.CheckMagic50(player);
	
	-- Set your job macro toolbar defaults here. Which set depends on the subjob
	AshitaCore:GetChatManager():QueueCommand(1, '/macro book 9');		-- DRK
	SetSubjobSet(player.SubJob);
	
	-- Load up the weapons bar. (This need only be done once.)
	if gcdisplay.GetToggle('GSwap') == true then		-- Only gear swap if this flag is true	
		gFunc.EquipSet(sets.Start_Weapons);	
		gcinclude.ProcessConditional(sets.Start_Weapon_Conditional,nil);
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
	
	if gcdisplay.GetToggle('GSwap') == false then		-- Only gear swap if this flag is true
		return;
	end
	
	local player = gData.GetPlayer();
	local ew = gData.GetEquipment();
	local eWeap = nil;

	if ew['Main'] ~= nil then
		eWeap = ew['Main'].Name;
	end;
		
	SetSubjobSet(player.SubJob);			-- Make sure the correct set is shown in case the subjob was changed.
	
	-- If player is not resting and has MP and has swapped weapons, set the weapon back to what 
	-- they had before the switch
	if player.Status ~= 'Resting' and gcdisplay.GetToggle('WSwap') == true then
		if gcinclude.weapon ~= nil and eWeap ~= gcinclude.weapon then
			gFunc.ForceEquip('Main', gcinclude.weapon);	
			gFunc.ForceEquip('Sub', gcinclude.offhand);	
		end
	end
		
	-- Now process the player status accordingly
	if player.Status == 'Engaged' then		-- Player is fighting. Priority (low to high): TP,evasion,accuracy
		gFunc.EquipSet(sets.TP);
		gcinclude.ProcessConditional(sets.TP_Conditional,nil);
		if gcdisplay.GetToggle('Eva') == true then
			gFunc.EquipSet(sets.TP_Evasion);
			gcinclude.ProcessConditional(sets.TP_Evasion_Conditional,nil);
		end
		if gcdisplay.GetToggle('Acc') == true then 
			gFunc.EquipSet(sets.TP_Accuracy);
			gcinclude.ProcessConditional(sets.TP_Accuracy_Conditional,nil);
		end
	elseif player.Status == 'Resting' then	-- Player kneeling. Priority (low to high): Resting,refresh
		gFunc.EquipSet(sets.Resting);
		gcinclude.ProcessConditional(sets.Resting_Conditional,nil);
		if (gcinclude.settings.bMagic and player.MPP < gcinclude.settings.RefreshGearMPP) then
			gFunc.EquipSet(sets.Resting_Refresh);
			gcinclude.ProcessConditional(sets.Resting_Refresh_Conditional,nil);
		end
		-- Weapon swap to a higher MP refresh while healing weapon if appropriate.
		if gcdisplay.GetToggle('WSwap') == true and player.MP < player.MaxMP then
			gcinclude.SwapToStave('dark',false);
		end
	else									-- Assume idling. Priority (low to high): Idle,refresh
		gFunc.EquipSet(sets.Idle);
		gcinclude.ProcessConditional(sets.Idle_Conditional,nil);
		if player.HPP < gcinclude.settings.RegenGearHPP then		-- if the player's HP is below the threshold setting, equip the idle regen gear
			gFunc.EquipSet(sets.Idle_Regen);
		end
		if profile.bMagic == true then				-- Magic subjob
			if player.MPP < gcinclude.settings.RefreshGearMPP then		-- if the player's MP is below the threshold setting, equip the idle refresh gear
				gFunc.EquipSet(sets.Idle_Refresh);
			end
		end
	end
	
	-- If player has indicated kiting, load movement gear set
	if (gcdisplay.GetToggle('Kite') == true) then
		gFunc.EquipSet(sets.Movement);
		gcinclude.ProcessConditional(sets.Movement_Conditional,nil);
	end
		
	gcinclude.CheckDefault ();
		
	-- Add TH gear if indicated
	if (gcdisplay.GetToggle('TH') == true) then
		gFunc.EquipSet(sets.TH);
		gcinclude.ProcessConditional(sets.TH_Conditional,nil);
	end
			
	-- Lastly, equip the appriopriate Damage Taken gear if desired
	if (gcdisplay.GetToggle('DT') == true) then
		gFunc.EquipSet('DT_' .. gcdisplay.GetCycle('DT_Type'));
		if gcdisplay.GetCycle('DT_Type') == gcinclude.PHY then
			gcinclude.ProcessConditional(sets.DT_Physical_Conditional,nil);
		elseif gcdisplay.GetCycle('DT_Type') == gcinclude.MAG then
			gcinclude.ProcessConditional(sets.DT_Magical_Conditional,nil);
		else
			gcinclude.ProcessConditional(sets.DT_Breath_Conditional,nil);
		end
	end
end

--[[
	HandleAbility is used to change the player's gear appropriately.
--]]

profile.HandleAbility = function()
	local ability = gData.GetAction();
	
	if gcdisplay.GetToggle('GSwap') == false then
		return;
	end
	
	if string.match(ability.Name, 'Arcane Circle') then
		gFunc.EquipSet(sets.ArcaneCircle);
		gcinclude.ProcessConditional(sets.ArcaneCircle_Conditional,nil);
	elseif string.match(ability.Name, 'Last Resort') then
		gFunc.EquipSet(sets.LastResort);
		gcinclude.ProcessConditional(sets.LastResort_Conditional,nil);	
	elseif string.match(ability.Name, 'Souleater') then
		gFunc.EquipSet(sets.Souleater);
		gcinclude.ProcessConditional(sets.Souleater_Conditional,nil);
	end
end
	
--[[
	HandleItem is the place to equip gear when a special item is used. Currently only 'Holy Water' 
	is supported
--]]

profile.HandleItem = function()
	local item = gData.GetAction();

	if gcdisplay.GetToggle('GSwap') == true then		-- Only gear swap if this flag is true
		if string.match(item.Name, 'Holy Water') then gFunc.EquipSet(gcinclude.sets.Holy_Water) end
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
		gcinclude.ProcessConditional(sets.Precast_Conditional,nil);
		
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
		gcinclude.CheckCancels();
	end
end

--[[
	HandleMidcast loads the appropriate gear for magic skill, duration, mab, macc, and potency
	
	There's an order to how the pieces are loaded: INT/MND, spell specific, macc, magic skill, obi, ele swap
	
--]]

profile.HandleMidcast = function()
	local player = gData.GetPlayer();
	local spell = gData.GetAction();
	local obi;
	local sSet;

	if gcdisplay.GetToggle('GSwap') == false then		-- Only gear swap if this flag is true	
		return;
	end
		
	-- First load the midcast set
	gFunc.EquipSet(sets.Midcast);
	gcinclude.ProcessConditional(sets.Midcast_Conditional,nil);
			
	-- Then the Spell Interruption set
	gFunc.EquipSet(sets.SIR);
	gcinclude.ProcessConditional(sets.SIR_Conditional,nil);
	
	-- Next, see if INT/MND gear should be loaded
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
		
	-- Then check spell specific gear
	if string.match(spell.Name, 'Phalanx') then
		gFunc.EquipSet(sets.Phalanx);
		gcinclude.ProcessConditional(sets.Phalanx_Conditional,nil);
	elseif string.match(spell.Name, 'Stoneskin') then
		gFunc.EquipSet(sets.Stoneskin);
		gcinclude.ProcessConditional(sets.Stoneskin_Conditional,nil);
		-- Stoneskin is heavily affected by MND, but it's an enhancing spell, so
		-- MND gear wasn't loaded above. Additionally, MND is more potent than
		-- Enhancing skill. Load that here.
		gFunc.EquipSet(sets.MND);
		gcinclude.ProcessConditional(sets.MND_Conditional,nil);
	elseif string.contains(spell.Name, 'Refresh') then
		gFunc.EquipSet(sets.Refresh);
		gcinclude.ProcessConditional(sets.Refresh_Conditional,nil);
	elseif string.contains(spell.Name, 'DreadSpikes') then
		gFunc.EquipSet(sets.Dread_Spikes);
		gcinclude.ProcessConditional(sets.DreadSpikes_Conditional,nil);
	elseif string.contains(spell.Name, 'Drain') then
		gFunc.EquipSet(sets.Drain);
		gcinclude.ProcessConditional(sets.Drain_Conditional,nil);
	end

	-- Then magical accuracy
	if gcdisplay.GetToggle('acc') == true then
		gFunc.EquipSet(sets.macc);
		gcinclude.ProcessConditional(sets.macc_Conditional,nil);
	end
		
	-- Then the appropriate magic skill
	mSet = gcinclude.WhichMagicSkill(spell.Name);

	if mSet ~= nil then
		gFunc.EquipSet(mSet);
		if mSet == 'Cure' then
			gcinclude.ProcessConditional(sets.Cure_Conditional,nil);
		elseif mSet == 'Dark' then
			gcinclude.ProcessConditional(sets.Dark_Conditional,nil);
		elseif mSet == 'Divine' then
			gcinclude.ProcessConditional(sets.Divine_Conditional,nil);
		elseif mSet == 'Enfeebling' then
			gcinclude.ProcessConditional(sets.Enfeebling_Conditional,nil);
		elseif mSet == 'Enhancing' then
			gcinclude.ProcessConditional(sets.Enhancing_Conditional,nil);
		elseif mSet == 'Elemental' then
			gcinclude.ProcessConditional(sets.Elemental_Conditional,nil);
		elseif mSet == 'Ninjitsu' then
			gcinclude.ProcessConditional(sets.Ninjitsu_Conditional,nil);
		elseif mSet == 'Summoning' then
			gcinclude.ProcessConditional(sets.Summoning_Conditional,nil);
		end				
	end

--[[		
	Then, regardless of type of spell, see if an obi would help. No need to check and see if the 
	player has the obi or not, if they do, it equips. If not, nothing happens.
		
	Note: This seems like a repeat of the obi check in the precast, but in this case it's checking
	for the spell damage type rather than the spell accuracy.
--]]
	if gcinclude.settings.bEleObis == false then
		gcinclude.CheckForObisGorgets();
	end		
	if gcinclude.settings.bEleObis == true then
		obi = gcinclude.CheckEleSpells(spell.Name,gcinclude.MagicEleDmg,gcinclude.OBI);
		if obi ~= nil then
			gFunc.ForceEquip('Waist',obi);
		end
	end
		
	stat = nil;
	-- Lastly, how about an elemental stave (use the MagicEleDmg in gcinclude) or summons
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
		gcinclude.SwapToStave(stat,false);
	end
end

--[[
	HandlePreshot loads Ranged Accuracy and Ranged Shot Speed Gear for a ranged attack
--]]

profile.HandlePreshot = function()
	if gcdisplay.GetToggle('GSwap') == true then		-- Only gear swap if this flag is true
		gFunc.EquipSet(sets.Preshot);
		gcinclude.ProcessConditional(sets.Preshot_Conditional,nil);
	end
end

--[[
	HandleMidshot loads Ranged Attack and Damage gear for a ranged attack
--]]

profile.HandleMidshot = function()
	if gcdisplay.GetToggle('GSwap') == true then		-- Only gear swap if this flag is true
		gFunc.EquipSet(sets.Midshot);
		gcinclude.ProcessConditional(sets.Midshot_Conditional,nil);
	end
end

--[[
	HandleWeaponskill loads the gear appropriately for the weapon skill you're doing
--]]

profile.HandleWeaponskill = function()
	
	if gcdisplay.GetToggle('GSwap') == true then		-- Only gear swap if this flag is true
	
		local ws = gData.GetAction();
		local canWS = gcinclude.CheckWsBailout();
 
		-- If conditions would cause the weaponskill to fail, the action will be
		-- cancelled so you do not lose your tp.
		if (canWS == false) then 
			gFunc.CancelAction();
			return;
		else
			local sWS = gcinclude.WsStat(ws.Name,'STR');	-- Equip appropriate gear for weapon skill
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
			elseif sWS == 'WS_STRAGI' then
				gcinclude.ProcessConditional(sets.WS_STRDEX_Conditional,nil);				
			elseif sWS == 'WS_STRDEX' then
				gcinclude.ProcessConditional(sets.WS_STRDEX_Conditional,nil);
			elseif sWS == 'WS_STRMND' then
				gcinclude.ProcessConditional(sets.WS_STRMND_Conditional,nil);
			elseif sWS == 'WS_STRINT' then
				gcinclude.ProcessConditional(sets.WS_STRINT_Conditional,nil);
			elseif sWS == 'WS_STRINT_30_20' then
				gcinclude.ProcessConditional(sets.WS_STRINT_30_20_Conditional,nil);
			elseif sWS == 'WS_STRVIT' then
				gcinclude.ProcessConditional(sets.WS_STRVIT_Conditional,nil);
			end
			
			-- See if an elemental gorget makes sense to equip
			if gcinclude.settings.bEleGorgets == false then
				gcinclude.CheckForObisGorgets();
			end		
			if gcinclude.settings.bEleGorgets == true then
				local sGorget = gcinclude.CheckEleGorget(ws.Name);
				if sGorget ~= nil then
					gFunc.ForceEquip('Neck',sGorget);
				end
			end
		end
	end
end

return profile;