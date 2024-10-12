local profile = {};
gcinclude = gFunc.LoadFile('common\\gcinclude.lua');

--[[
	This file contains all the gear sets associated with the MNK job.
	
	Gear Sets last updated: June 21, 2024	
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
		- Minor changes to artifact armor
		- Moderate changes to Spharai
		- "Boost" has a larger attack bonus of 20%, a longer recast of 30 seconds and a duration
		  of 3 minutes
		- "Focus" is learned at level 15 instead of 25, has a 3 minute duration and increases
		  crit rate
		- "Dodge" is learned at 25, has a 3 minute duration and increases guard rate
		- "Chakra"'s recast timer has been reduced to 2 minutes from 3. It also removed multiple
		  status effects the player is inflicted with
		- "Chi Blast" now removes 1 mob buff
		- "Boost"'s effect on "Chi Blast" has need lessened than on retail
--]]

--[[
	The TP sets are used when you are fighting. The accuracy set will be applied in a fractional 
	manner if /acc is specified	and the evasion set if /eva is specified. Please note that if you 
	have a subjob that can use a pet, none of the abilities are explicitly supported here.
--]]

	['TP'] = {
		Head = { 'Empress Hairpin', 'Entrancing Ribbon//SJBST' },
		Neck = { 'Spike Necklace', 'Justice Badge' },
		Ears = { 'Physical Earring', 'Reraise Earring' },
		Body = { 'Beetle Harness', 'Angler\'s Tunica', 'Lgn. Harness' },
		Hands = 'Ryl.Ftm. Gloves',
		Rings = { 'Courage Ring', 'Balance Ring' },
		Waist = 'Warrior\'s Belt',
		Legs = { 'San. Trousers', 'Ryl.Ftm. Trousers', 'Field Hose' },
		Feet = { 'Waders' },
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
		Head = 'Empress Hairpin',
		Neck = 'Spike Necklace',
		Rings = 'Balance Ring',
		Feet = 'Bounding Boots',
    },
	
--[[
	If evasion wanted, equip evasion gear. Remember that AGI converts to evasion: for every
	2 points of AGI you get 1 point of evasion
--]]
	
	['Evasion'] = {
		Head = 'Empress Hairpin',
		Feet = 'Bounding Boots',
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
		Head   = { 'Lilac Corsage//TOWN', 'Empress Hairpin', 'Entrancing Ribbon//SJBST' },
		Body   = { 'Ducal Aketon//TOWN-AK', 'Beetle Harness', 'Angler\'s Tunica', 'Lgn. Harness' },
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
	},
		
	-- If your subjob can use magic, then place any Spell Interruption Rate down 
	-- gear into the "SIR" gear set. This set is equipped in the gcinclude.HandleMidcast
	-- function that all spells go through.
	['SIR'] = {
	},
	
--[[
	Start weapons are where you define what you want the first row of equipment to look 
	like when you either log in as a MNK or switch your main job to MNK. Any other gear 
	you mention will be overridden by the Idle or Town set, so no need to include here.
--]]

	['Start_Weapons'] = {
    },

--[[
	Magic accuracy gear for the player and/or if you have a pet
--]]

	['Macc'] = {
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
	The second stage is Midcast. This is where you'll want to equip magic attack, or magic
	enhancing gear. (Magic Attack Bonus also happens here, but is broken out into it's own 
	gear set. See MAB.)
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
	Please note: if you want the recast reduction from fast cast, you must include the fast cast gear
	here too.
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
    },

	-- MND is used to determine the effectiveness of healing magic spells, 
	-- white magic enhancing spells and white magic enfeebling spells, by
	-- increasing the damage and accuracy. MND increases resistance to white
	-- magic spells as well as reducing the base damage taken from them. MND 
	-- is associated with the element water
	['MND'] = {
		Rings = 'Tranquility Ring',
    },

--[[
	And some spells are special cases, so they have individual gears sets.
--]]
	
	-- Stoneskin: Stoneskin Enhancement, Mind, and Enhancing Magic Skill. Mind is 3x more important than enhancing
	-- Magic Skill. The only piece of gear a WAR can wear to enhance stoneskin is a Stone Gorget. There's no gear
	-- that a WAR (or any job) can wear to enhance magic. Note: This gear set has no effect on Titan's Stoneskin
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
	
	MNK can use the following weapons: Hand-to-Hand (A+), Staff (B+), Club (C+)
		
	Please note that on Horizon you may have access to some weapon skills
	through your subjob. While not explicitly supported here, the appropriate
	weapon skill set will be loaded. If not listed below, you might have to
	create a custom gear set to support the skill. Remember, weapon skill sets
	are named WS_attr. If you name the set appropriately, that set will auto-
	matically be called when you use the weapon skill.
--]]

--[[
		* Strength based *

		H2H: Spinning Attack
		Staff: Heavy Swing, Shell Crusher, Full Swing
		Club: Brain Shaker, Skull Breaker, True Strike
-]]
	
	['WS_STR'] = {
		Rings = 'Courage Ring',
    },
	
--[[
		* Strength and Dexterity based, even weighting *
		
		H2H: Combo, Backhand Blow, Raging Fist
--]]

	['WS_STRDEX'] = {
		Rings = 'Courage Ring',
    },

--[[
		* Strength and Intelligence based, even weighting *
		
		Staff: Rock Crusher, Earth Crusher
--]]
	
	['WS_STRINT'] = {
		Rings = 'Courage Ring',	
    },

--[[
		* Strength and Mind based, even weighting *

		Staff: Retribution
		Club: Shining Strike, Black Halo
--]]

	['WS_STRMND'] = {
		Neck  = 'Justice Badge',
		Rings = { 'Courage Ring', 'Tranquility Ring' },
    },

--[[
		* Strength and Vitality based, even weighting *

		H2H: Howling Fist, Dragon Kick, Asuran Fists
--]]

	['WS_STRVIT'] = {
		Rings = 'Courage Ring',	
		Waist = 'Warrior\'s Belt',
    },
	
--[[
		* Intelligence based *
		
		Staff: Gates of Tartarus
--]]
	
	['WS_INT'] = {
    },
	
--[[
		* Intelligence and Mind based, even balance *
		
		Staff: Spirit Taker
--]]
	
	['WS_INTMND'] = {
		Neck  = 'Justice Badge',
		Rings = 'Tranquility Ring',
		Waist = 'Friar\'s Belt',
    },

--[[
		* Vitality based *
		
		H2H: Shoulder Tackle, One Inch Punch, Final Heaven
--]]

	['WS_VIT'] = {
    },
	
--[[
		* Skill based *
		
		Club: Starlight
--]]

	['WS_Skill'] = {
    },
	
--[[
	Movement tends to be used for kiting. Emphasis should be placed on gear that increases movement speed, but you 
	might also want gear that has evasion. The choice is yours.
--]]

	['Movement'] = { 
	},

--[[
	The following are abilities affected by gear
--]]
	
	['HundredFists'] = {
	},
	
	['Boost'] = {
	},
	
	['Focus'] = {
	},
	
	['Dodge'] = {
	},
	
	['Chakra'] = {
	},

	['ChiBlast'] = {						-- MND gear automatically loaded before this set
	},

	['Counterstance'] = {
	},
	
--[[
	Some subjobs really make no sense when combined with dragoon, but all abilities across all jobs that
	have gear that can be equipped by a PLD are included here.
--]]
	--* /BST *--
	-- CHR and Charm + gear. (Every +1 Charm adds 5% Charm duration)
	['Charm'] = {
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

local function HandlePetAction(PetAction)
	local pet = gData.GetPet();
	
	-- Only gear swap if this flag is true and the pet is a BST pet
	if gcdisplay.GetToggle('GSwap') == false or table.find(gcinclude.tSummonSkill,pet.Name) ~= nil then
		return;
	end

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
		['WAR'] = 1, ['MNK'] = 0, ['WHM'] = 0, ['BLM'] = 0, ['RDM'] = 0, ['THF'] = 0,
		['PLD'] = 0, ['DRK'] = 0, ['BST'] = 0, ['BRD'] = 0, ['RNG'] = 0, ['SMN'] = 0,
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

profile.OnLoad = function()
	local player = gData.GetPlayer();

	gSettings.AllowAddSet = true;
	gcinclude.Initialize();
	
	-- Coded order of operation override
	gcinclude.settings.priorityEngaged = 'CEF';
	gcinclude.settings.priorityMidCast = 'ABCDEGHF';
	gcinclude.settings.priorityWeaponSkill = 'ADBE';
	
	-- Set your job macro toolbar defaults here. Which set depends on the subjob
	AshitaCore:GetChatManager():QueueCommand(1, '/macro book 7');		-- MNK
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
	if player.Status == 'Engaged' then
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
	if pet ~= nil and table.find(gcinclude.tSummonSkill,pet.Name) ~= nil and gcdisplay.GetToggle('WSwap') == true then
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
		
	gcinclude.fEquipTheGear(sets.CurrentGear);
					
	-- Lastly, update the display, just in case
	gcdisplay.Update();
end		-- HandleDefault

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
	
	-- Now process the appropriate job ability. Start with abilities associated with WAR
	if string.match(ability.Name, 'Hundred Fists') then
		gcinclude.fMoveToCurrent(sets.HundredFists,sets.CurrentGear);
	elseif string.match(ability.Name, 'Boost') then
		gcinclude.fMoveToCurrent(sets.Boost,sets.CurrentGear);		
	elseif string.match(ability.Name, 'Focus') then
		gcinclude.fMoveToCurrent(sets.Focus,sets.CurrentGear);
	elseif string.match(ability.Name, 'Dodge') then
		gcinclude.fMoveToCurrent(sets.Dodge,sets.CurrentGear);		
	elseif string.match(ability.Name, 'Chakra') then
		gcinclude.fMoveToCurrent(sets.Chakra,sets.CurrentGear);
	elseif string.match(ability.Name, 'ChiBlast') then
		-- Chi Blast is greatly affected by MND
		gcinclude.fMoveToCurrent(sets.MND,sets.CurrentGear);		
		-- Now any other gear
		gcinclude.fMoveToCurrent(sets.ChiBlast,sets.CurrentGear);		
	elseif string.match(ability.Name, 'Counterstance') then
		gcinclude.fMoveToCurrent(sets.Counterstance,sets.CurrentGear);
		
	-- And now the subjob abilities
	-- /BST	
	elseif string.match(ability.Name, 'Charm') then	
		gcinclude.fMoveToCurrent(sets.Charm,sets.CurrentGear);
		local sStave = gcinclude.fCheckForEleGear('staff','light');
		if sStave ~= nil then
			gcinclude.fSwapToStave(sStave,false,sets.CurrentGear);
		end
	elseif string.match(ability.Name, 'Reward') then
		-- Pet reward. Make sure that pet food already equipped
		if profile.sAmmo == nil or string.find(string.lower(profile.sAmmo),'pet f') == nil then		-- something else equipped
			profile.bAmmo = gcinclude.doPetFood('max',nil);
		end	
		gcinclude.fMoveToCurrent(sets.Reward,sets.CurrentGear);
	elseif string.match(ability.Name, 'Tame') then
		gcinclude.fMoveToCurrent(sets.Tame,sets.CurrentGear);
	-- /WAR
	elseif string.match(ability.Name, 'Provoke') then
		gcinclude.fMoveToCurrent(sets.Provoke,sets.CurrentGear);
	elseif string.match(ability.Name, 'Berserk') then
		gcinclude.fMoveToCurrent(sets.Berserk,sets.CurrentGear);
	elseif string.match(ability.Name, 'Defender') then
		gcinclude.fMoveToCurrent(sets.Defender,sets.CurrentGear);
	elseif string.match(ability.Name, 'Warcry') then
		gcinclude.fMoveToCurrent(sets.Warcry,sets.CurrentGear);
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
		gcinclude.fMoveToCurrent(sets.ThirdEye,sets.CurrentGear);
	elseif string.match(ability.Name, 'Hasso') then
		gcinclude.fMoveToCurrent(sets.Hasso,sets.CurrentGear);
	elseif string.match(ability.Name, 'Meditate') then
		gcinclude.fMoveToCurrent(sets.Meditate,sets.CurrentGear);
	elseif string.match(ability.Name, 'Seigan') then
		gcinclude.fMoveToCurrent(sets.Seigan,sets.CurrentGear);
	-- /PLD
	elseif string.match(ability.Name, 'Holy Circle') then
		gcinclude.fMoveToCurrent(sets.HolyCircle,sets.CurrentGear);
	elseif string.match(ability.Name, 'Shield Bash') then
		gcinclude.fMoveToCurrent(sets.ShieldBash,sets.CurrentGear);
	elseif string.match(ability.Name, 'Sentinel') then
		gcinclude.fMoveToCurrent(sets.Sentinel,sets.CurrentGear);	
	elseif string.match(ability.Name, 'Cover') then
		gcinclude.fMoveToCurrent(sets.Cover,sets.CurrentGear);	
	-- /DRG
	elseif string.match(ability.Name, 'Ancient Circle') then
		gcinclude.fMoveToCurrent(sets.AncientCircle,sets.CurrentGear);	
	elseif string.match(ability.Name, 'Jump') then
		gcinclude.fMoveToCurrent(sets.Jump,sets.CurrentGear);
	elseif string.match(ability.Name, 'High Jump') then
		gcinclude.fMoveToCurrent(sets.HighJump,sets.CurrentGear);
	-- /DRK
	elseif string.match(ability.Name, 'Arcane Circle') then
		gcinclude.fMoveToCurrent(sets.ArcaneCircle,sets.CurrentGear);
	elseif string.match(ability.Name, 'Last Resort') then
		gcinclude.fMoveToCurrent(sets.LastResort,sets.CurrentGear);
	elseif string.match(ability.Name, 'Weapon Bash') then
		gcinclude.fMoveToCurrent(sets.WeaponBash,sets.CurrentGear);
	elseif string.match(ability.Name, 'Souleater') then
		gcinclude.fMoveToCurrent(sets.Souleater,sets.CurrentGear);	
	end
	gcinclude.fEquipTheGear(sets.CurrentGear);		-- Equip the composited HandleAbility set
end		-- HandleAbility
	
--[[
	HandleItem is the place to equip gear when a special item is used. Currently only 'Holy Water' 
	is supported
--]]

profile.HandleItem = function()
	local item = gData.GetAction();

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
	HandlePrecast loads Fast Cast, cast time reduction, and quick cast gear in anticipation of a spell
--]]

profile.HandlePrecast = function()
    local spell = gData.GetAction();
	local obi;
	local mSet;
		
	-- Only gear swap if this flag is trues
	if gcdisplay.GetToggle('GSwap') == false then		
		return;
	end

	-- Clear out the CurrentGear in case of leftovers
	gcinclude.ClearSet(gProfile.Sets.CurrentGear);
	
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
	gcinclude.ClearSet(gProfile.Sets.CurrentGear);
	
	-- Call the common HandleMidcast now
	gcinclude.HandleMidcast(false);
	
	gcinclude.fEquipTheGear(sets.CurrentGear);		-- Equip the composited midcast set
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
	gcinclude.ClearSet(gProfile.Sets.CurrentGear);
	
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