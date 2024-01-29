local profile = {};
gcinclude = gFunc.LoadFile('common\\gcinclude.lua');

--[[
	This file contains all the gear sets associated with the WAR job. While it isn't outside of the realm of 
	possibility that the subjob might be able to use gear sets too, that is not the emphasis of this program. 
	It is tailored to handle all the aspects of WAR. If you desire a gear set change to strengthen an ability
	from your subjob that is not supported by this program, you probably will have to make a custom gear set 
	and use the /gearset command to use it.
--]]

local sets = {
--[[
	The gear sets are usually defined by a pair of sets: the "main" one and an associated "conditional" set. The
	"main" is loaded when appropriate and the conditional is processed to see if any of the entries should be
	equipped too. ("Conditional" entries consist of gear that need to meet certain conditions before they will be
	equipped.) "main" sets contain your standard gear slot='gear piece' combinations and the "conditional" entries
	of the piece of gear, a description of the condition, the slot the piece equips into, the minimum level the 
	player must be to equip the piece, what job can equip the piece, and the conditional code with potentially
	associated information needed to determine if you can wear the piece. (All conditional gear's definitions
	can be found in "Conditional gear master list.txt found in ../common as well as some user-defined conditionals
	known to work. Just copy the entry from that file into the appropriate "conditional" set.
	
	It is recommended that "main" sets not include any gear found in the top line of your equipment grid (main hand,
	off hand, ranged weapon, ammo). Doing so will mean that TP will be reset to 0 whenever gear is changed which can
	be very frustrating. Further, any time you do change a weapon, it will convert back to what was defined in a set.
	Believe me, it's no fun	fighting Luashitacast!

	Also, not all sets need to be defined. There is nothing wrong with leaving a set "empty", but don't delete any
	of the sets. All the ones listed here (except for any custom sets) are expected to exist by Luashitacast.
		
	*** Note ***
	Unlike when summoner is used as a subjob, /bst's pets are charmed at the max level of your BST or the level
	of your WAR, whichever is lower. That means you can charm higher level mobs than you would expect with /bst.
	Just note though that you can't have two pets, so if you have charmed a pet with /bst, you can't summon your
	avatar and visa versa.
	
--]]

--[[
	The "Idle" set is what your character will wear when it is not fighting nor resting nor in town. Whether just 
	standing out of town or going to a different area, the "Idle" set will be equipped. If you've subbed /SMN,
	it is strongly recommended that you use gear that has avatar perpetuation cost down attributes on it here. If
	you've subbed /BST, gear swaps occur during the appropriate ability.
--]]
	
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
	
	['Resting_Regen'] = { 
	},
	['Resting_Regen_Conditional'] = {
	},
	
	['Resting_Refresh'] = {
	},
	['Resting_Refresh_Conditional'] = {
	},

	['Resting_Refresh_Weapon_Sub51'] = {
	},
	['Resting_Refresh_Weapon_Sub51_Conditional'] = {
	},
		
	-- If you have any Spell Interruption Rate down gear, put them into the "SIR" gear set.
	-- This gear set is equipped in the HandleMidcast function that all spells go through.
	-- Currently only gear equippable by any job is applicable here. There's no gear that's 
	-- specific for WAR that gives spell interruption rate down.
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
	},
	
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
	
	['TP_Tank'] = {
	},
	['TP_Tank_Conditional'] = {
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
	Magic Attack Bonus (MAB) is used for more than just spells, so it is broken out
--]]

	['MAB'] = {
	},
	['MAB_Conditional'] = {
		{'Uggalepih Pendant','MAB +8% if MPP <= 50%','Neck',70,'ALL','MP.LE.50P'},
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
	The second stage is Midcast. This is where you'll want to equip magic attack, or magic enhancing 
	gear. (Magic Attack Bonus also happens here, but is broken out into it's own gear set. See MAB.)
--]]	

	['Midcast'] = {
	},
	['Midcast_Conditional'] = {
	},

--[[
	Further, there is a break out for each type of spell. I've included a comment on the type of attributes
	the piece of gear should have. While the spell might have other attributes than those listed, the ones I have
	listed have gear that a WAR or anyone can wear.
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
	-- specific for WAR that gives any dark magic skill.	
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
	-- specific for WAR that gives any enfeebling magic skill.	
	['Enfeebling'] = {
	},
	['Enfeebling_Conditional'] = {
	},
	
	-- Enhancing: There is no gear that a WAR can wear to enhance any magic spell. Leave the Enhancing gear sets empty.
	['Enhancing'] = {
	},
	['Enhancing_Conditional'] = {
	},
	
	-- Elemental: Elemental Magic Skill. Currently only gear equippable by any job gives is applicable here. There's no gear
	-- that's specific for WAR that gives any elemental magic skill. Note: don't include elemental staves or elemental 
	-- obis/gorgets here, that is done automatically in the HandlePrecast/HandleMidcast functions (if /wswap is enabled).
	['Elemental'] = {
	},
	['Elemental_Conditional'] = {
	},

	-- Ninjitsu: There is no gear that a WAR can wear to add Ninjitsu skill. Leave the following two
	-- gear sets empty.	
	['Ninjitsu'] = {			-- Ninjitsu Skill, magic burst bonus, magic attack bonus
	},
	['Ninjitsu_Conditional'] = {
	},
	
	-- Summoning: Summoning Magic Skill and Avatar Perpetuation Cost. Currently only gear equippable by any job gives
	-- is applicable here. There's no gear that's specific for WAR that gives any summoning skill. Note: currently on 
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
	-- Magic Skill. The only piece of gear a WAR can wear to enhance stoneskin is a Stone Gorget. There's no gear
	-- that a WAR (or any job) can wear to enhance magic. Note: This gear set has no effect on Titan's Stoneskin
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
	-- WAR enhances Aspir. Aspir is part of Dark Magic, so potency which is based on dark magic skill will
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
	
	WAR can use the following weapons: Great Axe (A+), Axe (A-), Great Sword (B+), Scythe (B+), Staff (B),
	Sword (B), Club (B-), Dagger (B-), Polearm (B-), Hand to Hand (D), Archery (D), Marksmanship (D)
	
	Any other weapon will have no weaponskill available. Weapon skill sets are named based on stat(s) used, 
	regardless of weapon
--]]

--[[
		* Strength based or just skill based *

		Great Axe: Iron Tempest,Sturmwind,Keen Edge,Raging Rush,Metatron Torment
		Axe: Raging Axe,Smash Axe,Gale Axe,Avalanche Axe,Spinning Axe,Rampage,Mistral Axe,Decimation,Onslaught
		Great Sword: Hard Slash,Crescent Moon
		Scythe: Slice,Spinning Scythe,Vorpal Scythe
		Staff: Heavy Swing,Shell Crusher,Full Swing
		Sword: Flat Blade,Circle Blade,Spirits Within,Vorpal Blade
		Club: Starlight,Brainshaker,Moonlight,Skullbreaker,True Strike
		Polearm: Double Thrust,Leg Sweep,Impulse Drive
		H2H: Spinning Attack
-]]
	
	['WS_STR'] = {
    },
	['WS_STR_Conditional'] = {
	},

--[[
		* Strength and Agility based *

		Polearm: Vorpal Thrust
		Archery: Flaming Arrow^,Piercing Arrow^,Dulling Arrow^,Sidewinder^,Namas Arrow
		
		^ Sub must be RNG
--]]

	['WS_STRAGI'] = {
    },
	['WS_STRAGI_Conditional'] = {
	},
	
--[[
		* Strength and Dexterity based, even weighting *
		
		Sword: Fast Blade
		Polearm: Penta Thrust
		H2H: Combo,Backhand Blow,Raging Fist^
		
		^ Sub must be MNK
--]]

	['WS_STRDEX'] = {
    },
	['WS_STRDEX_Conditional'] = {
	},

--[[
		* Strength and Intelligence based, even weighting *
		
		Great Sword: Frostbite,Freezebite,Ground Strike
		Scythe: Dark Harvest,Shadow of Death,Nightmare Scythe,Spiral Hell,Catastrophe
		Staff: Rock Crusher,Earth Crusher
		Sword: Burning Blade
		Polearm: Thunder Thrust
--]]
	
	['WS_STRINT'] = {
    },
	['WS_STRINT_Conditional'] = {
	},

--[[
		* Strength and Intelligence based, 30%/20% weighting *

		Sword: Red Lotus Blade
--]]
	
	['WS_STRINT_30_20'] = {
    },
	['WS_STRINT_30_20_Conditional'] = {
	},


--[[
		* Strength and Mind based, even weighting *

		Great Sword: Shockwave,
		Scythe: Guillotine,Cross Reaper
		Staff: Starburst,Sunburst,Retribution
		Sword: Shining Blade,Seraph Blade,Swift Blade,Savage Blade,Knights of Round
		Club: Shining Strike,Seraph Strike,Judgement,Black Halo,Randgrith
--]]

	['WS_STRMND'] = {
    },
	['WS_STRMND_Conditional'] = {
	},

--[[
		* Strength and Vitality based, even weighting *

		Great Axe: Shield Break,Armor Break,Weapon Break,Full Break,Steel Cyclone
		Axe: Calamity
		Great Sword: Power Slash,Scourge
--]]

	['WS_STRVIT'] = {
    },
	['WS_STRVIT_Conditional'] = {
	},
	
--[[
		* Agility based *

		Polearm: Geirkogul
		Marksmanship: Hot Shot^,Split Shot^,Sniper Shot^,Slug Shot^
		
		^ Sub must be RNG
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
		* Intelligence based *
		
		Staff: Gates of Tartarus
--]]
	
	['WS_INT'] = {
    },
	['WS_INT_Conditional'] = {
	},
	
--[[
		* Intelligence and Mind based, even balance *
		
		Staff: Spirit Taker
--]]
	
	['WS_INTMND'] = {
    },
	['WS_INTMND_Conditional'] = {
	},
	
--[[
		* Dexterity based *
		
		Dagger: Wasp Sting,Eviseration
--]]
	
	['WS_DEX'] = {
    },
	['WS_DEX_Conditional'] = {
	},

--[[
		* Dexterity and Agility based *

		Marksmanship: Coronach
		
--]]
	
	['WS_DEXAGI'] = {
    },
	['WS_DEXAGI_Conditional'] = {
	},
	
--[[
		* Dexterity and Intelligence based *
		
		Dagger: Gust Slash
--]]
	
	['WS_DEXINT'] = {
    },
	['WS_DEXINT_Conditional'] = {
	},

--[[
		* Mind based *
		
		Dagger: Energy Steal
--]]

	['WS_MND'] = {
    },
	['WS_MND_Conditional'] = {
	},

--[[
		* Skill based *
		
		Club: Starlight,Moonlight
--]]

	['WS_Skill'] = {
    },
	['WS_Skill_Conditional'] = {
	},

--[[
		* HP based *
		
		Sword: Spirits Within
--]]

	['WS_HP'] = {
        Head = 'Beast Helm',				-- +15 HP
        Ear1 = 'Ethereal Earring',			-- +15 HP
        Ear2 = 'Physical Earring',			-- +25 HP
        Body = 'Wonder Kaftan',				-- +36 HP
        Hands = 'Wonder Mitts',				-- +12 HP
        Ring1 = 'Toreador\'s Ring',			-- +10 HP
        Waist = 'Powerful Rope',			-- +20 HP
        Legs = 'Wonder Braccae',			-- +21 HP
        Feet = 'Creek F Clomps',			-- +35 HP
    },
	['WS_HP_Conditional'] = {
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
	
	['Berserk'] = {
	},
	['Berserk_conditional'] = {
	},
	
	['Warcry'] = {
	},
	['Warcry_conditional'] = {
	},
	
	['Aggressor'] = {
	},
	['Aggressor_conditional'] = {
	},
	
--[[
	Some subjobs really make no sense when combined with dragoon, but all abilities across all jobs that
	have gear that can be equipped by a PLD are included here.
	
	The following sub jobs have no skills with equippable gear by a THF: WAR,DRG,BLM,MNK,WHM,RDM,RNG,NIN,
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
	local pet = gData.GetPet();
	
	-- Only gear swap if this flag is true
	if gcdisplay.GetToggle('GSwap') == false or string.find(gcinclude.MagicSkill['Summoning'],pet.Name) ~= nil then
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
	local subs = {['WAR'] = nil, ['MNK'] = 0, ['WHM'] = 1, ['BLM'] = 0, ['RDM'] = 0, ['THF'] = nil,
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
	gcinclude.settings.priorityEngaged = 'BCEFGH';
	gcinclude.settings.priorityMidCast = 'ABCDEFGH';
	gcinclude.settings.priorityWeaponSkill = 'ABDE';
	
	-- Determine if subjob uses magic and if the maximum MP is > 50.
	gcinclude.CheckMagic50(player);
	
	-- Set your job macro toolbar defaults here. Which set depends on the subjob
	AshitaCore:GetChatManager():QueueCommand(1, '/macro book 15');		-- WAR
	SetSubjobSet(player.SubJob);
	
	-- Load up the weapons bar. (This need only be done once.)
	gcinclude.MoveToCurrent(sets.Start_Weapons,sets.CurrentGear);
	gcinclude.ProcessConditional(sets.Start_Weapons_Conditional,nil,sets.CurrentGear);	
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

	-- Clear out the CurrentGear in case of leftovers
	gcinclude.ClearSet(sets.CurrentGear);
	
	-- If player is not resting and has MP and has swapped weapons, set the weapon back to what 
	-- they had before the switch
	if player.Status ~= 'Resting' and gcdisplay.GetToggle('WSwap') == true then
		if gcinclude.weapon ~= nil and eWeap ~= gcinclude.weapon then
			gFunc.ForceEquip('Main', gcinclude.weapon);	
			gFunc.ForceEquip('Sub', gcinclude.offhand);	
		end
	end
	
	-- The default set is the TP gear set. Load it up
	gcinclude.MoveToCurrent(sets.TP,sets.CurrentGear);
	gcinclude.ProcessConditional(sets.TP_Conditional,nil,sets.CurrentGear);	

	if gcdisplay.GetToggle('Tank') == true then
		gcinclude.MoveToCurrent(sets.TP_Tank,sets.CurrentGear);
		gcinclude.ProcessConditional(sets.TP_Tank_Conditional,nil,sets.CurrentGear);	
	end
		
	-- Now process the player status accordingly
	if player.Status == 'Engaged' then
		gcinclude.settings.priorityEngaged = string.upper(gcinclude.settings.priorityEngaged);
		for i = 1,string.len(gcinclude.settings.priorityEngaged),1 do
			cKey = string.sub(gcinclude.settings.priorityEngaged,i,i);
			if cKey == 'B' then		-- Pet (if out) is fighting
				if pet ~= nil and pet.Status == 'Engaged' then
					gcinclude.MoveToCurrent(sets.TP_Pet,sets.CurrentGear);
					gcinclude.ProcessConditional(sets.TP_Pet_Conditional,nil,sets.CurrentGear);
				end		
			elseif cKey == 'C' then		-- Evasion			
				if gcdisplay.GetToggle('Eva') == true then
					gcinclude.MoveToCurrent(sets.Evasion,sets.CurrentGear);
					gcinclude.ProcessConditional(sets.Evasion_Conditional,nil,sets.CurrentGear);
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
			end				
		end
	elseif player.Status == 'Resting' then	
		-- Player kneeling. Priority (low to high): regen,refresh
		gcinclude.MoveToCurrent(sets.Resting_Regen,sets.CurrentGear);
		gcinclude.ProcessConditional(sets.Resting_Regen_Conditional,nil,sets.CurrentGear);
		if gcinclude.settings.bSJ == true and player.MPP < gcinclude.settings.RefreshGearMPP then
			gcinclude.MoveToCurrent(sets.Resting_Refresh,sets.CurrentGear);
			gcinclude.ProcessConditional(sets.Resting_Refresh_Conditional,nil,sets.CurrentGear);
		end
		
		-- Weapon swap to a weapon that refreshes MP if player's subjob uses magic, weapon swapping
		-- is enabled (/wswap) and their MP is not at maximum
		if gcdisplay.GetToggle('WSwap') == true and gcinclude.settings.bSJ == true and player.MP < player.MaxMP then
			if gcinclude.settings.bStave == false then
				gcinclude.CheckForStaves();
			end
			if player.MainJobLevel < 51 then
				gcinclude.MoveToCurrent(sets.Resting_Refresh_Weapon_Sub51,sets.CurrentGear);
				gcinclude.ProcessConditional(sets.Resting_Refresh_Weapon_Sub51_Conditional,nil,sets.CurrentGear);
			else			
				gcinclude.SwapToStave('dark',false,sets.CurrentGear);			
			end
		end
		-- Check for common debuffs
		gcinclude.CheckCommonDebuffs();
	else									
		-- Assume idling. Priority (low to high): regen,refresh

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
		if gcinclude.settings.bSJ == true and player.MPP < gcinclude.settings.RefreshGearMPP then
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

	-- Clear out the CurrentGear in case of leftovers
	gcinclude.ClearSet(sets.CurrentGear);
	
	-- Now process the appropriate job ability. Start with abilities associated with WAR
	if string.match(ability.Name, 'Berserk') then
		gcinclude.MoveToCurrent(sets.Berserk,sets.CurrentGear);
		gcinclude.ProcessConditional(gcinclude.sets.Berserk_Conditional,nil,sets.CurrentGear);
	elseif string.match(ability.Name, 'Warcry') then
		gcinclude.MoveToCurrent(sets.Warcry,sets.CurrentGear);
		gcinclude.ProcessConditional(gcinclude.sets.Warcry_Conditional,nil,sets.CurrentGear);		
	elseif string.match(ability.Name, 'Aggressor') then
		gcinclude.MoveToCurrent(sets.Aggressor,sets.CurrentGear);
		gcinclude.ProcessConditional(gcinclude.sets.Aggressor_Conditional,nil,sets.CurrentGear);
		
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
--[[
		Abilities associated with subjobs go here. The following subjobs have
		no ability entries because of lack of gear or just doesn't make sense: 
		SMN,WAR,MNK,WHM,BLM,RDM,BRD,RNG,SAM
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

	-- Clear out the CurrentGear in case of leftovers
	gcinclude.ClearSet(sets.CurrentGear);
	
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
				-- See if Magic Attack Bonus useful. Note: Spells like bio/dia initial damage is
				-- affected by MAB, but not the tic. Not worth trying to support here. Also, Ninjitsu
				-- is affected by Ninjitsu Magic Attack Bonus and not just Magic Bonus.
				if string.find('Healing,Enfeebling,Enhancing,Ninjitsu,Summoning',mSet) == nil then
					gcinclude.MoveToCurrent(sets.MAB,sets.CurrentGear);
					gcinclude.ProcessConditional(sets.MAB_Conditional,nil,sets.CurrentGear);
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
		-- Clear out the CurrentGear in case of leftovers
		gcinclude.ClearSet(sets.CurrentGear);	
		
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
	
	-- Clear out the CurrentGear in case of leftovers
	gcinclude.ClearSet(sets.CurrentGear);
	
	gcinclude.MoveToCurrent(sets.Midshot,sets.CurrentGear);
	gcinclude.ProcessConditional(sets.Midshot_Conditional,nil,sets.CurrentGear);

	-- Equip the composited Midshot set
	gcinclude.EquipTheGear(sets.CurrentGear);	
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

	-- Clear out the CurrentGear in case of leftovers
	gcinclude.ClearSet(sets.CurrentGear);
	
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
			elseif sWS == 'WS_Skill' then
				gcinclude.MoveToCurrent(sets.WS_Skill,sets.CurrentGear);
				gcinclude.ProcessConditional(sets.WS_Skill_Conditional,nil,sets.CurrentGear);
			elseif sWS == 'WS_HP' then
				gcinclude.MoveToCurrent(sets.WS_HP,sets.CurrentGear);			
				gcinclude.ProcessConditional(sets.WS_HP_Conditional,nil,sets.CurrentGear);					
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
	
	-- Special case(s) for specific weapon skills go here
	ws.Name = string.lower(ws.Name);
	if string.find('red lotus blade,sanguine blade',ws.Name) ~= nil then
		gcinclude.MoveToCurrent(sets.MAB,sets.CurrentGear);
		gcinclude.ProcessConditional(sets.MAB_Conditional,nil,sets.CurrentGear);	
	end
	
	-- Equip the composited weaponskill set	
	gcinclude.EquipTheGear(sets.CurrentGear);	
end

return profile;