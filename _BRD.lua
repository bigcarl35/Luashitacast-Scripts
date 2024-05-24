local profile = {};
gcinclude = gFunc.LoadFile('common\\gcinclude.lua');

--[[
	This file contains all the gear sets associated with the BRD job. While it isn't outside of the realm of 
	possibility that the subjob might be able to use gear sets too, that is not the emphasis of this program. 
	It is tailored to handle all the aspects of BRD. If you desire a gear set change to strengthen an ability
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
	of your BRD, whichever is lower. That means you can charm higher level mobs than you would expect with /bst.
	Just note though that you can't have two pets, so if you have charmed a pet with /bst, you can't summon your
	avatar and visa versa.
	
--]]

--[[
	The TP sets are used when you are fighting. The accuracy set will be used if /acc is specified
	and the evasion set if /eva is specified. Please note that if you have a subjob that can use a
	pet, none of the abilities are supported here. Only main jobs that have pets (SMN,BST) support
	pet actions.
--]]

	['TP'] = {
    },
	
	['TP_Tank'] = {
	},
	
--[[
	If an accuracy emphasis is desired, the following set will replace the gear appropriately.
	Remember that DEX converts to accuracy: (horizon) for every 1 point of DEX you get 
	0.70 points of accuracy if wielding a 2H weapon, 0.65 for a 1H weapon, and 0.60 for H2H. 
	Tank_Accuracy is a subset of Accuracy. It lets you specify what accuracy gear to equip 
	that doesn't compromise your tanking set as much as a full-blown accuracy set would.
--]]
	
	['Accuracy'] = {
    },
	
	['Pet_Accuracy'] = {
    },
	
--[[
	If evasion wanted, equip evasion gear
--]]
	
	['Evasion'] = {
    },
	
	['Pet_Evasion'] = {
    },
	
--[[
	The Idle_Regen and Idle_Refresh gear sets replace the normal Idle set when the player's HP or MP
	go below a set percentage (defined in gcinclude.lua, but can be overriden in profile.OnLoad function).
--]]
	
	['Idle_Regen'] = {
	},
	
	['Idle_Refresh'] = {
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
		
	-- If you have any Spell Interruption Rate down gear, put them into the "SIR" gear set.
	-- This gear set is equipped in the HandleMidcast function that all spells go through.
	-- Currently only gear equippable by any job is applicable here. There's no gear that's 
	-- specific for WAR that gives spell interruption rate down.
	['SIR'] = {
	},
	
--[[
	Start weapons are where you define what you want the first row of equipment to look like when you
	either log in as a SMN or you switch your main job to SMN. Any other gear you mention will be overridden
	by the Idle or Town set, so no need to include here.
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
	Damage reduction gear depends on the type of damage. The most common is physical, but there's times when
	you'll want to reduce magic damage or breath damage. The three gear sets are defined below. The correct
	one will be equipped depending on whether DT is enabled and which set is equipped depends on the DT_TYPE 
	selected. Please consider not including gear that doesn't have any damage taken property so other wanted 
	stats can shine through.
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
	gear that reduces the time it takes to shoot (snap shot, rapid shot, haste).
--]]

	['Preshot'] = {
    },
	
--[[
	Midshot is the second stage of a ranged shot. This is where you place Ranged Accuracy, Ranged 
	Attack or Ranged Damage gear.
--]]

	['Midshot'] = {
    },

--[[
	The 'meat and potatos' of being a bard is the songs. The following sets allow the bard to
	individualize songs based on type plus emulate the precast/midcast of regular spells.
	
	Song_Precast is where you specify fastcast and song spellcast time reduction gear.
--]]

	['Song_Precast'] = {							
	},

--[[
	Song_Midcast is where you specify fastcast for recast reductions and song duration gear.
	Also included is the default instrumentation gear. Don't forget to use the //HORN and
	//STRING inline conditionals.
--]]	

	['Song_Midcast'] = {
	},

--[[
	Each of the sections below are so you can tailor your midcast gear for the specific type
	of song. The entries use the standard prioritization sequences and support inline and 
	conditional blocks.
--]]

	['Ballad'] = {
	},
	
	['Carol'] = {
	},
	
	['Elegy'] = {
	},
		
	['Etude'] = {
	},
	
	['Finale'] = {
	},
	
	['Hymnus'] = {
	},
	
	['Lullaby'] = {
	},
	
	['Madrigal'] = {
	},
	
	['Mambo'] = {
	},
	
	['March'] = {
	},
	
	['Mazurka'] = {
	},
	
	['Minne'] = {
	},
	
	['Minuet'] = {
		Range = 'Flute//HORN',
	},
	
	['Paeon'] = {
	},	
	
	['Prelude'] = {
	},
	
	['Requiem'] = {
	},
	
	['Threnody'] = {
	},
	
	['Virelai'] = {
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
	
	For songs, fast cast is used for recast reductions. You also include song duration gear here.
--]]	

	['Midcast'] = {
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
	
	-- Dark: Dark Magic Skill. Currently only gear equippable by any job gives is applicable here. There's no gear that's 
	-- specific for WAR that gives any dark magic skill.	
	['Dark'] = {
    },

	-- Divine: Divine Magic Skill.	
	['Divine'] = {
	},
	
	-- Enfeebling Magic Skill.	Currently only gear equippable by any job gives is applicable here. There's no gear that's 
	-- specific for WAR that gives any enfeebling magic skill.	Also consider putting magical accuracy gear here too.
	['Enfeebling'] = {
	},
	
	-- Enhancing: There is no gear that a WAR can wear to enhance any magic spell. Leave the Enhancing gear sets empty.
	['Enhancing'] = {
	},
	
	-- Elemental: Elemental Magic Skill. Currently only gear equippable by any job gives is applicable here. There's no gear
	-- that's specific for WAR that gives any elemental magic skill. Note: don't include elemental staves or elemental 
	-- obis/gorgets here, that is done automatically in the HandlePrecast/HandleMidcast functions (if /wswap is enabled).
	['Elemental'] = {
	},

	-- Ninjutsu: There is no gear that a WAR can wear to add Ninjutsu skill. Leave the following two
	-- gear sets empty.	
	['Ninjutsu'] = {			-- Ninjutsu Skill, magic burst bonus, magic attack bonus
	},
	
	-- Summoning: Summoning Magic Skill and Avatar Perpetuation Cost. Currently only gear equippable by any job gives
	-- is applicable here. There's no gear that's specific for WAR that gives any summoning skill. Note: currently on 
	-- HorizonXI summoning skills are ignored. Any gear piece that only gives summoning skill will be commented out	
	['Summoning'] = {
	},
	
--[[
	Next is stat-based gear, (in this case intelligence or mind)
--]]

	['INT'] = {
    },
	
	['MND'] = {
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
	
	-- Drain: Drain Enhancement, Dark Magic Skill, Dark Magic Accuracy. Currently no gear supports Drain enhancement.
	-- Drain is part of Dark Magic, so Potency which is based on dark magic skill will already be loaded in HandleMidcast 
	-- function and need not be repeated here. No current gear supports dark magic accuracy for any job. Magic attack 
	-- bonus and magic critical hit have no effect on potency. Leave the two Drain gear sets empty.
	['Drain'] = {
    },
	
	-- Aspir: Aspir Enhancement, Dark Magic Skill, Dark Magic Accuracy. Currently no gear equippable by a
	-- WAR enhances Aspir. Aspir is part of Dark Magic, so potency which is based on dark magic skill will
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
	The following are abilities affected by gear
--]]
	
--[[
	The following weapon skill gearsets are defined by the stat they emphasize. Listed are all of the sets that
	you will need to use every weapon skill that your job can do. The leading comment defines what weapon/weapon
	skill combination the set applies to.
	
	BRD can use the following weapons: Dagger (B-), Staff (C+), Sword (C-), Club (D)
	
	Any other weapon will have no weaponskill available. Weapon skill sets are named based on stat(s) used, 
	regardless of weapon
--]]

--[[
		* Strength based or just skill based *

		Dagger: Mercy Stroke
		Staff: Heavy Swing, Shell Crusher, Full Swing
		Sword: Flat Blade, Circle Blade, Vorpal Blade
		Club: Brain Shaker, Skull Breaker, True Strike
-]]
	
	['WS_STR'] = {
    },
	
--[[
		* Strength and Dexterity based, even weighting *
		
		Sword: Fast Blade
--]]

	['WS_STRDEX'] = {
    },

--[[
		* Strength and Intelligence based, even weighting *
		
		Staff: Rock Crusher, Earth Crusher
		Sword: Burning Blade, Red Lotus Blade
--]]
	
	['WS_STRINT'] = {
    },

--[[
		* Strength and Mind based, even weighting *

		Staff: Starburst, Sunburst, Retribution
		Sword: Shining Blade
		Club: Shining Strike, Black Halo
--]]

	['WS_STRMND'] = {
    },

--[[
		* Charisma based *
		
		Dagger: Shadowstitch
--]]
	
	['WS_CHR'] = {
    },
	
--[[
		* Dexterity based *
		
		Dagger: Wasp Sting, Eviseration
--]]
	
	['WS_DEX'] = {
    },

--[[
		* Dexterity and Intelligence based *
		
		Dagger: Gust Slash, Cyclone
--]]
	
	['WS_DEXINT'] = {
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
    },

--[[
		* Mind based *
		
		Dagger: Energy Steal,Energy Drain
--]]

	['WS_MND'] = {
    },

--[[
		* HP based *
		
		Sword: Spirits Within
--]]

	['WS_HP'] = {
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
	
	['SoulVoice'] = {
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
profile.prioritySongMidCast = 'ABDC';
profile.songs = T {
	['Ballad']   = 'Mage\'s Ballad,Mage\'s Ballad II',
	['Carol']    = 'Light Carol,Earth Carol,Water Carol,Fire Carol,Ice Carol,Lightning Carol,Dark Carol',
	['Elegy']    = 'Battlefield Elegy,Carnage Elegy',
	['Etude']    = 'Enchanting Etude,Spirited Etude,Learned Etude,Quick Etude,Vivacious Etude,Dextrous Etude,Sinewy Etude,Bewitching Etude,Logical Etude,Sage Etude,Swift Etude,Vital Etude,Uncanny Etude,Herculean Etude',
	['Finale']   = 'Magic Finale',
	['Hymnus']   = 'Goddess\'s Hymnus',
	['Lullaby']  = 'Foe Lullaby,Horde Lullaby',
	['Madrigal'] = 'Sword Madrigal,Blade Madrigal',
	['Mambo']    = 'Sheepfoe Mambo,Dragonfoe Mambo',
	['March']    = 'Advancing March,Victory March',
	['Mazurka']  = 'Raptor Mazurka,Chocobo Mazurka',
	['Minne']    = 'Knight\'s Minne,Knight\'s Minne II,Knight\'s Minne III,Knight\'s Minne IV',
	['Minuet']   = 'Valor Minuet,Valor Minuet II,Valor Minuet III,Valor Minuet IV',
	['Paeon']    = 'Army\'s Paeon,Army\'s Paeon II,Army\'s Paeon III,Army\'s Paeon IV,Army\'s Paeon V',
	['Prelude']  = 'Hunter\'s Prelude,Archer\'s Prelude',
	['Requiem']  = 'Foe Requiem,Foe Requiem II,Foe Requiem III,Foe Requiem IV,Foe Requiem V,Foe Requiem VI',
	['Threnody'] = 'Light Threnody,Dark Threnody,Earth Threnody,Water Threnody,Wind Threnody,Fire Threnody,Lightning Threnody',
	['Virelai']  = 'Maiden\'s Virelai',
	['Other']    = 'Herb Pastoral,Scop\'s Operetta,Fowl Aubade,Goblin Gavotte,Gold Capriccio,Shining Fantasia,Puppet\'s Operetta,Warding Round',
};
-- This breaks out all songs based on type of song skill. Please note that only the "base" of the song name is listed
profile.SongSkill = T{
	['Enhancing']  = 'Knight\'s Minne,Knight\'s Minne II,Knight\'s Minne III,Knight\'s Minne IV,Valor Minuet,Valor Minuet II,Valor Minuet III,Valor Minuet IV,Army\'s Paeon,Army\'s Paeon II,Army\'s Paeon III,Army\'s Paeon IV,Army\'s Paeon V,Herb Pastoral,Sword Madrigal,Sheepfoe Mambo,Scop\'s Operetta,Spirited Etude,Mage\'s Ballad,Mage\'s Ballad II,Learned Ballad,Quick Etude,Advancing March,Vivacious Etude,Hunter\'s Prelude,Dextrous Etude,Fowl Aubade,Sinewy Etude,Light Carol,Raptor Mazurka,Earth Carol,Wind Carol,Fire Carol,Ice Carol,Lightning Carol,Goblin Gavotte,Dark Carol,Blade Madrigal,Dragonfoe Mambo,Gold Capricco,Shining Fantasia,Victory March,Bewitching Etude,Logical Etude,Sage Etude,Swift Etude,Puppet\'s Operetta,Vital Etude,Archer\'s Prelude,Goddess\'s Hymnus,Uncanny Etude,Warding Round,Chocobo Mazurka,Herculean Etude,Maiden\'s Virelai',
	['Enfeebling'] = 'Foe Requiem,Foe Requiem II,Foe Requiem III,Foe Requiem IV,Foe Requiem V,Foe Requiem VI,Light Threnody,Dark Threnody,Earth Threnody,Foe Lullaby,Water Threnody,Wind Threnody,Fire Threnody,Ice Threnody,Horde Lullaby,Magic Finale,Carnage Elegy',
};

--[[
	BardSongToSet looks for the passed name (and the three other variants) in profile.sets and returns
	the 2 matching table references (or nil if not found).
--]]

local function BardSongToSet(sType)
	local sTypeC;
	local tType,tTypeC;
	
	if sType == nil then
		print(chat.header('StringToSet'):append(chat.message('Missing type. Skipping.')));
		return nil,nil;
	end
	sTypeC = sType .. '_Conditional';
	-- Loop through all the defined sets
	for k,_ in pairs(sets) do
		if k == sType then
			tType = sets[k];
		elseif k == sTypeC then
			tTypeC = sets[k];
		end
		if tType ~= nil and tTypeC ~= nil then
			break;
		end
	end
	return tType,tTypeC;
end		-- BardSongToSet

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
end		-- HandlePetAction

--[[
	SetSubjobSet is used to pick the appropriate set for the loaded macrobook based on
	which subjob is current. (If no change has occurred since the last time it was called,
	nothing is checked/changed.)
--]]

local function SetSubjobSet(chkSJ)
	local subs = {['WAR'] = 0, ['MNK'] = 0, ['WHM'] = 1, ['BLM'] = 1, ['RDM'] = 1, ['THF'] = 0,
				 ['PLD'] = 0, ['DRK'] = 0, ['BST'] = 0, ['BRD'] = nil, ['RNG'] = 0, ['SMN'] = 0,
				 ['SAM'] = 0, ['NIN'] = 0, ['DRG'] = 0, ['BLU'] = 0, ['COR'] = 0, ['PUP'] = 0,
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
end		-- SetSubjobSet

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
	gcinclude.settings.priorityEngaged = 'CEFGH';
	gcinclude.settings.priorityMidCast = 'ABCDEFGH';
	gcinclude.settings.priorityWeaponSkill = 'ABDE';
	profile.prioritySongMidCast = 'ABDC';
	
	-- Set your job macro toolbar defaults here. Which set depends on the subjob
	AshitaCore:GetChatManager():QueueCommand(1, '/macro book 15');		-- BRD
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
		return;
	end

	-- Assuming you're /bst, when you want to reward your pet and you do not have pet food 
	-- equipped or when you want to summon a pet and a jug is not equipped, the current item 
	-- in the ammo slot is saved. The following will set it back to what you had before 
	-- either of those two items were equipped.
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
		sets.CurrentGear['Main'] = gcinclude.weapon;
		sets.CurrentGear['Sub'] = gcinclude.offhand;
	end
	
	-- The default set is the TP gear set. Load it up
	gcinclude.MoveToCurrent(sets.TP,sets.CurrentGear);
		
	-- Now process the player status accordingly
	gcdisplay.SetLocksAction(gcinclude.LocksNumeric,player.Status);	
	if player.Status == 'Engaged' then
		gcinclude.settings.priorityEngaged = string.upper(gcinclude.settings.priorityEngaged);
		for i = 1,string.len(gcinclude.settings.priorityEngaged),1 do
			cKey = string.sub(gcinclude.settings.priorityEngaged,i,i);
			if cKey == 'C' then		-- Evasion			
				if gcdisplay.GetToggle('Eva') == true then		
					gcinclude.MoveToCurrent(sets.Evasion,sets.CurrentGear);
				end			
			elseif cKey == 'E' then		-- Accuracy	
				if gcdisplay.GetToggle('Acc') == true then 
					gcinclude.MoveToCurrent(sets.Accuracy,sets.CurrentGear);
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
		
		if gcinclude.MagicalJob('S') == true and player.MP < player.MaxMP then	
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
			gcinclude.MoveToCurrent(sets.Travel,sets.CurrentGear);
			
			-- if the player's HP is below the threshold setting, equip the idle regen gear
			if player.HPP < gcinclude.settings.RegenGearHPP then
				gcinclude.MoveToCurrent(sets.Idle_Regen,sets.CurrentGear);
			end
			
			-- if the player's MP is below the threshold setting, equip the idle refresh gear
			if gcinclude.MagicalJob('S') == true and player.MPP < gcinclude.settings.RefreshGearMPP then		-- if the player's MP is below the threshold setting, equip the idle refresh gear
				gcinclude.MoveToCurrent(sets.Idle_Refresh,sets.CurrentGear);
			end
		
			-- Check for common debuffs
			gcinclude.CheckCommonDebuffs(sets.CurrentGear);	
		end
	end
	
	-- Make sure to equip the appropriate elemental staff for the current pet (/smn only)
	if (pet ~= nil and player.SubJob == 'SMN') then
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
	if string.match(ability.Name, 'Soul Voice') then
		gcinclude.MoveToCurrent(sets.SoulVoice,sets.CurrentGear);
	-- And now the subjob abilities
	-- /BST	
	elseif string.contains(ability.Name, 'Charm') then	
		gcinclude.MoveToCurrent(sets.Charm,sets.CurrentGear);
		gcinclude.SwapToStave('light',false,sets.CurrentGear);
	elseif string.contains(ability.Name, 'Reward') then
		-- Pet reward. Make sure that pet food already equipped
		if profile.sAmmo == nil or string.find(string.lower(profile.sAmmo),'pet f') == nil then		-- something else equipped
			profile.bAmmo = gcinclude.doPetFood('max',nil);
		end	
		gcinclude.MoveToCurrent(sets.Reward,sets.CurrentGear);
	elseif string.contains(ability.Name, 'Tame') then
		gcinclude.MoveToCurrent(sets.Tame,sets.CurrentGear);
	-- /WAR
	elseif string.contains(ability.Name, 'Provoke') then
		gcinclude.MoveToCurrent(sets.Provoke,sets.CurrentGear);
	elseif string.contains(ability.Name, 'Berserk') then
		gcinclude.MoveToCurrent(sets.Berserk,sets.CurrentGear);
	elseif string.contains(ability.Name, 'Defender') then
		gcinclude.MoveToCurrent(sets.Defender,sets.CurrentGear);
	elseif string.contains(ability.Name, 'Warcry') then
		gcinclude.MoveToCurrent(sets.Warcry,sets.CurrentGear);
	-- /MNK
	elseif string.contains(ability.Name, 'Boost') then
		gcinclude.MoveToCurrent(sets.Boost,sets.CurrentGear);
	elseif string.contains(ability.Name, 'Focus') then
		gcinclude.MoveToCurrent(sets.Focus,sets.CurrentGear);
	elseif string.contains(ability.Name, 'Dodge') then
		gcinclude.MoveToCurrent(sets.Dodge,sets.CurrentGear);
	elseif string.contains(ability.Name, 'Chakra') then
		gcinclude.MoveToCurrent(sets.Chakra,sets.CurrentGear);
	-- /THF
	elseif string.contains(ability.Name, 'Steal') then
		gcinclude.MoveToCurrent(sets.Steal,sets.CurrentGear);
	elseif string.contains(ability.Name, 'Sneak Attack') then
		gcinclude.MoveToCurrent(sets.SneakAttack,sets.CurrentGear);
	elseif string.contains(ability.Name, 'Flee') then
		gcinclude.MoveToCurrent(sets.Flee,sets.CurrentGear);
	elseif string.contains(ability.Name, 'Trick Attack') then
		gcinclude.MoveToCurrent(sets.TrickAttack,sets.CurrentGear);
	elseif string.contains(ability.Name, 'Mug') then
		gcinclude.MoveToCurrent(sets.Mug,sets.CurrentGear);
	-- /WHM
	elseif string.contains(ability.Name, 'Divine Seal') then
		gcinclude.MoveToCurrent(sets.DivineSeal,sets.CurrentGear);
	-- /BLM
	elseif string.contains(ability.Name, 'Elemental Seal') then
		gcinclude.MoveToCurrent(sets.ElementalSeal,sets.CurrentGear);
	-- /RNG
	elseif string.contains(ability.Name, 'Sharpshot') then
		gcinclude.MoveToCurrent(sets.Sharpshot,sets.CurrentGear);
	elseif string.contains(ability.Name, 'Scavenge') then
		gcinclude.MoveToCurrent(sets.Scavenge,sets.CurrentGear);
	elseif string.contains(ability.Name, 'Camouflage') then
		gcinclude.MoveToCurrent(sets.Camouflage,sets.CurrentGear);
	elseif string.contains(ability.Name, 'Barrage') then
		gcinclude.MoveToCurrent(sets.Barrage,sets.CurrentGear);	
	-- /SAM
	elseif string.contains(ability.Name, 'Warding Circle') then
		gcinclude.MoveToCurrent(sets.WardingCircle,sets.CurrentGear);
	elseif string.contains(ability.Name, 'Third Eye') then
		gcinclude.MoveToCurrent(sets.ThirdEye,sets.CurrentGear);
	elseif string.contains(ability.Name, 'Hasso') then
		gcinclude.MoveToCurrent(sets.Hasso,sets.CurrentGear);
	elseif string.contains(ability.Name, 'Meditate') then
		gcinclude.MoveToCurrent(sets.Meditate,sets.CurrentGear);
	elseif string.contains(ability.Name, 'Seigan') then
		gcinclude.MoveToCurrent(sets.Seigan,sets.CurrentGear);
	-- /PLD
	elseif string.match(ability.Name, 'Holy Circle') then
		gcinclude.MoveToCurrent(sets.HolyCircle,sets.CurrentGear);
	elseif string.match(ability.Name, 'Shield Bash') then
		gcinclude.MoveToCurrent(sets.ShieldBash,sets.CurrentGear);
	elseif string.contains(ability.Name, 'Sentinel') then
		gcinclude.MoveToCurrent(sets.Sentinel,sets.CurrentGear);	
	elseif string.contains(ability.Name, 'Cover') then
		gcinclude.MoveToCurrent(sets.Cover,sets.CurrentGear);
	-- /DRG
	elseif string.contains(ability.Name, 'Ancient Circle') then
		gcinclude.MoveToCurrent(sets.AncientCircle,sets.CurrentGear);	
	elseif string.contains(ability.Name, 'Jump') then
		gcinclude.MoveToCurrent(sets.Jumps,sets.CurrentGear);
	-- /DRK
	elseif string.contains(ability.Name, 'Arcane Circle') then
		gcinclude.MoveToCurrent(sets.ArcaneCircle,sets.CurrentGear);
	elseif string.contains(ability.Name, 'Last Resort') then
		gcinclude.MoveToCurrent(sets.LastResort,sets.CurrentGear);
	elseif string.match(ability.Name, 'Weapon Bash') then
		gcinclude.MoveToCurrent(sets.WeaponBash,sets.CurrentGear);
	elseif string.contains(ability.Name, 'Souleater') then
		gcinclude.MoveToCurrent(sets.Souleater,sets.CurrentGear);		
	end
	gcinclude.EquipTheGear(sets.CurrentGear);		-- Equip the composited HandleAbility set
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

profile.HandlePrecast = function()
    local spell = gData.GetAction();
	local obi;
	local mSet;
		
	-- Only gear swap if this flag is trues
	if gcdisplay.GetToggle('GSwap') == false then		
		return;
	end

	-- Clear out the CurrentGear in case of leftovers
	gcinclude.ClearSet(sets.CurrentGear);
	
	-- Equip the precast gear set
	if spell.Skill == 'Singing' then
		gcinclude.MoveToCurrent(sets.Song_Precast,sets.CurrentGear);
	else
		gcinclude.MoveToCurrent(sets.Precast,sets.CurrentGear);
			
		-- See if an elemental obi should be equipped
		obi = gcinclude.CheckEleSpells(spell.Name,gcinclude.MagicEleAcc,gcinclude.OBI,nil);
		if obi ~= nil then
			sets.CurrentGear['Waist'] = obi;
		end
	end
	
	gcinclude.EquipTheGear(sets.CurrentGear);	
end		-- HandlePrecast

--[[
	HandleMidcast is the second function invoked when a player casts a spell. It equips gear appropriate for 
	magic skill, duration, magic attack bonus, magic accuracy, and potency. There's an order to how the pieces 
	are loaded: INT/MND, spell specific, macc, magic skill, obi, ele swap	
--]]

profile.HandleMidcast = function()
	local spell = gData.GetAction();
	
	if gcdisplay.GetToggle('GSwap') == false then		-- Only gear swap if this flag is true	
		return;
	end

	-- Clear out the CurrentGear in case of leftovers
	gcinclude.ClearSet(sets.CurrentGear);
	
	-- If we're dealing with a song, a different midcast routine should be called
	
	if spell.Skill == 'Singing' then
		profile.HandleSongMidcast();
	else
		-- Call the common HandleMidcast now
		gcinclude.HandleMidcast(false);
	end
	
	-- Equip the composited midcast set
	gcinclude.EquipTheGear(sets.CurrentGear);
end		-- HandleMidcast

--[[
	HandleSongMidcast is like gcinclude.HandleMidcast except it is for songs instead of magic
	spells.
--]]

function profile.HandleSongMidcast()
	local spell = gData.GetAction();

	-- Clear out the CurrentGear in case of leftovers
	gcinclude.ClearSet(sets.CurrentGear);
	
	profile.prioritySongMidCast = string.upper(profile.prioritySongMidCast);
	for i = 1,string.len(profile.prioritySongMidCast),1 do
		cKey = string.sub(profile.prioritySongMidCast,i,i);
		
		if cKey == 'A' then				-- midcast gear	
			gcinclude.MoveToCurrent(sets.Midcast,sets.CurrentGear);
		elseif cKey == 'B' then			-- Enfeeble/Enhancing gear
			if string.find(profile.SongSkill['Enhancing'],spell.Name) ~= nil then		
				gcinclude.MoveToCurrent(sets.Enhancing,sets.CurrentGear);			
			elseif string.find(profile.SongSkill['Enfeebling'],spell.Name) ~= nil then	
				gcinclude.MoveToCurrent(sets.Enfeebling,sets.CurrentGear);	
			end
		elseif cKey == 'C' then			-- Song skill type
			-- Determine the type of song from the song name
			local sType = nil;
			
			for k,str in pairs(profile.songs) do
				if string.find(str,spell.Name) ~= nil then
					sType = k;
					break;
				end
			end
		
			if sType ~= nil then
				local tTbl,tTblC;
				tTbl,tTblC = BardSongToSet(sType);			
				gcinclude.MoveToCurrent(tTbl,sets.CurrentGear);
			else
				print(chat.header('HandleSongMidcast'):append(chat.message('Unrecognized song: ' .. spell.Name)));
			end			
		elseif cKey == 'D' then			-- Magical accuracy
			if gcdisplay.GetToggle('acc') == true then
				gcinclude.MoveToCurrent(sets.Macc,sets.CurrentGear);
			end
		end
	end			
end		-- HandleSongMidcast

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
		
	gcinclude.MoveToCurrent(sets.Preshot,sets.CurrentGear);
	gcinclude.EquipTheGear(sets.CurrentGear);
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
	
	gcinclude.MoveToCurrent(sets.Midshot,sets.CurrentGear);

	-- Equip the composited Midshot set
	gcinclude.EquipTheGear(sets.CurrentGear);	
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
	gcinclude.EquipTheGear(sets.CurrentGear);
end		-- HandleWeaponskill

return profile;