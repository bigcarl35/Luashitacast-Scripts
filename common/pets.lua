local pets = {};

--[[
    This component contains all routines that deal with pets

    List of routines-
        Subroutines:
            Call911                 Determines spirit to summon and summons it
            FavoredJugPets          Updates jug pet list indicating favorites
            HandlePetAction         Handles all pet actions not specific to one job
            ptt                     Pet to Target information

        Functions:
            fElementByPetName       Determines element of smn pet
            lfFindAllJugs           Determines which jugs the player has that are accessible
            fIsValidJugPet          Determines if passed name is a valid jug pet
            fPetReward              Scans all containers for pet food returns best
            fSummonerPet            Determines if pet is a smn avatar/spirit
--]]

-- Table of all pet food (while associated with BST, any job can equip)
pets.tPetFood = {
    [1] = { ['name'] = 'pet food alpha',  ['lvl'] = 12, ['have'] = false },
    [2] = { ['name'] = 'pet food beta',   ['lvl'] = 24, ['have'] = false },
    [3] = { ['name'] = 'pet fd. gamma',   ['lvl'] = 36, ['have'] = false },
    [4] = { ['name'] = 'pet food delta',  ['lvl'] = 48, ['have'] = false },
    [5] = { ['name'] = 'pet fd. epsilon', ['lvl'] = 60, ['have'] = false },
    [6] = { ['name'] = 'pet food zeta',   ['lvl'] = 72, ['have'] = false }
};

-- list of all jug pets available on HorizonXI.
pets.tJugPets = {
    ['carrot broth']     = { ['name'] = 'Hare Familiar',    ['min'] = 23, ['max'] = 35, ['have'] = false, ['fav'] = false },
    ['herbal broth']     = { ['name'] = 'Sheep Familiar',   ['min'] = 23, ['max'] = 35, ['have'] = false, ['fav'] = false },
    ['humus']            = { ['name'] = 'Flowerpot Bill',   ['min'] = 28, ['max'] = 40, ['have'] = false, ['fav'] = false },
    ['meat broth']       = { ['name'] = 'Tiger Familiar',   ['min'] = 28, ['max'] = 40, ['have'] = false, ['fav'] = false },
    ['grass. broth']     = { ['name'] = 'Flytrap Familiar', ['min'] = 28, ['max'] = 40, ['have'] = false, ['fav'] = false },
    ['carrion broth']    = { ['name'] = 'Lizard Familiar',  ['min'] = 33, ['max'] = 45, ['have'] = false, ['fav'] = false },
    ['bug broth']        = { ['name'] = 'Mayfly Familiar',  ['min'] = 33, ['max'] = 45, ['have'] = false, ['fav'] = false },
    ['mole broth']       = { ['name'] = 'Eft Familiar',     ['min'] = 33, ['max'] = 45, ['have'] = false, ['fav'] = false },
    ['tree sap']         = { ['name'] = 'Beetle Familiar',  ['min'] = 38, ['max'] = 45, ['have'] = false, ['fav'] = false },
    ['antica broth']     = { ['name'] = 'Antlion Familiar', ['min'] = 38, ['max'] = 50, ['have'] = false, ['fav'] = false },
    ['fish broth']       = { ['name'] = 'Crab Familiar',    ['min'] = 23, ['max'] = 55, ['have'] = false, ['fav'] = false },
    ['blood broth']      = { ['name'] = 'Mite Familiar',    ['min'] = 43, ['max'] = 55, ['have'] = false, ['fav'] = false },
    ['f. carrot broth']  = { ['name'] = 'Keeneared Steffi', ['min'] = 43, ['max'] = 75, ['have'] = false, ['fav'] = false },
    ['s. herbal broth']  = { ['name'] = 'Lullaby Melodia',  ['min'] = 43, ['max'] = 75, ['have'] = false, ['fav'] = false },
    ['rich humus']       = { ['name'] = 'Flowerpot Ben',    ['min'] = 51, ['max'] = 75, ['have'] = false, ['fav'] = false },
    ['w. meat broth']    = { ['name'] = 'Saber Siravarde',  ['min'] = 51, ['max'] = 75, ['have'] = false, ['fav'] = false },
    ['seedbed soil']     = { ['name'] = 'Funguar Familiar', ['min'] = 33, ['max'] = 65, ['have'] = false, ['fav'] = false },
    ['qdv. bug broth']   = { ['name'] = 'Shellbuster Orob', ['min'] = 53, ['max'] = 75, ['have'] = false, ['fav'] = false },
    ['c. carrion broth'] = { ['name'] = 'Coldblood Como',   ['min'] = 53, ['max'] = 75, ['have'] = false, ['fav'] = false },
    ['fish oil broth']   = { ['name'] = 'Courier Carrie',   ['min'] = 23, ['max'] = 75, ['have'] = false, ['fav'] = false },
    ['alchemist water']  = { ['name'] = 'Homunculus',       ['min'] = 23, ['max'] = 75, ['have'] = false, ['fav'] = false },
    ['n. grass. broth']  = { ['name'] = 'Voracious Audrey', ['min'] = 53, ['max'] = 75, ['have'] = false, ['fav'] = false },
    ['l. mole broth']    = { ['name'] = 'Ambusher Allie',   ['min'] = 58, ['max'] = 75, ['have'] = false, ['fav'] = false },
    ['scarlet sap']      = { ['name'] = 'Panzer Galahad',   ['min'] = 63, ['max'] = 75, ['have'] = false, ['fav'] = false },
    ['c. blood broth']   = { ['name'] = 'Lifedrinker Lars', ['min'] = 63, ['max'] = 75, ['have'] = false, ['fav'] = false },
    ['f. antica broth']  = { ['name'] = 'Chopsuey Chucky',  ['min'] = 63, ['max'] = 75, ['have'] = false, ['fav'] = false },
    ['sun water']        = { ['name'] = 'Amigo Sabotender', ['min'] = 75, ['max'] = 75, ['have'] = false, ['fav'] = false }
};

-- BST pet special attacks
pets.BstPetAttack = {
    'Foot Kick','Whirl Claws','Big Scissors','Tail Blow','Blockhead','Sensilla Blades','Tegmina Buffet','Lamb Chop','Sheep Charge','Pentapeck',
    'Recoil Dive','Frogkick','Queasyshroom','Numbshroom','Shakeshroom','Nimble Snap','Cyclotail','Somersault','Tickling Tendrils','Sweeping Gouge',
    'Grapple','Double Claw','Spinning Top','Suction','Tortoise Stomp','Power Attack','Rhino Attack','Razor Fang','Claw Cyclone','Crossthrash',
    'Scythe Tail','Ripper Fang','Chomp Rush','Pecking Flurry','Sickle Slash','Mandibular Bite','Wing Slap','Beak Lunge','Head Butt','Wild Oats',
    'Needle Shot','Disembowel','Extirpating Salvo','Mega Scissors','Back Heel','Hoof Volley','Fluid Toss','Fluid Spread'
};
pets.BstPetMagicalAttack = {
    'Gloom Spray','Fireball','Acid Spray','Molting Plumage','Cursed Sphere','Nectarous Deluge','Charged Whisker','Nepenthic Plunge'
};
pets.BstPetMagicalAccuracy = { 'Toxic Spit','Acid Spray','Leaf Dagger','Venom Spray','Venom','Dark Spore','Sandblast','Dust Cloud',
    'Stink Bomb','Slug Family','Intimidate','Gloeosuccus','Spider Web','Filamented Hold','Choke Breath','Blaster','Snow Cloud','Roar',
    'Palsy Pollen','Spore','Brain Crush','Choke Breath','Silence Gas','Chaotic Eye','Sheep Song','Soporific','Predatory Glare',
    'Sudden Lunge','Numbing Noise','Jettatura','Bubble Shower','Spoil','Scream','Noisome Powder','Acid Mist','Rhinowrecker',
    'Swooping Frenzy','Venom Shower','Corrosive Ooze','Spiral Spin','Infrasonics','Hi-Freq Field','Purulent Ooze','Foul Waters',
    'Sandpit','Infected Leech','Pestilent Plume'
};

pets.SmnBPSkill = { 'Shining Ruby','Glittering Ruby','Crimson Howl','Inferno Howl','Frost Armor','Crystal Blessing','Aerial Armor','Hastega II',
    'Fleet Wind','Hastega','Earthen Ward','Earthen Armor','Rolling Thunder','Lightning Armor','Soothing Current','Ecliptic Growl','Heavenward Howl',
    'Ecliptic Howl','Noctoshield','Dream Shroud','Altana\'s Favor','Reraise','Reraise II','Reraise III','Raise','Raise II','Raise III','Wind\'s Blessing'
};
pets.SmnBPMagical = { 'Searing Light','Meteorite','Holy Mist','Inferno','Fire II','Fire IV','Meteor Strike','Conflag Strike','Diamond Dust',
    'Blizzard II','Blizzard IV','Heavenly Strike','Aerial Blast','Aero II','Aero IV','Wind Blade','Earthen Fury','Stone II','Stone IV','Geocrush',
    'Judgement Bolt','Thunder II','Thunder IV','Thunderstorm','Thunderspark','Tidal Wave','Water II','Water IV','Grand Fall','Howling Moon',
    'Lunar Bay','Ruinous Omen','Somnolence','Nether Blast','Night Terror','Level ? Holy'
};
pets.SmnBPPhysical = { 'Punch','Rock Throw','Barracuda Dive','Claw','Axe Kick','Shock Strike','Camisado','Poison Nails',
    'Moonlit Charge','Crescent Fang','Rock Buster','Tail Whip','Double Punch','Megalith Throw','Double Slap','Eclipse Bite','Mountain Buster',
    'Spinning Dive','Predator Claws','Rush','Chaotic Strike'
};
pets.SmnBPAccuracy = { 'Healing Ruby','Healing Ruby II','Whispering Wind','Spring Water','Diamond Storm','Sleepga','Shock Squall','Slowga',
    'Tidal Roar','Pavor Nocturnus','Ultimate Terror','Nightmare','Mewing Lullaby','Eerie Eye'
};
pets.SmnBPHybrid = { 'Burning Strike','Flaming Crush' };

pets.SmnBPRageList = { 'Searing Light','Howling Moon','Inferno','Earthen Fury','Tidal Wave','Aerial Blast','Diamond Dust','Judgment Bolt',
    'Ruinous Omen','Punch','Rock Throw','Barracuda Dive','Claw','Axe Kick','Shock Strike','Camisado','Poison Nails','Moonlit Charge',
    'Crescent Fang','Fire II','Stone II','Water II','Blizzard II','Thunder II','Aero II','Thunderspark','Rock Buster','Burning Strike',
    'Tail Whip','Double Punch','Megalith Throw','Double Slap','Meteorite','Fire IV','Stone IV','Water IV','Aero IV','Blizzard IV','Thunder IV',
    'Eclipse Bite','Nether Blast','Flaming Crush','Mountain Buster','Spinning Dive','Predator Claws','Rush','Chaotic Strike'
};
pets.SmnBPWardList = { 'Healing Ruby','Somnolence','Lunar Cry','Shining Ruby','Aerial Armor','Frost Armor','Nightmare','Rolling Thunder',
    'Lunar Roar','Slowga','Ultimate Terror','Whispering Wind','Crimson Howl','Sleepga','Lightning Armor','Ecliptic Howl','Glittering Ruby',
    'Earthen Ward','Spring Water','Hastega','Noctoshield','Ecliptic Growl','Dream Shroud','Healing Ruby II'
};

-- List of all pet commands
pets._PetCommands = 'FIGHT,HEEL,STAY,LEAVE,SIC,READY,STEADY WING,DISMISS,ASSAULT,RELEASE,RETREAT';

--[[
    HandlePetAction processes the passed in pet action so that the appropriate gear set
    is equipped.

    Parameter
        PetAction       Structure containing pet's action details

    Note: BST's Sic and Ready commands send the action name to this routine. That's
    why table searches are done. Also, SMN's Blood Pacts also identify the pet skill.
    Unlike BST, SMN's MidBP is split into groups
--]]

function pets.HandlePetAction(PetAction)
    local pet = gData.GetPet();
    local player = gData.GetPlayer();
    local sn;

    -- Ensure there's a pet
    if pet == nil or pet.Name == nil then
        return;
    end

    -- Check for BST's Sic or Ready attack skills
    if table.find(pets.BstPetAttack,PetAction.Name) ~= nil or
       table.find(pets.BstPetMagicAccuracy,PetAction.Name) ~= nil or
       table.find(pets.BstPetMagicAttack,PetAction.Name ~= nil then
        sn = utilities.fGetTableByName('PC_Sic_Ready');
        if sn ~= nil then
            gear.MoveToDynamicGS(sn,crossjobs.Sets.CurrentGear,false,'PC_Sic_Ready');
        end
    -- Next, SMN Blood pacts
    elseif table.find(pets.SmnBPSkill,PetAction.Name) ~= nil or
       table.find(pets.SmnBPMagical,PetAction.Name) ~= nil or
       table.find(pets.SmnBPPhysical,PetAction.Name) ~= nil or
       table.find(pets.SmnBPAccuracy,PetAction.Name) ~= nil or
       table.find(pets.SmnBPHybrid,PetAction.Name) ~= nil then
        gear.MoveToDynamicGS(gProfile.Sets.MidBP,crossjobs.Sets.CurrentGear,false,'MidBP');
    -- And DRG's Steady Wing'
    elseif PetAction.Name == 'Steady Wing' then
        sn = utilities.fGetTableByName('PC_Steady_Wing');
        if sn ~= nil then
            gear.MoveToDynamicGS(sn,crossjobs.Sets.CurrentGear,false,'PC_Steady_Wing');
        end
    -- Lastly, any other leftover commands
    else
        local sName = 'PC' .. string.gsub(PetAction.Name,' ','_');
        sn = utilities.fGetTableByName(sName);
        if sn ~= nil then
            gear.MoveToDynamicGS(sn,crossjobs.Sets.CurrentGear,false,sName);
        end
    end
    gear.EquipTheGear(sets.CurrentGear);
end		-- pets.HandlePetAction

--[[
    fIsValidJugPet determines if the passed item name is a valid jug pet name

    Returned
        T/F, is the jug pet name valid
--]]

function pets.fIsValidJugPet(sName)

    if sName == nil then
        return false;
    end

    sName = string.lower(sName);
    for i,j in pairs(pets.tJugPets) do
        if string.match(i,sName) then
            return(true);
        end
    end

    return false;
end     -- pets.fIsValidJugPet

--[[
    FavoredJugPets determines if there's any favored BST jug pets and updates
    tJugPets accordingly. This function needs to be run only once.
--]]

function pets.FavoredJugPets()
    local player = gData.GetPlayer();
    local t1,t2;

    if player.MainJob == 'BST' and gProfile.FavoredJugPets ~= nil then
        for i,j in pairs(gProfile.FavoredJugPets) do
            t1 = string.lower(i);
            for ii,jj in pairs(pets.tJugPets) do
                t2 = string.lower(jj['name']);
                if t1 == t2 then
                    pets.tJugPets[ii]['fav'] = j['favored'];
                    break;
                end
            end
        end
    end
end     -- pets.FavoredJugPets

--[[
    FindAllJugs searches all the accessible storage areas for pet jugs and
    updates tJugPets if any are found and level appropriate.
--]]

function lfFindAllJugs()
    local resources = AshitaCore:GetResourceManager();
    local player = gData.GetPlayer();
    local tStorage = gVars.EQUIPABLE_NONHOLIDAY;
    local iCount = 0;

    -- Clear the table ownership settings
    for i,j in pairs(pets.tJugPets) do
        j['have'] = false;
    end

    -- Now walk the equipable (in the field) storage areas
    for k,v in ipairs(tStorage) do
        local containerID = v['id'];
        -- then loop through the selected container looking for a jug pet's broth
        for j = 1,inventory:GetContainerCountMax(containerID),1 do
            local itemEntry = inventory:GetContainerItem(containerID, j);
            if (itemEntry.Id ~= 0 and itemEntry.Id ~= 65535) then
                local item = resources:GetItemById(itemEntry.Id);
                if item ~= nil then
                    -- then check the master list of jug pets
                    for kk,tpf in pairs(pets.tJugPets) do
                        if kk == string.lower(item.Name[1]) then
                            if (tpf['min'] <= player.MainJobSync) and
                                (tpf['max'] >= player.MainJobSync) then
                                -- finally, this one is possible to be selected
                                pets.tJugPets[kk]['have'] = true;
                                iCount = iCount + 1;
                            end
                        end
                    end
                end
            end
        end
    end
    return (iCount > 0);
end     -- lfFindAllJugs

--[[
    fWhichJugToEquip walks the master list and determines which jug should be
    equipped. It tries to comply with a favored jug. It returns the highest
    level jug to be equipped or nil if no jug found.

    Returned:
        Name of jug to equip or nil
--]]

function pets.fWhichJugToEquip()
    local player = gData.GetPlayer();
    local favored = nil;
    local nonfavored = nil;

    -- First see if a Jug is already equipped

    if lfFindAllJugs() == true then
        -- Nexc find the best favored and nonfavored
        for i,j in pairs(pets.tJugPets) do
            if j['have'] == true then
                if j['fav'] == true then
                    --  favored
                    if favored == nil then
                        favored = i;
                    else
                        if pets.tJugPets[favored]['max'] < j['max'] and pets.tJugPets[favored]['max'] < player.MainJobSync then
                            favored  = i;
                        end
                    end
                else
                    -- non-favored
                    if nonfavored == nil then
                        nonfavored = i;
                    else
                        if pets.tJugPets[nonfavored]['max'] < j['max'] and pets.tJugPets[nonfavored]['max'] < player.MainJobSync then
                            nonfavored  = i;
                        end
                    end
                end
            end
        end

        -- At this point we should have what we need
        if favored == nil and nonfavored == nil then
            print(chat.message('Info: No Jug Pets that can be used are available'));
            return(nil);
        elseif favored ~= nil and nonfavored == nil then
            return(favored);
        elseif nonfavored ~= nil and favored == nil then
            return(nonfavored);
        else
            -- Determine if favored is a better match than nonfavored
            if pets.tJugPets[favored]['max'] >= pets.tJugPets[nonfavored]['max'] then
                return(favored);
            else
                return(nonfavored);
            end
        end
    else
        print(chat.message('Info: No Jug Pets that can be used are available'));
        return(nil);
    end
end     -- pets.fWhichJugToEquip

--[[
    fPetReward scans all equipable storage containers for all of the pet foods
    you can equip. If you indicated a specific pet food, it focuses on that.
    If no food specified it looks for the first one it can equip (either starting
    and the maximum entry or the first entry). It returns the first valid pet
    food it found.

    Parameters
        sFood       Pet food to equip
        bMax        Scan from last to first or visa versa

    Returned
        T/F         Was a piece of food successfully found
--]]

function pets.fPetReward(sFood,bMax)
    local inventory = AshitaCore:GetMemoryManager():GetInventory();
    local resources = AshitaCore:GetResourceManager();
    local player = gData.GetPlayer();
    local tStorage = gVars.EQUIPABLE_NONHOLIDAY;
    local containerID;
    local i1,i2,step;
    local _ammo = 4;	-- Lock # for ammo slot

    if bMax == nil then
        bMax = true;
    end

    -- Make sure ammo slot isn't locked
    if locks.tLocks[_ammo]['lock'] == true then
        print(chat.message('Warning: Ammo slot is locked. Unable to equip any pet food'));
        return false;
    end

    -- Reset the pet food indicators
    for i,j in ipairs(pets.tPetFood) do
        j['have'] = false;
    end

    -- Now, note which pet foods the player has
    for i,j in ipairs(tStorage) do
        containerID = j['id'];
        -- then loop through the container
        for k = 1, inventory:GetContainerCountMax(containerID), 1 do
            local itemEntry = inventory:GetContainerItem(containerID, k);
            if (itemEntry.Id ~= 0 and itemEntry.Id ~= 65535) then
                local item = resources:GetItemById(itemEntry.Id);
                if item ~= nil then
                    local sName = string.lower(item.Name[1]);
                    for ii,jj in ipairs(pets.tPetFood) do
                        if sName == jj['name'] then
                            jj['have'] = true;
                        end
                    end
                end
            end
        end
    end

    -- Determine order to process
    local petFoodCount = #pets.tPetFood;
    if bMax == true then
        i1 = 1; i2 = petFoodCount; step = 1;
    else
        i1 = petFoodCount; i2 = 1; step = -1;
    end

    -- Then see if you can find the preferred food
    local iFound = -1;
    if sFood ~= nil then
        for i = i1,i2,step do
            if string.lower(sFood) == pets.tPetFood[i]['name'] and
                pets.tPetFood[i]['have'] == true and
                pets.tPetFood[i]['lvl'] <= player.MainJobSync then
                iFound = i;
                break;
            end
        end
    end

    -- Or the highest level food you can equip
    if iFound == -1 then
        for i = i1,i2,step do
            if pets.tPetFood[i]['have'] == true and
                pets.tPetFood[i]['lvl'] <= player.MainJobSync then
                iFound = i;
                break;
            end
        end
    end

    if iFound > 0 then
        local sName = pets.tPetFood[iFound]['name'];
        gFunc.ForceEquip('Ammo', sName);
        print(chat.message('Equipping: ' .. sName));
        return true;
    elseif sName ~= nil then
        print(chat.message('Warning: ' .. sFood .. ' not found or you cannot equip it.'));
        return false;
    else
        print(chat.message('Warning: No equipable pet food found.'));
        return false;
    end
end		-- pets.PetReward

--[[
    ptt provides a simple answer to a request until a better answer
    can be formulated. It's intended to help classes that control pets.
    It displays the distance between the player and the pet, the player
    and the target, and the pet and the target.
--]]

function pets.ptt()
    local pEntity = AshitaCore:GetMemoryManager():GetEntity();
    local myIndex = AshitaCore:GetMemoryManager():GetParty():GetMemberTargetIndex(0);
    local petIndex = AshitaCore:GetMemoryManager():GetEntity():GetPetTargetIndex(myIndex);
    local targetIndex = gData.GetTargetIndex();
    local x,y,z;

    print(' ');
    if petIndex ~= nil and petIndex > 0 then
        x = math.sqrt(AshitaCore:GetMemoryManager():GetEntity():GetDistance(petIndex));
        print(chat.message(string.format('Player to pet: %.1f',x) .. 'm'));
    else
        print(chat.message('You have no pet'));
    end

    if targetIndex ~= nil and targetIndex > 0 then
        x = math.sqrt(AshitaCore:GetMemoryManager():GetEntity():GetDistance(targetIndex));
        print(chat.message(string.format('Player to target: %.1f',x) .. 'm'));
    else
        print(chat.message('You have no target'));
    end

    if petIndex ~= nil and petIndex > 0 and targetIndex ~= nil and targetIndex > 0 then
        x = math.pow(pEntity:GetLocalPositionX(petIndex) - pEntity:GetLocalPositionX(targetIndex),2);
        y = math.pow(pEntity:GetLocalPositionY(petIndex) - pEntity:GetLocalPositionY(targetIndex),2);
        print(chat.message(string.format('Pet to target: %.1f', math.sqrt(x+y)) .. 'm'));
    end
end		-- pets.ptt

--[[
    fSummonerPet determines if the player has a SMN summoned pet.

    Returned:
        True/False
--]]

function pets.fSummonerPet()
    local pet = gData.GetPet();

    return (pet ~= nil and pets.fElementByPetName(pet.Name) ~= nil);
end     -- pets.fSummonerPet

--[[
    fElementByPetName determines what element is associated with the currently
    summoned SMN avatar/spirit and returns it.

    Returned:
        element of current pet or nil
--]]

function pets.fElementByPetName(pName)
    local lcName;
    local ele = nil;

    if pName == nil then
        return nil;
    end

    lcName = string.lower(pName);

    for i,j in pairs(gVars.tElemental_gear['staff']) do
        if string.find(utilities._AllElements,i) ~= nil then
            if table.find(j['Summons'],lcName) ~= nil then
                ele = i;
                break;
            end
        end
    end

    return ele;
end		-- pets.fElementByPetName

--[[
    Call911 determines which elemental spirit should be summoned and summons it. Intended as an emergency
    summons mechanism, what makes a spirit the best is how often it can cast spells. This depends on your
    summoning skill (max and current values), day's element, and weather's element, plus some other specific
    settings. Use the following formula:

        Casting Time = 48s + (Max Summoning Skill - Current Summoning Skill)/3 + adjustments

    where adjustments are:
        Spirit's element matches/opposes the day's element: -3/+3 secs
        Spirit's element matches/opposes the weather's element: -2/+2 secs
        Summoner wearing "Summoner's Spats": -5 secs
        Astral Flow enabled: -5 secs
        Light spirit in healing mode or buffering mode: 1/2 casting time

    Since this decision is about the initial cast, only matching the day's element and/or the weather's
    element will be considered here. Summoner's spats should be done through inline conditionals and the
    Light Spirit modes should be considered through the defaultSpirit setting in the summoner's job file.
    The affect of astal flow will happen regardless of which spirit is summoned.
--]]

function pets.Call911()
    local player = gData.GetPlayer();
    local pet = gData.GetPet();
    local environ = gData.GetEnvironment();
    local dayEle = string.lower(environ.DayElement);
    local weatherEle = string.lower(environ.WeatherElement);
    local iWhich = 0;
    local iCurrent = 0;
    local iScore;
    local rec = {
        [1] = { ['spirit'] = 'fire spirit',    ['SID'] = 288, ['ele'] = 'fire',    ['weak'] = 'water',   ['have'] = false, ['cd'] = false },
        [2] = { ['spirit'] = 'ice spirit',     ['SID'] = 289, ['ele'] = 'ice',     ['weak'] = 'fire',    ['have'] = false, ['cd'] = false },
        [3] = { ['spirit'] = 'air spirit',     ['SID'] = 290, ['ele'] = 'wind',    ['weak'] = 'ice',     ['have'] = false, ['cd'] = false },
        [4] = { ['spirit'] = 'earth spirit',   ['SID'] = 291, ['ele'] = 'earth',   ['weak'] = 'wind',    ['have'] = false, ['cd'] = false },
        [5] = { ['spirit'] = 'thunder spirit', ['SID'] = 292, ['ele'] = 'thunder', ['weak'] = 'earth',   ['have'] = false, ['cd'] = false },
        [6] = { ['spirit'] = 'water spirit',   ['SID'] = 293, ['ele'] = 'water',   ['weak'] = 'thunder', ['have'] = false, ['cd'] = false },
        [6] = { ['spirit'] = 'light spirit',   ['SID'] = 294, ['ele'] = 'light',   ['weak'] = 'dark',    ['have'] = false, ['cd'] = false },
        [7] = { ['spirit'] = 'dark spirit',    ['SID'] = 295, ['ele'] = 'dark',    ['weak'] = 'light',   ['have'] = false, ['cd'] = false }
        };

    -- Player must be either SMN/ or /SMN to use this function
    if not (player.MainJob == 'SMN' or player.SubJob == 'SMN') then
        print(chat.message('Warning: only a SMN/ or /SMN can invoke "911". Ignoring command'));
        return;
    end

    if pet ~= nil then          -- Any existing pet will block an elemental spirit summons
        print(chat.message('Warning: You already have a pet. Ignoring command'));
        return;
    end

    if player.MP < 10 then        -- All spirit summons cost 10mp
        print(chat.message('Warning: Insufficient mana to summon any element spirit. Ignoring command'))
        return;
    end

    -- Walk through the list of spirits determining the relative elemental score and whether
    -- the player has the spell and/or is it on cooldown.
    for i,j in ipairs(rec) do
        iScore = 0;
        -- Make sure that the player knows the spell and that it's not on cool down
        j['have'] = (AshitaCore:GetMemoryManager():GetPlayer():HasSpell(j['SID']) == true);
        j['cd'] = (AshitaCore:GetMemoryManager():GetRecast():GetSpellTimer(j['SID']) ~= 0);
        -- Now process the record
        if j['have'] == true and j['cd'] == false then
            -- First check the day's element
            if j['ele'] == dayEle then
                iScore = iScore - 3;
            elseif j['weak'] == dayEle then
                iScore = iScore + 3;
            end
            -- Next check the weather's element
            if j['ele'] == weatherEle then
                iScore = iScore - 2;
            elseif j['weak'] == WeatherElement then
                iScore = iScore + 2;
            end

            -- record it
            j['score'] = iScore;

            -- if appropriate, track the current "best" option
            if iScore < iCurrent then
                iWhich = i;
                iCurrent = iScore;
            end
        end
    end

    -- Now the fun begins. If a spirit was selected, then we're good. Otherwise we need to see about what the
    -- player has picked for a default spirit
    if iWhich == 0 then
        if gProfile.settings.defaultSpirit ~= nil then
            -- Since there is a default setting, find it in the list
            for i,j in ipairs(rec) do
                if j['spirit'] == string.lower(gProfile.settings.defaultSpirit) then
                    -- Since found, check to see if player has it and is it off cooldown
                    if j['have'] == true and j['cd'] == false then
                        iWhich = i;
                        break;
                    else
                        print(chat.message('Info: Default spirit: ' .. gProfile.settings.defaultSpirit .. ' is either unknown or on cool down. Defaulting to a spirit can summon'))
                        break;
                    end
                end
            end
        end

        -- It's possible that the default spirit had an issue. Pick the first entry that the player has and is not on cool down
        if iWhich == 0 then
            for i,j in ipairs(rec) do
                if rec['have'] == true and rec['cd'] == false then
                    iWhich = i;
                    break;
                end
            end

            if iWhich == 0 then
                -- If one isn't selected at this point, then no spirit is possible. Notify player
                print(chat.message('Warning: No spirit can be summoned at this time'));
                return;
            end
        end
    end

    -- Process the selected elemental spirit
    print(chat.message('Info: Summoning ' .. j[iWhich]['spirit']))''
    local sCmd = '/ma "' .. j[iWhich]['spirit'] .. '" <me>';
    AshitaCore:GetChatManager():QueueCommand(1, sCmd);
    return;
end     -- pets.Call911

return pets;
