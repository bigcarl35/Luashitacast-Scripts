local pets = T{};

local crossjobs = require('common.crossjobs');
local utilities = require('common.utilities');
local gear      = require('common.gear');

--[[
    This component contains all routines that deal with pets

    List of routines-
        Subroutines:
            FavoredJugPets          Updates jug pet list indicating favorites
            HandlePetAction         Handles all pet actions not specific to one job
            ptt                     Pet to Target information

        Functions:
            fElementByPetName       Determines element of smn pet
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
pets._PetFoodCount = #pets.tPetFood;

-- list of all jug pets available on HorizonXI.
pets.tJugPets = T {
    ['carrot broth']     = { ['name'] = 'Hare Familiar', ['min'] = 23, ['max'] = 35, ['have'] = false, ['fav'] = false },
    ['herbal broth']     = { ['name'] = 'Sheep Familiar', ['min'] = 23, ['max'] = 35, ['have'] = false, ['fav'] = false },
    ['humus']            = { ['name'] = 'Flowerpot Bill', ['min'] = 28, ['max'] = 40, ['have'] = false, ['fav'] = false },
    ['meat broth']       = { ['name'] = 'Tiger Familiar', ['min'] = 28, ['max'] = 40, ['have'] = false, ['fav'] = false },
    ['grass. broth']     = { ['name'] = 'Flytrap Familiar', ['min'] = 28, ['max'] = 40, ['have'] = false, ['fav'] = false },
    ['carrion broth']    = { ['name'] = 'Lizard Familiar', ['min'] = 33, ['max'] = 45, ['have'] = false, ['fav'] = false },
    ['bug broth']        = { ['name'] = 'Mayfly Familiar', ['min'] = 33, ['max'] = 45, ['have'] = false, ['fav'] = false },
    ['mole broth']       = { ['name'] = 'Eft Familiar', ['min'] = 33, ['max'] = 45, ['have'] = false, ['fav'] = false },
    ['tree sap']         = { ['name'] = 'Beetle Familiar', ['min'] = 38, ['max'] = 45, ['have'] = false, ['fav'] = false },
    ['antica broth']     = { ['name'] = 'Antlion Familiar', ['min'] = 38, ['max'] = 50, ['have'] = false, ['fav'] = false },
    ['fish broth']       = { ['name'] = 'Crab Familiar', ['min'] = 23, ['max'] = 55, ['have'] = false, ['fav'] = false },
    ['blood broth']      = { ['name'] = 'Mite Familiar', ['min'] = 43, ['max'] = 55, ['have'] = false, ['fav'] = false },
    ['f. carrot broth']  = { ['name'] = 'Keeneared Steffi', ['min'] = 43, ['max'] = 75, ['have'] = false, ['fav'] = false },
    ['s. herbal broth']  = { ['name'] = 'Lullaby Melodia', ['min'] = 43, ['max'] = 75, ['have'] = false, ['fav'] = false },
    ['rich humus']       = { ['name'] = 'Flowerpot Ben', ['min'] = 51, ['max'] = 75, ['have'] = false, ['fav'] = false },
    ['w. meat broth']    = { ['name'] = 'Saber Siravarde', ['min'] = 51, ['max'] = 75, ['have'] = false, ['fav'] = false },
    ['seedbed soil']     = { ['name'] = 'Funguar Familiar', ['min'] = 33, ['max'] = 65, ['have'] = false, ['fav'] = false },
    ['qdv. bug broth']   = { ['name'] = 'Shellbuster Orob', ['min'] = 53, ['max'] = 75, ['have'] = false, ['fav'] = false },
    ['c. carrion broth'] = { ['name'] = 'Coldblood Como', ['min'] = 53, ['max'] = 75, ['have'] = false, ['fav'] = false },
    ['fish oil broth']   = { ['name'] = 'Courier Carrie', ['min'] = 23, ['max'] = 75, ['have'] = false, ['fav'] = false },
    ['alchemist water']  = { ['name'] = 'Homunculus', ['min'] = 23, ['max'] = 75, ['have'] = false, ['fav'] = false },
    ['n. grass. broth']  = { ['name'] = 'Voracious Audrey', ['min'] = 53, ['max'] = 75, ['have'] = false, ['fav'] = false },
    ['l. mole broth']    = { ['name'] = 'Ambusher Allie', ['min'] = 58, ['max'] = 75, ['have'] = false, ['fav'] = false },
    ['scarlet sap']      = { ['name'] = 'Panzer Galahad', ['min'] = 63, ['max'] = 75, ['have'] = false, ['fav'] = false },
    ['c. blood broth']   = { ['name'] = 'Lifedrinker Lars', ['min'] = 63, ['max'] = 75, ['have'] = false, ['fav'] = false },
    ['f. antica broth']  = { ['name'] = 'Chopsuey Chucky', ['min'] = 63, ['max'] = 75, ['have'] = false, ['fav'] = false },
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
        sn = utilities.fGetTableByName('PC:Sic_Ready');
        if sn ~= nil then
            gear.MoveToDynamicGS(sn,crossjobs.Sets.CurrentGear,false,'PC:Sic_Ready');
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
        sn = utilities.fGetTableByName('PC:Steady_Wing');
        if sn ~= nil then
            gear.MoveToDynamicGS(sn,crossjobs.Sets.CurrentGear,false,'PC:Steady_Wing');
        end
    -- Lastly, any other leftover commands
    else
        local sName = 'PC:' .. PetAction.Name;
        sn = utilities.fGetTableByName(sName);
        if sn ~= nil then
            gear.MoveToDynamicGS(sn,crossjobs.Sets.CurrentGear,false,sName);
        end
    end
    gear.EquipTheGear(sets.CurrentGear);
end		-- pets.HandlePetAction

--[[
    FavoredJugPets determines if there's any favored BST jug pets and updates
    tJugPets accordingly.

    -- simplified
--]]

function pets.FavoredJugPets()
    local player = gData.GetPlayer();

    if player.MainJob == 'BST' and gProfile.FavoredJugs ~= nil then
        for i,j in pairs(gProfile.FavoredJugs) do
            pets.tJugPets[string.lower(j)] == true;
        end
    end
end     -- pets.FavoredJugPets

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
    local tStorage = utilities.EQUIPABLE_NONHOLIDAY;
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
    if bMax == true then
        i1 = 1; i2 = pets._PetFoodCount; step = 1;
    else
        i1 = pets._PetFoodCount; i2 = 1; step = -1;
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

    for i,j in pairs(utilities.tElemental_gear['staff']) do
        if string.find(utilities._AllElements,i) ~= nil then
            if table.find(j['Summons'],lcName) ~= nil then
                ele = i;
                break;
            end
        end
    end

    return ele;
end		-- pets.fElementByPetName

