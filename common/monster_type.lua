local monster_type = {};

--[[
    This component contains all routines that deal with processing inline conditionals

    List of routines-
        Subroutines:
            loadZoneMonstersByID    Loads the monster file associated with the passed zone id

        Functions:
            fMonsterTargetIs        Determines if the target monster is from the specified family/ecosystem
--]]

-- current_zone_monsters is the flattened version of monster_structure
local current_zone_monsters = {};

--[[
    loadZoneMonstersByID loads the appropriate monster file based on the passed in
    zone id

    Parameters
        zoneID  The identifying zone ID for the region the player is in
--]]

function monster_type.loadZoneMonstersByID(zoneID)
    -- Check for zoneID problem
    if not zone_id or zone_id <= 0 then
        current_zone_monsters = {} -- Clear old data safely
        return
    end
    local currentZoneName = AshitaCore:GetResourceManager():GetString('zones.names', zoneID);
    current_zone_monsters = {};

    -- Build cross-platform path string using relative addressing
    local path = string.format("./config/addons/luashitacast/common/MobDB/%d.lua", zoneID);

    -- Open the appropriate zone data, encapsulated to catch any issues
    local load_success, chunk_or_err = pcall(loadfile, path);
    if not load_success or not chunk_or_err then
        -- File doesn't exist, is unreadable, or contains syntax errors.
        -- Log a clean, non-crashing message to your Ashita console
        print(chat.message(string.format('Info: No valid data file found for Zone ID %d. Monster lookup disabled for this zone.',zoneID)));
        return
    end

    -- Now, load the for processing
    local run_success, data = pcall(chunk_or_err);
    if not run_success or type(data) ~= "table" then
        print(chat.message(string.format("Info: Data file incorrectly formatted for Zone %s (%d). Monster lookup disabled for this zone.", currentZoneName,zoneID)));
        return
    end

    -- Data is safe to flatten
    if data.eco then
        for eco_name, families in pairs(data.eco) do
            for fam_name, monsters in pairs(families) do
                for _, monster_name in ipairs(monsters) do
                    current_zone_monsters[monster_name:lower()] = {
                        ecosystem = eco_name:lower(),
                        family = fam_name:lower()
                    };
                end
            end
        end
        print(chat.message(string.format("Info: Monster data successfully initialized for Zone %s (%d).", currentZoneName,zoneID)));
    else
        current_zone_monsters = {};
        print(chat.message(string.format("Info: No data found for Zone %s (%d). Monster lookup disabled for this zone.", currentZoneName,zoneID)));
    end
end     -- monster_type.loadZoneMonstersByID

--[[
    fMonsterTargetIs determines if the targetted monster is from the specified family or specified ecosystem

    Parameters
        bFamily     T/F, Check the family entry (T) or the ecosystem entry (F)
        sList       One or more listed families/ecosystems to match against

    Return
        T/F
--]]

function monster_type.fMonsterTargetIs(bFamily,sList)
    local target = gData.GetTarget();

    bFamily = bFamily or false;
    sList = sList:lower();
    -- If current monster list is empty or passed list is empty or no target or target isn't a monster
    -- then bypass the routine
    if not current_zone_monsters or not sList or not target or not target.Type or target.Type ~= "Monster" then
        return false;
    end

    for _,j in pairs(current_zone_monsters) do
        if bFamily and string.find(sList,j.family) ~= nil and target.Name:lower() == j.monster_name then
            return true;
        elseif not bFamily and string.find(sList,j.ecosystem) ~= nil and target.Name:lower() == j.monster_name then
            return true;
        end
    end

    return false;
end     -- monster_type.fMonsterTargetIs

return monster_type;
