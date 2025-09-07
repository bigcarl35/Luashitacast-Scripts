local displaybar = T{};

local crossjobs = require('crossjobs');
local utilities = require('utilities');
local gcdisplay = require('gcdisplay');

--[[
    RegionDisplay determines if the player's nation owns the area the character is in
    or not and updates the display bar accordingly.
--]]

function displaybar.RegionDisplay()
    local zoneId = AshitaCore:GetMemoryManager():GetParty():GetMemberZone(0);

    -- Make sure the player's nation is known
    if crossjobs.OwnNation == -1 then
        crossjobs.OwnNation = AshitaCore:GetMemoryManager():GetPlayer():GetNation() + 1;
    end

    -- Determine if current zone in region controlled by player's nation
    for i,j in pairs(crossjobs.RegionControl) do
        if table.find(j['zones'],zoneId) ~= nil then
            if j['own'] == crossjobs.OwnNation then
                gcdisplay.SetCycle('Region','Owned');
            elseif j['own'] == 0 and utilities.fBuffed('Signet') == false then
                gcdisplay.SetCycle('Region','N/A');
            else
                gcdisplay.SetCycle('Region','Not Owned');
            end
            break;
        end
    end
end		-- displaybar.RegionDisplay
