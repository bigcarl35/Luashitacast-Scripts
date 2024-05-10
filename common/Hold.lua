--[[
	AccessibleLevel checks to see if the passed item is accessible to the player in the field and
	that the level of the item isn't too high.
--]]

function gcinclude.AccessibleLevel(sName)
	local inventory = AshitaCore:GetMemoryManager():GetInventory();
	local resources = AshitaCore:GetResourceManager();
	local player = gData.GetPlayer();
	local item,containerID;
	
	-- Retrieve the item's detals
	item = AshitaCore:GetResourceManager():GetItemByName(sName,2);
	if item == nil then
		return false;
	end
	
	-- Now, determine if the item can be accessed

	sName = string.lower(sName);
	for i,_ in pairs(gcinclude.EQUIPABLE) do
		containerID = gcinclude.EQUIPABLE[i][1];
		-- then loop through the container
		for j = 1,inventory:GetContainerCountMax(containerID),1 do
			local itemEntry = inventory:GetContainerItem(containerID, j);
			if (itemEntry.Id ~= 0 and itemEntry.Id ~= 65535) then
				local item1 = resources:GetItemById(itemEntry.Id);
				if string.lower(item1.Name[1]) == sName then
					return true;
				end
			end
		end
	end
	
	return false;
end		-- gcinclude.AccessibleLevel

								-- Check to see if you need to report the problem
								if string.find(gcinclude.GearWarnings,vRoot) == nil then
									print(chat.header('MoveToCurrent'):append(chat.message('Warning: ' .. vRoot .. ' cannot be equipped by your job. Skipping')));
									gcinclude.GearWarnings = gcinclude.GearWarnings .. vRoot .. ',';
								end
							else
							
	--print(chat.color1(1, 'White'),chat.color1(2, 'Green'),chat.color1(3, 'Blue'),
	--chat.color1(8, 'Red'),chat.color1(107, 'yellow'));

-and gcinclude.elemental_obis[k][2] then

gcinclude.TieredMagicJobs = 'WHM,RDM,PLD,SCH,BLM,DRK,BRD,GEO,RUN,NIN';
gcinclude.sMagicJobs = 'BLM,WHM,RDM,SMN,PLD,DRK,SCH,GEO,RUN';