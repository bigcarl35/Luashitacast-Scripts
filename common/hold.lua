function ppt()
	local pEntity = AshitaCore:GetMemoryManager():GetEntity();
	local myIndex = AshitaCore:GetMemoryManager():GetParty():GetMemberTargetIndex(0);
    local petIndex = AshitaCore:GetMemoryManager():GetEntity():GetPetTargetIndex(myIndex);
	local targetIndex = gData.GetTargetIndex();
	local x,y,z;
	
	print(' ');
	if petIndex ~= nil and petIndex > 0 then
		x = math.pow(pEntity:GetLocalPositionX(myIndex) - pEntity:GetLocalPositionX(petIndex),2);
		y = math.pow(pEntity:GetLocalPositionY(myIndex) - pEntity:GetLocalPositionY(petIndex),2);
		z = math.pow(pEntity:GetLocalPositionZ(myIndex) - pEntity:GetLocalPositionZ(petIndex),2);
		print(chat.message(string.format('Player to Pet: %.1f',math.sqrt(AshitaCore:GetMemoryManager():GetEntity():GetDistance(petIndex)))));
	else		
		print(chat.message('You have no pet'));
	end

	if targetIndex ~= nil and targetIndex > 0 then
		x = math.pow(pEntity:GetLocalPositionX(myIndex) - pEntity:GetLocalPositionX(targetIndex),2);
		y = math.pow(pEntity:GetLocalPositionY(myIndex) - pEntity:GetLocalPositionY(targetIndex),2);
		z = math.pow(pEntity:GetLocalPositionZ(myIndex) - pEntity:GetLocalPositionZ(targetIndex),2);	
		print(chat.message(string.format('Player to target: %d.1', math.sqrt(x+y+z)).. 'm'));
		print(chat.message(math.sqrt(AshitaCore:GetMemoryManager():GetEntity():GetDistance(targetIndex)) .. 'm'));
	else
		print(chat.message('You have no target'));
	end	

	if petIndex ~= nil and petIndex > 0 and targetIndex ~= nil and targetIndex > 0 then
		x = math.pow(pEntity:GetLocalPositionX(petIndex) - pEntity:GetLocalPositionX(targetIndex),2);
		y = math.pow(pEntity:GetLocalPositionY(petIndex) - pEntity:GetLocalPositionY(targetIndex),2);
		z = math.pow(pEntity:GetLocalPositionZ(petIndex) - pEntity:GetLocalPositionZ(targetIndex),2);	
		print(chat.message(string.format('Pet to target: %d.1', math.sqrt(x+y+z))));
		print(chat.message(math.sqrt(AshitaCore:GetMemoryManager():GetEntity():GetDistance(petIndex))));
		print(chat.message(math.sqrt(AshitaCore:GetMemoryManager():GetEntity():GetDistance(targetIndex))));
	end	
end		-- ppt