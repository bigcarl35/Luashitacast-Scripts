-- By example I have filled out the rse for both male and female tarutaru and the job gear for SMN,
-- but I need to check for actual spellings. (I have not used the contractions that I know some gear has.)
-- For races that have two sexes, you'll find an entry that's specific to the race and then entries for
-- the specific race/gender combination. If entries are non-gender specific, put them in the genderless
-- gear sets.

-- Each line item, include name (what the piece is called to equip it), and in a line comment: full name, id
gVars.SpecialGear = {
    ['race'] = {
        ['elvaan'] = {
        },
        ['elvaan-female'] = {
            ['rse1'] = {        -- starting rse gear
            },
            ['rse'] = {         -- rse
            },
            ['rse-belt'] = {    -- belt
            },
            ['rse-stone'] = {   -- belt
            },
            ['rse2'] = {        -- level 55-60+ rse
            },
            ['rse2-hp'] = {     -- belt
            },
            ['rse2-mp'] = {     -- belt
            },
        },
        ['elvaan-male'] = {
            ['rse1'] = {
            },
            ['rse'] = {
            },
            ['rse-belt'] = {
            },
            ['rse-stone'] = {
            },
            ['rse2'] = {
            },
            ['rse2-hp'] = {
            },
            ['rse2-mp'] = {
            },
        },
        ['galka'] = {
            ['rse1'] = {
            },
            ['rse'] = {
            },
            ['rse-belt'] = {
            },
            ['rse-stone'] = {
            },
            ['rse2'] = {
            },
            ['rse2-hp'] = {
            },
            ['rse2-mp'] = {
            },
        },
        ['hume'] = {
        },
        ['hume-female'] = {
            ['rse1'] = {
            },
            ['rse'] = {
            },
            ['rse-belt'] = {
            },
            ['rse-stone'] = {
            },
            ['rse2'] = {
            },
            ['rse2-hp'] = {
            },
            ['rse2-mp'] = {
            },
        },
        ['hume-male'] = {
            ['rse1'] = {
            },
            ['rse'] = {
            },
            ['rse-belt'] = {
            },
            ['rse-stone'] = {
            },
            ['rse2'] = {
            },
            ['rse2-hp'] = {
            },
            ['rse2-mp'] = {
            },
        },
        ['mithra'] = {
            ['rse1'] = {
            },
            ['rse'] = {
            },
            ['rse-belt'] = {
            },
            ['rse-stone'] = {
            },
            ['rse2'] = {
            },
            ['rse2-hp'] = {
            },
            ['rse2-mp'] = {
            },
        },
        ['tartaru'] = {
            ['rse1'] = {
                Body  = 'Tarutaru Kaftan',
                Hands = 'Tarutaru Mitts',
                Legs  = 'Tarutaru Braccae',
                Feet  = 'Tarutaru Clomps',
            },
            ['rse'] = {
                Earrings = { 'Marukaka\'s Earring','Waetoto\'s Earring' },
                Body  = 'Wonder Kaftan',
                Hands = 'Wonder Mitts',
                Ammo  = 'Sweet Sachet',
                Legs  = 'Wonder Braccae',
                Feet  = 'Wonder Clomps',
            },
            ['rse-belt'] = {
                Waist = 'Steppe Belt',
            },
            ['rse-stone'] = {
                Waist = 'Steppe Stone',
            },
            ['rse2'] = {
                Earrings = { 'Marukaka\'s Earring','Waetoto\'s Earring' },
            },
            ['rse2-hp'] = {
                Waist = 'Steppe Sash',
            },
            ['rse2-mp'] = {
                Waist = 'Steppe Rope',
            },
        },
        ['tarutaru-female'] = {
            ['rse2'] = {
                Hands = 'Creek F Mitts',
                Boots = 'Creek F Clomps',
            },
        },
        ['tarutaru-male'] = {
            ['level62-rse'] = {
                Hands = 'Creek M Mitts',
                Boots = 'Creek M Clomps',
            },
        },
        ['clam'] = {
            ['elvaan-female'] = {
            }.
            ['elvaan-male'] = {
            },
            ['galka'] = {
            },
            ['hume-female'] = {
            },
            ['home-male'] = {
            },
            ['mithra'] = {
            },
            ['tartaru-female'] = {
                Body = 'Tarutaru Top +1',
                Legs = 'Taru. Shorts +1'
            },
            ['tartaru-male'] = {
                Body = 'Tarutaru Maillot +1',
                Legs = 'Tarutaru Trunks +1',
            },
        },
        ['purgo'] = {
            ['elvaan-female'] = {
            }.
            ['elvaan-male'] = {
            },
            ['galka'] = {
            },
            ['hume-female'] = {
            },
            ['home-male'] = {
            },
            ['mithra'] = {
            },
            ['tartaru-female'] = {
                Body = 'Wonder Top +1',
            },
            ['tartaru-male'] = {
            },
        },
    },
    ['job'] = {
        ['BLM'] = {
            ['af'] = {
            },
            ['af+1'] = {
            },
            ['relic'] = {
            },
            ['relic+1'] = {
            },
        },
        ['BLU'] = {
            ['af'] = {
            },
            ['af+1'] = {
            },
            ['relic'] = {
            },
            ['relic+1'] = {
            },
        },
        ['BRD' = {
            ['af'] = {
            },
            ['af+1'] = {
            },
            ['relic'] = {
            },
            ['relic+1'] = {
            },
        },
        ['BST'] = {
            ['af'] = {
            },
            ['af+1'] = {
            },
            ['relic'] = {
            },
            ['relic+1'] = {
            },
        },
        ['COR'] = {
            ['af'] = {
            },
            ['af+1'] = {
            },
            ['relic'] = {
            },
            ['relic+1'] = {
            },
        },
        ['DNC'] = {
            ['af'] = {
            },
            ['af+1'] = {
            },
            ['relic'] = {
            },
            ['relic+1'] = {
            },
        },
        ['DRG'] = {
            ['af'] = {
            },
            ['af+1'] = {
            },
            ['relic'] = {
            },
            ['relic+1'] = {
            },
        },
        ['DRK'] = {
            ['af'] = {
            },
            ['af+1'] = {
            },
            ['relic'] = {
            },
            ['relic+1'] = {
            },
        },
        ['GEO'] = {
            ['af'] = {
            },
            ['af+1'] = {
            },
            ['relic'] = {
            },
            ['relic+1'] = {
            },
        },
        ['MNK'] = {
            ['af'] = {
            },
            ['af+1'] = {
            },
            ['relic'] = {
            },
            ['relic+1'] = {
            },
        },
        ['NIN'] = {
            ['af'] = {
            },
            ['af+1'] = {
            },
            ['relic'] = {
            },
            ['relic+1'] = {
            },
        },
        ['PLD'] = {
            ['af'] = {
            },
            ['af+1'] = {
            },
            ['relic'] = {
            },
            ['relic+1'] = {
            },
        },
        ['PUP'] = {
            ['af'] = {
            },
            ['af+1'] = {
            },
            ['relic'] = {
            },
            ['relic+1'] = {
            },
        },
        ['RDM'] = {
            ['af'] = {
            },
            ['af+1'] = {
            },
            ['relic'] = {
            },
            ['relic+1'] = {
            },
        },
        ['RNG'] = {
            ['af'] = {
            },
            ['af+1'] = {
            },
            ['relic'] = {
            },
            ['relic+1'] = {
            },
        },
        ['RUN'] = {
            ['af'] = {
            },
            ['af+1'] = {
            },
            ['relic'] = {
            },
            ['relic+1'] = {
            },
        },
        ['SAM'] = {
            ['af'] = {
            },
            ['af+1'] = {
            },
            ['relic'] = {
            },
            ['relic+1'] = {
            },
        },
        ['SCH'] = {
            ['af'] = {
            },
            ['af+1'] = {
            },
            ['relic'] = {
            },
            ['relic+1'] = {
            },
        },
        ['SMN'] = {
            ['af'] = {
                Main  = 'Kukulcan\'s Staff',
                Head  = 'Evoker\'s Horn',
                Body  = 'Evoker\'s Doublet',
                Hands = 'Evoker\'s Bracers',
                Legs  = 'Evoker\'s Spats',
                Feet  = 'Evoker\'s Pigaches',
            },
            ['af+1'] = {
                Head  = 'Evoker\'s Horn +1',
                Body  = 'Evoker\'s Doublet +1',
                Hands = 'Evoker\'s Bracers +1',
                Legs  = 'Evoker\'s Spats +1',
                Feet  = 'Evoker\'s Pigaches +1',
            },
            ['relic'] = {
                Head  = 'Summoner\'s Horn',
                Body  = 'Summoner\'s Doublet',
                Hands = 'Summoner\'s Bracers',
                Back  = 'Summoner\'s Cape',
                Legs  = 'Summoner\'s Spats',
                Feet  = 'Summoner\'s Pigaches',
            },
            ['relic+1'] = {
                Head  = 'Summoner\'s Horn +1',
                Body  = 'Summoner\'s Doublet +1',
                Hands = 'Summoner\'s Bracers +1',
                Legs  = 'Summoner\'s Spats +1',
                Feet  = 'Summoner\'s Pigaches +1',
            },
        },
        ['THF'] = {
            ['af'] = {
            },
            ['af+1'] = {
            },
            ['relic'] = {
            },
            ['relic+1'] = {
            },
        },
        ['WAR'] = {
            ['af'] = {
            },
            ['af+1'] = {
            },
            ['relic'] = {
            },
            ['relic+1'] = {
            },
        },
        ['WHM'] = {
            ['af'] = {
            },
            ['af+1'] = {
            },
            ['relic'] = {
            },
            ['relic+1'] = {
            },
        },
    },
};
