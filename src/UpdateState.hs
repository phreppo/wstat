module UpdateState
    (update_entry)
where

import WhileGrammar
import State

update_entry :: Entry -> State -> State
update_entry (identifier,value) s
    | identifier_in_state identifier s = assign_entry_in_state (identifier,value) s 
    | otherwise                        = add_entry_to_state (identifier,value) s 

identifier_in_state :: String -> State -> Bool
identifier_in_state identifier (S [])  = False
identifier_in_state identifier (S ((first_identifier,first_value):entries)) = 
    (first_identifier == identifier) || identifier_in_state identifier (state entries)
    
add_entry_to_state :: Entry -> State -> State
add_entry_to_state entry (S entries) = (state (entry:entries)) 

assign_entry_in_state :: Entry -> State -> State
assign_entry_in_state (identifier,value) (S entries) =
    state [ update_entry entry | entry <- entries]
    where update_entry (id,v) = if id == identifier then (identifier,value) else (id,v)