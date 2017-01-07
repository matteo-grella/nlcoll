------------------------------------------------------------------------------
--                               N L C O L L
--  N a t u r a l   L a n g u a g e   C o m p o n e n t   C o l l e c t i o n
--
--          Copyright 2009-2011 M. Grella, M. Nicola, D. Christen
--
--  In collaboration with Politecnico di Torino and Università di Torino.
--
--  Supported by
--   (a) Fondazione CRT under a "VivoMeglio" grant (Speak2Home)
--   (b) Region Piedmont under a “Converging Technologies” programme (ATLAS)
--        
--  The project aims at developing open-source linguistic technologies for the
--  Italian language to improve the welfare of people, especially impaired users.
--
--  This is free software; you can redistribute it and/or modify it under
--  terms of the GNU General Public License as published by the Free Software
--  Foundation; either version 2, or (at your option) any later version.
--  This software is distributed in the hope that it will be useful, but WITH
--  OUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
--  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License
--  for more details. Free Software Foundation, 59 Temple Place - Suite
--  330, Boston, MA 02111-1307, USA.
--
--  As a special exception, if other files instantiate generics from this
--  unit, or you link this unit with other files to produce an executable,
--  this unit does not by itself cause the resulting executable to be
--  covered by the GNU General Public License. This exception does not
--  however invalidate any other reasons why the executable file might be
--  covered by the GNU Public License.
--
------------------------------------------------------------------------------

pragma License (Modified_GPL);

separate (NLColl.Transition_Systems.Arc_Relocate.State)

function Get_Next_Correct_Actions   
  (State                : in State_Type;
   Gold_Dependency_Tree : in Dependency_Tree_Type) return Action_Type_Vectors.Vector is
begin
        
    return Correct_Actions : Action_Type_Vectors.Vector 
      := Action_Type_Vectors.Empty_Vector do
            
        if not State.Stack.Is_Empty and then not State.Buffer.Is_Empty then
                
            if Correct_Actions.Is_Empty then -- Priority to UNSHIFT
                    
                -----------
                -- RELOCATE / ARC_LEFT
                -----------
                
                for D in 1 .. Distance_Type'Last loop
                    exit when Count_Type (D) > State.Stack.Length;
                    
                    declare
                        I : constant Index_Type 
                          := State.Stack.Last_Index - Index_Type (D - 1);
                        
                        Stack_Token_ID : constant Token_Index_Type
                          := State.Stack.Element (I);
                        
                    begin    
                        if Gold_Dependency_Tree.Heads (State.Stack.Element (I)) = State.Buffer.Last_Element
                          and then Is_Resolved 
                            (ID                   => Stack_Token_ID,
                             Dependency_Tree      => State.Dependency_Tree,
                             Gold_Dependency_Tree => Gold_Dependency_Tree) then
                                
                            if D = 1 then
                                Correct_Actions.Append
                                  ((Name       => ARC_LEFT,
                                    Distance   => 1,
                                    Deprel     => Gold_Dependency_Tree.Deprels (Stack_Token_ID),
                                    Governor   => State.Buffer.Last_Element,
                                    Dependent  => Stack_Token_ID,
                                    Confidence => 1.0));
                            else
                                Correct_Actions.Append
                                  ((Name       => RELOCATE_LEFT,
                                    Distance   => D,
                                    Deprel     => Null_Unbounded_String,
                                    Governor   => NONE_ID,
                                    Dependent  => NONE_ID,
                                    Confidence => 1.0));                                
                            end if;

                            exit; -- exit at the first correct action
                        end if;
                    end;
                end loop;
            
            end if;
                
            if Correct_Actions.Is_Empty then -- Priority to Arc_Left
                    
                if Is_Resolved 
                  (ID                   => State.Buffer.Last_Element,
                   Dependency_Tree      => State.Dependency_Tree,
                   Gold_Dependency_Tree => Gold_Dependency_Tree) then
                        
                    ------------
                    -- RELOCATE / ARC_RIGHT
                    ------------

                    for D in 1 .. Distance_Type'Last loop
                        exit when Count_Type (D) > State.Stack.Length;
                            
                        declare
                            I : constant Index_Type 
                              := State.Stack.Last_Index - Index_Type (D - 1);
                        begin
                            if Gold_Dependency_Tree.Heads (State.Buffer.Last_Element)
                              = State.Stack.Element (I) then
                                    
                                if D = 1 then
                                    Correct_Actions.Append
                                      ((Name       => ARC_RIGHT,
                                        Distance   => 1,
                                        Deprel     => Gold_Dependency_Tree.Deprels (State.Buffer.Last_Element),
                                        Governor   => State.Stack.Element (I),
                                        Dependent  => State.Buffer.Last_Element,
                                        Confidence => 1.0));
                                else
                                    Correct_Actions.Append
                                      ((Name       => RELOCATE_RIGHT,
                                        Distance   => D,
                                        Deprel     => Null_Unbounded_String,
                                        Governor   => NONE_ID,
                                        Dependent  => NONE_ID,
                                        Confidence => 1.0));                                
                                end if;
                                    
                                exit; -- exit at the first correct action
                            end if;
                        end;
                    end loop;
                
                elsif Gold_Dependency_Tree.Heads (State.Buffer.Last_Element)
                  = State.Stack.Last_Element then 
                        
                    -------
                    -- WAIT
                    -------

                    Correct_Actions.Append
                      ((Name       => WAIT,
                        Distance   => 1,
                        Deprel     => Null_Unbounded_String,
                        Governor   => State.Stack.Last_Element,
                        Dependent  => State.Buffer.Last_Element,
                        Confidence => 1.0));
                        
                end if;
            end if;
        end if;
        
        --------
        -- ROOT
        --------
                    
        if Is_Second_To_Last_State (State) 
          and then Gold_Dependency_Tree.Heads (State.Stack.Last_Element) = ROOT_ID then
            
            Correct_Actions.Append
              ((Name       => ROOT,
                Deprel     => Gold_Dependency_Tree.Deprels (State.Stack.Last_Element),
                Distance   => 1,
                Governor   => ROOT_ID,
                Dependent  => State.Stack.Last_Element,
                Confidence => 1.0));
        end if;
        
        --------
        -- SHIFT
        --------
            
        if Correct_Actions.Is_Empty and then State.Buffer.Length > Count_Type(1) then
            Correct_Actions.Append (SHIFT_Action);
        end if;
            
    end return;
        
end Get_Next_Correct_Actions;
