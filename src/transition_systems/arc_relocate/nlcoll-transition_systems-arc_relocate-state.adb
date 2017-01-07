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

with Ada.Text_IO; use Ada.Text_IO;

package body NLColl.Transition_Systems.Arc_Relocate.State is

    procedure Perform_Shift
      (State  : in out State_Type;
       Action : in     Action_Type) is
    begin
        State.Stack.Append (Pop (State.Buffer));
        State.Prev_Action := Action;
    end Perform_Shift;
    
    procedure Perform_Relocate
      (State  : in out State_Type;
       Action : in     Action_Type) is
        
        Moving_Item_Index : constant Index_Type
          := State.Stack.Last_Index - Index_Type(Action.Distance) + 1;
        
        Moving_Item : constant Token_Index_Type
          := State.Stack.Element (Moving_Item_Index);
    begin
        State.Stack.Delete (Moving_Item_Index);
        State.Stack.Append (Moving_Item);

        State.Prev_Action := Action;
    end Perform_Relocate;

    procedure Perform_Wait
      (State  : in out State_Type;
       Action : in     Action_Type) is
    begin
        State.Stack.Append (Pop (State.Buffer));

        State.Prev_Action := Action;
    end Perform_Wait;
    
    procedure Perform_Unshift
      (State  : in out State_Type;
       Action : in     Action_Type) is
    begin
        for T in 1 .. Action.Distance loop
            State.Buffer.Append (Pop (State.Stack));
        end loop;
        
        State.Prev_Action := Action;
    end Perform_Unshift;
    
    procedure Perform_Root
      (State  : in out State_Type;
       Action : in     Action_Type) is
    begin
        Pop (State.Stack); -- remove dependent
        
        State.Dependency_Tree.Set_Arc
          (Dependent => Action.Dependent,
           Governor  => Action.Governor,
           Deprel    => Action.Deprel);
        
        State.Prev_Action := Action;
    end Perform_Root;
    
    procedure Perform_Arc_Left
      (State  : in out State_Type;
       Action : in     Action_Type) is
    begin
        
        Pop (State.Stack); -- remove dependent
        State.Prev_Action := Action;
        
        Set_Arc
          (Dependency_Tree => State.Dependency_Tree,
           Dependent       => Action.Dependent,
           Governor        => Action.Governor,
           Deprel          => Action.Deprel);
        
        if Is_Deterministic_State (State) then
            Perform_Shift (State, SHIFT_Action);
        end if;
        
    end Perform_Arc_Left;
    
    procedure Perform_Arc_Right
      (State  : in out State_Type;
       Action : in     Action_Type) is
    begin
        
        State.Buffer.Delete_Last;
        State.Buffer.Append (Pop (State.Stack));
        State.Prev_Action := Action;
        
        Set_Arc
          (Dependency_Tree => State.Dependency_Tree,
           Dependent       => Action.Dependent,
           Governor        => Action.Governor,
           Deprel          => Action.Deprel);
        
        if Is_Deterministic_State (State) then
            Perform_Shift (State, SHIFT_Action);
        end if;
 
    end Perform_Arc_Right;
    
    function Get_Next_Correct_Actions   
      (State                : in State_Type;
       Gold_Dependency_Tree : in Dependency_Tree_Type) return Action_Type_Vectors.Vector is separate;
    
    function Get_Next_Correct_Action
      (State                : in State_Type;
       Gold_Dependency_Tree : in Dependency_Tree_Type) return Action_Type is
        
        Actions : constant Action_Type_Vectors.Vector
          := Get_Next_Correct_Actions
            (State                => State,
             Gold_Dependency_Tree => Gold_Dependency_Tree);
    begin
        
        if Actions.Length /= 1 then 
            for Action of Actions loop
                Put_Line (Standard_Error, Action.Name'Img & ":" & Action.Distance'Img);
            end loop;
            
            raise Parser_State_Error 
              with "Expected Next_Correct_Actions.Length = 1, found " & Actions.Length'Img;
        end if;
        
        return Actions.First_Element;
          
    end Get_Next_Correct_Action;

    procedure Perform_Action
      (State  : in out State_Type;
       Action : in     Action_Type) is
    begin
        
        case Action.Name is
            when SHIFT          => Perform_Shift (State, Action);
            when UNSHIFT        => Perform_Unshift(State, Action);
            when RELOCATE_LEFT  => Perform_Relocate (State, Action);
            when RELOCATE_RIGHT => Perform_Relocate (State, Action);
            when WAIT           => Perform_Wait (State, Action);
            when ROOT           => Perform_Root (State, Action);
            when ARC_LEFT       => Perform_Arc_Left (State, Action);
            when ARC_RIGHT      => Perform_Arc_Right (State, Action);
            when NONE | WRONG   => null;
        end case;
        
    end Perform_Action;
    
    procedure State_Initialize
      (Sentence : in Sentence_Type;
       State    : in out State_Type) is
    begin
        
        for Token : Token_Access of reverse Sentence.Elements loop
            State.Buffer.Append (Token.ID);
        end loop;
        
        for I in Sentence.Elements.First_Index .. Sentence.Elements.Last_Index loop
            State.Dependency_Tree.Heads.Append (NONE_ID);
            State.Dependency_Tree.Deprels.Append (Null_Unbounded_String);
            State.Dependency_Tree.Dependents.Append (Empty_Dependents_Structure);
        end loop;
        
        if Is_Deterministic_State (State) then
            Perform_Shift (State, SHIFT_Action);
        end if;

        State.Log_Prob    := 0.0;
           
        State.Initialized := True;
    end State_Initialize;

    function State_Initialize
      (Sentence : in Sentence_Type) return State_Type is
    begin
        return State : State_Type do
            State_Initialize
              (Sentence       => Sentence,
               State          => State);
        end return; 
    end State_Initialize;
    
    procedure State_Finalize
      (State : in out State_Type) is
    begin
        State.Stack.Clear;
        State.Buffer.Clear;
        State.Dependency_Tree.Clear;
        State.Log_Prob    := 0.0;
        State.Initialized := False;
    end State_Finalize;
    
    procedure Print_State
      (State : in State_Type) is
    begin
        
        Put_Line ("Stack: ");
        if State.Stack.Is_Empty then 
            Put ("[]");
        else
            for I in State.Stack.First_Index .. State.Stack.Last_Index loop
                Put (State.Stack.Element (I)'Img & " ");
            end loop;
            New_Line;
        end if;
        New_Line;
        
        Put_Line ("Buffer: ");
        if State.Buffer.Is_Empty then
            Put ("[]");
        else
            for I in reverse State.Buffer.First_Index .. State.Buffer.Last_Index loop
                Put (State.Buffer.Element (I)'Img & " ");
            end loop;
            New_Line;
        end if;
        New_Line;
        
    end Print_State;
      
    package body State_Context is separate;
    
end NLColl.Transition_Systems.Arc_Relocate.State;
