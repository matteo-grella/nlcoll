------------------------------------------------------------------------------
--                               N L C O L L
--  N a t u r a l   L a n g u a g e   C o m p o n e n t   C o l l e c t i o n
--
--                       Copyright 2014 M. Grella
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

with NLColl.Linguistic_Description.Sentences; use NLColl.Linguistic_Description.Sentences;
with NLColl.Linguistic_Description.Syntax.Dependency_Trees; use NLColl.Linguistic_Description.Syntax.Dependency_Trees;

with NLColl.Transition_Systems.Easy_First.State; use NLColl.Transition_Systems.Easy_First.State;

package NLColl.Transition_Systems.Easy_First.Test is

    procedure Test_Transition_Sequence
      (Sentence             : in     Sentence_Type;
       Gold_Dependency_Tree : in     Dependency_Tree_Type;
       Verbose              : in     Boolean := False);

private

    use NLColl.Transition_Systems.Easy_First.State.Actions;

    function Get_Next_Correct_Action
      (State                : in State_Type;
       Gold_Dependency_Tree : in Dependency_Tree_Type) return Action_Type;

    function Get_Next_Random_Correct_Action
      (State                : in State_Type;
       Gold_Dependency_Tree : in Dependency_Tree_Type) return Action_Type;

    procedure Print_Current_State
      (Sentence             : in Sentence_Type;
       State                : in State_Type;
       Gold_Dependency_Tree : in Dependency_Tree_Type;
       Show_Gold            : in Boolean := False);

end NLColl.Transition_Systems.Easy_First.Test;

