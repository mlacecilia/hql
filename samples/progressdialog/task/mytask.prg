/****************************************************************************
** $HQL_BEGIN_LICENSE$
**
** Copyright (C) 2020 by Luigi Ferraris
**
** This file is part of HQL project
**
** HQL is free software: you can redistribute it and/or modify
** it under the terms of the GNU General Public License as published by
** the Free Software Foundation, either version 3 of the License, or
** (at your option) any later version.
**
** HQL is distributed in the hope that it will be useful,
** but WITHOUT ANY WARRANTY; without even the implied warranty of
** MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
** GNU General Public License for more details.
**
** You should have received a copy of the GNU General Public License
** along with HQL.  If not, see <http://www.gnu.org/licenses/>.
**
** $HQL_END_LICENSE$
****************************************************************************/
#include "hbclass.ch"
#include "hqlinclude.ch"

/*!

 \brief my_task class definition

*/
CLASS my_task INHERIT hql_qobject

   EXPORTED:
   METHOD init
   METHOD start

   PROTECTED:
   VAR nNumTasks                          INIT 100000
   VAR nSteps                             INIT 0
   VAR oDialog                            INIT NIL
   VAR oTimer                             INIT NIL
   METHOD __cancel
   METHOD __perform

   HIDDEN:

END CLASS

/*!

 \brief initialize object instance
 \param[in] string name OR NIL (MANDATORY)
 \param[in] parent (MANDATORY)
 \return SELF

*/
METHOD init( cName, oParent ) CLASS my_task

   ::hql_qobject:init( cName, oParent )

   WITH OBJECT ::oDialog := hqlProgressDialog( /*name*/, oParent )
      :setLabelText( "Task in progress..." )
      //:setCancelButtonText( "Cancel" )
      :setMinimum( 0 )
      :setMaximum( ::nNumTasks )
      :hqlOnCanceled( { || ::__cancel() } )
   END WITH

   WITH OBJECT ::oTimer := hqlTimer( /*name*/, Self )
      :hqlOnTimeout( { || ::__perform() } )
   END WITH

RETURN Self

METHOD start() CLASS my_task
   ::nSteps := 0
   ::oDialog:setValue( ::nSteps )
   ::oDialog:show()
   ::oTimer:start( 0 )
RETURN Self

// ==================== PROTECTED section ====================

METHOD __cancel() CLASS my_task

   ::oTimer:stop()
   ::oDialog:close()

   ::oTimer := NIL
   ::oDialog := NIL

RETURN Self

METHOD __perform() CLASS my_task

   //... perform one percent of the operation
   ::nSteps++
   ::oDialog:setValue( ::nSteps )
   HqlProcessEvents()   // to keep responsive

   IF ( ::nSteps > ::oDialog:maximum())
      ::oTimer:stop()
      ::oDialog:hide()
   ENDIF

RETURN NIL

// ==================== HIDDEN section ====================
