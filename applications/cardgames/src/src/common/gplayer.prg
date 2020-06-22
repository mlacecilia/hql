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
#include "cardgames.ch"

/*!

 \brief Returns a new g_player object instance

*/
FUNCTION gPlayer( ... )
RETURN g_player():new( ... )

/*!

 \brief define g_player class

*/
CLASS g_player INHERIT g_hand

   EXPORTED:
   METHOD init
   METHOD addScore
   METHOD clear
   METHOD name                            INLINE ::cName
   METHOD score                           INLINE ::nScore
   METHOD setName
   METHOD setScore

   PROTECTED:
   VAR cName                              INIT ""
   VAR nScore                             INIT 0

   HIDDEN:

   METHOD __h_parsingInit
ENDCLASS

/*!

 \brief initialize object instance
 \param(IN)
 \return self

*/
METHOD init( ... ) CLASS g_player
   ::g_hand:init()
   ::__h_parsingInit( ... )
RETURN self

/*!

 \brief add value to the current score
 \param(IN) numeric
 \return self

*/
METHOD addScore( arg1 ) CLASS g_player
   ::nScore += hb_DefaultValue( arg1, 0 )
RETURN self

/*!

 \brief clear/remove all cards
 \param(IN)
 \return self

*/
METHOD clear() CLASS g_player
   ::nScore := 0
   ::g_hand:clear()
RETURN self

/*!

 \brief set name
 \param(IN) string
 \return self

*/
METHOD setName( arg1 ) CLASS g_player
   IF ( hb_IsString(arg1) )
      ::cName := ALLTRIM(arg1)
   ENDIF
RETURN self

/*!

 \brief set score
 \param(IN) numeric
 \return self

*/
METHOD setScore( arg1 ) CLASS g_player
   IF ( hb_IsNumeric(arg1) )
      ::nScore := arg1
   ENDIF
RETURN self

// ==================== HIDDEN section ====================

/*!

 \brief [HIDDEN] parsing init arguments
 \param(IN) ...
 \return NIL

*/
METHOD __h_parsingInit( cName ) CLASS g_player
   ::setName( cName )
RETURN NIL
