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

 \brief Returns a new g_deck object instance

*/
FUNCTION gDeck( ... )
RETURN g_deck():new( ... )

/*!

 \brief define g_deck class

*/
CLASS g_deck INHERIT g_hand

   EXPORTED:
   METHOD init
   METHOD shuffle

   PROTECTED:

   HIDDEN:

ENDCLASS

/*!

 \brief initialize object instance
 \param(IN)
 \return self

*/
METHOD init() CLASS g_deck
   ::g_hand:init()
RETURN self

/*!

 \brief shuffle
 \param(IN)
 \return self

*/
METHOD shuffle( nTimes ) CLASS g_deck
   LOCAL nSize := ::size()
   LOCAL nT, nI, nK, temp
   nTimes := hb_DefaultValue( nTimes, 3 )
   IF ( nSize > 2 )
      FOR nT := 1 TO nTimes
         hb_RandomInt( nSize )
         FOR nI := 1 TO nSize
            nK := hb_RandomInt( nSize )
            WHILE ( nK == nI )
               nK := hb_RandomInt( nSize )
            END
            temp := ::aList[nI]
            ::aList[nI] := ::aList[nK]
            ::aList[nK] := temp
         NEXT
      NEXT
   ENDIF
RETURN self
