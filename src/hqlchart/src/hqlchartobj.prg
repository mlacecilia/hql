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

/*!

 \brief define hql_chartObj class

*/
CLASS hql_chartObj

   EXPORTED:
   METHOD color
   METHOD name                            INLINE ::cName
   METHOD randomColor                     INLINE ( ::__randomColor() )
   METHOD setColor
   METHOD setName
   METHOD setText
   METHOD setTooltip
   METHOD text                            INLINE ::cText
   METHOD tooltip                         INLINE ::cTooltip

   PROTECTED:
   VAR cColor                             INIT ""
   VAR cName                              INIT ""
   VAR cText                              INIT ""
   VAR cTooltip                           INIT ""
   METHOD __randomColor

ENDCLASS

/*!

 \brief returns color string
 \param(IN)
 \return string

*/
METHOD color() CLASS hql_chartObj
   IF ( EMPTY(::cColor) )
      ::cColor := ::__randomColor()
   ENDIF
RETURN ::cColor

/*!

 \brief set color as string (e.g. "#C0AE2D")
 \param(IN) string
 \return self

*/
METHOD setColor( arg1 ) CLASS hql_chartObj
   IF ( hb_IsString(arg1) .AND. hb_LeftEq( arg1, "#" ) .AND. LEN(ALLTRIM(arg1)) == 7 )
      arg1 := UPPER(ALLTRIM(arg1))
      IF ( hb_HexToNum( SUBSTR(arg1, 2) ) >= 0 .AND. hb_HexToNum( SUBSTR(arg1, 2) ) <= 16777215 )
         ::cColor := arg1
      ENDIF
   ENDIF
RETURN self

/*!

 \brief set name
 \param(IN) string
 \return self

*/
METHOD setName( arg1 ) CLASS hql_chartObj
   IF ( hb_IsString(arg1) )
      ::cName := ALLTRIM(arg1)
   ENDIF
RETURN self

/*!

 \brief set text
 \param(IN) string
 \return self

*/
METHOD setText( arg1 ) CLASS hql_chartObj
   IF ( hb_IsString(arg1) )
      ::cText := ALLTRIM(arg1)
   ENDIF
RETURN self

/*!

 \brief set tooltip
 \param(IN) string
 \return self

*/
METHOD setTooltip( arg1 ) CLASS hql_chartObj
   IF ( hb_IsString(arg1) )
      ::cTooltip := ALLTRIM(arg1)
   ENDIF
RETURN self

// ==================== PROTECTED section ====================

/*!

 \brief returns random color as "#RRGGBB" string
 \param(IN)
 \return string

*/
METHOD __randomColor() CLASS hql_chartObj
   LOCAL nRed := -1, nGreen := -1, nBlue := -1
   LOCAL cColor

   WHILE ( nRed == nGreen .AND. nGreen == nBlue )
      nRed := ( hb_RandomInt( 0, 16777215 ) % 256 )
      nGreen := ( hb_RandomInt( 0, 16777215 ) % 256 )
      nBlue := ( hb_RandomInt( 0, 16777215 ) % 256 )
   END

   cColor := '#' + hb_NumToHex( nRed, 2 ) + hb_NumToHex( nGreen, 2 ) + hb_NumToHex( nBlue, 2 )
RETURN cColor
