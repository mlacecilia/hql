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

 \brief Returns a new hql_chartPiece object instance

*/
FUNCTION hqlChartPiece( ... )
RETURN hql_chartPiece():new( ... )

/*!

 \brief define hql_chartPiece class

*/
CLASS hql_chartPiece INHERIT hql_chartObj

   EXPORTED:
   METHOD init
   METHOD absHeight                       INLINE ( ABS(::nHeight) )
   METHOD absWidth                        INLINE ( ABS(::nWidth) )
   METHOD height                          INLINE ::nHeight
   METHOD setHeight
   METHOD setWidth
   METHOD swapValues
   METHOD width                           INLINE ::nWidth

   PROTECTED:
   VAR nHeight                            INIT 0
   VAR nWidth                             INIT 0

   HIDDEN:

ENDCLASS

/*!

 \brief initialize object instance for given [name], [Qt flags]
 \param(IN) string, ...
 \return self

*/
METHOD init( nHeight, nWidth, cName, cColor, cText, cTooltip ) CLASS hql_chartPiece
   ::cColor := ::__randomColor()

   ::setHeight( nHeight )
   ::setWidth( nWidth )
   ::setName( cName )
   ::setColor( cColor )
   ::setText( cText )
   ::setTooltip( cTooltip )
RETURN self

/*!

 \brief set height
 \param(IN) numeric
 \return self

*/
METHOD setHeight( arg1 ) CLASS hql_chartPiece
   IF ( hb_IsNumeric(arg1) )
      ::nHeight := arg1
   ENDIF
RETURN self

/*!

 \brief set width
 \param(IN) numeric
 \return self

*/
METHOD setWidth( arg1 ) CLASS hql_chartPiece
   IF ( hb_IsNumeric(arg1) )
      ::nWidth := arg1
   ENDIF
RETURN self

/*!

 \brief swap width, height values
 \param[in] none
 \return self

*/
METHOD swapValues() CLASS hql_chartPiece
   LOCAL nTemp := ::nHeight
   ::nHeight := ::nWidth
   ::nWidth := nTemp
RETURN self

// ==================== PROTECTED section ====================
