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
#include "hqlchart.ch"

/*!

 \brief Returns a new hql_chartAxis object instance

*/
FUNCTION hqlChartAxis( ... )
RETURN hql_chartAxis():new( ... )

/*!

 \brief define hql_chartAxis class

*/
CLASS hql_chartAxis INHERIT hql_chartObj

   EXPORTED:
   METHOD init
   METHOD maximum                         INLINE ::nMaximum
   METHOD minimum                         INLINE ::nMinimum
   METHOD niceMin                         INLINE ::nNiceMin
   METHOD niceMax                         INLINE ::nNiceMax
   METHOD niceRange                       INLINE ( ::nNiceMax - ::nNiceMin )
   METHOD niceTickSpace                   INLINE ::nNiceTickSpace
   METHOD range                           INLINE ( ::nMaximum - ::nMinimum )
   METHOD setCoordinates
   METHOD setLimits
   METHOD setNice
   METHOD x1                              INLINE ::nX1
   METHOD x2                              INLINE ::nX2
   METHOD y1                              INLINE ::nY1
   METHOD y2                              INLINE ::nY2

   PROTECTED:
   VAR nMaximum                           INIT 0
   VAR nMinimum                           INIT 0
   VAR nNiceMax                           INIT 0
   VAR nNiceMin                           INIT 0
   VAR nNiceTickSpace                     INIT 0
   VAR nX1                                INIT 0
   VAR nX2                                INIT 0
   VAR nY1                                INIT 0
   VAR nY2                                INIT 0
   METHOD __setLimits
   METHOD __setNice

   HIDDEN:

ENDCLASS

/*!

 \brief initialize object instance
 \param(IN) ...
 \return self

*/
METHOD init( cName, cColor, cText, cTooltip ) CLASS hql_chartAxis
   ::cColor := "#000000"

   ::setName( cName )
   ::setColor( cColor )
   ::setText( cText )
   ::setTooltip( cTooltip )
RETURN self

/*!

 \brief set coordinates
 \param(IN) numeric
 \return self

*/
METHOD setCoordinates( nX1, nY1, nX2, nY2 ) CLASS hql_chartAxis
   IF ( hb_IsNumeric(nX1) .AND. hb_IsNumeric(nY1) .AND. hb_IsNumeric(nX2) .AND. hb_IsNumeric(nY2) )
      ::nX1 := nX1
      ::nY1 := nY1
      ::nX2 := nX2
      ::nY2 := nY2
   ENDIF
RETURN self

/*!

 \brief set axis limits
 \param(IN) numeric, numeric
 \return self

*/
METHOD setLimits( nMinimum, nMaximum ) CLASS hql_chartAxis
   ::__setLimits( nMinimum, nMaximum )
RETURN self

/*!

 \brief set axis nice limits
 \param(IN) numeric, numeric
 \return self

*/
METHOD setNice( nMinimum, nMaximum, nTickSpace ) CLASS hql_chartAxis
   ::__setNice( nMinimum, nMaximum, nTickSpace )
RETURN self

// ==================== PROTECTED section ====================

/*

 \brief [PROTECTED] set minimum and maximum values see data
 \param(IN) numeric, numeric
 \return NIL

*/
METHOD __setLimits( nMinimum, nMaximum ) CLASS hql_chartAxis
   nMinimum := hb_DefaultValue( nMinimum, 0 )
   nMaximum := hb_DefaultValue( nMaximum, 0 )
   ::nMaximum := MAX( nMinimum, nMaximum )
   ::nMinimum := MIN( nMinimum, nMaximum )
RETURN NIL

/*

 \brief [PROTECTED] set nice minimum, maximum and tick space
 \param(IN) numeric, numeric, numeric
 \return NIL

*/
METHOD __setNice( nMinimum, nMaximum, nTickSpace ) CLASS hql_chartAxis
   nMinimum := hb_DefaultValue( nMinimum, 0 )
   nMaximum := hb_DefaultValue( nMaximum, 0 )
   nTickSpace := hb_DefaultValue( nTickSpace, 0 )
   nTickSpace := IIF ( nTickSpace == 0, 1, nTickSpace )  /* to avoids errors on formula / 0 */

   ::nNiceMin := MIN( nMinimum, nMaximum )
   ::nNiceMax := MAX( nMinimum, nMaximum )
   ::nNiceTickSpace := nTickSpace
RETURN NIL

// ==================== HIDDEN section ====================
