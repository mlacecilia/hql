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

 \brief Returns a new hql_chartVbaScale object instance
      https://peltiertech.com/calculate-nice-axis-scales-in-excel-vba/
      jon at peltiertech.com


*/
FUNCTION hqlChartVbaScale( ... )
RETURN hql_chartVbaScale():new( ... )


/*!


*/
CLASS hql_chartVbaScale STATIC

   EXPORTED:
   METHOD init
   METHOD majorTickSpace                  INLINE ::nMajorTickSpace
   METHOD minorTickSpace                  INLINE ::nMinorTickSpace
   METHOD maximum                         INLINE ::nMaximum
   METHOD minimum                         INLINE ::nMinimum
   METHOD niceMin                         INLINE ::nNiceMin
   METHOD niceMax                         INLINE ::nNiceMax
   METHOD niceRange                       INLINE ( ::nNiceMax - ::nNiceMin )
   METHOD range                           INLINE ( ::nMaximum - ::nMinimum )
   METHOD setLimits

   PROTECTED:
   VAR nMaximum                           INIT 0
   VAR nMinimum                           INIT 0
   VAR nNiceMin                           INIT 0
   VAR nNiceMax                           INIT 0
   VAR nMajorTickSpace                    INIT 0
   VAR nMinorTickSpace                    INIT 0
   METHOD __calculate
   METHOD __setLimits

   HIDDEN:

ENDCLASS

/*!

 \brief initialize object instance for given arguments
 \param(IN) ...
 \return self

*/
METHOD init( nMinimum, nMaximum ) CLASS hql_chartVbaScale
   ::__setLimits( nMinimum, nMaximum )
   ::__calculate()
RETURN self

/*!

 \brief set min and max
 \param(IN) numeric, numeric
 \return self

*/
METHOD setLimits( nMinimum, nMaximum ) CLASS hql_chartVbaScale
   ::__setLimits( nMinimum, nMaximum )
   ::__calculate()
RETURN self

// ==================== PROTECTED section ====================

/*

 \brief [PROTECTED] calculate scale
 \param(IN)
 \return NIL

*/
METHOD __calculate() CLASS hql_chartVbaScale
   LOCAL nMaximum, nMinimum, nTemp, nPower, nScale, nSmall

   nMaximum := ::nMaximum
   nMinimum := ::nMinimum

   /* Check if the max and min are the same */
   IF ( nMaximum == nMinimum )
      nMaximum *= 1.01
      nMinimum *= 0.99
   ENDIF

   /* Check if dMax is bigger than dMin - swap them if not */
   IF ( nMaximum < nMinimum )
      nTemp := nMaximum
      nMaximum := nMinimum
      nMinimum := nTemp
   ENDIF

   /* Make dMax a little bigger and dMin a little smaller (by 1% of their difference) */
   IF ( nMaximum > 0 )
      /* dMax = dMax + (dMax - dMin) * 0.01 */
      nMaximum += (nMaximum - nMinimum) * 0.01
   ELSEIF ( nMaximum < 0 )
      /* dMax = WorksheetFunction.Min(dMax + (dMax - dMin) * 0.01, 0) */
      nMaximum := MIN(nMaximum + (nMaximum - nMinimum) * 0.01, 0)
   ELSE
      nMaximum := 0
   ENDIF

   IF ( nMinimum > 0 )
      /* dMin = WorksheetFunction.Max(dMin - (dMax - dMin) * 0.01, 0) */
      nMinimum := MAX(nMinimum - (nMaximum - nMinimum) * 0.01, 0)
   ELSEIF ( nMinimum < 0 )
      /* dMin = dMin - (dMax - dMin) * 0.01 */
      nMinimum -= (nMaximum - nMinimum) * 0.01
   ELSE
      nMinimum = 0
   ENDIF

   /* What if they are both 0? */
   IF ( nMaximum == 0 .AND. nMinimum == 0 )
      nMaximum := 1
   ENDIF

   /* This bit rounds the maximum and minimum values to reasonable values
      to chart.  If not done, the axis numbers will look very silly
      Find the range of values covered
   */
   nPower := LOG(nMaximum - nMinimum) / LOG(10)
   nScale := 10 ^ (nPower - INT(nPower))

   /* Find the scaling factor */
   IF ( nScale >= 0 .AND. nScale <= 2.5 )
      nScale := 0.2
      nSmall := 0.05
   ELSEIF ( nScale > 2.5 .AND. nScale <= 5 )
      nScale := 0.5
      nSmall := 0.1
   ELSEIF ( nScale > 5 .AND. nScale <= 7.5 )
      nScale := 1
      nSmall := 0.2
   ELSE
      nScale := 2
      nSmall := 0.5
   ENDIF

   /* Calculate the scaling factor (major & minor unit) */
   nScale *= 10 ^ INT(nPower)
   nSmall *= 10 ^ INT(nPower)


   /* Round the axis values to the nearest scaling factor */
   /* fnAxisScale.dMin = dScale * Int(dMin / dScale)
      ::nNiceMin := nScale * INT(nMinimum / nScale)
      when negative values are involved there is a problem
    */
   ::nNiceMin := nScale * hql_Floor( nMinimum / nScale )
   /* fnAxisScale.dMax = dScale * (Int(dMax / dScale) + 1)
      ::nNiceMax := nScale * (INT(nMaximum / nScale) + 1)
      when negative values are involved there is a problem
   */
   ::nNiceMax := nScale * hql_Ceiling( nMaximum / nScale )

   ::nMajorTickSpace := nScale
   ::nMinorTickSpace := nSmall

RETURN NIL

/*

 \brief [PROTECTED] set minimum and maximum values see data
 \param(IN) numeric, numeric
 \return NIL

*/
METHOD __setLimits( nMinimum, nMaximum ) CLASS hql_chartVbaScale
   nMinimum := hb_DefaultValue( nMinimum, 0 )
   nMaximum := hb_DefaultValue( nMaximum, 0 )
   ::nMinimum := MIN( nMinimum, nMaximum )
   ::nMaximum := MAX( nMinimum, nMaximum )
RETURN NIL
