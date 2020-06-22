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
#include "hqlhbqt.ch"

/*!

 \brief Returns new object instance of hql_lcdNumber
 \param(IN) ...
 \return object

*/
FUNCTION digitalClock( ... )
RETURN digital_clock():new( ... )

/*!

 \brief digital_clock class definition

*/
CLASS digital_clock INHERIT hql_lcdNumber

   EXPORTED:
   METHOD init

   PROTECTED:
   METHOD __hqlShowTime

END CLASS

/*!

 \brief Initialize a new object instance
 \param(IN) string, ...
 \return self

*/
METHOD init( ... ) CLASS digital_clock

   ::hql_lcdNumber:init( ... )

   ::setSegmentStyle( QLCDNumber_Filled )

   WITH OBJECT hqlTimer( /*name*/, Self )
      :hqlOnTimeout( { || ::__hqlShowTime() } )
      :start( 1000 )
   END WITH

   ::__hqlShowTime()

RETURN Self

// ==================== PROTECTED section ====================

// ==================== SLOTS/EVENTS section ====================

/*!

 \brief [INTERNAL] show current time
 \param[in] none
 \return NIL

*/
METHOD __hqlShowTime() CLASS digital_clock

   LOCAL oTime := QTime():currentTime()
   LOCAL cText

   // The current time is converted into a string with the format "hh:mm".
   // When QTime::second() is a even number, the colon in the string is replaced with a space.
   // This makes the colon appear and vanish every other second.
   IF ( ( oTime:second() % 2 ) == 0 )
      cText := oTime:toString( "hh" )
   ELSE
      cText := oTime:toString( "hh:mm" )
   ENDIF

   ::display( cText )

RETURN NIL

// ==================== HIDDEN section ====================
