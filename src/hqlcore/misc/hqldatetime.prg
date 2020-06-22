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
#include "hbtrace.ch"
#include "error.ch"
#include "hqlhbqt.ch"

/*!

 \brief [PUBLIC] function returns hql_date new object instance
 \param(IN) ...
 \return object

*/
FUNCTION hqlDate( ... )
RETURN hql_date():new( ... )

/*!

 \brief hql_date class definition
 \param(IN) ...
 \return nil

*/
CLASS hql_date INHERIT hql_hbDateTime

   EXPORTED:
   METHOD init
   METHOD hqlCurrentDate
   METHOD hqlSetDate
   METHOD fromHbDate
   METHOD fromHbString
   METHOD fromQdate
   METHOD hbDate
   METHOD hbNum
   METHOD QtDate
   METHOD toHbString

   PROTECTED:
   METHOD __compare_object                // override
   METHOD __toLocaleString                // override

   HIDDEN:

   METHOD __h_parsingInit
ENDCLASS

/*!

 \brief initialize object instance
 \param(IN) ...
 \return nil

*/
METHOD init( ... ) CLASS hql_date
   ::__h_parsingInit( ... )
RETURN NIL

/*!

 \brief returns today as hqlDate
 \param(IN)
 \return hqlDate

*/
METHOD hqlCurrentDate() CLASS hql_date
   LOCAL oObject := QDate():currentDate()
RETURN hqlDate( oObject:year(), oObject:month(), oObject:day() )

/*!

 \brief set current date; accepts:
   - arg1 [,arg2] as string; e.g. "20180228" (by default "yyyyMMdd" formatted) | "28022019", "ddMMyyyy"
   - arg1 as hb_Date | hb_DateTime
   - arg1 as QDate
   - arg1, arg2, arg3 as numeric value
   ELSE an invalid QDate is set
 \param(IN) ...
 \return self

*/
METHOD hqlSetDate( arg1, arg2, arg3 ) CLASS hql_date
   ::oQrealObject := ::__argToQdate(arg1, arg2, arg3)
RETURN self

/*!

 \brief returns a new hqlDate copied from hb_Date | hb_DateTime
 \param(IN) ...
 \return hqlDate

*/
METHOD fromHbDate( arg1 ) CLASS hql_date
   LOCAL oObject :=  IIF ( hb_IsDate(arg1) .OR. hb_IsDateTime(arg1), ::__argToQdate(arg1), QDate() )
RETURN hqlDate( oObject:year(), oObject:month(), oObject:day() )

/*!

 \brief returns a new hqlDate from given string and optionally (default "yyyyMMdd") formatted as ...
 \param(IN) string [,string]
 \return hqlDate

*/
METHOD fromHbString( arg1, arg2 ) CLASS hql_date
   LOCAL oObject :=  IIF ( hb_IsString(arg1), ::__argToQdate(arg1, hb_DefaultValue(arg2, HQL_DTOSQTMASK)), QDate() )
RETURN hqlDate( oObject:year(), oObject:month(), oObject:day() )

/*!

 \brief returns a new hqlDate copied from QDate
 \param(IN) QDate
 \return hqlDate

*/
METHOD fromQdate( arg1 ) CLASS hql_date
   LOCAL oObject :=  IIF ( hql_IsDerived(arg1, "QDate"), ::__argToQdate(arg1), QDate() )
RETURN hqlDate( oObject:year(), oObject:month(), oObject:day() )

/*!

 \brief returns as CL..pper hb_Date
 \param(IN) ...
 \return hb_Date

*/
METHOD hbDate() CLASS hql_date
RETURN ( hb_Date(::oQrealObject:year(), ::oQrealObject:month(), ::oQrealObject:day()) )

/*!

 \brief returns as Harbour numeric value
 \param(IN)
 \return numeric

*/
METHOD hbNum() CLASS hql_date
   LOCAL tDateTime := hb_DateTime(::oQrealObject:year(), ::oQrealObject:month(), ::oQrealObject:day(), 0, 0, 0, 0)
RETURN ( hb_TtoN(tDateTime) )

/*!

 \brief returns as QDate
 \param(IN)
 \return QDate

*/
METHOD QtDate() CLASS hql_date
RETURN ( QDate(::oQrealObject:year(), ::oQrealObject:month(), ::oQrealObject:day()) )

/*!

 \brief returns as string; by default "yyyyMMdd" used
 \param(IN) ...
 \return self

*/
METHOD toHbString( arg1 ) CLASS hql_date
RETURN ::oQrealObject:toString(hb_DefaultValue(arg1, HQL_DTOSQTMASK))

// ==================== PROTECTED section ====================

/*!

 \brief [PROTECTED] compare objects. WARNING must be override
 \param(IN) object , object
 \return numeric

*/
METHOD __compare_object( oX, oY ) CLASS hql_date
   LOCAL nRes
   IF ( (nRes := ::__binary_comparison(oX:year(), oY:year())) == 0 )
      IF ( (nRes := ::__binary_comparison(oX:month(), oY:month())) == 0 )
         nRes := ::__binary_comparison(oX:day(), oY:day())
      ENDIF
   ENDIF
RETURN nRes

/*!

 \brief [PROTECTED] helper to convert into locale string using QLocale_ShortFormat, QLocale_LongFormat, QLocale_NarrowFormat
 \param(IN) numeric
 \return string

   WARNING: I found a problem related with Qt and dateFormat string returned: it seems that Qt map different from "system" and "application"
   with QLocale():setDefault( QLocale():system() ) returns dd/MM/yyyy and it returns Italian (58), Italy (106)
   if I switch language ( e.g. QLocale( Italian, Italy ) the date format is dd/MM/yy
   for this reason, after changes language[s], the shown string is different


*/
METHOD __toLocaleString( nFormat ) CLASS hql_date
   LOCAL oLocal := QLocale()
   LOCAL cString
   cString := ::oQrealObject:toString( oLocal:dateFormat( nFormat ) )
RETURN cString

// ==================== HIDDEN section ====================

/*!

 \brief [HIDDEN] parsing init arguments

*/
METHOD __h_parsingInit( arg1, arg2, arg3 ) CLASS hql_date
   ::oQrealObject := ::__argToQdate( arg1, arg2, arg3 )
RETURN NIL

////////////////////////////////////////  ----  \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

/*!

 \brief [PUBLIC] function returns hql_dateTime new object instance
 \param(IN) ...
 \return object

*/
FUNCTION hqlDateTime( ... )
RETURN hql_dateTime():new( ... )

/*!

 \brief hql_dateTime class definition
 \param(IN) ...
 \return nil

*/
CLASS hql_dateTime INHERIT hql_hbDateTime

   EXPORTED:
   METHOD init
   METHOD hqlCurrentDateTime
   METHOD hqlDate
   METHOD hqlSetDateTime
   METHOD hqlTime
   METHOD fromHbDateTime
   METHOD fromHbString
   METHOD fromQdateTime
   METHOD hbDateTime
   METHOD hbNum
   METHOD QtDateTime
   METHOD toHbString

   PROTECTED:
   METHOD __compare_object                // override
   METHOD __toLocaleString                // override

   HIDDEN:

   METHOD __h_parsingInit
ENDCLASS

/*!

 \brief initialize object instance
 \param(IN) ...
 \return nil

*/
METHOD init( ... ) CLASS hql_dateTime
   ::__h_parsingInit( ... )
RETURN NIL

/*!

 \brief returns today as hqlDateTime
 \param(IN)
 \return hqlDateTime

*/
METHOD hqlCurrentDateTime() CLASS hql_dateTime
   LOCAL oObject := QDateTime():currentDateTime()
RETURN hqlDateTime( oObject:date(), oObject:time() )

/*!

 \brief returns date part as hqlDate
 \param(IN)
 \return hqlDate

*/
METHOD hqlDate() CLASS hql_dateTime
   LOCAL oObject := ::oQrealObject:date()
RETURN hqlDate( oObject:year(), oObject:month(), oObject:day() )

/*!

 \brief set current datetime; accepts:
   - arg1 [,arg2] as string; e.g. "20180228113045765" (by default "yyyyMMddHHmmsszzz" formatted) | ...
   - arg1 as hb_TimeStamp | hb_DateTime
   - arg1 as hb_Date, arg2 = NIL
   - arg1 as QDateTime
   - arg1 as QDate, arg2 as QTime
   - arg1 as QDate, arg2 = NIL
   - arg1, arg2, arg3, arg4, arg5, arg6[, arg7] as numeric value
   ELSE an invalid QDateTime is set
 \param(IN) ...
 \return self

*/
METHOD hqlSetDateTime( arg1, arg2, arg3, arg4, arg5, arg6, arg7 ) CLASS hql_dateTime
   ::oQrealObject := ::__argToQdateTime(arg1, arg2, arg3, arg4, arg5, arg6, arg7)
RETURN self
/*!

 \brief returns date part as hqlTime
 \param(IN)
 \return hqlTime

*/
METHOD hqlTime() CLASS hql_dateTime
   LOCAL oObject := ::oQrealObject:time()
RETURN hqlTime(oObject:hour(), oObject:minute(), oObject:second(), oObject:msec())

/*!

 \brief returns a new hqlDateTime copied from hb_TimeStamp | hb_DateTime
 \param(IN) ...
 \return hqlDateTime

*/
METHOD fromHbDateTime( arg1 ) CLASS hql_dateTime
   LOCAL oObject :=  IIF ( hb_IsTimeStamp(arg1) .OR. hb_IsDateTime(arg1), ::__argToQdateTime(arg1), QDateTime() )
RETURN hqlDateTime( oObject:date(), oObject:time() )

/*!

 \brief returns a new hqlDateTime from given string and optionally (default "yyyyMMddHHmmsszzz") formatted as ...
 \param(IN) string [,string]
 \return hqlDateTime

*/
METHOD fromHbString( arg1, arg2 ) CLASS hql_dateTime
   LOCAL oObject :=  IIF ( hb_IsString(arg1), ::__argToQdateTime(arg1, hb_DefaultValue(arg2, HQL_TTOSQTMASK)), QDateTime() )
RETURN hqlDateTime( oObject:date(), oObject:time() )

/*!

 \brief returns a new hqlDateTime copied from QDateTime
 \param(IN) QDate
 \return hqlDateTime

*/
METHOD fromQdateTime( arg1 ) CLASS hql_dateTime
   LOCAL oObject :=  IIF ( hql_IsDerived(arg1, "QDateTime"), ::__argToQdateTime(arg1), QDateTime() )
RETURN hqlDateTime( oObject:date(), oObject:time() )

/*!

 \brief returns as CL..pper hb_DateTime
 \param(IN) ...
 \return hb_DateTime

*/
METHOD hbDateTime() CLASS hql_dateTime
   LOCAL oQdate := ::oQrealObject:date()
   LOCAL oQtime := ::oQrealObject:time()
RETURN ( hb_DateTime(oQdate:year(), oQdate:month(), oQdate:day(), oQtime:hour(), oQtime:minute(), oQtime:second(), oQtime:msec()) )

/*!

 \brief returns as Harbour numeric value
 \param(IN)
 \return numeric

*/
METHOD hbNum() CLASS hql_dateTime
RETURN ( hb_TtoN( ::hbDateTime() ) )

/*!

 \brief returns as QDateTime
 \param(IN)
 \return QDateTime

*/
METHOD QtDateTime() CLASS hql_dateTime
RETURN ( QDateTime(::oQrealObject:date(), ::oQrealObject:time()) )
/*!

 \brief returns as string; by default "yyyyMMdd" used
 \param(IN) ...
 \return self

*/
METHOD toHbString( arg1 ) CLASS hql_dateTime
RETURN ::oQrealObject:toString(hb_DefaultValue(arg1, HQL_TTOSQTMASK))

// ==================== PROTECTED section ====================

/*!

 \brief [PROTECTED] compare objects. WARNING must be override
 \param(IN) object , object
 \return numeric

*/
METHOD __compare_object( oX, oY ) CLASS hql_dateTime
   LOCAL nRes
   LOCAL nX := hb_TtoN(oX:hbDateTime())
   LOCAL nY := hb_TtoN(oY:hbDateTime())
   nRes := ::__binary_comparison(nX, nY)   //toTime_t is deprecated from Qt4.7 but is the only function at this moment on HbQt
RETURN nRes

/*!

 \brief [PROTECTED] helper to convert into locale string using QLocale_ShortFormat, QLocale_LongFormat, QLocale_NarrowFormat
 \param(IN) numeric
 \return string

*/
METHOD __toLocaleString( nFormat ) CLASS hql_dateTime
RETURN ( ::oQrealObject:toString( QLocale():dateTimeFormat( nFormat ) ) )

// ==================== HIDDEN section ====================

/*!

 \brief [HIDDEN] parsing init arguments

*/
METHOD __h_parsingInit( arg1, arg2, arg3, arg4, arg5, arg6, arg7 ) CLASS hql_dateTime
   ::oQrealObject := ::__argToQdateTime( arg1, arg2, arg3, arg4, arg5, arg6, arg7 )
RETURN NIL

////////////////////////////////////////  ----  \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

/*!

 \brief [PUBLIC] function returns hql_time new object instance
 \param(IN) ...
 \return object

*/
FUNCTION hqlTime( ... )
RETURN hql_time():new( ... )

/*!

 \brief hql_time class definition
 \param(IN) ...
 \return nil

*/
CLASS hql_time INHERIT hql_hbDateTime

   EXPORTED:
   METHOD init
   METHOD hqlCurrentTime
   METHOD hqlSetTime
   METHOD fromHbTime
   METHOD fromHbString
   METHOD fromQtime
   METHOD hbTime
   METHOD hbNum
   METHOD QtTime
   METHOD toHbString

   PROTECTED:
   METHOD __compare_object                // override
   METHOD __toLocaleString                // override

   HIDDEN:

   METHOD __h_parsingInit
ENDCLASS

/*!

 \brief initialize object instance
 \param(IN) ...
 \return nil

*/
METHOD init( ... ) CLASS hql_time
   ::__h_parsingInit( ... )
RETURN NIL

/*!

 \brief returns now as hqlTime
 \param(IN)
 \return hqlTime

*/
METHOD hqlCurrentTime() CLASS hql_time
   LOCAL oObject := QTime():currentTime()
RETURN hqlTime( oObject:hour(), oObject:minute(), oObject:second(), oObject:msec() )

/*!

 \brief set current time; accepts:
   - arg1 [,arg2] as string; e.g. "113542987" (by default "HHmmsszzz" formatted) | ...
   - arg1 as hb_DateTime
   - arg1 as QTime
   - arg1, arg2, arg3 [,arg4] as numeric value
   ELSE an invalid QTime is set
 \param(IN) ...
 \return self

*/
METHOD hqlSetTime( arg1, arg2, arg3, arg4 ) CLASS hql_time
   ::oQrealObject := ::__argToQtime(arg1, arg2, arg3, arg4)
RETURN self

/*!

 \brief returns a new hqlTime copied from hb_DateTime
 \param(IN) ...
 \return hqlTime

*/
METHOD fromHbTime( arg1 ) CLASS hql_time
   LOCAL oObject :=  IIF ( hb_IsDateTime(arg1), ::__argToQtime(arg1), QTime() )
RETURN hqlTime( oObject:hour(), oObject:minute(), oObject:second(), oObject:msec() )

/*!

 \brief returns a new hqlTime from given string and optionally (default "HHmmsszzz") formatted as ...
 \param(IN) string [,string]
 \return hqlTime

*/
METHOD fromHbString( arg1, arg2 ) CLASS hql_time
   LOCAL oObject :=  IIF ( hb_IsString(arg1), ::__argToQtime(arg1, hb_DefaultValue(arg2, HQL_TITOSQTMASK)), QTime() )
RETURN hqlTime( oObject:hour(), oObject:minute(), oObject:second(), oObject:msec() )

/*!

 \brief returns a new hqlTime copied from QTime
 \param(IN) QTime
 \return hqlTime

*/
METHOD fromQtime( arg1 ) CLASS hql_time
   LOCAL oObject :=  IIF ( hql_IsDerived(arg1, "QTime"), ::__argToQtime(arg1), QTime() )
RETURN hqlTime( oObject:hour(), oObject:minute(), oObject:second(), oObject:msec() )

/*!

 \brief returns as CL..pper hb_DateTime
 \param(IN) ...
 \return hb_DateTime

*/
METHOD hbTime() CLASS hql_time
RETURN ( hb_DateTime(NIL, NIL, NIL, ::oQrealObject:hour(), ::oQrealObject:minute(), ::oQrealObject:second(), ::oQrealObject:msec()) )

/*!

 \brief returns as Harbour numeric value
 \param(IN)
 \return numeric

*/
METHOD hbNum() CLASS hql_time
   LOCAL tDateTime := hb_DateTime(0, 0, 0, ::oQrealObject:hour(), ::oQrealObject:minute(), ::oQrealObject:second(), ::oQrealObject:msec())
RETURN ( hb_TtoN(tDateTime) )

/*!

 \brief returns as QTime
 \param(IN)
 \return QTime

*/
METHOD QtTime() CLASS hql_time
RETURN ( QTime(::oQrealObject:hour(), ::oQrealObject:minute(), ::oQrealObject:second(), ::oQrealObject:msec()) )

/*!

 \brief returns as string; by default "HHmmsszzz" used
 \param(IN) ...
 \return self

*/
METHOD toHbString( arg1 ) CLASS hql_time
RETURN ::oQrealObject:toString(hb_DefaultValue(arg1, HQL_TITOSQTMASK))

// ==================== PROTECTED section ====================

/*!

 \brief [PROTECTED] compare objects. WARNING must be override
 \param(IN) object , object
 \return numeric

*/
METHOD __compare_object( oX, oY ) CLASS hql_time
   LOCAL nRes
   IF ( (nRes := ::__binary_comparison(oX:hour(), oY:hour())) == 0 )
      IF ( (nRes := ::__binary_comparison(oX:minute(), oY:minute())) == 0 )
         IF ( (nRes := ::__binary_comparison(oX:second(), oY:second())) == 0 )
            nRes := ::__binary_comparison(oX:msec(), oY:msec())
         ENDIF
      ENDIF
   ENDIF
RETURN nRes

/*!

 \brief [PROTECTED] helper to convert into locale string using QLocale_ShortFormat, QLocale_LongFormat, QLocale_NarrowFormat
 \param(IN) numeric
 \return string

*/
METHOD __toLocaleString( nFormat ) CLASS hql_time
RETURN ( ::oQrealObject:toString( QLocale():timeFormat( nFormat ) ) )

// ==================== HIDDEN section ====================

/*!

 \brief [HIDDEN] parsing init arguments

*/
METHOD __h_parsingInit( arg1, arg2, arg3, arg4 ) CLASS hql_time
   ::oQrealObject := ::__argToQtime( arg1, arg2, arg3, arg4 )
RETURN NIL

////////////////////////////////////////  ----  \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

/*!

 \brief common functions for hql_date, hql_datetime, hql_time

   REMEMBER hb_IsDate(var)      => TRUE if var is "D"
   REMEMBER hb_IsTimeStamp(var) => TRUE if var is "T"
   REMEMBER hb_IsDateTime(var) => TRUE if var is "T" .OR. "D"

*/
CLASS hql_hbDateTime STATIC

   ERROR HANDLER __hql_onError( ... )

   EXPORTED:
   METHOD hqlDestroyer
   METHOD toLocaleString
   OPERATOR "==" ARGS ...                 INLINE ::__OprEqEq( ... )     // do not use __OPxxxx see harbour/src/vm/classes.c
   OPERATOR "!=" ARGS ...                 INLINE ::__OprNotEq( ... )    // do not use __OPxxxx see harbour/src/vm/classes.c
   OPERATOR "<" ARGS ...                  INLINE ::__OprLess( ... )     // do not use __OPxxxx see harbour/src/vm/classes.c
   OPERATOR "<=" ARGS ...                 INLINE ::__OprLessEq( ... )   // do not use __OPxxxx see harbour/src/vm/classes.c
   OPERATOR ">" ARGS ...                  INLINE ::__OprGreat( ... )    // do not use __OPxxxx see harbour/src/vm/classes.c
   OPERATOR ">=" ARGS ...                 INLINE ::__OprGreatEq( ... )  // do not use __OPxxxx see harbour/src/vm/classes.c

   PROTECTED:
   VAR oQrealObject                       INIT NIL
   METHOD __binary_comparison
   METHOD __compare_object
   METHOD __argToQdate
   METHOD __argToQdateTime
   METHOD __argToQtime
   METHOD __toLocaleString

   METHOD __OprEqEq
   METHOD __OprNotEq
   METHOD __OprLess
   METHOD __OprLessEq
   METHOD __OprGreat
   METHOD __OprGreatEq

   HIDDEN:

   METHOD __h_hqlDestroyer
ENDCLASS

/*!

 \brief callable hql destroyer
 \param[in] none
 \return NIL

*/
METHOD hqlDestroyer() CLASS hql_hbDateTime
   ::__h_hqlDestroyer()
RETURN NIL

/*!

 \brief convert into locale string
 \param[in] format QLocale_ShortFormat, QLocale_LongFormat, QLocale_NarrowFormat
 \return string

*/
METHOD toLocaleString( nFormat ) CLASS hql_hbDateTime
   nFormat := INT(hb_DefaultValue(nFormat, QLocale_ShortFormat))
   nFormat := IIF( nFormat < QLocale_LongFormat .OR. nFormat > QLocale_NarrowFormat, QLocale_LongFormat, nFormat )
RETURN ( ::__toLocaleString(nFormat) )

// ==================== PROTECTED section ====================

/*!

 \brief [PROTECTED] returns QDate value; it accepts arguments as:
   - arg1 [,arg2] as string; e.g. "20180228" (by default "yyyyMMdd" formatted) | "28022019", "ddMMyyyy"
   - arg1 as hb_Date | hb_DateTime
   - arg1 as QDate
   - arg1, arg2, arg3 as numeric value
   ELSE an invalid QDate is returned
 \param(IN) ...
 \return QDate

*/
METHOD __argToQdate( arg1, arg2, arg3 ) CLASS hql_hbDateTime
   LOCAL oObject
   IF ( hb_IsString(arg1) )
      oObject := QDate():fromString(arg1, hb_DefaultValue(arg2, HQL_DTOSQTMASK)) // assume is a "yyyyMMdd" string formatted
   ELSEIF ( hb_IsDate(arg1) .OR. hb_IsDateTime(arg1) )
      oObject := QDate( YEAR(arg1), MONTH(arg1), DAY(arg1) )
   ELSEIF ( hql_IsDerived(arg1, "QDate") )
      oObject := QDate(arg1:year(), arg1:month(), arg1:day())
   ELSEIF ( hb_IsNumeric(arg1) .AND. hb_IsNumeric(arg2) .AND. hb_IsNumeric(arg3) )
      oObject := QDate(arg1, arg2, arg3)  // follow QDate arguments year, month, day
   ELSE
      oObject := QDate()
   ENDIF
RETURN oObject

/*!

 \brief [PROTECTED] returns QDateTime value; it accepts arguments as:
   - arg1 [,arg2] as string; e.g. "20180228113045765" (by default "yyyyMMddHHmmsszzz" formatted) | ...
   - arg1 as hb_TimeStamp | hb_DateTime
   - arg1 as hb_Date, arg2 = NIL
   - arg1 as QDateTime
   - arg1 as QDate, arg2 as QTime
   - arg1 as QDate, arg2 = NIL
   - arg1, arg2, arg3, arg4, arg5, arg6[, arg7] as numeric value
   ELSE an invalid QDateTime is returned
 \param(IN) ...
 \return QDateTime

*/
METHOD __argToQdateTime( arg1, arg2, arg3, arg4, arg5, arg6, arg7 ) CLASS hql_hbDateTime
   LOCAL oObject
   LOCAL oQdate, oQtime
   IF ( hb_IsString(arg1) )
      oObject := QDateTime():fromString(arg1, hb_DefaultValue(arg2, HQL_TTOSQTMASK)) // assume is a "yyyyMMddHHmmsszzz" string formatted
   ELSEIF ( hb_IsTimeStamp(arg1) .OR. hb_IsDateTime(arg1) )
      oQdate := ::__argToQdate(arg1)
      oQtime := ::__argToQtime(arg1)
      oObject := QDateTime(oQdate,oQtime)
   ELSEIF ( hb_IsDate(arg1) .AND. arg2 == NIL )
      oObject := QDateTime(::__argToQdate(arg1))
   ELSEIF ( hql_IsDerived(arg1, "QDateTime") )
      oObject := QDateTime(arg1:date(), arg1:time())
   ELSEIF ( hql_IsDerived(arg1, "QDate") .AND. hql_IsDerived(arg2, "QTime") )
      oObject := QDateTime(arg1, arg2)
   ELSEIF ( hql_IsDerived(arg1, "QDate") .AND. arg2 == NIL )
      oObject := QDateTime(arg1)
   ELSEIF ( hb_IsNumeric(arg1) .AND. hb_IsNumeric(arg2) .AND. hb_IsNumeric(arg3) .AND. hb_IsNumeric(arg4) .AND. hb_IsNumeric(arg5) .AND. hb_IsNumeric(arg6) )
      oQdate := ::__argToQdate(arg1, arg2, arg3)
      oQtime := ::__argToQtime(arg4, arg5, arg6, hb_DefaultValue(arg7,0))
      oObject := QDateTime(oQdate,oQtime)
   ELSE
      oObject := QDateTime()
   ENDIF
RETURN oObject

/*!

 \brief [PROTECTED] returns QTime value; it accepts arguments as:
   - arg1 [,arg2] as string; e.g. "113542987" (by default "HHmmsszzz" formatted) | ...
   - arg1 as hb_DateTime
   - arg1 as QTime
   - arg1, arg2, arg3 [,arg4] as numeric value
   ELSE an invalid QTime is returned
 \param(IN) ...
 \return QTime

*/
METHOD __argToQtime( arg1, arg2, arg3, arg4 ) CLASS hql_hbDateTime
   LOCAL oObject
   LOCAL nSec, nMsec
   IF ( hb_IsString(arg1) )
      oObject := QTime():fromString(arg1, hb_DefaultValue(arg2, HQL_TITOSQTMASK)) // assume is a "HHmmsszzz" string formatted
   ELSEIF ( hb_IsDateTime(arg1) )
      nSec := INT(hb_Sec(arg1))
      nMsec := ROUND((hb_Sec(arg1) - nSec) * 1000, 0)
      oObject := QTime( hb_Hour(arg1), hb_Minute(arg1), nSec, nMsec )
   ELSEIF ( hql_IsDerived(arg1, "QTime") )
      oObject := QTime(arg1:hour(), arg1:minute(), arg1:second(), arg1:msec())
   ELSEIF ( hb_IsNumeric(arg1) .AND. hb_IsNumeric(arg2) .AND. hb_IsNumeric(arg3) )
      oObject := QTime(arg1, arg2, arg3, hb_DefaultValue(arg4,0))  // follow QTime arguments hour, minute, second [,msec]
   ELSE
      oObject := QTime()
   ENDIF
RETURN oObject

/*
 \brief [PROTECTED] compare value
 \param(IN) numeric , numeric
 \return
   - `1` if x is higher than y
   - `0` if x is equal to y
   - `-1` if x is lower than y
*/
METHOD __binary_comparison( nX, nY ) CLASS hql_hbDateTime
   LOCAL nRes
   IF ( nX > nY )
      nRes := 1
   ELSEIF ( nX == nY )
      nRes := 0
   ELSE
      nRes := -1
   ENDIF
RETURN nRes

/*!

 \brief [PROTECTED] compare objects. WARNING must be override
 \param(IN) object , object
 \return numeric

*/
METHOD __compare_object( oX, oY ) CLASS hql_hbDateTime
//   LOCAL nRes

//   IF ( hql_IsDerived(::oQrealObject, "QDate") )
//      IF ( (nRes := ::__binary_comparison(x:year(), y:year())) == 0 )
//         IF ( (nRes := ::__binary_comparison(x:month(), y:month())) == 0 )
//            nRes := ::__binary_comparison(x:day(), y:day())
//         ENDIF
//      ENDIF
//   ELSEIF ( hql_IsDerived(::oQrealObject, "QTime") )
//      IF ( (nRes := ::__binary_comparison(x:hour(), y:hour())) == 0 )
//         IF ( (nRes := ::__binary_comparison(x:minute(), y:minute())) == 0 )
//            IF ( (nRes := ::__binary_comparison(x:second(), y:second())) == 0 )
//               nRes := ::__binary_comparison(x:msec(), y:msec())
//            ENDIF
//         ENDIF
//      ENDIF
//   ENDIF

//RETURN nRes
   HB_SYMBOL_UNUSED(oX)
   HB_SYMBOL_UNUSED(oY)
RETURN 0

/*!

 \brief [PROTECTED] helper to convert into locale string using QLocale_ShortFormat, QLocale_LongFormat, QLocale_NarrowFormat
 \param(IN) numeric
 \return string

*/
METHOD __toLocaleString( /*nFormat*/ ) CLASS hql_hbDateTime
//e.g. RETURN ( ::oQrealObject:toString( QLocale():datetimeFormat( nFormat ) ) )
RETURN ""

/*!

 \brief [HIDDEN] overloading "==" operator
 \param[in] object
 \return bool

*/
METHOD __OprEqEq( arg1 ) CLASS hql_hbDateTime
RETURN ( ::__compare_object( self, arg1) == 0 )

/*!

 \brief [HIDDEN] overloading "!=" operator
 \param[in] object
 \return bool

*/
METHOD __OprNotEq( arg1 ) CLASS hql_hbDateTime
RETURN ( !::__OprEqEq( arg1 ) )

/*!

 \brief [HIDDEN] overloading "<" operator
 \param[in] object
 \return bool

*/
METHOD __OprLess( arg1 ) CLASS hql_hbDateTime
RETURN ( ::__compare_object( self, arg1) < 0 )

/*!

 \brief [HIDDEN] overloading "<=" operator
 \param[in] object
 \return bool

*/
METHOD __OprLessEq( arg1 ) CLASS hql_hbDateTime
RETURN ( ::__compare_object( self, arg1) <= 0 )

/*!

 \brief [HIDDEN] overloading ">" operator
 \param[in] object
 \return bool

*/
METHOD __OprGreat( arg1 ) CLASS hql_hbDateTime
RETURN ( ::__compare_object( self, arg1) > 0 )

/*!

 \brief [HIDDEN] overloading ">=" operator
 \param[in] object
 \return bool

*/
METHOD __OprGreatEq( arg1 ) CLASS hql_hbDateTime
RETURN ( ::__compare_object( self, arg1) >= 0 )

// ==================== HIDDEN section ====================

/*!

 \brief [HIDDEN] hql destroyer.
 \param[in] none
 \return NIL

*/
METHOD __h_hqlDestroyer() CLASS hql_hbDateTime
   ::oQrealObject := NIL
RETURN NIL

/*!

 \brief onerror

*/
METHOD __hql_onError( ... ) CLASS hql_hbDateTime

   LOCAL cMsg := __GetMessage()
   LOCAL oErr

   cMsg := IIF( LEFT( cMsg, 1 ) == hb_Uchar(0x5F), SUBSTR( cMsg, 2 ), cMsg )
   IF __objHasMsgAssigned( ::oQrealObject, cMsg )
      RETURN __objSendMsg( ::oQrealObject, cMsg, ... )
   ENDIF

   oErr := ERRORNEW()
   oErr:Args          := { ... }
   oErr:CanDefault    := .F.
   oErr:CanRetry      := .F.
   oErr:CanSubstitute := .F.
   oErr:Description   := "Invalid class member"
   oErr:GenCode       := EG_NOMETHOD
   oErr:Severity      := ES_ERROR

RETURN HqlThrow( oErr )
