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
#ifndef __HQLHBQT_CH
#define __HQLHBQT_CH

#include "hbqtgui.ch"
#include "hbqtcore.ch"
#include "hqlversion.ch"
#include "hqlmath.ch"

// Singleton for Hql framework
#xtranslate HqlFw => __Hql_FrameWork()

// error throw
#xtranslate hqlThrow( <oErr> ) => EVAL( ERRORBLOCK(), <oErr> )

// used to identify SIGNALS or EVENTS handling methods within class; usage SIGNAL <name> CLASS <name> instead METHOD <name> CLASS <name>
#define SIGNAL METHOD

// enum *window and dialog center style
#define HQL_NOCENTER                0x00  // none centering requested, so based on Os style
#define HQL_PARENTCENTER            0x01  // (default) center related to parent geometry; whitout parent, desktop will be used as reference
#define HQL_DESKTOPCENTER           0x02  // center related to desktop geometry

// mask to convert date or time Harbour<->Qt
#define HQL_DTOSQTMASK              "yyyyMMdd"
#define HQL_TITOSQTMASK             "HHmmsszzz"
#define HQL_TTOSQTMASK              HQL_DTOSQTMASK + HQL_TITOSQTMASK
#define HQL_TIMEQTMASK              "HH:mm:ss"

// enum status datetime show
#define HQL_STATUSDTTIME            0x01  // shows only time
#define HQL_STATUSDTDATE            0x02  // shows only date
#define HQL_STATUSDTALL             hb_BitOr( HQL_STATUSDTTIME, HQL_STATUSDTDATE ) // (default) show all

// enum case letter
#define HQL_UPPERLOWERCASE          0x00 // case letter mixed (lower and upper accepted )
#define HQL_ALL_UPPERCASE           0x01 // case letter UPPER
#define HQL_ALL_LOWERCASE           0x02 // case letter lower

// add here undefined HbQt constants as <filename>.ch
#include "hql_qabstractanimation.ch"
#include "hql_qfontdialog.ch"
#include "hql_qurl.ch"
#include "hql_qmovie.ch"
#include "hql_qobject.ch"

#endif /* __HQLHBQT_CH */
