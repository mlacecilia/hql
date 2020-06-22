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
#ifndef __ADDRBOOK_CH
#define __ADDRBOOK_CH

#include "hqlinclude.ch"

#xtranslate appcnf => hqlSconfig()

#define FORMACTION_NONE                0
#define FORMACTION_INSERT              1
#define FORMACTION_EDIT                2
#define FORMACTION_DELETE              3
#define FORMACTION_MIN                 FORMACTION_NONE
#define FORMACTION_MAX                 FORMACTION_DELETE

#endif /* __ADDRBOOK_CH */
