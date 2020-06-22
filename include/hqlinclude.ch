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
#ifndef __HQLINCLUDE_CH
#define __HQLINCLUDE_CH

#include "hqlhbqt.ch"

#xtranslate hqlActiveWindow         => ;
   HqlFw:QtApplication:activeWindow()

#xtranslate hqlAddApplicationFont( <csource> ) => ;
   HqlFw:addApplicationFont( <csource> )

#xtranslate hqlAutoNameEnabled( <lbool> ) => ;
   HqlFw:autoNameEnabled( <lbool> )

#xtranslate hqlDesktopFreeHeight    => ;
   HqlFw:QtDeskTop:availableGeometry( -1 ):height()

#xtranslate hqlDesktopFreeWidth     => ;
   HqlFw:QtDeskTop:availableGeometry( -1 ):width()

#xtranslate hqlDesktopHeight        => ;
   HqlFw:QtDeskTop:screenGeometry( -1 ):height()

#xtranslate hqlDesktopWidth         => ;
   HqlFw:QtDeskTop:screenGeometry( -1 ):width()

#xtranslate hqlFocusWidget          => ;
   HqlFw:QtApplication:focusWidget()

#xtranslate HqlGetMainWindow        => ;
   HqlFw:getMainWindow()

#xtranslate hqlIsMainWindowDefined  => ;
   HqlFw:isMainWindowDefined()

#xtranslate hqlLoadQss( [<filename>] ) => ;
   HqlFw:loadQss( [<filename>] )

#xtranslate hqlOnAboutToQuit( [<block>] ) => ;
   HqlFw:onAboutToQuit( [<block>] )

#xtranslate hqlPostEvent( <oObject>, <oEvent> [,<nPrior>] ) => ;
   HqlFw:postEvent( <oObject>, <oEvent> [,<nPrior>] )

#xtranslate hqlProcessEvents( [<nFlags>] [,<nMsecs>] )   => ;
   HqlFw:processEvents( [<nFlags>] [,<nMsecs>] )

#xtranslate hqlQApplication         => ;
   HqlFw:QtApplication()

#xtranslate hqlQClipboard           => ;
   HqlFw:QtApplication:clipboard()

#xtranslate hqlQDesktop             => ;
   HqlFw:QtDeskTop()

#xtranslate hqlQLibraryInfo         => ;
   HqlFw:QtLibraryInfo()

#xtranslate hqlQLocale              => ;
   HqlFw:QtLocale()

#xtranslate hqlQResource            => ;
   HqlFw:QtResource()

#xtranslate hqlQuit([<nCode>])               => ;
   HqlFw:quit( [<nCode>] )

#xtranslate hqlRegisterResData( <rccdata> )  => ;
   HqlFw:registerResData( <rccdata> )

#xtranslate hqlRegisterResFile( <filename> ) => ;
   HqlFw:registerResFile( <filename> )

#xtranslate hqlSendEvent( <oObject>, <oEvent> ) => ;
   HqlFw:sendEvent( <oObject>, <oEvent> )

#xtranslate hqlSendKey( [<x,...>] ) => ;
   HqlFw:sendKey( <x> )

#xtranslate hqlSetStyle( <cstyle> ) => ;
   HqlFw:setStyle( <cstyle> )

#xtranslate hqlStart()              => ;
   HqlFw:start()

#xtranslate hqlSwitchLang( <nLang> [,<nCountry>] ) => ;
   HqlFw:switchLang( <nLang> [,<nCountry>] )

#xtranslate hqlTran( [<Args,...>] ) => ;
   HqlFw:translate( [<Args>] )

#xtranslate hqlTranslations         => ;
   HqlFw:translations()

#endif /* __HQLINCLUDE_CH */
