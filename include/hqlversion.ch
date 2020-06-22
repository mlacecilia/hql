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

/*!

   following https://semver.org/
   HQL_VER_MAJOR . HQL_VER_MINOR . HQL_VER_PATCH [-HQL_VER_STATUS] [+HQL_VER_BUILD]

   A) with named groups for those systems that support them (PCRE [Perl Compatible Regular Expressions, i.e. Perl, PHP and R], Python and Go).
   See: https://regex101.com/r/Ly7O1x/3/
   ^(?P<major>0|[1-9]\d*)\.(?P<minor>0|[1-9]\d*)\.(?P<patch>0|[1-9]\d*)(?:-(?P<prerelease>(?:0|[1-9]\d*|\d*[a-zA-Z-][0-9a-zA-Z-]*)(?:\.(?:0|[1-9]\d*|\d*[a-zA-Z-][0-9a-zA-Z-]*))*))?(?:\+(?P<buildmetadata>[0-9a-zA-Z-]+(?:\.[0-9a-zA-Z-]+)*))?$

   B) And one with numbered capture groups instead (so cg1 = major, cg2 = minor, cg3 = patch, cg4 = prerelease and cg5 = buildmetadata) that is compatible with ECMA Script (JavaScript), PCRE (Perl Compatible Regular Expressions, i.e. Perl, PHP and R), Python and Go.
   See: https://regex101.com/r/vkijKf/1/
   ^(0|[1-9]\d*)\.(0|[1-9]\d*)\.(0|[1-9]\d*)(?:-((?:0|[1-9]\d*|\d*[a-zA-Z-][0-9a-zA-Z-]*)(?:\.(?:0|[1-9]\d*|\d*[a-zA-Z-][0-9a-zA-Z-]*))*))?(?:\+([0-9a-zA-Z-]+(?:\.[0-9a-zA-Z-]+)*))?$

   ALERT! about HQL_VER_STATUS only lowercase; eg. "alpha", "beta", "stable"
   ALERT! about HQL_VER_BUILD only numeric; eg. 123 , 6784; do not strictly related to semVer (eg. 1.2.3)
   ALERT! about HQL_VER_BUILD only show when > 0
   ALERT! about HQL_VER_HEX three bytes: Major + Minor + Patch. This is recommented for 3rd party .c and .prg level code.

*/
#ifndef __HQLVERSION_CH
#define __HQLVERSION_CH

#define HQL_VER_MAJOR     6
#define HQL_VER_MINOR     1
#define HQL_VER_PATCH     1
#define HQL_VER_STATUS    ""
#define HQL_VER_BUILD     0
#define HQL_VER_HEX       0x060101

#endif /* __HQLVERSION_CH */
