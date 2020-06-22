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
#ifndef __QURL_CH
#define __QURL_CH

   // enum QUrl::UrlFormattingOption
   // flags QUrl::FormattingOptions
#define QUrl_None                                 0x0 // The format of the URL is unchanged.
#define QUrl_RemoveScheme                         0x1 // The scheme is removed from the URL.
#define QUrl_RemovePassword                       0x2 // Any password in the URL is removed.
#define QUrl_RemoveUserInfo                       hb_BitOr( QUrl_RemovePassword, 0x4 ) // Any user information in the URL is removed.
#define QUrl_RemovePort                           0x8 // Any specified port is removed from the URL.
#define QUrl_RemoveAuthority                       hb_BitOr( QUrl_RemoveUserInfo, QUrl_RemovePort, 0x10 )
#define QUrl_RemovePath                           0x20   // The URL's path is removed, leaving only the scheme, host address, and port (if present).
#define QUrl_RemoveQuery                          0x40   // The query part of the URL (following a '?' character) is removed.
#define QUrl_RemoveFragment                       0x80
#define QUrl_RemoveFilename                       0x800  // The filename (i.e. everything after the last '/' in the path) is removed. The trailing '/' is kept, unless StripTrailingSlash is set. Only valid if RemovePath is not set.
#define QUrl_PreferLocalFile                      0x200  // If the URL is a local file according to isLocalFile() and contains no query or fragment, a local file path is returned.
#define QUrl_StripTrailingSlash                   0x400  // The trailing slash is removed from the path, if one is present.
#define QUrl_NormalizePathSegments                0x1000 // Modifies the path to remove redundant directory separators, and to resolve "."s and ".."s (as far as possible).

#endif /* __QURL_CH */
