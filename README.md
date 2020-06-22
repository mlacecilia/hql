# **H**arbour **Q**tContribs **L**ayer
HQL is a xBase library; it is a layer between xBase sources and [QtContribs](https://sourceforge.net/projects/qtcontribs/) library (xBase binding to [Qt](https://www.qt.io/) framework).
HQL must be compiled with [Harbour](https://harbour.github.io/).
For this reason where [Harbour](https://harbour.github.io/) and [QtContribs](https://sourceforge.net/projects/qtcontribs/) can be installed, compiled and run, HQL can be used. 


# Table of Content

1. [Guarantees and Liability](#guarantees-and-liability)
1. [License](#license)
1. [Prerequisites](#prerequisites)
1. [How to Get](#how-to-get)
1. [How to Build](#how-to-build)
1. [Get Help](#get-help)

## Guarantees and Liability
HQL is distributed in the hope it will be useful, but there is *NO GUARANTEE* that it is complete, accurate, non-infringing or usable for any purpose whatsoever.
Contributors are *NOT LIABLE* for any damages that result from using HQL in any ways. For more legal details, see [License](#License).
The information of this document is subject to change without notice and does not represent any future commitment by the participants of the project.
   
## License
HQL is licensed under the Gnu GPL license; read [LICENSE.GPL](LICENSE.GPL) for details.
About documentation, notes, icons, images, fonts, in other words all materials not strictly related with sources required to build HQL library, is licensed under the Creative Commons license; read [LICENSE.CC](LICENSE.CC) for details.
   
## Prerequisites
- [ ] [Harbour](https://harbour.github.io/) installed and usable; for any questions or problems visit [Harbour Users Group](https://groups.google.com/forum/#!forum/harbour-users) forum.
- [ ] [QtContribs](https://sourceforge.net/projects/qtcontribs/) installed and usable; for any questions or problems visit [QtContribs Users Group](https://groups.google.com/forum/?hl=it&fromgroups#!forum/qtcontribs)
- [ ] [Qt](https://www.qt.io/) installed and usable.
*Any questions or problems related with these tools are out ot of scope of this project.*
- [ ] Supported ANSI C compiler / toolchain

## How to Get
*WARNING:* always read [CHANGELOG.txt](CHANGELOG.txt?raw=true) before usage.

Visit [HQL project](<https://github.com/projects/hql/>) and download.

* Using Git version control software installed on your system, issue this command:
`git clone https://github.com/hql/core.git`
You can get subsequent updates using this command:
`git pull`

* Alternative
Click on button [Clone or download](<https://github.com/projects/hql/>) at the top right.

## How to Build

*WARNING:* always keep in sync the C toolchain, with the same or upper version used to build Harbour and QtContribs. For this reason, I suggest to build Harbour and QtContribs by your self. To build Harbour and QtContribs follow their documentation.

#### Preferred way
Copy/expand HQL within Harbour tree under 'addons' directory 
```
harbour
+- addons
   +- hql
```

Enter Harbour 'hbmk2' command
```console
cd harbour/addons/hql
hbmk2 hql
```
*as suggestion, redirect STDOUT & STDERR to file*
```console
cd harbour/addons/hql
hbmk2 hql >errorlog.txt 2>&1
```
#### Alternative
Copy/expand HQL in your preferred path. Enter Harbour 'hbmk2' command
```console
cd yourHQLpath
hbmk2 hql
```

Copy to *harbour/addons/hql* directory:
- [x] 'include' directory
- [x] 'lib' directoy
- [x] *.hbc files

#### Build examples
HQL has a rich examples directory. To build each program, enter Harbour 'hbmk2' command in each directory; e.g.
```console
cd ...hql/samples/pushbutton
hbmk2 oop001
```
*WARNING:* sometimes, example has icons, images, fonts, translations, etc. has *external resources*. You need to copy these subfolders/files along with executable program.

## Get Help
ASAP will be defined.

---
Copyright &copy;&nbsp;2020-present Luigi Ferraris [![Creative Commons Attribution-ShareAlike 4.0](https://mirrors.creativecommons.org/presskit/buttons/80x15/svg/by-sa.svg)](https://creativecommons.org/licenses/by-sa/4.0/)
