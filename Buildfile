[*main*]
tasks=win32,all

[win32]
tool=env
LB_OS=win32

[all]
tasks=gitrev,buildutil,packets

[gitrev]
tool=lazbuild
project=gitrev.lpi
build=clean

[buildutil]
tool=lazbuild
project=buildutil.lpi
build=clean

[packets]
tasks=pack-gitrev,pack-buildutil

[pack-gitrev]
tool=zip
filename=gitrev-${LB_OS}.zip
files=gitrev.files

[gitrev.files]
gitrev.exe=
README.md=
git_rev.pas.sample=

[pack-buildutil]
tool=zip
filename=buildutil-${LB_OS}.zip
files=buildutil.files

[buildutil.files]
buildutil.exe=
