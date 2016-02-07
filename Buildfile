[*buildutil*]
tasks=gitrev,buildutil,packets

[gitrev]
tool=lazbuild
project=gitrev.lpi

[buildutil]
tool=lazbuild
project=buildutil.lpi

[packets]
tasks=pack-gitrev,pack-buildutil

[pack-gitrev]
tool=zip
filename=gitrev-win32.zip
files=gitrev.files

[gitrev.files]
gitrev.exe=
README.md=
git_rev.pas.sample=

[pack-buildutil]
tool=zip
filename=buildutil-win32.zip
files=buildutil.files

[buildutil.files]
buildutil.exe=
