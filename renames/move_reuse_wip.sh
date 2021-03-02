# Move the files with the clearest pattern of reuse to lib/ and
# update references, best effort.
#
# A couple of conflicts still have to be updated for the tests to run.

set +e

git mv biolink/repr.rkt biolink/lib/
perl -p -i -e 's@repr.rkt@lib/repr.rkt@g;' $(find biolink -type f -name \*.rkt)
perl -p -i -e 's@(?<=[^-])lib/([a-z0-9_-]+).rkt@./$1/mk.rkt@g;' $(find biolink/lib -type f -name \*.rkt)
#(cd biolink && raco test -m chk)

git mv biolink/string-search.rkt biolink/lib/
perl -p -i -e 's@string-search.rkt@lib/string-search.rkt@g;' $(find biolink -type f -name \*.rkt)
perl -p -i -e 's@(?<=[^-])lib/([a-z0-9_-]+).rkt@./$1/mk.rkt@g;' $(find biolink/lib -type f -name \*.rkt)
#(cd biolink && raco test -m chk)

git mv biolink/db.rkt biolink/lib/
perl -p -i -e 's@(?<=[^-])db.rkt@lib/db.rkt@g;' $(find biolink -type f -name \*.rkt)
perl -p -i -e 's@(?<=[^-])lib/([a-z0-9_-]+).rkt@./$1/mk.rkt@g;' $(find biolink/lib -type f -name \*.rkt)
#(cd biolink && raco test -m chk)

git mv biolink/mk.rkt biolink/lib/
perl -p -i -e 's@(?<=[^-])mk.rkt@lib/mk.rkt@g;' $(find biolink -type f -name \*.rkt)
perl -p -i -e 's@(?<=[^-])lib/([a-z0-9_-]+).rkt@./$1/mk.rkt@g;' $(find biolink/lib -type f -name \*.rkt)
#(cd biolink && raco test -m chk)

git mv biolink/mk-db.rkt biolink/lib/
perl -p -i -e 's@(?<=[^-])mk-db.rkt@lib/mk-db.rkt@g;' $(find biolink -type f -name \*.rkt)
perl -p -i -e 's@(?<=[^-])lib/([a-z0-9_-]+).rkt@./$1/mk.rkt@g;' $(find biolink/lib -type f -name \*.rkt)
#(cd biolink && raco test -m chk)

git mv biolink/common.rkt biolink/lib/
perl -p -i -e 's@(?<=[^-])common.rkt@lib/common.rkt@g;' $(find biolink -type f -name \*.rkt)
perl -p -i -e 's@(?<=[^-])lib/([a-z0-9_-]+).rkt@./$1/mk.rkt@g;' $(find biolink/lib -type f -name \*.rkt)
(cd biolink && raco test -m chk)
