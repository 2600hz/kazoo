#!/bin/bash

# Generate Kazoo SDKs using the Swagger file

## Languages commented out either fail to generate or generate server code
## (we only want to generate client SDKs)
langs=''
# langs="$langs CsharpDotNet2"
langs="$langs akka-scala"
# langs="$langs aspnet5"
# langs="$langs aspnetcore"
langs="$langs async-scala"
langs="$langs bash"
langs="$langs clojure"
# langs="$langs cpprest"
# langs="$langs csharp"
langs="$langs cwiki"
# langs="$langs dart"
langs="$langs dynamic-html"
langs="$langs elixir"
# langs="$langs erlang-server"
langs="$langs finch"
langs="$langs flash"
langs="$langs go"
# langs="$langs go-server"
langs="$langs groovy"
langs="$langs haskell"
langs="$langs html"
langs="$langs html2"
langs="$langs inflector"
langs="$langs java"
langs="$langs java-play-framework"
# langs="$langs javascript"
# langs="$langs javascript-closure-angular"
langs="$langs jaxrs"
langs="$langs jaxrs-cxf"
langs="$langs jaxrs-cxf-cdi"
langs="$langs jaxrs-cxf-client"
langs="$langs jaxrs-resteasy"
langs="$langs jaxrs-resteasy-eap"
langs="$langs jaxrs-spec"
langs="$langs jmeter"
langs="$langs lumen"
langs="$langs msf4j"
# langs="$langs nancyfx"
# langs="$langs nodejs-server"
# langs="$langs objc"
langs="$langs perl"
langs="$langs php"
langs="$langs python"
langs="$langs python-flask"
# langs="$langs qt5cpp"
langs="$langs rails5"
langs="$langs ruby"
langs="$langs scala"
langs="$langs scalatra"
langs="$langs silex-PHP"
langs="$langs sinatra"
langs="$langs slim"
langs="$langs spring"
# langs="$langs swagger"
# langs="$langs swagger-yaml"
# langs="$langs swift"
langs="$langs swift3"
langs="$langs tizen"
langs="$langs typescript-angular"
# langs="$langs typescript-angular2"
langs="$langs typescript-fetch"
langs="$langs typescript-jquery"
langs="$langs typescript-node"
langs="$langs undertow"
langs="$langs ze-ph"

img=swaggerapi/swagger-codegen-cli
docker pull $img >/dev/null || exit 1
mkdir out out/_logs

for lang in $langs; do
    log=out/_logs/$lang.log
    echo Generating SDK in $lang
    docker run --rm \
           -v ${PWD}/out:/out \
           $img generate \
           -i https://raw.githubusercontent.com/2600hz/kazoo/master/applications/crossbar/priv/api/swagger.json \
           -l $lang \
           -o /out/$lang \
           >$log 2>&1
    err=$?
    if [[ $err -ne 0 ]]; then
        echo An error occured!
        echo
        cat $log
        exit $err
    fi
done
