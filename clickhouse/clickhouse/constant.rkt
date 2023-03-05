#lang racket/base

(require (for-syntax racket/base
                     racket/syntax))

(provide (all-defined-out))

(define-syntax (define/lookup stx)
  (syntax-case stx ()
    ((_ name table)
     (with-syntax ((source (format-id stx "~as" #'name))
                   (lookup (format-id stx "~a?" #'name)))
       (syntax
        (begin
          (define source table)
          (define lookup
            (let* ((h (for/hash ((w (in-list source))) (values w #t))))
              (lambda (k) (hash-ref h k #f))))))))))

(define/lookup operation
  '(create-table))

;; type kind

(define/lookup primitive-type
  '(String
    UInt8 UInt16 UInt32 UInt64
    Int8 Int16 Int32 Int64
    Float32 Float64
    Date DateTime))

(define/lookup complex-type
  '(FixedString Array Tuple Enum8 Enum16 Nested Nullable))

;; engine kind

(define/lookup primitive-engine
  '(TinyLog Log StripeLog Memory Null Set))

(define/lookup complex-engine
  '(AggregatingMergeTree
    Buffer
    CollapsingMergeTree
    Distributed
    Join
    Merge
    MergeTree
    ReplacingMergeTree
    ReplicatedAggregatingMergeTree
    ReplicatedCollapsingMergeTree
    ReplicatedMergeTree
    ReplicatedReplacingMergeTree
    ReplicatedSummingMergeTree
    ReplicatedVersionedCollapsingMergeTree
    SummingMergeTree
    VersionedCollapsingMergeTree))

(define/lookup infix-operator
  '(+ - * / % = != < > <= >=))

;; XXX: see https://clickhouse.yandex/docs/en/query_language/functions/

(define/lookup table-function
  '(count))

(define/lookup arithmetic-function
  '(plus +
    minus -
    multiply *
    divide /
    intDiv
    intDivOrZero
    modulo %
    negate
    abs
    gcd
    lcm))

(define/lookup comparison-function
  '(equals =
    notEquals !=
    less <
    greater >
    lessOrEquals <=
    greaterOrEquals >=))

(define/lookup logical-function
  '(and or not xor))

(define/lookup type-conversion-function
  '(toUInt8 toUInt16 toUInt32 toUInt64
    toInt8 toInt16 toInt32 toInt64
    toFloat32 toFloat64
    toUInt8OrZero toUInt16OrZero toUInt32OrZero toUInt64OrZero
    toInt8OrZero toInt16OrZero toInt32OrZero toInt64OrZero
    toFloat32OrZero toFloat64OrZero
    toDate toDateTime
    toString
    toFixedString
    toStringCutToZero
    toYYYYMM toYYYYMMDD
    reinterpretAsUInt8 reinterpretAsUInt16 reinterpretAsUInt32 reinterpretAsUInt64
    reinterpretAsInt8 reinterpretAsInt16 reinterpretAsInt32 reinterpretAsInt64
    reinterpretAsFloat32 reinterpretAsFloat64
    reinterpretAsDate reinterpretAsDateTime
    reinterpretAsString
    cast))

(define/lookup date-time-function
  '(toYear toStartOfYear toRelativeYearNum
    toStartOfQuarter
    toMonth toDayOfMonth toStartOfMonth toRelativeMonthNum
    toDayOfWeek toRelativeWeekNum toMonday
    toStartOfDay toRelativeDayNum
    toStartOfHour toRelativeHourNum
    toMinute toStartOfMinute toStartOfFiveMinute toStartOfFifteenMinutes toRelativeMinuteNum
    toSecond toRelativeSecondNum
    toTime now today yesterday timeSlot timeSlots))

(define/lookup string-function
  '(empty
    notEmpty
    length
    lengthUTF8
    lower
    upper
    lowerUTF8
    upperUTF8
    reverse
    reverseUTF8
    concat
    substring
    substringUTF8
    appendTrailingCharIfAbsent
    convertCharset
    position
    positionUTF8
    match
    extract
    extractAll
    like
    notLike
    replaceOne
    replaceAll
    replaceRegexpOne
    replaceRegexpAll
    splitByChar
    splitByString
    arrayStringConcat
    alphaTokens))

(define/lookup conditional-function
  '(if))

(define/lookup mathematical-function
  '(e
    pi
    exp
    log
    exp2
    log2
    exp10
    log10
    sqrt
    cbrt
    erf
    erfc
    lgamma
    tgamma
    sin
    cos
    tan
    asin
    acos
    atan
    pow
    floor
    ceil
    round
    roundToExp2
    roundDuration
    roundAge))

(define/lookup array-function
  '(empty
    notEmpty
    length
    emptyArrayUInt8 emptyArrayUInt16 emptyArrayUInt32 emptyArrayUInt64
    emptyArrayInt8 emptyArrayInt16 emptyArrayInt32 emptyArrayInt64
    emptyArrayFloat32 emptyArrayFloat64
    emptyArrayDate emptyArrayDateTime
    emptyArrayString
    emptyArrayToSingle
    range
    array
    arrayConcat
    arrayElement
    has
    indexOf
    countEqual
    arrayEnumerate
    arrayEnumerateUniq
    arrayPopBack
    arrayPopFront
    arrayPushBack
    arrayPushFront
    arraySlice
    arrayUniq
    arrayJoin))

(define/lookup tuple-function
  '(in notIn globalIn globalNotIn
    tuple tupleElement))

(define/lookup bit-function
  '(bitAnd
    bitOr
    bitXor
    bitNot
    bitShiftLeft
    bitShiftRight))

(define/lookup hash-function
  '(halfMD5
    MD5
    sipHash64
    sipHash128
    cityHash64
    intHash32
    intHash64
    SHA1
    SHA224
    SHA256
    URLHash))

(define/lookup entropy-function
  '(rand
    rand64))

(define/lookup encoding-function
  '(hex
    unhex
    UUIDStringToNum
    UUIDNumToString
    bitmaskToList
    bitmaskToArray))

(define/lookup url-function
  '(protocol
    domain
    domainWithoutWWW
    topLevelDomain
    firstSignificantSubdomain
    cutToFirstSignificantSubdomain
    path
    pathFull
    queryString
    fragment
    queryStringAndFragment
    extractURLParameter
    extractURLParameters
    extractURLParameterNames
    URLHierarchy
    URLPathHierarchy
    decodeURLComponent
    cutWWW
    cutQueryString
    cutFragment
    cutQueryStringAndFragment
    cutURLParameter))

(define/lookup ip-function
  '(IPv4NumToString
    IPv4StringToNum
    IPv4NumToStringClassC
    IPv6NumToString
    IPv6StringToNum))

(define/lookup json-function
  ;; We call JSON a JSON, but Yandex.Metrica thinks JSON is a «visit». Okay.
  ;; XXX: see https://clickhouse.yandex/docs/en/query_language/functions/json_functions/
  '(visitParamHas
    visitParamExtractUInt
    visitParamExtractInt
    visitParamExtractFloat
    visitParamExtractBool
    visitParamExtractRaw
    visitParamExtractString))

(define/lookup higher-order-function
  '(lambda
    arrayMap
    arrayFilter
    arrayCount
    arrayExists
    arrayAll
    arraySum
    arrayFirst
    arrayFirstIndex
    arrayCumSum
    arraySort
    arrayReverseSort))

(define/lookup util-function
  '(hostName
    visibleWidth
    toTypeName
    blockSize
    materialize
    ignore
    sleep
    currentDatabase
    isFinite
    isInfinite
    isNaN
    hasColumnInTable
    bar
    transform
    formatReadableSize
    least
    greatest
    uptime
    version
    rowNumberInAllBlocks
    runningDifference
    MACNumToString
    MACStringToNum
    MACStringToOUI))

(define/lookup dict-function
  '(dictGetUInt8 dictGetUInt16 dictGetUInt32 dictGetUInt64
    dictGetInt8 dictGetInt16 dictGetInt32 dictGetInt64
    dictGetFloat32 dictGetFloat64
    dictGetDate dictGetDateTime
    dictGetUUID
    dictGetString
    dictGetTOrDefault
    dictIsIn
    dictGetHierarchy
    dictHas))

(define/lookup metrica-dict-function
  '(regionToCity
    regionToArea
    regionToDistrict
    regionToCountry
    regionToContinent
    regionToPopulation
    regionIn
    regionHierarchy
    regionToName))

(define/lookup function
  (append
   table-functions
   arithmetic-functions
   comparison-functions
   logical-functions
   type-conversion-functions
   date-time-functions
   string-functions
   conditional-functions
   mathematical-functions
   array-functions
   tuple-functions
   bit-functions
   hash-functions
   entropy-functions
   encoding-functions
   url-functions
   ip-functions
   json-functions
   higher-order-functions
   util-functions
   dict-functions
   metrica-dict-functions))

(module+ test
  (require rackunit)

  (define/lookup letter '(a b c))

  (check-equal? letters '(a b c))
  (check-equal? (letter? 'a) #t)
  (check-equal? (letter? 'b) #t)
  (check-equal? (letter? 'c) #t)
  (check-equal? (letter? 'd) #f)
  (check-equal? (letter? 'z) #f))