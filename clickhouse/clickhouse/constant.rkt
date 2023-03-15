#lang racket
(require (for-syntax racket/base
                     racket/syntax))
(provide (all-defined-out))

(define-syntax (define-clickhouse-syntax-set stx)
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

(define-clickhouse-syntax-set clickhouse-operation
  '(create-table))

;; type kind

(define-clickhouse-syntax-set clickhouse-primitive-type
  '(String
    UInt8 UInt16 UInt32 UInt64 UInt128 UInt256
    Int8 Int16 Int32 Int64 Int128 Int256
    Float32 Float64
    Date DateTime DateTime64))

(define-clickhouse-syntax-set clickhouse-complex-type
  '(FixedString Array Tuple Enum8 Enum16 Nested Nullable Decimal))

;; engine kind

(define-clickhouse-syntax-set clickhouse-primitive-engine
  '(TinyLog Log StripeLog Memory Null Set))

(define-clickhouse-syntax-set clickhouse-complex-engine
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

(define-clickhouse-syntax-set clickhouse-infix-operator
  '(+ - * / % = != < > <= >=))

;; XXX: see https://clickhouse.yandex/docs/en/query_language/functions/

(define-clickhouse-syntax-set clickhouse-table-function
  '(count))

(define-clickhouse-syntax-set clickhouse-arithmetic-function
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

(define-clickhouse-syntax-set clickhouse-comparison-function
  '(equals =
           notEquals !=
           less <
           greater >
           lessOrEquals <=
           greaterOrEquals >=))

(define-clickhouse-syntax-set clickhouse-logical-function
  '(and or not xor))

(define-clickhouse-syntax-set clickhouse-type-conversion-function
  '(toUInt8 toUInt16 toUInt32 toUInt64
            toInt8 toInt16 toInt32 toInt64
            toFloat32 toFloat64
            toUInt8 toUInt16 toUInt32 toUInt64 toUInt128 toUInt256
            toInt8 toInt16 toInt32 toInt64 toInt128 toInt256
            toFloat32 toFloat64
            toUInt8OrZero toUInt16OrZero toUInt32OrZero toUInt64OrZero toUInt128OrZero toUInt256OrZero
            toInt8OrZero toInt16OrZero toInt32OrZero toInt64OrZero toInt128OrZero toInt256OrZero
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

(define-clickhouse-syntax-set clickhouse-date-time-function
  '(toYear toStartOfYear toRelativeYearNum
           toStartOfQuarter
           toMonth toDayOfMonth toStartOfMonth toRelativeMonthNum
           toDayOfWeek toRelativeWeekNum toMonday
           toStartOfDay toRelativeDayNum
           toStartOfHour toRelativeHourNum
           toMinute toStartOfMinute toStartOfFiveMinute toStartOfFifteenMinutes toRelativeMinuteNum
           toSecond toRelativeSecondNum
           toTime now today yesterday timeSlot timeSlots))

(define-clickhouse-syntax-set clickhouse-string-function
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

(define-clickhouse-syntax-set clickhouse-conditional-function
  '(if))

(define-clickhouse-syntax-set clickhouse-mathematical-function
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

(define-clickhouse-syntax-set clickhouse-array-function
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

(define-clickhouse-syntax-set clickhouse-tuple-function
  '(in notIn globalIn globalNotIn
       tuple tupleElement))

(define-clickhouse-syntax-set clickhouse-bit-function
  '(bitAnd
    bitOr
    bitXor
    bitNot
    bitShiftLeft
    bitShiftRight))

(define-clickhouse-syntax-set clickhouse-hash-function
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

(define-clickhouse-syntax-set clickhouse-entropy-function
  '(rand
    rand64))

(define-clickhouse-syntax-set clickhouse-encoding-function
  '(hex
    unhex
    UUIDStringToNum
    UUIDNumToString
    bitmaskToList
    bitmaskToArray))

(define-clickhouse-syntax-set clickhouse-url-function
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

(define-clickhouse-syntax-set clickhouse-ip-function
  '(IPv4NumToString
    IPv4StringToNum
    IPv4NumToStringClassC
    IPv6NumToString
    IPv6StringToNum))

(define-clickhouse-syntax-set clickhouse-json-function
  ;; We call JSON a JSON, but Yandex.Metrica thinks JSON is a «visit». Okay.
  ;; XXX: see https://clickhouse.yandex/docs/en/query_language/functions/json_functions/
  '(visitParamHas
    visitParamExtractUInt
    visitParamExtractInt
    visitParamExtractFloat
    visitParamExtractBool
    visitParamExtractRaw
    visitParamExtractString))

(define-clickhouse-syntax-set clickhouse-higher-order-function
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

(define-clickhouse-syntax-set clickhouse-util-function
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

(define-clickhouse-syntax-set clickhouse-dict-function
  '(dictGetUInt8 dictGetUInt16 dictGetUInt32 dictGetUInt64
                 dictGetInt8 dictGetInt16 dictGetInt32 dictGetInt64
                 dictGetFloat32 dictGetFloat64
                 dictGetDate dictGetDateTime
                 dictGetUUID
                 dictGetString
                 dictGetTOrDefault
                 dictIsIn
                 dictGetHierarchy
                 dictHas
                 regionToCity
                 regionToArea
                 regionToDistrict
                 regionToCountry
                 regionToContinent
                 regionToPopulation
                 regionIn
                 regionHierarchy
                 regionToName))

(define-clickhouse-syntax-set clickhouse-function
  (append
   clickhouse-table-functions
   clickhouse-arithmetic-functions
   clickhouse-comparison-functions
   clickhouse-logical-functions
   clickhouse-type-conversion-functions
   clickhouse-date-time-functions
   clickhouse-string-functions
   clickhouse-conditional-functions
   clickhouse-mathematical-functions
   clickhouse-array-functions
   clickhouse-tuple-functions
   clickhouse-bit-functions
   clickhouse-hash-functions
   clickhouse-entropy-functions
   clickhouse-encoding-functions
   clickhouse-url-functions
   clickhouse-ip-functions
   clickhouse-json-functions
   clickhouse-higher-order-functions
   clickhouse-util-functions
   clickhouse-dict-functions))

(define-clickhouse-syntax-set clickhouse-number
  '(UInt8 UInt16 UInt32 UInt64 UInt128 UInt256
          Int8 Int16 Int32 Int64 Int128 Int256
          Float32 Float64))

(define-clickhouse-syntax-set clickhouse-string
  '(String FixedString))

(module+ test
  (require rackunit)

  (define-clickhouse-syntax-set letter '(a b c))

  (check-equal? letters '(a b c))
  (check-equal? (letter? 'a) #t)
  (check-equal? (letter? 'b) #t)
  (check-equal? (letter? 'c) #t)
  (check-equal? (letter? 'd) #f)
  (check-equal? (letter? 'z) #f))
