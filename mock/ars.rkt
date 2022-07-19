#lang racket/base

(require
  json
  "../common.rkt"
  "../config.rkt")

(provide
  post-query
  pull-query-status
  pull-query-result)

; Format:
;   qid '(<path to result> ...)
(define mock-queries
  (hash "3" '("test/ars/3-1.json" "test/ars/3-2.json" "test/ars/3-3.json")
        "e01" '("test/ars/e01.out-response.ara-explanatory.1dcc4dae-6b65-419b-8a75-ad2ff70776af.nres-72")
        "e02" '("test/ars/e02.out-response.ara-aragorn.885f2ccc-1a91-4b4b-93f4-d8c121551a32.nres-1" "test/ars/e02.out-response.ara-bte.684ab799-0a41-4a23-a91a-1451dca2d7da.nres-1" "test/ars/e02.out-response.ara-explanatory.4e727cc3-462d-442c-8e3f-6e53977cfdfd.nres-625")
        "e03" '("test/ars/e03.out-response.ara-aragorn.4268b777-0391-4dd6-b332-9040bf6ef2f4.nres-8" "test/ars/e03.out-response.ara-bte.60964e97-99d9-4064-b236-6322c22a73c1.nres-8" "test/ars/e03.out-response.ara-explanatory.7ddff266-4546-4782-842d-e3995fed1cdb.nres-1177")
        "e04" '("test/ars/e04.out-response.ara-aragorn.222e3298-07bf-4182-91e0-5339fed1b9ea.nres-65" "test/ars/e04.out-response.ara-bte.aba7b80d-8339-4b54-9a51-a1a7d2ee8057.nres-48" "test/ars/e04.out-response.ara-explanatory.d3b8b1bb-0200-468d-9f67-3a6cb5ab3a0f.nres-2526" "test/ars/e04.out-response.kp-openpredict.b49d7ac7-cf0e-46ff-bc7b-73b0e247d749.nres-505")
        "e05" '("test/ars/e05.out-response.ara-aragorn.c7550aa7-1ac8-4827-927f-aa2c19a19bf3.nres-86" "test/ars/e05.out-response.ara-bte.6914b4e2-634c-4bd8-8596-c7f0d2d39977.nres-86" "test/ars/e05.out-response.ara-explanatory.1375c6af-39a5-4b5d-b179-eb321996f94c.nres-8491")
        "e06" '("test/ars/e06.out-response.ara-arax.4edc6763-146f-40c5-a1ca-9db6f8251c4a.nres-685" "test/ars/e06.out-response.ara-bte.66f20d16-8f25-4207-a8b3-f5b31903e9b1.nres-18" "test/ars/e06.out-response.ara-explanatory.c4dda4ee-28ad-4087-be7d-714163d363a2.nres-816")
        "e07" '("test/ars/e07.out-response.ara-aragorn.1def5aee-822e-4d95-8936-d8c35e6cf485.nres-2" "test/ars/e07.out-response.ara-bte.2d5c5812-074e-4997-b77b-ca6e3b3b7d35.nres-2" "test/ars/e07.out-response.ara-explanatory.7c273ef3-d788-4c9c-9261-76eb85034e33.nres-1151")
        "e08" '("test/ars/e08.out-response.ara-aragorn.6698b782-0367-458a-9b5e-d3b964c0e329.nres-1" "test/ars/e08.out-response.ara-bte.33d0fe67-8f18-4557-9ab0-f3d7338c8817.nres-1" "test/ars/e08.out-response.ara-explanatory.cf8ae0e2-14c1-4f7b-8abc-2a9736aca8d7.nres-2025")
        "e09" '("test/ars/e09.out-response.ara-aragorn.8853e589-bf46-4c83-8f6d-41b1b403ba79.nres-7" "test/ars/e09.out-response.ara-bte.b5f3e6bf-c65b-4648-876f-cf10541e3e03.nres-6" "test/ars/e09.out-response.ara-explanatory.ec256b05-f5b0-48c5-b6dd-acf20fe5b51a.nres-2229")
        "e10" '("test/ars/e10.out-response.ara-aragorn.22efd26a-bc98-4e40-87ee-555aca3fe6ce.nres-2" "test/ars/e10.out-response.ara-bte.d647d78a-79cf-4f0d-940c-89d5a4f2eebb.nres-2" "test/ars/e10.out-response.ara-explanatory.30707daa-9495-4562-81e2-6ab7b82d14f7.nres-508")
        "e11" '("test/ars/e11.out-response.ara-aragorn.5ebb8c5a-59d3-42ee-9637-79107e2a8bce.nres-2" "test/ars/e11.out-response.ara-bte.b53de4e2-e863-450a-b955-a3f1c3a37bc6.nres-2" "test/ars/e11.out-response.ara-explanatory.2a28e927-3336-45b7-b692-02da96cb6203.nres-713")
        "e12" '("test/ars/e12.out-response.ara-explanatory.415a6c41-d815-4d61-a6c6-52b87db4e6a2.nres-3409")
        "e13" '("test/ars/e13.out-response.ara-aragorn.b08adcc1-3482-41c7-8af7-aa9eb828628c.nres-3" "test/ars/e13.out-response.ara-bte.d73edecd-b0cc-40b8-a744-8db726518a81.nres-3" "test/ars/e13.out-response.ara-explanatory.4c58be07-f9a1-44f4-afdd-6d466913859b.nres-3167")
        "e14" '("test/ars/e14.out-response.ara-aragorn.2a928d5b-9594-4ed1-b476-1c8d5a5f5230.nres-15" "test/ars/e14.out-response.ara-bte.a6d43ebe-c70c-4b71-9233-cf8b012a23b4.nres-15" "test/ars/e14.out-response.ara-explanatory.b1d6bbb5-7e2f-4363-a4ad-6015cc0f814c.nres-2943")
        "e15" '("test/ars/e15.out-response.ara-aragorn.b9bfa85a-56ba-4f0c-819b-abea2e2402ca.nres-3" "test/ars/e15.out-response.ara-bte.801fdf1c-ad74-46c5-ae39-3e182fdaab14.nres-3" "test/ars/e15.out-response.ara-explanatory.7af4092d-246e-4920-9e9f-d083a6a3c9a8.nres-2199")
        "e16" '("test/ars/e16.out-response.ara-explanatory.120964b6-0036-4873-8ae4-c3efab42c040.nres-2197")
        "e17" '("test/ars/e17.out-response.ara-arax.79b42b2c-5423-48e5-87c2-47808aefa763.nres-1000" "test/ars/e17.out-response.ara-explanatory.c6605b12-fc40-4abe-b893-741f51973141.nres-1867")
        "e18" '("test/ars/e18.out-response.ara-arax.86cb6f6e-6e4f-4cec-bae0-19d69ed75eb6.nres-88" "test/ars/e18.out-response.ara-explanatory.58f8adf9-6097-4128-92dd-b096dde53eee.nres-96")
        "e19" '("test/ars/e19.out-response.ara-explanatory.029df5b8-ee15-4319-9ae3-ec1013d44314.nres-135")
  ))

(define (post-query query)
  (cons 'done query))

(define (pull-query-status qid)
  (if (hash-has-key? mock-queries qid)
      'done
      'error))
(define (pull-query-result qid)
  (define result (hash-ref mock-queries qid))
  (map (lambda (path)
         (call-with-input-file (string-add-prefix (config-document-root SERVER-CONFIG) path)
           (lambda (input-port)
              (make-answer (jsexpr-object-ref (read-json input-port) 'message) "mock"))))
       result))
