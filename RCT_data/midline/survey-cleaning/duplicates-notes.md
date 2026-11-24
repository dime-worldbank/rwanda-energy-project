# Daily Duplicates
## Definition of Duplicates: hhid as character variable (science notation can hinder exact matching), more than one unique hhid in survey entries before applying any filter (consent, locate, finish)
## Action: Inspect the aggregated list everyday to ensure there is no missing duplicates
**Total Duplicates:** 46 (23 hhid)

---

## 2026-10-31 (6 hhid)

### (1) `321206010353` — YES
| KEY | Action |
|---|---|
| `uuid:4f343931-4265-424f-90b1-d9a9bf44ce0d` | Keep as it is |
| `uuid:1467ded2-fbc2-4b27-90a1-98fc18f9b16e` | Mutate `hh_id` → `321206010533` |

### (2) `321206020017` — YES
| KEY | Action |
|---|---|
| `uuid:ab392f2e-7bdc-47cf-878f-c0fe8590a457` | Correct |
| `uuid:83283964-694b-400d-8194-ac886facd55f` | Mutate `hh_id` → `321206020163` |

### (3) `360203040491` — YES
| KEY | Action |
|---|---|
| `uuid:91fe9ac9-b06d-49a4-a5fa-995b79ed7099` | Correct |
| `uuid:b82a8838-cde6-488e-a39d-0bea009069b8` | Mutate `hh_id` → `360203040053` |

### (4) `411705060284` — YES
| KEY | Action |
|---|---|
| `uuid:b44581db-3a98-4b0b-b79b-d4a35c2c03d0` | Drop the survey entries |
| `uuid:17d7f871-aede-4757-b345-5c6dd33b64fa` | Correct |

### (5) `411705060341` — YES
| KEY | Action |
|---|---|
| `uuid:db51c69d-82b0-4557-8d63-3c0c515f2db6` | Drop the survey |
| `uuid:547d2545-518e-4f67-a948-f5abe1d12c94` | Correct |

### (6) `411705060408` — YES
> **Note on this decision:** Following Marthe's note creates another duplicate. Upon inspecting this 408 duplicate, the household_head name preload matches with Key ending with 845. Also these two entries (from their GPS key) seem like the same household, by the same enumerator.

| KEY | Action | Marthe's note |
|---|---|---|
| `uuid:bb00dd51-de83-47cc-8f53-3786e8852776` | Drop the survey | Correct |
| `uuid:6f425e0f-49f2-4549-a459-2a586b4ce845` | Correct | Mutate `hh_id` → `411705060161` |

---

## 2026-11-01 (1 hhid)

### (1) `360301021051` — YES
| KEY | Action |
|---|---|
| `uuid:733f2009-c455-4a10-b2f7-5019144ab62d` | Correct (manually flagged – FOUND) |
| `uuid:0f48246d-9264-4e9e-933e-469f833e8c92` | Mutate `hh_id` → `360301020151` |

---

## 2026-11-02 (2 hhid)

### (1) `360204080238` — YES
| KEY | Action |
|---|---|
| `uuid:8cc3f58b-8c43-4344-92eb-d6f7af9b0adc` | Correct |
| `uuid:e95348dc-c0bf-4ecb-b19c-8cd662d05f43` | Mutate `hh_id` → `360204080283` |

### (2) `360204060090` — MISSED by HFC
> One was locate, one did not locate.

| KEY | Action |
|---|---|
| `uuid:0af03dc8-71e4-4055-94d3-88c98047f013` | Correct |
| `uuid:4de1e4d3-f620-4cc6-8910-f93c77485e74` | Drop |

---

## 2026-11-03 (6 hhid)

### (1) `360204070204` — YES
| KEY | Action |
|---|---|
| `uuid:a8f8c9e7-3925-4995-897d-5a3decc4b9f0` | Correct |
| `uuid:629621ad-9c6f-4593-9727-a54069137e9d` | Mutate `hh_id` → `360204070024` |

### (2) `360204070350` — YES
| KEY | Action |
|---|---|
| `uuid:2b9aa1fa-c698-44d4-8639-3a4a1b24460d` | Correct |
| `uuid:da7c7ac2-ab8d-4240-9032-abf68f72e921` | Mutate `hh_id` → `360204070530` |

### (3) `360204070473` — YES
| KEY | Action |
|---|---|
| `uuid:274e5809-9ef3-45f1-8cda-b4a699ca4535` | Correct |
| `uuid:0405a778-9f99-4ec4-a58b-e9f59b7850a2` | Mutate `hh_id` → `360204070743` (manually flagged – FOUND) |

### (4) `410602050637` — YES
| KEY | Action |
|---|---|
| `uuid:381cc9d6-f5e0-4010-baf3-569f7cbe6802` | Correct |
| `uuid:e508fd01-fabb-40f7-b0dc-3288d3484978` | Mutate `hh_id` → `410602050479` |

### (5) `360204070518`
| KEY | Action |
|---|---|
| `uuid:df27c890-7d13-430a-8274-8d4f8fa82f5e` | Correct |
| `uuid:62c7b83f-eb4b-41a5-a6ee-fe156c0cabbb` | Drop |

### (6) `321303030263`
| KEY | Action |
|---|---|
| `uuid:d6eb60c2-9376-4f91-900a-d71e7dc3dc94` | Correct |
| `uuid:16da0d6d-0f66-4fd8-9bb1-9737183c79e9` | Drop |

---

## 2026-11-04 (1 hhid)

### (1) `321304020151` — YES
| KEY | Action |
|---|---|
| `uuid:f176ba33-56b4-41bd-9cf7-38eea395a915` | Correct |
| `uuid:61ff7071-990a-4839-b82a-b164bfb664b2` | Mutate `hh_id` → `321304020511` |

---

## 2026-11-05 (2 hhid)

### (1) `320304020262`
> Both are nolocate and this will be reinterviewed — was included in the mop-up list.

| KEY | Action |
|---|---|
| `uuid:b6a662c6-6961-401f-989a-8d91e2730477` | Drop |
| `uuid:839925ec-c03c-4f83-8ff4-3bcf64359e9e` | Correct |

### (2) `320304100182` — YES
| KEY | Action |
|---|---|
| `uuid:73c28c5c-f2bc-4df6-9e85-0c971607c3b6` | Correct |
| `uuid:610e957a-8efd-4bdf-ad5d-9281b81ff205` | Mutate `hh_id` → `320304100081` |

---

## 2026-11-07 (3 hhid)

### (1) `310407050235`
| KEY | Action |
|---|---|
| `uuid:600a8263-5904-4ae2-90b0-38f03b45b6d7` | Correct |
| `uuid:aefbe428-265b-4825-aa85-77e167d63363` | Drop |

### (2) `320701030185`
| KEY | Action |
|---|---|
| `uuid:3d789a74-94e0-4b85-a3ae-131af1c91309` | Correct |
| `uuid:597ca638-c962-4b9c-9e00-48988718c5b0` | Drop |

### (3) `321101050339`
> Both are nolocate, so included in mop-up list.

| KEY | Action |
|---|---|
| `uuid:e0721da6-efa5-4401-811e-b8d94966b0ce` | Keep |
| `uuid:51afb2ff-4add-4c9e-82b9-6c0247421f44` | Drop |

---

## 2026-11-09 (2 hhid)

### (1) `411404010544` — YES
| KEY | Action |
|---|---|
| `uuid:417d291c-21e8-47f9-8ef6-83c2227539de` | Correct |
| `uuid:8cd08e0d-3b02-4cd9-82bf-f4ba65fa1f07` | Mutate `hh_id` → `411404010623` (manually flagged – FOUND) |

### (2) `411404030120` — YES
| KEY | Action |
|---|---|
| `uuid:accf3ef2-2f2f-4a7f-9aa2-1acfc1aaec33` | Correct |
| `uuid:233802cc-e99a-4dde-9a75-5d34b6be0155` | Mutate `hh_id` → `411404031020` |
