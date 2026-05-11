# Data Pipeline — Pantry Monitoring System

## Overview

This pipeline takes raw IoT sensor logs from community food pantries and produces a structured record of pantry activities with associated weight changes. The final output feeds a mixed-effects model that studies pantry usage patterns.

---

## Physical Setup

Each pantry is equipped with:
- **2 door sensors** (door1, door2) — two independent, side-by-side access doors. Either can be used on its own.
- **4 weight scales** (scale1–scale4) — one per physical shelf area. The split is a hardware constraint, not a semantic one. Weight changes are always summed across all 4 scales.

Four pantries are currently monitored: Greenwood, Beacon Hill, St. Paul Church Pantry, and Haller Lake.

---

## Data Source

All sensor readings are stored in an Azure SQL Server database (`PantryLogs` table). Each row is a timestamped reading containing:

| Column | Description |
|--------|-------------|
| `timestamp` | UTC timestamp of the reading |
| `device_id` | Pantry identifier |
| `door1_open` | Boolean — is door 1 open? |
| `door2_open` | Boolean — is door 2 open? |
| `scale1`–`scale4` | Weight in lbs for each shelf area |
| `air_temp` | Ambient temperature |
| `batt_percent` | Sensor battery level |

Data is fetched via `pantrydb()` in `SensorData_oo.R`, which also saves a timestamped CSV backup to `../data/`.

---

## Pipeline Steps

### Step 1 — Fetch and Clean Raw Data

**File:** `SensorData_oo.R` → `pantrydb()`

- Connects to Azure SQL and retrieves all rows from `PantryLogs`.
- Timestamps are converted from UTC to America/Los_Angeles.

---

### Step 2 — Extract and Repair Door Events

**File:** `event_estimate.R` → `door_events()`, `fix_long_door_events()`

The sensor stream is a sequence of open/close state changes. This step pairs each opening with its nearest closing to produce a **door event**: an (openTS, closeTS) pair representing one door interaction.

**Threshold:** A normal door event lasts at most **2.5 minutes** (`max_duration_mins`). Any event exceeding this duration is considered anomalous — most likely a missed close signal — and is repaired.

The repair logic applies one of four cases, determined by two conditions:
- Is the event duration **longer than 2× the threshold** (i.e., > 5 minutes)?
- Is there a **candidate close time** available from the other door?

---

#### Case 1 — Duration > 2× threshold, candidate available

The candidate (the other door's close time) becomes the close of a new synthetic event _A. A second synthetic event _B is created to cover the tail: it opens `max_duration` before the original close and closes at the original close.

```
Original:   [open ────────────────────────────── close]
Repaired:   [open ──── A_close]  [B_open ──── close]
                       ↑ candidate time
```

---

#### Case 2 — Duration > 2× threshold, no candidate available

Two synthetic events are created using only the threshold as an anchor. Event _A closes at `open + max_duration`. Event _B opens at `close - max_duration` and closes at the original close.

```
Original:   [open ────────────────────────────── close]
Repaired:   [open ── A_close]      [B_open ── close]
                     ↑ open + 2.5min  ↑ close - 2.5min
```

---

#### Case 3 — Duration ≤ 2× threshold, candidate available

The original closing timestamp is replaced with the candidate time. The original close is discarded. A single corrected event replaces the original.

```
Original:   [open ──────────── close]
Repaired:   [open ── close]
                     ↑ candidate time
```

---

#### Case 4 — Duration ≤ 2× threshold, no candidate available

The original closing timestamp is replaced with a synthetic close at `open + max_duration`. A single corrected event replaces the original.

```
Original:   [open ──────────── close]
Repaired:   [open ── close]
                     ↑ open + 2.5min
```

---

**Validation:** After any repair, synthetic events are rejected (marked `ambiguous`) if:
- The synthetic `openTS ≥ closeTS`, or
- The synthetic close overlaps with the next real door opening.

Ambiguous events are kept in their original form and flagged.

Each event carries a `source` field: `unchanged`, `replaced_closing`, `split_partA`, or `split_partB`.

This step runs independently for door1 and door2.

---

### Step 3 — Cluster into Activities

**File:** `event_estimate.R` → `pantry_activities()`

Door events from both doors are merged into a single timeline and grouped into **activities**.

An **activity** is a period of pantry use. Events from either door are merged into one activity when the gap between consecutive events is **≤ 1.5 minutes**. A gap larger than 1.5 minutes starts a new activity.

> **1.5 minutes** is a heuristic — the estimated minimum time for one person to leave and a new person to arrive and open the pantry again.

Each activity record contains:
- `activity_start` / `activity_end` — wall-clock timestamps
- `activity_duration` — total duration in seconds
- `event_count` — number of door events grouped into the activity
- `door_types` — which doors were used

---

### Step 4 — Attach Weight Changes

**File:** `event_estimate.R` → `activities_with_weights()`

For each activity, the pipeline retrieves two weight snapshots from the raw sensor log:
- **Before** — the most recent reading at or before `activity_start`
- **After** — the next reading at or after `activity_end`

The weight change per scale is:

```
delta_scaleN = after_scaleN - before_scaleN
```

Summed across all four scales:

```
total_weight_change = delta_scale1 + delta_scale2 + delta_scale3 + delta_scale4
```

> **Negative scale readings are clamped to 0** at this step, before deltas are computed. A negative reading means the scale is empty (sensor drift below zero) and carries no physical meaning.

---

### Step 5 — Classify Activities

**File:** `figures.R` (lines 344–354)

Each activity is classified based on its `total_weight_change`:

| Type | Condition | Meaning |
|------|-----------|---------|
| **Donation** | `total_weight_change > 0.3 lbs` | Someone left items |
| **Consumption** | `total_weight_change < −0.3 lbs` | Someone took items |
| **No Change** | `\|total_weight_change\| ≤ 0.3 lbs` | Looked but took/left nothing, or within sensor noise |

> **±0.3 lbs** is the noise threshold. Changes smaller than this are indistinguishable from sensor drift.

---

### Step 6 — Aggregate and Visualize

**File:** `figures.R`

The pipeline iterates over all 4 pantries and produces 13 charts covering:
- Total and per-day activity counts
- Day-of-week and time-of-day patterns
- Activity type distribution (Donation / Consumption / No Change)
- Average weight change per activity type

These charts are exploratory — for presenting to colleagues and validating the pipeline before modeling.

---

## Model Output

The ultimate output of the pipeline is:

```
Y_ik = count of activities for pantry i on day k
```

This daily activity count per pantry feeds the mixed-effects model. Weight change data per activity will be incorporated in a later phase of the analysis.

---

## Key Files

| File | Role |
|------|------|
| `SensorData_oo.R` | Database connection and raw data fetch |
| `event_estimate.R` | Door event extraction, repair, clustering, and weight calculation |
| `figures.R` | Activity classification and visualization |
| `main.R` | Entry point — runs the full pipeline for one pantry |
| `CONTEXT.md` | Domain glossary, thresholds, and known issues |

---

## Known Issues

### Beacon Hill — Scale 1 Malfunction
Scale 1 at Beacon Hill returned incorrect readings between 2026-01-29 16:26:30 and 2026-02-24 12:00:00 due to a hardware fault. Those readings are hardcoded to 0 in `event_estimate.R`. If future sensor faults occur at other pantries or dates, a similar correction block must be added manually in the same file.
