# Pantry Monitoring System — Domain Context

## Glossary

### Activity
A period of pantry use: one or more door events that occur within 1.5 minutes of each other, grouped into a single unit. Not equivalent to one person's visit — two people in close succession collapse into one activity. The pantries are physically small enough that truly simultaneous use by two people is extremely uncommon.

**Y_ik** in the mixed-effects model: count of activities for pantry `i` on day `k`.

### Door Event
A paired open/close of one door sensor. Each pantry has two independent, side-by-side access doors (door1, door2). Either door can be used independently.

### Area
A physical section of the pantry, each measured by one weight scale (scale1–scale4). The four areas are not semantically distinct — people place items anywhere. The split into 4 scales is a hardware constraint (easier to instrument 4 smaller areas than one large scale). Weight changes are always summed across all 4 areas.

### Donation
An activity where `total_weight_change > 0.3 lbs` (net weight gained — someone left items).

### Consumption
An activity where `total_weight_change < -0.3 lbs` (net weight lost — someone took items).

### No Change
An activity where `|total_weight_change| ≤ 0.3 lbs`. Includes:
- Someone who opened the pantry and took nothing
- Readings within sensor noise (±0.3 lbs threshold)

### Noise Threshold
0.3 lbs. Weight changes within ±0.3 lbs are treated as sensor noise and classified as No Change, not Donation or Consumption.

### Negative Scale Readings
A negative reading on any scale means the scale is empty — it is sensor drift below zero, not a physically meaningful value. **Any negative scale reading must be treated as 0.** This is applied in `activities_with_weights()` before weight deltas are computed.

---

## Key Thresholds

| Threshold | Value | Basis |
|-----------|-------|-------|
| Max door event duration | 2.5 min | Derived from observed data |
| Activity gap (merge window) | 1.5 min | Heuristic — time for one person to leave and another to arrive |
| Noise floor | 0.3 lbs | Domain judgment |

---

## Model

The analysis feeds a **mixed-effects model** (Ethan's model):

```
Y_ik ~ fixed effects (day covariates, pantry covariates, intervention flags)
     + random intercept per pantry
     + optional random slope per pantry
```

Current pipeline output: daily activity count per pantry (`Y_ik`).  
Future pipeline output: weight change information per activity (for extended analysis).

---

## Monitored Pantries

- Greenwood
- Beacon Hill
- St. Paul Church Pantry
- Haller Lake Pantry

---

## Known Issues

### Beacon Hill — Scale 1 Malfunction
Scale 1 readings at Beacon Hill are zeroed between 2026-01-29 16:26:30 and 2026-02-24 12:00:00 due to a known sensor malfunction. This is a one-time fix hardcoded in `SensorData_oo.R`. If future sensor malfunctions occur at other pantries or dates, a similar hardcoded correction block will need to be added.

### Weight Reading at Synthetic Event Midpoints (Unresolved)
When a long door event is split into two synthetic events (_A and _B), the "before" weight for event _B is fetched at a synthetic midpoint timestamp — a point in time during what was originally a single open period. The scale reading there may be mid-flux rather than a stable resting state, making the weight delta for _B unreliable.

Candidate fix under consideration: fetch the nearest available weight reading to the synthetic open timestamp, rather than the reading strictly before it. A better alternative may be to chain the readings — using _A's "after" reading as _B's "before" reading — so the two events share a clean handoff and together account for the full weight change of the original event.

---

## Code Notes

- `SensorData_Giacomo.R` — a collaborator's implementation. Do not modify.
- `report` table returned by `door_events()` — a debugging artifact showing event correction statistics. Not used in production pipeline.
- `fuzzyjoin` is imported but unused — legacy from earlier development.
