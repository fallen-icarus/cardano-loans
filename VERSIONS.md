# Cardano-Loans Protocol Versions

This document provides the official specification for each version of the Cardano-Loans smart
contract protocol.

A new protocol version (e.g., v1, v2) is released **only when the on-chain smart contract code is
changed**. Each version corresponds to a unique, immutable set of smart contracts.

---

## Protocol v2 - *In Development*

#### Changes
- Merged interest/penalty applications with payments.
- Added early loan termination clause from too many missed payments.
- Added ability to accept unsolicited offers.
- Added ability to link offers to specific asks.
- Updated how collateral value calculation is handled.

-   **Status:** 🟡 **LIVE but not audited**
-   **Plutus Version:** PlutusV2
-   **Commit Hash:** [`0b45bf8c8cbf2c931901881dfeda0c5eefd796fe`](https://github.com/fallen-icarus/cardano-loans/commit/0b45bf8c8cbf2c931901881dfeda0c5eefd796fe)

---

## Protocol v1 - *Current*

This version includes the core lending protocol with secured and unsecured loans, tradable bonds,
multi-asset loans, and credit history tracking.

-   **Status:** ✅ **Live & Audited**
-   **Plutus Version:** PlutusV2
-   **Commit Hash:** [`1890f6f1ea74599615b0dea5829fffaed6ea9200`](https://github.com/fallen-icarus/cardano-loans/commit/1890f6f1ea74599615b0dea5829fffaed6ea9200)
-   **Audit Details:**
    -   **Auditor:** [Cypher Enterprises](https://github.com/cypher-enterprises)
    -   **Report:** [**View Full Report**](https://github.com/cypher-enterprises/p2p-audit/blob/main/audit.pdf)
