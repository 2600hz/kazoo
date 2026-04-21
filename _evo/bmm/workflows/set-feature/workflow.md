---
name: set-feature
description: 'Set the active feature context for artifact organization. Creates subfolders in planning-artifacts and implementation-artifacts. Use when the user says "set feature", "switch feature", "new feature", or "what feature is active"'
---

# Set Feature Workflow

**Goal:** Set or switch the `active_feature` context so all subsequent planning and implementation artifacts are organized into a named subfolder.

---

## INITIALIZATION

Load config from `{project-root}/_evo/bmm/config.yaml` and resolve:

- `project_name`, `user_name`, `communication_language`
- `planning_artifacts`, `implementation_artifacts`
- `active_feature` (current value, may be empty)
- YOU MUST ALWAYS SPEAK OUTPUT in your Agent communication style with the config `{communication_language}`

---

## EXECUTION

### 1. Show Current State

Display the current active feature:

- If `active_feature` is set: "Active feature: **{{active_feature}}**"
- If empty: "No active feature set."

### 2. Ask for Feature Slug

Ask the user:

"What is the feature name? Use a short kebab-case slug (e.g. `admin-panel`, `auth-refactor`, `dashboard-financeiro`). This will be used as the subfolder name inside `planning-artifacts/` and `implementation-artifacts/`."

Wait for user input.

### 3. Validate Input

- Must be non-empty
- Suggest converting spaces to hyphens and lowercasing if needed
- Confirm the final slug with the user before proceeding

### 4. Update Config

Edit `{project-root}/_evo/bmm/config.yaml`:

- Find the `active_feature:` key
- Update its value to the confirmed slug
- Save the file

### 5. Create Subfolders

Create the following directories if they do not already exist:

- `{planning_artifacts}/{{active_feature}}/`
- `{implementation_artifacts}/{{active_feature}}/`

### 6. Confirm

Inform the user:

"Feature set to **{{active_feature}}**.

All artifacts will now be saved to:
- `{planning_artifacts}/{{active_feature}}/`
- `{implementation_artifacts}/{{active_feature}}/`

You can switch features anytime by running `/evo-bmm-set-feature`."

---

## SUCCESS METRICS

✅ `active_feature` updated in config.yaml
✅ Subfolders created in planning-artifacts and implementation-artifacts
✅ User clearly informed of the new active feature path
