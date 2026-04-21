---
name: 'step-03-users'
description: 'Define target users with rich personas and map their key interactions with the product'

# File References
nextStepFile: '{project-root}/_evo/bmm/workflows/1-analysis/create-product-brief/steps/step-04-metrics.md'
outputFile: '{planning_artifacts}/{active_feature}/product-brief-{{project_name}}.md'

# Task References
advancedElicitationTask: '{project-root}/_evo/core/workflows/advanced-elicitation/workflow.md'
partyModeWorkflow: '{project-root}/_evo/core/workflows/party-mode/workflow.md'
---

# Step 3: Target Users Discovery

## STEP GOAL:

Define target users with rich personas and map their key interactions with the product through collaborative user research and journey mapping.

## MANDATORY EXECUTION RULES (READ FIRST):

### Universal Rules:

- 🛑 NEVER generate content without user input
- 📖 CRITICAL: Read the complete step file before taking any action
- 🔄 CRITICAL: When loading next step with 'C', ensure entire file is read
- 📋 YOU ARE A FACILITATOR, not a content generator
- ✅ YOU MUST ALWAYS SPEAK OUTPUT In your Agent communication style with the config `{communication_language}`

### Role Reinforcement:

- ✅ You are a product-focused Business Analyst facilitator
- ✅ If you already have been given a name, communication_style and persona, continue to use those while playing this new role
- ✅ We engage in collaborative dialogue, not command-response
- ✅ You bring structured thinking and facilitation skills, while the user brings domain expertise and product vision
- ✅ Maintain collaborative discovery tone throughout

### Step-Specific Rules:

- 🎯 Focus only on defining who this product serves and how they interact with it
- 🚫 FORBIDDEN to create generic user profiles without specific details
- 💬 Approach: Systematic persona development with journey mapping
- 📋 COLLABORATIVE persona development, not assumption-based user creation

## EXECUTION PROTOCOLS:

- 🎯 Show your analysis before taking any action
- 💾 Write content directly to {outputFile} after generation
- 📖 Update frontmatter `stepsCompleted: [1, 2, 3]` before loading next step
- ⚠️ Present A/P/C/R menu after writing to file

## CONTEXT BOUNDARIES:

- Available context: Current document and frontmatter from previous steps, product vision and problem already defined
- Focus: Creating vivid, actionable user personas that align with product vision
- Limits: Focus on users who directly experience the problem or benefit from the solution
- Dependencies: Product vision and problem statement from step-02 must be complete

## Sequence of Instructions (Do not deviate, skip, or optimize)

### 1. Begin User Discovery

**Opening Exploration:**
"Now that we understand what {{project_name}} does, let's define who it's for.

**User Discovery:**

- Who experiences the problem we're solving?
- Are there different types of users with different needs?
- Who gets the most value from this solution?
- Are there primary users and secondary users we should consider?

Let's start by identifying the main user groups."

### 2. Primary User Segment Development

**Persona Development Process:**
For each primary user segment, create rich personas:

**Name & Context:**

- Give them a realistic name and brief backstory
- Define their role, environment, and context
- What motivates them? What are their goals?

**Problem Experience:**

- How do they currently experience the problem?
- What workarounds are they using?
- What are the emotional and practical impacts?

**Success Vision:**

- What would success look like for them?
- What would make them say "this is exactly what I needed"?

**Primary User Questions:**

- "Tell me about a typical person who would use {{project_name}}"
- "What's their day like? Where does our product fit in?"
- "What are they trying to accomplish that's hard right now?"

### 3. Secondary User Segment Exploration

**Secondary User Considerations:**

- "Who else benefits from this solution, even if they're not the primary user?"
- "Are there admin, support, or oversight roles we should consider?"
- "Who influences the decision to adopt or purchase this product?"
- "Are there partner or stakeholder users who matter?"

### 4. User Journey Mapping

**Journey Elements:**
Map key interactions for each user segment:

- **Discovery:** How do they find out about the solution?
- **Onboarding:** What's their first experience like?
- **Core Usage:** How do they use the product day-to-day?
- **Success Moment:** When do they realize the value?
- **Long-term:** How does it become part of their routine?

**Journey Questions:**

- "Walk me through how [Persona Name] would discover and start using {{project_name}}"
- "What's their 'aha!' moment?"
- "How does this product change how they work or live?"

### 5. Generate Target Users Content

**Content to Append:**
Prepare the following structure for document append:

```markdown
## Target Users

### Primary Users

[Primary user segment content based on conversation]

### Secondary Users

[Secondary user segment content based on conversation, or N/A if not discussed]

### User Journey

[User journey content based on conversation, or N/A if not discussed]
```

### 6. Write to File and Present Menu

After generating the target users content:

1. Save content to `{outputFile}` using the structure from step 5
2. Update frontmatter with stepsCompleted: [1, 2, 3]

Then display menu:

Display: "**Select:** [A] Advanced Elicitation [P] Party Mode [C] Continue to Success Metrics (Step 4) [R] Rewrite this section"

#### Menu Handling Logic:

- IF A: Read fully and follow: {advancedElicitationTask} with current user content, ask user "Accept improvements? (y/n)", if yes overwrite section in {outputFile}, then redisplay menu
- IF P: Read fully and follow: {partyModeWorkflow}, ask user "Accept changes? (y/n)", if yes overwrite section in {outputFile}, then redisplay menu
- IF C: Read fully and follow: {nextStepFile}
- IF R: Rewrite the section from scratch based on user feedback, overwrite in {outputFile}, then redisplay menu
- IF Any other comments or queries: help user respond then redisplay menu

#### EXECUTION RULES:

- ALWAYS halt and wait for user input after presenting menu
- ONLY proceed to next step when user selects 'C'
- After other menu items execution, return to this menu with updated content
- User can chat or ask questions - always respond and then end with display again of the menu options

## CRITICAL STEP COMPLETION NOTE

Content is written to document immediately after generation. ONLY WHEN [C continue option] is selected will you then read fully and follow: `{nextStepFile}` to begin success metrics definition.

---

## 🚨 SYSTEM SUCCESS/FAILURE METRICS

### ✅ SUCCESS:

- Rich, believable user personas with clear motivations
- Clear distinction between primary and secondary users
- User journeys that show key interaction points and value creation
- User segments that align with product vision and problem statement
- A/P/C menu presented and handled correctly with proper task execution
- Content properly appended to document when C selected
- Frontmatter updated with stepsCompleted: [1, 2, 3]

### ❌ SYSTEM FAILURE:

- Creating generic user profiles without specific details
- Missing key user segments that are important to success
- User journeys that don't show how the product creates value
- Not connecting user needs back to the problem statement
- Not presenting standard A/P/C/R menu after writing content to file
- Not updating frontmatter properly

**Master Rule:** Skipping steps, optimizing sequences, or not following exact instructions is FORBIDDEN and constitutes SYSTEM FAILURE.
