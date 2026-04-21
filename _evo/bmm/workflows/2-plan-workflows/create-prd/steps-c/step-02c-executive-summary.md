---
name: 'step-02c-executive-summary'
description: 'Generate and append the Executive Summary section to the PRD document'

# File References
nextStepFile: '{project-root}/_evo/bmm/workflows/2-plan-workflows/create-prd/steps-c/step-03-success.md'
outputFile: '{planning_artifacts}/{active_feature}/prd.md'

# Task References
advancedElicitationTask: '{project-root}/_evo/core/workflows/advanced-elicitation/workflow.md'
partyModeWorkflow: '{project-root}/_evo/core/workflows/party-mode/workflow.md'
---

# Step 2c: Executive Summary Generation

**Progress: Step 2c of 13** - Next: Success Criteria

## STEP GOAL:

Generate the Executive Summary content using insights from classification (step 2) and vision discovery (step 2b), then append it to the PRD document.

## MANDATORY EXECUTION RULES (READ FIRST):

### Universal Rules:

- 🛑 NEVER generate content without user input
- 📖 CRITICAL: Read the complete step file before taking any action
- 🔄 CRITICAL: When loading next step with 'C', ensure the entire file is read
- ✅ ALWAYS treat this as collaborative discovery between PM peers
- 📋 YOU ARE A FACILITATOR, not a content generator
- ✅ YOU MUST ALWAYS SPEAK OUTPUT In your Agent communication style with the config `{communication_language}`

### Role Reinforcement:

- ✅ You are a product-focused PM facilitator collaborating with an expert peer
- ✅ We engage in collaborative dialogue, not command-response
- ✅ Content is drafted collaboratively and written directly to file

### Step-Specific Rules:

- 🎯 Generate Executive Summary content based on discovered insights
- 💾 Write content directly to file after generation — no preview in chat
- 🎯 Content must be dense, precise, and zero-fluff (PRD quality standards)

## EXECUTION PROTOCOLS:

- 🎯 Show your analysis before taking any action
- 💾 Write content directly to {outputFile} after generation
- 📖 Update output file frontmatter, adding this step name to the end of the list of stepsCompleted
- ⚠️ Present A/P/C/R menu after writing to file

## CONTEXT BOUNDARIES:

- Current document and frontmatter from steps 1, 2, and 2b are available
- Project classification exists from step 2 (project type, domain, complexity, context)
- Vision and differentiator insights exist from step 2b
- Input documents from step 1 are available (product briefs, research, brainstorming, project docs)
- This step generates and appends the first substantive content to the PRD

## YOUR TASK:

Draft the Executive Summary section using all discovered insights, present it for user review, and append it to the PRD document when approved.

## EXECUTIVE SUMMARY GENERATION SEQUENCE:

### 1. Synthesize Available Context

Review all available context before drafting:
- Classification from step 2: project type, domain, complexity, project context
- Vision and differentiator from step 2b: what makes this special, core insight
- Input documents: product briefs, research, brainstorming, project docs

### 2. Draft Executive Summary Content

Generate the Executive Summary section using the content structure below. Apply PRD quality standards:
- High information density — every sentence carries weight
- Zero fluff — no filler phrases or vague language
- Precise and actionable — clear, specific statements
- Dual-audience optimized — readable by humans, consumable by LLMs

### 3. Write to File and Present Menu

After generating the Executive Summary content:

1. Append the content to `{outputFile}` using the structure from the Content Structure section below
2. Update frontmatter by adding this step name to the end of the stepsCompleted array

Then display menu:

Display: "**Select:** [A] Advanced Elicitation [P] Party Mode [C] Continue to Success Criteria (Step 3 of 13) [R] Rewrite this section"

#### Menu Handling Logic:
- IF A: Read fully and follow: {advancedElicitationTask} with the current executive summary content, process the enhanced content that comes back, ask user if they accept the improvements, if yes overwrite section in {outputFile} with improvements then redisplay menu, if no keep original then redisplay menu
- IF P: Read fully and follow: {partyModeWorkflow} with the current executive summary content, process the collaborative improvements, ask user if they accept the changes, if yes overwrite section in {outputFile} with improvements then redisplay menu, if no keep original then redisplay menu
- IF C: Read fully and follow: {nextStepFile}
- IF R: Rewrite the section from scratch based on user feedback, overwrite in {outputFile}, then redisplay menu
- IF Any other: help user respond, then redisplay menu

#### EXECUTION RULES:
- ALWAYS halt and wait for user input after presenting menu
- ONLY proceed to next step when user selects 'C'
- After other menu items execution, return to this menu

## APPEND TO DOCUMENT:

After generation, immediately append the following content structure directly to the document (before presenting the menu):

```markdown
## Executive Summary

{vision_alignment_content}

### What Makes This Special

{product_differentiator_content}

## Project Classification

{project_classification_content}
```

Where:
- `{vision_alignment_content}` — Product vision, target users, and the problem being solved. Dense, precise summary drawn from step 2b vision discovery.
- `{product_differentiator_content}` — What makes this product unique, the core insight, and why users will choose it over alternatives. Drawn from step 2b differentiator discovery.
- `{project_classification_content}` — Project type, domain, complexity level, and project context (greenfield/brownfield). Drawn from step 2 classification.

## CRITICAL STEP COMPLETION NOTE

Content is written to document immediately after generation. ONLY WHEN [C continue option] is selected will you then read fully and follow: `{nextStepFile}` to define success criteria.

---

## 🚨 SYSTEM SUCCESS/FAILURE METRICS

### ✅ SUCCESS:

- Executive Summary drafted using insights from steps 2 and 2b
- Content meets PRD quality standards (dense, precise, zero-fluff)
- Content properly appended to document immediately after generation
- A/P/C/R menu presented and handled correctly
- Frontmatter updated with stepsCompleted when C selected

### ❌ SYSTEM FAILURE:

- Generating content without incorporating discovered vision and classification
- Producing vague, fluffy, or low-density content
- Not presenting A/P/C/R menu after writing content to file
- Skipping directly to next step without appending content to file first

❌ **CRITICAL**: Reading only partial step file - leads to incomplete understanding and poor decisions
❌ **CRITICAL**: Proceeding with 'C' without fully reading and understanding the next step file
❌ **CRITICAL**: Making decisions without complete understanding of step requirements and protocols

**Master Rule:** Generate high-quality Executive Summary content from discovered insights. Present for review, refine collaboratively, and only save when the user approves. This is the first substantive content in the PRD — it sets the quality bar for everything that follows.
