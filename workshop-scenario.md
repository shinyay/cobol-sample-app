# Legacy Code Modernization Workshop

This workshop is designed to help you understand the process of modernizing legacy code with using **GitHub Copilot**.

## Scenario

## Step 1: Ask GitHub Copilot how to migrate the code to a new language

### Task 1: Ask GitHub Copilot on GitHub Copilot Chat

1. Open Copilot Chat with `Ctrl`+`Shift`+`I` or `Cmd`+`Shift`+`I`.
2. Ask GitHub Copilot for the following:

    ```plaintext
    When determining the scope of a COBOL to Java conversion project, how do I determine the scope of the code base, modules, and functionality my team needs to migrate? What are the steps involved?
   ```

<details>
<summary>3. GitHub Copilot Sample Response</summary>
To determine the scope of the code base, modules, and functionality for a COBOL to Java conversion project, follow these steps:

1. **Inventory the COBOL Codebase**:
   - Identify all COBOL programs, scripts, and related files.
   - Document the size of the codebase (e.g., lines of code, number of files).

2. **Identify Modules and Components**:
   - Break down the COBOL codebase into logical modules and components.
   - Document the purpose and functionality of each module.

3. **Analyze Dependencies**:
   - Identify dependencies between COBOL modules.
   - Document external dependencies (e.g., databases, third-party libraries).

4. **Assess Functionality**:
   - List all functionalities provided by the COBOL application.
   - Prioritize functionalities based on business importance.

5. **Evaluate Data Structures**:
   - Document data structures used in the COBOL application.
   - Identify data storage mechanisms (e.g., files, databases).

6. **Review Business Logic**:
   - Extract and document business rules and logic embedded in the COBOL code.

7. **Determine Integration Points**:
   - Identify integration points with other systems.
   - Document interfaces and communication protocols.

8. **Estimate Effort**:
   - Estimate the effort required to convert each module and functionality.
   - Consider complexity, size, and dependencies.

9. **Plan Testing and Validation**:
   - Define a testing strategy for the converted Java application.
   - Plan for unit tests, integration tests, and user acceptance tests.

10. **Create a Migration Roadmap**:
    - Develop a detailed migration plan with timelines and milestones.
    - Include risk assessment and mitigation strategies.

By following these steps, you can comprehensively determine the scope of the COBOL to Java conversion project and ensure a structured and efficient migration process.
</details>
