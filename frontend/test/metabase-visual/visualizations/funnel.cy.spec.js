import { restore, visitQuestionAdhoc } from "__support__/e2e/cypress";
import { SAMPLE_DB_ID } from "__support__/e2e/cypress_data";

describe("visual tests > visualizations > funnel", () => {
  beforeEach(() => {
    restore();
    cy.signInAsNormalUser();
  });

  it("empty", () => {
    const testQuery = {
      type: "native",
      native: {
        query:
          "select 'a' col1, 0 col2 union all\n" +
          "select 'b', 0 union all\n" +
          "select 'c', 0",
      },
      database: SAMPLE_DB_ID,
    };

    visitQuestionAdhoc({
      dataset_query: testQuery,
      display: "funnel",
      visualization_settings: {
        "funnel.type": "funnel",
      },
    });

    cy.findByTestId("funnel-chart");
    cy.percySnapshot();
  });

  it("normal", () => {
    const testQuery = {
      type: "native",
      native: {
        query:
          "select 'a' step, 1000 users union all\n" +
          "select 'b', 800 union all\n" +
          "select 'c', 400 union all\n" +
          "select 'd', 155 union all\n" +
          "select 'e', 0",
      },
      database: SAMPLE_DB_ID,
    };

    visitQuestionAdhoc({
      dataset_query: testQuery,
      display: "funnel",
      visualization_settings: {
        "funnel.type": "funnel",
      },
    });

    cy.findByTestId("funnel-chart");
    cy.percySnapshot();
  });
});
