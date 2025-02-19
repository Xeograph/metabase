import {
  restore,
  visitQuestionAdhoc,
  ensureDcChartVisibility,
} from "__support__/e2e/cypress";

import { SAMPLE_DB_ID } from "__support__/e2e/cypress_data";
import { SAMPLE_DATABASE } from "__support__/e2e/cypress_sample_database";

const { ORDERS, ORDERS_ID } = SAMPLE_DATABASE;

const testQuery = {
  type: "query",
  query: {
    "source-table": ORDERS_ID,
    aggregation: [["count"]],
    breakout: [
      [
        "field",
        ORDERS.CREATED_AT,
        {
          "temporal-unit": "year",
        },
      ],
    ],
  },
  database: SAMPLE_DB_ID,
};

describe("visual tests > visualizations > waterfall", () => {
  beforeEach(() => {
    restore();
    cy.signInAsNormalUser();
  });

  it("with positive and negative series", () => {
    visitQuestionAdhoc({
      dataset_query: testQuery,
      display: "waterfall",
      visualization_settings: {
        "graph.show_values": true,
        "graph.dimensions": ["CREATED_AT"],
        "graph.metrics": ["count"],
      },
    });

    ensureDcChartVisibility();
    cy.percySnapshot();
  });
});
