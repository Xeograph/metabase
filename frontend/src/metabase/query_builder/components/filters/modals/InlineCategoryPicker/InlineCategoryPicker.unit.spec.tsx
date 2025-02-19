/* eslint-disable @typescript-eslint/ban-ts-comment */

import React from "react";
import { render, screen, waitFor } from "@testing-library/react";
import userEvent from "@testing-library/user-event";

import { metadata } from "__support__/sample_database_fixture";

import Field from "metabase-lib/lib/metadata/Field";
import Filter from "metabase-lib/lib/queries/structured/Filter";
import Question from "metabase-lib/lib/Question";
import StructuredQuery from "metabase-lib/lib/queries/StructuredQuery";

import { InlineCategoryPickerComponent } from "./InlineCategoryPicker";
import { MAX_INLINE_CATEGORIES } from "./constants";

const smallCategoryField = new Field({
  database_type: "test",
  semantic_type: "type/Category",
  effective_type: "type/Text",
  base_type: "type/Text",
  table_id: 8,
  name: "small_category_field",
  has_field_values: "list",
  values: [["Michaelangelo"], ["Donatello"], ["Raphael"], ["Leonardo"]],
  dimensions: {},
  dimension_options: [],
  id: 137,
  metadata,
});

// we want to make sure we always get enough unique field values
// even if we change MAX_INLINE_CATEGORIES
const turtleFactory = () => {
  const name = ["Michaelangelo", "Donatello", "Raphael", "Leonardo"][
    Math.floor(Math.random() * 4)
  ];
  return [`${name}_${Math.round(Math.random() * 100000)}`];
};

const largeCategoryField = new Field({
  database_type: "test",
  semantic_type: "type/Category",
  effective_type: "type/Text",
  base_type: "type/Text",
  table_id: 8,
  name: "large_category_field",
  has_field_values: "list",
  values: new Array(MAX_INLINE_CATEGORIES + 1).fill(null).map(turtleFactory),
  dimensions: {},
  dimension_options: [],
  id: 138,
  metadata,
});

const emptyCategoryField = new Field({
  database_type: "test",
  semantic_type: "type/Category",
  effective_type: "type/Text",
  base_type: "type/Text",
  table_id: 8,
  name: "empty_category_field",
  has_field_values: "list",
  values: [],
  dimensions: {},
  dimension_options: [],
  id: 139,
  metadata,
});

// @ts-ignore
metadata.fields[smallCategoryField.id] = smallCategoryField;
// @ts-ignore
metadata.fields[largeCategoryField.id] = largeCategoryField;
// @ts-ignore
metadata.fields[emptyCategoryField.id] = emptyCategoryField;

const card = {
  dataset_query: {
    database: 5,
    query: {
      "source-table": 8,
    },
    type: "query",
  },
  display: "table",
  visualization_settings: {},
};

const question = new Question(card, metadata);
const query = question.query() as StructuredQuery;
const smallDimension = smallCategoryField.dimension();
const largeDimension = largeCategoryField.dimension();
const emptyDimension = emptyCategoryField.dimension();

describe("InlineCategoryPicker", () => {
  it("should render an inline category picker", () => {
    const testFilter = new Filter(
      ["=", ["field", smallCategoryField.id, null], undefined],
      null,
      query,
    );
    const changeSpy = jest.fn();
    const fetchSpy = jest.fn();

    render(
      <InlineCategoryPickerComponent
        query={query}
        filter={testFilter}
        newFilter={testFilter}
        onChange={changeSpy}
        fieldValues={smallCategoryField.values}
        fetchFieldValues={fetchSpy}
        dimension={smallDimension}
        onClear={changeSpy}
      />,
    );

    screen.getByTestId("category-picker");
    smallCategoryField.values.forEach(([value]) => {
      screen.getByText(value);
    });
  });

  it("should render a loading spinner while loading", async () => {
    const testFilter = new Filter(
      ["=", ["field", emptyCategoryField.id, null], undefined],
      null,
      query,
    );
    const changeSpy = jest.fn();
    const fetchSpy = jest.fn();

    render(
      <InlineCategoryPickerComponent
        query={query}
        filter={testFilter}
        newFilter={testFilter}
        onChange={changeSpy}
        fieldValues={emptyCategoryField.values}
        fetchFieldValues={fetchSpy}
        dimension={emptyDimension}
        onClear={changeSpy}
      />,
    );
    screen.getByTestId("loading-spinner");
    await waitFor(() => expect(fetchSpy).toHaveBeenCalled());
  });

  it("should render a warning message on api failure", async () => {
    const testFilter = new Filter(
      ["=", ["field", emptyCategoryField.id, null], undefined],
      null,
      query,
    );
    const changeSpy = jest.fn();
    const fetchSpy = jest.fn();

    render(
      <InlineCategoryPickerComponent
        query={query}
        filter={testFilter}
        newFilter={testFilter}
        onChange={changeSpy}
        fieldValues={emptyCategoryField.values}
        fetchFieldValues={fetchSpy}
        dimension={emptyDimension}
        onClear={changeSpy}
      />,
    );
    await waitFor(() => expect(fetchSpy).toHaveBeenCalled());
    screen.getByLabelText("warning icon");
  });

  it(`should render up to ${MAX_INLINE_CATEGORIES} checkboxes`, () => {
    const testFilter = new Filter(
      ["=", ["field", smallCategoryField.id, null], undefined],
      null,
      query,
    );
    const changeSpy = jest.fn();
    const fetchSpy = jest.fn();

    render(
      <InlineCategoryPickerComponent
        query={query}
        filter={testFilter}
        newFilter={testFilter}
        onChange={changeSpy}
        fieldValues={smallCategoryField.values}
        fetchFieldValues={fetchSpy}
        dimension={smallDimension}
        onClear={changeSpy}
      />,
    );

    screen.getByTestId("category-picker");
    smallCategoryField.values.forEach(([value]) => {
      screen.getByText(value);
    });
  });

  it(`should not render more than ${MAX_INLINE_CATEGORIES} checkboxes`, () => {
    const testFilter = new Filter(
      ["=", ["field", largeCategoryField.id, null], undefined],
      null,
      query,
    );
    const changeSpy = jest.fn();
    const fetchSpy = jest.fn();

    render(
      <InlineCategoryPickerComponent
        query={query}
        filter={testFilter}
        newFilter={testFilter}
        onChange={changeSpy}
        fieldValues={largeCategoryField.values}
        fetchFieldValues={fetchSpy}
        dimension={largeDimension}
        onClear={changeSpy}
      />,
    );

    expect(screen.queryByTestId("category-picker")).not.toBeInTheDocument();
    // should render general purpose picker instead
    screen.getByTestId("select-button");
  });

  it("should load existing filter selections", () => {
    const testFilter = new Filter(
      ["=", ["field", smallCategoryField.id, null], "Donatello", "Leonardo"],
      null,
      query,
    );
    const changeSpy = jest.fn();
    const fetchSpy = jest.fn();

    render(
      <InlineCategoryPickerComponent
        query={query}
        filter={testFilter}
        newFilter={testFilter}
        onChange={changeSpy}
        fieldValues={smallCategoryField.values}
        fetchFieldValues={fetchSpy}
        dimension={smallDimension}
        onClear={changeSpy}
      />,
    );

    screen.getByTestId("category-picker");
    expect(screen.getByLabelText("Donatello")).toBeChecked();
    expect(screen.getByLabelText("Leonardo")).toBeChecked();
    expect(screen.getByLabelText("Raphael")).not.toBeChecked();
    expect(screen.getByLabelText("Michaelangelo")).not.toBeChecked();
  });

  it("should save a filter based on selection", () => {
    const testFilter = new Filter(
      ["=", ["field", smallCategoryField.id, null], undefined],
      null,
      query,
    );
    const changeSpy = jest.fn();
    const fetchSpy = jest.fn();

    render(
      <InlineCategoryPickerComponent
        query={query}
        filter={testFilter}
        newFilter={testFilter}
        onChange={changeSpy}
        fieldValues={smallCategoryField.values}
        fetchFieldValues={fetchSpy}
        dimension={smallDimension}
        onClear={changeSpy}
      />,
    );

    screen.getByTestId("category-picker");
    userEvent.click(screen.getByLabelText("Raphael"));
    expect(changeSpy.mock.calls.length).toBe(1);
    expect(changeSpy.mock.calls[0][0]).toEqual([
      "=",
      ["field", 137, null],
      "Raphael",
    ]);
  });

  it("should fetch field values data if its not already loaded", async () => {
    const testFilter = new Filter(
      ["=", ["field", emptyCategoryField.id, null], undefined],
      null,
      query,
    );
    const changeSpy = jest.fn();
    const fetchSpy = jest.fn();

    render(
      <InlineCategoryPickerComponent
        query={query}
        filter={testFilter}
        newFilter={testFilter}
        onChange={changeSpy}
        fieldValues={emptyCategoryField.values}
        fetchFieldValues={fetchSpy}
        dimension={emptyDimension}
        onClear={changeSpy}
      />,
    );
    await waitFor(() => expect(fetchSpy).toHaveBeenCalled());

    expect(fetchSpy.mock.calls[0][0]).toEqual({ id: emptyCategoryField.id });
  });

  it("should not fetch field values data if it is already present", async () => {
    const testFilter = new Filter(
      ["=", ["field", largeCategoryField.id, null], undefined],
      null,
      query,
    );
    const changeSpy = jest.fn();
    const fetchSpy = jest.fn();

    render(
      <InlineCategoryPickerComponent
        query={query}
        filter={testFilter}
        newFilter={testFilter}
        onChange={changeSpy}
        fieldValues={largeCategoryField.values}
        fetchFieldValues={fetchSpy}
        dimension={largeDimension}
        onClear={changeSpy}
      />,
    );

    expect(fetchSpy).not.toHaveBeenCalled();
  });
});
