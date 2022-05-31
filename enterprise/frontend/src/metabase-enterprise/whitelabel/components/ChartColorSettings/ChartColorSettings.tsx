import React, { memo, useCallback, useMemo } from "react";
import { t } from "ttag";
import { flatten, omit, set } from "lodash";
import { useCurrentRef } from "metabase/hooks/use-current-ref";
import ColorPicker from "metabase/core/components/ColorPicker";
import { getChartColorGroups } from "./utils";
import {
  TableBody,
  TableBodyCell,
  TableBodyRow,
  TableHeader,
  TableLink,
  TableTitle,
} from "./ChartColorSettings.styled";

export interface ChartColorSettingsProps {
  colors: Record<string, string>;
  originalColors: Record<string, string>;
  onChange?: (colors: Record<string, string>) => void;
}

const ChartColorSettings = ({
  colors,
  originalColors,
  onChange,
}: ChartColorSettingsProps): JSX.Element => {
  const colorsRef = useCurrentRef(colors);
  const colorGroups = useMemo(getChartColorGroups, []);

  const handleChange = useCallback(
    (colorName: string, color?: string) => {
      if (color) {
        onChange?.(set({ ...colorsRef.current }, colorName, color));
      } else {
        onChange?.(omit({ ...colorsRef.current }, colorName));
      }
    },
    [colorsRef, onChange],
  );

  const handleReset = useCallback(() => {
    onChange?.(omit({ ...colorsRef.current }, flatten(colorGroups)));
  }, [colorsRef, colorGroups, onChange]);

  return (
    <ChartColorTable
      colors={colors}
      originalColors={originalColors}
      colorGroups={colorGroups}
      onChange={handleChange}
      onReset={handleReset}
    />
  );
};

interface ChartColorTable {
  colors: Record<string, string>;
  originalColors: Record<string, string>;
  colorGroups: string[][];
  onChange: (name: string, color?: string) => void;
  onReset: () => void;
}

const ChartColorTable = ({
  colors,
  originalColors,
  colorGroups,
  onChange,
  onReset,
}: ChartColorTable): JSX.Element => {
  return (
    <div>
      <TableHeader>
        <TableTitle>{t`Chart colors`}</TableTitle>
        <TableLink onClick={onReset}>{t`Reset to default colors`}</TableLink>
      </TableHeader>
      <TableBody>
        {colorGroups.map((colorGroup, index) => (
          <TableBodyRow key={index}>
            {colorGroup.map(colorName => (
              <ChartColorCell
                key={colorName}
                color={colors[colorName]}
                originalColor={originalColors[colorName]}
                colorName={colorName}
                onChange={onChange}
              />
            ))}
          </TableBodyRow>
        ))}
      </TableBody>
    </div>
  );
};

interface ChartColorCellProps {
  color?: string;
  originalColor: string;
  colorName: string;
  onChange: (colorName: string, color?: string) => void;
}

const ChartColorCell = memo(function ChartColorCell({
  color,
  originalColor,
  colorName,
  onChange,
}: ChartColorCellProps) {
  const handleChange = useCallback(
    (color?: string) => {
      onChange(colorName, color);
    },
    [colorName, onChange],
  );

  return (
    <TableBodyCell>
      <ColorPicker
        value={color ?? originalColor}
        isAuto={color == null || color === originalColor}
        onChange={handleChange}
      />
    </TableBodyCell>
  );
});

export default ChartColorSettings;
