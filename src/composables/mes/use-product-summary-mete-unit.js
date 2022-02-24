import {
  componentTypeEnum
} from '@enum-ms/mes'
import {
  DP
} from '@/settings/config'

export default function useProductSummaryMeteUnit({
  productType,
  w_unit = 't',
  l_unit = 'm',
  isSingle = false
}) {
  const unitDp = {
    't': 'COM_WT__T',
    'kg': 'COM_WT__KG',
    'm': 'COM_L__M',
    'mm': 'COM_L__MM'
  }
  const c_unit = {
    't': '吨',
    'kg': '千克',
    'm': '米',
    'mm': '毫米'
  }
  const w_dp = unitDp[w_unit]
  const l_dp = unitDp[l_unit]

  const unitObj = {
    [componentTypeEnum.ARTIFACT.V]: {
      label: isSingle ? '单重' : '重量',
      measure: '件',
      unit: w_unit,
      c_unit: c_unit[w_unit],
      dp: w_dp,
      DP: DP[w_dp]
    },
    [componentTypeEnum.MACHINE_PART.V]: {
      label: isSingle ? '单重' : '重量',
      measure: '件',
      unit: w_unit,
      c_unit: c_unit[w_unit],
      dp: w_dp,
      DP: DP[w_dp]
    },
    [componentTypeEnum.ASSEMBLE.V]: {
      label: isSingle ? '单重' : '重量',
      measure: '件',
      unit: w_unit,
      c_unit: c_unit[w_unit],
      dp: w_dp,
      DP: DP[w_dp]
    },
    [componentTypeEnum.ENCLOSURE.V]: {
      label: isSingle ? '单长' : '长度',
      measure: '张',
      unit: l_unit,
      c_unit: c_unit[l_unit],
      dp: l_dp,
      DP: DP[l_dp]
    }
  }

  return unitObj[productType]
}
