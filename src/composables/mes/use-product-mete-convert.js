import { componentTypeEnum } from '@enum-ms/mes'
import { DP } from '@/settings/config'
import { convertUnits } from '@/utils/convert/unit'

export default function useProductMeteConvert({
  productType,
  showUnit = false,
  weight = { num: 0, from: 'kg', to: 'kg', dp: 'COM_WT__KG' },
  length = { num: 0, from: 'mm', to: 'mm', dp: 'COM_L__MM' }
}) {
  const convertFn = {
    [componentTypeEnum.ARTIFACT.V]: weightConvert,
    [componentTypeEnum.MACHINE_PART.V]: weightConvert,
    [componentTypeEnum.ASSEMBLE.V]: lengthConvert,
    [componentTypeEnum.ENCLOSURE.V]: lengthConvert
  }
  return convertFn[productType]({ weight, length, showUnit })
}

function weightConvert({ weight, showUnit }) {
  const { num, from = 'kg', to = 'kg', dp = 'COM_WT__KG' } = weight
  return convertUnits(num, from, to, DP[dp] || 2, { showUnit })
}

function lengthConvert({ length, showUnit }) {
  const { num, from = 'mm', to = 'mm', dp = 'COM_L__MM' } = length
  return convertUnits(num, from, to, DP[dp] || 2, { showUnit })
}
