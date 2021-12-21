import { componentTypeEnum } from '@enum-ms/mes'
import { DP } from '@/settings/config'
import { convertUnits } from '@/utils/convert/unit'

export default function useProductMeteConvert({
  productType,
  W_CUR_UNIT = 'kg', W_TO_UNIT = 'kg', weight, W_DP,
  L_CUR_UNIT = 'mm', L_TO_UNIT = 'mm', length, L_DP,
  A_CUR_UNIT = '㎡', A_TO_UNIT = '㎡', surfaceArea, A_DP
}) {
  const convertFn = {
    [componentTypeEnum.ARTIFACT.V]: weightConvert,
    [componentTypeEnum.MACHINE_PART.V]: weightConvert,
    [componentTypeEnum.ASSEMBLE.V]: lengthConvert,
    [componentTypeEnum.ENCLOSURE.V]: lengthConvert
  }
  const convertMete = convertFn[productType]({
    W_CUR_UNIT, W_TO_UNIT, weight, W_DP,
    L_CUR_UNIT, L_TO_UNIT, length, L_DP,
    A_CUR_UNIT, A_TO_UNIT, surfaceArea, A_DP
  })
  return {
    convertMete, weightConvert, lengthConvert, surfaceConvert
  }
}

function weightConvert({ W_CUR_UNIT, W_TO_UNIT, weight, W_DP }) {
  return convertUnits(weight, W_CUR_UNIT, W_TO_UNIT, DP[W_DP] || 2)
}

function lengthConvert({ L_CUR_UNIT, L_TO_UNIT, length, L_DP }) {
  return convertUnits(length, L_CUR_UNIT, L_TO_UNIT, DP[L_DP] || 2)
}

function surfaceConvert({ A_CUR_UNIT, A_TO_UNIT, surfaceArea, A_DP }) {
  return convertUnits(length, A_CUR_UNIT, A_TO_UNIT, DP[A_DP] || 2)
}
