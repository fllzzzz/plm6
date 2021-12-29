import { wageQuotaTypeEnum } from '@enum-ms/mes'
import { DP } from '@/settings/config'
import { convertUnits } from '@/utils/convert/unit'
import useWageQuotaUnit from './use-wage-quota-unit'

export default function useWageQuotaMeteConvert({
  W_CUR_UNIT = 'kg', weight, W_DP,
  L_CUR_UNIT = 'mm', length, L_DP,
  A_CUR_UNIT = '㎡', surfaceArea, A_DP,
  showUnit = false,
  wageQuotaType
}) {
  const { UNIT, dp } = useWageQuotaUnit({ wageQuotaType })
  const convertFn = {
    [wageQuotaTypeEnum.WEIGHT.V]: function ({ W_CUR_UNIT, weight, W_DP }) {
      return convertUnits(weight, W_CUR_UNIT, UNIT, DP[W_DP] || DP[dp], { showUnit })
    },
    [wageQuotaTypeEnum.LENGTH.V]: function ({ L_CUR_UNIT, length, L_DP }) {
      return convertUnits(length, L_CUR_UNIT, UNIT, DP[L_DP] || DP[dp], { showUnit })
    },
    [wageQuotaTypeEnum.AREA.V]: function ({ A_CUR_UNIT, surfaceArea, A_DP }) {
      return convertUnits(surfaceArea, A_CUR_UNIT, UNIT, DP[A_DP] || DP[dp], { showUnit })
    }
  }
  const convertMete = convertFn[wageQuotaType]({
    W_CUR_UNIT, weight, W_DP,
    L_CUR_UNIT, length, L_DP,
    A_CUR_UNIT, surfaceArea, A_DP
  })
  return {
    convertMete
  }
}
