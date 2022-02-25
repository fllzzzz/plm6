import { wageQuotaTypeEnum } from '@enum-ms/mes'
import { convertUnits } from '@/utils/convert/unit'
import useWageQuotaUnit from './use-wage-quota-unit'

export default function useWageQuotaMeteConvert({
  W_CUR_UNIT = 'kg', weight,
  L_CUR_UNIT = 'mm', length,
  A_CUR_UNIT = '㎡', surfaceArea,
  showUnit = false,
  wageQuotaType
}) {
  const { UNIT, precision } = useWageQuotaUnit({ wageQuotaType })
  const convertFn = {
    [wageQuotaTypeEnum.WEIGHT.V]: function ({ W_CUR_UNIT, weight }) {
      return convertUnits(weight, W_CUR_UNIT, UNIT, precision, { showUnit })
    },
    [wageQuotaTypeEnum.LENGTH.V]: function ({ L_CUR_UNIT, length, L_DP }) {
      return convertUnits(length, L_CUR_UNIT, UNIT, precision, { showUnit })
    },
    [wageQuotaTypeEnum.AREA.V]: function ({ A_CUR_UNIT, surfaceArea, A_DP }) {
      return convertUnits(surfaceArea, A_CUR_UNIT, UNIT, precision, { showUnit })
    }
  }
  const convertMete = convertFn[wageQuotaType]({
    W_CUR_UNIT, weight,
    L_CUR_UNIT, length,
    A_CUR_UNIT, surfaceArea
  })
  return {
    convertMete
  }
}
