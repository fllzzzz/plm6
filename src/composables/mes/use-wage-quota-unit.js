import { wageQuotaTypeEnum } from '@enum-ms/mes'
import { DP } from '@/settings/config'

export default function useWageQuotaUnit({ wageQuotaType }) {
  const UNIT = wageQuotaType && wageQuotaTypeEnum.V[wageQuotaType].C_UNIT
  const unit = wageQuotaType && wageQuotaTypeEnum.V[wageQuotaType].unit
  const meteUnit = wageQuotaType && wageQuotaTypeEnum.V[wageQuotaType].meteUnit
  const field = wageQuotaType && wageQuotaTypeEnum.V[wageQuotaType].F
  const dp = wageQuotaType && wageQuotaTypeEnum.V[wageQuotaType].DP
  return {
    UNIT,
    dp,
    unit,
    meteUnit,
    field,
    precision: DP[dp]
  }
}
