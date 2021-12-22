import { wageQuotaTypeEnum } from '@enum-ms/mes'

export default function useWageQuotaUnit({ wageQuotaType }) {
  const UNIT = wageQuotaType && wageQuotaTypeEnum[wageQuotaTypeEnum.VK[wageQuotaType]].C_UNIT
  const unit = wageQuotaType && wageQuotaTypeEnum[wageQuotaTypeEnum.VK[wageQuotaType]].unit
  const meteUnit = wageQuotaType && wageQuotaTypeEnum[wageQuotaTypeEnum.VK[wageQuotaType]].meteUnit
  const field = wageQuotaType && wageQuotaTypeEnum[wageQuotaTypeEnum.VK[wageQuotaType]].F
  const dp = wageQuotaType && wageQuotaTypeEnum[wageQuotaTypeEnum.VK[wageQuotaType]].DP
  return {
    UNIT,
    dp,
    unit,
    meteUnit,
    field
  }
}
