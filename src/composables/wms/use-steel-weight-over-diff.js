import { computed } from 'vue'
import { STEEL_DIFF_UNIT } from '@/settings/config'
import { numOrPctEnum } from '@/utils/enum/modules/common'
import { isBlank } from '@/utils/data-type'
import { convertUnits } from '@/utils/convert/unit'

import useWmsConfig from '../store/use-wms-config'
// 计算重量是否在正常范围内
export default function useWeightOverDiff(baseUnit) {
  const { loaded, inboundSteelCfg } = useWmsConfig()

  // 超出提示
  const overDiffTip = computed(() => {
    if (!loaded) {
      return '请等待wms钢材入库重量配置信息加载后再试'
    }
    if (inboundSteelCfg.value.steelDiffType === numOrPctEnum.PERCENTAGE.V) {
      return `实际重量与理论重量的误差不可超过理论重量的${inboundSteelCfg.value.steelDiff || 0}%`
    }
    return `实际重量与理论重量的误差不可超过${inboundSteelCfg.value.steelDiff || 0} ${STEEL_DIFF_UNIT}`
  })

  // 提交校验
  function diffSubmitValidate(row, overDiffSubmittable) {
    if (overDiffSubmittable || !row.hasOver) {
      return true
    }
    return false
  }

  // 超出校验
  function weightOverDiff(row, { inboundSteelCfg, baseUnit }) {
    if (!loaded) {
      return '请等待wms钢材入库重量配置信息加载后再试'
    }
    if (isBlank(row.weighingTotalWeight) && isBlank(row.theoryTotalWeight)) return

    let hasOver = false
    const overNum = row.weighingTotalWeight - row.theoryTotalWeight
    const steelDiff = inboundSteelCfg.value.steelDiff
    const steelDiffType = inboundSteelCfg.value.steelDiffType
    if (steelDiffType === numOrPctEnum.PERCENTAGE.V) {
      hasOver =
      (Math.abs((row.weighingTotalWeight / row.theoryTotalWeight) * Math.pow(10, 5) - 1 * Math.pow(10, 5)) / Math.pow(10, 5)) * 100 >
      steelDiff
    }
    if (steelDiffType === numOrPctEnum.NUMBER.V) {
      hasOver =
      convertUnits(
        Math.abs(row.weighingTotalWeight - row.theoryTotalWeight),
        baseUnit.value.weight.unit,
        STEEL_DIFF_UNIT,
        baseUnit.value.weight.precision
      ) > steelDiff
    }
    row.hasOver = hasOver
    row.overNum = overNum
  }

  return {
    overDiffTip,
    weightOverDiff: (row) => weightOverDiff(row, { inboundSteelCfg, baseUnit }),
    diffSubmitValidate: (value, row) => diffSubmitValidate(row, inboundSteelCfg.value.overDiffSubmittable)
  }
}
