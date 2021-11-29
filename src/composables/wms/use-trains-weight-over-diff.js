import { computed } from 'vue'
import { STEEL_DIFF_UNIT } from '@/settings/config'
import { numOrPctEnum } from '@/utils/enum/modules/common'
import { isBlank } from '@/utils/data-type'
import { convertUnits } from '@/utils/convert/unit'

import useWmsConfig from '../store/use-wms-config'
// 计算重量是否在正常范围内
export default function useWeightOverDiff(baseUnit) {
  const { inboundSteelCfg } = useWmsConfig()

  // 超出提示
  const overDiffTip = computed(() => {
    if (inboundSteelCfg.value.trainsDiffType === numOrPctEnum.PERCENTAGE.V) {
      return `表格填写重量与车次重量的误差不可超过车次重量的${inboundSteelCfg.value.trainsDiff || 0}%`
    }
    return `表格填写重量与车次重量的误差不可超过${inboundSteelCfg.value.trainsDiff || 0}g`
  })

  return {
    overDiffTip,
    weightOverDiff: (trainsWeight, listWeight) => weightOverDiff(trainsWeight, listWeight, { inboundSteelCfg }),
    diffSubmitValidate: (hasOver) => diffSubmitValidate(hasOver, inboundSteelCfg.value.overDiffSubmittable)
  }
}

// 提交校验
function diffSubmitValidate(hasOver, overDiffSubmittable) {
  if (overDiffSubmittable || !hasOver) {
    return true
  }
  return false
}

function weightOverDiff(trainsWeight, listWeight, { inboundSteelCfg }) {
  if (isBlank(listWeight) && isBlank(trainsWeight)) return

  let hasOver = false
  const overNum = listWeight - trainsWeight
  const trainsDiff = inboundSteelCfg.value.trainsDiff
  const trainsDiffType = inboundSteelCfg.value.trainsDiffType
  if (trainsDiffType === numOrPctEnum.PERCENTAGE.V) {
    hasOver = Math.abs(listWeight / trainsWeight - 1) * 100 > trainsDiff
  }
  if (trainsDiffType === numOrPctEnum.NUMBER.V) {
    hasOver =
      convertUnits(
        Math.abs(listWeight - trainsWeight),
        'kg',
        STEEL_DIFF_UNIT,
        2
      ) > trainsDiff
  }
  return {
    hasOver,
    overNum
  }
}
