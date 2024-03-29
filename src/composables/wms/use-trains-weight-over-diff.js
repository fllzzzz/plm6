import { computed, ref } from 'vue'
import { STEEL_DIFF_UNIT } from '@/settings/config'
import { numOrPctEnum } from '@/utils/enum/modules/common'
import { isBlank, toFixed } from '@/utils/data-type'
import { convertUnits } from '@/utils/convert/unit'

import useWmsConfig from '../store/use-wms-config'
// 计算重量是否在正常范围内
export default function useWeightOverDiff() {
  const { loaded, inboundSteelCfg } = useWmsConfig()
  const overableNumber = ref(0)

  // 超出提示
  const overDiffTip = computed(() => {
    if (!loaded) {
      return '请等待wms车次重量配置信息加载后再试'
    }
    if (inboundSteelCfg.value.trainsDiffType === numOrPctEnum.PERCENTAGE.V) {
      return `车次重量与入库钢材重量的误差不可超过入库钢材重量的${inboundSteelCfg.value.trainsDiff || 0}%（${overableNumber.value}kg）`
    }
    return `车次重量与入库钢材重量的误差不可超过${inboundSteelCfg.value.trainsDiff || 0} ${STEEL_DIFF_UNIT}`
  })

  // 提交校验
  function diffSubmitValidate(hasOver, overDiffSubmittable) {
    if (overDiffSubmittable || !hasOver) {
      return true
    }
    return false
  }

  function weightOverDiff(trainsWeight, listWeight, { inboundSteelCfg }) {
    if (!loaded) {
      return '请等待wms车次重量配置信息加载后再试'
    }
    if (isBlank(listWeight) && isBlank(trainsWeight)) return {}
    let hasOver = false
    const trainsDiff = inboundSteelCfg.value.trainsDiff
    const trainsDiffType = inboundSteelCfg.value.trainsDiffType
    const overNum = +toFixed(trainsWeight - listWeight, 0)
    overableNumber.value = +toFixed(listWeight * (trainsDiff / 100), 0)
    if (trainsDiffType === numOrPctEnum.PERCENTAGE.V) {
      hasOver = +toFixed(Math.abs(listWeight - trainsWeight), 0) > overableNumber.value
    }
    if (trainsDiffType === numOrPctEnum.NUMBER.V) {
      hasOver = convertUnits(Math.abs(listWeight - trainsWeight), 'kg', STEEL_DIFF_UNIT, 0) > trainsDiff
    }
    return {
      hasOver,
      overNum
    }
  }

  return {
    overDiffTip,
    weightOverDiff: (trainsWeight, listWeight) => weightOverDiff(trainsWeight, listWeight, { inboundSteelCfg }),
    diffSubmitValidate: (hasOver) => diffSubmitValidate(hasOver, inboundSteelCfg.value.overDiffSubmittable)
  }
}
