import { computed } from 'vue'
import { STEEL_DIFF_UNIT } from '@/settings/config'
import { numOrPctEnum } from '@/utils/enum/modules/common'
import { isBlank } from '@/utils/data-type'
import { convertUnits } from '@/utils/convert/unit'

import useWmsConfig from '../store/use-wms-config'
// 计算重量是否在正常范围内
export default function useWeightOverDiff(baseUnit, { cfgType = 'inbound', weightField = 'weighingTotalWeight', compareWeightField = 'theoryTotalWeight', weightTip = '理论重量' } = {}) {
  const { loaded, inboundSteelCfg, purchaseCfg } = useWmsConfig()

  const weightCfg = computed(() => {
    if (cfgType === 'inbound') {
      return inboundSteelCfg.value
    }
    if (cfgType === 'purchase') {
      return purchaseCfg.value
    }
    return null
  })

  // 超出提示
  const overDiffTip = computed(() => {
    if (!loaded) {
      return `请等待${cfgType === 'inbound' ? 'wms钢材入库' : '采购钢材'}重量配置信息加载后再试`
    }
    if (weightCfg.value.steelDiffType === numOrPctEnum.PERCENTAGE.V) {
      return `实际重量与${weightTip}的误差不可超过${weightTip}的${weightCfg.value.steelDiff || 0}%`
    }
    return `实际重量与${weightTip}的误差不可超过${weightCfg.value.steelDiff || 0} ${STEEL_DIFF_UNIT}`
  })

  // 提交校验
  function diffSubmitValidate(row, overDiffSubmittable) {
    // if (overDiffSubmittable || !row.hasOver) {
    if (!row.hasOver) {
      return true
    }
    return false
  }

  // 超出校验
  function weightOverDiff(row, { weightCfg, baseUnit }) {
    if (!loaded) {
      return `请等待${cfgType === 'inbound' ? 'wms钢材入库' : '采购钢材'}重量配置信息加载后再试`
    }
    if (isBlank(row[weightField]) && isBlank(row[compareWeightField])) return
    let hasOver = false
    const overNum = row[weightField] - row[compareWeightField]
    const steelDiff = weightCfg.value.steelDiff
    const steelDiffType = weightCfg.value.steelDiffType
    if (steelDiffType === numOrPctEnum.PERCENTAGE.V) {
      hasOver =
      (Math.abs(parseInt((row[weightField] / row[compareWeightField]) * Math.pow(10, 5) - 1 * Math.pow(10, 5))) / Math.pow(10, 5)) * 100 >
      steelDiff
    }
    if (steelDiffType === numOrPctEnum.NUMBER.V) {
      hasOver =
      convertUnits(
        Math.abs(row[weightField] - row[compareWeightField]),
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
    currentCfg: weightCfg,
    weightOverDiff: (row) => weightOverDiff(row, { weightCfg, baseUnit }),
    diffSubmitValidate: (value, row) => diffSubmitValidate(row, weightCfg.value.overDiffSubmittable)
  }
}
