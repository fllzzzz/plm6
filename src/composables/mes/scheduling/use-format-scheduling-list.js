import { deepClone } from '@data-type/index'

export default function useFormatSchedulingList(schedulingList, schedulingMapTemplate) {
  // 任务表单：schedulingMap，对于页面任务分配的数量存储在schedulingMap中， k-v, k:productionLineId, v:表单内容
  const schedulingMap = deepClone(schedulingMapTemplate)
  schedulingList.forEach((t) => {
    // 赋值
    const _t = schedulingMap[t.productionLineId]
    if (_t) {
      // _t.id = t.id // 任务id，
      _t.quantity = t.totalSchedulingQuantity || undefined // 任务数量
      _t.sourceQuantity = t.totalSchedulingQuantity || 0
      _t.lastQuantity = t.totalSchedulingQuantity || undefined // 最后一次分配的任务数量，用于数量输入错误，还原到上一次
      _t.producedQuantity = t.producedQuantity || 0
    }
  })
  return schedulingMap
}
