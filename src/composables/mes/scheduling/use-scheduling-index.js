import { ref, nextTick } from 'vue'
import { ElMessage } from 'element-plus'

export default function useSchedulingIndex() {
  const lines = ref([])
  const modifying = ref(false)

  // 任务异常标红
  function handleRowClassName({ row, rowIndex }) {
    if (row.abnormalStatus > 0) {
      return 'abnormal-row'
    }
  }

  /**
 * 数量变化，cell颜色变化（->绿色）
 */
  function handelCellClassName({ row, column, rowIndex, columnIndex }) {
    if (!column.property) {
      return
    }
    const markColumn = ['productionLine']
    const columnArr = column.property.split('_') // [0]:字段名，[1]:生产线id
    // let mask = false
    if (markColumn.includes(columnArr[0])) {
      if ((row.schedulingMap[columnArr[1]].quantity || 0) > (row.sourceSchedulingMap[columnArr[1]].quantity || 0)) {
        return 'mask-td mask-td-green'
      }
      if ((row.schedulingMap[columnArr[1]].quantity || 0) < (row.sourceSchedulingMap[columnArr[1]].quantity || 0)) {
        return 'mask-td'
      }
    }
    return ''
  }

  /**
 * 处理分配数量修改
 */
  function handleQuantityChange(row, line, value) {
    const _val = value || 0
    if (row.schedulingMap[line.id].producedQuantity > _val) {
    // 分配数量小于已生产数量
      nextTick(() => {
        row.schedulingMap[line.id].quantity = row.schedulingMap[line.id].lastQuantity
        ElMessage.warning(`分配数量不得小于已生产数量，已生产数量：${row.schedulingMap[line.id].producedQuantity}`)
      })
      return
    }
    if (!value) {
    // 分配数量为 0/undefined
      nextTick(() => {
        row.schedulingMap[line.id].quantity = undefined
      })
      // 不通过构件数量计算，避免数据报废等分配数量不正确的情况
      row.unassignQuantity += row.schedulingMap[line.id].lastQuantity || 0
      row.assignQuantity -= row.schedulingMap[line.id].lastQuantity || 0
      row.schedulingMap[line.id].lastQuantity = undefined
      return
    }
    const changeQuantity = _val - (row.schedulingMap[line.id].lastQuantity || 0) // 变化的数量
    // 变化数量是否小于等于当前未分配数量
    if (changeQuantity <= row.unassignQuantity) {
    // 分配成功，改变最后一次分配的数量与未分配数量
      row.schedulingMap[line.id].lastQuantity = _val
      row.unassignQuantity -= changeQuantity
      row.assignQuantity += changeQuantity
    } else {
    // 分配失败，将分配情况还原到上一次
      nextTick(() => {
        row.schedulingMap[line.id].quantity = row.schedulingMap[line.id].lastQuantity
      })
      ElMessage.warning('分配数量大于未分配数量')
    }
  }

  return {
    lines,
    modifying,
    handleRowClassName,
    handelCellClassName,
    handleQuantityChange
  }
}
