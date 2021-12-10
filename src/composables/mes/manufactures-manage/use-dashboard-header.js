import { watch } from 'vue'

import {
  processingColorsEnum
} from '@enum-ms/production'

export default function useDashboardHeader({ colorCardTitle = '入库', emit, crud, fetchSummaryInfo }) {
  const colors = [{
    title: '未' + colorCardTitle,
    color: processingColorsEnum.UNSTART.COLOR,
    value: processingColorsEnum.UNSTART.V
  },
  {
    title: '部分' + colorCardTitle,
    color: processingColorsEnum.PROCESS.COLOR,
    value: processingColorsEnum.PROCESS.V
  },
  {
    title: '全部' + colorCardTitle,
    color: processingColorsEnum.COMPLETE.COLOR,
    value: processingColorsEnum.COMPLETE.V
  }
  ]

  watch(
    [() => crud.query.monomerId, () => crud.query.factoryId],
    () => {
      fetchSummaryInfo()
    },
    { immediate: true }
  )

  function getColor(row, { quantity = 'intWarehouseQuantity', compare = 'compareQuantity' }) {
    if (row[quantity] === 0) {
      return processingColorsEnum.UNSTART.COLOR
    }
    if (row[quantity] === row[compare]) {
      return processingColorsEnum.COMPLETE.COLOR
    }
    if (row[quantity] > 0 && row[quantity] < row[compare]) {
      return processingColorsEnum.PROCESS.COLOR
    }
    return processingColorsEnum.ABNORMAL.COLOR
  }

  function boxZoomOut() {
    if (crud.page.hasNextPage) {
      emit('load')
    }
  }
  return {
    colors,
    boxZoomOut,
    getColor
  }
}
