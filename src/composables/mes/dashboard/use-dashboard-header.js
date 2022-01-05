import { watch } from 'vue'

import {
  processingColorsEnum
} from '@enum-ms/production'

export default function useDashboardHeader({ colorCardTitles = ['未入库', '部分入库', '全部入库'], emit, crud, fetchSummaryInfo }) {
  const colors = [{
    title: colorCardTitles[0],
    color: processingColorsEnum.UNSTART.COLOR,
    value: processingColorsEnum.UNSTART.V
  },
  {
    title: colorCardTitles[1],
    color: processingColorsEnum.PROCESS.COLOR,
    value: processingColorsEnum.PROCESS.V
  },
  {
    title: colorCardTitles[2],
    color: processingColorsEnum.COMPLETE.COLOR,
    value: processingColorsEnum.COMPLETE.V
  }
  ]

  watch(
    [() => crud.query.monomerId, () => crud.query.factoryId],
    () => {
      if (typeof fetchSummaryInfo === 'function') fetchSummaryInfo()
    },
    { immediate: true }
  )

  function getColor(row, { quantity = 'inboundQuantity', compare = 'compareQuantity' }) {
    if (row[quantity] === 0) {
      return processingColorsEnum.UNSTART.COLOR
    }
    if (row[quantity] === row[compare]) {
      return processingColorsEnum.COMPLETE.COLOR
    }
    if (row.isProcess || row[quantity] > 0 && row[quantity] < row[compare]) {
      return processingColorsEnum.PROCESS.COLOR
    }
    return processingColorsEnum.ABNORMAL.COLOR
  }

  function getColorByValue(row, { field = 'type' }) {
    const _KEY = processingColorsEnum.VK[row[field]]
    if (_KEY) {
      return processingColorsEnum[_KEY].COLOR
    }
    return processingColorsEnum.ABNORMAL.COLOR
  }

  function getTagByValue(row, { field = 'type' }) {
    const _KEY = processingColorsEnum.VK[row[field]]
    if (_KEY) {
      return processingColorsEnum[_KEY].T
    }
    return processingColorsEnum.ABNORMAL.T
  }

  function boxZoomOut() {
    if (crud.page.hasNextPage) {
      emit('load')
    }
  }
  return {
    colors,
    boxZoomOut,
    getColor,
    getColorByValue,
    getTagByValue
  }
}
