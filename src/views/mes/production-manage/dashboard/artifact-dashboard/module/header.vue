<template>
  <div class="head-container">
    <div v-show="crud.searchToggle">
      <factory-select
        v-model="query.factoryId"
        show-all
        class="filter-item"
        style="width: 200px"
        @change="crud.toQuery"
      />
      <el-input
        v-model="query.name"
        size="small"
        placeholder="输入名称搜索"
        style="width: 170px"
        class="filter-item"
        clearable
        @keyup.enter="crud.toQuery"
      />
      <el-input
        v-model="query.serialNumber"
        size="small"
        placeholder="输入编号搜索"
        style="width: 170px"
        class="filter-item"
        clearable
        @keyup.enter="crud.toQuery"
      />
      <el-input
        v-model="query.specification"
        size="small"
        placeholder="输入规格搜索"
        style="width: 170px"
        class="filter-item"
        clearable
        @keyup.enter="crud.toQuery"
      />
      <el-input
        v-model="query.material"
        size="small"
        placeholder="输入材质搜索"
        style="width: 170px"
        class="filter-item"
        clearable
        @keyup.enter="crud.toQuery"
      />
      <rrOperation />
    </div>
    <crudOperation :show-grid="false" :show-refresh="false">
      <template #optRight>
        <color-card
          class="filter-item"
          v-model:value="query.processingStatus"
          :colors="colors"
          color-border
          select-able
          @change="crud.toQuery"
        />
      </template>
      <template #viewLeft>
        <scale class="filter-item" v-model:value="boxScale" :intervals="400" @zoom-out="boxZoomOut" />
      </template>
    </crudOperation>
  </div>
</template>

<script setup>
import { ref, defineExpose, defineEmits } from 'vue'

import useDashboardHeader from '@compos/mes/dashboard/use-dashboard-header'
import { regHeader } from '@compos/use-crud'
import crudOperation from '@crud/CRUD.operation'
import rrOperation from '@crud/RR.operation'
import ColorCard from '@comp/ColorCard'
import Scale from '@comp/Scale'
import factorySelect from '@comp-base/factory-select'

const defaultQuery = {
  name: '',
  serialNumber: '',
  specification: '',
  material: '',
  processingStatus: { value: undefined, resetAble: false },
  monomerId: { value: undefined, resetAble: false },
  areaId: { value: undefined, resetAble: false },
  factoryId: { value: undefined, resetAble: false },
  status: { value: undefined, resetAble: false }
}
const { crud, query, CRUD } = regHeader(defaultQuery)

const emit = defineEmits('load')

const boxScale = ref(1)
const { colors, boxZoomOut, getColor } = useDashboardHeader({ colorCardTitles: ['未生产', '生产中', '已完成'], emit, crud })

CRUD.HOOK.handleRefresh = (crud, res) => {
  res.data.content = res.data.content.map((v) => {
    v.processInfo = '-----------------------\n\n生产上报 / 已质检\n\n'
    const processList = v.process || []
    v.compareQuantity = crud.query.factoryId ? v.assignQuantity : v.quantity
    processList.forEach(process => {
      const _p = this.processMap[process.id]
      if (_p) {
        const _completed = v.compareQuantity === process.quantity && process.quantity === process.inspectionQuantity
        const _processInfo = _completed ? `√` : `${process.quantity} / ${process.inspectionQuantity}`
        v.processInfo += `${_p.name}：${_processInfo}\n\n`
        if (process.quantity) {
          v.started = true
        }
      }
    })
    v.boxColor = getColor(v, { quantity: 'completedQuantity', compare: 'compareQuantity' })
    return v
  })
}

defineExpose({
  boxScale
})
</script>
