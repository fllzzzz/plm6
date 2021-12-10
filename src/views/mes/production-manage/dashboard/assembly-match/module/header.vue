<template>
  <div class="head-container">
    <div v-show="crud.searchToggle">
      <factory-select v-model="query.factoryId" show-all class="filter-item" style="width: 200px" @change="crud.toQuery" />
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
        <div style="display: flex">
          <common-button type="primary" size="mini" @click="batchMatch">批量匹配</common-button>
          <el-checkbox
            v-model="checkAll"
            :indeterminate="isIndeterminate"
            border
            style="margin-left: 5px; margin-right: 10px"
            @change="handleCheckAllChange"
            >全选</el-checkbox
          >
          <color-card
            class="filter-item"
            v-model:value="query.processingStatus"
            :colors="colors"
            color-border
            select-able
            @change="crud.toQuery"
          />
        </div>
      </template>
      <template #viewLeft>
        <scale class="filter-item" v-model:value="boxScale" :intervals="400" @zoom-out="boxZoomOut" />
      </template>
    </crudOperation>
  </div>
</template>

<script setup>
import { ref, defineExpose, defineProps, defineEmits } from 'vue'

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

const emit = defineEmits(['load', 'checkedAll'])

const boxScale = ref(1)
const { colors, boxZoomOut, getColor } = useDashboardHeader({ colorCardTitles: ['不具备', '部分具备', '完全具备'], emit, crud })

CRUD.HOOK.handleRefresh = (crud, res) => {
  res.data.content = res.data.content.map((v) => {
    v.checked = false
    v.compareQuantity = crud.query.factoryId ? v.assignQuantity : v.quantity
    v.boxColor = getColor(v, { quantity: 'completedQuantity', compare: 'compareQuantity' })
    return v
  })
}

defineProps({
  isIndeterminate: {
    type: Boolean,
    default: false
  }
})
const checkAll = ref(false)
function handleCheckAllChange(val) {
  emit('checkedAll', val)
}
function batchMatch() {}

defineExpose({
  boxScale
})
</script>
