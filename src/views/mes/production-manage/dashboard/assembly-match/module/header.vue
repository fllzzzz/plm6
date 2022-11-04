<template>
  <div class="head-container">
    <div v-show="crud.searchToggle">
      <monomer-select-area-tabs
        :productType="componentTypeEnum.ARTIFACT.V"
        needConvert
        :project-id="projectId"
        @change="fetchMonomerAndArea"
      />
      <div>
        <tag-tabs
          v-model="query.classId"
          class="filter-item"
          :style="'width:calc(100% - 0px)'"
          :data="summaryList"
          :itemKey="'classId'"
          @change="tabChange"
        >
          <template #default="{ item }">
            <span>{{ item.name }}：</span>
            <span>{{ item.quantity }}件</span>
          </template>
        </tag-tabs>
      </div>
      <el-input
        v-model="query.serialNumber"
        size="small"
        placeholder="输入编号搜索"
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
          <color-card class="filter-item" v-model:value="query.status" :colors="colors" color-border @change="crud.toQuery" />
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

import { componentTypeEnum } from '@enum-ms/mes'
import { artifactInfo } from '@/api/mes/production-manage/dashboard/assembly-match'
import useDashboardHeader from '@compos/mes/dashboard/use-dashboard-header'
import { regHeader } from '@compos/use-crud'
import useGlobalProjectIdChangeToQuery from '@compos/use-global-project-id-change-to-query'
import crudOperation from '@crud/CRUD.operation'
import rrOperation from '@crud/RR.operation'
import ColorCard from '@comp/ColorCard'
import Scale from '@comp/Scale'
import tagTabs from '@comp-common/tag-tabs'
import factorySelect from '@comp-base/factory-select'
import monomerSelectAreaTabs from '@comp-base/monomer-select-area-tabs'

const summaryList = ref([])
const defaultQuery = {
  serialNumber: '',
  monomerId: { value: undefined, resetAble: false },
  areaId: { value: undefined, resetAble: false },
  status: { value: undefined, resetAble: false },
}
const { crud, query, CRUD } = regHeader(defaultQuery)
const projectId = useGlobalProjectIdChangeToQuery(crud)

const emit = defineEmits(['load', 'checkedAll', 'batchMatch', 'clear'])

const boxScale = ref(1)
const { colors, boxZoomOut, getColorByValue, getTagByValue } = useDashboardHeader({
  colorCardTitles: ['不具备', '部分具备', '完全具备'],
  emit,
  crud,
})

const checkAll = ref(false)
function handleCheckAllChange(val) {
  emit('checkedAll', val)
}
function batchMatch() {
  emit('batchMatch')
}

async function artifactInfoGet() {
  try {
    const data = await artifactInfo({
      projectId: query.projectId,
      monomerId: query.monomerId,
      areaId: query.areaId,
    })
    summaryList.value = data
  } catch (e) {
    console.log('获取区域下的构件汇总信息', e)
  }
}

CRUD.HOOK.handleRefresh = (crud, res) => {
  emit('clear')
  checkAll.value = false
  res.data.content = res.data.content.map((v) => {
    v.checked = false
    v.compareQuantity = crud.query.factoryId ? v.assignQuantity : v.quantity
    v.boxColor = getColorByValue(v, { field: 'type' })
    v.tagType = getTagByValue(v, { field: 'type' })
    return v
  })
}

function fetchMonomerAndArea({ monomerId, areaId }) {
  query.monomerId = monomerId
  query.areaId = areaId
  artifactInfoGet()
  crud.toQuery() 
}

defineProps({
  isIndeterminate: {
    type: Boolean,
    default: false,
  },
})

defineExpose({
  boxScale,
})
</script>
