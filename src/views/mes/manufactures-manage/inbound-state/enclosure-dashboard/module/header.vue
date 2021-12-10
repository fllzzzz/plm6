<template>
  <div class="head-container">
    <div v-show="crud.searchToggle">
      <monomer-select-area-tabs :project-id="globalProjectId" @change="fetchMonomerAndArea" />
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
        v-model="query.plate"
        size="small"
        placeholder="输入板型搜索"
        style="width: 170px"
        class="filter-item"
        clearable
        @keyup.enter="crud.toQuery"
      />
      <el-input
        v-model="query.color"
        size="small"
        placeholder="输入颜色搜索"
        style="width: 170px"
        class="filter-item"
        clearable
        @keyup.enter="crud.toQuery"
      />
      <rrOperation />
    </div>
    <crudOperation :show-grid="false" :show-refresh="false">
      <template #optRight>
        <color-card class="filter-item" v-model:value="query.status" :colors="colors" color-border select-able @change="crud.toQuery" />
      </template>
      <template #viewLeft>
        <span v-permission="crud.permission.get">
          <el-tag effect="plain" class="filter-item">
            <span>{{ query.factoryId ? '任务量' : '清单量' }}：</span>
            <span v-if="!summaryLoading">{{ toFixed(summaryInfo.mete, DP.MES_ENCLOSURE_L__M) }} kg</span>
            <i v-else class="el-icon-loading" />
          </el-tag>
          <el-tag effect="plain" class="filter-item">
            <span>入库量：</span>
            <span v-if="!summaryLoading">{{ toFixed(summaryInfo.inboundMete, DP.MES_ENCLOSURE_L__M) }} kg</span>
            <i v-else class="el-icon-loading" />
          </el-tag>
          <el-tag effect="plain" class="filter-item" type="success">
            <span>完成率：</span>
            <span v-if="!summaryLoading">{{ summaryInfo.inboundRate }}</span>
            <i v-else class="el-icon-loading" />
          </el-tag>
        </span>
        <scale class="filter-item" v-model:value="boxScale" :intervals="400" @zoom-out="boxZoomOut" />
      </template>
    </crudOperation>
  </div>
</template>

<script setup>
import { getSummaryForEnclosure as getSummary } from '@/api/mes/manufactures-manage/inbound'
import { ref, defineExpose, reactive, defineEmits } from 'vue'

import { DP } from '@/settings/config'
import { toFixed } from '@data-type'
import { mapGetters } from '@/store/lib'

import useDashboardHeader from '@compos/mes/dashboard/use-dashboard-header'
import { regHeader } from '@compos/use-crud'
import crudOperation from '@crud/CRUD.operation'
import rrOperation from '@crud/RR.operation'
import ColorCard from '@comp/ColorCard'
import Scale from '@comp/Scale'
import monomerSelectAreaTabs from '@comp-base/monomer-select-area-tabs'
import factorySelect from '@comp-base/factory-select'

const defaultQuery = {
  name: '',
  serialNumber: '',
  plate: '',
  color: '',
  status: { value: undefined, resetAble: false },
  monomerId: { value: undefined, resetAble: false },
  areaId: { value: undefined, resetAble: false },
  factoryId: { value: undefined, resetAble: false }
}
const { crud, query, CRUD } = regHeader(defaultQuery)

const { globalProjectId } = mapGetters(['globalProjectId'])
const emit = defineEmits('load')

const boxScale = ref(1)
const summaryLoading = ref(false)
let summaryInfo = reactive({
  mete: undefined,
  inboundMete: undefined,
  inboundRate: undefined
})

const { colors, boxZoomOut, getColor } = useDashboardHeader({
  colorCardTitles: ['未入库', '部分入库', '全部入库'],
  emit,
  crud,
  fetchSummaryInfo
})

async function fetchSummaryInfo() {
  if (!query.monomerId) {
    return
  }
  summaryLoading.value = true
  try {
    const params = {
      monomerId: query.monomerId,
      factoryId: query.factoryId
    }
    const { mete = 0, inboundMete = 0 } = await getSummary(params)
    summaryInfo = {
      mete,
      inboundMete,
      inboundRate: mete ? ((inboundMete / mete) * 100).toFixed(1) + '%' : 0
    }
  } catch (error) {
    console.log('获取汇总数据', error)
  } finally {
    summaryLoading.value = false
  }
}

CRUD.HOOK.handleRefresh = (crud, res) => {
  res.data.content = res.data.content.map((v) => {
    v.compareQuantity = crud.query.factoryId ? v.taskQuantity : v.quantity
    v.quantityInfo = `-------------------------\n\n清单数量：${v.quantity}\n\n`
    if (crud.query.factoryId) {
      v.quantityInfo += `任务数量：${v.taskQuantity}\n\n`
    }
    v.quantityInfo += `入库数量：${v.inboundQuantity}\n
        出库数量：${v.outboundQuantity}\n
        库存数量：${v.inboundQuantity - v.outboundQuantity}\n`
    v.boxColor = getColor(v, { quantity: 'inboundQuantity', compare: 'compareQuantity' })
    return v
  })
}

function fetchMonomerAndArea({ monomerId, areaId }) {
  query.monomerId = monomerId
  query.areaId = areaId
  crud.toQuery()
}

defineExpose({
  boxScale
})
</script>
