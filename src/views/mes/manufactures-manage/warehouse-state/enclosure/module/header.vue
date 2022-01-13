<template>
  <div class="head-container">
    <div v-show="crud.searchToggle">
      <monomer-select-area-tabs :project-id="globalProjectId" :productType="productType" needConvert @change="fetchMonomerAndArea" />
      <factory-select v-model="query.factoryId" clearable class="filter-item" style="width: 200px" @change="crud.toQuery" />
      <product-type-query :productType="productType" :toQuery="crud.toQuery" :query="query" />
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
    <crudOperation>
      <!-- <template v-slot:optRight>
           <export-button
          v-permission="permission.download"
          :params="{monomerId: query.monomerId, factoryId: query.factoryId}"
          :fn="download"
          show-btn-text
          btn-text="下载单体出入库详情"
          class="filter-item"
        />
        <print-table
          v-permission="permission.print"
          api-key="STEEL_MES_WAREHOUSE_STATE_ENCLOSURE"
          :params="{monomerId: query.monomerId, factoryId: query.factoryId}"
          size="mini"
          type="warning"
          class="filter-item"
        />
      </template> -->
      <template #viewLeft>
        <span v-permission="crud.permission.get">
          <el-tag effect="plain" class="filter-item">
            <span>{{ query.factoryId ? '任务量' : '清单量' }}：</span>
            <span
v-if="!summaryLoading"
              >{{ summaryInfo.quantity }} 张 | {{ convertUnits(summaryInfo.mete, 'mm', 'm', DP.MES_ENCLOSURE_L__M, true) }} m</span
            >
            <i v-else class="el-icon-loading" />
          </el-tag>
          <el-tag effect="plain" class="filter-item">
            <span>入库量：</span>
            <span
v-if="!summaryLoading"
              >{{ summaryInfo.inboundQuantity }} 张 |
              {{ convertUnits(summaryInfo.inboundMete, 'mm', 'm', DP.MES_ENCLOSURE_L__M, true) }} m</span
            >
            <i v-else class="el-icon-loading" />
          </el-tag>
          <el-tag effect="plain" class="filter-item">
            <span>出库量：</span>
            <span
v-if="!summaryLoading"
              >{{ summaryInfo.outboundQuantity }} 张 |
              {{ convertUnits(summaryInfo.outboundMete, 'mm', 'm', DP.MES_ENCLOSURE_L__M, true) }} m</span
            >
            <i v-else class="el-icon-loading" />
          </el-tag>
          <el-tag effect="plain" class="filter-item" type="success">
            <span>库存量：</span>
            <span
v-if="!summaryLoading"
              >{{ summaryInfo.stockQuantity }} 张 |
              {{ convertUnits(summaryInfo.stockMete, 'mm', 'm', DP.MES_ENCLOSURE_L__M, true) }} m</span
            >
            <i v-else class="el-icon-loading" />
          </el-tag>
        </span>
      </template>
    </crudOperation>
  </div>
</template>

<script setup>
import { getBoardForEnclosureSummary as getSummary } from '@/api/mes/manufactures-manage/common'
import { ref, watch } from 'vue'

import { componentTypeEnum } from '@enum-ms/mes'
import { DP } from '@/settings/config'
import { convertUnits } from '@/utils/convert/unit'
import { mapGetters } from '@/store/lib'
import checkPermission from '@/utils/system/check-permission'

import { regHeader } from '@compos/use-crud'
import crudOperation from '@crud/CRUD.operation'
import rrOperation from '@crud/RR.operation'
import productTypeQuery from '@comp-mes/header-query/product-type-query'
import monomerSelectAreaTabs from '@comp-base/monomer-select-area-tabs'
import factorySelect from '@comp-base/factory-select'

const defaultQuery = {
  name: '',
  serialNumber: '',
  specification: '',
  material: '',
  factoryId: { value: undefined, resetAble: false },
  monomerId: { value: undefined, resetAble: false },
  areaId: { value: undefined, resetAble: false }
}

const { crud, query, CRUD } = regHeader(defaultQuery)

const productType = componentTypeEnum.ENCLOSURE.V
const { globalProjectId } = mapGetters(['globalProjectId'])
const summaryInfo = ref({
  quantity: 0,
  inboundQuantity: 0,
  outboundQuantity: 0,
  stockQuantity: 0,
  mete: 0,
  inboundMete: 0,
  outboundMete: 0,
  stockMete: 0
})
const summaryLoading = ref(false)

watch(
  [() => crud.query.monomerId, () => crud.query.factoryId],
  () => {
    fetchSummaryInfo()
  },
  { immediate: true }
)

CRUD.HOOK.handleRefresh = (crud, res) => {
  res.data.content = res.data.content.map((v) => {
    v.length = v.length || 0
    v.totalLength = v.length * v.quantity
    v.stockQuantity = v.inboundQuantity - v.outboundQuantity || 0
    v.inboundLength = v.inboundQuantity * v.length
    v.outboundLength = v.outboundQuantity * v.length
    v.stockLength = v.stockQuantity * v.length
    return v
  })
}

async function fetchSummaryInfo() {
  if (!checkPermission(crud.permission.get) || !query.monomerId) {
    return
  }
  summaryLoading.value = true
  try {
    const params = {
      monomerId: query.monomerId,
      factoryId: query.factoryId
    }
    const {
      quantity = 0,
      inboundQuantity = 0,
      outboundQuantity = 0,
      stockQuantity = 0,
      mete = 0,
      outboundMete = 0,
      inboundMete = 0,
      stockMete = 0
    } = await getSummary(params)
    summaryInfo.value = {
      quantity,
      inboundQuantity,
      outboundQuantity,
      stockQuantity,
      mete,
      inboundMete,
      outboundMete,
      stockMete
    }
  } catch (error) {
    console.log('获取汇总数据', error)
  } finally {
    summaryLoading.value = false
  }
}

function fetchMonomerAndArea({ monomerId, areaId }) {
  query.monomerId = monomerId
  query.areaId = areaId
  crud.toQuery()
}
</script>
