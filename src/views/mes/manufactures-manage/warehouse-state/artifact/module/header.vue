<template>
  <div class="head-container">
    <div v-show="crud.searchToggle">
      <monomer-select-area-tabs :project-id="globalProjectId" :productType="productType" needConvert @change="fetchMonomerAndArea" />
      <factory-select v-model="query.factoryId" clearable class="filter-item" style="width: 200px" @change="crud.toQuery" />
      <product-type-query :productType="productType" :toQuery="crud.toQuery" :query="query" />
      <rrOperation />
    </div>
    <crudOperation>
      <template #optLeft>
        <print-table
          v-permission="crud.permission.print"
          api-key="mesWarehouseStateStructure"
          :params="{ ...query }"
          size="mini"
          type="warning"
          class="filter-item"
        />
      </template>
      <template #viewLeft>
        <span v-permission="crud.permission.get">
          <el-tag effect="plain" size="medium" class="filter-item">
            <span>清单量：</span>
            <span v-if="!summaryLoading">{{ summaryInfo.quantity }} 件 | {{ toFixed(summaryInfo.mete, DP.COM_WT__KG) }} kg</span>
            <i v-else class="el-icon-loading" />
          </el-tag>
          <el-tag effect="plain" size="medium" class="filter-item">
            <span>任务量：</span>
            <span v-if="!summaryLoading">{{ summaryInfo.taskQuantity }} 件 | {{ toFixed(summaryInfo.taskMete, DP.COM_WT__KG) }} kg</span>
            <i v-else class="el-icon-loading" />
          </el-tag>
          <el-tag effect="plain" size="medium" class="filter-item">
            <span>入库量：</span>
            <span v-if="!summaryLoading">
              {{ summaryInfo.inboundQuantity }} 件 | {{ toFixed(summaryInfo.inboundMete, DP.COM_WT__KG) }} kg
            </span>
            <i v-else class="el-icon-loading" />
          </el-tag>
          <el-tag effect="plain" size="medium" class="filter-item">
            <span>出库量：</span>
            <span v-if="!summaryLoading">
              {{ summaryInfo.outboundQuantity }} 件 | {{ toFixed(summaryInfo.outboundMete, DP.COM_WT__KG) }} kg
            </span>
            <i v-else class="el-icon-loading" />
          </el-tag>
          <el-tag effect="plain" size="medium" class="filter-item" type="success">
            <span>库存量：</span>
            <span v-if="!summaryLoading">{{ summaryInfo.stockQuantity }} 件 | {{ toFixed(summaryInfo.stockMete, DP.COM_WT__KG) }} kg</span>
            <i v-else class="el-icon-loading" />
          </el-tag>
        </span>
      </template>
    </crudOperation>
  </div>
</template>

<script setup>
import { getBoardForArtifactSummary as getSummary } from '@/api/mes/manufactures-manage/common'
import { ref, watch } from 'vue'

import { componentTypeEnum } from '@enum-ms/mes'
import { DP } from '@/settings/config'
import { toFixed } from '@data-type'
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

const productType = componentTypeEnum.ARTIFACT.V
const { globalProjectId } = mapGetters(['globalProjectId'])
const summaryInfo = ref({
  quantity: 0,
  taskQuantity: 0,
  inboundQuantity: 0,
  outboundQuantity: 0,
  stockQuantity: 0,
  mete: 0,
  taskMete: 0,
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
    v.weight = v.netWeight || 0
    v.totalWeight = v.weight * v.quantity
    v.stockQuantity = v.inboundQuantity - v.outboundQuantity || 0
    v.inboundWeight = v.inboundQuantity * v.weight
    v.outboundWeight = v.outboundQuantity * v.weight
    v.stockWeight = v.stockQuantity * v.weight
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
      taskQuantity = 0,
      inboundQuantity = 0,
      outboundQuantity = 0,
      stockQuantity = 0,
      mete = 0,
      taskMete = 0,
      outboundMete = 0,
      inboundMete = 0,
      stockMete = 0
    } = await getSummary(params)
    summaryInfo.value = {
      quantity,
      taskQuantity,
      inboundQuantity,
      outboundQuantity,
      stockQuantity,
      mete,
      taskMete,
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
