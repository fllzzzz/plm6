<template>
  <div class="head-container">
    <div v-show="crud.searchToggle">
      <factory-select
        v-model:value="query.factoryId"
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
    <crudOperation>
      <!-- <template v-slot:optRight>
        <export-button
          v-permission="permission.download"
          :params="{ monomerId: query.monomerId, factoryId: query.factoryId }"
          :fn="download"
          show-btn-text
          btn-text="下载单体出入库详情"
          class="filter-item"
        />
        <print-table
          v-permission="permission.print"
          api-key="STEEL_MES_WAREHOUSE_STATE_STRUCTURE"
          :params="{ monomerId: query.monomerId, factoryId: query.factoryId }"
          size="mini"
          type="warning"
          class="filter-item"
        />
      </template> -->
      <template #viewLeft>
        <span v-permission="crud.permission.get">
          <el-tag effect="plain" class="filter-item">
            <span>{{ query.factoryId ? '任务量' : '清单量' }}：</span>
            <span v-if="!summaryLoading">{{ summaryInfo.quantity }} 件 | {{ toFixed(summaryInfo.mete, DP.COM_WT__KG) }} kg</span>
            <i v-else class="el-icon-loading" />
          </el-tag>
          <el-tag effect="plain" class="filter-item">
            <span>入库量：</span>
            <span
v-if="!summaryLoading"
              >{{ summaryInfo.intWarehouseQuantity }} 件 | {{ toFixed(summaryInfo.inboundMete, DP.COM_WT__KG) }} kg</span
            >
            <i v-else class="el-icon-loading" />
          </el-tag>
          <el-tag effect="plain" class="filter-item">
            <span>出库量：</span>
            <span
v-if="!summaryLoading"
              >{{ summaryInfo.outWarehouseQuantity }} 件 | {{ toFixed(summaryInfo.outboundMete, DP.COM_WT__KG) }} kg</span
            >
            <i v-else class="el-icon-loading" />
          </el-tag>
          <el-tag effect="plain" class="filter-item" type="success">
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
import { getSummary } from '@/api/mes/manufactures-manage/warehouse/artifact'
import { ref, watch, reactive } from 'vue'

import { DP } from '@/settings/config'
import { toFixed } from '@data-type'
import checkPermission from '@/utils/system/check-permission'

import { regHeader } from '@compos/use-crud'
import crudOperation from '@crud/CRUD.operation'
import rrOperation from '@crud/RR.operation'
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

let summaryInfo = reactive({
  quantity: 0,
  intWarehouseQuantity: 0,
  outWarehouseQuantity: 0,
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
    v.weight = v.grossWeight || 0
    v.totalWeight = v.weight * v.quantity
    v.stockQuantity = v.intWarehouseQuantity - v.outWarehouseQuantity || 0
    v.inboundWeight = v.intWarehouseQuantity * v.weight
    v.outboundWeight = v.outWarehouseQuantity * v.weight
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
      intWarehouseQuantity = 0,
      outWarehouseQuantity = 0,
      stockQuantity = 0,
      mete = 0,
      outboundMete = 0,
      inboundMete = 0,
      stockMete = 0
    } = await getSummary(params)
    summaryInfo = {
      quantity,
      intWarehouseQuantity,
      outWarehouseQuantity,
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
</script>
