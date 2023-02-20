<template>
  <div>
    <div class="head-container">
      <el-date-picker
        v-model="query.times"
        type="daterange"
        range-separator=":"
        size="small"
        class="date-item filter-item"
        value-format="x"
        start-placeholder="开始日期"
        end-placeholder="结束日期"
        style="width: 374px"
        @change="fetchList"
      />
      <common-radio-button
        class="filter-item"
        v-model="form.materialType"
        :options="materialPurchaseClsEnum.ENUM"
        type="enum"
        size="small"
        @change="fetchList"
      />
      <common-radio-button
        class="filter-item"
        v-model="query.purchaseCreationState"
        :options="requisitionStatusEnum.ENUM"
        show-option-all
        type="enum"
        size="small"
        @change="fetchList"
      />
    </div>
    <common-table
      ref="tableRef"
      v-loading="orderLoading"
      :data="orderList"
      style="width: 100%"
      @select="manualSelect"
      @select-all="manualSelectAll"
    >
      <el-table-column type="selection" width="50" align="center" class="selection" />
      <el-table-column label="序号" type="index" align="center" width="55" />
      <el-table-column label="申购单号" prop="serialNumber" align="center">
        <template #default="{ row: { sourceRow: row } }">
          <receipt-sn-clickable v-if="row?.id" :receipt-types="['REQUISITIONS']" :receipt="row" />
        </template>
      </el-table-column>
      <el-table-column label="状态" prop="purchaseCreationState" align="center" width="90">
        <template #default="{ row }">
          <el-tag v-if="row.purchaseCreationState" effect="plain" :type="requisitionStatusEnum.V[row.purchaseCreationState].T">{{
            requisitionStatusEnum.VL[row.purchaseCreationState]
          }}</el-tag>
        </template>
      </el-table-column>
    </common-table>
  </div>
</template>

<script setup>
import { getRequisitionBrief } from '@/api/supply-chain/requisitions-manage/requisitions'
import { defineExpose, ref, inject, nextTick } from 'vue'
import moment from 'moment'

import { isNotBlank, isBlank } from '@data-type/index'
import { requisitionStatusEnum } from '@enum-ms/wms'
import { materialPurchaseClsEnum } from '@enum-ms/classification'
import ReceiptSnClickable from '@/components-system/wms/receipt-sn-clickable'

const form = inject('crud')?.form

const tableRef = ref()
const orderLoading = ref(false)
const orderList = ref([])
const query = ref({
  times: [moment().startOf('month').valueOf(), moment().valueOf()],
  // materialType: materialPurchaseClsEnum.STEEL.V,
  purchaseCreationState: undefined
})

async function fetchList() {
  orderList.value = []
  try {
    orderLoading.value = true
    const { content } = await getRequisitionBrief({ filterUseInventory: true, materialType: form.materialType, ...query.value })
    const idx = []
    orderList.value = content.map((v, i) => {
      if (isNotBlank(form.requisitionsKV[v.id])) {
        idx.push(i)
      }
      return v
    })
    nextTick(() => {
      idx.forEach((i) => {
        tableRef.value?.toggleRowSelection(orderList.value[i], undefined)
      })
    })
  } catch (er) {
    console.log(er, '获取申购单列表失败')
  } finally {
    orderLoading.value = false
  }
}

function manualSelect(select, row) {
  const boolSelect = Boolean(select.findIndex((v) => v.id === row.id) !== -1)
  if (boolSelect && isBlank(form.requisitionsKV[row.id])) {
    form.requisitionsKV[row.id] = row
  }
  if (!boolSelect && isNotBlank(form.requisitionsKV[row.id])) {
    delete form.requisitionsKV[row.id]
  }
}

function manualSelectAll(select) {
  const boolSelect = Boolean(select?.length)
  orderList.value.forEach((v) => {
    if (boolSelect && isBlank(form.requisitionsKV[v.id])) {
      form.requisitionsKV[v.id] = v
    }
    if (!boolSelect && isNotBlank(form.requisitionsKV[v.id])) {
      delete form.requisitionsKV[v.id]
    }
  })
}

defineExpose({
  fetchList
})
</script>
