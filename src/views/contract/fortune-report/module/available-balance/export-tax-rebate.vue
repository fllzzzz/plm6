<template>
  <el-card v-loading="tableLoading" class="card-detail">
    <div>出口退税</div>
    <div>
      总金额 <span class="blue"> <span v-thousand="props.detailRow.exportTaxRebate || 0" /> 元</span>
    </div>
    <div class="print-wrap">
      <print-table v-permission="permission.printDetail" api-key="exportTaxRebateList" :params="params" size="mini" type="warning" />
    </div>
  </el-card>
  <common-table ref="tableRef" v-loading="tableLoading" :data-format="columnsDataFormat" :data="list" :max-height="props.maxHeight - 92">
    <el-table-column type="index" prop="index" label="序号" align="center" width="60px" />
        <el-table-column key="drawbackDate" prop="drawbackDate" label="退税日期" show-overflow-tooltip align="center" />
        <el-table-column key="sendAmount" prop="sendAmount" label="发货额" show-overflow-tooltip align="right" />
        <el-table-column key="drawbackAmount" prop="drawbackAmount" label="退税总额" show-overflow-tooltip align="right" />
        <el-table-column key="accountantName" prop="accountantName" label="核算人" show-overflow-tooltip align="center" />
        <el-table-column key="agentName" prop="agentName" label="办理人" show-overflow-tooltip align="center" />
        <el-table-column key="auditorName" prop="auditorName" label="审核人" show-overflow-tooltip align="center" />
        <el-table-column key="remark" prop="remark" label="备注" show-overflow-tooltip align="left" />
  </common-table>
</template>

<script setup>
import { get } from '@/api/contract/export-tax-rebate'
import { defineProps, nextTick, inject, ref, computed, watch } from 'vue'

import { auditTypeEnum } from '@enum-ms/contract'

const props = defineProps({
  detailRow: {
    type: Object,
    default: () => {}
  },
  maxHeight: {
    type: Number,
    default: 400
  },
  secondPickerTime: {
    type: Object,
    default: () => {}
  }
})

const params = computed(() => {
  return {
    projectId: props.detailRow.id,
    auditStatus: auditTypeEnum.PASS.V,
    secondStartDate: props.secondPickerTime.startDate,
    secondEndDate: props.secondPickerTime.endDate
  }
})

watch(
  () => params.value,
  (data) => {
    if (data.projectId) {
      nextTick(() => {
        fetchList()
      })
    }
  },
  { deep: true, immediate: true }
)

const permission = inject('permission')
const tableLoading = ref(false)
const list = ref([])
const columnsDataFormat = ref([
  ['sendAmount', 'to-thousand'],
  ['drawbackAmount', 'to-thousand'],
  ['drawbackDate', ['parse-time', '{y}-{m}-{d}']]
])

async function fetchList() {
  let _list = []
  try {
    tableLoading.value = true
    _list = (await get(params.value)) || []
  } catch (error) {
    console.log('获取出口退税失败', error)
  } finally {
    list.value = _list
    tableLoading.value = false
  }
}
</script>

<style lang="scss" scoped>
.card-detail {
  margin-bottom: 20px;
  ::v-deep(.el-card__body) {
    line-height: 29px;
    position: relative;
    padding-right: 334px;
    > div:first-child {
      color: #706f6f;
      font-weight: bold;
      padding-right: 20px;
      display: inline-block;
      vertical-align: middle;
    }
    > div:not(:first-child, .print-wrap) {
      padding: 0 20px;
      display: inline-block;
      vertical-align: middle;
      border-left: 1px solid #ebeef5;
      .blue {
        color: #0079ff;
        font-weight: bold;
      }
    }
    .print-wrap {
      position: absolute;
      height: 29px;
      top: 0;
      right: 20px;
      bottom: 0;
      margin: auto;
    }
  }
}
</style>
