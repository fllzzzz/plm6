<!-- 基础类型 -->
<template>
  <el-card v-loading="tableLoading" class="card-detail">
    <div>{{ props.detailRow.name }}</div>
    <div>
      总金额 <span class="blue"> <span v-thousand="props.detailRow.amount || 0" /> 元</span>
    </div>
    <div v-if="props.detailRow.showRate">
      综合成本占比 <span class="blue">{{ props.detailRow.rate || 0 }} %</span>
    </div>
    <div class="print-wrap">
      <print-table v-permission="permission.printDetail" api-key="expenseReimburseSummary" :params="params" size="mini" type="warning" />
    </div>
  </el-card>
  <common-table
    ref="tableRef"
    v-loading="tableLoading"
    :data-format="columnsDataFormat"
    :data="list"
    :max-height="maxHeight - 92"
    @row-click="currentChange"
  >
    <el-table-column type="index" prop="index" label="序号" align="center" width="60px" />
    <el-table-column key="expenseSubjectName" prop="expenseSubjectName" label="项目" show-overflow-tooltip align="center" />
    <el-table-column key="reimburseAmount" prop="reimburseAmount" label="金额（元）" show-overflow-tooltip align="center" />
    <el-table-column key="rate" prop="rate" :label="`${props.detailRow.name}占比`" show-overflow-tooltip align="center" />
    <el-table-column
      key="costAscriptionRate"
      prop="costAscriptionRate"
      show-overflow-tooltip
      :label="`${props.detailRow.costAscriptionName}占比`"
      align="center"
    />
    <el-table-column
      v-if="props.detailRow.showRate"
      key="costRate"
      prop="costRate"
      show-overflow-tooltip
      label="综合成本占比"
      align="center"
    />
  </common-table>
  <cost-detail v-model="detailVisible" :detail-row="currentRow" />
</template>

<script setup>
import { getExpenseReimburse } from '@/api/contract/fortune-report/fortune-report'
import { defineProps, nextTick, inject, ref, computed, watch } from 'vue'

import { toFixed } from '@/utils/data-type'

import costDetail from './cost-detail'

const props = defineProps({
  detailRow: {
    type: Object,
    default: () => {}
  },
  maxHeight: {
    type: Number,
    default: 400
  }
})

const permission = inject('permission')

const params = computed(() => {
  return {
    projectId: props.detailRow.projectId,
    costAscriptionEnum: props.detailRow.costAscriptionEnum,
    expenseTypeId: props.detailRow.bizId
  }
})

watch(
  () => props.detailRow,
  (detail) => {
    if (detail.projectId) {
      nextTick(() => {
        fetchList()
      })
    }
  },
  { deep: true, immediate: true }
)

const tableLoading = ref(false)
const currentRow = ref({})
const detailVisible = ref(false)
const list = ref([])
const columnsDataFormat = ref([
  ['reimburseAmount', 'to-thousand'],
  ['rate', ['suffix', ' %']],
  ['costRate', ['suffix', ' %']],
  ['costAscriptionRate', ['suffix', ' %']]
])

function currentChange(row = {}) {
  currentRow.value = { ...params.value, ...row }
  detailVisible.value = true
}

async function fetchList() {
  let _list = []
  try {
    tableLoading.value = true
    _list = (await getExpenseReimburse(params.value)) || []
    _list.forEach((row) => {
      row.rate = toFixed((row.reimburseAmount / props.detailRow.amount) * 100, 2)
      row.costAscriptionRate = toFixed((row.reimburseAmount / props.detailRow.costAscriptionAmount) * 100, 2)
      row.costRate = toFixed((row.reimburseAmount / props.detailRow.costAmount) * 100, 2)
      row.costAscriptionName = props.detailRow.costAscriptionName
      row.showRate = props.detailRow.showRate
    })
  } catch (error) {
    console.log('获取摊销详情失败')
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
