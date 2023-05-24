<template>
  <el-card v-loading="tableLoading" class="amortization-detail">
    <div>{{ props.detailRow.name }}</div>
    <div>
      总金额 <span class="blue"> <span v-thousand="props.detailRow.amount || 0" /> 元</span>
    </div>
    <div>
      综合成本占比 <span class="blue">{{ props.detailRow.rate || 0 }} %</span>
    </div>
    <div class="print-wrap">
      <print-table v-permission="permission.printDetail" api-key="amortizationRecord" :params="params" size="mini" type="warning" />
    </div>
  </el-card>
  <common-table ref="tableRef" v-loading="tableLoading" :data-format="columnsDataFormat" :data="list" :max-height="maxHeight - 92">
    <el-table-column label="序号" type="index" align="center" width="60" />
    <el-table-column prop="date" key="date" show-overflow-tooltip label="摊销时间段" align="center">
      <template #default="{ row }">
        <span style="font-weight: bold">{{ row.date }}</span>
      </template>
    </el-table-column>
    <el-table-column key="amount" prop="amount" show-overflow-tooltip label="摊销金额" align="center" />
    <el-table-column key="productMete" prop="productMete" show-overflow-tooltip label="产量（吨）" align="center" />
    <el-table-column key="amortizationType" prop="amortizationType" show-overflow-tooltip label="摊销类型" align="center" />
    <el-table-column key="expenseRate" prop="expenseRate" show-overflow-tooltip label="摊销占比" align="center" />
    <el-table-column key="costRate" prop="costRate" show-overflow-tooltip label="综合成本占比" align="center" />
  </common-table>
</template>

<script setup>
import { getAmortizationList } from '@/api/contract/fortune-report/fortune-report'
import { defineProps, nextTick, inject, ref, computed, watch } from 'vue'

import { toFixed } from '@/utils/data-type'
import { amortizationTypeEnum, expenseClassEnum } from '@enum-ms/contract'

import moment from 'moment'

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
  const data = {
    projectId: props.detailRow.projectId,
    expenseClassEnum: props.detailRow.expenseClassEnum
  }
  // 这两种类型需要bizId
  if (data.expenseClassEnum === expenseClassEnum.MATERIAL_AUXILIARY.V || data.expenseClassEnum === expenseClassEnum.MATERIAL_OTHER.V) {
    data.bizId = props.detailRow.bizId
  }
  return data
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
const list = ref([])
const totalAmount = ref(0)
const columnsDataFormat = ref([
  ['amount', 'to-thousand'],
  ['costRate', ['suffix', ' %']],
  ['expenseRate', ['suffix', ' %']],
  ['amortizationType', ['parse-enum', amortizationTypeEnum]]
])

async function fetchList() {
  let _list = []
  totalAmount.value = 0
  try {
    tableLoading.value = true
    _list = (await getAmortizationList(params.value)) || []
    totalAmount.value = _list.reduce((pre, cur) => {
      if (cur) {
        return pre + Number(cur.amount)
      } else {
        return pre
      }
    }, 0)
    _list.forEach((row) => {
      const _startDate = moment(row.startDate).format('YYYY-MM-DD')
      const _endDate = moment(row.endDate).format('YYYY-MM-DD')
      row.date = `${_startDate} ~ ${_endDate}`
      row.costRate = toFixed((row.amount / props.detailRow.costAmount) * 100, 2)
      row.expenseRate = toFixed((row.amount / totalAmount.value) * 100, 2)
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
.amortization-detail {
  margin-bottom: 20px;
  ::v-deep(.el-card__body) {
    display: flex;
    align-items: center;
    > div:first-child {
      color: #706f6f;
      font-weight: bold;
      padding-right: 30px;
    }
    > div:not(:first-child, .print-wrap) {
      flex: 1;
      text-align: center;
      border-left: 1px solid #ebeef5;
      .blue {
        color: #0079ff;
        font-weight: bold;
      }
    }
  }
}
</style>
