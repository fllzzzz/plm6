<template>
  <el-card v-loading="tableLoading" class="card-detail">
    <div>收款</div>
    <div>
      总金额 <span class="blue"> <span v-thousand="props.detailRow.collectionAmount || 0" /> 元</span>
    </div>
    <div class="print-wrap">
      <print-table v-permission="permission.printDetail" api-key="projectCollectionRecord" :params="params" size="mini" type="warning" />
    </div>
  </el-card>
  <common-table ref="tableRef" v-loading="tableLoading" :data-format="columnsDataFormat" :data="list" :max-height="props.maxHeight - 92">
    <el-table-column type="index" prop="index" label="序号" align="center" width="60px" />
        <el-table-column key="collectionDate" prop="collectionDate" label="收款日期" show-overflow-tooltip align="center" />
        <el-table-column key="collectionAmount" prop="collectionAmount" label="收款金额" show-overflow-tooltip align="right" />
        <el-table-column key="collectionReason" prop="collectionReason" label="收款事由" show-overflow-tooltip align="center">
          <template #default="{ row }">
            <span>{{ dict.label?.['payment_reason']?.[row.collectionReason] }}</span>
          </template>
        </el-table-column>
        <el-table-column key="collectionMode" prop="collectionMode" label="收款方式" show-overflow-tooltip align="center" />
        <el-table-column key="collectionUnit" prop="collectionUnit" label="收款单位" show-overflow-tooltip align="center" />
        <el-table-column key="collectionDepositBank" prop="collectionDepositBank" label="收款银行" show-overflow-tooltip align="center" />
        <el-table-column key="paymentUnit" prop="paymentUnit" label="付款单位" show-overflow-tooltip align="center" />
  </common-table>
</template>

<script setup>
import { getCollectionList } from '@/api/contract/fortune-report/fortune-report'
import { defineProps, nextTick, inject, ref, computed, watch } from 'vue'

import { paymentFineModeEnum } from '@enum-ms/finance'

import useDict from '@compos/store/use-dict'

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

const dict = useDict(['payment_reason'])
const permission = inject('permission')

const params = computed(() => {
  return {
    projectId: props.detailRow.id,
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

const tableLoading = ref(false)
const list = ref([])
const columnsDataFormat = ref([
  ['collectionAmount', 'to-thousand'],
  ['collectionDate', ['parse-time', '{y}-{m}-{d}']],
  ['collectionMode', ['parse-enum', paymentFineModeEnum]]
])

async function fetchList() {
  let _list = []
  try {
    tableLoading.value = true
    _list = (await getCollectionList(params.value)) || []
  } catch (error) {
    console.log('获取收款详情失败', error)
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
