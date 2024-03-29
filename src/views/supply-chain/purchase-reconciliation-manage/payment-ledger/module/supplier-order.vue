<template>
  <div>
    <common-table :data="list" return-source-data :showEmptySymbol="false" :max-height="maxHeight">
      <el-table-column label="序号" type="index" align="center" width="60" />
      <el-table-column prop="supplierName" label="供应商" align="center" show-overflow-tooltip />
      <el-table-column prop="amount" label="累计合同额" align="right" show-overflow-tooltip>
        <template #default="{ row }">
          <span>{{ isNotBlank(row.amount)? toThousand(row.amount,decimalPrecision.supplyChain): 0 }}</span>
        </template>
      </el-table-column>
      <el-table-column prop="paymentAmount" key="paymentAmount" label="付款额" align="right" min-width="120" show-overflow-tooltip>
        <template v-if="checkPermission(props.permission.detail)" #header>
          <el-tooltip
            effect="light"
            placement="top"
            content="点击行可以查看详情"
          >
            <div style="display: inline-block">
              <span>累计入库额 </span>
              <i class="el-icon-info" />
            </div>
          </el-tooltip>
        </template>
        <template #default="{ row }">
          <div type="warning" class="clickable" @click.stop="openRecord(row, 'payment')">{{ row.paymentAmount }}</div>
        </template>
      </el-table-column>
      <el-table-column prop="inboundAmount" label="累计入库额" align="right" show-overflow-tooltip>
        <template #default="{ row }">
          <span>{{ isNotBlank(row.inboundAmount)? toThousand(row.inboundAmount,DP.YUAN): 0 }}</span>
        </template>
      </el-table-column>
      <el-table-column prop="paymentAmount" label="累计已付款" align="right" show-overflow-tooltip>
        <template #default="{ row }">
          <span>{{ isNotBlank(row.paymentAmount)? toThousand(row.paymentAmount,decimalPrecision.supplyChain): 0 }}</span>
        </template>
      </el-table-column>
      <el-table-column prop="paymentRate" label="付款比例" align="center" show-overflow-tooltip>
        <template #default="{ row }">
          <span v-if="row.inboundAmount">{{ ((row.paymentAmount/row.inboundAmount)*100).toFixed(2) }}%</span>
        </template>
      </el-table-column>
      <el-table-column prop="invoiceAmount" label="累计已开票" align="right" show-overflow-tooltip>
        <template #default="{ row }">
          <span>{{ isNotBlank(row.invoiceAmount)? toThousand(row.invoiceAmount,decimalPrecision.supplyChain): 0 }}</span>
        </template>
      </el-table-column>
      <el-table-column prop="invoiceRate" label="开票比例" align="center" show-overflow-tooltip>
        <template #default="{ row }">
          <span v-if="row.inboundAmount">{{ ((row.invoiceAmount/row.inboundAmount)*100).toFixed(2) }}%</span>
        </template>
      </el-table-column>
    </common-table>
    <!--分页组件-->
    <el-pagination
      :total="total"
      :current-page="queryPage.pageNumber"
      :page-size="queryPage.pageSize"
      style="margin-top: 8px"
      layout="total, prev, pager, next, sizes"
      @size-change="handleSizeChange"
      @current-change="handleCurrentChange"
    />
  </div>
</template>

<script setup>
import { getBySupplier } from '@/api/supply-chain/purchase-reconciliation-manage/payment-ledger'
import { ref, defineProps, watch } from 'vue'

import checkPermission from '@/utils/system/check-permission'
import { isNotBlank } from '@data-type/index'
import { toThousand } from '@data-type/number'
import { DP } from '@/settings/config'

import useMaxHeight from '@compos/use-max-height'
import usePagination from '@compos/use-pagination'
import useDecimalPrecision from '@compos/store/use-decimal-precision'

const { decimalPrecision } = useDecimalPrecision()

const props = defineProps({
  modelValue: {
    type: Boolean,
    require: true
  },
  query: {
    type: Object,
    default: () => {}
  },
  permission: {
    type: Object,
    default: () => {}
  }
})

const { handleSizeChange, handleCurrentChange, total, setTotalPage, queryPage } = usePagination({ fetchHook: fetchList })

watch(
  props.query,
  (val) => {
    if (val) {
      fetchList()
    }
  }
)

const list = ref([])
const tableLoading = ref(false)

const { maxHeight } = useMaxHeight({
  wrapperBox: '.supplier-order',
  paginate: true,
  extraHeight: 60
})

// 打开记录
function openRecord(row, type) {
  if (!checkPermission(props.permission.detail)) return
}

fetchList()
// 获取订单汇总
async function fetchList() {
  let _list = []
  tableLoading.value = true
  try {
    const { content = [], totalElements } = await getBySupplier({ ...props.query, ...queryPage })
    console.log('content: ', content, totalElements)
    _list = content
    setTotalPage(totalElements)
  } catch (error) {
    console.log('获取订单汇总失败', error)
  } finally {
    list.value = _list
    tableLoading.value = false
  }
}
</script>
