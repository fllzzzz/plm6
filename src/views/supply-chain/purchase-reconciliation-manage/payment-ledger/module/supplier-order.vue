<template>
  <div>
    <common-table :data="list" return-source-data :showEmptySymbol="false" :max-height="maxHeight">
      <el-table-column label="序号" type="index" align="center" width="60" />
      <el-table-column prop="supplierName" label="供应商" align="center" show-overflow-tooltip />
      <el-table-column prop="amount" label="累计合同额" align="center" show-overflow-tooltip>
        <template #default="{ row }">
          <span>{{ isNotBlank(row.amount)? toThousand(row.amount): 0 }}</span>
        </template>
      </el-table-column>
      <el-table-column prop="inboundAmount" label="累计入库额" align="center" show-overflow-tooltip>
        <template #default="{ row }">
          <span>{{ isNotBlank(row.inboundAmount)? toThousand(row.inboundAmount): 0 }}</span>
        </template>
      </el-table-column>
      <el-table-column prop="paymentAmount" label="累计已付款" align="center" show-overflow-tooltip>
        <template #default="{ row }">
          <span>{{ isNotBlank(row.paymentAmount)? toThousand(row.paymentAmount): 0 }}</span>
        </template>
      </el-table-column>
      <el-table-column prop="paymentRate" label="付款比例" align="center" show-overflow-tooltip>
        <template #default="{ row }">
          <span v-if="row.amount">{{ ((row.paymentAmount/row.amount)*100).toFixed(2) }}%</span>
        </template>
      </el-table-column>
      <el-table-column prop="invoiceAmount" label="累计已开票" align="center" show-overflow-tooltip>
        <template #default="{ row }">
          <span>{{ isNotBlank(row.invoiceAmount)? toThousand(row.invoiceAmount): 0 }}</span>
        </template>
      </el-table-column>
      <el-table-column prop="invoiceRate" label="开票比例" align="center" show-overflow-tooltip>
        <template #default="{ row }">
          <span>{{ ((row.invoiceAmount/row.amount)*100).toFixed(2) }}%</span>
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
import useMaxHeight from '@compos/use-max-height'
import usePagination from '@compos/use-pagination'
import { isNotBlank } from '@data-type/index'
import { toThousand } from '@data-type/number'

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
fetchList()
// 获取订单汇总
async function fetchList() {
  let _list = []
  tableLoading.value = true
  try {
    const { content = [], totalElements } = await getBySupplier({ ...props.query, ...queryPage })
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
