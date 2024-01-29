<template>
  <div>
    <common-table :data="tableData">
      <el-table-column label="序号" align="center" width="60" type="index"></el-table-column>
      <el-table-column label="购买方" align="center" prop="purchaser" width="220"></el-table-column>
      <el-table-column label="出售方" align="center" prop="branchCompanyName" width="220"></el-table-column>
      <el-table-column label="累计出售金额" align="center" prop="totalAmount">
        <template #default="{ row }">
          <div @click="openTotalDetaile(row)" style="cursor: pointer">
            <!-- <span>
              <el-badge>
                <svg-icon icon-class="notify"  style="color:#e6a23c;font-size:15px;"/>
              </el-badge>
            </span> -->
            <span style="color: #409eff; text-align: right; margin-left: 8px">{{ row.totalAmount }}</span>
          </div>
        </template>
      </el-table-column>
      <el-table-column label="累计收款" align="center" prop="collectionAmount">
        <template #default="{ row }">
          <div @click="openDetaile(row, 'collection')" style="cursor: pointer">
            <span v-if="row.unCheckCollectionCount !== 0">
              <el-badge :value="row.unCheckCollectionCount">
                <svg-icon icon-class="notify" style="color: #e6a23c; font-size: 15px" />
              </el-badge>
            </span>
            <span style="color: #409eff; text-align: right; margin-left: 8px">{{ row.collectionAmount }}</span>
          </div>
        </template>
      </el-table-column>
      <el-table-column label="收款率" align="center" prop="collectionRate"></el-table-column>
      <el-table-column label="累计开票" align="center" prop="invoiceAmount">
        <template #default="{ row }">
          <div @click="openDetaile(row, 'invoice')" style="cursor: pointer">
            <span v-if="row.unCheckInvoiceCount !== 0">
              <el-badge :value="row.unCheckInvoiceCount" :max="99" :hidden="row.unCheckInvoiceCount < 1">
                <svg-icon icon-class="notify" style="color: #e6a23c; font-size: 15px" />
              </el-badge>
            </span>
            <span style="color: #409eff; text-align: right; margin-left: 8px">{{ row.invoiceAmount }}</span>
          </div>
        </template>
      </el-table-column>
      <el-table-column label="开票率" align="center" prop="invoiceRate"></el-table-column>
    </common-table>
    <!-- 分页组件 -->
    <el-pagination
      :total="total"
      :current-page="queryPage.pageNumber"
      :page-size="queryPage.pageSize"
      layout="total, prev, pager, next, sizes"
      @size-change="handleSizeChange"
      @current-change="handleCurrentChange"
    />
    <collectionAndInvoice v-model="drawerVisible" :tabName="tabName" :currentRow="currentRow" />
    <totalAmount v-model="amountDetaile" :currentRow="currentRow" />
  </div>
</template>
<script setup>
import { defineProps, ref, watch } from 'vue'
import { getBuyList } from '@/api/contract/scrap-ledger'
import { scrapLedgerPM as permission } from '@/page-permission/contract'
import checkPermission from '@/utils/system/check-permission'
import usePagination from '@compos/use-pagination'
import collectionAndInvoice from './collection-and-invoice'
import totalAmount from './total-amount.vue'

const { handleSizeChange, handleCurrentChange, total, setTotalPage, queryPage } = usePagination({ fetchHook: fetchList })

const props = defineProps({
  queryId: {
    type: Number,
    default: 1
  },
  scrapType: {
    type: Number,
    default: undefined
  },
  startDate: {
    type: String,
    default: undefined
  },
  endDate: {
    type: String,
    default: undefined
  }
})

const tableData = ref([])
const drawerVisible = ref(false)
const currentRow = ref({})
const tabName = ref()
const amountDetaile = ref(false)

watch(
  () => props.queryId,
  (val) => {
    if (val === 1) {
      fetchList()
    }
  },
  { immediate: true }
)

watch(
  () => [props.startDate, props.endDate],
  () => {
    fetchList()
  }
)

watch(
  () => props.scrapType,
  () => {
    fetchList()
  }
)

async function fetchList() {
  if (!checkPermission(permission.get)) return
  tableData.value = []
  try {
    console.log(props.date)
    console.log(props.queryId)
    const { content, totalElements } = await getBuyList({
      ...queryPage,
      startDate: props.startDate,
      endDate: props.endDate,
      wasteClassificationId: props.scrapType
    })
    console.log(content)
    content.forEach((v) => {
      v.collectionRate = v.collectionRate * 100 + '%'
      v.invoiceRate = v.invoiceRate * 100 + '%'
      tableData.value.push(v)
    })
    setTotalPage(totalElements)
  } catch (error) {
    console.log(error)
  }
}

async function openDetaile(row, name) {
  drawerVisible.value = true
  currentRow.value = row
  tabName.value = name
}

async function openTotalDetaile(row) {
  amountDetaile.value = true
  currentRow.value = row
}
</script>

<style lang="scss" scoped>
::v-deep(.el-badge__content.is-fixed) {
  top: 5px;
  padding: 0 3px;
  line-height: 12px;
  height: 14px;
}
</style>
