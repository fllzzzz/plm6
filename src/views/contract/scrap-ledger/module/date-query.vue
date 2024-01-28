<template>
  <div>
    <common-table :data="tableData" :data-format="dataFormat">
      <el-table-column label="序号" align="center" width="60" type="index"></el-table-column>
      <el-table-column label="合同编号" align="center" prop="serialNumber"></el-table-column>
      <el-table-column label="购买单位" align="center" prop="purchaser"></el-table-column>
      <el-table-column label="废料类型" align="center" prop="wasteClassificationName"></el-table-column>
      <el-table-column label="核算单位" align="center" prop="measureUnit"></el-table-column>
      <el-table-column label="数量" align="center" prop="saleMete"></el-table-column>
      <el-table-column label="单价(元)" align="center" prop="price"></el-table-column>
      <el-table-column label="金额" align="center" prop="amount"></el-table-column>
      <el-table-column label="创建人" align="center" prop="createUserName"></el-table-column>
      <el-table-column label="出售日期" align="center" prop="saleDate"></el-table-column>
      <el-table-column label="审核人" align="center" prop="auditUserName"></el-table-column>
      <el-table-column label="审核日期" align="center" prop="auditTime"></el-table-column>
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
  </div>
</template>
<script setup>
import { defineProps, ref, watch } from 'vue'
import { getDateList } from '@/api/contract/scrap-ledger'
import usePagination from '@compos/use-pagination'

const { handleSizeChange, handleCurrentChange, total, setTotalPage, queryPage } = usePagination({ fetchHook: fetchList })

const props = defineProps({
  queryId: {
    type: Number,
    default: 1
  },
  date: {
    type: Array,
    default: () => []
  },
  scrapType: {
    type: Number,
    default: undefined
  }
})

watch(
  () => props.queryId,
  (val) => {
    if (val === 2) {
      fetchList()
    }
  },
  { immediate: true }
)

watch(
  () => props.date,
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

const dataFormat = ref([
  ['createTime', ['parse-time', '{y}-{m}-{d}']],
  ['saleDate', ['parse-time', '{y}-{m}-{d}']],
  ['auditTime', ['parse-time', '{y}-{m}-{d}']]
])

const tableData = ref([])

async function fetchList() {
  try {
    const { content, totalElements } = await getDateList({
      ...queryPage,
      startDate: props.date[0],
      endDate: props.date[1],
      wasteClassificationId: props.scrapType
    })
    console.log(content)
    tableData.value = content
    setTotalPage(totalElements)
  } catch (error) {
    console.log(error)
  }
}
</script>
