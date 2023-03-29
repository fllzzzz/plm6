<template>
  <div v-show="!props.showType" class="my-code" style="margin-top: 20px">*点击上方表格数据查看详情</div>
  <div v-show="props.showType" style="margin-top: 20px">
    <div class="head-container">
      <el-tag size="small" style="float: left">{{
        showType === 'INVENTORY'
          ? '清单总量'
          : showType === 'ASSIGNMENT'
          ? '任务量'
          : showType === 'STORAGE'
          ? '入库量'
          : showType === 'CUMULATIVE_SHIPMENT'
          ? '累计发运'
          : showType === 'SHIPMENT_MONTH'
          ? '本月发运'
          : showType === 'IN_STOCK'
          ? '库存'
          : '累计车次'
      }}</el-tag>
      <div class="filter-item" style="float: right">
        <print-table
          :api-key="
            showType === 'INVENTORY'
              ? 'mesShipMeteDetail'
              : showType === 'ASSIGNMENT'
              ? 'mesShipTaskMeteDetail'
              : showType === 'STORAGE'
              ? 'mesShipInboundMeteDetail'
              : showType === 'CUMULATIVE_SHIPMENT'
              ? 'mesShipTotalMeteDetail'
              : showType === 'SHIPMENT_MONTH'
              ? 'mesShipMonthMeteDetail'
              : showType === 'IN_STOCK'
              ? 'mesShipStockMeteDetail'
              : 'mesShipTrainMeteDetail'
          "
          :params="{
            ...query,
          }"
          size="mini"
          type="warning"
        />
      </div>
    </div>
    <common-table :data="list" v-loading="tableLoading" :max-height="props.maxHeight">
      <el-table-column prop="index" label="序号" align="center" width="45" type="index" />
      <el-table-column key="monomerName" prop="monomerName" label="单体" align="center" :show-overflow-tooltip="true" />
      <el-table-column key="areaName" prop="areaName" label="区域" align="center" :show-overflow-tooltip="true" />
      <el-table-column key="serialNumber" prop="serialNumber" label="编号" align="center" :show-overflow-tooltip="true" />
      <el-table-column key="specification" prop="specification" label="规格" align="center" :show-overflow-tooltip="true" />
      <el-table-column key="material" prop="material" label="材质" align="center" :show-overflow-tooltip="true" />
      <el-table-column key="quantity" prop="quantity" label="数量" align="center" :show-overflow-tooltip="true" />
      <el-table-column key="totalNetWeight" prop="totalNetWeight" label="总重（kg）" align="center" :show-overflow-tooltip="true" />
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
import { ref, defineProps, watch } from 'vue'
import { summaryDetail } from '@/api/mes/pack-and-ship/ship-summary'
import { projectSearchTypeEnum } from '@enum-ms/mes'
import usePagination from '@compos/use-pagination'

const props = defineProps({
  maxHeight: {
    type: String
  },
  showType: {
    type: String
  },
  query: {
    type: Object
  },
  workshopId: {
    type: Number
  },
  projectId: {
    type: Number
  }
})

const list = ref([])
const tableLoading = ref(false)

const { handleSizeChange, handleCurrentChange, total, setTotalPage, queryPage } = usePagination({ fetchHook: fetchDetail })

watch(
  () => props.showType,
  (val) => {
    if (val) {
      fetchDetail()
    }
  }, { immediate: true }
)
async function fetchDetail() {
  list.value = []
  if (!props.showType) {
    return
  }
  tableLoading.value = true
  try {
    const { content = [], totalElements } = await summaryDetail({
      projectId: props.projectId,
      ...props.query,
      workshopId: props.workshopId,
      shipEnumType: projectSearchTypeEnum[props.showType].V,
      ...queryPage
    })
    list.value = content
    setTotalPage(totalElements)
    tableLoading.value = false
  } catch (error) {
    console.log('获取详情失败', error)
    tableLoading.value = false
  }
}
</script>
<style lang="scss" scoped>
</style>
