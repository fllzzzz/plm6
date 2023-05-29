<template>
  <div v-show="!props.showType && props.query?.category !== 64" class="my-code" style="margin-top: 20px">*点击上方表格数据查看详情</div>
  <div v-show="props.showType" style="margin-top: 20px">
    <div class="head-container" v-show="props.query?.category !== 64">
      <el-tag class="filter-item" size="small" style="float: left;">{{
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
          v-permission="permission.print"
          :api-key="
            showType === 'INVENTORY'
              ? 'enclosureShipMeteDetail'
              : showType === 'ASSIGNMENT'
              ? 'enclosureShipTaskMeteDetail'
              : showType === 'STORAGE'
              ? 'enclosureShipInboundMeteDetail'
              : showType === 'CUMULATIVE_SHIPMENT'
              ? 'enclosureShipTotalMeteDetail'
              : showType === 'SHIPMENT_MONTH'
              ? 'enclosureShipMonthMeteDetail'
              : showType === 'IN_STOCK'
              ? 'enclosureShipStockMeteDetail'
              : 'mesShipTrainMeteDetail'
          "
          :params="{
            projectId: props.projectId,
            ...props.query,
            workshopId: props.workshopId,
            shipEnumType:
              showType === 'INVENTORY'
                ? projectSearchTypeEnum.INVENTORY.V
                : showType === 'ASSIGNMENT'
                ? projectSearchTypeEnum.ASSIGNMENT.V
                : showType === 'STORAGE'
                ? projectSearchTypeEnum.STORAGE.V
                : showType === 'CUMULATIVE_SHIPMENT'
                ? projectSearchTypeEnum.CUMULATIVE_SHIPMENT.V
                : showType === 'SHIPMENT_MONTH'
                ? projectSearchTypeEnum.SHIPMENT_MONTH.V
                : showType === 'IN_STOCK'
                ? projectSearchTypeEnum.IN_STOCK.V
                : projectSearchTypeEnum.ACCUMULATED_NUMBER.V,
          }"
          size="mini"
          type="warning"
        />
      </div>
    </div>
    <common-table
      v-show="props.query?.category !== 64"
      :data="list"
      v-loading="tableLoading"
      :show-empty-symbol="false"
      show-summary
      :summary-method="getSummaries"
      :max-height="maxHeight - 30"
    >
      <el-table-column prop="index" label="序号" align="center" width="45" type="index" />
      <el-table-column
        v-if="showType !== 'INVENTORY'"
        key="workshop.name"
        prop="workshop.name"
        label="车间"
        align="center"
        :show-overflow-tooltip="true"
      />
      <el-table-column key="area.name" prop="area.name" label="批次" align="center" :show-overflow-tooltip="true" />
      <el-table-column key="name" prop="name" label="名称" align="center" :show-overflow-tooltip="true" min-width="100px" />
      <el-table-column key="serialNumber" prop="serialNumber" label="编号" align="center" :show-overflow-tooltip="true" />
      <el-table-column
        v-if="props.query?.category !== 32"
        key="plate"
        prop="plate"
        label="板型"
        align="center"
        :show-overflow-tooltip="true"
        min-width="120px"
      />
      <el-table-column key="length" prop="length" label="单长（mm）" align="center" :show-overflow-tooltip="true">
        <template #default="{ row }">
          <span>{{ row.length || '-' }}</span>
        </template>
      </el-table-column>
      <el-table-column key="quantity" prop="quantity" label="数量（件）" align="center" :show-overflow-tooltip="true" width="90px" />
      <el-table-column key="totalLength" prop="totalLength" label="总长（m）" align="center" :show-overflow-tooltip="true">
        <template #default="{ row }">
          <span>{{ convertUnits(row.totalLength, 'mm', 'm', DP.MES_ENCLOSURE_L__M) || '-' }}</span>
        </template>
      </el-table-column>
    </common-table>
    <!--分页组件-->
    <el-pagination
      v-show="props.query?.category !== 64"
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
import { summaryDetail } from '@/api/ship-manage/pack-and-ship/enclosure-ship-summary'
import { convertUnits } from '@/utils/convert/unit'
import { DP } from '@/settings/config'
// import { tableSummary } from '@/utils/el-extra'
import { projectSearchTypeEnum } from '@enum-ms/mes'
// import { enclosureShipStatisticsTypeEnum } from '@enum-ms/ship-manage'
import useMaxHeight from '@compos/use-max-height'
import usePagination from '@compos/use-pagination'

const props = defineProps({
  modelValue: {
    type: Boolean,
    default: false
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
  },
  weightStatus: {
    type: Number
  },
  permission: {
    type: Object,
    default: () => {}
  }
})

const list = ref([])
const tableLoading = ref(false)

const { maxHeight } = useMaxHeight({ extraBox: ['.head-container'], wrapperBox: ['.detail-container'], paginate: true })
const { handleSizeChange, handleCurrentChange, total, setTotalPage, queryPage } = usePagination({ fetchHook: fetchDetail })

watch(
  () => props.showType,
  (val) => {
    if (val) {
      fetchDetail()
    }
  },
  { immediate: true, deep: true }
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

// 合计
function getSummaries(param) {
  const { columns, data } = param
  const sums = []
  columns.forEach((column, index) => {
    if (index === 0) {
      sums[index] = '合计'
      return
    }
    if (column.property === 'quantity') {
      const values = data.map((item) => Number(item[column.property]))
      let valuesSum = 0
      if (!values.every((value) => isNaN(value))) {
        valuesSum = values.reduce((prev, curr) => {
          const value = Number(curr)
          if (!isNaN(value)) {
            return prev + curr
          } else {
            return prev
          }
        }, 0)
      }
      sums[index] = valuesSum
    }
    if (column.property === 'totalLength') {
      const values = data.map((item) => Number(item[column.property]))
      let valuesSum = 0
      if (!values.every((value) => isNaN(value))) {
        valuesSum = values.reduce((prev, curr) => {
          const value = Number(curr)
          if (!isNaN(value)) {
            return prev + curr
          } else {
            return prev
          }
        }, 0)
      }
      sums[index] = convertUnits(valuesSum, 'mm', 'm', 2)
    }
  })
  return sums
}
</script>
<style lang="scss" scoped>
</style>
