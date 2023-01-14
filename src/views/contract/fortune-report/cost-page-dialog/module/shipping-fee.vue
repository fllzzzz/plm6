
<template>
  <div>
    <div class="head-container" style="display: flex; justify-content: space-between">
      <div style="width: 300px">
        <print-table api-key="shippingFeeList" :params="costTypeData.projectId" size="mini" type="warning" class="filter-item" />
      </div>
      <div>
        <el-tag style="margin-left:8px;">装载重量合计（单位:t）： {{ convertUnits(totalAmount?.actualWeight, 'kg', 't', DP.COM_WT__T) }} </el-tag>
        <el-tag type="danger">运输费合计（单位:元）：{{ toThousand(totalAmount?.totalPrice) }}</el-tag>
      </div>
    </div>
    <common-table
      ref="tableRef"
      :data="detailData"
      :empty-text="'暂无数据'"
      :max-height="maxHeight"
      row-key="id"
      style="width: 100%"
      show-summary
      :summary-method="getSummaries"
    >
      <el-table-column prop="index" label="序号" align="center" width="60" type="index" />
      <el-table-column
        key="serialNumber"
        prop="serialNumber"
        sortable="custom"
        label="车次"
        align="center"
      >
        <template v-slot="scope">
          <table-cell-tag :show="scope.row.deliveryStatus===deliveryStatusEnum.RETURN.V" name="已取消" color="#f56c6c"/>
          <span>{{ scope.row.serialNumber }}</span>
        </template>
      </el-table-column>
      <el-table-column key="auditTime" prop="auditTime" sortable="custom" label="承运日期" width="120">
        <template v-slot="scope">
          <span v-parse-time="{ val: scope.row.auditTime, fmt: '{y}-{m}-{d}' }" />
        </template>
      </el-table-column>
      <el-table-column key="productType" prop="productType" label="装载类型" width="165">
        <template v-slot="scope">
          <el-tag
            v-for="item in cleanArray(EO.getBits(packTypeEnum, scope.row.productType, 'V'))"
            style="margin-right: 5px"
            :key="item"
            :type="packTypeEnum.V[item].T"
            effect="light"
            disable-transitions
            >{{ packTypeEnum.VL[item] }}</el-tag
          >
        </template>
      </el-table-column>
      <el-table-column
        key="licensePlate"
        prop="licensePlate"
        sortable="custom"
        :show-overflow-tooltip="true"
        label="车牌号"
        align="center"
      />
      <el-table-column
        key="driverName"
        prop="driverName"
        sortable="custom"
        :show-overflow-tooltip="true"
        label="司机姓名"
        align="center"
      />
      <el-table-column
        key="driverPhone"
        prop="driverPhone"
        :show-overflow-tooltip="true"
        label="司机电话"
        align="center"
      />
      <el-table-column
        key="auditUserName"
        prop="auditUserName"
        sortable="custom"
        :show-overflow-tooltip="true"
        label="办理人"
        align="center"
      />
      <el-table-column
        :show-overflow-tooltip="true"
        prop="actualWeight"
        label="装载重量(t)"
        align="center"
      >
        <template v-slot="scope">
          <span>{{ convertUnits(scope.row.actualWeight, 'kg', 't', DP.COM_WT__T) }}</span>
        </template>
      </el-table-column>
      <el-table-column prop="priceType" label="计价方式" align="center">
        <template v-slot="scope">
          <span>{{ logisticsPriceTypeEnum.VL[scope.row.priceType] }}</span>
        </template>
      </el-table-column>
      <el-table-column
        :show-overflow-tooltip="true"
        prop="price"
        label="运输单价"
        align="right"
        min-width="120"
      >
        <template v-slot="scope">
          <span>{{ toFixed(scope.row.price, DP.YUAN) }}</span>
          <span :class="scope.row.priceType === logisticsPriceTypeEnum.WEIGHT.V ? 'blue':'orange'" style="margin-left:3px;">{{ logisticsPriceTypeEnum.V[scope.row.priceType].unit }}</span>
        </template>
      </el-table-column>
      <el-table-column
        :show-overflow-tooltip="true"
        prop="totalPrice"
        label="运输费(元)"
        align="right"
        min-width="120"
      >
        <template v-slot="scope">
          <span>{{ toFixed(scope.row.totalPrice, DP.YUAN) }}</span>
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
import { getShippingList, getShippingSummary } from '@/api/contract/fortune-report/detail-fee'
import { ref, defineProps, watch } from 'vue'

import { packTypeEnum, deliveryStatusEnum, logisticsPriceTypeEnum } from '@enum-ms/mes'
import { toThousand } from '@data-type/number'
import { tableSummary } from '@/utils/el-extra'
import useMaxHeight from '@compos/use-max-height'
import { DP } from '@/settings/config'
import { toFixed } from '@/utils/data-type'
import { cleanArray } from '@/utils/data-type/array'
import EO from '@enum'
import { convertUnits } from '@/utils/convert/unit'

import usePagination from '@compos/use-pagination'

const { handleSizeChange, handleCurrentChange, total, setTotalPage, queryPage } = usePagination({ fetchHook: fetchShippingFee })

const props = defineProps({
  costTypeData: {
    type: Object,
    default: () => {}
  }
})

const tableRef = ref()
const detailData = ref([])
const totalAmount = ref()

const { maxHeight } = useMaxHeight({
  paginate: true
})

watch(
  () => props.costTypeData.projectId,
  (value) => {
    fetchShippingFee()
    fetchSummary()
  },
  { immediate: true, deep: true }
)

// 合计
function getSummaries(param) {
  return tableSummary(param, {
    props: ['actualWeight', 'totalPrice'],
    toThousandFields: ['actualWeight', 'totalPrice']
  })
}

async function fetchSummary() {
  try {
    const data = await getShippingSummary(props.costTypeData.projectId)
    totalAmount.value = data || {}
  } catch (error) {
    console.log('运输汇总', error)
  }
}

async function fetchShippingFee() {
  try {
    const { content, totalElements } = await getShippingList({ projectId: props.costTypeData.projectId, ...queryPage })
    detailData.value = content || []
    setTotalPage(totalElements)
  } catch (error) {
    console.log('运输费用', error)
  }
}
</script>
<style lang="scss" scoped>
  .blue{
    color:#409eff;
  }
  .orange{
    color:#e6a23c;
  }
</style>
