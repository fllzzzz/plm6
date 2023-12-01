<template>
  <common-drawer
    ref="drawerRef"
    custom-class="detail-drawer"
    :title="`项目:${detailData.project.serialNumber + ' ' + detailData.project.shortName}`"
    v-model="drawerVisible"
    direction="rtl"
    :before-close="handleClose"
    size="70%"
  >
    <template #titleRight>
      <print-table
        :api-key="category === mesShipStatisticsTypeEnum.STRUCTURE.V ?'mesProjectShipDetail':'mesProjectShipDetailDirect'"
        v-permission="permission.print"
        :params="{
          projectId: props.projectId,
          workshopId: props.workshopId,
          ...props.query,
        }"
        size="mini"
        type="warning"
      />
    </template>
    <template #content>
      <el-date-picker
        v-model="date"
        start-placeholder="开始日期"
        end-placeholder="结束日期"
        style="margin-bottom: 20px;"
        value-format="x"
        type="daterange"
        @change="dateChange"
      />
      <!--表格渲染-->
      <common-table :data="list" v-loading="tableLoading" show-summary :summary-method="getSummaries" :max-height="maxHeight - 70">
        <el-table-column prop="index" label="序号" align="center" width="45" type="index" />
        <el-table-column key="monomerName" prop="monomerName" label="单体" align="center" :show-overflow-tooltip="true" />
        <el-table-column key="areaName" prop="areaName" label="区域" align="center" :show-overflow-tooltip="true" />
        <el-table-column key="serialNumber" prop="serialNumber" label="编号" align="center" :show-overflow-tooltip="true" />
        <el-table-column key="netWeight" prop="netWeight" label="单净重（kg）" align="center" :show-overflow-tooltip="true">
          <template v-slot="scope">
            <span>{{ toThousand(scope.row.netWeight, DP.COM_WT__KG) }}</span>
            <!-- <span style="margin-left: 3px">kg</span> -->
          </template>
        </el-table-column>
        <el-table-column key="grossWeight" prop="grossWeight" label="单毛重（kg）" align="center" :show-overflow-tooltip="true">
          <template v-slot="scope">
            <span>{{ toThousand(scope.row.grossWeight, DP.COM_WT__KG) }}</span>
            <!-- <span style="margin-left: 3px">kg</span> -->
          </template>
        </el-table-column>
        <el-table-column key="totalNetWeight" prop="totalNetWeight" label="总净重（kg）" align="center" :show-overflow-tooltip="true">
          <template v-slot="scope">
            <span>{{ toThousand(scope.row.totalNetWeight, DP.COM_WT__KG) }}</span>
            <!-- <span style="margin-left: 3px">kg</span> -->
          </template>
        </el-table-column>
        <el-table-column key="totalGrossWeight" prop="totalGrossWeight" label="总毛重（kg）" align="center" :show-overflow-tooltip="true">
          <template v-slot="scope">
            <span>{{ toThousand(scope.row.totalGrossWeight, DP.COM_WT__KG) }}</span>
            <!-- <span style="margin-left: 3px">kg</span> -->
          </template>
        </el-table-column>
        <el-table-column key="quantity" prop="quantity" label="清单数" align="center" :show-overflow-tooltip="true" />
        <el-table-column key="inboundQuantity" prop="inboundQuantity" label="入库数" align="center" :show-overflow-tooltip="true" />
        <el-table-column key="cargoQuantity" prop="cargoQuantity" label="发运数" align="center" :show-overflow-tooltip="true" />
        <el-table-column key="cargoNetWeight" prop="cargoNetWeight" label="发运量（kg）" align="center" :show-overflow-tooltip="true">
          <template v-slot="scope">
            <span>{{ toThousand(scope.row.cargoNetWeight, DP.COM_WT__KG) }}</span>
            <!-- <span style="margin-left: 3px">kg</span> -->
          </template>
        </el-table-column>
        <el-table-column key="cargoDate" label="发运时间" align="center" :show-overflow-tooltip="true">
          <template #default="{row}">
            <span v-if="row.cargoDate">{{ `${parseTime(row.cargoDate, '{y}-{m}-{d}')}` }}</span>
            <span v-else>-</span>
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
    </template>
  </common-drawer>
</template>

<script setup>
import { inboundDetail, inboundDetailDirect } from '@/api/ship-manage/pack-and-ship/ship-summary'
import useVisible from '@compos/use-visible'
import usePagination from '@compos/use-pagination'
import useMaxHeight from '@compos/use-max-height'
import { DP } from '@/settings/config'
import { tableSummary } from '@/utils/el-extra'
import { toThousand } from '@/utils/data-type/number'
import { mesShipStatisticsTypeEnum } from '@enum-ms/ship-manage'
import { defineProps, defineEmits, ref } from 'vue'
import { mesShipSummaryPM as permission } from '@/page-permission/ship-manage'
import { parseTime } from '@/utils/date'
// import moment from 'moment'

const emit = defineEmits(['update:visible', 'changeDate'])
const list = ref([])
const tableLoading = ref(false)
const props = defineProps({
  visible: {
    type: Boolean,
    default: false
  },
  detailData: {
    type: Object,
    default: () => {}
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
  category: {
    type: [Number, String],
    default: mesShipStatisticsTypeEnum.STRUCTURE.V
  }
})

const { visible: drawerVisible, handleClose } = useVisible({ emit, props, field: 'visible', showHook: fetchDetail })
const { handleSizeChange, handleCurrentChange, total, setTotalPage, queryPage } = usePagination({ fetchHook: fetchDetail })

const date = ref([])

// 高度
const { maxHeight } = useMaxHeight(
  {
    mainBox: '.detail-drawer',
    extraBox: ['.el-drawer__header'],
    wrapperBox: ['.el-drawer__body'],
    navbar: false,
    clientHRepMainH: true,
    paginate: true
  },
  drawerVisible
)

const dateChange = (date) => {
  console.log(date)
  emit('changeDate', date)
  fetchDetail()
}

async function fetchDetail() {
  list.value = []
  tableLoading.value = true
  try {
    const api = props.category === mesShipStatisticsTypeEnum.STRUCTURE.V ? inboundDetail : inboundDetailDirect
    const params = {
      projectId: props.projectId,
      ...props.query,
      workshopId: props.workshopId,
      ...queryPage
    }
    const { content = [], totalElements } = await api(params)
    list.value = content
    setTotalPage(totalElements)
    tableLoading.value = false
    console.log(list.value)
  } catch (error) {
    console.log('获取详情失败', error)
    tableLoading.value = false
  }
}

// 合计
function getSummaries(param) {
  const summary = tableSummary(param, {
    props: [
      'quantity',
      'totalNetWeight',
      'totalGrossWeight',
      'inboundQuantity',
      'cargoQuantity',
      'cargoQuantity',
      'cargoNetWeight',
      'cargoGrossWeight'
    ]
  })
  return summary
}
</script>

<style rel="stylesheet/scss" lang="scss" scoped>
</style>

