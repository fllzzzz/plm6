<template>
  <div class="app-container">
    <el-row v-loading="summaryLoading" :gutter="20" class="panel-group">
      <el-col :span="6" class="card-panel-col">
        <panel name="项目领料总量" num-color="#1890ff" :end-val="summaryInfo.plates || 0" />
      </el-col>
      <el-col :span="6" class="card-panel-col">
        <panel name="构件总重量" num-color="#1890ff" :end-val="summaryInfo.artifacts || 0" />
      </el-col>
      <el-col :span="6" class="card-panel-col">
        <panel name="损耗" num-color="#1890ff" :end-val="summaryInfo.loss || 0" />
      </el-col>
      <el-col :span="6" class="card-panel-col">
        <panel name="损耗率(%)" num-color="#1890ff" :end-val="summaryInfo.lossRate || 0" :precision="2" />
      </el-col>
    </el-row>

    <print-table
      api-key="mesMainMaterialTrack"
      v-permission="permission.print"
      :params="{
        projectId: globalProjectId,
      }"
      size="mini"
      type="warning"
      class="print-table"
      style="margin-top: 10px; margin-bottom: 10px; float: right"
    />
    <common-table v-loading="compareLoading" :data="compareList" :maxHeight="maxHeight" style="width: 100%">
      <el-table-column label="序号" type="index" align="center" width="60" />
      <el-table-column prop="name" :show-overflow-tooltip="true" label="品名" align="center">
        <template #default="{ row }">
          <span>{{ row.name }}</span>
        </template>
      </el-table-column>
      <el-table-column prop="thickness" :show-overflow-tooltip="true" label="厚度/规格" align="center">
        <template #default="{ row }">
          <span>{{ row.thickness }}</span>
        </template>
      </el-table-column>
      <el-table-column prop="material" :show-overflow-tooltip="true" label="材质" align="center">
        <template #default="{ row }">
          <span>{{ row.material }}</span>
        </template>
      </el-table-column>
      <el-table-column prop="listMete" :show-overflow-tooltip="true" label="清单量(kg)" align="center">
        <template #default="{ row }">
          <span>{{ row.listMete }}</span>
        </template>
      </el-table-column>
      <el-table-column prop="stockMete" :show-overflow-tooltip="true" label="库存量(库存=公共库+项目库)" align="center">
        <template #default="{ row }">
          <span class="tc-primary pointer" @click="handleStockClick(row)">{{ row.stockMete }}</span>
        </template>
      </el-table-column>
      <el-table-column prop="useMete" :show-overflow-tooltip="true" label="实际领用量(kg)" align="center">
        <template #default="{ row }">
          <span class="tc-primary pointer" @click="handleUseClick(row)">{{ row.useMete }}</span>
        </template>
      </el-table-column>
      <el-table-column prop="diff" :show-overflow-tooltip="true" label="差异(kg)" align="center">
        <template #default="{ row }">
          <span v-if="row.listMete >= row.useMete" class="tc-success">{{ row.diff }}</span>
          <span v-else class="tc-danger">{{ row.diff }}</span>
        </template>
      </el-table-column>
      <el-table-column prop="diffRate" :show-overflow-tooltip="true" label="差异率(%)" align="center">
        <template #default="{ row }">
          <span>{{ row.diffRate }}</span>
        </template>
      </el-table-column>
      <el-table-column prop="productMete" :show-overflow-tooltip="true" label="生产量(kg)" align="center">
        <template #default="{ row }">
          <span>{{ row.productMete }}</span>
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
    <!-- <div style="margin-top: 20px; position: relative" v-loading="echartsLoading">
      <el-date-picker
        v-model="year"
        type="year"
        size="mini"
        class="date-item filter-item"
        style="width: 150px !important; position: absolute; top: 0px; left: 0px; z-index: 1"
        placeholder="选择年"
        format="YYYY"
        value-format="YYYY"
        @change="updateChart"
      />
      <div v-loading="echartsLoading" id="recordMain" style="width: 100%; height: 350px"></div>
    </div>
    <production-record-detail v-model:visible="prDetailVisible" :projectId="globalProjectId" :month="month" />
    <outbound-record-detail v-model:visible="orDetailVisible" :projectId="globalProjectId" :month="month" /> -->
    <plate-use-record v-model:visible="plateUseVisible" :projectId="globalProjectId" :info="currentRow" />
    <stock-detail v-model:visible="stockVisible" :projectId="globalProjectId" :info="currentRow" />
  </div>
</template>

<script setup>
import { getSummary, getCompare } from '@/api/mes/production-manage/dashboard/main-material-track'
import { reactive, ref, watch, onMounted } from 'vue'

import { mainMaterialTrackPM as permission } from '@/page-permission/mes'
import checkPermission from '@/utils/system/check-permission'
import { mapGetters } from '@/store/lib'
// import moment from 'moment'
import panel from '@/components/Panel'
// import { prefixZero } from '@data-type/number'

import useMaxHeight from '@compos/use-max-height'
import usePagination from '@compos/use-pagination'
// import useBarRecordEcharts from '@compos/mes/production-manage/use-bar-record-echarts'
// import productionRecordDetail from './module/production-record-detail'
// import outboundRecordDetail from './module/outbound-record-detail'
import plateUseRecord from './module/plate-use-record'
import stockDetail from './module/stock-detail'

const { maxHeight } = useMaxHeight({ extraBox: ['.panel-group', '.print-table'], paginate: true })

const { globalProjectId } = mapGetters(['globalProjectId'])

// const year = ref(moment().year().toString())
// const month = ref()
// const prDetailVisible = ref(false)
// const orDetailVisible = ref(false)

// function showPRDetail(m) {
//   month.value = year.value + '-' + prefixZero(m)
//   prDetailVisible.value = true
// }

// function showORDetail(m) {
//   month.value = year.value + '-' + prefixZero(m)
//   orDetailVisible.value = true
// }

// const { updateChart, echartsLoading } = useBarRecordEcharts({ elementId: 'recordMain', globalProjectId, year, showPRDetail, showORDetail })

const summaryInfo = reactive({
  plates: 0,
  artifacts: 0
})
const summaryLoading = ref(false)

async function fetchSummary() {
  if (!checkPermission(permission.get)) {
    return
  }
  try {
    summaryLoading.value = true
    const data = await getSummary({
      projectId: globalProjectId.value
    })
    summaryInfo.artifacts = data.artifacts
    summaryInfo.plates = data.plates
    summaryInfo.loss = data.plates - data.artifacts
    summaryInfo.lossRate = data.plates ? ((data.plates - data.artifacts) / data.plates) * 100 : 0
  } catch (error) {
    console.log('汇总信息', error)
  } finally {
    summaryLoading.value = false
  }
}

const compareLoading = ref(false)
const compareList = ref([])
const { handleSizeChange, handleCurrentChange, total, setTotalPage, queryPage } = usePagination({ fetchHook: fetchCompareList })

async function fetchCompareList() {
  if (!checkPermission(permission.get)) {
    return
  }
  try {
    compareLoading.value = true
    const { content, totalElements } = await getCompare({
      projectId: globalProjectId.value,
      ...queryPage
    })
    setTotalPage(totalElements)
    compareList.value = content.map((v) => {
      v.diffRate = v.diffRate ? (v.diffRate).toFixed(2) : 0
      return v
    })
  } catch (error) {
    console.log('用量对比列表信息', error)
  } finally {
    compareLoading.value = false
  }
}

onMounted(() => {
  fetchSummary()
  fetchCompareList()
})

watch(
  globalProjectId,
  (val) => {
    fetchSummary()
    fetchCompareList()
    // updateChart()
  },
  { deep: true }
)

const plateUseVisible = ref(false)
const stockVisible = ref(false)
const currentRow = ref({})

function handleUseClick(row) {
  if (!checkPermission(permission.useRecordGet)) {
    return
  }
  currentRow.value = row
  plateUseVisible.value = true
}

function handleStockClick(row) {
  if (!checkPermission(permission.stockGet)) {
    return
  }
  currentRow.value = row
  stockVisible.value = true
}
</script>
