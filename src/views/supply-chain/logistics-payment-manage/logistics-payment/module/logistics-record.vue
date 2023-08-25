<template>
  <common-drawer
    ref="drawerRef"
    title="物流记录"
    :close-on-click-modal="false"
    v-model="visible"
    direction="rtl"
    :before-close="handleClose"
    custom-class="invoice-record"
    size="80%"
  >
    <template #titleAfter>
      <el-tag type="warning" effect="plain" size="medium">物流公司：{{detailInfo.supplierName}}</el-tag>
      <el-tag>运费总额：{{detailInfo.freight}}</el-tag>
    </template>
    <template #content>
      <div class="head-container" style="margin-bottom:5px;">
        <el-date-picker
          v-model="query.date"
          type="daterange"
          range-separator=":"
          size="small"
          value-format="x"
          class="filter-item date-item"
          start-placeholder="开始时间"
          end-placeholder="结束时间"
          style="width: 240px"
          @change="handleDateChange"
        />
        <el-input
          v-model="query.licensePlate"
          placeholder="车牌号搜索"
          class="filter-item"
          style="width: 200px"
          size="small"
          clearable
        />
        <el-input
          v-model="query.purchaseSn"
          placeholder="采购单"
          class="filter-item"
          style="width: 200px"
          size="small"
          clearable
        />
        <el-input
          v-model="query.purchaseUserName"
          placeholder="采购员搜索"
          class="filter-item"
          style="width: 200px"
          size="small"
          clearable
        />
        <el-input
          v-model="query.inboundSn"
          placeholder="入库单"
          class="filter-item"
          style="width: 200px"
          size="small"
          clearable
        />
        <!-- <el-input
          v-model="query.supplierName"
          placeholder="供应商"
          class="filter-item"
          style="width: 200px"
          size="small"
          clearable
        /> -->
        <common-button class="filter-item" size="small" type="success" icon="el-icon-search" @click.stop="fetchList">搜索</common-button>
        <common-button class="filter-item" size="small" type="warning" icon="el-icon-refresh" @click.stop="resetSubmit">重置</common-button>
        <div style="height:30px;">
          <div style="float:left;">
            <span v-if="isModify">
              <common-button type="warning" size="mini" @click="handelModifying(false, true)">取消录入</common-button>
              <common-button type="success" size="mini" @click="confirmModifying">预览并保存</common-button>
            </span>
            <common-button type="primary" v-if="!isModify && checkPermission(permission.freightChangeAdd)" :disabled="selectionData.length===0" @click="handelModifying(true)">运输费变更</common-button>
          </div>
          <div style="float:right;">
            <el-badge :value="modifyCount" :hidden="modifyCount <= 0">
              <common-button type="info" @click="changeRecordVisible=true">变更记录</common-button>
            </el-badge>
          </div>
        </div>
      </div>
      <common-table ref="tableRef" :data="list" v-loading="tableLoading" show-summary :summary-method="getSummaries" return-source-data :max-height="maxHeight-180" @selection-change="handleSelectionChange">
        <el-table-column type="selection" align="center" width="60" class="selection" :selectable="selectable" />
        <el-table-column prop="inboundTime" label="入库日期" width="100" align="center" show-overflow-tooltip>
          <template #default="{ row }">
             <table-cell-tag :show="row.boolPayment" name="已支付" :color="'#67c23a'" :offset="15" />
            <span>{{parseTime(row.inboundTime,'{y}-{m}-{d}')}}</span>
          </template>
        </el-table-column>
        <el-table-column prop="licensePlate" label="车牌号" align="center" show-overflow-tooltip />
        <el-table-column prop="loadingWeight" label="装载重量（吨）" align="center" show-overflow-tooltip />
        <el-table-column prop="freight" label="运输费" align="center" show-overflow-tooltip>
          <template #default="{ row }">
            <common-input-number
              v-if="isModify && (selectionData && selectionData.findIndex(v=>v.id===row.id)>-1)"
              v-model="row.newFreight"
              :step="1"
              :min="0"
              :max="99999999"
              :precision="2"
              size="small"
              style="width: 100%"
            />
            <template v-else>
              <span :style="`color:${row.boolFreightChanging?'red':''}`">{{ toThousand(row.freight,2) }}</span>
            </template>
          </template>
        </el-table-column>
        <el-table-column prop="purchaseSn" label="采购订单号" align="center" show-overflow-tooltip />
        <el-table-column prop="purchaseUserName" label="采购员" align="center" show-overflow-tooltip />
        <el-table-column prop="inboundSn" label="关联入库单" align="center" show-overflow-tooltip>
          <template #default="{ row }">
            <span class="clickable" @click="openRecord(row)"> {{ row.inboundSn }}</span>
          </template>
        </el-table-column>
        <el-table-column prop="supplierName" label="关联供应商" align="center" show-overflow-tooltip />
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
      <inbound-record v-model="recordVisible" :detailInfo="currentRow" :permission="permission"/>
      <mPreview v-model="previewVisible" :modified-data="modifiedData" @success="fetchList();getCount();" />
      <changeRecord v-model="changeRecordVisible" @success="fetchList();getCount();" :supplierId="detailInfo.supplierId" :permission="permission"/>
    </template>
  </common-drawer>
</template>

<script setup>
import { ElMessage } from 'element-plus'
import { logisticsRecordDetail, feeUnAudit } from '@/api/supply-chain/logistics-payment-manage/jd-logistics-record-ledger'
import { ref, nextTick, defineEmits, defineProps, watch, computed } from 'vue'

import checkPermission from '@/utils/system/check-permission'
import { digitUppercase, toThousand } from '@/utils/data-type/number'
import { tableSummary } from '@/utils/el-extra'
import { parseTime } from '@/utils/date'

import useVisible from '@/composables/use-visible'
import useMaxHeight from '@compos/use-max-height'
import usePagination from '@compos/use-pagination'
import inboundRecord from './inbound-record'
import mPreview from './preview'
import changeRecord from './change-record.vue'

const emit = defineEmits(['update:modelValue'])
const isModify = ref(false)

const props = defineProps({
  modelValue: {
    type: Boolean,
    require: true
  },
  detailInfo: {
    type: Object,
    default: () => {}
  },
  permission: {
    type: Object,
    default: () => {}
  },
  queryDate: {
    type: Object,
    default: () => {}
  }
})

const { visible, handleClose } = useVisible({ emit, props })
const { handleSizeChange, handleCurrentChange, total, setTotalPage, queryPage } = usePagination({ fetchHook: fetchList })
const currentRow = ref({})
const recordVisible = ref(false)
const changeRecordVisible = ref(false)
const query = ref({})
const modifyCount = ref(0)

// 请求参数
const params = computed(() => {
  // 汇总列表
  return {
    supplierId: props.detailInfo.supplierId,
    projectId: props.detailInfo.projectId,
    ...query.value
  }
})

watch(
  visible,
  (val) => {
    if (val) {
      query.value = {
        date: props.queryDate?.startDate && props.queryDate?.endDate ? [props.queryDate.startDate, props.queryDate.endDate] : [],
        startDate: props.queryDate?.startDate,
        endDate: props.queryDate?.endDate
      }
      fetchList()
      getCount()
    }
  },
  { immediate: true }
)

const tableRef = ref()
const list = ref([])
const drawerRef = ref()
const tableLoading = ref(false)
const selectionData = ref([])
const modifiedData = ref([])
const previewVisible = ref(false)

const { maxHeight } = useMaxHeight(
  {
    mainBox: '.invoice-record',
    extraBox: '.el-drawer__header',
    wrapperBox: '.el-drawer__body',
    paginate: true,
    minHeight: 300,
    navbar: false,
    clientHRepMainH: true
  },
  drawerRef
)

function selectable(row) {
  return !row.boolPayment
}

// 合计
function getSummaries(param) {
  const summary = tableSummary(param, {
    props: ['freight']
  })
  const num = summary[2]
  if (num) {
    summary[3] = digitUppercase(num)
    summary[2] = toThousand(num, 2)
  }
  return summary
}

// 时间变动
function handleDateChange(val) {
  if (query.value.date && query.value.date.length > 1) {
    query.value.startDate = val[0]
    query.value.endDate = val[1]
  } else {
    query.value.startDate = undefined
    query.value.endDate = undefined
  }
  fetchList()
}

// 重置搜索
function resetSubmit() {
  query.value = {}
  query.value.date = []
  fetchList()
}

function handelModifying(status, reset = false) {
  // 数据还原
  if (reset) {
    list.value.forEach((v) => {
      v.freight = v.originFreight
      v.newFreight = v.originFreight
    })
    tableRef.value.clearSelection()
  }
  isModify.value = status
}

function handleSelectionChange(val) {
  selectionData.value = val
}
function confirmModifying() {
  modifiedData.value = selectionData.value.filter((v) => v.newFreight !== v.originFreight)
  if (modifiedData.value.length > 0) {
    previewVisible.value = true
  } else {
    ElMessage.error('无更改数据')
  }
}

async function getCount() {
  try {
    modifyCount.value = await feeUnAudit({ supplierId: props.detailInfo.supplierId })
  } catch (error) {
    console.log('变更记录未审核数量', error)
  }
}

// 获取物流记录
async function fetchList() {
  let _list = []
  tableLoading.value = true
  try {
    const { content = [], totalElements } = await logisticsRecordDetail({ ...params.value, ...queryPage })
    content.map(v => {
      v.loadingWeight = v.loadingWeight ? (v.loadingWeight / 1000).toFixed(2) : 0
      v.originFreight = v.freight
      v.newFreight = v.freight
    })
    _list = content
    setTotalPage(totalElements)
  } catch (error) {
    console.log('获取物流记录失败', error)
  } finally {
    list.value = _list
    tableLoading.value = false
  }
}

// 打开入库记录
function openRecord(row) {
  currentRow.value = row.sourceRow
  nextTick(() => {
    recordVisible.value = true
  })
}
</script>
<style lang="scss" scoped>
.clickable {
  cursor: pointer;
  color:#409eff;
}
</style>
