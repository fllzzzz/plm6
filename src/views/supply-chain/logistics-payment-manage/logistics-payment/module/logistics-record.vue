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
      <common-table :data="list" v-loading="tableLoading" show-summary :summary-method="getSummaries" :data-format="dataFormat" :max-height="maxHeight">
        <el-table-column label="序号" type="index" align="center" width="60" />
        <el-table-column prop="inboundTime" label="入库日期" width="90" align="center" show-overflow-tooltip />
        <el-table-column prop="licensePlate" label="车牌号" align="center" show-overflow-tooltip />
        <el-table-column prop="loadingWeight" label="装载重量（吨）" align="center" show-overflow-tooltip />
        <el-table-column prop="freight" label="运输费" align="center" show-overflow-tooltip />
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
    </template>
  </common-drawer>
</template>

<script setup>
import { logisticsRecordDetail } from '@/api/supply-chain/logistics-payment-manage/jd-logistics-record-ledger'
import { ref, nextTick, defineEmits, defineProps, watch, computed } from 'vue'

import { digitUppercase, getDP, toThousand } from '@/utils/data-type/number'
import { tableSummary } from '@/utils/el-extra'

import useVisible from '@/composables/use-visible'
import useMaxHeight from '@compos/use-max-height'
import usePagination from '@compos/use-pagination'
import inboundRecord from './inbound-record'

const emit = defineEmits(['update:modelValue'])

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

// 请求参数
const params = computed(() => {
  // 汇总列表
  return {
    supplierId: props.detailInfo.supplierId,
    projectId: props.detailInfo.projectId
  }
})

watch(
  visible,
  (val) => {
    if (val) {
      fetchList()
    }
  },
  { immediate: true }
)

const list = ref([])
const drawerRef = ref()
const tableLoading = ref(false)
const dataFormat = ref([
  ['inboundTime', ['parse-time', '{y}-{m}-{d}']],
  ['loadingWeight', ['to-fixed', 2]],
  ['freight', 'to-thousand']
])

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

// 合计
function getSummaries(param) {
  const summary = tableSummary(param, {
    props: ['freight']
  })
  const num = summary[2]
  if (num) {
    const dp = getDP(num)
    summary[3] = digitUppercase(num)
    summary[2] = toThousand(num, dp)
  }
  return summary
}

// 获取物流记录
async function fetchList() {
  let _list = []
  tableLoading.value = true
  try {
    const { content = [], totalElements } = await logisticsRecordDetail({ ...params.value, ...queryPage })
    content.map(v => {
      v.loadingWeight = v.loadingWeight ? v.loadingWeight / 1000 : 0
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
