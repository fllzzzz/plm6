<template>
  <common-drawer
    ref="dialogRef"
    title="物流记录"
    :close-on-click-modal="false"
    v-model="visible"
    direction="rtl"
    :before-close="handleClose"
    custom-class="invoice-record"
    size="80%"
  >
    <template #titleAfter>
      <el-tag v-if="props.type===logisticsSearchTypeEnum.PRODUCT.V" effect="plain">{{ detailInfo.projectName }}</el-tag>
      <el-tag v-if="props.type===logisticsSearchTypeEnum.MATERIAL.V" effect="plain">{{ detailInfo.serialNumber }}</el-tag>
      <el-tag v-if="props.type===logisticsSearchTypeEnum.COMPANY.V" effect="plain">{{ detailInfo.supplierName }}</el-tag>
      <common-radio-button
        v-model="freightChangeType"
        :options="freightChangeTypeEnum.ENUM"
        showOptionAll
        type="enumSL"
        size="small"
        class="filter-item"
        @change="fetchList"
      />
    </template>
    <template #titleRight>
      <div class="print-wrap">
        <print-table
          v-permission="props.permission?.print"
          api-key="purchaseLogisticsRecord"
          :params="{ ...params }"
          size="mini"
          type="warning"
        />
      </div>
    </template>
    <template #content>
      <common-table :data="list" :data-format="dataFormat" :max-height="maxHeight">
        <el-table-column label="序号" type="index" align="center" width="60" />
        <el-table-column prop="shipDate" label="运输日期" align="center" show-overflow-tooltip />
        <el-table-column prop="name" label="所属项目或采购合同" align="center" show-overflow-tooltip v-if="props.type===logisticsSearchTypeEnum.COMPANY.V" />
        <el-table-column prop="supplierName" label="物流公司" align="center" show-overflow-tooltip v-if="props.type!==logisticsSearchTypeEnum.COMPANY.V" />
        <el-table-column prop="type" label="运输属性" align="center" show-overflow-tooltip />
        <el-table-column prop="licensePlate" label="车牌号" align="center" show-overflow-tooltip />
        <el-table-column prop="loadingWeight" label="装载重量(kg)" align="center" show-overflow-tooltip />
        <el-table-column prop="carModel" label="车型" align="center" show-overflow-tooltip />
        <el-table-column prop="driverName" label="司机姓名" align="center" show-overflow-tooltip />
        <el-table-column prop="driverPhone" label="电话" align="center" show-overflow-tooltip />
        <el-table-column prop="freight" label="运费额" align="center" show-overflow-tooltip />
        <el-table-column prop="changeFreight" label="状态" align="center" show-overflow-tooltip />
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
import { logisticsRecordDetail } from '@/api/supply-chain/logistics-payment-manage/logistics-record-ledger'
import { ref, defineEmits, defineProps, watch, computed } from 'vue'

import { logisticsSearchTypeEnum } from '@enum-ms/contract'
import { freightChangeTypeEnum } from '@enum-ms/mes'
import useVisible from '@/composables/use-visible'
import useMaxHeight from '@compos/use-max-height'
import usePagination from '@compos/use-pagination'

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
  type: {
    type: [String, Number],
    default: undefined
  },
  permission: {
    type: Object,
    default: () => {}
  }
})

const freightChangeType = ref(undefined)
const { visible, handleClose } = useVisible({ emit, props })
const { handleSizeChange, handleCurrentChange, total, setTotalPage, queryPage } = usePagination({ fetchHook: fetchList })

// 请求参数
const params = computed(() => {
  return {
    id: props.detailInfo.id || props.detailInfo.supplierId,
    type: props.type,
    branchCompanyId: props.detailInfo.supplierId ? props.detailInfo.branchCompanyId : undefined,
    changeFreight: freightChangeType.value
  }
})

watch(
  visible,
  (val) => {
    if (val) {
      freightChangeType.value = undefined
      fetchList()
    }
  }
)

const list = ref([])
const dialogRef = ref()
const tableLoading = ref(false)
const dataFormat = ref([
  ['shipDate', ['parse-time', '{y}-{m}-{d}']],
  ['type', ['parse-enum', logisticsSearchTypeEnum]],
  ['loadingWeight', ['to-fixed', 2]],
  ['freight', 'to-thousand'],
  ['changeFreight', ['parse-enum', freightChangeTypeEnum, { f: 'SL' }]]
])

const { maxHeight } = useMaxHeight(
  {
    mainBox: '.logistics-record-detail',
    extraBox: '.el-drawer__header',
    wrapperBox: '.el-drawer__body',
    extraHeight: '5vh',
    minHeight: 300,
    navbar: false,
    clientHRepMainH: true
  },
  dialogRef
)

// 获取物流记录
async function fetchList() {
  let _list = []
  tableLoading.value = true
  try {
    const { content = [], totalElements } = await logisticsRecordDetail({ ...params.value, ...queryPage })
    _list = content
    setTotalPage(totalElements)
  } catch (error) {
    console.log('获取物流记录失败', error)
  } finally {
    list.value = _list
    tableLoading.value = false
  }
}
</script>
