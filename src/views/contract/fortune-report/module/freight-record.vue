<template>
  <common-drawer
    customClass="freight-record-drawer"
    :close-on-click-modal="false"
    append-to-body
    v-model="visible"
    :before-close="handleClose"
    :title="`成品运费记录 ${props.detailRow.project}`"
    :wrapper-closable="true"
    size="70%"
  >
    <template #titleRight>
      <el-tag effect="plain" type="warning" size="medium">成品运费：{{ props.detailRow.freight }}</el-tag>
      <div class="print-wrap">
        <print-table v-permission="permission.printDetail" api-key="purchaseLogisticsRecord" :params="params" size="mini" type="warning" />
      </div>
    </template>
    <template #content>
      <common-table :data="list" :data-format="columnsDataFormat" :max-height="maxHeight">
        <el-table-column label="序号" type="index" align="center" width="60" />
        <el-table-column prop="shipDate" label="运输日期" align="center" show-overflow-tooltip />
        <el-table-column prop="type" label="运输属性" align="center" show-overflow-tooltip />
        <el-table-column prop="licensePlate" label="车牌号" align="center" show-overflow-tooltip>
          <template #default="{ row }">
            <table-cell-tag :show="Boolean(row.boolPersonalEnum)" name="个人" :offset="15" />
            <span>{{ row.licensePlate }}</span>
          </template>
        </el-table-column>
        <el-table-column prop="loadingWeight" label="装载重量(kg)" align="center" show-overflow-tooltip />
        <el-table-column prop="carModel" label="车型" align="center" show-overflow-tooltip />
        <el-table-column prop="driverName" label="司机姓名" align="center" show-overflow-tooltip />
        <el-table-column prop="driverPhone" label="电话" align="center" show-overflow-tooltip />
        <el-table-column prop="freight" label="运费额" align="right" show-overflow-tooltip />
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
import { ref, defineEmits, defineProps, watch, computed, inject } from 'vue'

import { logisticsSearchTypeEnum } from '@enum-ms/contract'
import { freightChangeTypeEnum } from '@enum-ms/mes'
import { DP } from '@/settings/config'

import useMaxHeight from '@compos/use-max-height'
import useVisible from '@compos/use-visible'
import usePagination from '@compos/use-pagination'

const props = defineProps({
  modelValue: {
    type: Boolean,
    require: true
  },
  detailRow: {
    type: Object,
    default: () => {}
  },
  secondPickerTime: {
    type: Object,
    default: () => {}
  }
})

const list = ref([])
const tableLoading = ref(false)

const emit = defineEmits(['update:modelValue'])
const { visible, handleClose } = useVisible({ emit, props })
const { handleSizeChange, handleCurrentChange, total, setTotalPage, queryPage } = usePagination({ fetchHook: fetchList })

const permission = inject('permission')

const params = computed(() => {
  return {
    id: props.detailRow.id,
    type: logisticsSearchTypeEnum.PRODUCT.V,
    secondStartDate: props.secondPickerTime.startDate,
    secondEndDate: props.secondPickerTime.endDate
  }
})

watch(
  () => visible.value,
  (val) => {
    if (val) {
      fetchList()
    }
  }
)

// 列格式转换
const columnsDataFormat = computed(() => {
  return [
    ['shipDate', ['parse-time', '{y}-{m}-{d}']],
    ['type', ['parse-enum', logisticsSearchTypeEnum]],
    ['loadingWeight', ['to-fixed', DP.COM_WT__KG]],
    ['freight', 'to-thousand'],
    ['changeFreight', ['parse-enum', freightChangeTypeEnum, { f: 'SL' }]]
  ]
})

const { maxHeight } = useMaxHeight(
  {
    mainBox: '.freight-record-drawer',
    extraBox: ['.el-drawer__header'],
    wrapperBox: ['.el-drawer__body'],
    paginate: true
  },
  visible
)

// 获取成品运费记录
async function fetchList() {
  let _list = []
  tableLoading.value = true
  try {
    const { content = [], totalElements } = await logisticsRecordDetail({ ...params.value, ...queryPage })
    _list = content
    setTotalPage(totalElements)
  } catch (error) {
    console.log('获取成品运费记录失败', error)
  } finally {
    list.value = _list
    tableLoading.value = false
  }
}
</script>
